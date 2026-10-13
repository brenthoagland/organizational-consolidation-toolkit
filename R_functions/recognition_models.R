# recognition_models.R
#
# Regression of recognition counts on ascriptive statuses and local types
# (the article's Appendix D), built on `pscl`. methods/04 and 05 call
# pscl::zeroinfl() directly; these functions prepare the predictors
# before the fit and read the models after it.
#
#   dummy_coded()              predictors as treatment-coded factors
#   check_ascriptive_cells()   the empty-cell stop before a fit
#   check_recognition_fit()    standard errors that cannot be computed (a stop),
#                              very large ones and a negbin dispersion at the
#                              Poisson limit (notes)
#   predicted_profiles()       predicted recognition per intersectional profile
#   recognition_model_table()  the three-model comparison of Tables D3-D5
#   unrecognized_members()     the diagnostic step after Figure 9: who meets
#                              the ideal's criteria but goes unrecognized

#' Predictors as treatment-coded factors, for the recognition models.
#'
#' Characters become factors, ordered factors lose their polynomial
#' contrasts, and unused levels are dropped, so an A < B < C criterion
#' enters the model as level dummies against a reference, matching the
#' article's dummy coding. Interactions ("a:b") name their columns.
#'
#' @param data       data.frame per data/data-dictionary.md
#' @param predictors character vector of predictor terms
#' @return `data` with those columns recoded
dummy_coded <- function(data, predictors) {
  vars <- unique(unlist(strsplit(predictors, ":", fixed = TRUE)))
  missing_cols <- setdiff(vars, names(data))
  if (length(missing_cols) > 0) {
    stop("columns not found in data: ", paste(missing_cols, collapse = ", "))
  }
  for (v in vars) {
    if (is.character(data[[v]])) data[[v]] <- factor(data[[v]])
    if (is.ordered(data[[v]]))   data[[v]] <- factor(data[[v]], ordered = FALSE)
    if (is.factor(data[[v]]))    data[[v]] <- droplevels(data[[v]])
  }
  data
}

#' Predicted nominations for every intersectional profile (input to the poset).
#'
#' Predicts each member's expected nomination count (marginaleffects,
#' type = "response") and averages within every observed combination of the
#' profile variables -- race x gender x local type in the article. One row
#' per profile present in the data: cross-product cells nobody
#' occupies do not appear, so check which cells are missing before
#' interpreting the rest. Because the average is over each profile's
#' observed members, any controls in the model enter at those members'
#' values -- profiles that differ in control composition carry that
#' difference into their predictions.
#'
#' @param model        a fitted pscl::zeroinfl() model (methods/04)
#' @param data         data.frame of members to profile (typically the data
#'                     the model was fit to)
#' @param profile_vars columns whose combinations define the profiles
#' @return data.frame, one row per observed profile, sorted by predicted:
#'   the profile_vars, `predicted` (mean expected count over the profile's
#'   observed members), `conf.low` / `conf.high` (a delta-method 95%
#'   confidence interval for that profile mean, from
#'   marginaleffects::avg_predictions), `n` (members holding the profile
#'   whose outcome is known, the members the model was fit on), and
#'   `n_unknown` (members holding the profile whose outcome is NA: they
#'   enter the prediction with their covariates but never the fit)
predicted_profiles <- function(model, data, profile_vars) {
  missing_cols <- setdiff(profile_vars, names(data))
  if (length(missing_cols) > 0) {
    stop("columns not found in data: ", paste(missing_cols, collapse = ", "))
  }

  # avg_predictions averages member-level predictions within each profile
  # and propagates the uncertainty by the delta method, so the interval
  # belongs to the profile mean (not an average of member intervals)
  out <- as.data.frame(marginaleffects::avg_predictions(
    model, newdata = data, by = profile_vars, type = "response"))
  out <- out[, c(profile_vars,
                 intersect(c("estimate", "conf.low", "conf.high"), names(out))),
             drop = FALSE]
  names(out)[names(out) == "estimate"] <- "predicted"

  out <- merge(out, occupancy_counts(model, data, profile_vars),
               by = profile_vars, sort = FALSE)

  out <- out[order(-out$predicted), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Members per profile, split by whether the model's outcome is known.
#'
#' `n` counts the members the model was fit on (outcome not NA);
#' `n_unknown` counts members holding the profile whose outcome is NA.
#' Used by predicted_profiles(), predict_positions(), and
#' collapse_to_cube(), so every saved `n` means the same thing.
#'
#' @param model a fitted model whose response names the outcome column
#' @param data  the members, with the profile columns
#' @param by    the profile columns
#' @return data.frame: the profile columns (as character), `n`, `n_unknown`
occupancy_counts <- function(model, data, by) {
  outcome <- all.vars(stats::terms(model))[1]
  unknown <- if (outcome %in% names(data)) is.na(data[[outcome]]) else
    rep(FALSE, nrow(data))
  keys <- data[, by, drop = FALSE]
  for (b in by) keys[[b]] <- as.character(keys[[b]])
  counts <- stats::aggregate(list(n = as.numeric(!unknown),
                                  n_unknown = as.numeric(unknown)),
                             by = keys, FUN = sum)
  counts$n <- as.integer(counts$n)
  counts$n_unknown <- as.integer(counts$n_unknown)
  counts
}

#' Stop before a fit that cannot be estimated.
#'
#' The recognition models carry the pairwise intersections of the
#' ascriptive statuses. If no member with a known outcome holds some
#' pair of levels, the intersection term for that cell is collinear with
#' the main effects and pscl::zeroinfl() fails inside optim with no hint
#' of the cause. This check names the empty cell and the remedies first.
#'
#' @param data       the members, before the fit
#' @param ascriptive the ascriptive status columns
#' @param outcome    the status-position count about to be modelled
#' @return invisibly TRUE; stops with a message otherwise
check_ascriptive_cells <- function(data, ascriptive, outcome) {
  if (length(ascriptive) < 2) return(invisible(TRUE))
  known <- data[!is.na(data[[outcome]]), , drop = FALSE]
  empty <- character(0)
  for (pair in utils::combn(ascriptive, 2, simplify = FALSE)) {
    lev <- lapply(pair, function(a) levels(factor(data[[a]])))
    counts <- table(factor(as.character(known[[pair[1]]]), levels = lev[[1]]),
                    factor(as.character(known[[pair[2]]]), levels = lev[[2]]))
    idx <- which(counts == 0, arr.ind = TRUE)
    if (nrow(idx) > 0) {
      empty <- c(empty, paste(lev[[1]][idx[, 1]], lev[[2]][idx[, 2]], sep = " x "))
    }
  }
  if (length(empty) > 0) {
    stop(sprintf(paste0(
      "no member with a known %s count holds %s: %s. The pairwise ",
      "intersection of the ascriptive statuses cannot be estimated for an ",
      "empty cell, and zeroinfl() would fail inside optim. The script can ",
      "run without the intersection terms: set `intersections <- ",
      "character(0)` in this script."),
      outcome, if (length(empty) == 1) "this cell" else "these cells",
      paste(empty, collapse = "; ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' Check a fitted recognition model before its results are used.
#'
#' pscl::zeroinfl() can report convergence and still return a model whose
#' standard errors cannot be computed. The check stops in that case,
#' before anything is saved, and names the parameters the data cannot
#' pin down, the ascriptive-status cells in which the outcome does not
#' vary, and three remedies. It notes, and continues past, two weaker
#' signs of thin data: a coefficient with a very large standard error,
#' and a negative binomial dispersion at its Poisson limit.
#'
#' @param model      a fitted pscl::zeroinfl() model
#' @param outcome    the status-position count it models
#' @param data       the members it was fit on
#' @param ascriptive the ascriptive status columns
#' @param label      optional name of the model, for the messages
#' @param se_limit   the standard error above which a coefficient is noted
#' @return invisibly TRUE; stops in case 1
check_recognition_fit <- function(model, outcome, data, ascriptive, label = NULL,
                                  se_limit = 50) {
  which_model <- if (is.null(label)) sprintf("the recognition model for `%s`", outcome)
                 else sprintf("the \"%s\" model for `%s`", label, outcome)
  v <- tryCatch(suppressWarnings(stats::vcov(model)), error = function(e) NULL)
  usable <- !is.null(v) && all(is.finite(diag(v))) && all(diag(v) >= 0)

  # the ascriptive-status cells in which the outcome does not vary
  known <- data[!is.na(data[[outcome]]), , drop = FALSE]
  cells <- character(0)
  if (length(ascriptive) > 0 && nrow(known) > 0) {
    key <- interaction(known[ascriptive], sep = " x ", drop = TRUE)
    n_cell <- table(key)
    z_cell <- tapply(known[[outcome]] == 0, key, sum)
    bad <- names(n_cell)[z_cell == 0 | z_cell == n_cell]
    cells <- sprintf("%s (%d of %d zero)", bad, z_cell[bad], n_cell[bad])
  }
  cell_text <- if (length(cells) > 0) {
    paste0("Ascriptive-status cells in which the outcome does not vary (every ",
           "count zero, or none): ", paste(cells, collapse = "; "), ".")
  } else {
    paste0("The outcome varies in every ascriptive-status cell; the thin level ",
           "may sit in a control or a local type.")
  }
  remedies <- sprintf(paste0(
    "Remedies: give the zero-inflation part only an intercept (formula ",
    "`%s ~ predictors | 1`), drop the intersection terms (`intersections <- ",
    "character(0)` in this script), or collapse sparse levels."), outcome)

  if (!usable) {
    nm <- names(stats::coef(model))
    H  <- model$optim$hessian
    involved <- character(0)
    if (!is.null(H) && all(is.finite(H))) {
      if (length(nm) < ncol(H)) nm <- c(nm, "log(theta)")   # negbin: the dispersion row
      e <- eigen(H, symmetric = TRUE)
      weak <- which(abs(e$values) < 1e-6 * max(abs(e$values)))
      for (k in weak) involved <- c(involved, nm[abs(e$vectors[, k]) > 0.3])
    }
    involved <- unique(involved)
    shown <- paste(utils::head(involved, 8), collapse = ", ")
    if (length(involved) > 8) shown <- paste0(shown, ", and ", length(involved) - 8, " more")
    stop(sprintf(paste0(
      "%s: its standard errors cannot be computed, so it has no confidence ",
      "intervals and the steps that follow cannot run. Parameters the data ",
      "cannot pin down: %s. %s %s"),
      which_model,
      if (nzchar(shown)) shown else "(not recoverable from the Hessian)",
      cell_text, remedies), call. = FALSE)
  }

  se <- sqrt(diag(v))
  names(se) <- names(stats::coef(model))[seq_along(se)]
  big <- se[is.finite(se) & se > se_limit]
  if (length(big) > 0) {
    message(sprintf(paste0(
      "NOTE: %s has %d coefficient(s) the data cannot pin down (standard error ",
      "above %g: %s). Read their exponentiated values in the tables as limits, ",
      "not estimates; the predicted positions stay usable. %s"),
      which_model, length(big), se_limit,
      paste(sprintf("%s (SE %.0f)", names(big), big), collapse = ", "),
      remedies))
  }

  if (identical(model$dist, "negbin") && !is.null(model$theta)) {
    se_lt <- model$SE.logtheta
    if (model$theta > 1e4 || is.null(se_lt) || !is.finite(se_lt) || se_lt > 5) {
      message(sprintf(paste0(
        "NOTE: %s was fit as a negative binomial, but its dispersion ran to the ",
        "Poisson limit (theta = %.3g). The counts show no extra spread beyond the ",
        "zero inflation; count_model = \"poisson\" in import-data.R fits the same ",
        "model with one parameter fewer."),
        which_model, model$theta))
    }
  }
  invisible(TRUE)
}

#' The article's three-model comparison for one status position.
#'
#' For each status position the article compares three zero-inflated
#' models (Tables D3-D5): ascriptive statuses with their intersections,
#' the local types alone, and both combined, each with the same controls.
#' Read across a row to see how a coefficient changes when the other
#' dimension is added; read the fit rows to see what each dimension adds.
#' This function lays fitted models out in that shape: exponentiated
#' coefficients (odds ratios in the zero block, incidence rate ratios in
#' the count block) with standard errors and significance stars, the zero
#' block above the count block, and modelsummary's fit rows beneath.
#'
#' @param models named list of fitted zeroinfl models, in column order --
#'               typically list("Ascrip." = ..., "Local Type" = ...,
#'               "Combined" = ...)
#' @param term_order optional character vector of variable names giving
#'               the row order within each block (intercept first, then
#'               these, then anything else); default: modelsummary's own
#'               order, the terms as the models introduce them
#' @param digits decimal places for estimates and standard errors
#' @return data.frame: `term` (e.g. "race Black (Zero)"), one column per
#'   model, then the fit rows (Num.Obs., AIC, BIC, ...) with `term` naming
#'   the statistic. Stars: + p < 0.1, * 0.05, ** 0.01, *** 0.001.
recognition_model_table <- function(models, term_order = NULL, digits = 3) {
  stopifnot(is.list(models), !is.null(names(models)))
  tab <- modelsummary::modelsummary(
    models, output = "data.frame",
    estimate = "{estimate} ({std.error}){stars}", statistic = NULL,
    exponentiate = TRUE, fmt = digits)
  est <- tab[tab$part == "estimates", , drop = FALSE]
  gof <- tab[tab$part == "gof", , drop = FALSE]

  block <- ifelse(grepl("^zero_", est$term), "Zero", "Count")
  raw   <- sub("^(count|zero)_", "", est$term)

  # variable names, longest first, so "grade_level" is matched before "grade"
  vars <- unique(unlist(lapply(models, function(m) {
    all.vars(stats::delete.response(stats::terms(m)))
  })))
  vars <- vars[order(-nchar(vars))]
  var_of <- function(p) {           # the variable a coefficient name starts with
    hit <- vars[startsWith(p, vars)]
    if (length(hit)) hit[1] else NA_character_
  }
  label_one <- function(x) {
    if (x == "(Intercept)") return("Intercept")
    parts <- strsplit(x, " \u00d7 ", fixed = TRUE)[[1]]
    parts <- vapply(parts, function(p) {
      v <- var_of(p)
      if (is.na(v)) return(p)
      lev <- substring(p, nchar(v) + 1)
      if (nzchar(lev)) paste(v, lev) else v
    }, character(1))
    paste(parts, collapse = " \u00d7 ")
  }
  label <- vapply(raw, label_one, character(1))

  # row order within a block: intercept, then term_order (main terms
  # before interactions), then anything else in modelsummary's order
  if (is.null(term_order)) term_order <- character(0)
  rank <- vapply(raw, function(x) {
    if (x == "(Intercept)") return(0)
    parts <- strsplit(x, " \u00d7 ", fixed = TRUE)[[1]]
    pos <- match(vapply(parts, var_of, character(1)), term_order,
                 nomatch = length(term_order) + 1)
    max(pos) + (length(parts) > 1) * 0.5   # an interaction follows its last main term
  }, numeric(1))
  o <- order(match(block, c("Zero", "Count")), rank, seq_along(raw))
  est <- est[o, , drop = FALSE]
  est$term <- paste0(label[o], " (", block[o], ")")

  keep <- setdiff(names(tab), c("part", "statistic"))
  out <- rbind(est[, keep, drop = FALSE], gof[, keep, drop = FALSE])
  rownames(out) <- NULL
  out
}

#' Who meets the ideal's local criteria but goes unrecognized?
#'
#' The article's diagnostic step after Figure 9: "if studiousness were
#' rewarded consistently, teachers would have recognized twenty additional
#' Black, Latinx, Biracial, and Asian students who are studious local
#' types as 'respected.'" This function produces that count for any run.
#' It takes the members who share the ideal actor's local type and, for
#' each ascriptive profile among them, compares how many were recognized
#' (one nomination or more) with how many would be if their type were
#' rewarded at the ideal's rate. The table reports two rates: the
#' hierarchy's own model with each member's ascriptive statuses set to
#' the ideal's (controls and posteriors kept at their real values; the
#' sum of P(Y >= 1)), and, beside it, the ideal cell's observed share of
#' recognized members applied to the profile's n. The difference between
#' the model rate and the observed count is the "additional" members; the
#' Total row sums it over every profile except the ideal's own.
#'
#' @param model   the hierarchy's recognition model
#' @param members the members it was fit on, with `local_type`
#' @param ideal   a choose_ideal() return, or a named list of levels
#' @param outcome the status-position count the hierarchy was built on
#'                (default: the model's response)
#' Members whose outcome is unknown (NA) cannot be counted as recognized
#' or not; they are left out of every count and reported in `n_unknown`,
#' with a message. Nothing is imputed.
#'
#' @return data.frame, one row per ascriptive profile among members of the
#'   ideal's local type, plus a Total row: the ascriptive columns,
#'   `n` (members with a known outcome), `n_unknown` (members left out
#'   because the outcome is NA), `recognized` (observed, >= 1 nomination),
#'   `at_ideal_rate` (the model's expected recognized with ascriptive
#'   statuses set to the ideal's), `at_ideal_share` (the ideal cell's
#'   observed share times n), `additional` (at_ideal_rate - recognized;
#'   NA for the ideal's own row)
unrecognized_members <- function(model, members, ideal, outcome = NULL) {
  if (inherits(ideal, "oc_ideal")) ideal <- ideal$full
  if (is.null(names(ideal)) || !"local_type" %in% names(ideal)) {
    stop("`ideal` must name a level for each dimension, local_type included")
  }
  if (is.null(outcome)) outcome <- all.vars(stats::terms(model))[1]
  ascriptive <- setdiff(names(ideal), "local_type")

  same <- members[as.character(members$local_type) == ideal$local_type, ,
                  drop = FALSE]
  if (nrow(same) == 0) stop("no member holds the ideal's local type")
  # the analysis sample: members whose outcome is known
  unknown <- is.na(same[[outcome]])
  key_all <- same[, ascriptive, drop = FALSE]
  for (a in ascriptive) key_all[[a]] <- as.character(key_all[[a]])
  n_unknown <- stats::aggregate(list(n_unknown = as.numeric(unknown)),
                                by = key_all, FUN = sum)
  if (any(unknown)) {
    message(sum(unknown), " member(s) of the ideal's local type have an ",
            "unknown ", outcome, " count and are left out of every count ",
            "(n_unknown); nothing is imputed")
  }
  same <- same[!unknown, , drop = FALSE]
  if (nrow(same) == 0) stop("every member of the ideal's local type has an ",
                            "unknown ", outcome, " count")
  y <- same[[outcome]]

  # the ideal's rate: the same members with the ideal's ascriptive statuses
  cf <- same
  for (a in ascriptive) {
    cf[[a]] <- factor(ideal[[a]], levels = levels(factor(members[[a]])))
  }
  p_rec <- 1 - predict(model, newdata = cf, type = "prob", at = 0:1)[, 1]

  is_ideal <- Reduce(`&`, lapply(ascriptive, function(a) {
    as.character(same[[a]]) == ideal[[a]]
  }))
  ideal_share <- if (any(is_ideal)) mean(y[is_ideal] >= 1) else NA_real_

  key <- same[, ascriptive, drop = FALSE]
  for (a in ascriptive) key[[a]] <- as.character(key[[a]])
  out <- stats::aggregate(
    list(n = rep(1, nrow(same)), recognized = as.numeric(y >= 1),
         at_ideal_rate = p_rec),
    by = key, FUN = sum)
  out$n <- as.integer(out$n)
  out$recognized <- as.integer(out$recognized)
  # every profile among the ideal's type keeps its row, including one
  # whose members all have an unknown outcome: it shows n = 0 and its
  # n_unknown, and nothing else, so the Total's n_unknown counts everyone
  out <- merge(out, n_unknown, by = ascriptive, all = TRUE, sort = FALSE)
  out$n_unknown <- as.integer(out$n_unknown)
  out$n[is.na(out$n)] <- 0L
  out <- out[, c(ascriptive, "n", "n_unknown", "recognized", "at_ideal_rate"),
             drop = FALSE]
  out$at_ideal_share <- ifelse(out$n > 0, out$n * ideal_share, NA_real_)
  row_ideal <- Reduce(`&`, lapply(ascriptive, function(a) out[[a]] == ideal[[a]]))
  out$additional <- ifelse(row_ideal | out$n == 0, NA_real_,
                           out$at_ideal_rate - out$recognized)
  out <- out[order(!row_ideal, -out$n), , drop = FALSE]   # the ideal's row first

  total <- out[1, , drop = FALSE]
  for (a in ascriptive) total[[a]] <- if (a == ascriptive[1]) "Total" else ""
  total$n <- sum(out$n)
  total$n_unknown <- sum(out$n_unknown)
  total$recognized <- sum(out$recognized, na.rm = TRUE)
  total$at_ideal_rate <- sum(out$at_ideal_rate, na.rm = TRUE)
  total$at_ideal_share <- sum(out$at_ideal_share, na.rm = TRUE)
  total$additional <- sum(out$additional, na.rm = TRUE)
  out <- rbind(out, total)
  rownames(out) <- NULL
  attr(out, "ideal") <- ideal
  attr(out, "outcome") <- outcome
  out
}
