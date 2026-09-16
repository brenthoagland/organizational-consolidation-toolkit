# latent_types.R
#
# Latent class analysis of the local criteria (the article's local types),
# built on `poLCA`, and the run of record that carries one fit to
# methods/04 and methods/05 so the classes are never silently renumbered.
#
#   lca_fit_stats()          the class-count sweep (Table C1's columns)
#   as_table_c1()            the sweep with Table C1's headers
#   plot_class_count_sweep() the sweep as a figure
#   blrt()                   bootstrap likelihood ratio test for k vs k+1
#   fit_local_types()        the poLCA call methods/03 makes, for the sweep and the bootstrap
#   classification_table()   Table C2: average posterior by modal class
#   classification_error_matrix()  Table C3: P(assigned | true)
#   draw_type_mosaic()       Figure 8: local types by an ascriptive status
#   save_local_types()       write the run of record
#   oc_local_types()         read the run of record (or refit, if allowed)
#   label_local_types()      display labels for the classes
#
# Class labels are exchangeable across runs; methods/03 shows relabeling
# with poLCA::poLCA.reorder.

# poLCA requires every manifest variable coded as positive integers 1..K.
# Factors keep their level order; numerics compress to consecutive codes
# (so 10/20/30 becomes 1/2/3); NAs stay NA.
as_lca_codes <- function(x) {
  if (is.factor(x)) return(as.integer(x))
  if (is.logical(x)) return(as.integer(x) + 1L)
  if (is.character(x)) return(as.integer(factor(x)))
  if (is.numeric(x)) {
    u <- sort(unique(x[!is.na(x)]))
    return(match(x, u))
  }
  stop("cannot code a variable of class ", paste(class(x), collapse = "/"),
       " for poLCA")
}

# The original response labels, in the same code order as_lca_codes()
# produces, so poLCA's Pr(1), Pr(2), ... columns can be read back as the
# levels they came from (A, B, ...; q1, q2, ...; FALSE, TRUE).
lca_response_levels <- function(x) {
  if (is.factor(x))    return(levels(x))
  if (is.logical(x))   return(c("FALSE", "TRUE"))
  if (is.character(x)) return(levels(factor(x)))
  if (is.numeric(x))   return(as.character(sort(unique(x[!is.na(x)]))))
  stop("cannot label a variable of class ", paste(class(x), collapse = "/"))
}

# One class means the criteria draw no distinguishable difference between
# members: no local types separate out. That is a finding, not an error,
# but it leaves the type-based steps nothing to work with. The scripts
# that need types share this message.
one_class_message <- paste0(
  "One local type: on these criteria members show no distinguishable ",
  "difference -- no separate local types emerge. That single class is ",
  "a finding, not an error, but it leaves nothing for the ",
  "type-based steps to work with: there are no types to cross with ",
  "ascriptive status (methods/03), enter as regression terms ",
  "(methods/04), or order in a hierarchy (methods/05). methods/01 still ",
  "describes how each criterion aligns with ascriptive status. Set ",
  "n_types >= 2 in import-data.R to run the type-based steps.")

# entropy.R2() is reproduced from Daniel Oberski's poLCA entropy-R^2
# code (gist.github.com/daob/c2b6d83815ddd57cde3cebfdc2c267b3, the
# revision that drops zero posterior probabilities before taking logs:
# p*log(p) tends to 0 as p does, but 0*log(0) is NaN in R). Citable
# treatment:
# Oberski, Daniel. 2016. "Mixture Models: Latent Profile and Latent
# Class Analysis." Pp. 275-287 in Modern Statistical Methods for HCI,
# edited by Judy Robertson and Maurits Kaptein. Cham, Switzerland:
# Springer. (Also cited in the article's Appendix C.)
machine_tolerance <- sqrt(.Machine$double.eps)
entropy.R2 <- function(fit) {
  entropy <- function(p) {
    p <- p[p > machine_tolerance] # since Lim_{p->0} p log(p) = 0
    sum(-p * log(p))
  }
  error_prior <- entropy(fit$P) # Class proportions
  error_post <- mean(apply(fit$posterior, 1, entropy))
  R2_entropy <- (error_prior - error_post) / error_prior
  R2_entropy
}

#' Fit a latent class model of local types.
#'
#' The poLCA call methods/03 makes in the open, for the places that
#' repeat it at other class counts: the sweep (lca_fit_stats()), the
#' bootstrap test (blrt()), and the refit path of oc_local_types().
#' Codes the indicator columns 1..K, fits `poLCA`, and returns the fit
#' augmented with the membership objects downstream sections use:
#'   $assignment  modal class per member, as a factor, so regressions
#'                treat classes as nominal categories (the underlying
#'                codes equal $predclass)
#'   $posterior   member x class posterior probabilities (poLCA's own)
#'   $entropy_r2  entropy-based R^2 (how separated the classes are)
#'   $class_share modal class-size proportions ($P holds the model's mixing
#'                proportions)
#' BIC and AIC are already on the poLCA fit as $bic / $aic.
#'
#' @param data data.frame per data/data-dictionary.md
#' @param indicators local criteria columns to fit the classes on
#' @param nclass number of latent classes
#' @param nrep independent starts; keep >= 10 for a final model to reduce
#'   the risk of reporting a local maximum (more starts help; none guarantee
#'   the global one)
#' @param na_rm drop rows with missing indicators (FALSE keeps them and
#'   estimates from the observed values, poLCA's `na.rm`)
#' @param probs_start optional starting values, e.g. from poLCA.reorder()
fit_local_types <- function(data, indicators, nclass = 4, nrep = 10,
                            maxiter = 5000, tol = 1e-10, na_rm = FALSE,
                            probs_start = NULL, verbose = FALSE) {
  missing_cols <- setdiff(indicators, names(data))
  if (length(missing_cols) > 0) {
    stop("columns not found in data: ", paste(missing_cols, collapse = ", "))
  }

  manifest <- as.data.frame(lapply(data[indicators], as_lca_codes))

  f <- stats::as.formula(
    paste("cbind(", paste(indicators, collapse = ", "), ") ~ 1"))

  fit <- poLCA::poLCA(f, manifest, nclass = nclass, maxiter = maxiter,
                      graphs = FALSE, tol = tol, na.rm = na_rm,
                      probs.start = probs_start, nrep = nrep,
                      verbose = verbose, calc.se = TRUE)

  fit$assignment <- factor(fit$predclass, levels = seq_len(nclass))
  fit$entropy_r2 <- if (nclass > 1) entropy.R2(fit) else NA_real_
  fit$class_share <- as.vector(
    prop.table(table(factor(fit$predclass, levels = seq_len(nclass)))))
  fit$indicators <- indicators
  fit
}

#' Persist / reuse section 03's local types.
#'
#' Class labels are exchangeable and EM can stop at different maxima, so
#' sections 04 and 05 should consume one run of record rather than refit.
#' save_local_types() writes it (including the posterior, for proportional
#' assignment); load_local_types() returns the saved assignment when it
#' matches the data -- same members, same class count, (when `indicators`
#' is supplied) the same criterion columns, and the same criterion values
#' -- and NULL otherwise. The value check matters: two datasets can share
#' an id scheme and column names while holding different people's answers,
#' and reusing the run of record across them silently assigns the old
#' data's classes to the new data.
local_types_content_hash <- function(data, indicators) {
  rlang::hash(lapply(data[indicators], as.character))
}

save_local_types <- function(fit, data, id,
                             path = oc_output_file("local_types.rds")) {
  saveRDS(list(assignment = fit$assignment,
               posterior = fit$posterior,
               nclass = length(fit$P),
               P = fit$P,
               probs = fit$probs,
               indicators = fit$indicators,
               member_id = data[[id]],
               content_hash = local_types_content_hash(data, fit$indicators)),
          path)
  invisible(path)
}

#' Full run-of-record load: the whole saved record (assignment, posterior,
#' mixing proportions P, item-response profiles probs, ...) when it matches
#' the data, NULL otherwise. load_local_types() is the assignment-only view.
load_local_types_record <- function(data, id, nclass, indicators = NULL,
                                    path = oc_output_file("local_types.rds")) {
  if (!file.exists(path)) return(NULL)
  lt <- readRDS(path)
  if (!identical(as.character(lt$member_id), as.character(data[[id]]))) return(NULL)
  if (!identical(as.integer(lt$nclass), as.integer(nclass))) return(NULL)
  if (!is.null(indicators) && !is.null(lt$indicators) &&
      !identical(as.character(lt$indicators), as.character(indicators))) {
    return(NULL)   # criteria changed since the run of record: don't reuse it
  }
  if (!is.null(lt$content_hash)) {
    hash_cols <- if (!is.null(lt$indicators)) as.character(lt$indicators)
                 else indicators
    if (!is.null(hash_cols) && all(hash_cols %in% names(data)) &&
        !identical(lt$content_hash,
                   local_types_content_hash(data, hash_cols))) {
      return(NULL) # same ids, different answers: this is not that run's data
    }
  } else {
    message("run of record predates content hashing -- re-run methods/03 ",
            "to refresh it")
  }
  lt
}

load_local_types <- function(data, id, nclass, indicators = NULL,
                             path = oc_output_file("local_types.rds")) {
  lt <- load_local_types_record(data, id, nclass, indicators, path)
  if (is.null(lt)) NULL else lt$assignment
}

#' Display labels for anonymous classes.
#'
#' Classes are anonymous until the analyst names them: each class displays
#' as "type <k>" unless `type_labels` (import-data.R) supplies an interpreted
#' name (class number -> label). Returns a factor whose levels follow the
#' class order, so "type 2" and a named class sort the same way.
label_local_types <- function(assignment, type_labels = NULL) {
  lev <- levels(factor(assignment))
  lab <- if (!is.null(type_labels)) {
    m <- as.character(type_labels)[match(lev, names(type_labels))]
    ifelse(is.na(m), paste("type", lev), m)
  } else {
    paste("type", lev)
  }
  factor(lab[match(as.character(assignment), lev)], levels = lab)
}

#' The single entry point to the local types for every downstream script.
#'
#' Returns the run of record (methods/03) when it matches the data,
#' refitting (seeded) as a last resort -- so every downstream script
#' consumes the same classes, at cfg$n_types. The returned record
#' contains:
#'   $assignment  modal class per member (factor 1..k)
#'   $display     the same, as display labels (label_local_types)
#'   $labels      the display label per class, in class order
#'   $posterior, $P, $probs, $nclass  the fit pieces exhibits draw on
#' Stops by default: with no matching saved run, it stops and
#' points to methods/03 rather than silently refitting -- a quiet refit
#' can renumber classes without the analyst noticing. Pass
#' allow_refit = TRUE (a deliberate choice, not a default) to refit
#' in place with a seeded fit.
oc_local_types <- function(data, cfg = oc_config, need_probs = FALSE,
                           seed = 1234, allow_refit = FALSE,
                           path = oc_output_file("local_types.rds")) {
  rec <- load_local_types_record(data, cfg$id, cfg$n_types,
                                 indicators = cfg$criteria, path = path)
  if (!is.null(rec) && need_probs && is.null(rec$probs)) {
    message("run of record predates response-profile storage -- ",
            "re-run methods/03 to refresh it")
    rec <- NULL
  }
  if (is.null(rec)) {
    if (!allow_refit) {
      stop("no saved local types match this data. Run methods/03 first ",
           "to create the run of record (it persists the classes every ",
           "later script consumes), or pass allow_refit = TRUE to refit ",
           "here -- knowing a refit can renumber the classes.",
           call. = FALSE)
    }
    message("no saved local types match this data -- refitting ",
            "(allow_refit = TRUE). Class numbering belongs to THIS fit.")
    set.seed(seed)
    fit <- fit_local_types(data, cfg$criteria, nclass = cfg$n_types,
                           nrep = 10)
    rec <- list(assignment = fit$assignment, posterior = fit$posterior,
                nclass = length(fit$P), P = fit$P, probs = fit$probs,
                indicators = fit$indicators,
                content_hash = local_types_content_hash(data, fit$indicators))
  }
  rec$display <- label_local_types(rec$assignment, cfg$type_labels)
  rec$labels <- levels(rec$display)
  rec
}

#' Table C2: average posterior probabilities by modal class.
#'
#' Rows are the modal (assigned) classes; each row holds the posterior
#' probability of every class averaged over the members assigned to the
#' row's class, so rows sum to 1. The diagonal is the model's confidence
#' in its own assignments. 1 - sum(diag(T)) / sum(T) weights every class
#' equally regardless of size; the member-weighted classification error
#' methods/03 prints is a different quantity.
classification_table <- function(fit) {
  k <- length(fit$P)
  post <- fit$posterior
  tab <- t(vapply(seq_len(k),
                  function(j) colMeans(post[fit$predclass == j, , drop = FALSE]),
                  numeric(k)))
  dimnames(tab) <- list(assigned = seq_len(k), posterior = seq_len(k))
  tab
}

#' Table C3: the classification error matrix, P(assigned | true).
#'
#' Columns are the true (latent) classes, rows the modal assignments.
#' Each column shares out a class's posterior mass by where its members
#' were assigned: entry (j, k) is the sum of class k's posterior over the
#' members assigned to class j, divided by class k's total posterior, so
#' columns sum to 1 and the diagonal is the share of each true class that
#' modal assignment recovers.
classification_error_matrix <- function(fit) {
  k <- length(fit$P)
  post <- fit$posterior
  tab <- vapply(seq_len(k), function(true) {
    mass <- vapply(seq_len(k), function(assigned) {
      sum(post[fit$predclass == assigned, true])
    }, numeric(1))
    mass / sum(post[, true])
  }, numeric(k))
  dimnames(tab) <- list(assigned = seq_len(k), true = seq_len(k))
  tab
}

#' Bootstrap likelihood ratio test (BLRT) for adjacent class counts.
#'
#' Does the k1-class model fit significantly better than the k0-class model?
#' A supplementary check for class selection, alongside the AIC, BIC,
#' classification error, and interpretability that the article's
#' Appendix C weighs.
#' Parametric bootstrap (McLachlan & Peel 2000): simulate datasets from the
#' fitted k0 model (the null), refit both models on each, and compare the
#' observed LR statistic against that null distribution. Resampling the
#' observed members instead would draw from the alternative whenever k1
#' structure is present, miscalibrating the p-value. Slow at
#' num_bootstrap = 1000, the usual convention; use a smaller value while
#' exploring. boot_nrep = 1 keeps each refit fast at the
#' cost of some local-maximum noise in the null draws.
#' Returns lr_stat, p_value, and the bootstrap statistics.
blrt <- function(data, indicators, k0, k1 = k0 + 1, num_bootstrap = 1000,
                 nrep = 10, boot_nrep = 1, maxiter = 5000, tol = 1e-10,
                 na_rm = FALSE) {
  fit0 <- fit_local_types(data, indicators, nclass = k0, nrep = nrep,
                          maxiter = maxiter, tol = tol, na_rm = na_rm)
  fit1 <- fit_local_types(data, indicators, nclass = k1, nrep = nrep,
                          maxiter = maxiter, tol = tol, na_rm = na_rm)
  lr_stat <- -2 * (fit0$llik - fit1$llik)

  # simulate one dataset from the fitted k0 model: class from the mixing
  # proportions, then each indicator from that class's response profile
  simulate_from <- function(fit, n) {
    cls <- sample(seq_along(fit$P), n, replace = TRUE, prob = fit$P)
    sim <- lapply(indicators, function(v) {
      pr <- fit$probs[[v]]
      vapply(cls, function(cl)
        sample(seq_len(ncol(pr)), 1, prob = pr[cl, ]), integer(1))
    })
    names(sim) <- indicators
    as.data.frame(sim)
  }

  boot_stats <- numeric(num_bootstrap)
  for (i in seq_len(num_bootstrap)) {
    null_sample <- simulate_from(fit0, nrow(data))
    b0 <- fit_local_types(null_sample, indicators, nclass = k0,
                          nrep = boot_nrep, maxiter = maxiter, tol = tol)
    b1 <- fit_local_types(null_sample, indicators, nclass = k1,
                          nrep = boot_nrep, maxiter = maxiter, tol = tol)
    boot_stats[i] <- -2 * (b0$llik - b1$llik)
  }

  list(k0 = k0, k1 = k1,
       lr_stat = lr_stat,
       # add-one so a p-value of exactly 0 (impossible) cannot be reported
       p_value = (sum(boot_stats >= lr_stat) + 1) / (num_bootstrap + 1),
       boot_stats = boot_stats)
}

#' Fit statistics across a range of class counts (for model selection).
#'
#' Refits the model at each k and collects the statistics the class-selection
#' figure and goodness-of-fit table report. Each k gets nrep independent EM
#' starts, the same 10 as the final fit; the start with the highest
#' log-likelihood is kept.
lca_fit_stats <- function(data, indicators, k_range = 1:10, nrep = 10,
                          maxiter = 5000, tol = 1e-10, na_rm = FALSE) {
  rows <- lapply(k_range, function(k) {
    fit <- fit_local_types(data, indicators, nclass = k, nrep = nrep,
                           maxiter = maxiter, tol = tol, na_rm = na_rm)
    data.frame(nclass = k,
               llik = fit$llik,
               npar = fit$npar,
               resid_df = fit$resid.df,
               aic = fit$aic,
               bic = fit$bic,
               Gsq = fit$Gsq,               # the article's ratio.G2
               Chisq = fit$Chisq,
               entropy_r2 = fit$entropy_r2,
               # member-weighted modal classification error (Table C1's
               # Error): the mean chance a member is not in its modal class
               class_error = mean(1 - apply(fit$posterior, 1, max)),
               # smallest modal class as a share (Table C1's Smallest Class):
               # a tiny class warns of an unstable, hard-to-name type
               smallest_class = min(fit$class_share))
  })
  do.call(rbind, rows)
}

# Present a lca_fit_stats() sweep in the shape of the article's Table C1
# (Appendix C), so a reader can hold the toolkit's output beside the paper.
as_table_c1 <- function(fit_stats) {
  out <- fit_stats[, c("nclass", "llik", "resid_df", "bic", "aic",
                       "Gsq", "entropy_r2", "class_error", "smallest_class")]
  names(out) <- c("N Classes", "Log-likelihood", "Resid.df", "BIC", "AIC",
                  "ratio.G2", "Entropy.R2", "Error", "Smallest Class")
  out
}

#' The class-count sweep as a figure: AIC and BIC against the number of
#' classes, from lca_fit_stats(). The elbow you point at when justifying
#' your class count.
plot_class_count_sweep <- function(fit_stats) {
  sweep <- rbind(
    data.frame(nclass = fit_stats$nclass, criterion = "AIC",
               value = fit_stats$aic),
    data.frame(nclass = fit_stats$nclass, criterion = "BIC",
               value = fit_stats$bic))
  ggplot2::ggplot(sweep, ggplot2::aes(nclass, value, color = criterion)) +
    ggplot2::geom_line(linewidth = 0.6) + ggplot2::geom_point(size = 2) +
    ggplot2::scale_x_continuous(breaks = fit_stats$nclass) +
    ggplot2::labs(x = "Number of classes", y = "Information criterion",
                  color = NULL, title = "Class-count sweep") +
    theme_oc(base_size = 12)
}

#' The article's Figure 8 mosaic for one ascriptive status: tile areas
#' show the joint distribution of local types and the status. The
#' shading is the article's specification: shading_max sets its cutoffs
#' by a permutation test on the largest Pearson residual (levels 0.9 and
#' 0.99; `seed` fixes that draw), and c = c(40, 0) is chroma, muted
#' color for cells that pass a cutoff and gray for the rest. Draws on the
#' current device (wrap in pdf()/dev.off() to save) and returns the
#' table invisibly.
draw_type_mosaic <- function(data, ascriptive, cfg = oc_config, seed = 1234) {
  keep <- !is.na(data$local_type) & !is.na(data[[ascriptive]])
  tab <- table(local_type = paste0("t", data$local_type[keep]),
               data[[ascriptive]][keep])
  names(dimnames(tab))[2] <- ascriptive
  set.seed(seed)
  print(vcd::mosaic(tab, direction = c("v", "h"), gp = vcd::shading_max,
                    gp_args = list(c = c(40, 0)),
                    legend = vcd::legend_resbased(pvalue = FALSE)))
  if (oc_synthetic_run(cfg)) {
    grid::grid.text(
      "Synthetic data illustration -- not the article's results",
      y = grid::unit(4, "pt"), just = "bottom",
      gp = grid::gpar(cex = 0.55, col = "grey40"))
  }
  invisible(tab)
}
