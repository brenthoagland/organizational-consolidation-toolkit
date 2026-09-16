# status_hierarchy_steps.R
#
# The steps of the article's Figure 9 construction. methods/05 calls
# them in order, after fitting the recognition model on the members:
#
#   predict_positions()  every intersectional position valued by prediction
#   choose_ideal()       the ideal actor among the occupied positions
#   collapse_to_cube()   the positions reduced to the ideal-vs-rest cube
#
# status_hierarchy_poset.R draws the cube; recognition_models.R fits the
# model and runs the diagnostic step after Figure 9.
#
# Terms. A *dimension* is one axis of the hierarchy: each ascriptive
# status, plus the local type. A *position* is one combination of
# dimension levels (White, female, type 1). The *ideal actor* is the
# position with the most recognition. A *cube cell* is a position after
# every dimension is reduced to "the ideal's level" or "the rest".
# Design notes: docs/poset-design.md.

#' Predicted recognition for every position.
#'
#' For each position, the model predicts every member's recognition as
#' if they held that position (its ascriptive statuses and type), with
#' their own controls; the average over members is the position's value.
#' The type is set through `type_terms`: the posterior columns under
#' proportional assignment (the position's class at 1, the others at 0),
#' or the `local_type` factor.
#'
#' @param model      the recognition model (pscl::zeroinfl(), as methods/05 fits it)
#' @param data       the members the model was fit on, with a `local_type`
#'   factor whose level order is the class order (label_local_types())
#' @param dimensions the position's axes: the ascriptive statuses and
#'   "local_type", in the order the labels should read
#' @param type_terms the model's type terms: the posterior columns
#'   ("type_prob2", ...) or "local_type"
#' @return one row per position, highest value first: the dimension
#'   columns, `predicted`, `conf.low`, `conf.high` (delta-method
#'   interval), and `n`, the members whose own position it is. The
#'   dimensions and the stacked member-by-position frame are attached
#'   as attributes for collapse_to_cube().
predict_positions <- function(model, data, dimensions, type_terms) {
  ascriptive   <- setdiff(dimensions, "local_type")
  proportional <- !identical(type_terms, "local_type")
  type_levels  <- levels(factor(data[["local_type"]]))
  if (proportional && length(type_terms) != length(type_levels) - 1L) {
    stop(sprintf("`type_terms` names %d posterior columns for %d classes; ",
                 length(type_terms), length(type_levels)),
         "class 1 is the reference, so there should be one fewer",
         call. = FALSE)
  }
  lev <- lapply(dimensions, function(d) levels(factor(data[[d]])))
  names(lev) <- dimensions
  grid <- expand.grid(lev, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  pos_cols <- paste0(".pos_", dimensions)
  pieces <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    d <- data
    for (a in ascriptive) {
      d[[a]] <- factor(grid[[a]][i], levels = levels(factor(data[[a]])))
    }
    if (proportional) {
      cls <- match(grid[["local_type"]][i], type_levels)       # class k = level k
      for (k in seq_along(type_terms)) {
        d[[type_terms[k]]] <- as.numeric((k + 1L) == cls)      # type_prob(k+1)
      }
    } else {
      d[["local_type"]] <- factor(grid[["local_type"]][i], levels = type_levels)
    }
    for (dm in dimensions) d[[paste0(".pos_", dm)]] <- grid[[dm]][i]
    pieces[[i]] <- d
  }
  cf <- do.call(rbind, pieces)

  pred <- as.data.frame(marginaleffects::avg_predictions(
    model, newdata = cf, by = pos_cols, type = "response"))
  names(pred)[match(pos_cols, names(pred))] <- dimensions
  names(pred)[names(pred) == "estimate"] <- "predicted"
  pred <- pred[, c(dimensions,
                   intersect(c("predicted", "conf.low", "conf.high"),
                             names(pred))), drop = FALSE]
  for (dm in dimensions) pred[[dm]] <- as.character(pred[[dm]])

  # n: members in the position whose outcome is known (the fit);
  # n_unknown: members in the position whose outcome is NA
  occ <- occupancy_counts(model, data, dimensions)
  pred <- merge(pred, occ, by = dimensions, all.x = TRUE, sort = FALSE)
  pred$n[is.na(pred$n)] <- 0L
  pred$n_unknown[is.na(pred$n_unknown)] <- 0L
  pred <- pred[order(-pred$predicted), , drop = FALSE]
  name_of <- function(rows) apply(rows[, dimensions, drop = FALSE], 1, paste, collapse = " / ")
  if (any(pred$n == 0)) {
    message(sum(pred$n == 0), " position(s) have no member with a known count ",
            "(predictions for positions nobody occupies): ",
            paste(name_of(pred[pred$n == 0, , drop = FALSE]), collapse = "; "))
  }
  if (any(pred$n > 0 & pred$n < 3)) {
    thin <- pred[pred$n > 0 & pred$n < 3, , drop = FALSE]
    message(nrow(thin), " position(s) have fewer than 3 members with a known ",
            "count: ", paste(name_of(thin), collapse = "; "),
            " -- read them with the small-cell caution in ",
            "docs/decisions-you-will-face.md; the cube's min_position_n shows ",
            "which cells pool them")
  }
  rownames(pred) <- NULL
  attr(pred, "dimensions") <- dimensions
  attr(pred, "counterfactual") <- cf
  pred
}

#' Choose the ideal actor.
#'
#' Among positions with at least `min_n` members, the ideal is the one
#' with the highest lower confidence bound (`rule = "conf"`) or the
#' highest predicted value (`rule = "max"`, the article's rule). When the
#' two rules disagree, a message names both candidates. `ideal` sets the
#' actor on theoretical grounds instead; the raw maximum is still
#' reported, and the drawing warns if another cell outscores it.
#'
#' @param positions a predict_positions() return
#' @param rule  "conf" (default) or "max"
#' @param ideal optional named list, dimension -> level
#' @param min_n the members a position needs to be a candidate
#' @return a list of class "oc_ideal": `$full`, the ideal as a named
#'   list of levels; `$row`, its row of `positions` (value, interval,
#'   n); `$raw_top`, the position with the highest predicted value among
#'   those with at least `min_n` members; `$top_any`, the highest on the
#'   whole grid regardless of n; `$min_n`, the cutoff;
#'   `$rule`, "conf", "max", or "supplied"
choose_ideal <- function(positions, rule = c("conf", "max"), ideal = NULL,
                         min_n = 3) {
  rule <- match.arg(rule)
  dimensions <- attr(positions, "dimensions")
  if (is.null(dimensions)) {
    stop("`positions` must come from predict_positions()", call. = FALSE)
  }
  cand <- positions[positions$n >= min_n, , drop = FALSE]
  if (nrow(cand) == 0) {
    stop("no position has n >= ", min_n, " members -- the grid is too ",
         "sparse to choose an ideal. Coarsen a dimension or revisit the ",
         "small-cell cutoff (docs/decisions-you-will-face.md).",
         call. = FALSE)
  }
  # the highest prediction among candidates (n >= min_n), and the highest
  # on the whole grid, which may sit below the cutoff
  raw_top <- cand[which.max(cand$predicted), , drop = FALSE]
  top_any <- positions[which.max(positions$predicted), , drop = FALSE]
  if (!is.null(ideal)) {
    if (!all(dimensions %in% names(ideal))) {
      stop("`ideal` must name every dimension: ",
           paste(dimensions, collapse = ", "), call. = FALSE)
    }
    full <- stats::setNames(lapply(ideal[dimensions], as.character), dimensions)
    rule <- "supplied"
  } else {
    top <- if (rule == "conf" && "conf.low" %in% names(cand)) {
      cand[which.max(cand$conf.low), , drop = FALSE]
    } else {
      raw_top
    }
    if (!identical(as.character(unlist(top[dimensions])),
                   as.character(unlist(raw_top[dimensions])))) {
      message(sprintf(
        paste0("\"%s\" has the highest prediction (%.2f) but only %d ",
               "member(s); taking \"%s\" instead (highest lower confidence ",
               "bound, n = %d). Pass ideal = ... to override, or ",
               "rule = \"max\" for the raw maximum."),
        paste(unlist(raw_top[dimensions]), collapse = " / "),
        raw_top$predicted, raw_top$n,
        paste(unlist(top[dimensions]), collapse = " / "), top$n))
    }
    full <- stats::setNames(as.list(as.character(unlist(top[dimensions]))),
                            dimensions)
  }
  row <- positions[Reduce(`&`, lapply(dimensions, function(dm) {
    as.character(positions[[dm]]) == full[[dm]]
  })), , drop = FALSE]
  structure(list(full = full, row = row, raw_top = raw_top, top_any = top_any,
                 rule = rule, min_n = min_n),
            class = "oc_ideal")
}

#' Value the ideal-vs-rest cube.
#'
#' Each dimension is reduced to the ideal's level versus the rest
#' (dichotomize_against_ideal()), and each cube cell pools the positions
#' that share its pattern. Its value is the delta-method average of the
#' predictions for those positions, taken over the stacked frame that
#' predict_positions() attached, so the interval belongs to the pooled
#' mean. `n` counts the members in the cell; a message names cells with
#' no members and cells with fewer than three.
#'
#' @param model     the recognition model
#' @param positions a predict_positions() return
#' @param data      the members, as passed to predict_positions()
#' @param ideal     a choose_ideal() return, or a named list of levels
#' @return one row per cube cell, highest value first: the cube dimension
#'   columns (factors, the ideal's level first), `predicted`, `conf.low`,
#'   `conf.high`, `n`. The cube's dimensions, their display names, and
#'   the ideal in cube terms are attached as attributes, which
#'   status_hierarchy_poset() reads.
collapse_to_cube <- function(model, positions, data, ideal) {
  dimensions <- attr(positions, "dimensions")
  cf <- attr(positions, "counterfactual")
  if (is.null(dimensions) || is.null(cf)) {
    stop("`positions` must come from predict_positions()", call. = FALSE)
  }
  ideal_full <- if (inherits(ideal, "oc_ideal")) ideal$full else ideal

  data_bin <- dichotomize_against_ideal(data, ideal_full)
  dims_bin <- attr(data_bin, "bin_dimensions")
  pos <- cf[, paste0(".pos_", dimensions), drop = FALSE]
  names(pos) <- dimensions
  pos_bin <- dichotomize_against_ideal(pos, ideal_full)
  bin_cols <- paste0(".bin_", dims_bin)
  for (i in seq_along(dims_bin)) {
    cf[[bin_cols[i]]] <- as.character(pos_bin[[dims_bin[i]]])
  }
  cells <- as.data.frame(marginaleffects::avg_predictions(
    model, newdata = cf, by = bin_cols, type = "response"))
  names(cells)[match(bin_cols, names(cells))] <- dims_bin
  names(cells)[names(cells) == "estimate"] <- "predicted"
  cells <- cells[, c(dims_bin,
                     intersect(c("predicted", "conf.low", "conf.high"),
                               names(cells))), drop = FALSE]

  occ <- occupancy_counts(model, data_bin, dims_bin)
  cells <- merge(cells, occ, by = dims_bin, all.x = TRUE, sort = FALSE)
  cells$n[is.na(cells$n)] <- 0L
  cells$n_unknown[is.na(cells$n_unknown)] <- 0L
  # the smallest n among the full positions each cell pools, so a thin
  # position does not vanish inside a well-populated cell
  pos_tab <- positions[, dimensions, drop = FALSE]
  pos_key <- dichotomize_against_ideal(pos_tab, ideal_full)[, dims_bin, drop = FALSE]
  for (b in dims_bin) pos_key[[b]] <- as.character(pos_key[[b]])
  min_n <- stats::aggregate(list(min_position_n = positions$n), by = pos_key, FUN = min)
  min_n$min_position_n <- as.integer(min_n$min_position_n)
  cells <- merge(cells, min_n, by = dims_bin, all.x = TRUE, sort = FALSE)
  for (b in dims_bin) {
    cells[[b]] <- factor(cells[[b]], levels = levels(factor(data_bin[[b]])))
  }
  cells <- cells[order(-cells$predicted), , drop = FALSE]
  rownames(cells) <- NULL

  if (any(cells$n == 0)) {
    message(sum(cells$n == 0), " cube cell(s) have no members: their ",
            "values are predictions for positions nobody occupies ",
            "(n = 0 in the cube)")
  }
  if (any(cells$n > 0 & cells$n < 3)) {
    message(sum(cells$n > 0 & cells$n < 3), " cube cell(s) have fewer ",
            "than 3 members -- read their positions with the small-cell ",
            "caution in docs/decisions-you-will-face.md")
  }
  attr(cells, "dimensions") <- dims_bin
  attr(cells, "ideal") <- stats::setNames(
    as.list(as.character(unlist(ideal_full))), dims_bin)
  attr(cells, "dimension_names") <- stats::setNames(
    ifelse(dimensions == "local_type", "Type",
           tools::toTitleCase(gsub("_", " ", dimensions))),
    dims_bin)
  cells
}
