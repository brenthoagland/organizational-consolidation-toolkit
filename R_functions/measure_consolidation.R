# measure_consolidation.R
#
# The initial assessment of organizational consolidation (the article's
# Figure 4): how tightly ascriptive statuses align with local criteria.
#
#   measure_consolidation()    pairwise associations, mutual information,
#                              total correlation
#   plot_initial_assessment()  the two-panel figure
#   cramers_v()                Cramér's V (also the type-level V, methods/03)
#   point_biserial(), eta_ratio(), mutual_information(), total_correlation()
#                              the statistics measure_consolidation() chooses among
#' Measure organizational consolidation on a member-level table
#'
#' Quantifies how tightly ascriptive statuses align with local criteria.
#' As in the article's Figure 4, each pair's association is measured with
#' the statistic matched to its levels of measurement:
#'   binary x binary                      -> phi
#'   binary x continuous/ordinal          -> point-biserial r
#'   nominal x continuous/ordinal         -> eta (correlation ratio)
#'   nominal x nominal                    -> Cramér's V
#' Ordered factors count as ordinal; a variable with two observed values
#' counts as binary. Because these statistics have different scales, all
#' values are reported as magnitudes on a 0-1 scale for comparability;
#' the `measure` column names the statistic behind each cell.
#'
#' @param data      data.frame, one row per organizational member (see data/data-dictionary.md).
#' @param ascriptive character vector of ascriptive-status columns (e.g. race, gender).
#' @param criteria   character vector of local-criteria columns.
#' @param n_permutations draws for the total-correlation permutation null.
#' @return list of class "oc_consolidation":
#'   $pairwise          one row per ascriptive-by-criterion pair (measure,
#'                      magnitude, mutual_information, n_complete). Each pair
#'                      uses its own complete cases -- with missing data,
#'                      different pairs rest on different members; check
#'                      n_complete.
#'   $by_ascriptive     mean magnitude per ascriptive status (NA if any pair is degenerate)
#'   $total_correlation multivariate dependency across all listed variables,
#'                      in nats (complete cases across all the variables)
#'   $n                 members in the table (not the per-pair sample sizes)
measure_consolidation <- function(data, ascriptive, criteria,
                                  n_permutations = 20) {
  vars <- c(ascriptive, criteria)
  missing_cols <- setdiff(vars, names(data))
  if (length(missing_cols) > 0) {
    stop("columns not found in data: ", paste(missing_cols, collapse = ", "))
  }
  # The information measures need categories. The article binned every
  # continuous indicator before analysis (quartiles, quintiles); do the
  # same before this step, with R_functions/discretize.R, rather than
  # have it happen here out of sight.
  many_valued <- vars[vapply(data[vars], function(x)
    is.numeric(x) && length(unique(x[!is.na(x)])) > 12, logical(1))]
  if (length(many_valued) > 0) {
    stop("numeric columns with more than 12 distinct values: ",
         paste(many_valued, collapse = ", "),
         ". Discretize them first (R_functions/discretize.R), as the ",
         "article did with its continuous indicators.", call. = FALSE)
  }

  pairs <- expand.grid(ascriptive = ascriptive, criterion = criteria,
                       stringsAsFactors = FALSE)
  matched <- mapply(function(a, cr) matched_association(data[[a]], data[[cr]]),
                    pairs$ascriptive, pairs$criterion, SIMPLIFY = FALSE)
  pairs$measure   <- vapply(matched, `[[`, character(1), "measure")
  pairs$magnitude <- vapply(matched, `[[`, numeric(1), "magnitude")
  pairs$mutual_information <- mapply(function(a, cr)
    mutual_information(data[[a]], data[[cr]]),
    pairs$ascriptive, pairs$criterion)
  pairs$n_complete <- mapply(function(a, cr)
    sum(!(is.na(data[[a]]) | is.na(data[[cr]]))),
    pairs$ascriptive, pairs$criterion)

  # mean() left to propagate NA so a degenerate pair stays visible
  rollup <- data.frame(
    ascriptive = ascriptive,
    mean_magnitude = vapply(ascriptive, function(a)
      mean(pairs$magnitude[pairs$ascriptive == a]), numeric(1)),
    row.names = NULL)

  # Total correlation computed straight from the sample overstates
  # dependence: with several categorical variables the joint table has
  # many empty cells, and empty cells look like structure. To measure
  # that overstatement, shuffle each column independently (each variable
  # keeps its distribution; any dependence between them is destroyed) and
  # recompute. The shuffled value is what pure sampling noise produces,
  # so the reported quantity is observed minus shuffled. Shuffling uses
  # the random number generator; set a seed to reproduce a run exactly.
  tc_observed <- total_correlation(data, vars)
  tc_null <- mean(replicate(n_permutations, {
    shuffled <- as.data.frame(lapply(data[vars], sample))
    total_correlation(shuffled, vars)
  }))

  structure(
    list(pairwise = pairs,
         by_ascriptive = rollup,
         total_correlation = tc_observed,
         total_correlation_null = tc_null,
         total_correlation_excess = tc_observed - tc_null,
         n = nrow(data)),
    class = "oc_consolidation")
}

#' @export
print.oc_consolidation <- function(x, ...) {
  cat("Organizational consolidation summary (n =", x$n, "members)\n\n")
  cat("Mean association magnitude (0-1) by ascriptive status",
      "(Cohen 1988: ~0.1 low, ~0.3 medium, ~0.5 high):\n")
  for (i in seq_len(nrow(x$by_ascriptive))) {
    cat(sprintf("  %-10s %.3f\n",
                x$by_ascriptive$ascriptive[i], x$by_ascriptive$mean_magnitude[i]))
  }
  cat(sprintf(paste0(
    "\nTotal correlation across all variables: %.3f nats",
    " (%.3f above the permutation null of %.3f)\n"),
    x$total_correlation, x$total_correlation_excess, x$total_correlation_null))
  cat("Pairwise detail in $pairwise; each pair's statistic in $pairwise$measure.\n")
  invisible(x)
}

# ---- level-matched pairwise association (the article's Figure 4) ------

#' Levels of measurement for measure selection: "binary" (two observed
#' values, whatever the storage type), "quantitative" (numeric or ordered
#' factor -- continuous or ordinal), else "nominal".
var_level <- function(x) {
  n_obs <- length(unique(x[!is.na(x)]))
  if (n_obs <= 2) return("binary")
  if (is.numeric(x) || is.ordered(x)) return("quantitative")
  "nominal"
}

# ordinal scores: ordered factors by level rank, numerics unchanged
as_scores <- function(x) {
  if (is.ordered(x)) as.numeric(x) else as.numeric(x)
}

#' Point-biserial correlation magnitude: binary vs continuous/ordinal.
point_biserial <- function(bin, num) {
  ok <- !(is.na(bin) | is.na(num))
  b <- as.numeric(droplevels(factor(bin[ok])))
  s <- as_scores(num)[ok]
  if (length(unique(b)) < 2 || stats::sd(s) == 0) return(NA_real_)
  abs(stats::cor(b, s))
}

#' Correlation ratio (eta) magnitude: nominal vs continuous/ordinal.
eta_ratio <- function(nom, num) {
  ok <- !(is.na(nom) | is.na(num))
  g <- droplevels(factor(nom[ok]))
  s <- as_scores(num)[ok]
  if (nlevels(g) < 2) return(NA_real_)
  grand <- mean(s)
  ss_total <- sum((s - grand)^2)
  if (ss_total == 0) return(NA_real_)
  means <- tapply(s, g, mean)
  ns <- tapply(s, g, length)
  sqrt(sum(ns * (means - grand)^2) / ss_total)
}

#' Select and compute the level-matched association for one pair.
#' The ascriptive side is nominal or binary per the data requirements
#' (data/data-dictionary.md); the
#' criterion may be binary, nominal, or quantitative.
matched_association <- function(a, cr) {
  la <- var_level(a); lc <- var_level(cr)
  if (la == "binary" && lc == "binary") {
    list(measure = "phi", magnitude = cramers_v(a, cr))  # V == |phi| on 2x2
  } else if (la == "binary" && lc == "quantitative") {
    list(measure = "point-biserial", magnitude = point_biserial(a, cr))
  } else if (lc == "binary" && la == "quantitative") {
    list(measure = "point-biserial", magnitude = point_biserial(cr, a))
  } else if (la == "nominal" && lc == "quantitative") {
    list(measure = "eta", magnitude = eta_ratio(a, cr))
  } else if (lc == "nominal" && la == "quantitative") {
    list(measure = "eta", magnitude = eta_ratio(cr, a))
  } else {
    list(measure = "cramers_v", magnitude = cramers_v(a, cr))
  }
}

#' Cramér's V between two nominal variables (classical, uncorrected estimator).
#' Interpret against Cohen's (1988) thresholds. Rows with NA in either variable
#' are dropped; returns NA if either variable has fewer than two observed levels.
cramers_v <- function(x, y) {
  ok <- !(is.na(x) | is.na(y))
  tab <- table(droplevels(factor(x[ok])), droplevels(factor(y[ok])))
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  if (nrow(tab) < 2 || ncol(tab) < 2) return(NA_real_)
  chi2 <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$statistic)
  unname(sqrt(chi2 / (sum(tab) * (min(nrow(tab), ncol(tab)) - 1))))
}

#' Mutual information between two variables, in nats. Complete cases only.
mutual_information <- function(x, y) {
  ok <- !(is.na(x) | is.na(y))
  p_xy <- table(x[ok], y[ok]) / sum(ok)
  p_x <- rowSums(p_xy)
  p_y <- colSums(p_xy)
  expected <- outer(p_x, p_y)
  nonzero <- p_xy > 0
  sum(p_xy[nonzero] * log(p_xy[nonzero] / expected[nonzero]))
}

#' Total correlation across a set of variables (multivariate dependency), in nats.
#' Sum of marginal entropies minus the joint entropy: zero exactly when the
#' variables are mutually independent, and equal to mutual_information() for
#' two variables.
#' Complete cases across `vars` only.
total_correlation <- function(data, vars) {
  d <- data[stats::complete.cases(data[vars]), vars, drop = FALSE]
  entropy <- function(p) {
    p <- p[p > 0]
    -sum(p * log(p))
  }
  marginal <- sum(vapply(vars, function(v) entropy(table(d[[v]]) / nrow(d)), numeric(1)))
  joint <- entropy(table(interaction(d, drop = TRUE)) / nrow(d))
  marginal - joint
}

#' The article's Figure 4 layout on your data: pairwise association
#' magnitudes beside mutual information (as % uncertainty reduced), one
#' 0-1 scale. `result` is what measure_consolidation() returns; `data`
#' supplies the ascriptive columns, whose entropies scale the mutual
#' information. Returns a drawable object (cowplot); on a synthetic run
#' the provenance caption is added.
plot_initial_assessment <- function(result, data, cfg = oc_config) {
  display_name <- function(v) {
    vn <- cfg$variable_names
    if (!is.null(vn) && v %in% names(vn)) unname(vn[[v]])
    else tools::toTitleCase(gsub("_", " ", v))
  }
  entropy <- function(x) {
    p <- table(x) / sum(table(x))
    p <- p[p > 0]
    -sum(p * log(p))
  }
  tiles <- result$pairwise
  tiles$uncertainty_reduced <- tiles$mutual_information /
    vapply(tiles$ascriptive, function(a) entropy(data[[a]]), numeric(1))
  tiles$Criterion <- factor(
    vapply(tiles$criterion, display_name, character(1)),
    levels = rev(vapply(cfg$criteria, display_name, character(1))))
  tiles$Status <- factor(
    vapply(tiles$ascriptive, display_name, character(1)),
    levels = vapply(cfg$ascriptive, display_name, character(1)))

  panel_theme <- ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold", size = 12,
                                                      hjust = 0.5))
  fig_v <- ggplot2::ggplot(tiles, ggplot2::aes(Status, Criterion,
                                                fill = magnitude)) +
    ggplot2::geom_tile(width = 0.95, height = 0.95, color = "white",
                       linewidth = 0.4) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", magnitude)),
                       size = 3.5, fontface = "bold") +
    ggplot2::scale_fill_gradient(limits = c(0, 1), low = "white",
                                 high = "#2166ac",
                                 name = "Magnitude\n(0\u20131 scale)") +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = NULL, y = NULL, title = "Pairwise associations") +
    panel_theme
  fig_mi <- ggplot2::ggplot(tiles, ggplot2::aes(Status, Criterion,
                                                 fill = uncertainty_reduced)) +
    ggplot2::geom_tile(width = 0.95, height = 0.95, color = "white",
                       linewidth = 0.4) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(uncertainty_reduced,
                                                            accuracy = 0.1)),
                       size = 3.5, fontface = "bold") +
    ggplot2::scale_fill_gradient(limits = c(0, 1), low = "white",
                                 high = "#2166ac") +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = NULL, y = NULL,
                  title = "Mutual information\n(as % uncertainty reduced)") +
    panel_theme +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   legend.position = "none")
  combined <- cowplot::plot_grid(
    fig_v + ggplot2::theme(legend.position = "none"),
    fig_mi,
    cowplot::get_legend(fig_v),
    nrow = 1, rel_widths = c(1.12, 0.78, 0.3), align = "h", axis = "tb")
  if (oc_synthetic_run(cfg)) {
    combined <- cowplot::ggdraw(cowplot::add_sub(
      combined, "Synthetic data illustration \u2014 not the article's results",
      size = 7, colour = "grey40", x = 0.02, hjust = 0))
  }
  combined
}
