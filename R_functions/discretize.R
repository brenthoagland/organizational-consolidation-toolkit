# discretize.R
#
#   discretize()  quantile-bin a numeric criterion into ordered categories
#' Quantile-bin a numeric criterion into n ordered categories (quartiles by
#' default) so ARM and LCA can treat it as a categorical indicator.
#' Heavy ties (many zeros, say) can collapse quantile edges; the function
#' then bins over the distinct values, and a constant input yields one bin,
#' so the result may have fewer than n levels rather than an error.
#' @param x numeric vector
#' @param n number of bins (integer >= 2)
#' @return ordered factor with levels q1, q2, ...
discretize <- function(x, n = 4) {
  if (length(n) != 1 || !is.finite(n) || n != as.integer(n) || n < 2) {
    stop("`n` must be a single integer >= 2")
  }
  if (all(is.na(x))) stop("`x` is entirely missing")
  probs <- seq(0, 1, length.out = n + 1)
  breaks <- unique(stats::quantile(x, probs = probs, na.rm = TRUE))
  if (length(breaks) < 3 && length(unique(x[!is.na(x)])) > 1) {
    # heavy ties (e.g. mostly zeros) collapsed the quantile edges:
    # bin over the distinct values instead so unequal values still separate
    breaks <- unique(stats::quantile(unique(x[!is.na(x)]), probs = probs))
  }
  if (length(breaks) < 2) breaks <- c(breaks, breaks + 1)  # constant input: single bin
  cut(x, breaks = breaks, include.lowest = TRUE, ordered_result = TRUE,
      labels = paste0("q", seq_len(length(breaks) - 1)))
}
