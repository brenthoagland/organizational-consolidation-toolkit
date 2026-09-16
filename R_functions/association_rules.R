# association_rules.R
#
# Association rule mining of attribute bundles for status positions
# (the article's Appendix B), built on `arules`. methods/02 calls
# arules::apriori() directly; these functions set up the transactions
# before it and handle the rules after it, as ordinary arules objects
# you can inspect, subset, and filter with arules' tools.
#
#   noms_to_transactions()  one transaction per nomination (or per member)
#   add_rule_measures()     Appendix B's measures on mined rules, sorted by lift
#   rules_for()             the rules for one status position
#   rules_to_draw()         the subset the rule network draws

#' Build a transactions table for rule mining.
#'
#' Binarizes each attribute into one logical dummy per observed level
#' (named var_level). Two units of analysis:
#'   unit = "member":     one row per member; each outcome becomes TRUE if the
#'                        member received any nomination for it.
#'   unit = "nomination": one row per individual nomination (rows expanded by
#'                        the outcome counts); each row carries exactly one
#'                        outcome. This preserves how often members are seen,
#'                        not just whether they are seen.
#' @param data data.frame per data/data-dictionary.md
#' @param attributes columns to binarize into lhs items (ascriptive + criteria)
#' @param outcomes   status-position count columns (rhs items)
#' @param unit       "nomination" (the article's unit) or "member"
noms_to_transactions <- function(data, attributes, outcomes,
                                 unit = c("nomination", "member")) {
  unit <- match.arg(unit)
  missing_cols <- setdiff(c(attributes, outcomes), names(data))
  if (length(missing_cols) > 0) {
    stop("columns not found in data: ", paste(missing_cols, collapse = ", "))
  }
  na_out <- outcomes[vapply(outcomes, function(o) anyNA(data[[o]]), logical(1))]
  if (length(na_out) > 0) {
    stop("NA nomination counts in ", paste(na_out, collapse = ", "), ": rule ",
         "mining needs a known count for every member. Under the nomination ",
         "unit an unknown count cannot be expanded; under the member unit it ",
         "would silently read as never nominated. Before methods/02, drop ",
         "those members or recode a blank that truly means zero -- leaving ",
         "them NA is right for the regressions, not here.", call. = FALSE)
  }
  if (unit == "nomination" && length(outcomes) < 2) {
    stop("nomination-unit mining needs two or more status positions: a ",
         "single outcome sits in every nomination, so every rule's lift is ",
         "1 and none survive the lift > 1 filter. Set arm_unit = \"member\" ",
         "in import-data.R for the \"ever nominated\" question, or declare two ",
         "or more recognition outcomes.", call. = FALSE)
  }

  if (unit == "nomination") {
    # Reshape to long format and expand rows according to nomination counts
    base <- data[, c(attributes, outcomes), drop = FALSE]
    base <- tidyr::pivot_longer(base, dplyr::all_of(outcomes),
                                names_to = "nomination", values_to = "value")
    base <- tidyr::uncount(base, value)
    for (out in outcomes) base[[out]] <- base$nomination == out
    base$nomination <- NULL
  } else {
    base <- data[, c(attributes, outcomes), drop = FALSE]
    for (out in outcomes) base[[out]] <- base[[out]] > 0
  }

  # One logical dummy per observed attribute level
  for (attr_col in attributes) {
    lv <- levels(factor(base[[attr_col]]))
    dummy_names <- paste0(attr_col, "_", sanitize_level(lv))
    if (anyDuplicated(dummy_names) > 0) {
      stop("levels of `", attr_col, "` collide after sanitizing (",
           paste(lv[duplicated(dummy_names) |
                    duplicated(dummy_names, fromLast = TRUE)],
                 collapse = ", "), ") -- rename the levels")
    }
    for (i in seq_along(lv)) {
      base[[dummy_names[i]]] <- base[[attr_col]] == lv[i]
    }
    base[[attr_col]] <- NULL
  }
  as.data.frame(base)
}

# Readable dummy suffixes: "1-2" -> "1to2", ">2" -> "over2", "<1" -> "under1"
sanitize_level <- function(l) {
  l <- gsub("-", "to", l, fixed = TRUE)
  l <- gsub(">", "over", l, fixed = TRUE)
  l <- gsub("<", "under", l, fixed = TRUE)
  gsub("[^A-Za-z0-9]+", "", l)
}

#' Appendix B's measures on mined rules, sorted by lift.
#'
#' methods/02 mines the rules with arules::apriori(), consequents
#' restricted to the status positions. This adds the measures the
#' article reports beside support, confidence, and lift: the number of
#' attributes in the bundle, coverage, conviction, boost, and added
#' value (https://mhahsler.github.io/arules/docs/measures), and sorts
#' by lift. Lift compares how often lhs and rhs co-occur against
#' independence: 1 is chance, above 1 the bundle makes the status
#' position more likely; methods/02 applies `lift > 1` afterward.
add_rule_measures <- function(rules, transactions) {
  arules::quality(rules) <- cbind(
    arules::quality(rules),
    attributes_in_combination = arules::size(arules::lhs(rules)),
    coverage_ = arules::interestMeasure(rules, measure = "coverage",
                                        transactions = transactions),
    conviction = arules::interestMeasure(rules, measure = "conviction",
                                         transactions = transactions),
    boost = arules::interestMeasure(rules, measure = "boost",
                                    transactions = transactions),
    added_value = arules::interestMeasure(rules, measure = "addedValue",
                                          transactions = transactions))
  arules::sort(rules, by = "lift", decreasing = TRUE)
}

#' Save the retained rules as Table B1's columns, an empty set included.
#'
#' With no rules, arules' data-frame coercion has no columns, and the
#' saved file would be a lone empty string. This writes the header row
#' the non-empty case has, so an adopter reading the CSV sees the schema
#' and a zero-row table, not a blank.
#'
#' @param rules an arules `rules` object, possibly empty
#' @param file  the CSV path
#' @return invisibly the data frame written
write_rules_csv <- function(rules, file) {
  df <- if (length(rules) > 0) methods::as(rules, "data.frame") else
    stats::setNames(data.frame(matrix(nrow = 0, ncol = 11)),
                    c("rules", "support", "confidence", "coverage", "lift",
                      "count", "attributes_in_combination", "coverage_",
                      "conviction", "boost", "added_value"))
  utils::write.csv(df, file, row.names = FALSE)
  invisible(df)
}

#' Rules predicting one status position (rhs filter).
rules_for <- function(rules, outcome) {
  arules::subset(rules, subset = arules::`%in%`(rhs, outcome))
}

#' The rules the network figure draws, highest lift first within each
#' status position. The article's Figure 6 drew every rule that survived
#' its chain (48), and cap = NULL does the same up to `auto_above` rules;
#' past that the function says so and draws `auto_cap` per position,
#' because hundreds of diamonds are unreadable and slow to lay out. A
#' number caps each position at that many rules; Inf draws every rule
#' regardless.
rules_to_draw <- function(rules, outcomes, cap = NULL,
                          auto_above = 200, auto_cap = 25) {
  if (is.null(cap) && length(rules) > auto_above) {
    message(length(rules), " rules survive the filter chain; drawing the ",
            auto_cap, " highest-lift per status position. Set ",
            "max_rules_per_position in import-data.R to change this.")
    cap <- auto_cap
  }
  do.call(c, lapply(outcomes, function(o) {
    r <- rules_for(rules, o)
    if (is.null(cap) || !is.finite(cap)) r else head(r, cap)
  }))
}
