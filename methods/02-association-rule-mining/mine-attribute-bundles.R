# =====================================================================
# 02 Surfacing Salient Attributes: Association Rule Mining
# Finds the attribute bundles tied to each status position (arules).
# Fill in import-data.R and run it once first.
# Run from the repo root:  Rscript methods/02-association-rule-mining/mine-attribute-bundles.R
# =====================================================================
library(tidyverse)
library(arules)
for (f in list.files("R_functions", pattern = "[.]R$", full.names = TRUE)) source(f)
source("import-data.R")

data <- oc_load_data()

# Build the transactions. With arm_unit = "nomination", the article's
# unit, each nomination is one transaction, so frequently nominated
# members weigh more; it needs two or more status positions. With
# arm_unit = "member", each member is one transaction (ever nominated
# or not).
unit <- if (is.null(oc_config$arm_unit)) "nomination" else oc_config$arm_unit
transactions <- noms_to_transactions(
  data,
  attributes = c(oc_config$ascriptive, oc_config$criteria),
  outcomes   = oc_config$outcomes,
  unit       = unit
)

# Mine the rules (arules::apriori), as the article's Appendix B does.
# The algorithm first identifies frequent itemsets, combinations of
# attributes that meet the support threshold (0.05: present in at least
# 5% of transactions), then generates rules by assigning status
# positions as consequents and evaluates them on confidence (0.50, which
# Appendix B reads as the consequent following the antecedent more often
# than chance) and lift.
# Only status positions may appear as a consequent; attributes appear
# only in the antecedent. minlen = 2 excludes the empty-antecedent rule
# ({} => outcome), which restates the outcome's base rate.
min_support    <- 0.05
min_confidence <- 0.50
rules <- apriori(
  transactions,
  parameter  = list(supp = min_support, conf = min_confidence,
                    target = "rules", minlen = 2),
  appearance = list(lhs = setdiff(colnames(transactions), oc_config$outcomes),
                    rhs = oc_config$outcomes),
  control    = list(verbose = FALSE))
rules <- add_rule_measures(rules, transactions)   # conviction, boost, added value; sorted by lift
cat(length(rules), "rules pass the support and confidence thresholds\n")

# Finally, retain only the rules with lift > 1: those that predict the
# status position better than chance.
rules <- subset(rules, lift > 1)
cat(length(rules), "rules remain after lift > 1\n\n")

# Save every retained rule with its support, confidence, lift, and
# conviction, as Table B1 does, before drawing, so an empty set is
# recorded too (as a header row and no rows).
csv <- oc_output_file("attribute_rules.csv")
write_rules_csv(rules, csv)
cat("wrote", csv, "\n")

if (length(rules) == 0) {
  # No rule survived the filters, so there is nothing to draw. This is a
  # result, not an error; the empty table is saved above. A network from
  # an earlier run must not survive beside it.
  cat(sprintf(paste0("\nNo rules survived the filters (support >= %s, ",
                     "confidence >= %s, lift > 1), so the refined set is ",
                     "empty and there is no rule network to draw. Lower ",
                     "min_support or min_confidence in this script to look ",
                     "further.\n"), min_support, min_confidence))
  pdf_file <- oc_output_file("rule_network.pdf")
  if (file.exists(pdf_file)) {
    file.remove(pdf_file)
    cat("removed", pdf_file, "(from an earlier run)\n")
  }
} else {
  inspect(head(rules, 15))

  # Rules per status position
  for (out in oc_config$outcomes) {
    cat(sprintf("%-15s %d rules\n", out, length(rules_for(rules, out))))
  }

  # Draw the rule network: attribute values feed rule nodes, and each
  # rule points at the status position it predicts. The script draws
  # every surviving rule, as the article's Figure 6 did with its 48,
  # unless max_rules_per_position in import-data.R sets a cap or more than
  # 200 rules survive (then it draws the 25 highest-lift per position).
  plot_rules <- rules_to_draw(rules, oc_config$outcomes,
                              cap = oc_config$max_rules_per_position)
  cat("plotting", length(plot_rules), "of", length(rules), "rules\n")
  p <- plot_rule_network(
    plot_rules,
    outcomes = oc_config$outcomes,
    labels   = oc_dummy_labels(data, c(oc_config$ascriptive, oc_config$criteria),
                               oc_config$outcomes, oc_config$labels)
  )
  pdf_file <- oc_output_file("rule_network.pdf")
  ggsave(pdf_file, oc_provenance(p), width = 7, height = 5.5,
         device = cairo_pdf)
  cat("wrote", pdf_file, "\n")
}

# docs/interpreting-results.md explains how to read the output.
