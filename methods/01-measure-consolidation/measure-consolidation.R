# =====================================================================
# 01 An Initial Assessment of Organizational Consolidation
# Measures how tightly ascriptive statuses align with local criteria:
# pairwise associations and mutual information (the article's Figure 4).
# Fill in import-data.R and run it once first.
# Run from the repo root:  Rscript methods/01-measure-consolidation/measure-consolidation.R
# =====================================================================
library(tidyverse)
for (f in list.files("R_functions", pattern = "[.]R$", full.names = TRUE)) source(f)
source("import-data.R")

data <- oc_load_data()

# measure_consolidation() measures each pair with the statistic matched
# to its levels of measurement (phi, point-biserial, eta, Cramér's V)
# and reports every value as a 0-1 magnitude. The `measure` column of
# the saved table names each pair's statistic.
set.seed(1234)   # the permutation null draws at random; the seed makes reruns match
result <- measure_consolidation(
  data,
  ascriptive = oc_config$ascriptive,
  criteria   = oc_config$criteria,
  n_permutations = 20   # draws for the total-correlation permutation null
)
print(result)

# Save the pairwise table and the figure: associations beside mutual
# information (as % uncertainty reduced), in Figure 4's layout.
csv <- oc_output_file("pairwise_associations.csv")
write.csv(result$pairwise, csv, row.names = FALSE)
cat("wrote", csv, "\n")

combined <- plot_initial_assessment(result, data)
pdf_file <- oc_output_file("initial_assessment.pdf")
ggsave(pdf_file, combined, width = 8.2, height = 4.5, device = cairo_pdf)
cat("wrote", pdf_file, "\n")

# docs/interpreting-results.md explains how to read the output. To see
# the diagnostics move with consolidation, point data_path in import-data.R
# at the moderate or high synthetic file and rerun.
