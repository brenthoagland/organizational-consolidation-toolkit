# =====================================================================
# 03 Identifying Local Types: Latent Class Analysis
# Identifies the local types with latent class analysis (poLCA) and tests
# whether they overlap with or cut across the ascriptive statuses; saves
# the run of record methods/04 and 05 use.
# Fill in import-data.R and run it once first.
# Run from the repo root:  Rscript methods/03-latent-class-analysis/recover-local-types.R
# =====================================================================
library(tidyverse)
library(poLCA)
library(vcd)
for (f in list.files("R_functions", pattern = "[.]R$", full.names = TRUE)) source(f)
source("import-data.R")

data <- oc_load_data()
criteria <- oc_config$criteria

# --- Model selection ------------------------------------------------------
# Estimate solutions ranging from one to six classes and compare fit
# statistics, classification error, and interpretability, as the
# article's Appendix C does. poLCA estimates each solution from ten sets
# of starting values and keeps the one with the highest log-likelihood,
# because the EM algorithm can converge on a local maximum. BIC tends to
# underestimate the number of classes in small samples; the article
# selected its four-class solution on lower AIC, reduced classification
# error, no trivial classes, and high interpretability. When BIC and AIC
# disagree, a bootstrap likelihood ratio test compares the k-class
# solution with k + 1 (slow at num_bootstrap = 1000):
# blrt(data, criteria, k0 = 3, num_bootstrap = 1000)
set.seed(1234)
# class_range in import-data.R names the counts compared here (the
# article searched one to ten; the default 1:6 keeps this quick)
class_range <- if (is.null(oc_config$class_range)) 1:6 else oc_config$class_range
fit_stats <- lca_fit_stats(data, criteria, k_range = class_range, nrep = 10)

# Goodness of fit statistics across the candidate solutions, in the
# shape of Table C1: log-likelihood, BIC, AIC, entropy-based R^2,
# classification error (modal assignment error, member-weighted), and
# the smallest class's share.
print(as_table_c1(fit_stats), digits = 4, row.names = FALSE)
csv <- oc_output_file("class_count_sweep.csv")
write.csv(as_table_c1(fit_stats), csv, row.names = FALSE)
cat("wrote", csv, "(goodness of fit statistics, in Table C1's shape)\n")

# BIC and AIC across the candidate solutions, as in Figure C1.
fig <- plot_class_count_sweep(fit_stats)
pdf_file <- oc_output_file("class_count_sweep.pdf")
ggsave(pdf_file, oc_provenance(fig), width = 6.5, height = 4,
       device = cairo_pdf)
cat("wrote", pdf_file, "\n")
cat("Compare the solutions, set n_types in import-data.R to the number of",
    "classes you select, and run this script again; methods/04 and 05",
    "use that choice.\n")

# --- The selected solution ------------------------------------------------
# Estimate the solution with n_types classes from import-data.R. Only the
# local criteria enter, as the manifest indicators; the ascriptive
# statuses stay external to the LCA, so that whether the local types
# overlap with or cut across them can be tested below. poLCA requires
# categorical indicators coded to begin at 1, here in each criterion's
# own level order. nrep = 10 sets of starting values, as above.
# na.rm = FALSE keeps members with missing indicators; poLCA estimates
# from their observed values. (lca_fit_stats() above makes this same
# call at each candidate number of classes.)
manifest <- as.data.frame(lapply(data[criteria], as_lca_codes))
f <- as.formula(paste("cbind(", paste(criteria, collapse = ", "), ") ~ 1"))
set.seed(1234)
LCA <- poLCA(f, manifest, nclass = oc_config$n_types, nrep = 10,
             maxiter = 5000, tol = 1e-10, na.rm = FALSE, calc.se = TRUE,
             graphs = FALSE, verbose = FALSE)
LCA$indicators  <- criteria                       # the run of record names them
LCA$assignment  <- factor(LCA$predclass, levels = seq_len(oc_config$n_types))
LCA$class_share <- as.vector(prop.table(table(LCA$assignment)))
LCA$entropy_r2  <- if (oc_config$n_types > 1) entropy.R2(LCA) else NA_real_

# Class numbers are arbitrary and can change between runs. To fix an
# order, reorder the classes and refit from the reordered start:
# probs.start <- poLCA.reorder(LCA$probs.start, c(2, 1, 4, 3))
# LCA <- poLCA(f, manifest, nclass = 4, nrep = 1, probs.start = probs.start, ...)

data$local_type <- LCA$assignment                 # modal class membership
# Save the run of record, posterior class probabilities included.
# Methods/04 and 05 load it instead of refitting, so the class numbering
# holds. It is saved even for one class, so those sections can report
# the one-type result.
save_local_types(LCA, data, oc_config$id)

# The steps below need at least two local types. With one, the criteria
# draw no distinguishable difference between members; the script reports
# that and stops.
if (oc_config$n_types >= 2) {
  # Model quality: entropy-based R^2 and modal assignment error, as in
  # Appendix C (East High: 0.74 and 0.16, moderately strong separation).
  # Below about 0.8, classification error carries into the later
  # sections, and proportional assignment (methods/04's default) matters
  # more (Bakk and Vermunt 2016; Bonikowski and DiMaggio 2016).
  if (LCA$entropy_r2 < 0.8) message(sprintf(
    "NOTE: entropy R^2 = %.2f: classes weakly separated; see the note in this script", LCA$entropy_r2))

  # Response probabilities by class: for each class, criterion, and
  # level, the probability of that response (poLCA's item-response
  # probabilities). `level` is the response's own label; `code` is
  # poLCA's index. Read this table to name the classes in import-data.R.
  probs_long <- do.call(rbind, lapply(names(LCA$probs), function(v) {
    m <- LCA$probs[[v]]
    labs <- lca_response_levels(data[[v]])
    levels_out <- if (length(labs) == ncol(m)) labs else colnames(m)
    data.frame(criterion   = v,
               local_type  = rep(paste("type", seq_len(nrow(m))),
                                 times = ncol(m)),
               level       = rep(levels_out, each = nrow(m)),
               code        = rep(colnames(m), each = nrow(m)),
               probability = as.vector(m))
  }))
  csv <- oc_output_file("class_probabilities.csv")
  write.csv(probs_long, csv, row.names = FALSE)
  cat("wrote", csv, "\n")

  cat("\nClass shares:\n")
  print(round(LCA$class_share, 2))
  cat("\nEntropy R^2:", round(LCA$entropy_r2, 2), "\n")
  # Classification error (modal assignment error, member-weighted), as
  # in Table C1's Error column.
  cat("Classification error:",
      round(mean(1 - apply(LCA$posterior, 1, max)), 2), "\n\n")

  # Appendix C's two classification tables. Table C2: the average
  # posterior probability of each class among the members assigned to
  # each class (rows sum to 1). Table C3: the classification error
  # matrix, P(assigned | true) (columns sum to 1).
  c2 <- classification_table(LCA)
  cat("Table C2, average posterior probabilities by modal class:\n")
  print(round(c2, 3))
  csv <- oc_output_file("classification_by_modal_class.csv")
  write.csv(as.data.frame.matrix(c2), csv)
  cat("wrote", csv, "\n")
  c3 <- classification_error_matrix(LCA)
  cat("\nTable C3, classification error matrix, P(assigned | true):\n")
  print(round(c3, 3))
  csv <- oc_output_file("classification_error_matrix.csv")
  write.csv(as.data.frame.matrix(c3), csv)
  cat("wrote", csv, "\n\n")
  for (v in criteria) {
    m <- LCA$probs[[v]]
    labs <- lca_response_levels(data[[v]])
    if (length(labs) == ncol(m)) colnames(m) <- labs
    cat(v, "\n")
    print(round(m, 2))
  }

  # Whether the local types overlap with or cut across the ascriptive
  # statuses, the most direct assessment of organizational consolidation:
  # Cramér's V between local type and each status.
  type_v <- data.frame(
    ascriptive = oc_config$ascriptive,
    cramers_v  = vapply(oc_config$ascriptive,
                        function(a) cramers_v(data$local_type, data[[a]]),
                        numeric(1)),
    n_types    = oc_config$n_types,
    row.names  = NULL)
  for (i in seq_len(nrow(type_v))) {
    cat(sprintf("Cramér's V, local_type x %s: %.3f\n",
                type_v$ascriptive[i], type_v$cramers_v[i]))
  }
  # Save the type-level V, the number you cite.
  csv <- oc_output_file("type_by_ascriptive_V.csv")
  write.csv(type_v, csv, row.names = FALSE)
  cat("wrote", csv, "\n")

  # The same alignment as mosaics, as in the article's Figure 8: tile
  # areas show the joint distribution of local types and each ascriptive
  # status, and color marks the cells that depart from independence by
  # more than chance.
  for (a in oc_config$ascriptive) {
    pdf_file <- oc_output_file(sprintf("type_by_%s_mosaic.pdf", a))
    pdf(pdf_file, width = 6.5, height = 4.5)
    draw_type_mosaic(data, a)
    dev.off()
    cat("wrote", pdf_file, "\n")
  }
} else {
  message(one_class_message)
}

# docs/interpreting-results.md explains how to read the output.
