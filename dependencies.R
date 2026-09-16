# Packages used across the toolkit (renv.lock will pin exact versions).
pkgs <- c(
  "arules",                  # association rule mining (Appendix B)
  "poLCA",                   # latent class analysis   (Appendix C)
  "pscl", "marginaleffects", # zero-inflated Poisson + predictions (Appendix D)
  "modelsummary",            # the three-model comparison tables (Tables D3-D5)
  "vcd",                     # mosaic displays (Figure 8)
  "igraph",                  # rule-network layout (Figure 6)
  "tidygraph", "ggraph",     # rule network graph + grammar (Figure 6)
  "cowplot",                 # network + legend panel assembly (Figure 6)
  "tidyverse", "scales",     # dplyr/tidyr/ggplot2 + label formatting
  "rmarkdown", "knitr",      # knit synthetic_walkthrough.Rmd
  "renv"
)
# install.packages(setdiff(pkgs, rownames(installed.packages())))
