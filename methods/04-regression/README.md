# 04 · Local Types as Regression Inputs

Model the recognition counts for each status position with zero-inflated Poisson regression (`pscl`): a logit for the odds of no nominations, and a count model for the rate of nominations among members at risk of being labeled. Local types enter by proportional assignment, the article's main specification. The script saves the predicted recognition for each intersectional profile, prints the article's three-model comparison for each status position (ascriptive statuses with their intersections, local types alone, both combined; Appendix D, Tables D3–D5), and prints the modal-assignment check and the Vuong comparisons against ordinary Poisson and negative binomial.

- **Run on your own data:** `predict-recognition.R` (calls functions in the repo-root `R_functions/`).
