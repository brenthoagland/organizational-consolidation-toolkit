# 05 · Mapping Intersectionality: Partial Order Set Modeling

Build the status hierarchy of the article's Figure 9 from predicted
recognition, and draw it as a Hasse diagram, the standard picture of a
partial order. The shape recognition takes over that order diagnoses
the degree of consolidation.

The hierarchy has two layers. The **order** comes from composition: how
many dimensions a profile shares with the ideal actor decides which
profiles it dominates, and an arrow connects each profile to the ones it
immediately dominates. The **predicted values** set the heights. The
script draws the hierarchy twice: first with height as the number of
shared dimensions and the values annotated, then with each profile at
its predicted value on a metric axis. The arrows are the claim about
structure; the heights are the model's estimate of how much recognition
that structure carries. A dashed arrow marks a profile predicted above
one that dominates it, a finding about dimensions pulling against each
other, not a drawing error.

`build-status-hierarchy.R` runs the construction one step per call:
the local types from the run of record, one recognition model with
proportional type terms, every position valued by prediction
(`predict_positions()`), the ideal actor found on the full grid
(`choose_ideal()`), each dimension reduced to the ideal's level versus
the rest and each of the 2^k cube cells valued as the average of the
positions it pools (`collapse_to_cube()`), and the cube drawn twice.
It closes with the article's diagnostic step after Figure 9: among
members who share the ideal's local type, how many in each ascriptive
profile were recognized, and how many would have been had that type
been rewarded at the ideal's rate (`unrecognized_members()`).

The drawing engine (`status_hierarchy_poset()`) accepts any table of
profiles with a predicted value: one row per profile, the dimension
columns, and a numeric prediction (from a predict call at the profile's
covariate values, or averaged over its members; report which). Interval
columns `outcome_lo` and `outcome_hi` are optional and add whiskers.
Design notes: [`docs/poset-design.md`](../../docs/poset-design.md).

**Observed profiles only.** The script values every position of the
full grid, occupied or not, and saves them with their occupancy in
`output/status_hierarchy_<outcome>_positions.csv`. To draw only the
positions that members occupy, filter that table on `n` and hand it to
the drawing engine with the dimensions and the ideal named:

```r
positions <- read.csv("output/status_hierarchy_respected_positions.csv")
occupied  <- positions[positions$n >= 3, ]
drawn <- status_hierarchy_poset(
  occupied,
  dimensions  = c("race", "gender", "local_type"),
  ideal       = list(race = "White", gender = "male", local_type = "type 1"),
  layout      = "outcome",
  label_style = "plain")
print(drawn$plot)
```

Each remaining position keeps its predicted value from the full fit;
what changes is the order. A profile dominates another only when it
matches the ideal on every dimension the other matches and at least one
more, so removing positions removes arrows and can leave profiles
incomparable, and the shape metrics describe the drawn set, not the
full grid.

- **Run on your own data:** `build-status-hierarchy.R` (calls functions in the repo-root `R_functions/`).
