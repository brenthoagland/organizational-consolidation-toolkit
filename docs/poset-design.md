# Poset design notes

How the steps in [`status_hierarchy_steps.R`](../R_functions/status_hierarchy_steps.R)
and the drawing in [`status_hierarchy_poset()`](../R_functions/status_hierarchy_poset.R)
construct the article's status hierarchy ("Mapping Intersectionality:
Partial Order Set Modeling"), so that every rule behind Figure 9 is
explicit.

## 1. Purpose

The engine turns predicted recognition per intersectional profile into a
partial order and draws it as a Hasse diagram, the standard drawing of a
poset (Davey and Priestley 2002). The order shows how proximity to the
organization's ideal actor channels recognition, and its shape (chain,
diamond, or spread lattice) tracks the degree of organizational
consolidation in the article's Table 2 sense: the shape recognition
takes over the order. The chosen dimensions fix the order, so with
every cell of the dichotomized cube present it is the same lattice in
every regime; consolidation registers in the value
layer, as verticality (compression) and clarity (within-tier spread,
violations).

Posets preserve multidimensional relationships that single rankings
compress, showing how an organization evaluates whole persons (Cheng
2015, 2018, 2020, 2022); Cheng's (2022) cube of privilege dimensions,
drawn as a Hasse diagram, is the closest published analogue to the
lattice methods/05 builds. The motivation is intersectional: ascriptive
statuses and local types are embodied together and combine
non-additively (Cho, Crenshaw, and McCall 2013; Collins 2019; McCall
2005), and the regression alternative, ever higher-order interaction
terms, grows unwieldy as dimensions multiply (Block, Golder, and Golder
2023). The poset takes the combinations as its units.

These posets are not Galois lattices (Ganter, Stumme, and Wille 2005),
which require every pair of attribute sets to have a meet and a join.
They order the intersectional profiles by how many high-value
attributes each shares with the ideal actor, with no requirement of
algebraic closure.

The drawing has two layers. The order layer is the Hasse-diagram
content (drawing mechanics follow Freese 2004): only the cover relation,
the transitive reduct, is drawn, as arrows from dominant to dominated.
The value layer sets height: each profile sits at its predicted
recognition on a metric axis. Classical Hasse diagrams omit arrowheads
and let height show direction; Figure 9 draws arrows so that height is
free to show the values. When recognition tracks proximity to the
ideal, the layers agree. A violation is a dominance pair the value
order contradicts: drawn dashed, a finding rather than a drawing error.

## 2. Input

`status_hierarchy_poset()` takes a data frame with one row per profile:
one column per dimension (any number; the article uses race, gender,
and local type), an `outcome` column of predicted recognition from any
model, and optional `outcome_lo` and `outcome_hi` columns for
uncertainty. Dimensions must be categorical.

The engine derives a binary match-to-ideal per dimension (§4), so
multi-level dimensions work directly, with one consequence: profiles
that share a match pattern become order-equivalent, with no edges
between them (counted in `equivalence_rate`). Collapsing first with
[`dichotomize_against_ideal()`](../R_functions/status_hierarchy_poset.R)
gives the clean 2^k lattice of Figure 9. That is what methods/05 does:
it fits one recognition model with proportional type terms, values
every position by prediction, then averages those predictions into each
ideal-vs-rest cube cell (§6). The two routes, multi-level direct or
collapse then average, answer slightly different questions.

## 3. Ideal actor

The article orders profiles relative to the organization's ideal actor
(Williams 2000), the intersectional profile that receives the most
recognition, functioning as the organization's controlling image
(Collins 2002 [1990]). The ideal actor is an ideal type, a standard for
comparison, not the average member of a category (Fiske and Taylor
2017).

In `status_hierarchy_poset()` the ideal is the profile with the maximum
`outcome`, or the one `ideal` names; a tie for the maximum requires an
explicit `ideal`. `choose_ideal()`, the step methods/05 runs first,
takes by default the occupied position with the highest lower
confidence bound rather than the highest point value. The two often
agree; when they differ, most visibly when the leading cell is small,
the function names both candidates with their predictions and cell
sizes, and methods/05 prints the rule it used beside the chosen ideal's
value and interval. A departure is a flag, not a verdict. `rule =
"max"` takes the point maximum, the article's rule; `ideal =` sets the
ideal on theoretical grounds.

## 4. Match to the ideal

For each profile and dimension, a binary indicator: does the profile
hold the ideal's level on that dimension? This is the article's "how
many high-value characteristics they share with the ideal." Every
non-ideal level collapses into one "does not match" state: if the
ideal's race is White, then Black, Latinx, and Other become Non-White.

## 5. Order, tiers, and edges

- **Dominance:** A dominates B when A's set of matched dimensions
  contains B's. This is the Boolean lattice on the match indicators; the
  ideal is at the top and the profile matching nothing at the bottom.
- **Tier:** the number of matched dimensions, 0 to D.
- **Edges:** the cover relations, the transitive reduct of the order:
  an edge only where no profile lies strictly between the pair. On a
  full lattice these are the pairs whose match sets differ by one
  dimension. The pattern of edges is the lattice.

## 6. Cells nobody occupies

`status_hierarchy_poset()` draws whatever profiles it is given and
reports how many cross-product cells are absent. `collapse_to_cube()`
gives it every cube cell, because each position is valued by prediction
with the members' own controls: a cell nobody occupies carries the value
the model expects for that position, not a summary of members who hold
it. The `n` column records occupancy, the engine prints a message for
every cell with `n = 0`, and the ideal is searched among occupied
positions only (`n >= 3`). Read an unoccupied node as a prediction about
a position, never as a finding about people. To draw observed profiles
only, pass them to `status_hierarchy_poset()` yourself (the recipe in
`methods/05-status-hierarchy/README.md`).

## 7. Layout

The article's construction is a pair of drawings.

- **Step one, `layout = "formal"`:** height is the tier; the predicted
  values appear as annotations beneath the labels.
- **Step two, `layout = "outcome"`:** the same cube and x positions,
  each profile at its predicted value on a metric axis. `axis_limits`
  pins the axis, so runs you mean to compare (regimes, waves, subgroups)
  share one scale. This layout also draws the `outcome_lo` and
  `outcome_hi` whiskers.
- **`layout = "levels"`** is a compact variant for crowded grids:
  profiles of similar value share an evenly spaced row. It compresses
  the value axis, so prefer the pair when the stretch of the values is
  the point.

The x position is a parallel projection of the match-pattern cube: each
dimension has a fixed horizontal shift, so edges losing the same
dimension are parallel. The two-step layouts share one symmetric
projection (first dimension left, last right, middle straight down),
which lets step two keep step one's x; `x_displacement` overrides it.
Profiles sharing a match pattern fan out around it. A collision nudge
triggers a warning; on real data it means profiles in the same cube
column nearly tie, which is a finding.

## 8. Monotonicity

Tier is set by matching the ideal; height is set by predicted
recognition; the two can disagree. A profile matching fewer dimensions
can be predicted above one matching more. The engine reports each such
violation rather than hiding it: height stays at the predicted value,
the contradicting edge is dashed, the count is in `shape_metrics`, and
a supplied ideal that is outscored triggers a warning. A violation is a
finding, that the organization rewards some non-ideal profiles, not a
drawing error. Banding heights by tier would hide the conflict;
re-ranking by prediction alone would lose the intersectional-proximity
story.

## 9. Shape metrics

The article describes status hierarchies by their verticality (the
depth of status distinctions) and clarity (the unambiguity of relative
positions), two of the architectural features in Accominotti, Lynn, and
Sauder (2022). The engine returns `shape_metrics` so that the claim can
be checked: tier separation, within-tier spread, comparability (the
share of pairs the order ranks; equivalent pairs are reported as
`equivalence_rate`), the monotonicity-violation count, and compression
(the top-to-bottom range of the predicted values).

Comparability and equivalence rate depend only on which cells the poset
holds. Tier separation, within-tier spread, violations, and compression
respond to the predicted values, and compression is the diagnostic that
moves with consolidation: methods/05 run on the three synthetic files
gives 1.8 → 4.3 → 6.2 from low to high at near-constant total
recognition, with no violation in any file. The association magnitudes
of methods/01
diagnose consolidated criteria; the lattice diagnoses consolidated
recognition. The two are the theory's condition and its outcome,
reported separately.

## 10. Return value

`list(nodes, edges, ideal, violations, shape_metrics, dropped_cells,
projection, plot)`: inspectable, not only a picture.

## 11. References

- Accominotti, Fabien, Freda Lynn, and Michael Sauder. 2022. "The
  Architecture of Status Hierarchies: Variations in Structure and Why
  They Matter for Inequality." *RSF: The Russell Sage Foundation
  Journal of the Social Sciences* 8(6):87–102.
- Block, Ray, Jr., Matt Golder, and Sona N. Golder. 2023. "Evaluating
  Claims of Intersectionality." *The Journal of Politics*
  85(3):795–811.
- Cheng, Eugenia. 2015. *How to Bake Pi: An Edible Exploration of the
  Mathematics of Mathematics*. New York: Basic Books.
- Cheng, Eugenia. 2018. *The Art of Logic in an Illogical World*. New
  York: Basic Books.
- Cheng, Eugenia. 2020. *X + Y: A Mathematician's Manifesto for
  Rethinking Gender*. New York: Basic Books.
- Cheng, Eugenia. 2022. *The Joy of Abstraction: An Exploration of
  Math, Category Theory, and Life*. Cambridge, UK: Cambridge
  University Press.
- Cho, Sumi, Kimberlé Williams Crenshaw, and Leslie McCall. 2013.
  "Toward a Field of Intersectionality Studies: Theory, Applications,
  and Praxis." *Signs: Journal of Women in Culture and Society*
  38(4):785–810.
- Collins, Patricia Hill. 2002 [1990]. *Black Feminist Thought:
  Knowledge, Consciousness, and the Politics of Empowerment*. New
  York: Routledge.
- Collins, Patricia Hill. 2019. *Intersectionality as Critical Social
  Theory*. Durham, NC: Duke University Press.
- Davey, Brian A., and Hilary A. Priestley. 2002. *Introduction to
  Lattices and Order*, 2nd ed. Cambridge, UK: Cambridge University
  Press.
- Fiske, Susan T., and Shelley E. Taylor. 2017. *Social Cognition:
  From Brains to Culture*, 3rd ed. London, UK: Sage.
- Freese, Ralph. 2004. "Automated Lattice Drawing." Pp. 112–127 in
  *Concept Lattices: Second International Conference on Formal Concept
  Analysis*. Springer.
- Ganter, Bernhard, Gerd Stumme, and Rudolf Wille, eds. 2005. *Formal
  Concept Analysis: Foundations and Applications*. Springer.
- McCall, Leslie. 2005. "The Complexity of Intersectionality." *Signs:
  Journal of Women in Culture and Society* 30(3):1771–1800.
- Williams, Joan C. 2000. *Unbending Gender: Why Family and Work
  Conflict and What to Do about It*. New York: Oxford University
  Press.

