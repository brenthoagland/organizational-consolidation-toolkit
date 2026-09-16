# status_hierarchy_poset.R
#
# The partial order of intersectional profiles and its Hasse diagram (the
# drawing behind the article's Figure 9). Works from any table of
# profiles with a predicted value; methods/05 hands it the cube that
# status_hierarchy_steps.R builds.
#
#   status_hierarchy_poset()      order the profiles and draw the diagram
#   dichotomize_against_ideal()   reduce each dimension to the ideal's level vs the rest
#
# Design notes: docs/poset-design.md.
#' Build a status-hierarchy poset from profile predictions, drawn as a Hasse diagram
#'
#' Given one row per intersectional profile with a predicted recognition
#' value, the function:
#'   1. takes the ideal actor to be the highest-predicted profile, unless
#'      `ideal` supplies one;
#'   2. reduces each dimension to "matches the ideal" or not;
#'   3. orders the profiles: A outranks B when A matches the ideal on every
#'      dimension B does, plus at least one more (a profile's tier is its
#'      number of matched dimensions);
#'   4. lays the order out as the article's two-step construction. Step
#'      one (layout = "formal") puts height at the tier and annotates the
#'      predicted values beneath the profiles. Step two (layout =
#'      "outcome") keeps the same x positions and puts height at the
#'      predicted value on a metric axis; `axis_limits` pins that axis so
#'      drawings from different runs share one scale;
#'   5. draws an arrow for each cover relation (an edge only where no
#'      observed profile sits between the pair). Consolidation shows in
#'      the shape: chain, diamond, or spread.
#'
#' layout = "levels" is a compact variant of step two for crowded grids:
#' profiles with similar values share an evenly spaced row, and each row's
#' value or range labels the axis. It compresses the value axis, so prefer
#' the two-step pair when the point is how far the values stretch.
#'
#' A monotonicity violation, a profile matching fewer ideal dimensions but
#' predicted above one matching more, is reported rather than hidden:
#' returned in $violations, counted in $shape_metrics, and drawn dashed.
#' Among intermediate profiles it is a finding about which dimensions
#' carry the recognition weight. If a supplied ideal is outscored by
#' another profile, the function warns.
#'
#' @param profiles   data.frame, one row per intersectional profile. An
#'                   optional `label` column overrides the default
#'                   "level / level / level" node labels. A
#'                   collapse_to_cube() return has `dimensions`,
#'                   `ideal`, and `dimension_names` as attributes, which
#'                   are the defaults below.
#' @param dimensions character vector of dimension columns (e.g. race, gender, local_type).
#' @param outcome    name of the predicted-recognition column.
#' @param ideal      optional named list, the ideal's level on each
#'                   dimension; NULL = the highest-predicted profile's levels.
#' @param outcome_lo,outcome_hi optional column names bounding the outcome
#'                   (e.g. conf.low / conf.high); drawn as vertical
#'                   whiskers behind each node. layout = "outcome" only.
#' @param layout     "formal" (step one), "outcome" (step two), or
#'                   "levels" (default, the compact variant); see above.
#' @param axis_limits layout = "outcome" only: c(lo, hi) for the value
#'                   axis. Pass the same limits to every run you mean to
#'                   compare. Must cover the outcome range; NULL scales
#'                   the axis to this run.
#' @param axis_breaks optional break positions for a pinned axis
#'                   (default: the integers inside axis_limits).
#' @param x_displacement optional named vector, dimension -> horizontal
#'                   shift for profiles missing the ideal on that
#'                   dimension (recentered to sum to zero). Overrides the
#'                   layout's default; use it to match a published
#'                   figure's arrow directions.
#' @param annotate_values print each profile's predicted value in gray
#'                   beneath its label. Default TRUE for the two-step
#'                   layouts, FALSE for "levels".
#' @param dimension_names optional named vector, dimension column ->
#'                   display name in the caption (e.g. c(race_bin =
#'                   "Race")). Default strips "_bin", replaces
#'                   underscores with spaces, and title-cases.
#' @param label_style "match" reproduces the published figure's
#'                   typography: a dimension's level set bold where it
#'                   holds the ideal's level, italic where it departs
#'                   (plotmath; overrides any `label` column; falls back
#'                   to plain labels with a warning for level names
#'                   plotmath cannot render). "plain" (default) uses the
#'                   `label` column as given.
#' @param label_size text size for the node labels.
#' @return list:
#'   $nodes          one row per profile: dimensions, outcome, match indicators,
#'                   tier, x/y plot coordinates, profile_id, label
#'   $edges          cover relations (from = subordinate, to = dominant), with
#'                   a `violation` flag where the outcome order inverts
#'   $ideal          named list, the ideal actor's level per dimension
#'   $violations     all dominance pairs whose outcomes invert
#'   $shape_metrics  tier_separation (Spearman rho of tier vs outcome),
#'                   within_tier_spread (mean within-tier SD / overall SD),
#'                   comparability (share of profile pairs the dominance
#'                   order strictly ranks; equivalent pairs excluded),
#'                   equivalence_rate (share of pairs with identical match
#'                   patterns -- the order cannot rank these),
#'                   monotonicity_violations (count),
#'                   compression (top-to-bottom outcome range)
#'   $dropped_cells  cross-product cells with no observed profile
#'   $projection     named vector, each dimension's horizontal displacement
#'                   (identical across the two-step layouts by construction)
#'   $plot           ggplot rendering
#' @export
status_hierarchy_poset <- function(profiles,
                                   dimensions = attr(profiles, "dimensions"),
                                   outcome = "predicted",
                                   ideal   = attr(profiles, "ideal"),
                                   outcome_lo = NULL,
                                   outcome_hi = NULL,
                                   layout = c("levels", "outcome", "formal"),
                                   axis_limits = NULL,
                                   axis_breaks = NULL,
                                   x_displacement = NULL,
                                   annotate_values = NULL,
                                   dimension_names = attr(profiles, "dimension_names"),
                                   label_style = c("plain", "match"),
                                   label_size = 3.9) {
  layout <- match.arg(layout)
  label_style <- match.arg(label_style)
  if (is.null(dimensions)) {
    stop("`dimensions` is required unless `profiles` has them as an ",
         "attribute (a collapse_to_cube() return does)", call. = FALSE)
  }
  if (layout != "outcome" && !(is.null(outcome_lo) && is.null(outcome_hi))) {
    stop('whiskers need a linear outcome axis -- use layout = "outcome"')
  }
  if (layout != "outcome" && !(is.null(axis_limits) && is.null(axis_breaks))) {
    stop('axis_limits/axis_breaks pin the value axis -- use layout = "outcome"')
  }
  if (!is.null(axis_limits) &&
      (!is.numeric(axis_limits) || length(axis_limits) != 2 ||
       axis_limits[1] >= axis_limits[2])) {
    stop("`axis_limits` must be c(lo, hi) with lo < hi")
  }
  if (is.null(annotate_values)) {
    annotate_values <- layout %in% c("formal", "outcome")
  }
  missing_cols <- setdiff(c(dimensions, outcome, outcome_lo, outcome_hi),
                          names(profiles))
  if (length(missing_cols) > 0) {
    stop("columns not found in profiles: ", paste(missing_cols, collapse = ", "))
  }
  if (nrow(profiles) == 0) {
    stop("profiles is empty -- if you filtered small cells, every cell fell below the cutoff")
  }
  if (anyDuplicated(profiles[dimensions]) > 0) {
    stop("profiles must have one row per unique dimension combination")
  }
  if (anyNA(profiles[dimensions])) {
    stop("NA in dimension columns -- drop or recode those profiles first")
  }
  out <- profiles[[outcome]]
  if (!is.numeric(out)) stop("`", outcome, "` must be numeric")
  if (any(!is.finite(out))) {
    stop("`", outcome, "` contains NA or non-finite values")
  }
  if (xor(is.null(outcome_lo), is.null(outcome_hi))) {
    stop("supply both outcome_lo and outcome_hi, or neither")
  }
  if (!is.null(axis_limits) &&
      (min(out) < axis_limits[1] || max(out) > axis_limits[2])) {
    stop(sprintf(
      "axis_limits [%g, %g] do not cover the outcome range [%g, %g] -- ",
      axis_limits[1], axis_limits[2], min(out), max(out)),
      "widen the shared scale (a clipped profile is a silently wrong figure)")
  }
  n <- nrow(profiles)

  # --- ideal actor -----------------------------------------------------------
  if (is.null(ideal)) {
    top <- which(out == max(out))
    if (length(top) > 1) {
      stop("two or more profiles tie for the maximum ", outcome,
           "; supply `ideal` explicitly")
    }
    ideal <- as.list(profiles[top, dimensions, drop = FALSE])
    ideal <- lapply(ideal, as.character)
  } else {
    if (!all(dimensions %in% names(ideal))) {
      stop("`ideal` must name every dimension")
    }
    ideal <- lapply(ideal[dimensions], as.character)
    ideal_row <- which(Reduce(`&`, lapply(dimensions, function(d)
      as.character(profiles[[d]]) == ideal[[d]])))
    top_out <- if (length(ideal_row) == 1) out[ideal_row] else -Inf
    if (any(out > top_out)) {
      best <- which.max(out)
      warning("supplied ideal is outscored by profile ",
              paste(as.character(unlist(profiles[best, dimensions])),
                    collapse = "/"),
              " -- the local ideal may differ from the theoretical one")
    }
  }

  # --- match-to-ideal indicators and tiers -----------------------------------
  match_mat <- vapply(dimensions, function(d)
    as.character(profiles[[d]]) == ideal[[d]], logical(n))
  match_mat <- matrix(match_mat, nrow = n,
                      dimnames = list(NULL, dimensions))
  tier <- rowSums(match_mat)

  # --- dominance and cover edges ---------------------------------------------
  dominates <- function(i, j) {  # i strictly dominates j
    all(match_mat[j, ] <= match_mat[i, ]) && tier[i] > tier[j]
  }
  edges <- data.frame(from = integer(0), to = integer(0))
  violations <- data.frame(dominant = integer(0), subordinate = integer(0),
                           outcome_dominant = numeric(0),
                           outcome_subordinate = numeric(0))
  # Pairs with identical match patterns cannot be ranked by the order;
  # they count toward equivalence_rate, not comparability. (Counting
  # them as comparable would inflate the metric exactly where non-ideal
  # levels of a multi-level dimension collapse together.)
  comparable_pairs <- 0
  equivalent_pairs <- 0
  dom <- matrix(FALSE, n, n)
  if (n > 1) {
    for (i in seq_len(n - 1)) {
      for (j in (i + 1):n) {
        hi <- if (dominates(i, j)) i else if (dominates(j, i)) j else NA
        if (all(match_mat[i, ] == match_mat[j, ])) {
          equivalent_pairs <- equivalent_pairs + 1
        } else if (!is.na(hi)) {
          comparable_pairs <- comparable_pairs + 1
        }
        if (is.na(hi)) next
        lo <- if (hi == i) j else i
        dom[hi, lo] <- TRUE
        if (out[hi] < out[lo]) {                  # dominance inverted by outcome
          violations <- rbind(violations, data.frame(
            dominant = hi, subordinate = lo,
            outcome_dominant = out[hi], outcome_subordinate = out[lo]))
        }
      }
    }
    # Cover relation: an edge only where no observed profile sits
    # strictly between. Tier distance is not enough: when an intermediate
    # cell is unobserved, neighbours in the observed order can be two
    # tiers apart.
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (dom[i, j] && !any(dom[i, ] & dom[, j])) {
          edges <- rbind(edges, data.frame(from = j, to = i))
        }
      }
    }
  }
  edges$violation <- if (nrow(edges) > 0) {
    mapply(function(f, t) out[t] < out[f], edges$from, edges$to)
  } else logical(0)

  # --- nodes and layout ------------------------------------------------------
  # label_style "match": each dimension's level bold where it holds the
  # ideal's level, italic where it departs (plotmath; overrides `label`).
  # "plain": the `label` column, or "level / level / level".
  # Display casing follows the published Figure 9: each level starts with
  # a capital ("female" -> "Female", "type 1" -> "Type 1"); a "Non-" prefix
  # keeps what follows as it is ("Non-White", "Non-type 1"). The data's own
  # level names are untouched.
  cap_first <- function(x) sub("^([a-z])", "\\U\\1", x, perl = TRUE)
  if (label_style == "match") {
    lev_by_dim <- lapply(dimensions, function(d) unique(as.character(profiles[[d]])))
    dup_levels <- unique(unlist(lev_by_dim))[
      vapply(unique(unlist(lev_by_dim)), function(l)
        sum(vapply(lev_by_dim, function(x) l %in% x, logical(1))) > 1,
        logical(1))]
    if (length(dup_levels) > 0) {
      warning("level name(s) shared across dimensions (",
              paste(dup_levels, collapse = ", "),
              ") -- match-style labels omit dimension names, so consider ",
              "renaming for unambiguous labels")
    }
    esc <- function(s) gsub('(["\\\\])', "\\\\\\1", s)   # plotmath string literal
    label <- vapply(seq_len(n), function(i)
      paste(vapply(seq_along(dimensions), function(j)
        sprintf('%s("%s")',
                if (match_mat[i, j]) "bold" else "italic",
                esc(cap_first(as.character(profiles[[dimensions[j]]][i])))),
        character(1)), collapse = "~"), character(1))
    label_plain <- apply(profiles[dimensions], 1, function(r)
      paste(cap_first(as.character(r)), collapse = " "))
    parse_ok <- vapply(label, function(l)
      !inherits(try(parse(text = l), silent = TRUE), "try-error"), logical(1))
    if (!all(parse_ok)) {
      warning("label_style = \"match\" cannot render some level names -- ",
              "falling back to plain labels")
      label_style <- "plain"
    }
  }
  if (label_style == "plain") {
    label <- if ("label" %in% names(profiles)) as.character(profiles$label)
             else apply(profiles[dimensions], 1, function(r)
               paste(cap_first(as.character(r)), collapse = " / "))
  }
  # Horizontal position. Each dimension gets a fixed sideways shift, and a
  # profile's x is the sum of the shifts for the dimensions on which it
  # misses the ideal. Every edge that loses the same dimension is then
  # parallel, so the lattice looks like a cube. The shifts sum to zero, so
  # the profile missing on every dimension sits directly below the ideal.
  # Profiles with the same match pattern (several non-ideal levels of one
  # dimension) fan out around that pattern's x.
  # The two-step layouts use one set of shifts, so step two keeps step
  # one's x: first dimension to the left, last to the right, middle ones
  # straight down. The levels layout nudges middle dimensions off zero,
  # because its rows would otherwise stack profiles on top of each other.
  # `x_displacement` overrides either.
  k <- length(dimensions)
  if (!is.null(x_displacement)) {
    if (is.null(names(x_displacement)) ||
        !all(dimensions %in% names(x_displacement))) {
      stop("`x_displacement` must be a named vector covering every dimension")
    }
    u <- as.numeric(x_displacement[dimensions])
    u <- u - mean(u)
    if (max(abs(u)) == 0) stop("`x_displacement` collapses to all-zero")
  } else if (layout == "levels") {
    u <- 3.0 * (seq_len(k) - (k + 1) / 2)
    u[abs(u) < 0.8] <- 0.9
    u <- u - sum(u) / k
  } else {
    u <- 5.2 * (2 * seq_len(k) - (k + 1)) / max(k - 1, 1)
  }
  base_x <- as.numeric((1 - match_mat) %*% u)
  pattern <- apply(match_mat, 1, paste, collapse = "")
  x <- base_x
  for (p in unique(pattern)) {
    ix <- which(pattern == p)
    m <- length(ix)
    if (m > 1) {
      off <- seq(-(m - 1) / 2, (m - 1) / 2, length.out = m) * 0.5
      x[ix[order(out[ix], decreasing = TRUE)]] <- base_x[ix][1] + off
    }
  }
  colnames(match_mat) <- paste0("matches_", dimensions)
  nodes <- cbind(profiles, as.data.frame(match_mat))
  nodes$tier <- tier
  nodes$profile_id <- seq_len(n)
  if (label_style == "match") {
    # plotmath labels are one line, as in the published figure
    label_width <- nchar(label_plain)
  } else {
    # wrap long labels onto two lines (at the space nearest the middle):
    # halves their footprint, so the lattice packs like the article's
    wrap_label <- function(l) {
      if (nchar(l) <= 16 || !grepl(" ", l)) return(l)
      sp <- gregexpr(" ", l)[[1]]
      cut <- sp[which.min(abs(sp - nchar(l) / 2))]
      paste0(substr(l, 1, cut - 1), "\n", substr(l, cut + 1, nchar(l)))
    }
    label <- vapply(label, wrap_label, character(1), USE.NAMES = FALSE)
    label_width <- vapply(strsplit(label, "\n", fixed = TRUE),
                          function(ls) max(nchar(ls)), numeric(1))
  }
  nodes$label <- label
  nodes$x <- x
  nodes$y <- out
  if (layout == "formal") {
    # step one: height is the tier; predicted values appear only as
    # annotations
    nodes$y_draw <- tier
  } else if (layout == "levels") {
    # Compact variant: profiles with similar predicted values share a
    # row, and rows are evenly spaced. A new row opens wherever the
    # sorted values jump by more than 8% of their range, a display
    # tolerance for a near-tie. The axis is labeled with
    # each row's value or range, so it still reads in order, and near-ties
    # share a row instead of acquiring an artificial rank.
    span <- if (max(out) > min(out)) c(min(out), max(out)) else c(0, 1)
    ord <- order(out, decreasing = TRUE)
    gap_thr <- 0.08 * diff(range(out))
    lvl <- integer(n)
    lvl[ord[1]] <- 1L
    if (n > 1) {
      for (m in 2:n) {
        lvl[ord[m]] <- lvl[ord[m - 1]] +
          as.integer(out[ord[m - 1]] - out[ord[m]] > gap_thr)
      }
    }
    n_lvl <- max(lvl)
    nodes$y_draw <- span[2] - (lvl - 1) / max(n_lvl - 1, 1) * diff(span)
    lvl_centers <- span[2] - (seq_len(n_lvl) - 1) / max(n_lvl - 1, 1) * diff(span)
    lvl_labels <- vapply(seq_len(n_lvl), function(L) {
      v <- out[lvl == L]
      hi <- sprintf("%.1f", max(v))
      lo <- sprintf("%.1f", min(v))
      if (hi == lo) hi else paste0(hi, "-", lo)
    }, character(1))
    # a crowded level splits into sub-rows of at most three, highest
    # values in the top row; the level's axis label shows the value
    # range, so the axis stays a single ordered readout
    lvl_gap <- diff(span) / max(n_lvl - 1, 1)
    row_h <- min(0.075 * diff(span), 0.3 * lvl_gap)
    for (L in seq_len(n_lvl)) {
      ix <- which(lvl == L)
      m <- length(ix)
      if (m > 3) {
        r <- rank(-out[ix], ties.method = "first")
        sub <- (r - 1) %/% 3
        n_sub <- max(sub) + 1
        nodes$y_draw[ix] <- nodes$y_draw[ix] +
          (n_sub - 1) / 2 * row_h - sub * row_h
      }
    }
  } else {
    nodes$y_draw <- out
  }
  # Before drawing, x is adjusted for legibility and y is never touched:
  # vertical position is the finding, horizontal position only keeps
  # labels apart. Where two label boxes would collide, both are pushed
  # apart horizontally. In the two-step layouts x encodes the
  # construction (step two must reuse step one's positions), so a nudge
  # there earns a warning: profiles in the same cube column sit too
  # close on the value axis, and the remedies are wider axis_limits, a
  # bigger figure, or reading the near-tie as a finding.
  rx_node <- (0.12 + 0.047 * label_width) * (label_size / 3.9)
  y_span <- max(nodes$y_draw) - min(nodes$y_draw)
  if (y_span <= 0) y_span <- 1
  n_lines <- vapply(strsplit(nodes$label, "\n", fixed = TRUE),
                    length, numeric(1))
  ry_node <- 0.033 * y_span * (0.5 + 0.5 * n_lines)
  if (layout != "levels" && n > 1) {
    collides <- FALSE
    for (i in seq_len(n - 1)) for (j in (i + 1):n) {
      if (abs(nodes$y_draw[i] - nodes$y_draw[j]) < ry_node[i] + ry_node[j] &&
          1.18 * (rx_node[i] + rx_node[j]) -
            abs(nodes$x[i] - nodes$x[j]) > 0.001) collides <- TRUE
    }
    if (collides) {
      warning("profiles overlap on the cube projection -- nudging ",
              "horizontally, so step-one and step-two x positions may no ",
              "longer match; profiles in the same column sit close on the ",
              "value axis")
    }
  }
  for (pass in 1:80) {
    moved <- FALSE
    for (i in seq_len(n - 1)) for (j in (i + 1):n) {
      if (abs(nodes$y_draw[i] - nodes$y_draw[j]) < ry_node[i] + ry_node[j]) {
        gap <- 1.18 * (rx_node[i] + rx_node[j]) - abs(nodes$x[i] - nodes$x[j])
        if (gap > 0.001) {
          s_ij <- if (nodes$x[i] > nodes$x[j]) 1
                  else if (nodes$x[i] < nodes$x[j]) -1
                  else if (i < j) 1 else -1
          nodes$x[i] <- nodes$x[i] + s_ij * gap / 2
          nodes$x[j] <- nodes$x[j] - s_ij * gap / 2
          moved <- TRUE
        }
      }
    }
    if (!moved) break
  }
  nodes$x <- nodes$x - mean(range(nodes$x))   # recenter after the nudges
  nodes$label_y <- nodes$y_draw               # labels are the nodes; no drift
  # labels have fixed physical size, so keep-outs are set in axis units
  # against the full drawn span -- the pinned axis when there is one
  y_span_eff <- if (!is.null(axis_limits)) diff(axis_limits) else y_span

  # --- shape metrics ---------------------------------------------------------
  within_sd <- tapply(out, tier, stats::sd)
  within_sd <- mean(within_sd[!is.na(within_sd)])
  overall_sd <- stats::sd(out)
  shape_metrics <- data.frame(
    tier_separation = suppressWarnings(
      stats::cor(tier, out, method = "spearman")),
    within_tier_spread = if (is.finite(within_sd) && overall_sd > 0)
      within_sd / overall_sd else 0,
    comparability = if (n > 1) comparable_pairs / choose(n, 2) else 1,
    equivalence_rate = if (n > 1) equivalent_pairs / choose(n, 2) else 0,
    monotonicity_violations = nrow(violations),
    compression = max(out) - min(out)
  )

  # declared factor levels count even when unobserved: an empty level is
  # exactly a dropped cell
  dropped_cells <- prod(vapply(profiles[dimensions], function(d)
    if (is.factor(d)) nlevels(d) else length(unique(d)), numeric(1))) - n

  # --- plot ------------------------------------------------------------------
  # As in the article's Figure 9, the label is the node. Arrows run from
  # the dominant profile down to the one it outranks, trimmed so they do
  # not pierce the labels; violations are dashed; dotted gridlines mark
  # the outcome levels; whiskers (outcome_lo/hi) sit behind the labels.
  seg <- if (nrow(edges) > 0) {
    data.frame(x = nodes$x[edges$to], y = nodes$y_draw[edges$to],
               xend = nodes$x[edges$from], yend = nodes$y_draw[edges$from],
               violation = edges$violation)
  } else {
    data.frame(x = numeric(0), y = numeric(0), xend = numeric(0),
               yend = numeric(0), violation = logical(0))
  }
  # Trim each end at an elliptical keep-out around the label. Horizontal
  # clearance scales with label length; the trimmed fraction is capped so
  # short edges keep a visible shaft.
  if (nrow(seg) > 0) {
    node_rx <- (0.12 + 0.047 * label_width) * (label_size / 3.9)
    # arrows travel from the dominant profile down: the departing end
    # clears the label and any value annotation beneath it (ry_bot), the
    # arriving head stops just above the label's top edge (ry_top)
    if (layout == "levels") {
      ry_top <- ry_bot <- 0.062 * y_span
      t_cap <- 0.35
    } else if (layout == "formal") {
      ry_top <- 0.05 * y_span_eff
      ry_bot <- (if (annotate_values) 0.09 else 0.05) * y_span_eff
      t_cap <- 0.42
    } else {
      # the value axis runs more units per label height than the tier
      # axis, so its keep-out fractions are proportionally smaller
      ry_top <- 0.035 * y_span_eff
      ry_bot <- (if (annotate_values) 0.062 else 0.035) * y_span_eff
      t_cap <- 0.42
    }
    dx <- seg$xend - seg$x
    dy <- seg$yend - seg$y
    rx_from <- node_rx[edges$to]      # seg starts at the dominant node
    rx_to   <- node_rx[edges$from]    # and ends at the dominated one
    t_start <- pmin(1 / sqrt((dx / rx_from)^2 + (dy / ry_bot)^2), t_cap)
    t_end   <- pmin(1 / sqrt((dx / rx_to)^2   + (dy / ry_top)^2), t_cap)
    seg$x    <- seg$x    + t_start * dx
    seg$y    <- seg$y    + t_start * dy
    seg$xend <- seg$xend - t_end * dx
    seg$yend <- seg$yend - t_end * dy

    # two edges leaving the same node at nearly the same angle stack into
    # what looks like one extra-thick arrow; push each of such a pair a
    # little along its perpendicular so both stay visible
    if (nrow(seg) > 1) {
      x_scale <- max(diff(range(nodes$x)), 1)
      ang <- atan2((seg$yend - seg$y) / y_span, (seg$xend - seg$x) / x_scale)
      for (a in seq_len(nrow(seg) - 1)) for (b in (a + 1):nrow(seg)) {
        shared <- (edges$to[a] == edges$to[b]) ||
                  (edges$from[a] == edges$from[b])
        if (shared && abs(ang[a] - ang[b]) < 0.05) {
          for (e in c(a, b)) {
            s_e <- if (e == a) 1 else -1
            px <- -sin(ang[e]) * 0.022 * x_scale * s_e
            py <-  cos(ang[e]) * 0.022 * y_span * s_e
            seg$x[e] <- seg$x[e] + px;  seg$xend[e] <- seg$xend[e] + px
            seg$y[e] <- seg$y[e] + py;  seg$yend[e] <- seg$yend[e] + py
          }
        }
      }
    }
  }

  # Gridlines are explicit segments at the levels the data occupies. In
  # the levels layout each line shows its row's value or range.
  if (layout == "levels") {
    grid_y <- lvl_centers
    grid_lab <- lvl_labels
  } else if (layout == "formal") {
    grid_y <- 0:length(dimensions)     # one dotted line per tier, unlabeled
  } else if (!is.null(axis_limits)) {
    grid_y <- if (!is.null(axis_breaks)) axis_breaks
              else seq(ceiling(axis_limits[1]), floor(axis_limits[2]))
  } else {
    pad <- 0.04 * y_span
    br <- pretty(range(nodes$y_draw), n = 5)
    grid_y <- br[br >= min(nodes$y_draw) - pad & br <= max(nodes$y_draw) + pad]
  }

  plot <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = data.frame(y = grid_y),
      ggplot2::aes(x = -Inf, xend = Inf, y = y, yend = y),
      colour = "grey62", linetype = "dotted", linewidth = 0.5)
  if (!is.null(outcome_lo) && !is.null(outcome_hi)) {
    whisk <- data.frame(x = nodes$x,
                        lo = profiles[[outcome_lo]],
                        hi = profiles[[outcome_hi]])
    plot <- plot + ggplot2::geom_linerange(
      data = whisk, ggplot2::aes(x = x, ymin = lo, ymax = hi),
      colour = "grey80", linewidth = 0.7, alpha = 0.5)
  }
  plot <- plot +
    ggplot2::geom_segment(
      data = seg,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                   linetype = violation),
      color = "grey35", linewidth = 0.75,
      arrow = ggplot2::arrow(length = grid::unit(7, "pt"),
                             type = "closed", angle = 22)) +
    ggplot2::geom_label(data = nodes[nodes$tier < length(dimensions), ],
                        ggplot2::aes(x = x, y = label_y, label = label),
                        parse = (label_style == "match"),
                        family = "Helvetica", size = label_size, colour = "grey10",
                        fontface = if (label_style == "match") "plain" else "bold",
                        lineheight = 0.85,
                        fill = "white", label.size = 0,
                        label.padding = grid::unit(2, "pt"),
                        label.r = grid::unit(0, "pt")) +
    # the ideal actor gets a second channel beyond bold: a shaded box
    ggplot2::geom_label(data = nodes[nodes$tier == length(dimensions), ],
                        ggplot2::aes(x = x, y = label_y, label = label),
                        parse = (label_style == "match"),
                        family = "Helvetica", size = label_size, colour = "grey10",
                        fontface = if (label_style == "match") "plain" else "bold",
                        lineheight = 0.85, fill = "grey85",
                        label.size = 0.3,
                        label.padding = grid::unit(3, "pt"),
                        label.r = grid::unit(0, "pt")) +
    (if (annotate_values) {
      ann_off <- (if (layout == "formal") 0.053 else 0.035) * y_span_eff
      ggplot2::geom_label(
        data = cbind(nodes, .value = out),
        ggplot2::aes(x = x, y = label_y - ann_off,
                     label = sprintf("%.1f", .value)),
        family = "Helvetica", size = label_size *
          (if (layout == "formal") 0.80 else 0.72),
        colour = "grey45", fill = "white", label.size = 0,
        label.padding = grid::unit(1.5, "pt"),
        label.r = grid::unit(0, "pt"))
    } else NULL) +
    ggplot2::scale_linetype_manual(
      values = c(`FALSE` = "solid", `TRUE` = "dashed"),
      guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = 1.9)) +
    (if (layout == "levels") {
      ggplot2::scale_y_continuous(breaks = grid_y, labels = grid_lab)
    } else if (layout == "outcome" && !is.null(axis_limits)) {
      # the shared scale: pinned limits, padded below for the lowest
      # profile's annotation, so two runs drawn with the same axis_limits
      # are directly comparable
      ggplot2::scale_y_continuous(
        breaks = grid_y, labels = grid_y, limits = axis_limits,
        expand = ggplot2::expansion(
          add = c((if (annotate_values) 0.094 else 0.04), 0.02) *
            diff(axis_limits)))
    } else NULL) +
    # the caption decodes the arrow directions, so the figure is
    # self-explanatory outside the article
    ggplot2::labs(x = NULL, y = if (layout == "formal") NULL else outcome,
                  caption = paste0(
                    if (layout == "formal") paste0(
                      "Height = characteristics shared with the ideal actor",
                      if (annotate_values)
                        "; predicted values annotated in gray, not as position.\n"
                      else ".\n")
                    else "",
                    "Arrows point from dominant to dominated (dashed = the outcome order inverts).\n",
                    "Each dimension's losses run in one direction: ",
                    paste(sprintf("%s %s",
                                  if (!is.null(dimension_names)) {
                                    ifelse(dimensions %in% names(dimension_names),
                                           dimension_names[dimensions],
                                           dimensions)
                                  } else {
                                    tools::toTitleCase(
                                      gsub("_", " ", sub("_bin$", "", dimensions)))
                                  },
                                  ifelse(u == min(u), "(down-left)",
                                         ifelse(u == max(u), "(down-right)",
                                                "(down)"))),
                          collapse = ", "),
                    ". Shaded box = ideal actor.")) +
    ggplot2::theme_minimal(base_family = "Helvetica", base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_text(size = 13),
                   axis.text.y = ggplot2::element_text(size = 11),
                   plot.caption = ggplot2::element_text(
                     size = 7, colour = "grey40", hjust = 0),
                   plot.background = ggplot2::element_rect(fill = "white",
                                                           colour = NA))
  if (layout == "formal") {
    # step one's axis is the tier, already decoded in the caption
    plot <- plot + ggplot2::theme(axis.text.y = ggplot2::element_blank())
  }

  list(nodes = nodes, edges = edges, ideal = ideal, violations = violations,
       shape_metrics = shape_metrics, dropped_cells = dropped_cells,
       projection = stats::setNames(u, dimensions), plot = plot)
}

#' Dichotomize dimensions against an ideal actor's levels.
#'
#' As in the article's Figure 9, each multi-level dimension collapses to
#' two levels, the ideal's level versus everything else, so a poset over k
#' dimensions is a readable lattice of 2^k profiles. Two-level dimensions
#' pass through under their own names. Take `ideal` from the levels of
#' your highest-predicted profile, or supply it on theoretical grounds and
#' treat a warning that another profile outscores it as a finding.
#'
#' @param data  data.frame holding the dimension columns (member-level or
#'   profile-level; the recoding is the same)
#' @param ideal named list or vector, dimension column -> the ideal's level
#' @param suffix appended to each new column name (default "_bin")
#' @return `data` with one added factor column per dichotomized dimension
#'   (levels: the ideal's level first, then "non-<level>"), plus an
#'   attribute `bin_dimensions` naming the columns to use as poset
#'   dimensions -- new ones where created, original names where a
#'   dimension was already binary
dichotomize_against_ideal <- function(data, ideal, suffix = "_bin") {
  if (length(ideal) == 0 || is.null(names(ideal)) || any(names(ideal) == "")) {
    stop("`ideal` must be named: dimension column -> the ideal's level")
  }
  dims_out <- character(0)
  for (v in names(ideal)) {
    if (!v %in% names(data)) stop("column not found in data: ", v)
    lev <- as.character(ideal[[v]])
    vals <- as.character(data[[v]])
    if (anyNA(vals)) {
      stop("NA in `", v, "` -- drop or recode those rows before dichotomizing")
    }
    if (!lev %in% vals) {
      stop("level \"", lev, "\" is not observed in `", v, "`")
    }
    if (length(unique(vals)) <= 2) {
      dims_out <- c(dims_out, v)
      next
    }
    bin <- paste0(v, suffix)
    if (bin %in% names(data)) {
      stop("`", bin, "` already exists in data -- rename it or pass a ",
           "different `suffix`")
    }
    other <- paste0("Non-", lev)   # the published figures' compound style
    data[[bin]] <- factor(ifelse(vals == lev, lev, other),
                          levels = c(lev, other))
    dims_out <- c(dims_out, bin)
  }
  attr(data, "bin_dimensions") <- dims_out
  data
}
