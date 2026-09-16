# plot_rule_network.R
#
# The rule network of the article's Figure 6: attribute values feed rule
# nodes, and each rule points at the status position it predicts.
#
#   plot_rule_network()  the figure, from a rules object
#   (rule_network_frames(), rule_network_graph(), rule_network_legend(),
#    layout_rule_orbital() are its steps)
#
# Three node types:
#   attribute_value -- the specific values (Male, Grades: A's, ...). Edges out only.
#   rule            -- each mined rule is a node. Edges IN from antecedents,
#                      out to its consequent. Sized by conviction, colored by
#                      consequent, alpha by lift. Unlabeled.
#   status_position -- the outcomes. Edges IN only.
#
# Layout is orbital rather than force-directed-from-scratch:
#   1. Status positions pinned to the vertices of a regular polygon
#   2. Rules orbit their consequent in concentric rings by complexity
#      (2-antecedent -> inner, 3 -> middle, 4+ -> outer); conviction
#      modulates radial tightness, lift angular prominence, and rules with
#      similar antecedent sets (Jaccard) sit adjacent
#   3. Attributes orbit outside the rules; attributes feeding rules for
#      several status positions ("polysemous") sit at cluster boundaries
# then a short constrained force pass relaxes overlaps.
#
# The figure must survive grayscale print (the journal prints in
# grayscale; color appears online): consequents are distinguished by
# color and edge line type, and the three node roles by shape
# (circle / diamond / square), so nothing depends on color alone.
#
# Item labels are parsed from arules' brace-and-comma encoding, so item
# names containing "," "{" "}" would split wrongly -- names built by
# noms_to_transactions() avoid these characters.

# Consequent palette + line types, assigned in `outcomes` order.
oc_rule_palette   <- c("#1B9E77", "#6A5ACD", "#D95F02", "#C2317E", "#666666")
oc_rule_linetypes <- c("solid", "dashed", "dotted", "dotdash", "longdash")

# Internal: one rules object -> tripartite edge list + rule-node metadata.
rule_network_frames <- function(rules, outcomes, color_map, lty_map) {
  rules_df <- arules::DATAFRAME(rules, separate = TRUE)
  if (nrow(rules_df) == 0) {
    stop("no rules to draw -- the rule set is empty. If the lift > 1 filter ",
         "removed every rule, no bundle beats its status position's base ",
         "rate on this data; if you mined a single outcome under ",
         "arm_unit = \"nomination\", switch to \"member\" (see methods/02).",
         call. = FALSE)
  }

  rhs_seen <- unique(gsub("[{}]", "", as.character(rules_df$RHS)))
  stray <- setdiff(rhs_seen, outcomes)
  if (length(stray) > 0) {
    stop("rule consequents not in `outcomes`: ", paste(stray, collapse = ", "))
  }
  if (any(!is.finite(rules_df$lift))) {
    stop("rules carry NA/non-finite lift -- filter them before plotting")
  }

  edges <- NULL
  rule_nodes <- NULL
  for (i in seq_len(nrow(rules_df))) {
    lhs_items <- trimws(strsplit(gsub("[{}]", "", rules_df$LHS[i]), ",")[[1]])
    rhs_item  <- gsub("[{}]", "", rules_df$RHS[i])
    rule_id   <- paste0("rule_", i)

    edges <- rbind(edges,
      data.frame(from = lhs_items, to = rule_id, edge_type = "antecedent",
                 edge_hue = "grey70", edge_lty = "solid"),
      data.frame(from = rule_id, to = rhs_item, edge_type = "consequent",
                 edge_hue = color_map[rhs_item], edge_lty = lty_map[rhs_item]))
    rule_nodes <- rbind(rule_nodes,
      data.frame(name = rule_id, lift = rules_df$lift[i],
                 conviction = rules_df$conviction[i], rhs = rhs_item))
  }
  list(edges = edges, rule_nodes = rule_nodes)
}

# Internal: frames -> tidygraph with node metadata. Conviction is Inf when a
# rule's confidence is 1 (and NA for some rule forms); cap at the largest
# finite value so the size scale and orbit radii stay defined -- highly
# consolidated data can produce confidence-1 rules.
rule_network_graph <- function(edges, rule_nodes, outcomes) {
  finite_conv <- rule_nodes$conviction[is.finite(rule_nodes$conviction)]
  cap <- if (length(finite_conv) > 0) max(finite_conv) else 1
  rule_nodes$conviction[!is.finite(rule_nodes$conviction)] <- cap

  g <- igraph::graph_from_data_frame(edges, directed = TRUE)

  igraph::V(g)$node_type <- dplyr::case_when(
    igraph::V(g)$name %in% outcomes         ~ "status_position",
    igraph::V(g)$name %in% rule_nodes$name  ~ "rule",
    TRUE                                    ~ "attribute_value")

  lookup <- function(v) setNames(v, rule_nodes$name)[igraph::V(g)$name]
  igraph::V(g)$lift       <- ifelse(igraph::V(g)$node_type == "rule", lookup(rule_nodes$lift), NA)
  igraph::V(g)$conviction <- ifelse(igraph::V(g)$node_type == "rule", lookup(rule_nodes$conviction), NA)
  igraph::V(g)$rhs        <- ifelse(igraph::V(g)$node_type == "rule", lookup(rule_nodes$rhs), NA)
  igraph::V(g)$degree     <- igraph::degree(g, mode = "all")

  tidygraph::as_tbl_graph(g)
}

#' Association-rule network plot (attribute -> rule -> status position).
#'
#' @param rules    arules rules with add_rule_measures() applied (already filtered, e.g. lift > 1)
#' @param outcomes character vector of status positions; order sets color/line type
#' @param labels   named vector dummy-name -> display label (oc_dummy_labels());
#'                 NULL falls back to the raw item names
#' @param seed     layout seed
#' @param antecedent_alpha alpha for the grey antecedent edges; lower it when
#'                 the network is dense and the consequent structure should
#'                 dominate the figure
#' @return ggplot object (network + legend side panel)
plot_rule_network <- function(rules, outcomes, labels = NULL, seed = 42,
                              antecedent_alpha = 0.35) {
  color_map <- setNames(rep_len(oc_rule_palette, length(outcomes)), outcomes)
  lty_map   <- setNames(rep_len(oc_rule_linetypes, length(outcomes)), outcomes)

  frames <- rule_network_frames(rules, outcomes, color_map, lty_map)
  tg <- rule_network_graph(frames$edges, frames$rule_nodes, outcomes)

  rule_conv <- frames$rule_nodes$conviction
  finite_conv <- rule_conv[is.finite(rule_conv)]
  conv_cap <- if (length(finite_conv) > 0) max(finite_conv) else 1
  conv_range <- range(pmin(rule_conv, conv_cap), na.rm = TRUE)
  lift_range <- range(frames$rule_nodes$lift, na.rm = TRUE)

  tg <- tidygraph::mutate(
    tg,
      label = ifelse(node_type == "rule", "",
                     if (is.null(labels)) name
                     else ifelse(is.na(labels[name]), name, labels[name])),
      node_size = dplyr::case_when(
        node_type == "status_position" ~ 5,
        node_type == "rule" ~ scales::rescale(conviction, from = conv_range, to = c(1.2, 3.5)),
        TRUE ~ scales::rescale(degree, to = c(1.5, 4.5))),
      lift_alpha = ifelse(node_type == "rule",
                          scales::rescale(lift, from = lift_range, to = c(0.45, 1.0)),
                          NA))

  # --- layout ----------------------------------------------------------------
  coords <- layout_rule_orbital(tg, outcomes, seed = seed)
  layout_coords <- ggraph::create_layout(tg, layout = "stress")
  idx <- match(layout_coords$name, coords$name)
  layout_coords$x <- coords$x[idx]
  layout_coords$y <- coords$y[idx]

  # --- network panel -----------------------------------------------------------
  p <- ggraph::ggraph(layout_coords) +
    ggraph::geom_edge_link(
      ggplot2::aes(filter = (edge_type == "antecedent")),
      colour = "grey60", alpha = antecedent_alpha, width = 0.3,
      end_cap = ggraph::circle(2, "mm"), start_cap = ggraph::circle(1.5, "mm")) +
    ggraph::geom_edge_link(
      ggplot2::aes(filter = (edge_type == "consequent"),
                   colour = edge_hue, linetype = edge_lty),
      alpha = 0.65, width = 0.55,
      end_cap = ggraph::circle(3, "mm"), start_cap = ggraph::circle(1.5, "mm")) +
    ggraph::scale_edge_colour_identity() +
    ggraph::scale_edge_linetype_identity() +
    ggraph::geom_node_point(
      data = function(d) dplyr::filter(d, node_type == "attribute_value"),
      ggplot2::aes(size = node_size),
      shape = 21, fill = "#E8E0D8", colour = "grey40", stroke = 0.4) +
    ggraph::geom_node_point(
      data = function(d) dplyr::filter(d, node_type == "rule"),
      ggplot2::aes(size = node_size, fill = rhs, alpha = lift_alpha),
      shape = 23, colour = "grey30", stroke = 0.25) +
    ggplot2::scale_fill_manual(values = color_map, guide = "none") +
    ggplot2::scale_alpha_identity() +
    ggraph::geom_node_point(
      data = function(d) dplyr::filter(d, node_type == "status_position"),
      ggplot2::aes(size = node_size),
      shape = 22, fill = "#2C3E50", colour = "#1A252F", stroke = 0.8) +
    ggplot2::scale_size_identity() +
    ggraph::geom_node_label(
      data = function(d) dplyr::filter(d, node_type == "status_position"),
      ggplot2::aes(label = label),
      family = "Helvetica", size = 3.2, colour = "#2C3E50", fontface = "bold",
      fill = ggplot2::alpha("white", 0.85), label.size = 0,
      label.padding = grid::unit(1.5, "pt"), label.r = grid::unit(0, "pt"),
      nudge_y = 0.12) +
    ggraph::geom_node_label(
      data = function(d) dplyr::filter(d, node_type == "attribute_value"),
      ggplot2::aes(label = label),
      family = "Helvetica", size = 2.2, colour = "grey15", fontface = "plain",
      fill = ggplot2::alpha("white", 0.80), label.size = 0,
      label.padding = grid::unit(1, "pt"), label.r = grid::unit(0, "pt"),
      nudge_y = 0.08) +
    # let edge-node labels spill into the margin rather than clip at the
    # panel (e.g. a long attribute label on the far left), and give them
    # room to spill into
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Helvetica"),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(6, 12, 6, 14))

  cowplot::plot_grid(p, rule_network_legend(outcomes, color_map, lty_map),
                     ncol = 2, rel_widths = c(4, 1), align = "v")
}

# Legend as a side panel (built by hand: three node shapes + one edge row
# per consequent, which ggplot guides can't compose from identity scales).
rule_network_legend <- function(outcomes, color_map, lty_map) {
  k <- length(outcomes)
  leg <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 10.5, label = "Nodes",
                      hjust = 0, size = 2.8, colour = "grey25",
                      fontface = "bold", family = "Helvetica") +
    ggplot2::annotate("point", x = 0.3, y = 9.5, shape = 21, size = 3,
                      fill = "#E8E0D8", colour = "grey40", stroke = 0.4) +
    ggplot2::annotate("text", x = 0.8, y = 9.5, label = "Attribute value",
                      hjust = 0, size = 2.3, colour = "grey25", family = "Helvetica") +
    ggplot2::annotate("point", x = 0.1 + 0.2 * (seq_len(k) - 1), y = 8.5,
                      shape = 23, size = 2.5,
                      fill = ggplot2::alpha(color_map, 0.7),
                      colour = "grey30", stroke = 0.25) +
    ggplot2::annotate("text", x = 0.2 * k + 0.4, y = 8.5, label = "Rule",
                      hjust = 0, size = 2.3, colour = "grey25", family = "Helvetica") +
    ggplot2::annotate("text", x = 0.1, y = 7.9, label = "Diamond size ~ conviction",
                      hjust = 0, size = 1.9, colour = "grey45",
                      fontface = "italic", family = "Helvetica") +
    ggplot2::annotate("point", x = 0.3, y = 7.2, shape = 22, size = 3.2,
                      fill = "#2C3E50", colour = "#1A252F", stroke = 0.8) +
    ggplot2::annotate("text", x = 0.8, y = 7.2, label = "Status position",
                      hjust = 0, size = 2.3, colour = "grey25", family = "Helvetica") +
    ggplot2::annotate("text", x = 0, y = 6.0, label = "Edges",
                      hjust = 0, size = 2.8, colour = "grey25",
                      fontface = "bold", family = "Helvetica") +
    ggplot2::annotate("segment", x = 0.05, xend = 1.5, y = 5.2, yend = 5.2,
                      colour = "grey70", linewidth = 0.4) +
    ggplot2::annotate("text", x = 1.7, y = 5.2, label = "Antecedent",
                      hjust = 0, size = 2.1, colour = "grey40", family = "Helvetica")
  for (i in seq_len(k)) {
    y_i <- 4.3 - 0.8 * (i - 1)
    leg <- leg +
      ggplot2::annotate("segment", x = 0.05, xend = 1.5, y = y_i, yend = y_i,
                        colour = color_map[i], linewidth = 0.7,
                        linetype = lty_map[i]) +
      ggplot2::annotate("text", x = 1.7, y = y_i,
                        label = tools::toTitleCase(outcomes[i]),
                        hjust = 0, size = 2.1, colour = color_map[i],
                        family = "Helvetica")
  }
  leg +
    ggplot2::coord_cartesian(xlim = c(-0.1, 3.5),
                             ylim = c(min(2.7, 4.3 - 0.8 * (k - 1)) - 0.5, 11.2),
                             clip = "off") +   # the italic note may overrun the panel
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                   plot.margin = ggplot2::margin(3, 3, 3, 8))
}

# ============================================================================
# Orbital layout
# ============================================================================

layout_rule_orbital <- function(tg, outcomes,
                                polygon_radius = 4.3,
                                orbit_2 = 1.0, orbit_3 = 1.7, orbit_4 = 2.4,
                                attr_orbit = 3.0,
                                conviction_spread = 0.4,
                                lift_angular_weight = 0.6,
                                gravity_orbital = 0.06, gravity_attrs = 0.04,
                                repulsion_strength = 0.20, repulsion_radius = 0.8,
                                intra_cluster_mult = 2.5,
                                cross_cluster_radius = 2.5,
                                cross_cluster_strength = 0.15,
                                damping = 0.88,
                                iterations = 800,
                                cool_start = 1.0, cool_end = 0.01,
                                seed = 42) {
  set.seed(seed)

  nodes <- as.data.frame(tg, what = "vertices")
  el <- igraph::as_edgelist(tidygraph::as.igraph(tg), names = FALSE)
  edges_idx <- data.frame(from = el[, 1], to = el[, 2])
  n <- nrow(nodes)
  k <- length(outcomes)

  # ---- Pin status positions to a regular polygon ----
  angles <- pi / 2 + pi / k + 2 * pi * (seq_len(k) - 1) / k
  status_coords <- data.frame(
    name = outcomes,
    x = polygon_radius * cos(angles),
    y = polygon_radius * sin(angles))
  center <- c(0, 0)

  rule_cluster <- setNames(nodes$rhs[nodes$node_type == "rule"],
                           nodes$name[nodes$node_type == "rule"])

  # Antecedent counts + sets per rule (sets feed the Jaccard ordering)
  rule_n_antecedents <- setNames(rep(0, n), nodes$name)
  rule_antecedent_sets <- setNames(vector("list", n), nodes$name)
  attr_weights <- list()
  attr_connected_rules <- vector("list", n)
  for (i in seq_len(nrow(edges_idx))) {
    from_i <- edges_idx$from[i]; to_i <- edges_idx$to[i]
    if (nodes$node_type[from_i] == "attribute_value" &&
        nodes$node_type[to_i] == "rule") {
      rule_n_antecedents[to_i] <- rule_n_antecedents[to_i] + 1
      rule_antecedent_sets[[to_i]] <- c(rule_antecedent_sets[[to_i]], nodes$name[from_i])
      attr_connected_rules[[from_i]] <- c(attr_connected_rules[[from_i]], to_i)
      attr_name <- nodes$name[from_i]
      rule_rhs <- rule_cluster[nodes$name[to_i]]
      if (!is.na(rule_rhs)) {
        if (is.null(attr_weights[[attr_name]]))
          attr_weights[[attr_name]] <- setNames(rep(0, k), outcomes)
        attr_weights[[attr_name]][rule_rhs] <- attr_weights[[attr_name]][rule_rhs] + 1
      }
    }
  }

  # Orbit radius per rule: ring by antecedent count, then conviction pulls
  # toward the nominal radius (low conviction drifts outward)
  conv_range <- range(nodes$conviction[nodes$node_type == "rule"], na.rm = TRUE)
  rule_orbit_radius <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    if (nodes$node_type[i] != "rule") next
    na_count <- rule_n_antecedents[i]
    base_r <- if (na_count <= 2) orbit_2 else if (na_count == 3) orbit_3 else orbit_4
    conv <- nodes$conviction[i]
    rule_orbit_radius[i] <- if (!is.na(conv) && conv_range[2] > conv_range[1]) {
      conv_norm <- (conv - conv_range[1]) / (conv_range[2] - conv_range[1])
      base_r + conviction_spread * (1 - conv_norm)
    } else base_r
  }

  polysemous <- sapply(attr_weights, function(w) sum(w > 0) >= 2)

  rule_target_idx <- rep(NA_integer_, n)
  for (i in seq_len(n)) {
    if (nodes$node_type[i] == "rule") {
      rhs <- rule_cluster[nodes$name[i]]
      if (!is.na(rhs)) rule_target_idx[i] <- which(nodes$name == rhs)
    }
  }

  # ---- Jaccard-similar rules adjacent, high lift front-facing ----
  jaccard <- function(a, b) {
    if (length(a) == 0 && length(b) == 0) return(1)
    length(intersect(a, b)) / length(union(a, b))
  }
  compute_angular_order <- function(rule_indices) {
    nr <- length(rule_indices)
    if (nr <= 1) return(rule_indices)
    lifts <- nodes$lift[rule_indices]
    lifts[is.na(lifts)] <- 0
    jac_mat <- matrix(0, nr, nr)
    for (i in seq_len(nr)) for (j in seq_len(nr)) if (i != j) {
      jac_mat[i, j] <- 1 - jaccard(rule_antecedent_sets[[rule_indices[i]]],
                                   rule_antecedent_sets[[rule_indices[j]]])
    }
    start <- which.max(lifts)
    visited <- logical(nr); ord <- integer(nr)
    ord[1] <- start; visited[start] <- TRUE
    for (m in seq_len(nr - 1) + 1) {
      dists <- jac_mat[ord[m - 1], ]; dists[visited] <- Inf
      ord[m] <- which.min(dists); visited[ord[m]] <- TRUE
    }
    ordered_indices <- rule_indices[ord]
    if (lift_angular_weight > 0) {
      # center the highest-lift rules in the arc (facing the figure center)
      rank_by_lift <- rank(-lifts[ord], ties.method = "first")
      center_order <- integer(nr)
      left <- 1; right <- nr; toggle <- TRUE
      for (m in seq_len(nr)) {
        pos <- which(rank_by_lift == m)
        if (toggle) { center_order[left] <- pos; left <- left + 1 }
        else { center_order[right] <- pos; right <- right - 1 }
        toggle <- !toggle
      }
      ordered_indices <- ordered_indices[center_order]
    }
    ordered_indices
  }

  # ---- Initialize positions ----
  x <- rep(0, n); y <- rep(0, n)
  vx <- rep(0, n); vy <- rep(0, n)
  fixed <- rep(FALSE, n)

  for (i in seq_len(n)) {
    if (nodes$node_type[i] == "status_position") {
      idx <- which(status_coords$name == nodes$name[i])
      x[i] <- status_coords$x[idx]; y[i] <- status_coords$y[idx]
      fixed[i] <- TRUE
    }
  }

  # Rules: arc around their consequent, ordered by Jaccard + lift
  rule_groups <- list()
  for (i in seq_len(n)) {
    if (nodes$node_type[i] != "rule") next
    rhs <- rule_cluster[nodes$name[i]]
    if (is.na(rhs)) next
    key <- paste0(rhs, "\r", min(rule_n_antecedents[i], 4))
    rule_groups[[key]] <- c(rule_groups[[key]], i)
  }
  for (key in names(rule_groups)) {
    indices <- rule_groups[[key]]
    rhs <- strsplit(key, "\r")[[1]][1]
    sp_idx <- which(status_coords$name == rhs)
    sp_x <- status_coords$x[sp_idx]; sp_y <- status_coords$y[sp_idx]
    ordered <- compute_angular_order(indices)
    total <- length(ordered)
    base_angle <- atan2(sp_y - center[2], sp_x - center[1])
    arc_span <- pi * 1.5
    for (m in seq_along(ordered)) {
      i <- ordered[m]
      angle <- base_angle - arc_span / 2 + arc_span * ((m - 0.5) / total)
      x[i] <- sp_x + rule_orbit_radius[i] * cos(angle) + rnorm(1, 0, 0.03)
      y[i] <- sp_y + rule_orbit_radius[i] * sin(angle) + rnorm(1, 0, 0.03)
    }
  }

  # Attributes: exclusive ones orbit their cluster; polysemous ones sit at
  # the boundary between the clusters they feed (log-dampened weights)
  max_conn <- max(sapply(seq_len(n), function(j)
    if (nodes$node_type[j] == "attribute_value") length(attr_connected_rules[[j]]) else 0))
  for (i in seq_len(n)) {
    if (nodes$node_type[i] != "attribute_value" || fixed[i]) next
    nm <- nodes$name[i]
    w <- attr_weights[[nm]]
    connected <- attr_connected_rules[[i]]
    n_conn <- length(connected)
    if (is.null(w) || sum(w) == 0) {
      x[i] <- rnorm(1, 0, 1); y[i] <- rnorm(1, 0, 1)
      next
    }
    if (isTRUE(polysemous[nm])) {
      active <- names(w[w > 0])
      w_log <- log(1 + w[active])
      w_norm <- w_log / sum(w_log)
      if (length(active) == 2) {
        sp1 <- which(status_coords$name == active[1])
        sp2 <- which(status_coords$name == active[2])
        w1 <- w_norm[active[1]]
        mid_x <- status_coords$x[sp1] + w1 * (status_coords$x[sp2] - status_coords$x[sp1])
        mid_y <- status_coords$y[sp1] + w1 * (status_coords$y[sp2] - status_coords$y[sp1])
        # push perpendicular to the cluster-cluster axis, outward from center
        perp_x <- -(status_coords$y[sp2] - status_coords$y[sp1])
        perp_y <- status_coords$x[sp2] - status_coords$x[sp1]
        if (perp_x * (mid_x - center[1]) + perp_y * (mid_y - center[2]) < 0) {
          perp_x <- -perp_x; perp_y <- -perp_y
        }
        perp_len <- sqrt(perp_x^2 + perp_y^2)
        if (perp_len > 0.001) { perp_x <- perp_x / perp_len; perp_y <- perp_y / perp_len }
        push <- attr_orbit * 0.4 + 0.08 * n_conn
        x[i] <- mid_x + push * perp_x + rnorm(1, 0, 0.15)
        y[i] <- mid_y + push * perp_y + rnorm(1, 0, 0.15)
      } else {
        tx <- sum(w_norm * status_coords$x[match(active, status_coords$name)])
        ty <- sum(w_norm * status_coords$y[match(active, status_coords$name)])
        dx <- tx - center[1]; dy <- ty - center[2]
        dist <- sqrt(dx^2 + dy^2)
        if (dist > 0.001) {
          push <- attr_orbit * 0.4 + 0.08 * n_conn
          tx <- tx + push * dx / dist; ty <- ty + push * dy / dist
        }
        x[i] <- tx + rnorm(1, 0, 0.15); y[i] <- ty + rnorm(1, 0, 0.15)
      }
    } else {
      primary <- names(which.max(w))
      sp_idx <- which(status_coords$name == primary)
      sp_x <- status_coords$x[sp_idx]; sp_y <- status_coords$y[sp_idx]
      rule_cx <- mean(x[connected]); rule_cy <- mean(y[connected])
      rule_dx <- rule_cx - sp_x; rule_dy <- rule_cy - sp_y
      angle <- if (sqrt(rule_dx^2 + rule_dy^2) > 0.001) atan2(rule_dy, rule_dx)
               else atan2(sp_y - center[2], sp_x - center[1])
      degree_scale <- 0.4 + 0.8 * ((n_conn - 1) / max(max_conn - 1, 1))
      target_dist <- (orbit_4 + 0.4) + degree_scale * (attr_orbit - orbit_4 - 0.4)
      x[i] <- sp_x + target_dist * cos(angle) + rnorm(1, 0, 0.15)
      y[i] <- sp_y + target_dist * sin(angle) + rnorm(1, 0, 0.15)
    }
  }

  # ---- Force-directed refinement ----
  for (iter in seq_len(iterations)) {
    temp <- cool_start + (cool_end - cool_start) * (iter / iterations)
    fx <- rep(0, n); fy <- rep(0, n)

    for (i in seq_len(n)) {
      if (fixed[i]) next
      # rules hold their orbit distance
      if (nodes$node_type[i] == "rule" && !is.na(rule_target_idx[i])) {
        ti <- rule_target_idx[i]
        dx <- x[i] - x[ti]; dy <- y[i] - y[ti]
        dist <- sqrt(dx^2 + dy^2)
        if (dist > 0.001) {
          radial_error <- dist - rule_orbit_radius[i]
          fx[i] <- fx[i] - gravity_orbital * radial_error * (dx / dist)
          fy[i] <- fy[i] - gravity_orbital * radial_error * (dy / dist)
        }
        # repelled by the other status positions
        for (sp in seq_len(n)) {
          if (nodes$node_type[sp] != "status_position" || sp == ti) next
          dx <- x[i] - x[sp]; dy <- y[i] - y[sp]
          dist <- sqrt(dx^2 + dy^2)
          if (dist < cross_cluster_radius && dist > 0.001) {
            push <- cross_cluster_strength * (cross_cluster_radius - dist) / dist
            fx[i] <- fx[i] + push * dx; fy[i] <- fy[i] + push * dy
          }
        }
      }
      # attributes drift toward their connected rules (weakly if polysemous)
      if (nodes$node_type[i] == "attribute_value") {
        connected <- attr_connected_rules[[i]]
        if (length(connected) > 0) {
          grav <- if (isTRUE(polysemous[nodes$name[i]])) gravity_attrs * 0.3 else gravity_attrs
          fx[i] <- fx[i] + grav * (mean(x[connected]) - x[i])
          fy[i] <- fy[i] + grav * (mean(y[connected]) - y[i])
        }
      }
    }

    for (i in seq_len(n - 1)) {
      if (fixed[i]) next
      for (j in (i + 1):n) {
        dx <- x[j] - x[i]; dy <- y[j] - y[i]
        dist <- sqrt(dx^2 + dy^2)
        if (dist < repulsion_radius && dist > 0.001) {
          mult <- if (nodes$node_type[i] == "rule" && nodes$node_type[j] == "rule" &&
                      !is.na(rule_cluster[nodes$name[i]]) &&
                      identical(rule_cluster[nodes$name[i]], rule_cluster[nodes$name[j]]))
            intra_cluster_mult else 1.0
          force <- repulsion_strength * mult * (repulsion_radius - dist) / dist
          if (!fixed[i]) { fx[i] <- fx[i] - force * dx; fy[i] <- fy[i] - force * dy }
          if (!fixed[j]) { fx[j] <- fx[j] + force * dx; fy[j] <- fy[j] + force * dy }
        }
      }
    }

    for (i in seq_len(n)) {
      if (fixed[i]) next
      vx[i] <- (vx[i] + fx[i] * temp) * damping
      vy[i] <- (vy[i] + fy[i] * temp) * damping
      x[i] <- x[i] + vx[i]; y[i] <- y[i] + vy[i]
    }

    # attributes stay outside the outermost rule orbit
    for (i in seq_len(n)) {
      if (fixed[i] || nodes$node_type[i] != "attribute_value") next
      connected <- attr_connected_rules[[i]]
      if (length(connected) == 0) next
      nm <- nodes$name[i]
      w <- attr_weights[[nm]]
      if (isTRUE(polysemous[nm])) {
        dx <- x[i] - center[1]; dy <- y[i] - center[2]
        dist <- sqrt(dx^2 + dy^2)
        min_dist <- orbit_4 * 0.8   # polysemous attributes sit at cluster
                                    # boundaries, so their floor is lower
        if (dist < min_dist && dist > 0.001) {
          x[i] <- center[1] + dx * min_dist / dist
          y[i] <- center[2] + dy * min_dist / dist
        }
      } else if (!is.null(w) && sum(w) > 0) {
        primary <- names(which.max(w))
        sp_idx <- which(status_coords$name == primary)
        dx <- x[i] - status_coords$x[sp_idx]; dy <- y[i] - status_coords$y[sp_idx]
        dist <- sqrt(dx^2 + dy^2)
        degree_scale <- 0.4 + 0.8 * ((length(connected) - 1) / max(max_conn - 1, 1))
        min_dist <- orbit_4 + degree_scale
        if (dist < min_dist && dist > 0.001) {
          x[i] <- status_coords$x[sp_idx] + dx * min_dist / dist
          y[i] <- status_coords$y[sp_idx] + dy * min_dist / dist
        }
      }
    }
  }

  data.frame(name = nodes$name, x = x, y = y, stringsAsFactors = FALSE)
}
