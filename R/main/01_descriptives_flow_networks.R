# ==============================================================================
# 01_descriptives_flow_networks.R
#
# Descriptive figures for the directional skill diffusion paper:
#
#   Fig. 1  Aggregate flow networks — Cognitive vs Physical (wage quintiles)
#   Fig. 2  Individual-skill flow networks — top 6 per domain
#   Fig. 3  Binned adoption rates vs structural distance and signed wage gap
#
# Inputs:
#   Loaded via R/00_setup_comun.R → object `dt` (17.3M rows, diffusion == 1/0)
#   Required columns: diffusion, domain, s_wage, t_wage, wage_gap,
#                     structural_distance, skill_name
#
# Outputs  (all written to output/output_main/figs/):
#   fig1_flow_networks_aggregate.pdf
#   fig2_flow_networks_skills.pdf
#   fig3_adoption_gradients.pdf
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------------------
source("R/00_setup_comun.R")   # loads dt, sets fig_dir, defines colour palette

library(ggraph)
library(igraph)
library(patchwork)
library(stringr)
library(scales)
library(tibble)

fig_dir <- file.path("output", "output_main", "figs")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# Aesthetic constants
COL_UP   <- "#2166AC"
COL_DOWN <- "#B2182B"
pal_direction <- c(Up = COL_UP, Down = COL_DOWN)
pal_domain    <- c(Cognitive = "#2ABAB2", Physical = "#888888")

FONT <- "sans"   # swap to "Inconsolata" if available

arrow_sharp <- arrow(angle = 10, length = unit(6, "mm"), type = "closed")

theme_flow <- theme_void() +
  theme(
    plot.title    = element_text(face = "bold", size = 28, hjust = 0.5, family = FONT),
    plot.subtitle = element_text(size = 20, hjust = 0.5, face = "bold", family = FONT),
    plot.margin   = margin(25, 10, 25, 10)
  )

theme_gradient <- theme_minimal(base_size = 18) +
  theme(
    text              = element_text(family = FONT),
    plot.title        = element_text(face = "bold", size = 20),
    axis.title.x      = element_text(margin = margin(t = 10)),
    axis.title.y      = element_text(margin = margin(r = 10)),
    legend.position   = "bottom",
    legend.title      = element_blank(),
    panel.grid.minor  = element_blank()
  )

# ------------------------------------------------------------------------------
# 1. Wage quintile assignment and direction
# ------------------------------------------------------------------------------
message(">>> Step 1: Assigning wage quintiles and diffusion direction...")

q_breaks <- quantile(dt$s_wage, probs = seq(0, 1, 0.2), na.rm = TRUE)

dt[, wage_q_src  := cut(s_wage, breaks = q_breaks,
                         labels = paste0("Q", 1:5), include.lowest = TRUE)]
dt[, wage_q_dest := cut(t_wage, breaks = q_breaks,
                         labels = paste0("Q", 1:5), include.lowest = TRUE)]
dt[, direction := fcase(
  as.integer(wage_q_dest) > as.integer(wage_q_src), "Up",
  as.integer(wage_q_dest) < as.integer(wage_q_src), "Down",
  default = "Lateral"
)]

# Aggregate adoption rates by domain and direction (for subtitles)
stats_dir <- dt[direction != "Lateral", .(
  rate = mean(diffusion, na.rm = TRUE)
), by = .(domain, direction)]

get_rate <- function(dom, dir) {
  r <- stats_dir[domain == dom & direction == dir, rate]
  if (length(r) == 0) return(NA_real_)
  r
}

text_cog <- sprintf(
  "Upward adoption rate: %.1f%%  |  Downward: %.1f%%",
  get_rate("Cognitive", "Up")   * 100,
  get_rate("Cognitive", "Down") * 100
)
text_phy <- sprintf(
  "Downward adoption rate: %.1f%%  |  Upward: %.1f%%",
  get_rate("Physical", "Down") * 100,
  get_rate("Physical", "Up")   * 100
)

message("  ", text_cog)
message("  ", text_phy)

# ==============================================================================
# Fig. 1 — Aggregate flow networks (Cognitive vs Physical)
# ==============================================================================
message("\n>>> Fig. 1: Aggregate flow networks...")

rate_by_edge <- dt[direction != "Lateral", .(
  rate = mean(diffusion, na.rm = TRUE)
), by = .(domain, wage_q_src, wage_q_dest, direction)]
rate_by_edge[, rate_norm := rate / max(rate), by = domain]

verts <- tibble(name = paste0("Q", 1:5), x = 1:5, y = 0)

make_flow_panel <- function(dom, subtitle_str) {
  edges <- rate_by_edge[domain == dom][order(-rate)][1:8]
  g <- graph_from_data_frame(
    d        = edges[, .(from = wage_q_src, to = wage_q_dest,
                         weight = rate_norm, direction)],
    vertices = verts,
    directed = TRUE
  )
  sub_col <- if (dom == "Cognitive") COL_UP else COL_DOWN
  ggraph(g, layout = "linear") +
    geom_edge_arc(
      aes(edge_alpha = weight, edge_width = weight, edge_colour = direction),
      strength = 0.35, arrow = arrow_sharp,
      end_cap = circle(10, "mm"), show.legend = FALSE
    ) +
    scale_edge_width(range = c(2, 16)) +
    scale_edge_alpha(range = c(0.4, 1)) +
    scale_edge_colour_manual(values = pal_direction) +
    geom_node_point(size = 24, color = "black", fill = "white", shape = 21, stroke = 2.5) +
    geom_node_text(aes(label = name), vjust = 0.4, size = 9,
                   fontface = "bold", family = FONT) +
    labs(
      title    = paste(dom, "Skills"),
      subtitle = subtitle_str
    ) +
    theme_flow +
    theme(plot.subtitle = element_text(color = sub_col))
}

p_legend_fig1 <- ggplot() +
  annotate("segment", x = 1, xend = 1.8, y = 0.5, yend = 0.5, color = COL_UP,
           linewidth = 4, arrow = arrow(angle = 10, length = unit(6, "mm"), type = "closed")) +
  annotate("text", x = 2, y = 0.5, label = "Upward diffusion",
           color = COL_UP,   size = 8, fontface = "bold", hjust = 0, family = FONT) +
  annotate("segment", x = 4.5, xend = 5.3, y = 0.5, yend = 0.5, color = COL_DOWN,
           linewidth = 4, arrow = arrow(angle = 10, length = unit(6, "mm"), type = "closed")) +
  annotate("text", x = 5.5, y = 0.5, label = "Downward diffusion",
           color = COL_DOWN, size = 8, fontface = "bold", hjust = 0, family = FONT) +
  xlim(0.5, 8.5) + ylim(0.4, 0.6) + theme_void()

caption_fig1 <- str_wrap(paste0(
  "Note: Adoption defined as skill k crossing RCA \u2265 1 in occupation j at t1, ",
  "conditional on j not specialised at t0 and source i specialised at t0. ",
  "Rates represent the empirical mean probability across 17.3M transitions ",
  "in the O*NET rolling panel (2015\u20132024). ",
  "Edge thickness proportional to adoption rate. ",
  "Nodes = wage quintiles (Q1 lowest \u2192 Q5 highest)."
), width = 115)

p_fig1 <- (make_flow_panel("Cognitive", text_cog) |
            make_flow_panel("Physical",  text_phy)) /
          p_legend_fig1 +
  plot_layout(heights = c(10, 1.2)) +
  plot_annotation(
    caption = caption_fig1,
    theme = theme(
      plot.caption          = element_text(size = 14, hjust = 0.5, family = FONT,
                                           color = "grey30", lineheight = 1.1,
                                           margin = margin(t = 18, b = 8)),
      plot.caption.position = "plot"
    )
  )

ggsave(
  file.path(fig_dir, "fig1_flow_networks_aggregate.pdf"),
  p_fig1, width = 20, height = 12, device = cairo_pdf
)
message("  Saved: fig1_flow_networks_aggregate.pdf")

# ==============================================================================
# Fig. 2 — Individual-skill flow networks (3 × 2 grids)
# ==============================================================================
message("\n>>> Fig. 2: Individual-skill flow networks...")

skill_stats <- dt[direction != "Lateral", .(
  rate    = mean(diffusion, na.rm = TRUE),
  n_total = .N
), by = .(skill_name, domain, direction)]

# Top 6 Cognitive skills by upward success (min 5000 opportunities)
top_cog <- skill_stats[domain == "Cognitive" & direction == "Up" & n_total > 5000][
  order(-rate)][1:6, skill_name]

# Top 6 Physical skills by downward propagation rate
top_phy <- skill_stats[domain == "Physical" & direction == "Down" & n_total > 5000][
  order(-rate)][1:6, skill_name]

skill_edges <- dt[skill_name %in% c(top_cog, top_phy) & direction != "Lateral", .(
  rate = mean(diffusion, na.rm = TRUE)
), by = .(skill_name, domain, wage_q_src, wage_q_dest, direction)]
skill_edges[, rate_norm := rate / max(rate), by = skill_name]

render_skill_panel <- function(s_name) {
  skill_data <- skill_edges[skill_name == s_name]
  g <- graph_from_data_frame(
    d        = skill_data[, .(from = wage_q_src, to = wage_q_dest,
                              weight = rate_norm, direction)],
    vertices = verts,
    directed = TRUE
  )
  ggraph(g, layout = "linear") +
    geom_edge_arc(
      aes(edge_alpha = weight, edge_width = weight, edge_colour = direction),
      strength = 0.3, arrow = arrow(angle = 10, length = unit(4, "mm"), type = "closed"),
      end_cap = circle(5, "mm"), show.legend = FALSE
    ) +
    scale_edge_width(range = c(0.8, 6)) +
    scale_edge_alpha(range = c(0.3, 1)) +
    scale_edge_colour_manual(values = pal_direction) +
    geom_node_point(size = 12, color = "black", fill = "white", shape = 21, stroke = 1.2) +
    geom_node_text(aes(label = name), vjust = 0.4, size = 4,
                   fontface = "bold", family = FONT) +
    labs(title = str_wrap(s_name, width = 30)) +
    theme_void() +
    theme(
      plot.title  = element_text(face = "bold", size = 14, hjust = 0.5,
                                 family = FONT, lineheight = 0.9,
                                 margin = margin(b = 8)),
      plot.margin = margin(10, 5, 10, 5)
    )
}

caption_skills <- str_wrap(paste0(
  "Note: Adoption rates = empirical mean probability of RCA \u2265 1 at t1 ",
  "within the O*NET rolling panel (2015\u20132024). ",
  "Edge thickness normalised by maximum rate per skill for cross-panel legibility."
), width = 120)

grid_cog <- wrap_plots(lapply(top_cog, render_skill_panel), ncol = 3) +
  plot_annotation(
    title   = "Cognitive Skills: Top 6 by Upward Adoption Rate",
    caption = caption_skills,
    theme   = theme(
      plot.title   = element_text(size = 22, face = "bold", family = FONT, color = COL_UP),
      plot.caption = element_text(size = 12, family = FONT, color = "grey30",
                                  hjust = 0.5, margin = margin(t = 14))
    )
  )

grid_phy <- wrap_plots(lapply(top_phy, render_skill_panel), ncol = 3) +
  plot_annotation(
    title   = "Physical Skills: Top 6 by Downward Propagation Rate",
    caption = caption_skills,
    theme   = theme(
      plot.title   = element_text(size = 22, face = "bold", family = FONT, color = COL_DOWN),
      plot.caption = element_text(size = 12, family = FONT, color = "grey30",
                                  hjust = 0.5, margin = margin(t = 14))
    )
  )

p_fig2 <- grid_cog / grid_phy

ggsave(
  file.path(fig_dir, "fig2_flow_networks_skills.pdf"),
  p_fig2, width = 20, height = 18, device = cairo_pdf
)
message("  Saved: fig2_flow_networks_skills.pdf")

# ==============================================================================
# Fig. 3 — Binned adoption rate gradients (structural distance & wage gap)
# ==============================================================================
message("\n>>> Fig. 3: Adoption rate gradients...")

NBINS  <- 100L
Y_LIM  <- 0.40

add_ci <- function(D) {
  D[, se  := sqrt((rate * (1 - rate)) / pmax(n, 1))]
  D[, low  := pmax(rate - 1.96 * se, 0)]
  D[, high := pmin(rate + 1.96 * se, 1)]
  D
}

# Panel A: structural distance (cap at 0.9 to suppress sparse tail)
dt_a <- dt[structural_distance <= 0.9]
dt_a[, dist_bin := as.integer(NBINS * (frank(structural_distance,
                                             ties.method = "random") - 1) / .N) + 1L]
stats_dist <- dt_a[, .(x = mean(structural_distance), rate = mean(diffusion), n = .N),
                   by = .(domain, dist_bin)]
stats_dist <- add_ci(stats_dist)

p_dist <- ggplot(stats_dist, aes(x = x, y = rate, color = domain, fill = domain)) +
  geom_point(size = 1.8, alpha = 0.4) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 10),
              se = FALSE, linewidth = 1.5) +
  scale_color_manual(values = pal_domain) +
  scale_fill_manual(values  = pal_domain) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, Y_LIM),
                     breaks = seq(0, Y_LIM, 0.10)) +
  labs(title = "(A)", x = "Structural distance (cosine, RCA-weighted)",
       y = "Adoption rate") +
  theme_gradient

# Panel B: signed wage gap
dt[, signed_bin := as.integer(NBINS * (frank(wage_gap,
                                             ties.method = "random") - 1) / .N) + 1L]
stats_gap <- dt[, .(x = mean(wage_gap), rate = mean(diffusion), n = .N),
                by = .(domain, signed_bin)]
stats_gap <- add_ci(stats_gap)

p_gap <- ggplot(stats_gap, aes(x = x, y = rate, color = domain, fill = domain)) +
  geom_point(size = 1.8, alpha = 0.4) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 10),
              se = FALSE, linewidth = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  annotate("text", x = quantile(stats_gap$x, 0.02, na.rm = TRUE),
           y = Y_LIM * 0.88,
           label = "Downward\n(target < source)", size = 4, color = "grey35", hjust = 0) +
  annotate("text", x = quantile(stats_gap$x, 0.95, na.rm = TRUE),
           y = Y_LIM * 0.88,
           label = "Upward\n(target > source)", size = 4, color = "grey35", hjust = 1) +
  scale_color_manual(values = pal_domain) +
  scale_fill_manual(values  = pal_domain) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, Y_LIM),
                     breaks = seq(0, Y_LIM, 0.10)) +
  labs(title = "(B)", x = "Signed log wage gap (target \u2212 source)", y = NULL) +
  theme_gradient

p_fig3 <- (p_dist | p_gap) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14))

ggsave(
  file.path(fig_dir, "fig3_adoption_gradients.pdf"),
  p_fig3, width = 14, height = 6, device = cairo_pdf
)
message("  Saved: fig3_adoption_gradients.pdf")

# ==============================================================================
# Summary
# ==============================================================================
message("\n>>> Descriptive figures complete.")
message(sprintf("  Outputs written to: %s", fig_dir))
message(sprintf("  Cognitive  up: %.1f%%  down: %.1f%%",
                get_rate("Cognitive","Up")*100, get_rate("Cognitive","Down")*100))
message(sprintf("  Physical   up: %.1f%%  down: %.1f%%",
                get_rate("Physical","Up")*100,  get_rate("Physical","Down")*100))
message(sprintf("  Physical Down/Up ratio: %.2fx",
                get_rate("Physical","Down") / get_rate("Physical","Up")))