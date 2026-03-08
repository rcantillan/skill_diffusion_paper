# ==============================================================================
# 01_descriptives_flow_networks.R
#
#   Fig. 1  Composite arc figure: aggregate arc (top) + 2×3 skill grid (bottom)
#           per domain. Skill grid has a single domain-coloured border wrapping
#           all 6 panels — no individual cell backgrounds.
#   Fig. 2  Adoption rate gradients vs skill relatedness and wage gap.
#           Single legend, no duplication.
#
# Inputs:
#   dt             loaded via 00_setup_comun.R (dt_con_cs_nestedness.rds)
#                  columns: diffusion, domain, wage_gap, structural_distance,
#                           source, target, skill_name, wage_up, wage_down,
#                           up_dummy
#   PATH_DYADS_V12 all_events_final_enriched_REAL.rds — s_wage / t_wage
#
# Outputs (output/output_main/figs/):
#   fig1_composite_arc_skills.pdf
#   fig2_adoption_gradients.pdf
# ==============================================================================

# ggplot2 MUST load first — prevents ffi_list2 session corruption
library(ggplot2)
library(data.table)
library(ggraph)
library(igraph)
library(patchwork)
library(scales)
library(stringr)
library(tibble)
library(grid)       # for rectGrob background trick

source("R/00_setup_comun.R")

fig_dir <- file.path("output", "output_main", "figs")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# Aesthetic constants
# ------------------------------------------------------------------------------
PAL_DOMAIN <- c("Socio-cognitive"  = "#2ABAB2",
                "Physical-sensory" = "#CC4444")
PAL_DIR    <- c(Up   = "#2166AC",   # blue  = upward
                Down = "#B2182B")   # red   = downward
FONT       <- "Helvetica"

theme_sciadv <- theme_classic(base_size = 7, base_family = FONT) +
  theme(
    strip.text        = element_text(size = 7, face = "plain",
                                     margin = margin(b = 3, t = 3)),
    strip.background  = element_blank(),
    axis.title.x      = element_text(size = 7, margin = margin(t = 5)),
    axis.title.y      = element_text(size = 7, margin = margin(r = 5)),
    axis.text         = element_text(size = 6),
    axis.ticks        = element_line(linewidth = 0.3, colour = "grey30"),
    axis.ticks.length = unit(2, "pt"),
    panel.grid        = element_blank(),
    panel.border      = element_rect(colour = "grey30", linewidth = 0.4,
                                     fill = NA),
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.margin       = margin(6, 8, 6, 6, "pt"),
    legend.position   = "none"   # default: suppress; shown explicitly where needed
  )

theme_flow <- theme_void(base_family = FONT) +
  theme(
    plot.title    = element_text(face = "bold", size = 14, hjust = 0.5,
                                 colour = "grey10", family = FONT,
                                 margin = margin(b = 3)),
    plot.subtitle = element_text(size = 10, hjust = 0.5, family = FONT,
                                 margin = margin(b = 12), colour = "grey35"),
    plot.margin   = margin(10, 18, 18, 18)
  )

# Open arrows: narrower angle + "open" type = sharp defined arrowhead, not fat
arrow_flow    <- arrow(angle = 18, length = unit(4, "mm"), type = "open", ends = "last")
arrow_flow_sm <- arrow(angle = 18, length = unit(2.8, "mm"), type = "open", ends = "last")
verts         <- tibble(name = paste0("Q", 1:5), x = 1:5, y = 0)

# ------------------------------------------------------------------------------
# 1. Merge s_wage / t_wage from v12
# ------------------------------------------------------------------------------
message(">>> Step 1: Merging wage levels from v12 dataset...")

if (!file.exists("R/99_paths_local.R"))
  stop("R/99_paths_local.R not found.")
source("R/99_paths_local.R")

dt_wages <- readRDS(PATH_DYADS_V12)[
  , .(s_wage = s_wage[1L], t_wage = t_wage[1L]),
  by = .(source, target, skill_name)
]

# In-place join — avoids column name conflicts
dt[dt_wages,
   on  = c("source", "target", "skill_name"),
   `:=`(s_wage = i.s_wage, t_wage = i.t_wage)]

rm(dt_wages); gc(); gc()
message(sprintf("  Non-NA s_wage: %s rows",
                format(sum(!is.na(dt[["s_wage"]])), big.mark = ",")))

# ------------------------------------------------------------------------------
# 2. Quintile + direction coding
# ------------------------------------------------------------------------------
message(">>> Step 2: Quintiles and direction...")

q_breaks <- quantile(dt[["s_wage"]], probs = seq(0, 1, 0.2), na.rm = TRUE)

dt[, wage_q_src  := cut(s_wage, breaks = q_breaks,
                         labels = paste0("Q", 1:5), include.lowest = TRUE)]
dt[, wage_q_dest := cut(t_wage, breaks = q_breaks,
                         labels = paste0("Q", 1:5), include.lowest = TRUE)]
dt[, direction   := fcase(
  as.integer(wage_q_dest) > as.integer(wage_q_src), "Up",
  as.integer(wage_q_dest) < as.integer(wage_q_src), "Down",
  default = "Lateral"
)]
dt[, domain_label := fcase(
  domain == "Cognitive", "Socio-cognitive",
  domain == "Physical",  "Physical-sensory",
  default               , domain
)]

stats_dir <- dt[direction != "Lateral", .(
  rate = mean(diffusion, na.rm = TRUE), n = .N
), by = .(domain_label, direction)]

get_rate <- function(dom, dir) {
  r <- stats_dir[domain_label == dom & direction == dir, rate]
  if (!length(r)) return(NA_real_); r
}

subtitle_cog <- sprintf("Upward: %.1f%%  |  Downward: %.1f%%",
                        get_rate("Socio-cognitive",  "Up")   * 100,
                        get_rate("Socio-cognitive",  "Down") * 100)
subtitle_phy <- sprintf("Downward: %.1f%%  |  Upward: %.1f%%",
                        get_rate("Physical-sensory", "Down") * 100,
                        get_rate("Physical-sensory", "Up")   * 100)

# ==============================================================================
# Fig. 1 — Composite arc figure
# ==============================================================================
message("\n>>> Fig. 1...")

# Aggregate edge rates
rate_by_edge <- dt[direction != "Lateral", .(
  rate = mean(diffusion, na.rm = TRUE), n = .N
), by = .(domain_label, wage_q_src, wage_q_dest, direction)]
rate_by_edge <- rate_by_edge[n >= 500L]
rate_by_edge[, rate_norm := rate / max(rate, na.rm = TRUE), by = domain_label]

# ---- Aggregate arc panel ----------------------------------------------------
make_arc_agg <- function(dom, subtitle_str, show_legend = FALSE) {
  edges   <- rate_by_edge[domain_label == dom]
  dom_col <- PAL_DOMAIN[[dom]]
  if (!nrow(edges)) return(ggplot() + theme_void())

  g <- igraph::graph_from_data_frame(
    d        = edges[, .(from = wage_q_src, to = wage_q_dest,
                         weight = rate_norm, direction)],
    vertices = verts, directed = TRUE
  )

  p <- ggraph(g, layout = "linear") +
    geom_edge_arc(
      aes(edge_alpha = weight, edge_width = weight, edge_colour = direction),
      strength = 0.45, arrow = arrow_flow, end_cap = circle(7, "mm"),
      show.legend = show_legend
    ) +
    scale_edge_width(range = c(0.3, 3.5), guide = "none") +
    scale_edge_alpha(range = c(0.30, 1.0), guide = "none") +
    scale_edge_colour_manual(
      values = PAL_DIR,
      labels = c(Up = "Upward diffusion", Down = "Downward diffusion"),
      guide  = guide_legend(title = NULL, direction = "horizontal",
                            override.aes = list(edge_width = 2.5,
                                                edge_alpha = 0.9))
    ) +
    geom_node_point(size = 12, colour = dom_col, fill = "white",
                    shape = 21, stroke = 1.6) +
    geom_node_text(aes(label = name), vjust = 0.45, size = 4,
                   fontface = "bold", colour = "grey15", family = FONT) +
    scale_x_continuous(expand = expansion(add = 0.65)) +
    coord_cartesian(clip = "off") +
    labs(title = dom, subtitle = subtitle_str) +
    theme_flow +
    theme(plot.title = element_text(colour = dom_col))

  if (show_legend)
    p <- p + theme(legend.position = "bottom",
                   legend.text = element_text(size = 8, family = FONT))
  p
}

# ---- Skill arc panel (NO individual background) -----------------------------
make_arc_skill <- function(s_name, dom_col) {
  edata <- skill_edges[skill_name == s_name]
  if (!nrow(edata)) return(ggplot() + theme_void())

  g <- igraph::graph_from_data_frame(
    d        = edata[, .(from = wage_q_src, to = wage_q_dest,
                         weight = rate_norm, direction)],
    vertices = verts, directed = TRUE
  )

  ggraph(g, layout = "linear") +
    geom_edge_arc(
      aes(edge_alpha = weight, edge_width = weight, edge_colour = direction),
      strength = 0.42, arrow = arrow_flow_sm, end_cap = circle(4, "mm"),
      show.legend = FALSE
    ) +
    scale_edge_width(range = c(0.2, 2.0)) +
    scale_edge_alpha(range = c(0.25, 1.0)) +
    scale_edge_colour_manual(values = PAL_DIR) +
    geom_node_point(size = 7, colour = dom_col, fill = "white",
                    shape = 21, stroke = 1.1) +
    geom_node_text(aes(label = name), vjust = 0.45, size = 2.5,
                   fontface = "bold", colour = "grey20", family = FONT) +
    scale_x_continuous(expand = expansion(add = 0.60)) +
    coord_cartesian(clip = "off") +
    labs(title = str_wrap(s_name, width = 24)) +
    theme_void(base_family = FONT) +
    theme(
      plot.title  = element_text(face = "plain", size = 10, hjust = 0.5,
                                 family = FONT, lineheight = 1.1,
                                 colour = "grey10",
                                 margin = margin(b = 2, t = 4)),
      plot.margin = margin(6, 8, 12, 8)
    )
}

# ---- Individual skill data --------------------------------------------------
skill_stats <- dt[direction != "Lateral", .(
  rate = mean(diffusion, na.rm = TRUE), n_total = .N
), by = .(skill_name, domain_label, direction)]

top_cog <- skill_stats[
  domain_label == "Socio-cognitive" & direction == "Up" & n_total > 5000L
][order(-rate)][seq_len(min(6L, .N)), skill_name]

top_phy <- skill_stats[
  domain_label == "Physical-sensory" & direction == "Down" & n_total > 5000L
][order(-rate)][seq_len(min(6L, .N)), skill_name]

skill_edges <- dt[
  skill_name %in% c(top_cog, top_phy) & direction != "Lateral",
  .(rate = mean(diffusion, na.rm = TRUE)),
  by = .(skill_name, domain_label, wage_q_src, wage_q_dest, direction)
]
skill_edges[, rate_norm := rate / max(rate, na.rm = TRUE), by = skill_name]

# ---- Assemble one domain column ---------------------------------------------
assemble_col <- function(skills, agg_panel, dom) {
  dom_col <- PAL_DOMAIN[[dom]]
  bg_col  <- adjustcolor(dom_col, alpha.f = 0.06)

  plots <- lapply(skills, make_arc_skill, dom_col = dom_col)
  while (length(plots) < 6L) plots <- c(plots, list(ggplot() + theme_void()))

  # 2×3 grid with NO individual cell backgrounds
  skill_grid <- wrap_plots(plots, ncol = 2, nrow = 3) +
    plot_layout(heights = c(1, 1, 1)) +
    # Single background box wrapping the whole 2×3 grid
    plot_annotation(
      theme = theme(
        plot.background = element_rect(
          fill      = bg_col,
          colour    = dom_col,
          linewidth = 1.8
        ),
        plot.margin = margin(8, 8, 8, 8)
      )
    )

  agg_panel / skill_grid +
    plot_layout(heights = c(2.4, 3.8))
}

col_cog <- assemble_col(top_cog,
                        make_arc_agg("Socio-cognitive",  subtitle_cog),
                        "Socio-cognitive")
col_phy <- assemble_col(top_phy,
                        make_arc_agg("Physical-sensory", subtitle_phy,
                                     show_legend = TRUE),
                        "Physical-sensory")

p_fig1 <- (col_cog | col_phy) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(plot.margin = margin(4, 4, 4, 4))) &
  theme(legend.position  = "bottom",
        legend.text      = element_text(size = 8, family = FONT),
        legend.key.size  = unit(12, "pt"))

ggsave(file.path(fig_dir, "fig1_composite_arc_skills.pdf"),
       p_fig1, width = 7.0, height = 10.5, units = "in", device = cairo_pdf)
message("  Saved: fig1_composite_arc_skills.pdf")

# ==============================================================================
# Fig. 2 — Adoption rate gradients
# ==============================================================================
message("\n>>> Fig. 2...")

NBINS <- 100L
Y_LIM <- 0.40

add_ci <- function(D) {
  D[, se   := sqrt((rate * (1 - rate)) / pmax(n, 1L))]
  D[, low  := pmax(rate - 1.96 * se, 0)]
  D[, high := pmin(rate + 1.96 * se, 1)]
  D
}

# ---- Panel A: skill relatedness ---------------------------------------------
dist_cap <- quantile(dt[["structural_distance"]], 0.90, na.rm = TRUE)
dt_a     <- dt[structural_distance <= dist_cap]
dt_a[, relatedness := 1 - structural_distance]
dt_a[, rel_bin := as.integer(
  NBINS * (frank(relatedness, ties.method = "random") - 1L) / .N) + 1L]

stats_rel <- dt_a[, .(x = mean(relatedness, na.rm = TRUE),
                       rate = mean(diffusion, na.rm = TRUE), n = .N),
                   by = .(domain_label, rel_bin)]
stats_rel <- add_ci(stats_rel)

x_lo  <- floor(min(stats_rel$x,   na.rm = TRUE) * 20) / 20
x_hi  <- ceiling(max(stats_rel$x, na.rm = TRUE) * 20) / 20
med_r <- median(dt_a[["relatedness"]], na.rm = TRUE)

p_rel <- ggplot(stats_rel,
                aes(x, rate, colour = domain_label, fill = domain_label)) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.18, colour = NA) +
  geom_point(size = 1.2, alpha = 0.40) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 10),
              se = FALSE, linewidth = 1.0) +
  geom_vline(xintercept = med_r, linetype = "dotted",
             linewidth = 0.3, colour = "grey55") +
  annotate("text", x = med_r + 0.01, y = Y_LIM * 0.10,
           label = "Median", size = 2.8, colour = "grey50",
           hjust = 0, family = FONT) +
  scale_colour_manual(values = PAL_DOMAIN, name = NULL) +
  scale_fill_manual(values   = PAL_DOMAIN, name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, Y_LIM), breaks = seq(0, Y_LIM, 0.10)) +
  scale_x_continuous(expand = expansion(add = 0.02)) +
  coord_cartesian(xlim = c(x_lo - 0.02, x_hi + 0.02),
                  ylim = c(0, Y_LIM), clip = "off") +
  labs(title = "(A)", x = "Skill relatedness", y = "Adoption rate") +
  theme_sciadv   # legend.position = "none" already in theme_sciadv

# ---- Panel B: signed log wage gap ------------------------------------------
dt[, signed_bin := as.integer(
  NBINS * (frank(wage_gap, ties.method = "random") - 1L) / .N) + 1L]

stats_gap <- dt[, .(x = mean(wage_gap, na.rm = TRUE),
                     rate = mean(diffusion, na.rm = TRUE), n = .N),
                 by = .(domain_label, signed_bin)]
stats_gap <- add_ci(stats_gap)

xr         <- range(stats_gap$x, na.rm = TRUE)
x_ann_down <- xr[1] + diff(xr) * 0.03
x_ann_up   <- xr[2] - diff(xr) * 0.03

p_gap <- ggplot(stats_gap,
                aes(x, rate, colour = domain_label, fill = domain_label)) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.18, colour = NA) +
  geom_point(size = 1.2, alpha = 0.40) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 10),
              se = FALSE, linewidth = 1.0) +
  geom_vline(xintercept = 0, linetype = "dashed",
             linewidth = 0.3, colour = "grey45") +
  annotate("text", x = x_ann_down, y = Y_LIM * 0.94,
           label = "\u2190 Downward\n(lower-wage target)",
           size = 2.8, colour = "grey25", hjust = 0,
           family = FONT, lineheight = 1.1) +
  annotate("text", x = x_ann_up, y = Y_LIM * 0.94,
           label = "Upward \u2192\n(higher-wage target)",
           size = 2.8, colour = "grey25", hjust = 1,
           family = FONT, lineheight = 1.1) +
  scale_colour_manual(values = PAL_DOMAIN, name = NULL) +
  scale_fill_manual(values   = PAL_DOMAIN, name = NULL, guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, Y_LIM), breaks = seq(0, Y_LIM, 0.10)) +
  scale_x_continuous(expand = expansion(add = 0.05)) +
  coord_cartesian(ylim = c(0, Y_LIM), clip = "off") +
  labs(title = "(B)",
       x = "Signed log wage gap (target \u2212 source)", y = NULL) +
  theme_sciadv   # legend.position = "none" from base theme

# ---- Legend: built as standalone panel, assembled below ---------------------
leg_df <- data.frame(
  x = 1:2,
  domain_label = factor(names(PAL_DOMAIN), levels = names(PAL_DOMAIN))
)
p_leg <- ggplot(leg_df, aes(x = x, y = 1, colour = domain_label)) +
  geom_line(aes(group = domain_label), linewidth = 1.2) +
  scale_colour_manual(values = PAL_DOMAIN, name = NULL) +
  theme_void(base_family = FONT) +
  theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.key.size  = unit(18, "pt"),
    legend.text      = element_text(size = 7.5, family = FONT),
    legend.title     = element_blank(),
    legend.spacing.x = unit(8, "pt")
  ) +
  guides(colour = guide_legend(
    override.aes = list(linewidth = 1.4, alpha = 1)
  ))

# Stack: [A | B] on top, legend strip below
p_fig2 <- (p_rel | p_gap) / p_leg +
  plot_layout(heights = c(10, 1))

ggsave(file.path(fig_dir, "fig2_adoption_gradients.pdf"),
       p_fig2, width = 7.0, height = 3.8, units = "in", device = cairo_pdf)
message("  Saved: fig2_adoption_gradients.pdf")

# ==============================================================================
# Summary
# ==============================================================================
message("\n>>> Done.")
message(sprintf("  Socio-cognitive   up: %.2f%%  down: %.2f%%",
                get_rate("Socio-cognitive",  "Up")   * 100,
                get_rate("Socio-cognitive",  "Down") * 100))
message(sprintf("  Physical-sensory  up: %.2f%%  down: %.2f%%",
                get_rate("Physical-sensory", "Up")   * 100,
                get_rate("Physical-sensory", "Down") * 100))
message(sprintf("  Physical-sensory Down/Up ratio: %.2fx",
                get_rate("Physical-sensory", "Down") /
                  get_rate("Physical-sensory", "Up")))