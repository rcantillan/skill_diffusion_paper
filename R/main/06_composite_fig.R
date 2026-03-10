# ==============================================================================
# 06_composite_fig.R
#
# Composite figure: ATC nestedness model (top) + macro-projection Panel C (bottom)
#
# Panel A (top):    Nestedness model — directional friction by archetype (2×3)
#                   Reconstructed from coefs_nestedness.csv
# Panel B (bottom): Observed vs ATC vs NULL adoption rates by quintile × domain
#                   Rebuilt from atc_quintile_domain.csv
#
# Adds:
#   - Large figure labels "A" and "B" at top-left of each block
#   - Cleaner Science Advances styling
#
# Output: output/output_main/figs/fig_composite_nestedness_projection.pdf
#         [10.5 × 16.0 in, cairo_pdf, Helvetica]
# ==============================================================================

gc()
library(data.table)
library(ggplot2)
library(patchwork)
library(grid)

out_figs <- file.path("output", "output_main", "figs")
dir.create(out_figs, showWarnings = FALSE, recursive = TRUE)

path_coefs <- file.path("output", "output_main", "tables", "coefs_nestedness.csv")
path_data  <- file.path("output", "output_main", "tables", "atc_quintile_domain.csv")

stopifnot(
  "coefs_nestedness.csv not found — run 04_structural_models_nestedness.R" =
    file.exists(path_coefs),
  "atc_quintile_domain.csv not found — run 05_atc_polarization_main.R" =
    file.exists(path_data)
)

# Mean structural distance (used to evaluate linear predictor at D_BAR)
D_BAR <- 0.7441985

# ==============================================================================
# SECTION 1 — Shared theme
# ==============================================================================
PT_TICK  <- 12
PT_TITLE <- 14
PT_STRIP <- 13
PT_ANNOT <- 13

mm_from_pt <- function(pt) pt * 0.352778

theme_sciadv <- theme_classic(base_size = PT_TICK, base_family = "Helvetica") +
  theme(
    strip.text        = element_text(size = PT_STRIP, face = "plain",
                                     margin = margin(b = 4, t = 4), colour = "black"),
    strip.background  = element_rect(fill = "white", colour = NA),
    axis.title.x      = element_text(size = PT_TITLE, margin = margin(t = 5), colour = "black"),
    axis.title.y      = element_text(size = PT_TITLE, margin = margin(r = 5), colour = "black"),
    axis.text         = element_text(size = PT_TICK, colour = "black"),
    axis.ticks        = element_line(linewidth = 0.3, colour = "black"),
    axis.ticks.length = unit(2, "pt"),
    panel.grid        = element_blank(),
    panel.border      = element_rect(colour = "black", linewidth = 1, fill = NA),
    axis.line         = element_blank(),
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.background   = element_rect(fill = "white", colour = NA),
    panel.spacing.x   = unit(8, "pt"),
    panel.spacing.y   = unit(8, "pt"),
    plot.margin = margin(t = 10, r = 18, b = 8, l = 8, unit = "pt"),
    legend.position   = "none"
  )

# ==============================================================================
# SECTION 2 — Nestedness figure (Panel A of composite)
# ==============================================================================
message("[1] Building nestedness figure from coefs_nestedness.csv...")

coefs <- fread(path_coefs)

ARCHETYPES   <- c("SC_Specialized", "SC_Scaffolding", "Physical_Terminal")
PANEL_LEVELS <- c("Panel A", "Panel B")
XLIM <- c(-2.2,  2.2)
YLIM <- c(-3.4,  2.0)
GAP0 <- 0.16
STEP <- 0.01

PAL_ARCH <- c(
  SC_Specialized    = "#2ABAB2",
  SC_Scaffolding    = "#1a7a77",
  Physical_Terminal = "#CC4444"
)

ARCH_LABS <- c(
  SC_Specialized    = "SC Specialized",
  SC_Scaffolding    = "SC Scaffolding",
  Physical_Terminal = "Physical Terminal"
)

PANEL_LABS <- c(
  "Panel A" = "(A) Source FE",
  "Panel B" = "(B) Target FE"
)

get_est <- function(panel, arch, cname) {
  v <- coefs[panel_short == panel & archetype == arch & coef == cname, estimate]
  if (length(v) == 0 || is.na(v[1])) 0 else v[1]
}

build_nest_panel <- function(panel_label) {
  x_left  <- seq(XLIM[1], -GAP0, by = STEP)
  x_right <- seq(GAP0,  XLIM[2], by = STEP)

  lines <- rbindlist(lapply(ARCHETYPES, function(arch) {
    b_up <- get_est(panel_label, arch, "Theta_up")
    b_dn <- get_est(panel_label, arch, "Theta_dn")
    b_k  <- get_est(panel_label, arch, "kappa")
    b_d  <- get_est(panel_label, arch, "delta")

    left  <- data.table(panel = panel_label, archetype = arch, side = "L", x = x_left)
    left[,  y := b_dn * x + b_d * D_BAR]

    right <- data.table(panel = panel_label, archetype = arch, side = "R", x = x_right)
    right[, y := b_up * x + b_k + b_d * D_BAR]

    rbind(left, right)
  }))

  jumps <- rbindlist(lapply(ARCHETYPES, function(arch) {
    b_k <- get_est(panel_label, arch, "kappa")
    b_d <- get_est(panel_label, arch, "delta")
    data.table(panel = panel_label, archetype = arch,
               x = 0, ymin = b_d * D_BAR, ymax = b_d * D_BAR + b_k)
  }))

  annots <- rbindlist(lapply(ARCHETYPES, function(arch) {
    data.table(
      panel     = panel_label,
      archetype = arch,
      label = sprintf(
        "\u03b2\u2191 = %+.3f\n\u03b2\u2193 = %+.3f\n\u03ba = %+.3f",
        get_est(panel_label, arch, "Theta_up"),
        get_est(panel_label, arch, "Theta_dn"),
        get_est(panel_label, arch, "kappa")
      ),
      x = XLIM[1] + 0.12,
      y = YLIM[1] + 0.08
    )
  }))

  list(lines = lines, jumps = jumps, annots = annots)
}

pa_nest <- build_nest_panel("Panel A")
pb_nest <- build_nest_panel("Panel B")

all_lines  <- rbind(pa_nest$lines,  pb_nest$lines)
all_jumps  <- rbind(pa_nest$jumps,  pb_nest$jumps)
all_annots <- rbind(pa_nest$annots, pb_nest$annots)

all_lines[,  panel := factor(panel, levels = PANEL_LEVELS)]
all_jumps[,  panel := factor(panel, levels = PANEL_LEVELS)]
all_annots[, panel := factor(panel, levels = PANEL_LEVELS)]

all_lines[,  archetype := factor(archetype, levels = ARCHETYPES)]
all_jumps[,  archetype := factor(archetype, levels = ARCHETYPES)]
all_annots[, archetype := factor(archetype, levels = ARCHETYPES)]

fig_nest <- ggplot() +
  geom_line(
    data = all_lines,
    aes(x = x, y = y, colour = archetype,
        group = interaction(panel, archetype, side)),
    linewidth = 1.35, lineend = "butt"
  ) +
  geom_segment(
    data = all_jumps,
    aes(x = x, xend = x, y = ymin, yend = ymax, colour = archetype),
    linewidth = 0.8
  ) +
  geom_vline(xintercept = 0, linetype = "dotted",
             linewidth = 0.25, colour = "grey70") +
  geom_hline(yintercept = 0, linetype = "dotted",
             linewidth = 0.25, colour = "grey70") +
  geom_text(
    data = all_annots,
    aes(x = x, y = y, label = label),
    family = "Helvetica",
    size = mm_from_pt(PT_ANNOT * 1.25),
    hjust = 0, vjust = 0,
    lineheight = 1.10,
    colour = "grey20"
  ) +
  facet_grid(
    panel ~ archetype,
    labeller = labeller(archetype = ARCH_LABS, panel = PANEL_LABS)
  ) +
  scale_colour_manual(values = PAL_ARCH) +
  scale_x_continuous(
    breaks = seq(-2, 2, by = 1),
    expand = c(0,0)
  ) +
  scale_y_continuous(
    breaks = seq(-3, 2, by = 1),
    expand = c(0,0)
  ) +
  coord_cartesian(xlim = XLIM, ylim = YLIM, clip = "on") +
  labs(
    x = "Signed log wage gap (target \u2212 source)",
    y = "Linear predictor (cloglog scale)"
  ) +
  theme_sciadv

message("    Nestedness figure built.")

# ==============================================================================
# SECTION 3 — Projection panel (Panel B of composite)
# ==============================================================================
message("[2] Building projection panel from atc_quintile_domain.csv...")

quint_domain <- fread(path_data)

plot_C <- melt(
  quint_domain,
  id.vars       = c("wage_quintile", "domain"),
  measure.vars  = c("r_obs", "r_ATC", "r_NULL"),
  variable.name = "series",
  value.name    = "rate"
)
plot_C[, series := fcase(
  series == "r_obs",  "Observed",
  series == "r_ATC",  "ATC (estimated)",
  series == "r_NULL", "NULL (distance-only)"
)]
plot_C[, series := factor(
  series,
  levels = c("Observed", "ATC (estimated)", "NULL (distance-only)")
)]
plot_C[, domain := factor(domain, levels = c("Cognitive", "Physical"))]

Q_LABS <- c("Q1", "Q2", "Q3", "Q4", "Q5")

DOMAIN_LABS <- c(
  Cognitive = "Socio-cognitive",
  Physical  = "Physical-sensory"
)

PAL_MODEL <- c(
  "Observed"             = "black",
  "ATC (estimated)"      = "#2ABAB2",
  "NULL (distance-only)" = "grey55"
)

LTY_MODEL <- c(
  "Observed"             = "solid",
  "ATC (estimated)"      = "solid",
  "NULL (distance-only)" = "dashed"
)

theme_sciadv_proj <- theme_sciadv +
  theme(
    legend.position   = "bottom",
    legend.text       = element_text(size = PT_TICK),
    legend.key.width  = unit(1.0, "cm"),
    legend.spacing.x  = unit(6, "pt"),
    panel.spacing.x   = unit(18, "pt"),
    plot.margin       = margin(t = 10, r = 8, b = 8, l = 8, unit = "pt")
  )

fig_proj <- ggplot(
  plot_C,
  aes(x = wage_quintile,
      y = rate,
      colour = series,
      linetype = series,
      group = series)
) +
  geom_line(linewidth = 1.05, lineend = "round") +
  geom_point(size = 2.6) +
  facet_wrap(
    ~ domain,
    ncol     = 2,
    labeller = labeller(domain = DOMAIN_LABS)
  ) +
  scale_colour_manual(values = PAL_MODEL) +
  scale_linetype_manual(values = LTY_MODEL) +
  scale_x_continuous(
    breaks = 1:5,
    labels = Q_LABS,
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  scale_y_continuous(
    limits = c(0, 0.30),
    breaks = seq(0, 0.30, by = 0.05),
    expand = expansion(mult = c(0, 0.04))
  ) +
  labs(
    x = "Destination wage quintile (Q1 = lowest wage, Q5 = highest wage)",
    y = "Adoption rate"
  ) +
  theme_sciadv_proj

message("    Projection panel built.")

# ==============================================================================
# SECTION 4 — Add figure labels A / B without inset_element
# ==============================================================================
message("[3] Adding figure labels...")

fig_nest_labeled <- fig_nest +
  labs(tag = "(A)") +
  theme(
    plot.tag = element_text(
      family = "Helvetica",
      #face   = "bold",
      size   = 20,
      hjust  = 0,
      vjust  = 1
    ),
    plot.tag.position = c(0.00, 1.02),
    plot.margin = margin(t = 20, r = 18, b = 8, l = 8, unit = "pt")
  )

fig_proj_labeled <- fig_proj +
  labs(tag = "(B)") +
  theme(
    plot.tag = element_text(
      family = "Helvetica",
      #face   = "bold",
      size   = 20,
      hjust  = 0,
      vjust  = 1
    ),
    plot.tag.position = c(0.00, 1.02),
    plot.margin = margin(t = 20, r = 8, b = 8, l = 8, unit = "pt")
  )

# ==============================================================================
# SECTION 5 — Compose and save
# ==============================================================================
message("[4] Composing figure...")

fig_composite <- fig_nest_labeled / fig_proj_labeled +
  plot_layout(heights = c(1.85, 1))

out_path <- file.path(out_figs, "fig_composite_nestedness_projection.pdf")

if (file.exists(out_path)) {
  try(unlink(out_path), silent = TRUE)
}

ggsave(
  filename = out_path,
  plot     = fig_composite,
  width    = 10.5,
  height   = 16.0,
  units    = "in",
  device   = cairo_pdf,
  bg       = "white"
)

message(sprintf("\nSaved: %s  [10.5 x 16.0 in]", out_path))
message("Done.")