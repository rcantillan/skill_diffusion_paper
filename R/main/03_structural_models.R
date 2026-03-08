# ==============================================================================
# 03_structural_models.R
#
# Baseline directional gravity models (no nestedness interaction)
#
# Estimates the two-panel identification strategy:
#
#   Panel A — destination-side FE: feglm(..., fixef = c("source", "skill_name"))
#             Source occupation and skill fixed effects absorb unobserved
#             heterogeneity in the supply-side propensity to diffuse.
#
#   Panel B — origin-side FE:      feglm(..., fixef = c("target", "skill_name"))
#             Target occupation and skill fixed effects absorb unobserved
#             heterogeneity in the demand-side receptiveness to adoption.
#
# Concordance between Panel A and Panel B friction patterns provides the
# main identification criterion for the ATC claim.
#
# Formula (both panels):
#   diffusion ~ (up_dummy + wage_up + wage_down + structural_distance):domain
#
#   up_dummy    indicator for upward wage gap (kappa: level shift at 0)
#   wage_up     pmax(0, wage_gap)          (Theta_up: upward slope)
#   wage_down   pmin(0, wage_gap)          (Theta_dn: downward slope)
#   structural_distance  cosine_wt_dist    (delta: structural friction)
#
# Outputs (output/output_main/):
#   models/m_fe_source.rds        Panel A model object
#   models/m_fe_target.rds        Panel B model object
#   tables/coefs_baseline.csv     Long-format coefficient table
#   figs/fig_main_atc_baseline.pdf  Piecewise predictor figure (Fig. 2)
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------------------
gc()
library(fixest)
library(ggplot2)
library(data.table)
source("R/utils.R")   # extract_coefs()

out_models <- file.path("output", "output_main", "models")
out_tables <- file.path("output", "output_main", "tables")
out_figs   <- file.path("output", "output_main", "figs")
for (d in c(out_models, out_tables, out_figs))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# 1. Load data
#
# This script does NOT source 00_setup_comun.R — that file creates dt_sample
# (a 50% subsample) which is only needed for the SI stability checks.
# The baseline models (Panels A and B) are estimated on the FULL dataset.
# ~17.3M rows at ~4.5 GB RAM: load once, drop unneeded columns immediately.
# ------------------------------------------------------------------------------
if (file.exists("R/99_paths_local.R")) source("R/99_paths_local.R")
dt_path <- if (exists("PATH_TRIADIC")) PATH_TRIADIC else "dt_con_cs_nestedness.rds"
if (!file.exists(dt_path))
  stop("Data file not found: ", dt_path,
       "\nSet PATH_TRIADIC in R/99_paths_local.R")

message("Loading data from: ", dt_path)
dt_model <- readRDS(dt_path)
if (!is.data.table(dt_model)) dt_model <- as.data.table(dt_model)
gc()

# Drop all columns except what the models need — critical for RAM
needed <- c("diffusion", "wage_gap", "structural_distance",
            "domain", "source", "target", "skill_name")
drop   <- setdiff(names(dt_model), needed)
if (length(drop)) dt_model[, (drop) := NULL]
gc(); gc()

# Build directional wage variables in-place (no copy)
dt_model[, wage_up   := pmax(0,  wage_gap)]
dt_model[, wage_down := pmin(0,  wage_gap)]
dt_model[, up_dummy  := fifelse(wage_gap > 0, 1L, 0L)]
dt_model[, domain    := factor(domain, levels = c("Cognitive", "Physical"))]
gc()

message(sprintf(
  ">>> Estimation dataset: %s rows | diffusion rate: %.2f%%",
  format(nrow(dt_model), big.mark = ","),
  mean(dt_model$diffusion) * 100
))

# ------------------------------------------------------------------------------
# 2. Panel A — destination-side FE: source + skill
# ------------------------------------------------------------------------------
message("\n>>> Panel A: feglm with FE(source, skill_name)...")

m_fe_source <- feglm(
  diffusion ~ (up_dummy + wage_up + wage_down + structural_distance):domain,
  data     = dt_model,
  family   = binomial(link = "cloglog"),
  fixef    = c("source", "skill_name"),
  cluster  = c("source", "target", "skill_name"),
  mem.clean = TRUE, nthreads = 0, lean = TRUE
)
gc(); gc()
summary(m_fe_source)

saveRDS(m_fe_source, file.path(out_models, "m_fe_source.rds"))
message("  Saved: models/m_fe_source.rds")

# ------------------------------------------------------------------------------
# 3. Panel B — origin-side FE: target + skill
# ------------------------------------------------------------------------------
message("\n>>> Panel B: feglm with FE(target, skill_name)...")

m_fe_target <- feglm(
  diffusion ~ (up_dummy + wage_up + wage_down + structural_distance):domain,
  data     = dt_model,
  family   = binomial(link = "cloglog"),
  fixef    = c("target", "skill_name"),
  cluster  = c("source", "target", "skill_name"),
  mem.clean = TRUE, nthreads = 0, lean = TRUE
)
gc(); gc()
summary(m_fe_target)

saveRDS(m_fe_target, file.path(out_models, "m_fe_target.rds"))
message("  Saved: models/m_fe_target.rds")

# ------------------------------------------------------------------------------
# 4. Coefficient table (long format)
# ------------------------------------------------------------------------------
message("\n>>> Extracting coefficients...")

coefs <- rbind(
  extract_coefs(m_fe_source, "Panel A"),
  extract_coefs(m_fe_target, "Panel B")
)
print(coefs)

fwrite(coefs, file.path(out_tables, "coefs_baseline.csv"))
message("  Saved: tables/coefs_baseline.csv")

# Quick ATC check: physical upward slope should not differ from zero (or be
# negative); cognitive upward slope should be positive and larger than downward
coefs_wide <- dcast(coefs, panel_short + coef ~ ., value.var = "estimate")
message("\n  ATC directional check:")
message(sprintf("  Panel A  Cognitive β↑ = %.3f   β↓ = %.3f",
  coefs[panel_short == "Panel A" & coef == "Theta_up_Cog", estimate],
  coefs[panel_short == "Panel A" & coef == "Theta_dn_Cog", estimate]))
message(sprintf("  Panel A  Physical  β↑ = %.3f   β↓ = %.3f",
  coefs[panel_short == "Panel A" & coef == "Theta_up_Phy", estimate],
  coefs[panel_short == "Panel A" & coef == "Theta_dn_Phy", estimate]))
message(sprintf("  Panel B  Cognitive β↑ = %.3f   β↓ = %.3f",
  coefs[panel_short == "Panel B" & coef == "Theta_up_Cog", estimate],
  coefs[panel_short == "Panel B" & coef == "Theta_dn_Cog", estimate]))
message(sprintf("  Panel B  Physical  β↑ = %.3f   β↓ = %.3f",
  coefs[panel_short == "Panel B" & coef == "Theta_up_Phy", estimate],
  coefs[panel_short == "Panel B" & coef == "Theta_dn_Phy", estimate]))

# ==============================================================================
# 5. Figure — piecewise linear predictor (Fig. 2 in paper)
#
# Science Advances figure specifications:
#   Width  : 7.0 in (2-column); Height: 8.5 in (max 9.5 in)
#   Font   : Helvetica (sans-serif preferred by AAAS)
#   Sizes  : tick labels 10 pt; axis titles + strip 11 pt; annots 9 pt
#   Labels : sentence case; panel tags (A)/(B) as facet row strips
#   No minor ticks; no grid lines; axes do not exceed data range
#   Output : cairo_pdf (vector, fonts embedded)
# ==============================================================================
message("\n>>> Building SciAdv-compliant piecewise predictor figure...")

# -- Colour palette -----------------------------------------------------------
PAL <- c(Cognitive = "#2ABAB2", Physical = "#CC4444")

DOMAIN_LABS <- c(Cognitive = "Socio-cognitive", Physical = "Physical-sensory")
PANEL_LABS  <- c(
  "(A) FE(source, skill)" = "(A)  Source FE",
  "(B) FE(target, skill)" = "(B)  Target FE"
)

# -- Layout constants ---------------------------------------------------------
XLIM  <- c(-2.2,  2.2)
YLIM  <- c(-3.6,  2.0)
GAP0  <- 0.16
STEP  <- 0.01

if (file.exists("R/99_paths_local.R")) source("R/99_paths_local.R")
dt_path <- if (exists("PATH_TRIADIC")) PATH_TRIADIC else "dt_con_cs_nestedness.rds"
D_BAR <- mean(readRDS(dt_path)[["structural_distance"]], na.rm = TRUE)
message("D_BAR = ", round(D_BAR, 4))

# -- Typography ---------------------------------------------------------------
mm_from_pt <- function(pt) pt * 0.352778

PT_TICK  <- 10
PT_TITLE <- 11
PT_STRIP <- 11
PT_ANNOT <-  9

# -- Coefficient extractor ----------------------------------------------------
get_coef <- function(m, term) {
  b <- coef(m)
  if (term %in% names(b)) return(unname(b[[term]]))
  parts <- strsplit(term, ":", fixed = TRUE)[[1]]
  if (length(parts) == 2) {
    alt <- paste0(parts[2], ":", parts[1])
    if (alt %in% names(b)) return(unname(b[[alt]]))
  }
  0
}

coef_by_domain <- function(m) {
  list(
    b_up    = c(Cognitive = get_coef(m, "wage_up:domainCognitive"),
                Physical  = get_coef(m, "wage_up:domainPhysical")),
    b_down  = c(Cognitive = get_coef(m, "wage_down:domainCognitive"),
                Physical  = get_coef(m, "wage_down:domainPhysical")),
    b_kappa = c(Cognitive = get_coef(m, "up_dummy:domainCognitive"),
                Physical  = get_coef(m, "up_dummy:domainPhysical")),
    b_delta = c(Cognitive = get_coef(m, "structural_distance:domainCognitive"),
                Physical  = get_coef(m, "structural_distance:domainPhysical"))
  )
}

# -- Data builders ------------------------------------------------------------
# side = "L" / "R" keeps the two segments in separate ggplot2 groups so that
# geom_line does not connect them across the x = 0 discontinuity.
make_lines <- function(dom, x_seq, bb, panel_label, side) {
  d <- data.table(panel = panel_label, domain = dom, side = side, x = x_seq)
  d[, y :=
      bb$b_up[dom]    * pmax(0, x) +
      bb$b_down[dom]  * pmin(0, x) +
      bb$b_kappa[dom] * as.integer(x > 0) +
      bb$b_delta[dom] * D_BAR]
  d
}

make_jump <- function(dom, bb, panel_label) {
  data.table(
    panel = panel_label, domain = dom, x = 0,
    ymin  = bb$b_delta[dom] * D_BAR,
    ymax  = bb$b_delta[dom] * D_BAR + bb$b_kappa[dom]
  )
}

build_panel_data <- function(m, panel_label) {
  bb      <- coef_by_domain(m)
  x_left  <- seq(XLIM[1], -GAP0, by = STEP)
  x_right <- seq(GAP0,  XLIM[2], by = STEP)
  doms    <- c("Cognitive", "Physical")

  lines <- rbindlist(c(
    lapply(doms, make_lines, x_seq = x_left,  bb = bb,
           panel_label = panel_label, side = "L"),
    lapply(doms, make_lines, x_seq = x_right, bb = bb,
           panel_label = panel_label, side = "R")
  ))
  jumps <- rbindlist(lapply(doms, make_jump, bb = bb, panel_label = panel_label))

  # Annotation positions: Cognitive bottom-left, Physical top-right
  # to avoid overlap with the data lines in each domain
  annot_x  <- c(Cognitive = XLIM[1] + 0.12, Physical = XLIM[1] + 0.12)
  annot_y  <- c(Cognitive = YLIM[1] + 0.10, Physical = YLIM[1] + 0.10)
  annot_hj <- c(Cognitive = 0,              Physical = 0)
  annot_vj <- c(Cognitive = 0,              Physical = 0)
  
  annots <- data.table(
  panel  = panel_label,
  domain = doms,
  label  = sprintf(
    "\u03b2\u2191 = %+0.3f\n\u03b2\u2193 = %+0.3f\n\u03ba = %+0.3f",
    bb$b_up[doms], bb$b_down[doms], bb$b_kappa[doms]
  ),
  x  = annot_x[doms],
  y  = annot_y[doms],
  hj = annot_hj[doms],
  vj = annot_vj[doms]
)
  list(lines = lines, jumps = jumps, annots = annots)
}

# -- Assemble -----------------------------------------------------------------
pa <- build_panel_data(m_fe_source, "(A) FE(source, skill)")
pb <- build_panel_data(m_fe_target, "(B) FE(target, skill)")

panel_levels <- c("(A) FE(source, skill)", "(B) FE(target, skill)")

all_lines  <- rbind(pa$lines,  pb$lines)
all_jumps  <- rbind(pa$jumps,  pb$jumps)
all_annots <- rbind(pa$annots, pb$annots)

all_lines[ , panel := factor(panel, levels = panel_levels)]
all_jumps[ , panel := factor(panel, levels = panel_levels)]
all_annots[, panel := factor(panel, levels = panel_levels)]

# -- Theme --------------------------------------------------------------------
theme_sciadv <- theme_classic(base_size = PT_TICK, base_family = "Helvetica") +
  theme(
    strip.text        = element_text(size = PT_STRIP, face = "plain",
                                     margin = margin(b = 4, t = 4)),
    strip.background  = element_blank(),
    axis.title.x      = element_text(size = PT_TITLE, margin = margin(t = 5)),
    axis.title.y      = element_text(size = PT_TITLE, margin = margin(r = 5)),
    axis.text         = element_text(size = PT_TICK),
    axis.ticks        = element_line(linewidth = 0.3, colour = "grey30"),
    axis.ticks.length = unit(2, "pt"),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    panel.border      = element_rect(colour = "grey30", linewidth = 0.4, fill = NA),
    panel.background  = element_rect(fill = "white", colour = NA),
    panel.spacing.x   = unit(6, "pt"),
    panel.spacing.y   = unit(8, "pt"),
    plot.margin       = margin(t = 6, r = 8, b = 6, l = 6, unit = "pt"),
    legend.position   = "none"
  )

# -- Plot ---------------------------------------------------------------------
fig_atc <- ggplot() +

  geom_line(
    data = all_lines,
    aes(x = x, y = y, colour = domain,
        group = interaction(panel, domain, side)),
    linewidth = 1.2, lineend = "butt"
  ) +

  geom_segment(
    data = all_jumps,
    aes(x = x, xend = x, y = ymin, yend = ymax),
    linewidth = 0.7, colour = "grey20"
  ) +

  # Subtle reference lines — do not compete with data
  geom_vline(xintercept = 0, linetype = "dotted",
             linewidth = 0.25, colour = "grey70") +
  geom_hline(yintercept = 0, linetype = "dotted",
             linewidth = 0.25, colour = "grey70") +

 geom_text(
  data = all_annots,
  aes(x = x, y = y, label = label, hjust = hj, vjust = vj),
  family = "Helvetica", size = mm_from_pt(PT_ANNOT),
  lineheight = 1.15, colour = "grey20"
) +

  facet_grid(
    panel ~ domain,
    labeller = labeller(domain = DOMAIN_LABS, panel = PANEL_LABS)
  ) +

  scale_colour_manual(values = PAL) +

  scale_x_continuous(
    breaks = seq(-2, 2, by = 1),
    expand = expansion(mult = 0, add = 0.05)
  ) +
  scale_y_continuous(
    breaks = seq(-3, 2, by = 1),
    expand = expansion(mult = 0, add = 0.10)
  ) +

  coord_cartesian(xlim = XLIM, ylim = YLIM, clip = "on") +

  labs(
    x = "Signed log wage gap (target \u2212 source)",
    y = "Linear predictor (cloglog scale)"
  ) +

  theme_sciadv

# -- Save ---------------------------------------------------------------------
ggsave(
  file.path(out_figs, "fig_main_atc_baseline.pdf"),
  fig_atc, width = 7.0, height = 8.5, units = "in", device = cairo_pdf
)
message("  Saved: figs/fig_main_atc_baseline.pdf  [7.0 x 8.5 in, SciAdv compliant]")

message("\n>>> 03_structural_models.R complete.")
