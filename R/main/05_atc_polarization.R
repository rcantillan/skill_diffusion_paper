# ==============================================================================
# 05_atc_polarization_main.R
#
# ATC-implied occupational polarization
# Main-text predictive figure for Science Advances
#
# Cantillan & Bucca (2026)
#
# PURPOSE
# -------
# This script projects the estimated Panel A directional diffusion model onto the
# full occupational risk set and evaluates whether the ATC mechanism reproduces
# the observed macro-pattern of occupational polarization across the wage
# distribution.
#
# MAIN-TEXT FIGURE
# ----------------
#   Panel A  Observed adoption rate by destination wage quintile × domain
#   Panel B  Observed Cognitive / Physical adoption ratio by wage quintile
#   Panel C  Observed vs predicted adoption rates by quintile and domain:
#            ATC (estimated asymmetric diffusion) vs NULL (distance-only)
#
# KEY IMPROVEMENTS
# ----------------
# 1. Panel B now shows ONLY the observed ratio (cleaner main-text story).
# 2. Panel C remains the model-comparison panel (Observed vs ATC vs NULL).
# 3. ATC vs NULL fit summaries are computed explicitly:
#      - RMSE and MAE by domain and overall
#      - Q5-Q1 gradient recovery by domain
#      - Polarization index recovery
# 4. Figure style is cleaner and closer to Science Advances main-text norms.
#
# IDENTIFICATION ENGINE
# ---------------------
# Uses the baseline Panel A model estimated in:
#   03_structural_models.R
#
# Specifically:
#   output/output_main/models/m_fe_source.rds
#
# Model:
#   diffusion ~ (up_dummy + wage_up + wage_down + structural_distance):domain
#   FE: source + skill_name
#
# COUNTERFACTUAL WORLDS
# ---------------------
#   ATC  = estimated asymmetric diffusion:
#          beta_up(g), beta_dn(g), kappa(g), delta(g)
#
#   NULL = distance-only comparator:
#          beta_up = beta_dn = kappa = 0
#          delta(g) unchanged
#
# IMPORTANT
# ---------
# NULL is NOT assumed to be flat by construction. If structural distance is
# unevenly distributed across destination wage quintiles, NULL may still imply a
# gradient. The substantive test is whether ATC reproduces the observed gradient
# substantially better than NULL.
#
# AGGREGATION
# -----------
# For each triadic opportunity (i, j, s):
#   eta_ijs(world) = beta_up * wage_up + beta_dn * wage_down +
#                    kappa * up_dummy + delta * structural_distance
#
# Cloglog hazard kernel:
#   h_ijs = exp(eta_ijs)
#
# Aggregate to target-skill cell:
#   H_js = sum_i exp(eta_ijs)
#
# Calibrated adoption probability:
#   P_js = 1 - exp(-alpha * H_js)
#
# alpha is calibrated separately for each world so that:
#   mean(P_js) = observed mean adoption rate in the risk set
#
# OUTPUTS
# -------
# output/output_main/tables/atc_quintile_domain.csv
# output/output_main/tables/atc_ratio_quintile.csv
# output/output_main/tables/atc_model_fit_summary.csv
# output/output_main/tables/atc_panel_c_annotations.csv
# output/output_main/figs/fig_atc_polarization_main.pdf
#
# REQUIREMENTS
# ------------
# - dt_con_cs_nestedness.rds (or PATH_TRIADIC override)
# - dt_state_2015.rds        (or PATH_STATE2015 override)
# - output/output_main/models/m_fe_source.rds
# - R/utils.R providing extract_coefs()
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------------------
gc()
library(data.table)
library(ggplot2)
library(patchwork)
library(fixest)
library(grid)

source("R/utils.R")   # must provide extract_coefs()

out_tables <- file.path("output", "output_main", "tables")
out_figs   <- file.path("output", "output_main", "figs")
for (d in c(out_tables, out_figs)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

if (file.exists("R/99_paths_local.R")) source("R/99_paths_local.R")

PATH_DT <- if (exists("PATH_TRIADIC")) {
  PATH_TRIADIC
} else {
  "dt_con_cs_nestedness.rds"
}

PATH_STATE <- if (exists("PATH_STATE2015")) {
  PATH_STATE2015
} else {
  "dt_state_2015.rds"
}

PATH_MODEL_A <- if (exists("PATH_MODEL_A_BASELINE")) {
  PATH_MODEL_A_BASELINE
} else {
  file.path("output", "output_main", "models", "m_fe_source.rds")
}

stopifnot(file.exists(PATH_DT))
stopifnot(file.exists(PATH_STATE))
stopifnot(file.exists(PATH_MODEL_A))

message("\n==============================================================")
message("05_atc_polarization_main.R")
message("ATC macro-projection: observed vs ATC vs NULL")
message("==============================================================")
message("PATH_DT      : ", PATH_DT)
message("PATH_STATE   : ", PATH_STATE)
message("PATH_MODEL_A : ", PATH_MODEL_A)

# ------------------------------------------------------------------------------
# 1. Load Panel A baseline coefficients (domain-level)
# ------------------------------------------------------------------------------
message("\n[1] Loading Panel A baseline model...")

m_a <- readRDS(PATH_MODEL_A)
coefs_a <- extract_coefs(m_a, "Panel A")

if (!is.data.table(coefs_a)) coefs_a <- as.data.table(coefs_a)

required_coef_cols <- c("panel_short", "coef", "estimate")
miss_coef_cols <- setdiff(required_coef_cols, names(coefs_a))
if (length(miss_coef_cols) > 0L) {
  stop("extract_coefs() returned a table missing required columns: ",
       paste(miss_coef_cols, collapse = ", "))
}

message("  Coefficients extracted:")
print(coefs_a)

get_est <- function(cname) {
  val <- coefs_a[coef == cname, estimate]
  if (length(val) != 1L || is.na(val)) {
    stop("Missing or ambiguous coefficient: ", cname)
  }
  val
}

COEF_ATC <- list(
  Cognitive = list(
    b_up = get_est("Theta_up_Cog"),
    b_dn = get_est("Theta_dn_Cog"),
    b_k  = get_est("kappa_Cog"),
    b_d  = get_est("delta_Cog")
  ),
  Physical = list(
    b_up = get_est("Theta_up_Phy"),
    b_dn = get_est("Theta_dn_Phy"),
    b_k  = get_est("kappa_Phy"),
    b_d  = get_est("delta_Phy")
  )
)

COEF_NULL <- list(
  Cognitive = list(
    b_up = 0,
    b_dn = 0,
    b_k  = 0,
    b_d  = COEF_ATC$Cognitive$b_d
  ),
  Physical = list(
    b_up = 0,
    b_dn = 0,
    b_k  = 0,
    b_d  = COEF_ATC$Physical$b_d
  )
)

message("\n  Panel A coefficients used for projection:")
message(sprintf(
  "  Cognitive | beta_up=%+.4f beta_dn=%+.4f kappa=%+.4f delta=%+.4f | ATC index=%+.4f",
  COEF_ATC$Cognitive$b_up, COEF_ATC$Cognitive$b_dn,
  COEF_ATC$Cognitive$b_k,  COEF_ATC$Cognitive$b_d,
  COEF_ATC$Cognitive$b_up - COEF_ATC$Cognitive$b_dn
))
message(sprintf(
  "  Physical  | beta_up=%+.4f beta_dn=%+.4f kappa=%+.4f delta=%+.4f | ATC index=%+.4f",
  COEF_ATC$Physical$b_up, COEF_ATC$Physical$b_dn,
  COEF_ATC$Physical$b_k,  COEF_ATC$Physical$b_d,
  COEF_ATC$Physical$b_up - COEF_ATC$Physical$b_dn
))

# ------------------------------------------------------------------------------
# 2. Load triadic risk-set data
# ------------------------------------------------------------------------------
message("\n[2] Loading triadic risk-set data...")

dt <- readRDS(PATH_DT)
if (!is.data.table(dt)) dt <- as.data.table(dt)

needed <- c(
  "diffusion", "wage_gap", "structural_distance",
  "domain", "source", "target", "skill_name"
)
miss_needed <- setdiff(needed, names(dt))
if (length(miss_needed) > 0L) {
  stop("Triadic dataset is missing required columns: ",
       paste(miss_needed, collapse = ", "))
}

drop_cols <- setdiff(names(dt), needed)
if (length(drop_cols) > 0L) dt[, (drop_cols) := NULL]
gc(); gc()

dt[, domain := as.character(domain)]

valid_domains <- c("Cognitive", "Physical")
bad_domains <- setdiff(unique(dt$domain), valid_domains)
if (length(bad_domains) > 0L) {
  stop("Unexpected domain values in PATH_DT: ", paste(bad_domains, collapse = ", "))
}

message(sprintf(
  "  Triadic rows: %s | mean diffusion = %.5f",
  format(nrow(dt), big.mark = ","),
  mean(dt$diffusion, na.rm = TRUE)
))

dt[, wage_up   := pmax(0, wage_gap)]
dt[, wage_down := pmin(0, wage_gap)]
dt[, up_dummy  := fifelse(wage_gap > 0, 1.0, 0.0)]
dt[, wage_gap  := NULL]

# ------------------------------------------------------------------------------
# 3. Build ATC and NULL hazards at the triadic level
# ------------------------------------------------------------------------------
message("\n[3] Computing triadic hazards under ATC and NULL...")

for (dom in valid_domains) {
  cc_atc  <- COEF_ATC[[dom]]
  cc_null <- COEF_NULL[[dom]]

  dt[domain == dom,
     exp_eta_ATC := exp(
       cc_atc$b_up * wage_up +
       cc_atc$b_dn * wage_down +
       cc_atc$b_k  * up_dummy +
       cc_atc$b_d  * structural_distance
     )]

  dt[domain == dom,
     exp_eta_NULL := exp(
       cc_null$b_d * structural_distance
     )]
}

message("  Distribution of exp(eta) by world:")
message("    ATC  | ",
        paste(round(quantile(dt$exp_eta_ATC,  c(.01, .50, .99), na.rm = TRUE), 4),
              collapse = " / "))
message("    NULL | ",
        paste(round(quantile(dt$exp_eta_NULL, c(.01, .50, .99), na.rm = TRUE), 4),
              collapse = " / "))

# ------------------------------------------------------------------------------
# 4. Collapse to target-skill cells: H_js = sum_i exp(eta_ijs)
# ------------------------------------------------------------------------------
message("\n[4] Aggregating to target-skill cells...")

consistency_check <- dt[, .(
  n_diffusion = uniqueN(diffusion),
  n_domain    = uniqueN(domain)
), by = .(target, skill_name)]

n_inconsistent <- consistency_check[n_diffusion > 1L | n_domain > 1L, .N]
rm(consistency_check); gc()

if (n_inconsistent > 0L) {
  stop("Found ", n_inconsistent,
       " target-skill cells with inconsistent diffusion or domain values.")
}

H_js <- dt[, .(
  H_raw_ATC  = sum(exp_eta_ATC,  na.rm = TRUE),
  H_raw_NULL = sum(exp_eta_NULL, na.rm = TRUE),
  diffusion  = first(diffusion),
  domain     = first(domain)
), by = .(target, skill_name)]

rm(dt); gc(); gc()

message(sprintf(
  "  target-skill cells: %s | observed mean adoption = %.5f",
  format(nrow(H_js), big.mark = ","),
  mean(H_js$diffusion, na.rm = TRUE)
))

# ------------------------------------------------------------------------------
# 5. Load baseline state and merge destination wage quintiles
# ------------------------------------------------------------------------------
message("\n[5] Loading 2015 baseline state...")

dt_state <- readRDS(PATH_STATE)
if (!is.data.table(dt_state)) dt_state <- as.data.table(dt_state)

state_needed <- c("occupation", "skill_name", "domain", "has_skill_2015", "wage_quintile")
miss_state_needed <- setdiff(state_needed, names(dt_state))
if (length(miss_state_needed) > 0L) {
  stop("Baseline state dataset is missing required columns: ",
       paste(miss_state_needed, collapse = ", "))
}

occ_q <- unique(dt_state[, .(occupation, wage_quintile)])

if (occ_q[, .N, by = occupation][N > 1L, .N] > 0L) {
  stop("Some occupations have multiple wage_quintile values in PATH_STATE.")
}

H_js <- merge(
  H_js,
  occ_q,
  by.x = "target",
  by.y = "occupation",
  all.x = TRUE
)

n_miss_q <- H_js[is.na(wage_quintile), .N]
if (n_miss_q > 0L) {
  warning(n_miss_q,
          " target-skill cells are missing destination wage quintile and will be dropped.")
  H_js <- H_js[!is.na(wage_quintile)]
}

message(sprintf(
  "  target-skill cells after quintile merge: %s",
  format(nrow(H_js), big.mark = ",")
))

# ------------------------------------------------------------------------------
# 6. Calibrate alpha in each world
# ------------------------------------------------------------------------------
message("\n[6] Calibrating alpha for ATC and NULL...")

r_obs <- mean(H_js$diffusion, na.rm = TRUE)
message(sprintf("  Observed mean adoption rate (target-skill risk set) = %.8f", r_obs))

calibrate_alpha <- function(H_raw, r_target, label = "") {
  f <- function(a) mean(1 - exp(-a * H_raw), na.rm = TRUE) - r_target

  low <- 1e-15
  if (f(low) > 0) {
    warning(label, ": tiny alpha already exceeds target. Returning low bound.")
    return(low)
  }

  upper <- 1
  while (f(upper) < 0 && upper < 1e8) upper <- upper * 10

  if (f(upper) < 0) {
    stop(label, ": unable to bracket alpha. Check H_raw distribution.")
  }

  uniroot(f, c(low, upper), tol = 1e-14)$root
}

alpha_ATC  <- calibrate_alpha(H_js$H_raw_ATC,  r_obs, "ATC")
alpha_NULL <- calibrate_alpha(H_js$H_raw_NULL, r_obs, "NULL")

H_js[, P_ATC  := 1 - exp(-alpha_ATC  * H_raw_ATC)]
H_js[, P_NULL := 1 - exp(-alpha_NULL * H_raw_NULL)]

stopifnot(abs(mean(H_js$P_ATC,  na.rm = TRUE) - r_obs) < 1e-6)
stopifnot(abs(mean(H_js$P_NULL, na.rm = TRUE) - r_obs) < 1e-6)

message(sprintf("  alpha_ATC  = %.10f", alpha_ATC))
message(sprintf("  alpha_NULL = %.10f", alpha_NULL))
message("  Calibrated probability quantiles:")
message("    P_ATC  | ",
        paste(round(quantile(H_js$P_ATC,  c(.01, .50, .99), na.rm = TRUE), 4),
              collapse = " / "))
message("    P_NULL | ",
        paste(round(quantile(H_js$P_NULL, c(.01, .50, .99), na.rm = TRUE), 4),
              collapse = " / "))

# ------------------------------------------------------------------------------
# 7. Aggregate to destination wage quintile × domain
# ------------------------------------------------------------------------------
message("\n[7] Aggregating to quintile × domain...")

quint_domain <- H_js[, .(
  r_obs   = mean(as.numeric(diffusion), na.rm = TRUE),
  r_ATC   = mean(P_ATC,  na.rm = TRUE),
  r_NULL  = mean(P_NULL, na.rm = TRUE),
  n_cells = .N
), by = .(wage_quintile, domain)][order(domain, wage_quintile)]

print(quint_domain)

fwrite(quint_domain, file.path(out_tables, "atc_quintile_domain.csv"))
message("  Saved: tables/atc_quintile_domain.csv")

# ------------------------------------------------------------------------------
# 8. Build observed cognitive/physical ratio by quintile
# ------------------------------------------------------------------------------
message("\n[8] Computing cognitive / physical ratios...")

ratio_dt <- dcast(
  quint_domain,
  wage_quintile ~ domain,
  value.var = c("r_obs", "r_ATC", "r_NULL")
)

required_ratio_cols <- c(
  "r_obs_Cognitive", "r_obs_Physical",
  "r_ATC_Cognitive", "r_ATC_Physical",
  "r_NULL_Cognitive", "r_NULL_Physical"
)
miss_ratio_cols <- setdiff(required_ratio_cols, names(ratio_dt))
if (length(miss_ratio_cols) > 0L) {
  stop("Ratio table is missing required columns after cast: ",
       paste(miss_ratio_cols, collapse = ", "))
}

ratio_dt[, ratio_obs  := r_obs_Cognitive  / r_obs_Physical]
ratio_dt[, ratio_ATC  := r_ATC_Cognitive  / r_ATC_Physical]
ratio_dt[, ratio_NULL := r_NULL_Cognitive / r_NULL_Physical]

fwrite(ratio_dt, file.path(out_tables, "atc_ratio_quintile.csv"))
message("  Saved: tables/atc_ratio_quintile.csv")

# ------------------------------------------------------------------------------
# 9. Model-fit summaries for paper / SI
# ------------------------------------------------------------------------------
message("\n[9] Computing model-fit summaries...")

fit_summary <- quint_domain[, .(
  rmse_ATC  = sqrt(mean((r_obs - r_ATC)^2,  na.rm = TRUE)),
  rmse_NULL = sqrt(mean((r_obs - r_NULL)^2, na.rm = TRUE)),
  mae_ATC   = mean(abs(r_obs - r_ATC),  na.rm = TRUE),
  mae_NULL  = mean(abs(r_obs - r_NULL), na.rm = TRUE)
), by = domain]

overall_fit <- data.table(
  domain = "Overall",
  rmse_ATC  = sqrt(mean((quint_domain$r_obs - quint_domain$r_ATC)^2,  na.rm = TRUE)),
  rmse_NULL = sqrt(mean((quint_domain$r_obs - quint_domain$r_NULL)^2, na.rm = TRUE)),
  mae_ATC   = mean(abs(quint_domain$r_obs - quint_domain$r_ATC),  na.rm = TRUE),
  mae_NULL  = mean(abs(quint_domain$r_obs - quint_domain$r_NULL), na.rm = TRUE)
)

fit_summary <- rbind(fit_summary, overall_fit, fill = TRUE)

grad_summary <- quint_domain[, .(
  obs_q5_q1  = r_obs[wage_quintile == 5]  - r_obs[wage_quintile == 1],
  atc_q5_q1  = r_ATC[wage_quintile == 5]  - r_ATC[wage_quintile == 1],
  null_q5_q1 = r_NULL[wage_quintile == 5] - r_NULL[wage_quintile == 1]
), by = domain]

fit_summary <- merge(fit_summary, grad_summary, by = "domain", all = TRUE)
fit_summary[, better_rmse := fifelse(rmse_ATC < rmse_NULL, "ATC", "NULL")]
fit_summary[, better_mae  := fifelse(mae_ATC  < mae_NULL,  "ATC", "NULL")]

summary_lines <- quint_domain[, .(
  q1_obs   = r_obs[wage_quintile == 1],
  q5_obs   = r_obs[wage_quintile == 5],
  q1_atc   = r_ATC[wage_quintile == 1],
  q5_atc   = r_ATC[wage_quintile == 5],
  q1_null  = r_NULL[wage_quintile == 1],
  q5_null  = r_NULL[wage_quintile == 5],
  obs_gap  = r_obs[wage_quintile == 5]  - r_obs[wage_quintile == 1],
  atc_gap  = r_ATC[wage_quintile == 5]  - r_ATC[wage_quintile == 1],
  null_gap = r_NULL[wage_quintile == 5] - r_NULL[wage_quintile == 1]
), by = domain]

cog_obs_gap  <- summary_lines[domain == "Cognitive", obs_gap]
phy_obs_gap  <- summary_lines[domain == "Physical",  obs_gap]
cog_atc_gap  <- summary_lines[domain == "Cognitive", atc_gap]
phy_atc_gap  <- summary_lines[domain == "Physical",  atc_gap]
cog_null_gap <- summary_lines[domain == "Cognitive", null_gap]
phy_null_gap <- summary_lines[domain == "Physical",  null_gap]

pol_obs  <- cog_obs_gap  - phy_obs_gap
pol_atc  <- cog_atc_gap  - phy_atc_gap
pol_null <- cog_null_gap - phy_null_gap

fit_summary <- rbind(
  fit_summary,
  data.table(
    domain       = "Polarization index",
    rmse_ATC     = NA_real_,
    rmse_NULL    = NA_real_,
    mae_ATC      = NA_real_,
    mae_NULL     = NA_real_,
    obs_q5_q1    = pol_obs,
    atc_q5_q1    = pol_atc,
    null_q5_q1   = pol_null,
    better_rmse  = ifelse(abs(pol_obs - pol_atc) < abs(pol_obs - pol_null), "ATC", "NULL"),
    better_mae   = ifelse(abs(pol_obs - pol_atc) < abs(pol_obs - pol_null), "ATC", "NULL")
  ),
  fill = TRUE
)

print(fit_summary)
fwrite(fit_summary, file.path(out_tables, "atc_model_fit_summary.csv"))
message("  Saved: tables/atc_model_fit_summary.csv")

# Annotation table for Panel C / caption use
panel_c_ann <- quint_domain[, .(
  rmse_ATC  = sqrt(mean((r_obs - r_ATC)^2)),
  rmse_NULL = sqrt(mean((r_obs - r_NULL)^2)),
  mae_ATC   = mean(abs(r_obs - r_ATC)),
  mae_NULL  = mean(abs(r_obs - r_NULL)),
  obs_gap   = r_obs[wage_quintile == 5]  - r_obs[wage_quintile == 1],
  atc_gap   = r_ATC[wage_quintile == 5]  - r_ATC[wage_quintile == 1],
  null_gap  = r_NULL[wage_quintile == 5] - r_NULL[wage_quintile == 1]
), by = domain]

panel_c_ann[, rmse_label := sprintf("RMSE: ATC %.3f | NULL %.3f", rmse_ATC, rmse_NULL)]
panel_c_ann[, gap_label  := sprintf("Q5-Q1: Obs %.3f | ATC %.3f | NULL %.3f",
                                    obs_gap, atc_gap, null_gap)]

fwrite(panel_c_ann, file.path(out_tables, "atc_panel_c_annotations.csv"))
message("  Saved: tables/atc_panel_c_annotations.csv")

# ------------------------------------------------------------------------------
# 10. Prepare plotting data
# ------------------------------------------------------------------------------
message("\n[10] Preparing plotting data...")

Q_LABS <- c("Q1\n(lowest)", "Q2", "Q3", "Q4", "Q5\n(highest)")

PAL_DOMAIN <- c(
  Cognitive = "#2ABAB2",
  Physical  = "#CC4444"
)

PAL_MODEL <- c(
  Observed = "black",
  ATC      = "#2ABAB2",
  NULL     = "grey55"
)

DOMAIN_LABS <- c(
  Cognitive = "Socio-cognitive",
  Physical  = "Physical-sensory"
)

plot_A <- copy(quint_domain)
plot_A[, domain := factor(domain, levels = c("Cognitive", "Physical"))]

plot_B <- ratio_dt[, .(
  wage_quintile,
  ratio_obs
)]

compute_crossing <- function(x, y, target = 1) {
  idx <- which(diff(sign(y - target)) != 0)
  if (length(idx) == 0L) return(NA_real_)
  i <- idx[1]
  x1 <- x[i]
  x2 <- x[i + 1]
  y1 <- y[i]
  y2 <- y[i + 1]
  x1 + (target - y1) * (x2 - x1) / (y2 - y1)
}

cross_obs <- compute_crossing(plot_B$wage_quintile, plot_B$ratio_obs, target = 1)

plot_C <- melt(
  quint_domain,
  id.vars = c("wage_quintile", "domain"),
  measure.vars = c("r_obs", "r_ATC", "r_NULL"),
  variable.name = "series",
  value.name = "rate"
)

plot_C[, series := fifelse(series == "r_obs",  "Observed",
                    fifelse(series == "r_ATC", "ATC", "NULL"))]
plot_C[, series := factor(series, levels = c("Observed", "ATC", "NULL"))]
plot_C[, domain := factor(domain, levels = c("Cognitive", "Physical"))]

# Panel C annotation placement
ann_C <- copy(panel_c_ann)
ann_C[, domain := factor(domain, levels = c("Cognitive", "Physical"))]
ymax_C <- plot_C[, .(y_top = max(rate, na.rm = TRUE)), by = domain]
ann_C <- merge(ann_C, ymax_C, by = "domain", all.x = TRUE)
ann_C[, x := 1.05]
ann_C[, y1 := y_top * 0.98]
ann_C[, y2 := y_top * 0.87]

# ------------------------------------------------------------------------------
# 11. Science Advances figure
# ------------------------------------------------------------------------------
message("\n[11] Building Science Advances figure...")

PT_BASE  <- 11
PT_TITLE <- 13
PT_STRIP <- 12

theme_sciadv_main <- theme_classic(base_size = PT_BASE, base_family = "Helvetica") +
  theme(
    strip.text        = element_text(size = PT_STRIP, face = "plain"),
    strip.background  = element_blank(),
    axis.title        = element_text(size = PT_TITLE),
    axis.text         = element_text(size = PT_BASE, colour = "grey10"),
    legend.title      = element_blank(),
    legend.text       = element_text(size = PT_BASE - 0.5),
    legend.position   = "bottom",
    legend.background = element_rect(fill = "white", colour = NA),
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.background   = element_rect(fill = "white", colour = NA),
    panel.border      = element_blank(),
    panel.grid        = element_blank(),
    axis.line         = element_line(linewidth = 0.35, colour = "grey20"),
    axis.ticks        = element_line(linewidth = 0.3, colour = "grey30"),
    axis.ticks.length = unit(2, "pt"),
    plot.title        = element_text(size = PT_TITLE, face = "plain", colour = "grey10"),
    plot.margin       = margin(t = 6, r = 8, b = 6, l = 6, unit = "pt"),
    legend.key.width  = unit(0.9, "cm"),
    legend.spacing.x  = unit(4, "pt"),
    panel.spacing.x   = unit(10, "pt"),
    panel.spacing.y   = unit(10, "pt")
  )

# Compact annotations for Panel C: use only ONE line per facet
ann_C_plot <- copy(panel_c_ann)
ann_C_plot[, domain := factor(domain, levels = c("Cognitive", "Physical"))]
ann_C_plot[, short_label := sprintf("RMSE: %.3f vs %.3f | Q5-Q1: %.3f vs %.3f",
                                    rmse_ATC, rmse_NULL, atc_gap, null_gap)]

ymax_C <- plot_C[, .(y_top = max(rate, na.rm = TRUE)), by = domain]
ann_C_plot <- merge(ann_C_plot, ymax_C, by = "domain", all.x = TRUE)
ann_C_plot[, x := 1.05]
ann_C_plot[, y := y_top * 0.98]

# Panel A ----------------------------------------------------
pA <- ggplot(
  plot_A,
  aes(x = wage_quintile, y = r_obs, colour = domain, group = domain)
) +
  geom_line(linewidth = 1.1, lineend = "round") +
  geom_point(size = 2.8) +
  scale_colour_manual(values = PAL_DOMAIN, labels = DOMAIN_LABS) +
  scale_x_continuous(
    breaks = 1:5,
    labels = Q_LABS,
    expand = expansion(mult = c(0.03, 0.03))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.10))) +
  labs(
    title = "(A) Observed adoption rates across the wage distribution",
    x = "Destination wage quintile",
    y = "Observed adoption rate"
  ) +
  theme_sciadv_main +
  theme(
    legend.position = "none"
  )

# Panel B ----------------------------------------------------
pB <- ggplot(
  plot_B,
  aes(x = wage_quintile, y = ratio_obs)
) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey60", linewidth = 0.35) +
  geom_line(linewidth = 1.1, colour = "black", lineend = "round") +
  geom_point(size = 2.8, colour = "black") +
  scale_x_continuous(
    breaks = 1:5,
    labels = Q_LABS,
    expand = expansion(mult = c(0.03, 0.03))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    title = "(B) Observed cognitive-to-physical adoption ratio",
    x = "Destination wage quintile",
    y = "Cognitive / Physical ratio"
  ) +
  theme_sciadv_main +
  theme(
    legend.position = "none"
  )

if (is.finite(cross_obs)) {
  pB <- pB +
    annotate(
      "text",
      x = cross_obs,
      y = max(plot_B$ratio_obs, na.rm = TRUE),
      label = sprintf("Crossing \u2248 Q%.1f", cross_obs),
      family = "Helvetica",
      size = 3.3,
      vjust = -0.15,
      colour = "black"
    )
}

# Panel C ----------------------------------------------------
pC <- ggplot(
  plot_C,
  aes(x = wage_quintile, y = rate, colour = series, linetype = series, group = series)
) +
  geom_line(linewidth = 1.0, lineend = "round") +
  geom_point(size = 2.5) +
  facet_wrap(
    ~ domain,
    ncol = 2,
    labeller = labeller(domain = DOMAIN_LABS)
  ) +
  geom_text(
    data = ann_C_plot,
    aes(x = x, y = y, label = short_label),
    inherit.aes = FALSE,
    hjust = 0, vjust = 1,
    family = "Helvetica",
    size = 2.9,
    colour = "grey20"
  ) +
  scale_colour_manual(values = PAL_MODEL) +
  scale_linetype_manual(values = c(Observed = "solid", ATC = "solid", NULL = "dashed")) +
  scale_x_continuous(
    breaks = 1:5,
    labels = Q_LABS,
    expand = expansion(mult = c(0.03, 0.03))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.06, 0.18))) +
  labs(
    title = "(C) Observed versus model-implied adoption rates",
    x = "Destination wage quintile",
    y = "Adoption rate"
  ) +
  theme_sciadv_main +
  theme(
    legend.position = "bottom"
  )

# Assemble ---------------------------------------------------
fig_main <- pA / pB / pC +
  plot_layout(heights = c(0.95, 0.90, 1.35), guides = "collect") &
  theme(
    legend.position  = "bottom",
    legend.direction = "horizontal"
  )

ggsave(
  filename = file.path(out_figs, "fig_atc_polarization_main.pdf"),
  plot = fig_main,
  width = 10.0,
  height = 11.0,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

message("  Saved: figs/fig_atc_polarization_main.pdf")

# ------------------------------------------------------------------------------
# 12. Console summary for manuscript interpretation
# ------------------------------------------------------------------------------
message("\n[12] Manuscript-oriented summary...")

print(summary_lines)

message(sprintf("\n  Polarization index (Q5-Q1 Cognitive gap minus Q5-Q1 Physical gap):"))
message(sprintf("    Observed = %+0.4f", pol_obs))
message(sprintf("    ATC      = %+0.4f", pol_atc))
message(sprintf("    NULL     = %+0.4f", pol_null))

if (abs(pol_obs - pol_atc) < abs(pol_obs - pol_null)) {
  message("    Result: ATC is closer to the observed polarization gradient than NULL.")
} else {
  message("    Result: NULL is not outperformed by ATC on this scalar summary. Inspect outputs.")
}

if (all(panel_c_ann$rmse_ATC < panel_c_ann$rmse_NULL)) {
  message("    Result: ATC reduces RMSE relative to NULL in both domains.")
} else {
  message("    Result: ATC does not reduce RMSE in both domains. Inspect domain-level fit.")
}

message("\n==============================================================")
message("Done.")
message("Key outputs:")
message("  - output/output_main/tables/atc_quintile_domain.csv")
message("  - output/output_main/tables/atc_ratio_quintile.csv")
message("  - output/output_main/tables/atc_model_fit_summary.csv")
message("  - output/output_main/tables/atc_panel_c_annotations.csv")
message("  - output/output_main/figs/fig_atc_polarization_main.pdf")
message("==============================================================")