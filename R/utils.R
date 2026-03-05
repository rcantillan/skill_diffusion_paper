# =============================================================================
# utils.R — Shared utility functions for the ATC analysis pipeline
# Source this file at the top of any script that needs extract_coefs().
# =============================================================================

#' Extract ATC-relevant coefficients from a fixest model
#' @param m A fixest model object
#' @param panel_short Character: "Panel A" or "Panel B"
#' @param threshold_val Numeric: RCA threshold (optional, for sensitivity scripts)
#' @return data.table with columns: panel_short, coef, estimate, se [, threshold]
extract_coefs <- function(m, panel_short, threshold_val = NULL) {
  ct <- fixest::coeftable(m)
  rn <- rownames(ct)

  find_one <- function(pats) {
    for (p in pats) {
      h <- grep(p, rn, value = TRUE)[1L]
      if (!is.na(h)) return(c(ct[h, "Estimate"], ct[h, "Std. Error"]))
    }
    c(NA_real_, NA_real_)
  }

  params <- list(
    Theta_up_Cog = find_one(c("wage_up:domainCognitive", "domainCognitive:wage_up")),
    Theta_dn_Cog = find_one(c("wage_down:domainCognitive", "domainCognitive:wage_down")),
    kappa_Cog    = find_one(c("up_dummy:domainCognitive", "domainCognitive:up_dummy")),
    delta_Cog    = find_one(c("structural_distance:domainCognitive", "domainCognitive:structural_distance")),
    Theta_up_Phy = find_one(c("wage_up:domainPhysical", "domainPhysical:wage_up")),
    Theta_dn_Phy = find_one(c("wage_down:domainPhysical", "domainPhysical:wage_down")),
    kappa_Phy    = find_one(c("up_dummy:domainPhysical", "domainPhysical:up_dummy")),
    delta_Phy    = find_one(c("structural_distance:domainPhysical", "domainPhysical:structural_distance"))
  )

  out <- data.table::data.table(
    panel_short = panel_short,
    coef        = names(params),
    estimate    = vapply(params, `[`, numeric(1), 1),
    se          = vapply(params, `[`, numeric(1), 2)
  )
  if (!is.null(threshold_val)) out[, threshold := threshold_val]
  out
}
