# ==============================================================================
# 00_build_dyads.R
#
# Triadic dataset construction — v12 FINAL
#
# Logic: Strict directional flow (risk set 2015 → 2024) + baseline status
#        gaps at t0.
#
# Structural distance measures (all computed from 2015 baseline):
#   - jaccard_dist       binary Jaccard distance
#   - cosine_bin_dist    binary cosine distance
#   - cosine_wt_dist     RCA-weighted cosine distance  [used as structural_distance]
#   - sp_dist            shortest-path distance on the 2015 occupational network
#
# Output:
#   data/derived/all_events_v12_FINAL.rds   (~17.3M rows, local only)
#
# Runtime: ~2–4 hours. Requires ≥ 32 GB RAM.
# Run once; all estimation scripts use the cached output.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------------------
gc()
library(data.table)
library(readxl)
library(progress)
library(Matrix)
library(lsa)
library(igraph)
library(stringr)

# ------------------------------------------------------------------------------
# 1. Paths (override via R/99_paths_local.R)
# ------------------------------------------------------------------------------
if (file.exists("R/99_paths_local.R")) source("R/99_paths_local.R")

path_2015    <- if (exists("PATH_ONET_2015"))      PATH_ONET_2015     else "data/db_15_1"
path_2024    <- if (exists("PATH_ONET_2024"))      PATH_ONET_2024     else "data/db_29_2_text"
path_cw      <- if (exists("PATH_CROSSWALK"))      PATH_CROSSWALK     else "data/crosswalk"
path_bls_15  <- if (exists("PATH_BLS_WAGES_2015")) PATH_BLS_WAGES_2015 else "data/national_M2015_dl.xlsx"

output_data_dir <- "data/derived"
dir.create(output_data_dir, showWarnings = FALSE, recursive = TRUE)

file_cw_soc10_19 <- "2010_to_2019_Crosswalk.csv"

# ==============================================================================
# 2. Helper functions
# ==============================================================================

# Standardise SOC codes: remove dashes, trailing ".00", and dots
clean_id <- function(x) {
  s <- as.character(x)
  s <- gsub("-",     "", s)
  s <- gsub("\\.00$","", s)
  s <- gsub("\\.",   "", s)
  stringr::str_trim(s)
}

# Load a single O*NET content model file; returns NULL if not found
load_onet <- function(folder, filename) {
  f <- file.path(folder, filename)
  if (!file.exists(f)) {
    pat   <- gsub(".txt", "", filename, fixed = TRUE)
    f_alt <- list.files(folder, pattern = pat, full.names = TRUE)
    if (length(f_alt) > 0) f <- f_alt[1] else return(NULL)
  }
  d <- fread(f, sep = "\t", quote = "", na.strings = c("NA", "n/a", "", "*"),
             showProgress = FALSE)
  setnames(d, names(d), make.names(names(d)))
  if ("O.NET.SOC.Code" %in% names(d)) d[, soc_code := clean_id(O.NET.SOC.Code)]
  val_col <- grep("Data.?Value", names(d), value = TRUE)[1]
  if (!is.na(val_col)) d[, value := as.numeric(get(val_col))]
  d
}

# Compute Revealed Comparative Advantage for each (occupation, skill) pair
calc_rca <- function(d) {
  # Average across duplicate rows that may arise after the SOC crosswalk
  agg     <- d[, .(v = mean(value, na.rm = TRUE)), by = .(soc_code, Element.Name)]
  occ_sum <- agg[, .(ot = sum(v)), by = soc_code]
  ski_sum <- agg[, .(st = sum(v)), by = Element.Name]
  gt      <- sum(agg$v)
  agg <- merge(agg, occ_sum, by = "soc_code")
  agg <- merge(agg, ski_sum, by = "Element.Name")
  agg[, rca := (v / ot) / (st / gt)]
  agg[, .(soc_code, skill = Element.Name, rca)]
}

save_derived <- function(x, name) {
  saveRDS(x, file.path(output_data_dir, name))
}

# ==============================================================================
# 3. Baseline O*NET processing (2015, t0)
# ==============================================================================
message("\n>>> Step 1: Processing O*NET 2015 (baseline)...")

onet_files <- c("Skills.txt", "Abilities.txt", "Knowledge.txt", "Work Activities.txt")
dt_15 <- rbindlist(lapply(onet_files, function(x) load_onet(path_2015, x)), fill = TRUE)
dt_15 <- dt_15[Scale.ID == "IM" & !is.na(value)]

# Apply SOC 2010 -> 2019 crosswalk
cw_file <- file.path(path_cw, file_cw_soc10_19)
if (file.exists(cw_file)) {
  cw <- fread(cw_file)
  cw_clean <- unique(cw[, .(
    soc10 = clean_id(`O*NET-SOC 2010 Code`),
    soc19 = clean_id(`O*NET-SOC 2019 Code`)
  )])
  dt_15 <- merge(dt_15, cw_clean, by.x = "soc_code", by.y = "soc10",
                 all.x = FALSE, allow.cartesian = TRUE)
  dt_15[, soc_code := soc19]
  rm(cw, cw_clean)
} else {
  warning("Crosswalk file not found: ", cw_file,
          "\nProceeding without SOC harmonisation.")
}

rca_15 <- calc_rca(dt_15)
setnames(rca_15, "rca", "rca_t0")
message("  Occupations (2015, mapped): ", uniqueN(rca_15$soc_code))
rm(dt_15); gc()

# ==============================================================================
# 4. Outcome O*NET processing (2024, t1)
# ==============================================================================
message("\n>>> Step 2: Processing O*NET 2024 (outcome)...")

dt_24 <- rbindlist(lapply(onet_files, function(x) load_onet(path_2024, x)), fill = TRUE)
dt_24 <- dt_24[Scale.ID == "IM" & !is.na(value)]
rca_24 <- calc_rca(dt_24)
setnames(rca_24, "rca", "rca_t1")
rm(dt_24); gc()

# ==============================================================================
# 5. Skill classification and structural distances (2015 baseline)
# ==============================================================================
message("\n>>> Step 3: Computing skill clusters and structural distances (2015)...")

mat_dt           <- rca_15[rca_t0 > 1]
skill_classes    <- data.table(skill = unique(rca_15$skill), domain = "Unknown")
dist_dt          <- NULL
occ_edges_for_sp <- NULL

if (nrow(mat_dt) > 0) {

  # --- A. Skill clustering via Louvain (2015 co-specialisation network) ---
  mat_wide  <- dcast(mat_dt, skill ~ soc_code, value.var = "rca_t0", fun.aggregate = length)
  mat       <- as.matrix(mat_wide[, -1])
  rownames(mat) <- mat_wide$skill

  sim_skill <- lsa::cosine(t(mat))
  sim_skill[is.nan(sim_skill)] <- 0
  diag(sim_skill) <- 0

  g_skill <- graph_from_adjacency_matrix(sim_skill, mode = "undirected", weighted = TRUE)
  g_skill <- delete_edges(g_skill, E(g_skill)[weight < 0.1])
  lou     <- cluster_louvain(g_skill)

  skill_classes <- data.table(skill = lou$names, cluster = paste0("C", lou$membership))
  # Cluster C1 maps to Cognitive; all others to Physical.
  # Verify this assignment against Figure 1 after construction.
  skill_classes[, domain := fifelse(cluster == "C1", "Cognitive", "Physical")]

  # --- B. Occupation x skill matrices (binary and RCA-weighted) ---
  message("  Building occupation x skill matrices...")

  occ_wide_bin <- dcast(mat_dt, soc_code ~ skill, fun.aggregate = length, value.var = "rca_t0")
  occ_ids      <- occ_wide_bin$soc_code
  occ_mat_bin  <- as.matrix(occ_wide_bin[, -1])
  occ_mat_bin  <- (occ_mat_bin > 0) * 1L

  occ_wide_wt <- dcast(mat_dt, soc_code ~ skill, fun.aggregate = mean, value.var = "rca_t0")
  occ_mat_wt  <- as.matrix(occ_wide_wt[, -1])
  occ_mat_wt[is.na(occ_mat_wt)] <- 0

  M_bin <- Matrix::Matrix(occ_mat_bin, sparse = TRUE)
  M_wt  <- Matrix::Matrix(occ_mat_wt,  sparse = TRUE)

  # --- Distance 1: Jaccard (binary), sparse ---
  message("  Computing Jaccard distance (binary)...")
  inter_bin <- tcrossprod(M_bin)
  rs_bin    <- Matrix::rowSums(M_bin)
  inter_sum <- summary(inter_bin)

  j_dt <- data.table(
    source = occ_ids[inter_sum$i],
    target = occ_ids[inter_sum$j],
    inter  = as.numeric(inter_sum$x)
  )[source != target]

  rs_vec <- as.numeric(rs_bin); names(rs_vec) <- occ_ids
  j_dt[, union       := rs_vec[source] + rs_vec[target] - inter]
  j_dt[, jaccard_sim := fifelse(union > 0, inter / union, 0)]
  j_dt  <- j_dt[is.finite(jaccard_sim) & jaccard_sim > 0]
  j_dt[, jaccard_dist := 1 - jaccard_sim]
  j_dt[, c("inter", "union", "jaccard_sim") := NULL]

  # --- Distance 2: Cosine (binary), reuses inter_sum ---
  message("  Computing cosine distance (binary)...")
  cbin_dt <- data.table(
    source = occ_ids[inter_sum$i],
    target = occ_ids[inter_sum$j],
    dot    = as.numeric(inter_sum$x)
  )[source != target]

  cbin_dt[, cosine_bin_sim  := dot / (sqrt(rs_vec[source]) * sqrt(rs_vec[target]))]
  cbin_dt  <- cbin_dt[is.finite(cosine_bin_sim) & cosine_bin_sim > 0]
  cbin_dt[, cosine_bin_dist := 1 - cosine_bin_sim]
  cbin_dt[, c("dot", "cosine_bin_sim") := NULL]

  # --- Distance 3: Cosine (RCA-weighted) ---
  message("  Computing cosine distance (RCA-weighted)...")
  dot_wt   <- tcrossprod(M_wt)
  norm_wt  <- sqrt(Matrix::rowSums(M_wt^2))
  norm_vec <- as.numeric(norm_wt); names(norm_vec) <- occ_ids
  dotw_sum <- summary(dot_wt)

  cwt_dt <- data.table(
    source = occ_ids[dotw_sum$i],
    target = occ_ids[dotw_sum$j],
    dot    = as.numeric(dotw_sum$x)
  )[source != target]

  cwt_dt[, cosine_wt_sim  := dot / (norm_vec[source] * norm_vec[target])]
  cwt_dt  <- cwt_dt[is.finite(cosine_wt_sim) & cosine_wt_sim > 0]
  cwt_dt[, cosine_wt_dist := 1 - cosine_wt_sim]
  cwt_dt[, c("dot", "cosine_wt_sim") := NULL]

  # Merge all pairwise distances
  message("  Merging distance measures...")
  setkey(j_dt,    source, target)
  setkey(cbin_dt, source, target)
  setkey(cwt_dt,  source, target)

  dist_dt <- merge(j_dt, cbin_dt, all = TRUE)
  dist_dt <- merge(dist_dt, cwt_dt, all = TRUE)
  dist_dt[is.na(jaccard_dist),    jaccard_dist    := 1]
  dist_dt[is.na(cosine_bin_dist), cosine_bin_dist := 1]
  dist_dt[is.na(cosine_wt_dist),  cosine_wt_dist  := 1]

  # --- Shortest-path edge list (2015 occupational network) ---
  message("  Building shortest-path edge list (cosine-weighted similarity >= 0.10)...")
  occ_edges_for_sp <- dist_dt[, .(source, target, sim = 1 - cosine_wt_dist)]
  occ_edges_for_sp <- occ_edges_for_sp[source != target]
  occ_edges_for_sp[, sim    := pmin(1, pmax(0, sim))]   # clamp to [0,1]
  occ_edges_for_sp  <- occ_edges_for_sp[sim >= 0.10]
  occ_edges_for_sp[, w_dist := pmax(0, 1 - sim)]
  occ_edges_for_sp[, sim    := NULL]
  setkey(occ_edges_for_sp, source, target)
  occ_edges_for_sp <- occ_edges_for_sp[,
    .(w_dist = min(w_dist, na.rm = TRUE)), by = .(source, target)]

  save_derived(occ_edges_for_sp, "occ_edges_for_sp_2015.rds")

  rm(mat, sim_skill, g_skill, lou, mat_dt,
     occ_wide_bin, occ_mat_bin, occ_wide_wt, occ_mat_wt,
     M_bin, M_wt, inter_bin, dot_wt, rs_bin, norm_wt,
     inter_sum, dotw_sum, j_dt, cbin_dt, cwt_dt)
  gc()
}

# ==============================================================================
# 6. Dyad construction (risk set and diffusion outcome)
# ==============================================================================
message("\n>>> Step 4: Building dyadic risk set...")

panel <- merge(rca_15, rca_24, by = c("soc_code", "skill"), all = TRUE)
panel[is.na(rca_t0), rca_t0 := 0]
panel[is.na(rca_t1), rca_t1 := 0]

# Source occupations: specialised at t0 (RCA > 1)
sources_t0 <- panel[rca_t0 > 1.0, .(source = soc_code, skill)]

# Target occupations: NOT specialised at t0 — these are at risk of adoption
targets_risk <- panel[rca_t0 <= 1.0, .(target = soc_code, skill, rca_t1)]
targets_risk[, diffusion := fifelse(rca_t1 > 1.0, 1L, 0L)]

message("  Total risk set (target x skill pairs): ",
        format(nrow(targets_risk), big.mark = ","))

# For each skill: cross all sources with all at-risk targets.
# Cap negatives at 2000 per skill to manage dataset size.
all_dyads_list <- list()
skills_vec     <- unique(sources_t0$skill)
pb <- progress_bar$new(
  format = "  [:bar] :percent eta: :eta | skill :current/:total",
  total  = length(skills_vec), clear = FALSE
)

for (sk in skills_vec) {
  pb$tick()
  srcs <- sources_t0[skill == sk]$source
  tgts <- targets_risk[skill == sk]
  if (length(srcs) == 0 || nrow(tgts) == 0) next

  pos <- tgts[diffusion == 1]
  neg <- tgts[diffusion == 0]
  if (nrow(neg) > 2000) neg <- neg[sample(.N, 2000)]
  tgts_final <- rbind(pos, neg)

  pairs <- as.data.table(expand.grid(
    source = srcs, target = tgts_final$target, stringsAsFactors = FALSE
  ))
  pairs <- pairs[source != target]
  pairs <- merge(pairs, tgts_final[, .(target, diffusion)], by = "target")
  pairs[, skill_name := sk]

  all_dyads_list[[sk]] <- pairs
}

all_events <- rbindlist(all_dyads_list)
rm(all_dyads_list, panel, sources_t0, targets_risk); gc()
message("\n  Dyadic event base: ", format(nrow(all_events), big.mark = ","), " rows")

# ==============================================================================
# 7. Enrichment: wages, status gaps, domain labels, structural distances
# ==============================================================================
message("\n>>> Step 5: Enriching with wages (BLS 2015) and structural distances...")

get_wages <- function(path) {
  if (!file.exists(path)) return(NULL)
  d   <- as.data.table(read_excel(path, sheet = 1))
  cc  <- grep("OCC_CODE", names(d), value = TRUE)[1]
  cw  <- grep("A_MEDIAN",  names(d), value = TRUE)[1]
  if (is.na(cc) || is.na(cw)) return(NULL)
  d[, w := suppressWarnings(as.numeric(as.character(get(cw))))]
  d[, c := clean_id(get(cc))]
  d[!is.na(w), .(wage = mean(w), log_w = log(mean(w))), by = c]
}

w15 <- get_wages(path_bls_15)
if (is.null(w15)) stop("Could not load BLS 2015 wages. Check PATH_BLS_WAGES_2015.")

s_wage <- copy(w15); setnames(s_wage, c("c","wage","log_w"), c("source","s_wage","log_s_wage"))
t_wage <- copy(w15); setnames(t_wage, c("c","wage","log_w"), c("target","t_wage","log_t_wage"))
all_events <- merge(all_events, s_wage, by = "source", all.x = TRUE)
all_events <- merge(all_events, t_wage, by = "target", all.x = TRUE)
rm(w15, s_wage, t_wage)

# Log wage gap (primary status measure for ATC estimation)
all_events[, wage_gap  := log_t_wage - log_s_wage]
all_events[, wage_up   := pmax(0,  wage_gap)]
all_events[, wage_down := pmin(0,  wage_gap)]
all_events[, up_dummy  := fifelse(wage_gap > 0, 1L, 0L)]

# Auxiliary absolute/relative gaps (used in descriptive scripts only)
all_events[, wage_diff_abs := t_wage - s_wage]
all_events[, wage_diff_rel := (t_wage - s_wage) / s_wage]

# Domain labels
all_events <- merge(all_events, skill_classes[, .(skill, domain)],
                    by.x = "skill_name", by.y = "skill", all.x = TRUE)

# Structural distances
if (!is.null(dist_dt)) {
  message("  Merging structural distances (Jaccard, cosine binary, cosine weighted)...")
  setkey(all_events, source, target)
  setkey(dist_dt,    source, target)
  all_events <- merge(all_events, dist_dt, all.x = TRUE)
  all_events[is.na(jaccard_dist),    jaccard_dist    := 1]
  all_events[is.na(cosine_bin_dist), cosine_bin_dist := 1]
  all_events[is.na(cosine_wt_dist),  cosine_wt_dist  := 1]
} else {
  all_events[, c("jaccard_dist", "cosine_bin_dist", "cosine_wt_dist") := 1]
}

# Shortest-path distance
if (!is.null(occ_edges_for_sp) && nrow(occ_edges_for_sp) > 0) {
  message("  Computing shortest-path distances on 2015 occupational network...")
  g_occ  <- igraph::graph_from_data_frame(
    occ_edges_for_sp[, .(source, target, w_dist)], directed = FALSE)
  nodes  <- igraph::V(g_occ)$name
  dmat   <- igraph::distances(g_occ, weights = igraph::E(g_occ)$w_dist)

  pairs_need <- unique(all_events[source != target, .(source, target)])
  i_idx <- match(pairs_need$source, nodes)
  j_idx <- match(pairs_need$target, nodes)
  sp    <- rep(NA_real_, nrow(pairs_need))
  ok    <- !is.na(i_idx) & !is.na(j_idx)
  sp[ok] <- dmat[cbind(i_idx[ok], j_idx[ok])]

  max_finite <- suppressWarnings(max(sp[is.finite(sp)], na.rm = TRUE))
  if (!is.finite(max_finite)) max_finite <- 1
  sp[!is.finite(sp) | is.na(sp)] <- max_finite + 1

  pairs_need[, sp_dist := sp]
  setkey(pairs_need, source, target)
  setkey(all_events, source, target)
  all_events <- merge(all_events, pairs_need, all.x = TRUE)
  rm(dmat, pairs_need, g_occ)
} else {
  message("  occ_edges_for_sp not available; sp_dist set to NA.")
  all_events[, sp_dist := NA_real_]
}

# Primary structural distance variable used in all estimation scripts
all_events[, structural_distance := cosine_wt_dist]

# ==============================================================================
# 8. Final diagnostics and save
# ==============================================================================
message("\n>>> Step 6: Final diagnostics...")

final_dt <- all_events[!is.na(wage_diff_rel) & !is.na(domain)]
final_dt[, year_adoption := 2023L]
rm(all_events); gc()

message(sprintf(
  "  Final dataset: %s rows | diffusion rate: %.2f%% | sources: %d | targets: %d",
  format(nrow(final_dt), big.mark = ","),
  mean(final_dt$diffusion) * 100,
  uniqueN(final_dt$source),
  uniqueN(final_dt$target)
))

message("  ATC directionality check:")
print(final_dt[, .(
  min_log_gap      = min(wage_gap,       na.rm = TRUE),
  max_log_gap      = max(wage_gap,       na.rm = TRUE),
  mean_up_gap      = mean(wage_up,       na.rm = TRUE),
  mean_down_gap    = mean(abs(wage_down),na.rm = TRUE)
)])

save_path <- file.path(output_data_dir, "all_events_v12_FINAL.rds")
saveRDS(final_dt, save_path)
message("\n>>> Saved: ", save_path)

rm(list = setdiff(ls(), "final_dt")); gc()
message("Done. Object 'final_dt' available in session.")