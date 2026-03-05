# ==============================================================================
# SCRIPT GLOBAL SCD: "THE GOLD STANDARD" (FULL INTEGRATION)
# Framework: Asymmetric Trajectory Channeling (ATC)
# Metodología: Hosseinioun et al. (2025) & Abrevaya & Muris (2020)
# ==============================================================================

# 0. SETUP Y OPTIMIZACIÓN DE SISTEMA -------------------------------------------
gc()
if (!require("progressr")) install.packages("progressr")
if (!require("future.apply")) install.packages("future.apply")

library(data.table)
library(fixest)         # Para modelos ClogLog con FE masivos
library(vegan)          # Para métricas de nestedness (NODF)
library(future.apply)   # Para paralelización
library(progressr)      # Barra de avance para procesos paralelos
library(ggplot2)
library(scales)
library(tidyverse)

# Configuración de núcleos (usa N-1 para no colapsar el sistema)
n_cores <- parallel::detectCores() - 1
plan(multisession, workers = n_cores)

# Configurar el manejador de la barra de progreso
handlers(handler_progress(
  format = "[:bar] :percent :eta :message",
  clear = FALSE
))

message(">>> SCD Thesis Lab: Sistema listo con ", n_cores, " núcleos.")


# 2. CÁLCULO DE NESTEDNESS (MÉTODO HOSSEINIOUN ET AL. 2025) --------------------
# El paper define cs = (N_obs - mean(N_null)) / sd(N_null) 
message(">>> Iniciando cálculo de Contribution to Nestedness (cs)...")

# Crear matriz binaria Ocupación-Skill
occ_skill_pairs <- unique(dt[, .(soc = source, skill = skill_name)])
M_wide <- dcast(occ_skill_pairs, soc ~ skill, fun.aggregate = length, fill = 0)
M <- as.matrix(M_wide[, -1])
skill_names <- colnames(M)

# OPTIMIZACIÓN TURBO: Pre-ordenar la matriz por totales marginales [cite: 857, 861]
# 'nestednodf' es lento porque re-ordena en cada iteración. Ordenamos una vez aquí.
M <- M[order(rowSums(M), decreasing = TRUE), order(colSums(M), decreasing = TRUE)]
skill_names <- colnames(M) 

# Calcular NODF Observado desactivando el re-ordenamiento interno para ganar velocidad
N_obs <- nestednodf(M, order = FALSE)$statistic["NODF"]

# Parámetro de simulación: 500 simulaciones (matches SI Section S3.3)
n_sim_run <- 500 

with_progress({
  p <- progressor(steps = length(skill_names))
  
  cs_results <- future_lapply(seq_along(skill_names), function(i) {
    p(message = sprintf("Procesando: %s", skill_names[i]))
    
    focal_vec <- M[, i]
    # Generar ensamble nulo manteniendo la 'generality' constante [cite: 337]
    null_dist <- replicate(n_sim_run, {
      M_null_col <- M
      M_null_col[, i] <- sample(focal_vec) 
      # Usar order = FALSE es crítico para que la barra avance rápido
      nestednodf(M_null_col, order = FALSE)$statistic["NODF"]
    })
    
    # Retornar cs (Z-score de contribución estructural) 
    return((N_obs - mean(null_dist)) / sd(null_dist))
  }, future.seed = TRUE)
})

gc()
skill_stats <- data.table(skill_name = skill_names, cs = unlist(cs_results))

# Categorización: cs > 0 actúa como andamiaje (scaffolding) [cite: 336, 339]
skill_stats[, nestedness_cat := ifelse(cs > 0, "Nested", "Un-nested")]
skill_stats[, nestedness_tercile := cut(cs, breaks = quantile(cs, c(0, 1/3, 2/3, 1), na.rm=T), 
                                        labels = c("Low", "Mid", "High"), include.lowest = TRUE)]

dt <- merge(dt, skill_stats, by = "skill_name", all.x = TRUE)
gc()
#saveRDS(dt, file = "dt_diadas.rds")



# ==============================================================================
# PARTE 3: MUESTREO ESTRATIFICADO
# ==============================================================================

message("\n>>> Preparando muestra...")
dt <- dt_diadas; rm(dt_diadas)

n_total <- nrow(dt)
if (n_total > 5000000) {
  set.seed(12345)
  target_n <- 5000000
  prop_pos <- mean(dt$diffusion)
  
  n_pos_sample <- round(target_n * prop_pos)
  n_neg_sample <- target_n - n_pos_sample
  
  dt_pos <- dt[diffusion == 1][sample(.N, min(.N, n_pos_sample))]
  dt_neg <- dt[diffusion == 0][sample(.N, n_neg_sample)]
  dt_model <- rbind(dt_pos, dt_neg)[sample(.N)]
  
  message("Muestra: ", format(nrow(dt_model), big.mark = ","))
} else {
  dt_model <- copy(dt)
}

dt_model <- dt_model[! is.na(delta_up_wage) & !is.na(delta_down_wage) & 
                       !is.na(structural_distance) & !is.na(domain) & 
                       !is.na(nestedness_tercile)]

message("Observaciones finales: ", format(nrow(dt_model), big.mark = ","))

rm(dt, dt_pos, dt_neg)
gc()
glimpse(dt)

# 3. ESTIMACIÓN DE MODELOS (INTERVAL-CENSORED) ---------------------------------
# Implementamos la especificación ClogLog de Abrevaya & Muris (2020)

# MODELO 1: 2-Way FE (Source + Target)
message("\n>>> Estimando Modelo 1: 2-Way FE (Control Ocupacional)...")
m_2way <- feglm(
  diffusion ~ delta_up_wage * domain * nestedness_tercile + 
    delta_down_wage * domain * nestedness_tercile + 
    structural_distance * domain,
  data = dt_model, family = binomial(link = "cloglog"),
  fixef = c("source", "target"), 
  cluster = c("source", "target")
)

summary(m_2way)
gc()

# MODELO 2: 3-Way FE (Source + Target + Skill) - EL MODELO DEFINITIVO
message("\n>>> Estimando Modelo 2: 3-Way FE (Control Total Relacional)...")
m_3way <- feglm(
  diffusion ~ delta_up_wage * domain * nestedness_tercile + 
    delta_down_wage * domain * nestedness_tercile + 
    structural_distance * domain,
  data = dt_model, family = binomial(link = "cloglog"),
  fixef = c("source", "target", "skill_name"), # Control por inobservables de skill
  cluster = c("source", "target")
)
summary(m_3way)

# 4. NODE-LEVEL BOOTSTRAP (INFERENCIA ROBUSTA) ---------------------------------
# Necesario para corregir la dependencia de red en las díadas
all_nodes <- unique(c(dt_model$source, dt_model$target))

bootstrap_atc <- function(data, nodes, n_boot = 100) {
  boot_coefs <- matrix(NA, nrow = n_boot, ncol = length(coef(m_3way)))
  colnames(boot_coefs) <- names(coef(m_3way))
  
  message("\n>>> Ejecutando Bootstrap (B=", n_boot, ")...")
  for (b in 1:n_boot) {
    boot_nodes <- sample(nodes, length(nodes), replace = TRUE)
    boot_data <- data[source %in% boot_nodes & target %in% boot_nodes]
    
    m_b <- try(feglm(diffusion ~ delta_up_wage * domain * nestedness_tercile + 
                       delta_down_wage * domain * nestedness_tercile + 
                       structural_distance * domain,
                     data = boot_data, family = binomial(link = "cloglog"),
                     fixef = c("source", "target", "skill_name")), silent = TRUE)
    
    if (!inherits(m_b, "try-error")) boot_coefs[b, ] <- coef(m_b)
    if (b %% 10 == 0) cat("Iteración:", b, "\n")
  }
  return(boot_coefs)
}

boot_results <- bootstrap_atc(dt_model, all_nodes, n_boot = 100)

# 5. RESUMEN Y GUARDADO --------------------------------------------------------
message("\n>>> Tabla Final de Resultados (Comparación de Modelos):")
print(etable(m_2way, m_3way, headers = c("2-Way FE", "3-Way FE"), fitstat = ~ pr2 + n))

saveRDS(list(
  m_2way = m_2way, 
  m_3way = m_3way, 
  cs_metrics = skill_stats, 
  boot = boot_results
), "SCD_Final_Analysis_Hosseinioun.rds")

message("\n========================================")
message(">>> ANÁLISIS COMPLETADO CON ÉXITO <<<")
message("========================================")