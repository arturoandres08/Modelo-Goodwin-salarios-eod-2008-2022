# ============================================================
# 20e_svar_nocovid_lag_selection.R
# Selección de rezagos en submuestra noCOVID (2008Q1-2019Q4)
# Basado en la lógica del repo:
# - 17 genera processed/svar_core_inputs_2008Q1_2022Q4.rds
# - 19e lo carga, reconstruye q desde year/trimestre y luego llama build_core()
# ============================================================

message("== 20e) Selección de rezagos en noCOVID (AIC/BIC/HQ/FPE) ==")

source("codes/00_setup.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(vars)
  library(tibble)
  library(zoo)
  library(fs)
})

# ------------------------------------------------------------
# Parámetros
# ------------------------------------------------------------
BASELINE_TAG <- "baseline_mean_winsor_nocovid"
WAGE_COL     <- "w_real_mean_winsor"
P_MAX        <- 8L
OUT_DIR      <- file.path("tables", "svar_core", "covid_nocovid_lags")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Cargar funciones e input núcleo
# ------------------------------------------------------------
source("codes/18_svar_core_functions.R")

# Mismo input que usa 19e
in_path <- fs::path(CFG$dirs$processed, "svar_core_inputs_2008Q1_2022Q4.rds")
stopifnot(file.exists(in_path))

# Igual que 19e: reconstruir q antes de build_core()
df <- load_rds(in_path) %>%
  dplyr::mutate(
    q = zoo::as.yearqtr(paste0(year, " Q", trimestre), format = "%Y Q%q")
  ) %>%
  dplyr::arrange(q)

# Build core igual que 19e
X <- build_core(df, wage_col = WAGE_COL, a_col = "a_prod")

# Submuestra noCOVID: hasta 2019Q4
cutoff_q <- zoo::as.yearqtr("2019 Q4", format = "%Y Q%q")
X_nocovid <- X %>%
  dplyr::filter(q <= cutoff_q)

if (nrow(X_nocovid) < 25) {
  stop("La submuestra noCOVID quedó muy pequeña; revisa el filtro.")
}

# Matriz endógena
Y <- X_nocovid %>%
  dplyr::select(ga, gw, v) %>%
  as.matrix()

# ------------------------------------------------------------
# Selección formal con VARselect
# ------------------------------------------------------------
sel <- vars::VARselect(Y, lag.max = P_MAX, type = "const")

# sel$criteria: criterios en filas y rezagos en columnas
crit_tbl <- as.data.frame(sel$criteria)
crit_tbl$criterion <- rownames(crit_tbl)
rownames(crit_tbl) <- NULL

crit_long <- crit_tbl %>%
  tidyr::pivot_longer(
    cols = -criterion,
    names_to = "p_label",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    p = readr::parse_number(p_label),
    sample = "nocovid (2008Q1-2019Q4)",
    tag = BASELINE_TAG,
    wage_col = WAGE_COL
  ) %>%
  dplyr::select(sample, tag, wage_col, criterion, p, value) %>%
  dplyr::arrange(criterion, p)

best_tbl <- crit_long %>%
  dplyr::group_by(sample, tag, wage_col, criterion) %>%
  dplyr::slice_min(order_by = value, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::rename(best_p = p, best_value = value)

# Resumen con la selección estándar reportada por vars::VARselect
selection_tbl <- tibble::tibble(
  criterion = names(sel$selection),
  best_p = as.integer(sel$selection)
) %>%
  dplyr::mutate(
    sample = "nocovid (2008Q1-2019Q4)",
    tag = BASELINE_TAG,
    wage_col = WAGE_COL
  ) %>%
  dplyr::select(sample, tag, wage_col, criterion, best_p)

# ------------------------------------------------------------
# Chequeo simple de tamaño muestral efectivo
# ------------------------------------------------------------
k <- ncol(Y)
Tn <- nrow(Y)

feasibility_tbl <- tibble::tibble(
  p = 1:P_MAX,
  T_total = Tn,
  T_eff = Tn - (1:P_MAX),
  params_per_eq_const = k * (1:P_MAX) + 1,
  obs_per_param = (Tn - (1:P_MAX)) / (k * (1:P_MAX) + 1)
) %>%
  dplyr::mutate(
    sample = "nocovid (2008Q1-2019Q4)",
    tag = BASELINE_TAG,
    wage_col = WAGE_COL
  ) %>%
  dplyr::select(sample, tag, wage_col, dplyr::everything())

# ------------------------------------------------------------
# Consola
# ------------------------------------------------------------
message("\n--- Selección reportada por VARselect en noCOVID ---")
print(selection_tbl)

message("\n--- Mejor rezago por criterio (tabla larga) ---")
print(best_tbl)

message("\n--- Observaciones por parámetro (aprox.) ---")
print(feasibility_tbl)

# ------------------------------------------------------------
# Guardar outputs
# ------------------------------------------------------------
readr::write_csv(
  crit_long,
  file.path(OUT_DIR, "lag_selection_nocovid_criteria_long.csv")
)

readr::write_csv(
  best_tbl,
  file.path(OUT_DIR, "lag_selection_nocovid_best_by_criterion.csv")
)

readr::write_csv(
  selection_tbl,
  file.path(OUT_DIR, "lag_selection_nocovid_selection_table.csv")
)

readr::write_csv(
  feasibility_tbl,
  file.path(OUT_DIR, "lag_selection_nocovid_feasibility.csv")
)

message("\nArchivos guardados en: ", OUT_DIR)
message("Listo.")
