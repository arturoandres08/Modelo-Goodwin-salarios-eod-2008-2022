# codes/06_real_income_and_quarterly.R
# ===============================================================
# 06) Salarios reales trimestrales (base IPC dic-2018 = 100)
# - Insumos:
#   * data/processed/nominal_quarterly_2008_2022.rds   (script 04)
#   * data/processed/ipc_monthly_base2018_100.rds      (script 05)
# - Salidas:
#   * data/processed/real_quarterly_2008_2022.rds
#   * tables/real_quarterly_2008_2022.csv
#   * auditorías: data/validation + auditoria_variables/validation
# ===============================================================

# ---- 0) Setup global ----
source(here::here("codes", "00_setup.R"))

# ---- Helper: guardar auditorías en ambos lugares ----
save_validation_csv <- function(df, filename) {
  p1 <- fs::path(CFG$dirs$validation, filename)
  fs::dir_create(fs::path_dir(p1))
  readr::write_csv(df, p1)
  
  p2 <- fs::path(CFG$dirs$aud_val, filename)
  fs::dir_create(fs::path_dir(p2))
  readr::write_csv(df, p2)
  
  invisible(list(data_validation = p1, audit_validation = p2))
}

# ---- 1) Cargar insumos ----
stopifnot(file.exists(CFG$paths$nominal_quarterly))
stopifnot(file.exists(CFG$paths$ipc_monthly))

nom <- load_rds(CFG$paths$nominal_quarterly)
ipc <- load_rds(CFG$paths$ipc_monthly)

stopifnot(is.data.frame(nom), is.data.frame(ipc))

# ---- 2) Normalizar llaves en nominal trimestral ----
nom <- nom %>%
  dplyr::mutate(
    year = as.integer(year),
    trimestre_raw = trimestre,
    trimestre = dplyr::case_when(
      is.numeric(trimestre) ~ as.integer(trimestre),
      TRUE ~ suppressWarnings(as.integer(stringr::str_extract(as.character(trimestre), "[1-4]")))
    )
  )

# Si no existe version, créala (para no romper joins/validaciones)
if (!"version" %in% names(nom)) {
  nom <- nom %>% dplyr::mutate(version = "single")
}

# checks básicos de nominal
bad_nom_tri <- nom %>%
  dplyr::filter(is.na(year) | is.na(trimestre) | !(trimestre %in% 1:4)) %>%
  dplyr::distinct(year, trimestre_raw, trimestre, version)

if (nrow(bad_nom_tri) > 0) {
  save_validation_csv(bad_nom_tri, "audit_06_bad_nominal_trimestre.csv")
  stop("06: nominal_quarterly trae trimestres inválidos. Revisa audit_06_bad_nominal_trimestre.csv")
}

# ---- 3) ESTANDARIZAR IPC (AQUÍ VA TU ARREGLO) ----
# Queremos terminar con: periodo (Date), year (int), month (int), ipc (num)
if (!"periodo" %in% names(ipc)) {
  if ("Periodo" %in% names(ipc)) ipc <- dplyr::rename(ipc, periodo = Periodo)
}
stopifnot("periodo" %in% names(ipc))

ipc <- ipc %>% dplyr::mutate(periodo = as.Date(periodo))

if (!"year" %in% names(ipc))  ipc <- ipc %>% dplyr::mutate(year  = lubridate::year(periodo))
if (!"month" %in% names(ipc)) ipc <- ipc %>% dplyr::mutate(month = lubridate::month(periodo))

if (!"ipc" %in% names(ipc)) {
  if ("ipc_base2018_100" %in% names(ipc)) {
    ipc <- dplyr::rename(ipc, ipc = ipc_base2018_100)
  } else if ("IPC" %in% names(ipc)) {
    ipc <- dplyr::rename(ipc, ipc = IPC)
  } else if ("ipc_index" %in% names(ipc)) {
    ipc <- dplyr::rename(ipc, ipc = ipc_index)
  }
}
stopifnot("ipc" %in% names(ipc))

ipc <- ipc %>%
  dplyr::mutate(
    year = as.integer(year),
    month = as.integer(month),
    ipc = as.numeric(ipc)
  ) %>%
  dplyr::select(periodo, year, month, ipc) %>%
  dplyr::arrange(year, month)

# ---- 4) Check base dic-2018 (debería ser 100) ----
ipc_base <- ipc %>%
  dplyr::filter(year == CFG$params$base_year, month == CFG$params$base_month_ipc) %>%
  dplyr::pull(ipc)

if (length(ipc_base) != 1 || !is.finite(ipc_base)) {
  save_validation_csv(
    ipc %>% dplyr::filter(year == CFG$params$base_year, month == CFG$params$base_month_ipc),
    "audit_06_ipc_base_not_found.csv"
  )
  stop("06: No pude encontrar IPC base (dic-2018) o no es finito. Revisa audit_06_ipc_base_not_found.csv")
}

# tolerancia por redondeo
if (abs(ipc_base - 100) > 1e-6) {
  warning(paste0("06: IPC base dic-", CFG$params$base_year,
                 " no es exactamente 100 (=", round(ipc_base, 6), ")."))
}

# ---- 5) IPC trimestral (promedio de meses del trimestre) ----
ipc_q <- ipc %>%
  dplyr::mutate(trimestre = as.integer(ceiling(month / 3))) %>%
  dplyr::group_by(year, trimestre) %>%
  dplyr::summarise(
    ipc_q = mean(ipc, na.rm = TRUE),
    n_months = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::arrange(year, trimestre)

# auditoría: trimestres con menos de 3 meses
audit_ipc_q <- ipc_q %>% dplyr::filter(n_months < 3)
if (nrow(audit_ipc_q) > 0) {
  save_validation_csv(audit_ipc_q, "audit_06_ipc_quarters_with_missing_months.csv")
}

# auditoría: faltantes en el rango objetivo (2008–2022)
exp_grid <- tidyr::expand_grid(
  year = CFG$params$year_min:CFG$params$year_max,
  trimestre = 1:4
)

missing_ipc_q <- exp_grid %>%
  dplyr::anti_join(ipc_q %>% dplyr::select(year, trimestre), by = c("year","trimestre"))

if (nrow(missing_ipc_q) > 0) {
  save_validation_csv(missing_ipc_q, "audit_06_missing_ipc_quarters.csv")
  stop("06: faltan trimestres de IPC en el rango 2008–2022. Revisa audit_06_missing_ipc_quarters.csv")
}

# ---- 6) Unir nominal + IPC trimestral ----
real_df <- nom %>%
  dplyr::left_join(ipc_q %>% dplyr::select(year, trimestre, ipc_q), by = c("year","trimestre")) %>%
  dplyr::mutate(deflator = ipc_q / 100)

# check join
bad_join <- real_df %>% dplyr::filter(is.na(ipc_q) | !is.finite(deflator)) %>%
  dplyr::distinct(year, trimestre, version)

if (nrow(bad_join) > 0) {
  save_validation_csv(bad_join, "audit_06_nominal_without_ipc_match.csv")
  stop("06: hay trimestres nominales sin match de IPC. Revisa audit_06_nominal_without_ipc_match.csv")
}

# ---- 7) Deflactar columnas monetarias (crea *_real) ----
# excluimos llaves / columnas no monetarias típicas
key_cols <- c("year","trimestre","version","t_index","n","n_obs","n_raw","n_months",
              "trimestre_raw","ipc_q","deflator")

num_cols <- names(real_df)[vapply(real_df, is.numeric, logical(1))]
cand_cols <- setdiff(num_cols, intersect(num_cols, key_cols))

# regla: columnas que parezcan monetarias (wmean/media/p50/p99/ingreso/sueldo/salario/etc)
money_cols <- cand_cols[
  stringr::str_detect(
    cand_cols,
    stringr::regex("(wmean|media|mean|avg|p50|p99|ingsueld|ingreso|sueldo|salario|nom)", ignore_case = TRUE)
  )
]

# si no detecta ninguna, usa todas las numéricas no-key (pero avisando)
if (length(money_cols) == 0) {
  warning("06: No detecté columnas monetarias por nombre; deflactaré todas las numéricas no-key.")
  money_cols <- cand_cols
}

for (cc in money_cols) {
  real_df[[paste0(cc, "_real")]] <- real_df[[cc]] / real_df$deflator
}

# orden final
real_df <- real_df %>%
  dplyr::arrange(version, year, trimestre)

# ---- 8) Guardar outputs ----
out_rds <- fs::path(CFG$dirs$processed, "real_quarterly_2008_2022.rds")
save_rds(real_df, out_rds)

# CSV para paper (agregado, debería ser pequeño)
save_table_csv(real_df, "real_quarterly_2008_2022", dir = CFG$dirs$out_tables)

# Auditoría resumen (qué columnas se deflactaron)
audit_cols <- tibble::tibble(
  deflated_columns = money_cols,
  created_columns_real = paste0(money_cols, "_real")
)
save_validation_csv(audit_cols, "audit_06_deflated_columns.csv")

message("✅ 06 listo: salarios reales trimestrales guardados en processed + tables + auditorías en validation.")
