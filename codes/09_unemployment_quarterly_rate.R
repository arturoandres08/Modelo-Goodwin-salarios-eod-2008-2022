# codes/09_unemployment_quarterly_rate.R
# ===============================================================
# 09) Tasa de desempleo trimestral (2008-2022)
# - Lee: data/interim/eod_unemployment_individual_2008_2022.rds
# - Guarda:
#   * data/processed/unemployment_quarterly_2008_2022.rds
#   * tables/unemployment_quarterly_2008_2022.csv
#   * Auditorías en data/validation + auditoria_variables/validation
# ===============================================================

source(here::here("codes","00_setup.R"))

message("\n== 09) Tasa de desempleo trimestral ==\n")

stopifnot(file.exists(CFG$paths$unemp_individual_2008_2022))

desemp0 <- load_rds(CFG$paths$unemp_individual_2008_2022) %>%
  janitor::clean_names()

# ---------------------------------------------------------------
# Helper: guardar validación "dual" (data/validation + auditoría)
# ---------------------------------------------------------------
save_validation_dual <- function(df, name) {
  fs::dir_create(CFG$dirs$validation)
  readr::write_csv(df, fs::path(CFG$dirs$validation, paste0(name, ".csv")))
  save_audit_validation_csv(df, name)
  invisible(TRUE)
}

# ---------------------------------------------------------------
# Checks mínimos de columnas
# ---------------------------------------------------------------
req_cols <- c("sitocup1", "factor_hd", "year_final", "tri_fix")
stopifnot(all(req_cols %in% names(desemp0)))

# ---------------------------------------------------------------
# Normalizar tipos + trimestre a formato "T1..T4"
# ---------------------------------------------------------------
norm_tri <- function(x) {
  x <- toupper(as.character(x))
  # si viene como 1..4 -> T1..T4
  x <- stringr::str_replace(x, "^([1-4])$", "T\\1")
  # si viene ya como T1..T4 se mantiene
  x[!x %in% paste0("T", 1:4)] <- NA_character_
  x
}

desemp0 <- desemp0 %>%
  dplyr::mutate(
    sitocup1  = suppressWarnings(as.numeric(sitocup1)),
    factor_hd = suppressWarnings(as.numeric(factor_hd)),
    year_final = as.integer(year_final),
    tri_fix   = norm_tri(tri_fix)
  )

# pesos y llaves críticas
stopifnot(all(!is.na(desemp0$year_final)))
stopifnot(all(!is.na(desemp0$tri_fix)))
stopifnot(all(!is.na(desemp0$factor_hd)))
stopifnot(all(desemp0$factor_hd > 0))
stopifnot(all(!is.na(desemp0$sitocup1)))

# ---------------------------------------------------------------
# Definición FINAL (según tu revisión de sitocup1)
# ---------------------------------------------------------------
# Ocupado: 1 En trabajo, 3 En huelga, 5 Trab. sin remuneración, 8 Trabajador sin remuneración
# Desocupado: 6 Busca trabajo por 1ra vez, 7 Cesante
# Inactivo: 2 Inactivo temporal, 4 Inactivo por otras razones, 9 Inactivo
desemp0 <- desemp0 %>%
  dplyr::mutate(
    status = dplyr::case_when(
      sitocup1 %in% c(1, 3, 5, 8) ~ "ocupado",
      sitocup1 %in% c(6, 7)       ~ "desocupado",
      sitocup1 %in% c(2, 4, 9)    ~ "inactivo",
      TRUE                        ~ "otro"
    ),
    ocupado    = status == "ocupado",
    desocupado = status == "desocupado"
  )

# ---------------------------------------------------------------
# Auditorías de clasificación (para detectar códigos raros)
# ---------------------------------------------------------------
aud_status <- desemp0 %>%
  dplyr::count(status, name = "n") %>%
  dplyr::mutate(pct = 100 * n / sum(n)) %>%
  dplyr::arrange(dplyr::desc(n))

aud_codes <- desemp0 %>%
  dplyr::count(sitocup1, name = "n") %>%
  dplyr::arrange(dplyr::desc(n))

save_validation_dual(aud_status, "audit_09_status_distribution")
save_validation_dual(aud_codes,  "audit_09_sitocup1_counts")

# ---------------------------------------------------------------
# Cálculo trimestral (ponderado)
# ---------------------------------------------------------------
desemp_tri <- desemp0 %>%
  dplyr::group_by(year = year_final, trimestre = tri_fix) %>%
  dplyr::summarise(
    ocupados    = sum(as.numeric(ocupado)    * factor_hd, na.rm = TRUE),
    desocupados = sum(as.numeric(desocupado) * factor_hd, na.rm = TRUE),
    fuerza_trab = ocupados + desocupados,
    tasa_desemp = dplyr::if_else(
      fuerza_trab > 0,
      100 * desocupados / fuerza_trab,
      NA_real_
    ),
    n_obs  = dplyr::n(),
    n_otro = sum(status == "otro"),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    t_index = year + q_num(trimestre) / 10
  ) %>%
  dplyr::arrange(year, q_num(trimestre))

stopifnot(nrow(desemp_tri) > 0)

# ---------------------------------------------------------------
# Checks de serie (duplicados + cobertura + rangos)
# ---------------------------------------------------------------
dups <- desemp_tri %>%
  dplyr::count(year, trimestre, name = "n") %>%
  dplyr::filter(n > 1)

stopifnot(nrow(dups) == 0)

exp <- tidyr::expand_grid(
  year = CFG$params$year_min:CFG$params$year_max,
  trimestre = paste0("T", 1:4)
)

missing_yq <- exp %>%
  dplyr::anti_join(desemp_tri %>% dplyr::distinct(year, trimestre),
                   by = c("year", "trimestre"))

aud_out_of_range <- desemp_tri %>%
  dplyr::filter(!is.na(tasa_desemp) & (tasa_desemp < 0 | tasa_desemp > 100))

aud_zero_force <- desemp_tri %>%
  dplyr::filter(is.na(fuerza_trab) | fuerza_trab <= 0)

save_validation_dual(dups,          "audit_09_duplicates_year_quarter")
save_validation_dual(missing_yq,    "audit_09_missing_year_quarter")
save_validation_dual(aud_out_of_range, "audit_09_rate_out_of_range")
save_validation_dual(aud_zero_force,   "audit_09_force_zero_or_na")

# ---------------------------------------------------------------
# Guardar outputs
# ---------------------------------------------------------------
save_rds(desemp_tri, CFG$paths$unemp_quarterly)
save_table_csv(desemp_tri, "unemployment_quarterly_2008_2022", dir = CFG$dirs$out_tables)

message("✔ 09 listo: tasa de desempleo trimestral guardada + auditorías.\n")
