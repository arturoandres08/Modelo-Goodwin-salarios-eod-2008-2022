# codes/05_ipc_build_index.R
# ===============================================================
# 05) IPC: construir índice mensual base dic-2018 = 100 (reproducible)
# - Lee Excel BCCh
# - Construye índice acumulado
# - Reescala para que (base_year, base_month_ipc) = 100
# - Guarda RDS en processed + auditorías en validation (+ copia humana)
# ===============================================================

source(here::here("codes", "00_setup.R"))

message("== 05) IPC: leyendo archivo ==")

# ---- 1) Leer excel ----
stopifnot(file.exists(CFG$files$ipc_excel))

ipc_raw <- readxl::read_excel(CFG$files$ipc_excel) %>%
  janitor::clean_names()

# Esperamos que traiga: periodo (fecha) y una columna con la variación mensual en %
# En tu diagnóstico: names(ipc_raw) = c("periodo", "x1_ipc_general_empalme_bc_ch")
stopifnot("periodo" %in% names(ipc_raw))

# Detectar columna de variación mensual (la 2da por defecto, o por patrón)
cand_var <- names(ipc_raw)[names(ipc_raw) != "periodo"]
var_col <- cand_var[1]
# si existiera algo mejor por patrón, lo usa
pat <- stringr::str_which(cand_var, stringr::regex("ipc|empalme|vari", ignore_case = TRUE))
if (length(pat) > 0) var_col <- cand_var[pat[1]]

ipc <- ipc_raw %>%
  dplyr::transmute(
    periodo = as.Date(periodo),
    var_mensual = suppressWarnings(as.numeric(.data[[var_col]]))
  ) %>%
  dplyr::filter(!is.na(periodo)) %>%
  dplyr::mutate(
    year  = lubridate::year(periodo),
    month = lubridate::month(periodo)
  ) %>%
  dplyr::arrange(year, month)

# ---- 2) Construir índice acumulado y reescalar base ----
ipc <- ipc %>%
  dplyr::mutate(
    factor = 1 + var_mensual / 100,
    indice_bruto = 100 * cumprod(factor)
  )

base_row <- ipc %>%
  dplyr::filter(year == CFG$params$base_year, month == CFG$params$base_month_ipc)

# Debe existir exactamente 1 fila base y ser finita
stopifnot(nrow(base_row) == 1)
ipc_base <- base_row$indice_bruto[1]
stopifnot(is.finite(ipc_base))

ipc <- ipc %>%
  dplyr::mutate(
    ipc_base2018_100 = indice_bruto / ipc_base * 100,
    # aliases para compatibilidad
    ipc = ipc_base2018_100,
    IPC = ipc_base2018_100
  )

message("✓ Base usada: ", CFG$params$base_year, "-", sprintf("%02d", CFG$params$base_month_ipc),
        " | indice_bruto_base=", round(ipc_base, 6))

# ---- 3) Auditorías 05 ----
# 3.1 Cobertura: meses esperados vs observados
ipc_months_by_year <- ipc %>%
  dplyr::count(year, name = "n_months") %>%
  dplyr::arrange(year)

audit_missing <- ipc %>%
  dplyr::count(year, month, name = "n") %>%
  dplyr::right_join(
    tidyr::expand_grid(
      year = min(ipc$year, na.rm = TRUE):max(ipc$year, na.rm = TRUE),
      month = 1:12
    ),
    by = c("year", "month")
  ) %>%
  dplyr::mutate(missing = is.na(n)) %>%
  dplyr::filter(missing) %>%
  dplyr::select(year, month)

# Guardar auditorías: data/validation + copia humana en auditoria_variables/validation
readr::write_csv(ipc_months_by_year, fs::path(CFG$dirs$validation, "audit_05_ipc_months_by_year.csv"))
readr::write_csv(audit_missing,      fs::path(CFG$dirs$validation, "audit_05_ipc_missing_months.csv"))

save_audit_validation_csv(ipc_months_by_year, "audit_05_ipc_months_by_year")
save_audit_validation_csv(audit_missing,      "audit_05_ipc_missing_months")

# ---- 4) Guardar output ----
save_rds(ipc, CFG$paths$ipc_monthly)

# (opcional) CSV “paper”
save_table_csv(ipc, "ipc_monthly_base2018_100")

message("✓ 05 listo: IPC mensual guardado en processed (RDS).")
message("✓ Auditorías 05 guardadas en data/validation y auditoria_variables/validation.")
