# codes/06_real_income_and_quarterly.R
source(here("codes", "00_setup.R"))

# ------------------------------------------------------------
# 1) Cargar inputs
# ------------------------------------------------------------
dat_ok <- load_rds(CFG$paths$eod_clean_2008_2022)
dat_w  <- load_rds(CFG$paths$eod_winsor_2008_2022)
ipc    <- load_rds(CFG$paths$ipc_monthly)

# Validaciones mínimas de columnas clave
req_dat <- c("year_final", "month", "tri_fix", "factor_hd", "ingsueld")
stopifnot(all(req_dat %in% names(dat_ok)))
stopifnot(all(c("ingsueld_w") %in% names(dat_w)))   # dat_w debe traer ingsueld_w

req_ipc <- c("year", "month", "IPC")
stopifnot(all(req_ipc %in% names(ipc)))

# ------------------------------------------------------------
# 2) IPC key + chequeos (evitar join que infle filas)
# ------------------------------------------------------------
ipc_key <- ipc %>%
  dplyr::select(year, month, IPC) %>%
  dplyr::mutate(
    year  = as.integer(year),
    month = as.integer(month),
    IPC   = as.numeric(IPC)
  )

# Duplicados IPC (por seguridad): si existen, guardamos y colapsamos
dup_ipc <- ipc_key %>%
  dplyr::count(year, month) %>%
  dplyr::filter(n > 1)

if (nrow(dup_ipc) > 0) {
  save_table_csv(dup_ipc, "validation_ipc_duplicates_before_join", dir = CFG$dirs$validation)
  
  ipc_key <- ipc_key %>%
    dplyr::group_by(year, month) %>%
    dplyr::summarise(IPC = mean(IPC, na.rm = TRUE), .groups = "drop")
}

# IPC inválido (NA o <=0) -> detener (deflactar así rompe todo)
bad_ipc <- ipc_key %>% dplyr::filter(is.na(IPC) | IPC <= 0)
if (nrow(bad_ipc) > 0) {
  save_table_csv(bad_ipc, "validation_ipc_bad_values", dir = CFG$dirs$validation)
  stop("IPC inválido (NA o <= 0). Revisa data/validation/validation_ipc_bad_values.csv")
}

# ------------------------------------------------------------
# 3) Deflactar (sin winsor / winsor) + auditoría de join
# ------------------------------------------------------------
prep_and_deflate <- function(df, income_var) {
  # df: dat_ok o dat_w
  # income_var: "ingsueld" o "ingsueld_w"
  df2 <- df %>%
    dplyr::mutate(
      year_final = as.integer(year_final),
      month      = as.integer(month),
      factor_hd  = as.numeric(factor_hd),
      tri_fix    = toupper(as.character(tri_fix)),
      income     = as.numeric(.data[[income_var]])
    ) %>%
    dplyr::left_join(ipc_key, by = c("year_final" = "year", "month" = "month"))
  
  # Auditoría: observaciones sin IPC
  miss <- df2 %>%
    dplyr::filter(is.na(IPC)) %>%
    dplyr::count(year_final, month, name = "n_obs") %>%
    dplyr::arrange(year_final, month)
  
  if (nrow(miss) > 0) {
    # guardamos y detenemos: es mejor que deflactar con NA
    save_table_csv(miss, paste0("validation_missing_ipc_after_join_", income_var), dir = CFG$dirs$validation)
    stop(
      "Faltan valores de IPC para algunos (year_final, month). ",
      "Revisa data/validation/validation_missing_ipc_after_join_", income_var, ".csv"
    )
  }
  
  # Deflactar
  df2 %>%
    dplyr::mutate(ing_real = income * (100 / IPC))
}

dat_real   <- prep_and_deflate(dat_ok, "ingsueld")
dat_real_w <- prep_and_deflate(dat_w,  "ingsueld_w")

# ------------------------------------------------------------
# 4) Resúmenes trimestrales REALES + dispersión
# ------------------------------------------------------------
res_real <- dat_real %>%
  dplyr::group_by(year = year_final, trimestre = tri_fix) %>%
  dplyr::summarise(
    n_obs              = dplyr::n(),
    masa_salarial_real = sum(ing_real * factor_hd, na.rm = TRUE),
    suma_pesos         = sum(factor_hd, na.rm = TRUE),
    media_pond_real    = masa_salarial_real / suma_pesos,
    media_simple_real  = mean(ing_real, na.rm = TRUE),
    var_real           = stats::var(ing_real, na.rm = TRUE),
    sd_real            = stats::sd(ing_real, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(t_index = year + q_num(trimestre) / 10, version = "sin_winsor") %>%
  dplyr::arrange(year, t_index)

res_real_w <- dat_real_w %>%
  dplyr::group_by(year = year_final, trimestre = tri_fix) %>%
  dplyr::summarise(
    n_obs              = dplyr::n(),
    masa_salarial_real = sum(ing_real * factor_hd, na.rm = TRUE),
    suma_pesos         = sum(factor_hd, na.rm = TRUE),
    media_pond_real    = masa_salarial_real / suma_pesos,
    media_simple_real  = mean(ing_real, na.rm = TRUE),
    var_real           = stats::var(ing_real, na.rm = TRUE),
    sd_real            = stats::sd(ing_real, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(t_index = year + q_num(trimestre) / 10, version = "winsor_p99_5") %>%
  dplyr::arrange(year, t_index)

res_real_all <- dplyr::bind_rows(res_real, res_real_w)

# ------------------------------------------------------------
# 5) Guardar outputs
# ------------------------------------------------------------
save_rds(res_real_all, CFG$paths$real_quarterly)
save_table_csv(res_real_all, "real_quarterly_2008_2022", dir = CFG$dirs$out_tables)

message("✔ 06 listo: agregados reales trimestrales guardados + validaciones OK")
