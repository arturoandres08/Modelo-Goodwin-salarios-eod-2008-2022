# codes/09_unemployment_quarterly_rate.R
source(here("codes", "00_setup.R"))

desemp0 <- load_rds(CFG$paths$unemp_individual_2008_2022)

# ---- checks mínimos de columnas ----
req_cols <- c("sitocup1", "factor_hd", "year_final", "tri_fix")
stopifnot(all(req_cols %in% names(desemp0)))

# ---- cálculo trimestral ----
desemp_tri <- desemp0 %>%
  dplyr::mutate(
    ocupado    = sitocup1 %in% c(1, 8),
    desocupado = sitocup1 %in% c(2, 7)
  ) %>%
  dplyr::group_by(year = year_final, trimestre = tri_fix) %>%
  dplyr::summarise(
    ocupados    = sum(as.numeric(ocupado)    * factor_hd, na.rm = TRUE),
    desocupados = sum(as.numeric(desocupado) * factor_hd, na.rm = TRUE),
    fuerza_trab = ocupados + desocupados,
    tasa_desemp = dplyr::if_else(
      !is.na(fuerza_trab) & fuerza_trab > 0,
      100 * desocupados / fuerza_trab,
      NA_real_
    ),
    n_obs = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    t_index = year + q_num(trimestre) / 10
  ) %>%
  dplyr::arrange(year, t_index)

stopifnot(nrow(desemp_tri) > 0)

# ---- auditorías rápidas ----
aud_zero_force <- desemp_tri %>%
  dplyr::filter(is.na(fuerza_trab) | fuerza_trab <= 0) %>%
  dplyr::select(year, trimestre, t_index, ocupados, desocupados, fuerza_trab, tasa_desemp, n_obs)

aud_out_of_range <- desemp_tri %>%
  dplyr::filter(!is.na(tasa_desemp) & (tasa_desemp < 0 | tasa_desemp > 100)) %>%
  dplyr::select(year, trimestre, t_index, ocupados, desocupados, fuerza_trab, tasa_desemp, n_obs)

save_table_csv(aud_zero_force,   "audit_unemployment_force_zero_or_na", dir = CFG$dirs$out_tables)
save_table_csv(aud_out_of_range, "audit_unemployment_rate_out_of_range", dir = CFG$dirs$out_tables)

# ---- guardar outputs ----
save_rds(desemp_tri, CFG$paths$unemp_quarterly)
save_table_csv(desemp_tri, "unemployment_quarterly_2008_2022", dir = CFG$dirs$out_tables)

message("✔ 09 listo: tasa de desempleo trimestral guardada + auditorías")
