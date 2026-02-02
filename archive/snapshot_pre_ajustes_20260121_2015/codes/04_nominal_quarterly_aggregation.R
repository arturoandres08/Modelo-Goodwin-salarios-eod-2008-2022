# codes/04_nominal_quarterly_aggregation.R
source(here("codes", "00_setup.R"))

ing0 <- load_rds(CFG$paths$eod_calendar_normalized)

# ------------------------------------------------------------
# 0) Helper: cuantil ponderado (opcional)
# ------------------------------------------------------------
wtd_quantile <- function(x, w, probs = 0.995) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) == 0) return(NA_real_)
  ord <- order(x)
  x <- x[ord]; w <- w[ord]
  cw <- cumsum(w) / sum(w)
  # primer x donde cw >= p
  x[which(cw >= probs)[1]]
}

# Por defecto NO cambiamos tu lógica (winsor no ponderado),
# pero dejamos la puerta abierta por config:
if (is.null(CFG$params$winsor_weighted)) {
  CFG$params$winsor_weighted <- FALSE
}

# ------------------------------------------------------------
# 1) Filtro ventana y limpieza mínima
# ------------------------------------------------------------
dat_ok <- ing0 %>%
  dplyr::filter(
    !is.na(ingsueld), ingsueld >= 0,
    !is.na(factor_hd), factor_hd > 0,
    !is.na(year_final), !is.na(tri_fix),
    dplyr::between(year_final, CFG$params$year_min, CFG$params$year_max)
  )

stopifnot(nrow(dat_ok) > 0)

# ------------------------------------------------------------
# 2) Winsor p99.5 por año (tu regla)
#    - Por defecto: cuantil NO ponderado
#    - Opcional: cuantil ponderado (si CFG$params$winsor_weighted = TRUE)
# ------------------------------------------------------------
thr <- dat_ok %>%
  dplyr::group_by(year_final) %>%
  dplyr::summarise(
    n_year = dplyr::n(),
    suma_pesos_year = sum(factor_hd, na.rm = TRUE),
    p995 = dplyr::if_else(
      CFG$params$winsor_weighted,
      wtd_quantile(ingsueld, factor_hd, probs = CFG$params$winsor_p),
      stats::quantile(ingsueld, CFG$params$winsor_p, na.rm = TRUE, type = 7)
    ),
    .groups = "drop"
  )

# Guardar validación del threshold (sirve mucho para auditoría)
save_table_csv(thr, "validation_winsor_threshold_by_year", dir = CFG$dirs$validation)

dat_w <- dat_ok %>%
  dplyr::left_join(thr %>% dplyr::select(year_final, p995), by = "year_final") %>%
  dplyr::mutate(
    ingsueld_w = pmin(ingsueld, p995)
  )

# ------------------------------------------------------------
# 3) Resúmenes trimestrales NOMINALES
# ------------------------------------------------------------
res_nom_w <- dat_w %>%
  dplyr::group_by(year = year_final, trimestre = tri_fix) %>%
  dplyr::summarise(
    n_obs         = dplyr::n(),
    masa_salarial = sum(ingsueld_w * factor_hd, na.rm = TRUE),
    suma_pesos    = sum(factor_hd, na.rm = TRUE),
    media_pond    = masa_salarial / suma_pesos,
    media_simple  = mean(ingsueld_w, na.rm = TRUE),
    p99_5         = dplyr::first(p995),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    t_index  = year + q_num(trimestre) / 10,
    version  = "winsor_p99_5"
  ) %>%
  dplyr::arrange(year, t_index)

res_nom_now <- dat_ok %>%
  dplyr::group_by(year = year_final, trimestre = tri_fix) %>%
  dplyr::summarise(
    n_obs         = dplyr::n(),
    masa_salarial = sum(ingsueld * factor_hd, na.rm = TRUE),
    suma_pesos    = sum(factor_hd, na.rm = TRUE),
    media_pond    = masa_salarial / suma_pesos,
    media_simple  = mean(ingsueld, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    t_index  = year + q_num(trimestre) / 10,
    version  = "sin_winsor"
  ) %>%
  dplyr::arrange(year, t_index)

res_nom <- dplyr::bind_rows(res_nom_w, res_nom_now) %>%
  dplyr::arrange(version, year, t_index)

# ------------------------------------------------------------
# 4) Guardar outputs (interim + processed + paper)
# ------------------------------------------------------------
save_rds(dat_ok,  CFG$paths$eod_clean_2008_2022)
save_rds(dat_w,   CFG$paths$eod_winsor_2008_2022)
save_rds(res_nom, CFG$paths$nominal_quarterly)

save_table_csv(res_nom, "nominal_quarterly_2008_2022", dir = CFG$dirs$out_tables)

message("✔ 04 listo: nominal trimestral (processed) + bases individuales (interim) + validación winsor (data/validation)")
