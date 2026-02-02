# codes/14_cv_adjusted_mean_build.R
source(here("codes", "00_setup.R"))

rea <- load_rds(CFG$paths$real_quarterly)

req_cols <- c("t_index","version","media_pond_real","sd_real")
stopifnot(all(req_cols %in% names(rea)))

# ---------------------------------------------------------------
# 1) Normalización mínima de tipos (robustez)
# ---------------------------------------------------------------
rea_cv <- rea %>%
  dplyr::mutate(
    t_index         = as.numeric(t_index),
    version         = as.character(version),
    media_pond_real = suppressWarnings(as.numeric(media_pond_real)),
    sd_real         = suppressWarnings(as.numeric(sd_real))
  )

# ---------------------------------------------------------------
# 2) CV y media ajustada (SIN truncar)
# ---------------------------------------------------------------
rea_cv <- rea_cv %>%
  dplyr::mutate(
    cv_real = dplyr::if_else(
      !is.na(media_pond_real) & media_pond_real > 0 & !is.na(sd_real) & sd_real >= 0,
      sd_real / media_pond_real,
      NA_real_
    ),
    media_ajustada_cv = dplyr::if_else(
      !is.na(cv_real),
      media_pond_real * (1 - cv_real),  # <- SIN pmax(), se permite negativo
      NA_real_
    )
  )

# Checks suaves: avisos si algo se ve raro
if (any(rea_cv$cv_real < 0, na.rm = TRUE)) warning("Hay cv_real < 0. Revisa sd_real/media_pond_real.")
if (any(rea_cv$cv_real > 2, na.rm = TRUE)) warning("Hay cv_real > 2 en algunos trimestres (dispersión muy alta).")

# ---------------------------------------------------------------
# 3) Separar por versión + guardar
# ---------------------------------------------------------------
res_cv_sin <- rea_cv %>% dplyr::filter(version == "sin_winsor")
res_cv_win <- rea_cv %>% dplyr::filter(version == "winsor_p99_5")

stopifnot(nrow(res_cv_sin) > 0, nrow(res_cv_win) > 0)

# RDS (processed)
save_rds(res_cv_sin, CFG$paths$cv_adjusted_sinwinsor)
save_rds(res_cv_win, CFG$paths$cv_adjusted_winsor)

# CSV (paper tables)
save_table_csv(res_cv_sin, "cv_adjusted_mean_real_quarterly_sinwinsor_2008_2022", dir = CFG$dirs$out_tables)
save_table_csv(res_cv_win, "cv_adjusted_mean_real_quarterly_winsor_2008_2022",   dir = CFG$dirs$out_tables)

# (opcional pero recomendado) auditoría: ambas versiones juntas
save_table_csv(rea_cv, "cv_adjusted_mean_real_quarterly_all_versions_2008_2022", dir = CFG$dirs$out_tables)

message("✔ 14 listo: CV ajustado guardado (processed) + tabla all_versions")
