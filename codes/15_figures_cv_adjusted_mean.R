# codes/15_figures_cv_adjusted_mean.R
source(here::here("codes","00_setup.R"))

res_cv_real   <- load_rds(CFG$paths$cv_adjusted_sinwinsor)
res_cv_real_w <- load_rds(CFG$paths$cv_adjusted_winsor)

# ---------------------------
# Helpers
# ---------------------------
pick_theme <- function() {
  if (exists("theme_thesis")) theme_thesis(base_size = 12) else ggplot2::theme_minimal(base_size = 12)
}

# t_index -> year/trimestre -> fecha (mes final del trimestre)
to_fecha_from_tindex <- function(t_index) {
  ti <- as.numeric(t_index)
  year <- as.integer(floor(ti))
  trimestre <- as.integer(round((ti - floor(ti)) * 10))
  fecha <- lubridate::make_date(year = year, month = trimestre * 3L, day = 1L)
  list(year = year, trimestre = trimestre, fecha = fecha)
}

# ---------------------------
# 0) Construir dataset comparación (sin_winsor vs winsor_p99_5)
# ---------------------------
cv_comp <- dplyr::bind_rows(
  res_cv_real   %>% dplyr::mutate(version = "sin_winsor"),
  res_cv_real_w %>% dplyr::mutate(version = "winsor_p99_5")
) %>%
  dplyr::mutate(
    t_index = as.numeric(t_index),
    version = factor(as.character(version), levels = c("sin_winsor", "winsor_p99_5"))
  ) %>%
  dplyr::filter(!is.na(t_index))

aux <- to_fecha_from_tindex(cv_comp$t_index)
cv_comp <- cv_comp %>%
  dplyr::mutate(
    year = aux$year,
    trimestre = aux$trimestre,
    fecha_tri = aux$fecha
  ) %>%
  dplyr::filter(trimestre %in% 1:4, !is.na(fecha_tri))

# ---------------------------
# 1) Figura base del 15: media ajustada (comparación versiones)
# ---------------------------
p_cv_comp <- ggplot2::ggplot(
  cv_comp %>% dplyr::filter(!is.na(media_ajustada_cv)),
  ggplot2::aes(fecha_tri, media_ajustada_cv, color = version)
) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.6, linetype = "dashed", alpha = 0.5) +
  ggplot2::geom_line(linewidth = 1.0) +
  ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  ggplot2::labs(
    title = "Media ajustada por coeficiente de variación (CV)",
    subtitle = "Trimestral, 2008–2022",
    x = "Año",
    y = "Pesos reales (media ajustada)",
    color = "Versión"
  ) +
  pick_theme() +
  ggplot2::theme(legend.position = "bottom")

save_plot_file(p_cv_comp, "cv_adjusted_mean_comparison_real_quarterly_2008_2022",
               dir = CFG$dirs$out_figures, device = "pdf")
save_plot_file(p_cv_comp, "cv_adjusted_mean_comparison_real_quarterly_2008_2022",
               dir = CFG$dirs$out_figures, device = "png")

# ---------------------------
# 2) Figura A: CV en el tiempo (por versión)
# ---------------------------
p_cv_time <- ggplot2::ggplot(
  cv_comp %>% dplyr::filter(!is.na(cv_real)),
  ggplot2::aes(fecha_tri, cv_real, color = version)
) +
  ggplot2::geom_hline(yintercept = 1, linewidth = 0.6, linetype = "dashed", alpha = 0.5) +
  ggplot2::geom_line(linewidth = 1.0) +
  ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  ggplot2::labs(
    title = "Coeficiente de variación (CV) del ingreso real",
    subtitle = "CV = sd/mean, trimestral 2008–2022",
    x = "Año",
    y = "CV (sd/mean)",
    color = "Versión"
  ) +
  pick_theme() +
  ggplot2::theme(legend.position = "bottom")

save_plot_file(p_cv_time, "cv_real_quarterly_comparison_2008_2022",
               dir = CFG$dirs$out_figures, device = "pdf")
save_plot_file(p_cv_time, "cv_real_quarterly_comparison_2008_2022",
               dir = CFG$dirs$out_figures, device = "png")

# ---------------------------
# 3) Figura B: Media real vs media ajustada (facet por versión)
# ---------------------------
mean_long <- cv_comp %>%
  dplyr::select(fecha_tri, version, media_pond_real, media_ajustada_cv) %>%
  tidyr::pivot_longer(
    cols = c(media_pond_real, media_ajustada_cv),
    names_to = "serie",
    values_to = "valor"
  ) %>%
  dplyr::filter(!is.na(valor)) %>%
  dplyr::mutate(
    serie = dplyr::recode(
      serie,
      media_pond_real = "Media real",
      media_ajustada_cv = "Media ajustada (1 - CV)"
    )
  )

p_mean_vs_adj <- ggplot2::ggplot(mean_long, ggplot2::aes(fecha_tri, valor, color = serie)) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.6, linetype = "dashed", alpha = 0.5) +
  ggplot2::geom_line(linewidth = 1.0) +
  ggplot2::facet_wrap(~ version, ncol = 1) +
  ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  ggplot2::labs(
    title = "Media real vs media ajustada por CV",
    subtitle = "Comparación dentro de cada versión, trimestral 2008–2022",
    x = "Año",
    y = "Pesos reales",
    color = "Serie"
  ) +
  pick_theme() +
  ggplot2::theme(legend.position = "bottom")

save_plot_file(p_mean_vs_adj, "mean_real_vs_cv_adjusted_by_version_2008_2022",
               dir = CFG$dirs$out_figures, device = "pdf")
save_plot_file(p_mean_vs_adj, "mean_real_vs_cv_adjusted_by_version_2008_2022",
               dir = CFG$dirs$out_figures, device = "png")

message("✔ 15 listo: figura media ajustada + CV en el tiempo + media real vs ajustada guardadas")
