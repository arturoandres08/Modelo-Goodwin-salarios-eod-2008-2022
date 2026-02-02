# codes/15_figures_cv_adjusted_mean.R
source(here("codes", "00_setup.R"))

res_cv_real   <- load_rds(CFG$paths$cv_adjusted_sinwinsor)
res_cv_real_w <- load_rds(CFG$paths$cv_adjusted_winsor)

cv_comp <- dplyr::bind_rows(
  res_cv_real   %>% dplyr::transmute(t_index, media_ajustada_cv, version = "sin_winsor"),
  res_cv_real_w %>% dplyr::transmute(t_index, media_ajustada_cv, version = "winsor_p99_5")
) %>%
  dplyr::mutate(
    t_index  = as.numeric(t_index),
    version  = factor(version, levels = c("sin_winsor", "winsor_p99_5"))
  ) %>%
  dplyr::filter(!is.na(t_index), !is.na(media_ajustada_cv)) %>%
  dplyr::arrange(version, t_index)

p_cv_comp <- ggplot2::ggplot(cv_comp, ggplot2::aes(t_index, media_ajustada_cv, color = version)) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.6, linetype = "dashed", alpha = 0.5) +
  ggplot2::geom_line(linewidth = 1.0) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(
    title = "Media ajustada por coeficiente de variación (CV)",
    subtitle = "Trimestral, 2008–2022",
    x = "Año (trimestre)",
    y = "Pesos (reales)",
    color = "Versión"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "bottom")

save_plot_file(p_cv_comp, "cv_adjusted_mean_comparison_real_quarterly_2008_2022",
               dir = CFG$dirs$out_figures, device = "pdf")
save_plot_file(p_cv_comp, "cv_adjusted_mean_comparison_real_quarterly_2008_2022",
               dir = CFG$dirs$out_figures, device = "png")

message("✔ 15 listo: figuras CV guardadas")
