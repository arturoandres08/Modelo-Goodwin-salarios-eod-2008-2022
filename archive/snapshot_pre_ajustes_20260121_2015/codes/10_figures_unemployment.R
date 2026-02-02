# codes/10_figures_unemployment.R
source(here("codes", "00_setup.R"))

desemp_tri <- load_rds(CFG$paths$unemp_quarterly)

# ---- checks mínimos ----
req_cols <- c("t_index", "tasa_desemp")
stopifnot(all(req_cols %in% names(desemp_tri)))

desemp_tri <- desemp_tri %>%
  dplyr::arrange(t_index)

p_desemp <- desemp_tri %>%
  ggplot2::ggplot(ggplot2::aes(t_index, tasa_desemp)) +
  ggplot2::geom_line(linewidth = 1.1, na.rm = TRUE) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  ggplot2::labs(
    title = "Tasa de desempleo trimestral (EOD, 2008–2022)",
    x = "Año (trimestre)",
    y = "Porcentaje"
  ) +
  ggplot2::theme_minimal(base_size = 13)

# (Opcional) Si quieres ver bien puntos por trimestre:
# p_desemp <- p_desemp + ggplot2::geom_point(size = 1.1, na.rm = TRUE)

save_plot_file(
  p_desemp,
  "unemployment_rate_quarterly_2008_2022",
  dir = CFG$dirs$out_figures,
  device = "png"
)
save_plot_file(
  p_desemp,
  "unemployment_rate_quarterly_2008_2022",
  dir = CFG$dirs$out_figures,
  device = "pdf"
)

message("✔ 10 listo: figura desempleo guardada en outputs/paper/figures")
