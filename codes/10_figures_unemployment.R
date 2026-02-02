# codes/10_figures_unemployment.R
# ===============================================================
# 10) Figuras desempleo trimestral (EOD)
# - Lee: CFG$paths$unemp_quarterly
# - Produce: figures/unemployment_rate_quarterly_2008_2022.{png,pdf}
# ===============================================================

source(here::here("codes","00_setup.R"))
message("\n== 10) Figuras desempleo trimestral ==\n")

# --- insumo ---
stopifnot(file.exists(CFG$paths$unemp_quarterly))
desemp_tri <- load_rds(CFG$paths$unemp_quarterly) %>% janitor::clean_names()

# --- checks mínimos ---
req_cols <- c("year", "trimestre", "t_index", "tasa_desemp")
stopifnot(all(req_cols %in% names(desemp_tri)))

desemp_tri <- desemp_tri %>%
  dplyr::mutate(
    t_index = as.numeric(t_index),
    tasa_desemp = as.numeric(tasa_desemp)
  ) %>%
  dplyr::arrange(t_index)

# sanity checks (no deberían fallar si 09 está OK)
stopifnot(all(is.finite(desemp_tri$t_index)))
stopifnot(all(is.na(desemp_tri$tasa_desemp) | (desemp_tri$tasa_desemp >= 0 & desemp_tri$tasa_desemp <= 100)))

# --- theme formal “tesis” (consistente) ---
theme_tesis <- function() {
  ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.minor = ggplot2::element_blank()
    )
}

# --- plot ---
p_desemp <- ggplot2::ggplot(desemp_tri, ggplot2::aes(x = t_index, y = tasa_desemp)) +
  ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  ggplot2::labs(
    title = "Tasa de desempleo trimestral (EOD, 2008–2022)",
    x = "Año-trimestre (t_index)",
    y = "Porcentaje"
  ) +
  theme_tesis()

# --- guardar outputs ---
fs::dir_create(CFG$dirs$out_figures)

save_plot_file(
  p_desemp,
  "unemployment_rate_quarterly_2008_2022",
  dir = CFG$dirs$out_figures,
  device = "png",
  width = 10, height = 5, dpi = 300
)

save_plot_file(
  p_desemp,
  "unemployment_rate_quarterly_2008_2022",
  dir = CFG$dirs$out_figures,
  device = "pdf",
  width = 10, height = 5
)

# --- verificación de que quedaron guardados ---
png_path <- fs::path(CFG$dirs$out_figures, "unemployment_rate_quarterly_2008_2022.png")
pdf_path <- fs::path(CFG$dirs$out_figures, "unemployment_rate_quarterly_2008_2022.pdf")
stopifnot(file.exists(png_path), file.exists(pdf_path))

message("✅ 10 listo: figura guardada en: ", CFG$dirs$out_figures)
