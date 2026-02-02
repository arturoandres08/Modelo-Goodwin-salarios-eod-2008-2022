# codes/07_figures_wages_real.R
source(here("codes", "00_setup.R"))

nom <- load_rds(CFG$paths$nominal_quarterly)
rea <- load_rds(CFG$paths$real_quarterly)

# ------------------------------------------------------------
# 0) Validaciones mínimas
# ------------------------------------------------------------
req_nom <- c("t_index", "media_pond", "version")
req_rea <- c("t_index", "media_pond_real", "version")

stopifnot(all(req_nom %in% names(nom)))
stopifnot(all(req_rea %in% names(rea)))

# Orden consistente de versiones (para leyendas/colores)
ver_levels <- c("sin_winsor", "winsor_p99_5")
nom <- nom %>% dplyr::mutate(version = factor(version, levels = ver_levels))
rea <- rea %>% dplyr::mutate(version = factor(version, levels = ver_levels))

# ------------------------------------------------------------
# 1) Figura: nominal media ponderada (con/sin winsor)
# ------------------------------------------------------------
p_nom <- nom %>%
  dplyr::filter(!is.na(t_index), !is.na(media_pond), !is.na(version)) %>%
  ggplot2::ggplot(ggplot2::aes(t_index, media_pond, color = version)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(
    title = "Ingreso nominal mensual: media ponderada trimestral",
    x = "Año (trimestre)", y = "Pesos", color = "Versión"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "bottom")

save_plot_file(p_nom, "nominal_media_pond_trimestral", dir = CFG$dirs$out_figures, device = "png")
save_plot_file(p_nom, "nominal_media_pond_trimestral", dir = CFG$dirs$out_figures, device = "pdf")

# ------------------------------------------------------------
# 2) Figura: real media ponderada (con/sin winsor)
# ------------------------------------------------------------
p_real <- rea %>%
  dplyr::filter(!is.na(t_index), !is.na(media_pond_real), !is.na(version)) %>%
  ggplot2::ggplot(ggplot2::aes(t_index, media_pond_real, color = version)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(
    title = "Ingreso real mensual: media ponderada trimestral",
    x = "Año (trimestre)", y = "Pesos (base dic-2018=100)", color = "Versión"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "bottom")

save_plot_file(p_real, "real_media_pond_trimestral", dir = CFG$dirs$out_figures, device = "png")
save_plot_file(p_real, "real_media_pond_trimestral", dir = CFG$dirs$out_figures, device = "pdf")

# ------------------------------------------------------------
# 3) Ribbon ±1sd (real) por versión
# ------------------------------------------------------------
# Para ribbon necesitamos sd_real sí o sí
stopifnot("sd_real" %in% names(rea))

rea_w <- rea %>% dplyr::filter(version == "winsor_p99_5")
rea_n <- rea %>% dplyr::filter(version == "sin_winsor")

# Winsor
p_ribbon_w <- rea_w %>%
  dplyr::filter(!is.na(t_index), !is.na(media_pond_real), !is.na(sd_real)) %>%
  ggplot2::ggplot(ggplot2::aes(t_index, media_pond_real)) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = media_pond_real - sd_real,
      ymax = media_pond_real + sd_real
    ),
    alpha = 0.18
  ) +
  ggplot2::geom_line(linewidth = 1.1) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(
    title = "Ingreso real promedio y banda ±1 DE (winsor p99.5)",
    x = "Año (trimestre)", y = "Pesos"
  ) +
  ggplot2::theme_minimal(base_size = 13)

save_plot_file(p_ribbon_w, "ribbon_real_winsor", dir = CFG$dirs$out_figures, device = "png")
save_plot_file(p_ribbon_w, "ribbon_real_winsor", dir = CFG$dirs$out_figures, device = "pdf")

# Sin winsor
p_ribbon_n <- rea_n %>%
  dplyr::filter(!is.na(t_index), !is.na(media_pond_real), !is.na(sd_real)) %>%
  ggplot2::ggplot(ggplot2::aes(t_index, media_pond_real)) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = media_pond_real - sd_real,
      ymax = media_pond_real + sd_real
    ),
    alpha = 0.18
  ) +
  ggplot2::geom_line(linewidth = 1.1) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(
    title = "Ingreso real promedio y banda ±1 DE (sin winsor)",
    x = "Año (trimestre)", y = "Pesos"
  ) +
  ggplot2::theme_minimal(base_size = 13)

save_plot_file(p_ribbon_n, "ribbon_real_sin_winsor", dir = CFG$dirs$out_figures, device = "png")
save_plot_file(p_ribbon_n, "ribbon_real_sin_winsor", dir = CFG$dirs$out_figures, device = "pdf")

message("✔ 07 listo: figuras salarios guardadas en outputs/paper/figures")
