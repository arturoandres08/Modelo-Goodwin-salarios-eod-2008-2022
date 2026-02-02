# codes/07_figures_wages_real.R
# ===============================================================
# 07) Figuras salarios reales (incluye SD)
# - Lee: data/processed/real_quarterly_2008_2022.rds
# - Produce:
#   * figures/wages_real_mean_with_sd.{png,pdf}
#   * figures/wages_real_sd.{png,pdf}
# ===============================================================

source(here::here("codes","00_setup.R"))

message("\n== 07) Figuras salarios reales ==\n")

# ---- 1) Cargar output de 06 ----
stopifnot(file.exists(CFG$paths$real_quarterly))

rea <- readRDS(CFG$paths$real_quarterly)

# (Opcional) si quieres estandarizar nombres a snake_case:
# rea <- rea %>% janitor::clean_names()

# ---- 2) Helpers seguros (sin if_else vectorizado con nombres(.)) ----
parse_trimestre <- function(x){
  if (is.numeric(x)) return(as.integer(x))
  xx <- toupper(as.character(x))
  xx <- gsub("^T", "", xx)
  suppressWarnings(as.integer(xx))
}

add_col_if_missing <- function(df, nm, value){
  if (!nm %in% names(df)) df[[nm]] <- value
  df
}

# ---- 3) Normalización de tipos ----
rea <- rea %>%
  dplyr::mutate(
    version   = as.character(version),
    year      = as.integer(year),
    trimestre = parse_trimestre(trimestre)
  )

# ---- 4) Compatibilidad de nombres (por si tu 06 cambia) ----
# mean_real: si no existe pero existe media_pond_real, lo usamos
if (!"mean_real" %in% names(rea) && "media_pond_real" %in% names(rea)) {
  rea <- rea %>% dplyr::mutate(mean_real = as.numeric(media_pond_real))
}

# sd_real: si alguna vez viene con otro nombre, lo puedes mapear aquí
# if (!"sd_real" %in% names(rea) && "sd_real_new" %in% names(rea)) {
#   rea <- rea %>% dplyr::mutate(sd_real = as.numeric(sd_real_new))
# }

# t_index: usa el que venga de 06; si no viene, lo crea
rea <- add_col_if_missing(
  rea, "t_index",
  rea$year + rea$trimestre/10
)

# ---- 5) Checks mínimos (antes de graficar) ----
req <- c("version","year","trimestre","t_index","mean_real","sd_real")
miss <- setdiff(req, names(rea))
if (length(miss) > 0) {
  stop("07: faltan columnas en real_quarterly: ", paste(miss, collapse = ", "))
}

stopifnot(all(rea$trimestre %in% 1:4))
stopifnot(!anyNA(rea$mean_real))
stopifnot(!anyNA(rea$sd_real))

rea <- rea %>%
  dplyr::arrange(version, year, trimestre)

# Etiquetas más lindas (opcional)
rea <- rea %>%
  dplyr::mutate(
    version_lab = dplyr::case_when(
      version == "sin_winsor"   ~ "Sin winsor",
      version == "winsor_p99_5" ~ "Winsor p99.5",
      TRUE ~ version
    )
  )

# ---- 6) Theme formal “tesis” ----
theme_tesis <- function() {
  ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

# ===============================================================
# 1) Media real + banda +/- 1 SD  (FACET por versión para evitar superposición)
# ===============================================================
p_mean <- ggplot2::ggplot(
  rea,
  ggplot2::aes(x = t_index, y = mean_real)
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = mean_real - sd_real,
      ymax = mean_real + sd_real
    ),
    fill = "grey80", alpha = 0.5, colour = NA
  ) +
  ggplot2::geom_line(color = "black", linewidth = 0.9) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(
    title = "Salario real trimestral (media) con banda ±1 desviación estándar",
    x = "Año-trimestre (t_index)",
    y = "Pesos reales (base IPC dic-2018 = 100)"
  ) +
  ggplot2::facet_wrap(~version_lab, ncol = 1, scales = "free_y") +
  theme_tesis()

save_plot_file(p_mean, "wages_real_mean_with_sd", device = "png", width = 10, height = 6, dpi = 300)
save_plot_file(p_mean, "wages_real_mean_with_sd", device = "pdf", width = 10, height = 6, dpi = 300)

# ===============================================================
# 2) Serie de SD real  (FACET por versión)
# ===============================================================
p_sd <- ggplot2::ggplot(
  rea,
  ggplot2::aes(x = t_index, y = sd_real)
) +
  ggplot2::geom_line(color = "black", linewidth = 0.9) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(
    title = "Salario real trimestral: desviación estándar (SD)",
    x = "Año-trimestre (t_index)",
    y = "SD en pesos reales"
  ) +
  ggplot2::facet_wrap(~version_lab, ncol = 1, scales = "free_y") +
  theme_tesis()

save_plot_file(p_sd, "wages_real_sd", device = "png", width = 10, height = 6, dpi = 300)
save_plot_file(p_sd, "wages_real_sd", device = "pdf", width = 10, height = 6, dpi = 300)

message("✅ 07 listo: figuras guardadas en /figures.\n")
