# codes/13_figures_imacec_employment_productivity.R
source(here("codes", "00_setup.R"))

imacec_tri <- load_rds(CFG$paths$imacec_quarterly)
empleo_tri <- load_rds(CFG$paths$employment_quarterly)
prod_total <- load_rds(CFG$paths$prod_total_quarterly)
prod_nm    <- load_rds(CFG$paths$prod_nonmining_quarterly)

# ---------------------------------------------------------------
# 0) Normalizadores (robustos, como en el 12)
# ---------------------------------------------------------------
to_q_int <- function(x) {
  if (is.numeric(x)) return(as.integer(x))
  as.integer(stringr::str_extract(toupper(as.character(x)), "[1-4]"))
}

# Fecha trimestral: mes final del trimestre (3,6,9,12)
to_fecha_tri <- function(year, trimestre) {
  y <- as.integer(year)
  q <- to_q_int(trimestre)
  # usa make_date para evitar problemas de parseo
  lubridate::make_date(year = y, month = q * 3L, day = 1L)
}

# Asegurar tipos consistentes (por si algo vino como character)
imacec_tri <- imacec_tri %>%
  dplyr::mutate(
    year = as.integer(year),
    trimestre = to_q_int(trimestre),
    t_index = as.numeric(t_index)
  )

empleo_tri <- empleo_tri %>%
  dplyr::mutate(
    year = as.integer(year),
    trimestre = to_q_int(trimestre),
    t_index = as.numeric(t_index)
  )

prod_total <- prod_total %>%
  dplyr::mutate(
    year = as.integer(year),
    trimestre = to_q_int(trimestre)
  )

prod_nm <- prod_nm %>%
  dplyr::mutate(
    year = as.integer(year),
    trimestre = to_q_int(trimestre)
  )

# Checks mínimos (para que falle informando)
stopifnot(all(c("year","trimestre") %in% names(prod_total)))
stopifnot(all(c("year","trimestre") %in% names(prod_nm)))

# ---------------------------------------------------------------
# 1) Productividad total
# ---------------------------------------------------------------
p_prod_total <- prod_total %>%
  dplyr::mutate(fecha_tri = to_fecha_tri(year, trimestre)) %>%
  dplyr::filter(!is.na(fecha_tri), !is.na(prod_indice)) %>%
  ggplot2::ggplot(ggplot2::aes(fecha_tri, prod_indice)) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::labs(
    title = "Índice de productividad laboral (IMACEC total)",
    subtitle = "Base 2018 = 100, 2008–2022",
    x = "Año", y = "Índice (2018 = 100)"
  ) +
  ggplot2::theme_minimal(base_size = 12)

save_plot_file(p_prod_total, "productivity_total_base2018_100", dir = CFG$dirs$out_figures, device = "pdf")
save_plot_file(p_prod_total, "productivity_total_base2018_100", dir = CFG$dirs$out_figures, device = "png")

# ---------------------------------------------------------------
# 2) Productividad no minera
# ---------------------------------------------------------------
p_prod_nm <- prod_nm %>%
  dplyr::mutate(fecha_tri = to_fecha_tri(year, trimestre)) %>%
  dplyr::filter(!is.na(fecha_tri), !is.na(prod_indice)) %>%
  ggplot2::ggplot(ggplot2::aes(fecha_tri, prod_indice)) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::labs(
    title = "Índice de productividad laboral (IMACEC no minero)",
    subtitle = "Base 2018 = 100, 2008–2022",
    x = "Año", y = "Índice (2018 = 100)"
  ) +
  ggplot2::theme_minimal(base_size = 12)

save_plot_file(p_prod_nm, "productivity_nonmining_base2018_100", dir = CFG$dirs$out_figures, device = "pdf")
save_plot_file(p_prod_nm, "productivity_nonmining_base2018_100", dir = CFG$dirs$out_figures, device = "png")

# ---------------------------------------------------------------
# 3) Productividad total vs no minera (comparado)
# ---------------------------------------------------------------
prod_long <- dplyr::bind_rows(
  prod_total %>%
    dplyr::mutate(fecha_tri = to_fecha_tri(year, trimestre)) %>%
    dplyr::transmute(fecha_tri, serie = "Total", indice = prod_indice),
  prod_nm %>%
    dplyr::mutate(fecha_tri = to_fecha_tri(year, trimestre)) %>%
    dplyr::transmute(fecha_tri, serie = "No minera", indice = prod_indice)
) %>%
  dplyr::filter(!is.na(fecha_tri), !is.na(indice)) %>%
  dplyr::arrange(fecha_tri)

p_prod_both <- ggplot2::ggplot(prod_long, ggplot2::aes(fecha_tri, indice, color = serie)) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::labs(
    title = "Productividad laboral: IMACEC total vs no minero",
    subtitle = "Índices trimestrales, base 2018 = 100",
    x = "Año", y = "Índice (2018 = 100)", color = "Serie"
  ) +
  ggplot2::theme_minimal(base_size = 12)

save_plot_file(p_prod_both, "productivity_total_vs_nonmining_base2018_100", dir = CFG$dirs$out_figures, device = "pdf")
save_plot_file(p_prod_both, "productivity_total_vs_nonmining_base2018_100", dir = CFG$dirs$out_figures, device = "png")

# ---------------------------------------------------------------
# 4) IMACEC total vs empleo (comparado)
# ---------------------------------------------------------------
imacec_emp <- imacec_tri %>%
  dplyr::select(year, trimestre, t_index, imacec_indice) %>%
  dplyr::left_join(
    empleo_tri %>% dplyr::select(year, trimestre, t_index, empleo_indice),
    by = c("year","trimestre","t_index")
  ) %>%
  dplyr::mutate(fecha_tri = to_fecha_tri(year, trimestre))

# Aviso si el join quedó con muchos NA en empleo_indice
share_na_emp <- mean(is.na(imacec_emp$empleo_indice))
if (is.finite(share_na_emp) && share_na_emp > 0.05) {
  warning("Más del 5% quedó sin empleo_indice en IMACEC vs empleo (NA). Revisa consistencia trimestre/t_index.")
}

imacec_emp_long <- imacec_emp %>%
  tidyr::pivot_longer(
    cols = c(imacec_indice, empleo_indice),
    names_to = "serie",
    values_to = "indice"
  ) %>%
  dplyr::filter(!is.na(fecha_tri), !is.na(indice)) %>%
  dplyr::mutate(
    serie = dplyr::recode(
      serie,
      imacec_indice = "IMACEC total (índice)",
      empleo_indice = "Empleo (ocupados, índice)"
    )
  )

p_imacec_emp <- ggplot2::ggplot(imacec_emp_long, ggplot2::aes(fecha_tri, indice, color = serie)) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::labs(
    title = "IMACEC total vs Empleo (ocupados)",
    subtitle = "Índices trimestrales, base 2018 = 100, 2008–2022",
    x = "Año", y = "Índice (2018 = 100)", color = "Serie"
  ) +
  ggplot2::theme_minimal(base_size = 12)

save_plot_file(p_imacec_emp, "imacec_total_vs_employment_base2018_100", dir = CFG$dirs$out_figures, device = "pdf")
save_plot_file(p_imacec_emp, "imacec_total_vs_employment_base2018_100", dir = CFG$dirs$out_figures, device = "png")

message("✔ 13 listo: figuras IMACEC/Empleo/Productividad guardadas")
