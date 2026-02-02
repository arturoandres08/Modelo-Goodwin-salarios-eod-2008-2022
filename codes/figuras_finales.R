# ===============================================================
# FIGURAS FINALES (IDENTIFICATION / DIAGNÓSTICOS) EOD 2008–2022
# - Usa SOLO las tablas ya construidas en auditoria_variables/tablas
# - Genera TODOS los gráficos que estás usando en LaTeX:
#   * medias_trimestrales_winsorizadas
#   * medias_trimestrales_pond_winsor_vs_sinwinsor
#   * medias_trimestrales_reales_comparacion
#   * masa_salarial_trimestral_nominal_winsor
#   * masa_salarial_real_trimestral_winsor
#   * desviacion_estandar_trimestral_real
#   * varianza_trimestral_real (extra, por si la quieres)
#   * ribbon_ingreso_real_sinwinsor
#   * ribbon_ingreso_real_winsor
#   * media_vs_media_ajustada_cv_sinwinsor
#   * media_vs_media_ajustada_cv_winsor
#   * media_ajustada_cv_comparacion
#   * tasa_desempleo_trimestral_2008_2022
#   * obs_boxplots_anuales_log   (boxplots de log(ing_real))
# ===============================================================

# --------------------- 0) Paquetes -----------------------------
pkgs <- c("tidyverse", "here", "readr", "scales", "fs", "readxl", "janitor")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install)) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
invisible(lapply(pkgs, library, character.only = TRUE))

# --------------------- 0.1 Helpers -----------------------------
# Si tienes helpers propios, los usamos
if (file.exists(here("scripts", "00_helpers_paths.R"))) {
  source(here("scripts", "00_helpers_paths.R"))
}

# save_plot() y save_table() de respaldo (por si no están definidos)
if (!exists("save_plot")) {
  save_plot <- function(p, name,
                        dir = here("auditoria_variables", "graficos"),
                        width = 10, height = 5, dpi = 300,
                        device = c("png")) {
    device <- match.arg(device)
    fs::dir_create(dir)
    f <- fs::path(dir, paste0(name, ".", device))
    ggplot2::ggsave(
      filename = f,
      plot     = p,
      width    = width,
      height   = height,
      dpi      = dpi,
      units    = "in"
    )
    message("✔ Gráfico guardado en: ", f)
    invisible(f)
  }
}

if (!exists("save_table")) {
  save_table <- function(x, name,
                         dir = here("auditoria_variables", "tablas")) {
    fs::dir_create(dir)
    f <- fs::path(dir, paste0(name, ".csv"))
    readr::write_csv(x, f)
    message("✔ Tabla guardada en: ", f)
    invisible(f)
  }
}

# Helper para trimestre → número (T1..T4 → 1..4)
q_num <- function(tri) match(toupper(tri), c("T1","T2","T3","T4"))

# --------------------- 1) Cargar tablas ------------------------
tab_dir <- here("auditoria_variables", "tablas")

res_tri_w       <- read_csv(
  file.path(tab_dir, "resumen_trimestral_winsor_2008_2022.csv"),
  show_col_types = FALSE
)
res_tri_now     <- read_csv(
  file.path(tab_dir, "resumen_trimestral_sin_winsor_2008_2022.csv"),
  show_col_types = FALSE
)
res_tri_real    <- read_csv(
  file.path(tab_dir, "resumen_trimestral_real_sinwinsor_2008_2022.csv"),
  show_col_types = FALSE
)
res_tri_real_w  <- read_csv(
  file.path(tab_dir, "resumen_trimestral_real_winsor_2008_2022.csv"),
  show_col_types = FALSE
)
desemp_tri      <- read_csv(
  file.path(tab_dir, "resumen_trimestral_desempleo_2008_2022.csv"),
  show_col_types = FALSE
)

# Aseguramos existencia de t_index (por si la tabla original no lo trae)
if (!"t_index" %in% names(res_tri_w)) {
  res_tri_w <- res_tri_w %>%
    mutate(t_index = year + q_num(trimestre) / 10)
}
if (!"t_index" %in% names(res_tri_now)) {
  res_tri_now <- res_tri_now %>%
    mutate(t_index = year + q_num(trimestre) / 10)
}
if (!"t_index" %in% names(res_tri_real)) {
  res_tri_real <- res_tri_real %>%
    mutate(t_index = year + q_num(trimestre) / 10)
}
if (!"t_index" %in% names(res_tri_real_w)) {
  res_tri_real_w <- res_tri_real_w %>%
    mutate(t_index = year + q_num(trimestre) / 10)
}
if (!"t_index" %in% names(desemp_tri)) {
  desemp_tri <- desemp_tri %>%
    mutate(t_index = year_final + q_num(tri_fix) / 10)
}

# ===============================================================
# F1: Medias nominales (winsor) y comparación con/sin winsor
# ===============================================================

# F1.1: medias nominales SOLO winsor → medias_trimestrales_winsorizadas
p_medias_winsor <- res_tri_w %>%
  ggplot(aes(t_index, media_pond)) +
  geom_line(linewidth = 1, color = "#1f77b4") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Ingreso nominal mensual winsorizado (p99.5):\nmedias ponderadas trimestrales",
    x = "Año (trimestre)",
    y = "Pesos"
  ) +
  theme_minimal(base_size = 13)

save_plot(p_medias_winsor, "medias_trimestrales_winsorizadas")

# F1.2: medias nominales con vs sin winsor → medias_trimestrales_pond_winsor_vs_sinwinsor
res_tri_comp_nom <- bind_rows(
  res_tri_w   %>% mutate(variante = "winsor_p99_5"),
  res_tri_now %>% mutate(variante = "sin_winsor")
)

p_medias_comp_nom <- res_tri_comp_nom %>%
  ggplot(aes(t_index, media_pond, color = variante)) +
  geom_line(linewidth = 1.1) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Ingreso nominal mensual: medias ponderadas trimestrales\ncon y sin winsor p99.5",
    x = "Año (trimestre)",
    y = "Pesos",
    color = "Versión"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

save_plot(p_medias_comp_nom, "medias_trimestrales_pond_winsor_vs_sinwinsor")

# ===============================================================
# F2: Medias reales y masa salarial nominal/real
# ===============================================================

# F2.1: medias reales (winsor vs sin winsor) → medias_trimestrales_reales_comparacion
res_tri_comp_real <- bind_rows(
  res_tri_real   %>% mutate(version = "sin_winsor"),
  res_tri_real_w %>% mutate(version = "winsor_p99_5")
)

p_medias_comp_real <- res_tri_comp_real %>%
  ggplot(aes(t_index, media_pond_real, color = version)) +
  geom_line(linewidth = 1.1) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Ingreso real mensual: medias ponderadas trimestrales",
    x = "Año (trimestre)",
    y = "Pesos",
    color = "Versión"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

save_plot(p_medias_comp_real, "medias_trimestrales_reales_comparacion")

# F2.2: masa salarial nominal winsor → masa_salarial_trimestral_nominal_winsor
p_masa_nom <- res_tri_w %>%
  ggplot(aes(t_index, masa_salarial)) +
  geom_line(color = "#2E86AB", linewidth = 1.1) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Masa salarial nominal trimestral (winsorizada al p99.5)",
    x = "Año (trimestre)",
    y = "Pesos"
  ) +
  theme_minimal(base_size = 13)

save_plot(p_masa_nom, "masa_salarial_trimestral_nominal_winsor")

# F2.3: masa salarial real winsor → masa_salarial_real_trimestral_winsor
p_masa_real <- res_tri_real_w %>%
  ggplot(aes(t_index, masa_salarial_real)) +
  geom_line(color = "#2E86AB", linewidth = 1.1) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Masa salarial real trimestral (winsor p99.5)",
    x = "Año (trimestre)",
    y = "Pesos"
  ) +
  theme_minimal(base_size = 13)

save_plot(p_masa_real, "masa_salarial_real_trimestral_winsor")

# ===============================================================
# F3: Dispersión de salarios reales (DE y varianza)
# ===============================================================

# Aseguramos que sd_real/var_real existan; si no, advertimos
if (!("sd_real" %in% names(res_tri_real))) {
  warning("res_tri_real no tiene sd_real/var_real; deberías recalcularlos en el script maestro.")
}
if (!("sd_real" %in% names(res_tri_real_w))) {
  warning("res_tri_real_w no tiene sd_real/var_real; deberías recalcularlos en el script maestro.")
}

disp_full <- bind_rows(
  res_tri_real   %>% mutate(version = "sin_winsor"),
  res_tri_real_w %>% mutate(version = "winsor_p99_5")
)

# F3.1: desviación estándar → desviacion_estandar_trimestral_real
p_sd_real <- disp_full %>%
  ggplot(aes(t_index, sd_real, color = version)) +
  geom_line(linewidth = 1.1) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Desviación estándar de ingresos reales\n(trimestral, 2008–2022)",
    x = "Año (trimestre)",
    y = "Desviación estándar (pesos)",
    color = "Versión"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

save_plot(p_sd_real, "desviacion_estandar_trimestral_real")

# F3.2: varianza (extra) → varianza_trimestral_real
if ("var_real" %in% names(disp_full)) {
  p_var_real <- disp_full %>%
    ggplot(aes(t_index, var_real, color = version)) +
    geom_line(linewidth = 1.1) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Varianza de ingresos reales\n(trimestral, 2008–2022)",
      x = "Año (trimestre)",
      y = "Varianza (pesos^2)",
      color = "Versión"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
  
  save_plot(p_var_real, "varianza_trimestral_real")
}

# ===============================================================
# F4: Ribbons ±1 DE alrededor de la media real
# ===============================================================

# F4.1: sin winsor → ribbon_ingreso_real_sinwinsor
plot_ribbon_real <- res_tri_real %>%
  ggplot(aes(t_index, media_pond_real)) +
  geom_ribbon(aes(
    ymin = media_pond_real - sd_real,
    ymax = media_pond_real + sd_real
  ),
  fill = "#1f77b4", alpha = 0.18
  ) +
  geom_line(color = "#1f77b4", linewidth = 1.1) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Ingreso real promedio y banda ±1 DE (sin winsor)",
    x = "Año (trimestre)",
    y = "Pesos"
  ) +
  theme_minimal(base_size = 13)

save_plot(plot_ribbon_real, "ribbon_ingreso_real_sinwinsor")

# F4.2: winsor → ribbon_ingreso_real_winsor
plot_ribbon_real_w <- res_tri_real_w %>%
  ggplot(aes(t_index, media_pond_real)) +
  geom_ribbon(aes(
    ymin = media_pond_real - sd_real,
    ymax = media_pond_real + sd_real
  ),
  fill = "#1f77b4", alpha = 0.18
  ) +
  geom_line(color = "#1f77b4", linewidth = 1.1) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Ingreso real promedio y banda ±1 DE\n(winsorizado al p99.5)",
    x = "Año (trimestre)",
    y = "Pesos"
  ) +
  theme_minimal(base_size = 13)

save_plot(plot_ribbon_real_w, "ribbon_ingreso_real_winsor")

# ===============================================================
# F5: Media ajustada por coeficiente de variación (CV)
# ===============================================================

res_cv_real <- res_tri_real %>%
  mutate(
    cv_real = if_else(media_pond_real > 0,
                      sd_real / media_pond_real,
                      NA_real_),
    media_ajustada_cv = media_pond_real * (1 - cv_real)
  )

res_cv_real_w <- res_tri_real_w %>%
  mutate(
    cv_real = if_else(media_pond_real > 0,
                      sd_real / media_pond_real,
                      NA_real_),
    media_ajustada_cv = media_pond_real * (1 - cv_real)
  )

# Guardamos las tablas por si quieres usarlas después
save_table(res_cv_real,   "resumen_trimestral_real_sinwinsor_cv_2008_2022")
save_table(res_cv_real_w, "resumen_trimestral_real_winsor_cv_2008_2022")

# F5.1: comparación media ajustada (sin vs winsor) → media_ajustada_cv_comparacion
cv_comp <- bind_rows(
  res_cv_real %>%
    select(t_index, media_ajustada_cv) %>%
    mutate(version = "sin_winsor"),
  res_cv_real_w %>%
    select(t_index, media_ajustada_cv) %>%
    mutate(version = "winsor_p99_5")
)

g_media_ajustada_cv <- cv_comp %>%
  ggplot(aes(t_index, media_ajustada_cv, color = version)) +
  geom_line(linewidth = 1.1) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Media ajustada por coeficiente de variación\n(trimestral, 2008–2022)",
    x = "Año (trimestre)",
    y = "Pesos"
  ) +
  theme_minimal(base_size = 13)

save_plot(g_media_ajustada_cv, "media_ajustada_cv_comparacion")

# F5.2: solo sin winsor → media_vs_media_ajustada_cv_sinwinsor
g_media_sin_w <- res_cv_real %>%
  select(t_index, media_pond_real, media_ajustada_cv) %>%
  tidyr::pivot_longer(
    cols      = c(media_pond_real, media_ajustada_cv),
    names_to  = "tipo",
    values_to = "valor"
  ) %>%
  mutate(
    tipo = recode(
      tipo,
      media_pond_real   = "Media ponderada",
      media_ajustada_cv = "Media ajustada por CV"
    )
  ) %>%
  ggplot(aes(t_index, valor, color = tipo)) +
  geom_line(linewidth = 1.0) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Ingreso real: media vs media ajustada por CV\n(sin winsor)",
    x = "Año (trimestre)",
    y = "Pesos",
    color = "Serie"
  ) +
  theme_minimal(base_size = 13)

save_plot(g_media_sin_w, "media_vs_media_ajustada_cv_sinwinsor")

# F5.3: solo winsor → media_vs_media_ajustada_cv_winsor
g_media_w <- res_cv_real_w %>%
  select(t_index, media_pond_real, media_ajustada_cv) %>%
  tidyr::pivot_longer(
    cols      = c(media_pond_real, media_ajustada_cv),
    names_to  = "tipo",
    values_to = "valor"
  ) %>%
  mutate(
    tipo = recode(
      tipo,
      media_pond_real   = "Media ponderada",
      media_ajustada_cv = "Media ajustada por CV"
    )
  ) %>%
  ggplot(aes(t_index, valor, color = tipo)) +
  geom_line(linewidth = 1.0) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Ingreso real: media vs media ajustada por CV\n(winsor p99.5)",
    x = "Año (trimestre)",
    y = "Pesos",
    color = "Serie"
  ) +
  theme_minimal(base_size = 13)

save_plot(g_media_w, "media_vs_media_ajustada_cv_winsor")

# ===============================================================
# F6: Tasa de desempleo trimestral → tasa_desempleo_trimestral_2008_2022
# ===============================================================

p_desemp <- desemp_tri %>%
  ggplot(aes(t_index, tasa_desemp)) +
  geom_line(color = "#D55E00", linewidth = 1.2) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Tasa de desempleo trimestral (EOD, 2008–2022)",
    x = "Año (trimestre)",
    y = "Porcentaje"
  ) +
  theme_minimal(base_size = 14)

save_plot(p_desemp, "tasa_desempleo_trimestral_2008_2022")

# ===============================================================
# F7: Boxplots anuales de ingreso REAL (escala log)
#     → obs_boxplots_anuales_log
# ===============================================================

ings_nom_path <- file.path(tab_dir, "ingsueld_factorhd_nominal_2008_2022.xlsx")
ipc_path      <- file.path(tab_dir, "ipc_mensual_base2018_100.csv")

if (file.exists(ings_nom_path) && file.exists(ipc_path)) {
  
  ings_nom <- readxl::read_xlsx(ings_nom_path) %>%
    janitor::clean_names()
  ipc      <- read_csv(ipc_path, show_col_types = FALSE)
  
  # Normalizamos IPC: year, month, ipc
  ipc <- ipc %>%
    janitor::clean_names() %>%
    rename_with(~"ipc", matches("ipc$")) %>%
    rename(year = matches("year"), month = matches("month"))
  
  # ==== ARREGLO: elegir columna de año correcta (vector, no función) ====
  year_candidates <- c("year_final", "year", "ano", "agno")
  year_name <- year_candidates[year_candidates %in% names(ings_nom)][1]
  
  if (is.na(year_name)) {
    year_col_vec <- rep(NA_integer_, nrow(ings_nom))
  } else {
    year_col_vec <- suppressWarnings(as.integer(ings_nom[[year_name]]))
  }
  # ======================================================================
  
  mes_map <- c(
    "enero"="01","febrero"="02","marzo"="03","abril"="04","mayo"="05","junio"="06",
    "julio"="07","agosto"="08","septiembre"="09","setiembre"="09","octubre"="10",
    "noviembre"="11","diciembre"="12","ene"="01","feb"="02","mar"="03","abr"="04",
    "may"="05","jun"="06","jul"="07","ago"="08","sep"="09","oct"="10","nov"="11","dic"="12"
  )
  
  infer_month_from_filename <- function(fname){
    f <- tolower(basename(fname))
    # 1) nombre de mes
    pat   <- paste(names(mes_map), collapse = "|")
    m_chr <- stringr::str_extract(f, pat)
    if (!is.na(m_chr)) return(unname(mes_map[m_chr]))
    # 2) 01-12
    mm <- stringr::str_match(f, "([01][0-9])")[, 2]
    if (!is.na(mm) && mm %in% sprintf("%02d", 1:12)) return(mm)
    # 3) t1..t4
    if (stringr::str_detect(f, "t1")) return("03")
    if (stringr::str_detect(f, "t2")) return("06")
    if (stringr::str_detect(f, "t3")) return("09")
    if (stringr::str_detect(f, "t4")) return("12")
    NA_character_
  }
  infer_month_vec <- function(x) vapply(x, infer_month_from_filename, character(1))
  
  ings_box <- ings_nom %>%
    mutate(
      archivo   = tolower(archivo),
      ingsueld  = as.numeric(ingsueld),
      # top-code nominal
      ingsueld  = ifelse(ingsueld > 10000000 | ingsueld < 0,
                         NA_real_, ingsueld),
      factor_hd = as.numeric(factor_hd),
      year_file = suppressWarnings(
        as.integer(stringr::str_extract(archivo, "(19|20)\\d{2}"))
      ),
      # usamos year_col_vec (vector) para evitar conflicto con función year()
      year_col  = year_col_vec,
      year_final = dplyr::coalesce(
        dplyr::if_else(
          dplyr::between(year_col, 1900L, 2100L),
          year_col, NA_integer_
        ),
        year_file
      ),
      month_chr = ifelse(
        !is.na(mes),
        stringr::str_pad(mes, 2, pad = "0"),
        infer_month_vec(archivo)
      ),
      month = suppressWarnings(as.integer(month_chr))
    ) %>%
    filter(
      !is.na(ingsueld),
      !is.na(year_final), between(year_final, 2008, 2022),
      !is.na(month),
      !is.na(factor_hd), factor_hd > 0
    )
  
  # Join con IPC y calcular ing_real
  ings_box <- ings_box %>%
    left_join(
      ipc %>% select(year, month, ipc),
      by = c("year_final" = "year", "month" = "month")
    ) %>%
    filter(!is.na(ipc)) %>%
    mutate(
      ing_real = ingsueld * (100 / ipc)
    )
  
  # Boxplots en escala log10 de niveles (como en el script corregido)
  box_log <- ings_box %>%
    filter(ing_real > 0) %>%
    ggplot(aes(x = factor(year_final), y = ing_real)) +
    geom_boxplot(
      fill = "#1f77b4",
      colour = "grey30",
      outlier.alpha = 0.15,
      outlier.size  = 0.8
    ) +
    scale_y_log10(labels = scales::comma) +
    labs(
      title = "Boxplots de ingreso REAL mensual por año\n(escala log, 2008–2022)",
      x = "Año",
      y = "Ingreso real mensual (log)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  save_plot(box_log, "obs_boxplots_anuales_log")
  
} else {
  warning("No encontré ingsueld_factorhd_nominal_2008_2022.xlsx o ipc_mensual_base2018_100.csv; no se generará obs_boxplots_anuales_log.")
}

message("✔ Script de figuras finales terminado.")

# ===============================================================
# F8: Panel apilado — desempleo, salarios REALES y productividad IMACEC
# ===============================================================

if (!requireNamespace("magick", quietly = TRUE)) {
  install.packages("magick", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork", repos = "https://cloud.r-project.org")
}

library(magick)
library(patchwork)
library(grid)

path_g <- here("auditoria_variables", "graficos")

# A) Tasa de desempleo (PNG ya creado)
img_desemp <- magick::image_read(
  file.path(path_g, "tasa_desempleo_trimestral_2008_2022.png")
)

# B) Media salarial REAL mensual promedio winsorizada (PNG ya creado)
#    OJO: este es el gráfico nuevo que me mostraste:
#    "ingreso_real_promedio_trimestral_winsor.png"
img_wage <- magick::image_read(
  file.path(path_g, "ingreso_real_promedio_trimestral_winsor.png")
  # si lo guardaste con otro nombre, cámbialo aquí
)

# C) Productividad del trabajo: IMACEC total
#    1) Si no existe PNG, lo creamos a partir del PDF
imacec_pdf  <- file.path(path_g, "G1_productividad_total_imacec_total.pdf")
imacec_png  <- file.path(path_g, "G1_productividad_total_imacec_total.png")

if (!file.exists(imacec_png)) {
  img_pdf <- magick::image_read_pdf(imacec_pdf, density = 300)
  magick::image_write(img_pdf, path = imacec_png, format = "png")
  message("✔ Convertido G1_productividad_total_imacec_total.pdf → PNG")
}

#    2) Ahora siempre leemos el PNG (sin depender del PDF)
img_imacec <- magick::image_read(imacec_png)

# Pasar cada imagen a "plot" para patchwork
p_desemp_img <- wrap_elements(rasterGrob(as.raster(img_desemp)))
p_wage_img   <- wrap_elements(rasterGrob(as.raster(img_wage)))
p_imacec_img <- wrap_elements(rasterGrob(as.raster(img_imacec)))

# Panel apilado (tipo pB_2 / pB_3)
panel_stack <- p_desemp_img / p_wage_img / p_imacec_img

# Guardar panel final (PNG como el resto)
save_plot(
  panel_stack,
  "panel_desempleo_salario_real_imacec",
  width  = 10,
  height = 9
)

# ===============================================================
# F9: Primera diferencia de las series
#      - Δ tasa de desempleo
#      - Δ ingreso real promedio (winsor p99.5)
#      - Δ productividad laboral (IMACEC total)
# ===============================================================

# 1) Cargar índice de productividad (IMACEC total) desde tablas
prod_total_tab <- readr::read_csv(
  file.path(tab_dir, "indice_productividad_trimestral_2008_2022.csv"),
  show_col_types = FALSE
)
# Aseguramos t_index (por si acaso)
if (!"t_index" %in% names(prod_total_tab)) {
  prod_total_tab <- prod_total_tab %>%
    dplyr::mutate(t_index = year + trimestre / 10)
}

# 2) Primera diferencia de cada serie ---------------------------

# a) Desempleo
desemp_diff <- desemp_tri %>%
  dplyr::arrange(t_index) %>%
  dplyr::transmute(
    t_index,
    d_desemp = tasa_desemp - dplyr::lag(tasa_desemp)
  )

# b) Ingreso REAL promedio (winsor p99.5)
wage_diff <- res_tri_real_w %>%
  dplyr::arrange(t_index) %>%
  dplyr::transmute(
    t_index,
    d_wage_real = media_pond_real - dplyr::lag(media_pond_real)
  )

# c) Productividad laboral (IMACEC total)
prod_diff <- prod_total_tab %>%
  dplyr::arrange(t_index) %>%
  dplyr::transmute(
    t_index,
    d_prod = prod_indice - dplyr::lag(prod_indice)
  )

# 3) Unir en una sola tabla (por t_index) ----------------------

diff_panel <- desemp_diff %>%
  dplyr::inner_join(wage_diff, by = "t_index") %>%
  dplyr::inner_join(prod_diff, by = "t_index") %>%
  dplyr::filter(!is.na(d_desemp),
                !is.na(d_wage_real),
                !is.na(d_prod))

# 4) Gráficos individuales de las primeras diferencias ---------

library(ggplot2)
library(patchwork)

p_d_desemp <- diff_panel %>%
  ggplot(aes(t_index, d_desemp)) +
  geom_line(color = "#D55E00", linewidth = 1) +
  labs(
    title = "Δ tasa de desempleo trimestral",
    x = NULL,
    y = "p.p. (Δ %)"
  ) +
  theme_minimal(base_size = 12)

p_d_wage <- diff_panel %>%
  ggplot(aes(t_index, d_wage_real)) +
  geom_line(color = "black", linewidth = 1) +
  labs(
    title = "Δ ingreso real mensual promedio (winsor p99.5)",
    x = NULL,
    y = "Pesos de 2018"
  ) +
  theme_minimal(base_size = 12)

p_d_prod <- diff_panel %>%
  ggplot(aes(t_index, d_prod)) +
  geom_line(color = "#0072B2", linewidth = 1) +
  labs(
    title = "Δ índice de productividad laboral (IMACEC total)",
    x = "Año (trimestre)",
    y = "Índice (2018 = 100)"
  ) +
  theme_minimal(base_size = 12)

# 5) Panel apilado con las tres primeras diferencias -----------

panel_diff <- p_d_desemp / p_d_wage / p_d_prod +
  plot_layout(heights = c(1, 1, 1))

save_plot(
  panel_diff,
  "panel_primeras_diferencias_desemp_salario_prod",
  width  = 10,
  height = 9,
  dpi    = 300
)
