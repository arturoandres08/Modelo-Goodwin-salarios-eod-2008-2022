# ===============================================================
# 11) Media ajustada por coeficiente de variación (CV)
#     - Usa tablas trimestrales de salarios reales:
#       * resumen_trimestral_real_sinwinsor_2008_2022.csv
#       * resumen_trimestral_real_winsor_2008_2022.csv
# ===============================================================

# --- 0. Librerías y helpers ------------------------------------

library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(here)

# Helpers de rutas / guardado (si existen en tu proyecto)
if (file.exists(here("scripts", "00_helpers_paths.R"))) {
  source(here("scripts", "00_helpers_paths.R"))
}
if (file.exists(here("scripts", "00_build_utils.R"))) {
  source(here("scripts", "00_build_utils.R"))
}

# Función auxiliar para guardar tablas, por si no está definida
if (!exists("save_table")) {
  save_table <- function(df, name) {
    path <- here("auditoria_variables", "tablas", paste0(name, ".csv"))
    readr::write_csv(df, path)
    message("Tabla guardada en: ", path)
  }
}

# Función auxiliar para guardar gráficos, por si no está definida
if (!exists("save_plot")) {
  save_plot <- function(p, name, width = 10, height = 6) {
    path <- here("auditoria_variables", "graficos", paste0(name, ".png"))
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    ggplot2::ggsave(path, plot = p, width = width, height = height, dpi = 300)
    message("Gráfico guardado en: ", path)
  }
}

# ===============================================================
# 1) Cargar tablas trimestrales de salarios reales
# ===============================================================

ruta_sin_w   <- here("auditoria_variables", "tablas",
                     "resumen_trimestral_real_sinwinsor_2008_2022.csv")
ruta_winsor  <- here("auditoria_variables", "tablas",
                     "resumen_trimestral_real_winsor_2008_2022.csv")

res_tri_real    <- read_csv(ruta_sin_w, show_col_types = FALSE)
res_tri_real_w  <- read_csv(ruta_winsor, show_col_types = FALSE)

# Esperamos que ya tengan, al menos:
# year, trimestre, t_index,
# media_pond_real, sd_real, var_real, etc.

# ===============================================================
# 2) Calcular coeficiente de variación y media ajustada por CV
# ===============================================================

# --- 2.1 Versión sin winsor ------------------------------------

res_cv_real <- res_tri_real %>%
  mutate(
    cv_real = if_else(
      media_pond_real > 0,
      sd_real / media_pond_real,
      NA_real_
    ),
    media_ajustada_cv = media_pond_real * (1 - cv_real)
  )

# --- 2.2 Versión winsorizada p99.5 -----------------------------

res_cv_real_w <- res_tri_real_w %>%
  mutate(
    cv_real = if_else(
      media_pond_real > 0,
      sd_real / media_pond_real,
      NA_real_
    ),
    media_ajustada_cv = media_pond_real * (1 - cv_real)
  )

# ===============================================================
# 3) Guardar tablas con CV y media ajustada
# ===============================================================

save_table(res_cv_real,
           "resumen_trimestral_real_sinwinsor_cv_2008_2022")

save_table(res_cv_real_w,
           "resumen_trimestral_real_winsor_cv_2008_2022")

# ===============================================================
# 4) Gráfico: media ajustada por CV, sin winsor vs winsor
# ===============================================================

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
    x     = "Año (trimestre)",
    y     = "Pesos",
    color = "Versión"
  ) +
  theme_minimal(base_size = 13)

print(g_media_ajustada_cv)
save_plot(g_media_ajustada_cv, "media_ajustada_cv_comparacion")

# ===============================================================
# 5) Gráficos individuales (opcional)
# ===============================================================

# a) Solo sin winsor: media vs media ajustada
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

print(g_media_sin_w)
save_plot(g_media_sin_w, "media_vs_media_ajustada_cv_sinwinsor")

# b) Solo winsor p99.5: media vs media ajustada
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

print(g_media_w)
save_plot(g_media_w, "media_vs_media_ajustada_cv_winsor")

# ===============================================================
# FIN DEL SCRIPT
# ===============================================================
