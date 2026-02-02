# ===============================================================
#  ÍNDICES TRIMESTRALES: IMACEC, EMPLEO Y PRODUCTIVIDAD
#  Base 2018 = 100, período 2008–2022
# ===============================================================

library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(here)

# Si tienes helpers de rutas, los cargamos (no es obligatorio)
if (file.exists(here("scripts", "00_helpers_paths.R"))) {
  source(here("scripts", "00_helpers_paths.R"))
}

# ---------------------------------------------------------------
# 1) IMACEC MENSUAL → TRIMESTRAL (TOTAL Y NO MINERO)
# ---------------------------------------------------------------

# 1.1 Leer Excel original (hoja "Cuadro")
imacec_raw <- read_excel(
  here("data", "imacec", "CCNN2018_IMACEC_01_A.xlsx"),
  sheet = "Cuadro"
)

# Renombrar columnas clave
imacec_raw <- imacec_raw %>%
  rename(
    periodo          = `Periodo`,
    imacec           = `1. Imacec`,
    imacec_no_minero = `7. Imacec no minero`
  )

# 1.2 Construir fecha, año y mes
imacec_mensual <- imacec_raw %>%
  mutate(
    fecha = as.Date(periodo),
    year  = year(fecha),
    month = month(fecha)
  ) %>%
  filter(!is.na(fecha)) %>%
  filter(year >= 2008, year <= 2022) %>%
  arrange(fecha)

# 1.3 Pasar a frecuencia trimestral
imacec_tri <- imacec_mensual %>%
  mutate(
    trimestre = quarter(fecha)   # 1,2,3,4
  ) %>%
  group_by(year, trimestre) %>%
  summarise(
    imacec           = mean(imacec, na.rm = TRUE),
    imacec_no_minero = mean(imacec_no_minero, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    t_index = year + trimestre / 10
  )

# 1.4 Reescalar a base 2018 = 100
base_imacec_total <- imacec_tri %>%
  filter(year == 2018) %>%
  summarise(base = mean(imacec, na.rm = TRUE)) %>%
  pull(base)

base_imacec_nm <- imacec_tri %>%
  filter(year == 2018) %>%
  summarise(base = mean(imacec_no_minero, na.rm = TRUE)) %>%
  pull(base)

imacec_tri <- imacec_tri %>%
  mutate(
    imacec_indice           = imacec / base_imacec_total * 100,
    imacec_no_minero_indice = imacec_no_minero / base_imacec_nm * 100
  )

# 1.5 Guardar tablas de IMACEC (por si las necesitas en otros scripts)
write_csv(
  imacec_tri %>% select(year, trimestre, t_index, imacec_indice),
  here("auditoria_variables", "tablas", "imacec_trimestral_indice_2018_100.csv")
)

write_csv(
  imacec_tri %>% select(year, trimestre, t_index, imacec_no_minero_indice),
  here("auditoria_variables", "tablas", "imacec_no_minero_trimestral_indice_2018_100.csv")
)

# ---------------------------------------------------------------
# 2) EMPLEO TRIMESTRAL E ÍNDICE (OCUPADOS)
# ---------------------------------------------------------------

# 2.1 Leer tabla de desempleo que ya construiste
desemp_tri <- read_csv(
  here("auditoria_variables", "tablas", "resumen_trimestral_desempleo_2008_2022.csv")
)

# 2.2 Serie trimestral de empleo (ocupados)
empleo_tri <- desemp_tri %>%
  transmute(
    year      = year_final,
    trimestre = as.integer(substr(tri_fix, 2, 2)),  # "T1" → 1
    t_index,
    ocupados
  ) %>%
  filter(year >= 2008, year <= 2022)

# 2.3 Índice de empleo base 2018 = 100
empleo_base_2018 <- empleo_tri %>%
  filter(year == 2018) %>%
  summarise(base = mean(ocupados, na.rm = TRUE)) %>%
  pull(base)

empleo_tri <- empleo_tri %>%
  mutate(
    empleo_indice = ocupados / empleo_base_2018 * 100
  )

# (Opcional) Guardar índice de empleo
write_csv(
  empleo_tri,
  here("auditoria_variables", "tablas", "empleo_trimestral_indice_2018_100.csv")
)

# ---------------------------------------------------------------
# 3) ÍNDICE DE PRODUCTIVIDAD DEL TRABAJO
#    a) Con IMACEC total
#    b) Con IMACEC no minero
# ---------------------------------------------------------------

# Nos aseguramos que trimestre es numérico en IMACEC
imacec_tri <- imacec_tri %>%
  mutate(trimestre = as.integer(trimestre))

# 3.1 Productividad con IMACEC TOTAL
prod_total <- imacec_tri %>%
  select(year, trimestre, t_index, imacec_indice) %>%
  left_join(
    empleo_tri %>% select(year, trimestre, t_index, empleo_indice),
    by = c("year", "trimestre", "t_index")
  ) %>%
  mutate(
    prod_raw = imacec_indice / empleo_indice
  )

prod_base_2018_total <- prod_total %>%
  filter(year == 2018) %>%
  summarise(base = mean(prod_raw, na.rm = TRUE)) %>%
  pull(base)

prod_total <- prod_total %>%
  mutate(prod_indice = prod_raw / prod_base_2018_total * 100)

write_csv(
  prod_total,
  here("auditoria_variables", "tablas", "indice_productividad_trimestral_2008_2022.csv")
)

# 3.2 Productividad con IMACEC NO MINERO
prod_no_minero <- imacec_tri %>%
  select(year, trimestre, t_index, imacec_no_minero_indice) %>%
  left_join(
    empleo_tri %>% select(year, trimestre, t_index, empleo_indice),
    by = c("year", "trimestre", "t_index")
  ) %>%
  mutate(
    prod_raw = imacec_no_minero_indice / empleo_indice
  )

prod_base_2018_nm <- prod_no_minero %>%
  filter(year == 2018) %>%
  summarise(base = mean(prod_raw, na.rm = TRUE)) %>%
  pull(base)

prod_no_minero <- prod_no_minero %>%
  mutate(prod_indice = prod_raw / prod_base_2018_nm * 100)

write_csv(
  prod_no_minero,
  here("auditoria_variables", "tablas", "indice_productividad_no_minera_2008_2022.csv")
)

message(">>> Script completado: IMACEC, empleo e índices de productividad (total y no minero) creados correctamente.")

# ---------------------------------------------------------------
# 4) GRÁFICOS DE LA PRODUCTIVIDAD TRIMESTRAL
# ---------------------------------------------------------------

library(ggplot2)

# Creamos carpeta de gráficos si no existe
dir.create(here("auditoria_variables", "graficos"), showWarnings = FALSE)

# 4.1. Agregar fecha trimestral (usamos el último mes del trimestre: 3, 6, 9, 12)
prod_total_plot <- prod_total %>%
  mutate(
    fecha_tri = lubridate::ymd(paste0(year, "-", trimestre * 3, "-01"))
  )

prod_no_minero_plot <- prod_no_minero %>%
  mutate(
    fecha_tri = lubridate::ymd(paste0(year, "-", trimestre * 3, "-01"))
  )

# 4.2. Data en formato largo para comparar productividad total vs no minera
prod_long <- bind_rows(
  prod_total_plot %>%
    transmute(year, trimestre, fecha_tri, tipo = "Total", indice = prod_indice),
  prod_no_minero_plot %>%
    transmute(year, trimestre, fecha_tri, tipo = "No minera", indice = prod_indice)
)

# 4.3. Gráfico: índice de productividad (IMACEC total)
g_prod_total <- ggplot(prod_total_plot,
                       aes(x = fecha_tri, y = prod_indice)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Índice de productividad laboral (IMACEC total)",
    subtitle = "Base 2018 = 100, 2008–2022",
    x = "Año",
    y = "Índice (2018 = 100)"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = here("auditoria_variables", "graficos",
                  "G1_productividad_total_imacec_total.pdf"),
  plot = g_prod_total,
  width = 7, height = 4.5
)

# 4.4. Gráfico: índice de productividad (IMACEC no minero)
g_prod_nm <- ggplot(prod_no_minero_plot,
                    aes(x = fecha_tri, y = prod_indice)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Índice de productividad laboral (IMACEC no minero)",
    subtitle = "Base 2018 = 100, 2008–2022",
    x = "Año",
    y = "Índice (2018 = 100)"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = here("auditoria_variables", "graficos",
                  "G2_productividad_total_imacec_no_minero.pdf"),
  plot = g_prod_nm,
  width = 7, height = 4.5
)

# 4.5. Gráfico combinado: productividad total vs no minera
g_prod_both <- ggplot(prod_long,
                      aes(x = fecha_tri, y = indice, color = tipo)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Productividad laboral: IMACEC total vs no minero",
    subtitle = "Índices trimestrales, base 2018 = 100",
    x = "Año",
    y = "Índice (2018 = 100)",
    color = "Serie"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = here("auditoria_variables", "graficos",
                  "G3_productividad_total_vs_no_minera.pdf"),
  plot = g_prod_both,
  width = 7, height = 4.5
)

message(">>> Gráficos de productividad guardados en auditoria_variables/graficos")

# ---------------------------------------------------------------
# 4) GRÁFICO: IMACEC TOTAL vs EMPLEO (Índices base 2018 = 100)
# ---------------------------------------------------------------

library(ggplot2)
library(tidyr)   # para pivot_longer

# Carpeta de gráficos
dir.create(here("auditoria_variables", "graficos"), showWarnings = FALSE)

# 4.1 Unir IMACEC total y empleo por año-trimestre
empleo_imacec <- imacec_tri %>%
  select(year, trimestre, t_index, imacec_indice) %>%
  left_join(
    empleo_tri %>% select(year, trimestre, t_index, empleo_indice),
    by = c("year", "trimestre", "t_index")
  ) %>%
  # fecha del trimestre (usamos el último mes del trimestre: 3, 6, 9, 12)
  mutate(
    fecha_tri = lubridate::ymd(paste0(year, "-", trimestre * 3, "-01"))
  ) %>%
  arrange(fecha_tri)

# 4.2 Pasar a formato largo para graficar ambas series
empleo_imacec_long <- empleo_imacec %>%
  pivot_longer(
    cols      = c(imacec_indice, empleo_indice),
    names_to  = "serie",
    values_to = "indice"
  ) %>%
  mutate(
    serie = dplyr::recode(
      serie,
      imacec_indice  = "IMACEC total (índice)",
      empleo_indice  = "Empleo (ocupados, índice)"
    )
  )

# 4.3 Gráfico comparativo
g_empleo_imacec <- ggplot(empleo_imacec_long,
                          aes(x = fecha_tri, y = indice, color = serie)) +
  geom_line(linewidth = 0.8) +
  labs(
    title    = "IMACEC total vs Empleo (ocupados)",
    subtitle = "Índices trimestrales, base 2018 = 100, 2008–2022",
    x        = "Año",
    y        = "Índice (2018 = 100)",
    color    = "Serie"
  ) +
  theme_minimal(base_size = 12)

# Guardar en PDF
ggsave(
  filename = here("auditoria_variables", "graficos",
                  "G0_imacec_total_vs_empleo_indice_2018_100.pdf"),
  plot   = g_empleo_imacec,
  width  = 7,
  height = 4.5
)

message(">>> Gráfico IMACEC vs empleo guardado en auditoria_variables/graficos")
