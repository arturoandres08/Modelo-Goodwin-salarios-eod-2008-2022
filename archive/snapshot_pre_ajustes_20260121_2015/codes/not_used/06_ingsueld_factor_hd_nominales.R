# ===============================================================
# Exporta ingsueld y factor_hd (NOMINALES) desde EOD
# Periodo: 2008–2022
# Salida:
#   auditoria_variables/tablas/ingsueld_factorhd_nominal_2008_2022.{csv,xlsx}
#   auditoria_variables/tablas/resumen_nominal_ingsueld_por_trimestre_2008_2022.csv
# ===============================================================

# ---- 0) Paquetes + helpers ----
pkgs <- c("tidyverse","haven","fs","janitor","stringr","lubridate","here","writexl")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

# Usa las utilidades de rutas/guardado
source(here("scripts","00_helpers_paths.R"))

# ---- 1) Rutas ----
dir_eod  <- here("data","eod")
out_dir  <- here("auditoria_variables","tablas")
fs::dir_create(out_dir)

# ---- 2) Utilidades ----
mes_map <- c(
  "enero"="01","febrero"="02","marzo"="03","abril"="04","mayo"="05","junio"="06",
  "julio"="07","agosto"="08","septiembre"="09","setiembre"="09","octubre"="10",
  "noviembre"="11","diciembre"="12",
  "ene"="01","feb"="02","mar"="03","abr"="04","may"="05","jun"="06",
  "jul"="07","ago"="08","sep"="09","oct"="10","nov"="11","dic"="12"
)

tri_map <- c("01"="T1","02"="T1","03"="T1",
             "04"="T2","05"="T2","06"="T2",
             "07"="T3","08"="T3","09"="T3",
             "10"="T4","11"="T4","12"="T4")

to_trimester <- function(m) paste0("T", ceiling(as.integer(m)/3))

infer_year <- function(x){
  y <- stringr::str_extract(basename(x), "(19|20)\\d{2}")
  as.integer(y)
}

infer_month <- function(x){
  f <- tolower(basename(x))
  # busca palabras de mes
  hit <- names(mes_map)[stringr::str_detect(
    f, paste0("\\b(", paste(names(mes_map), collapse="|"), ")\\b")
  )]
  if (length(hit)) return(mes_map[hit[1]])
  # fallback: _MM_ o -MM-
  mm <- stringr::str_match(f, "[^0-9]((0[1-9])|(1[0-2]))[^0-9]")[,2]
  if (length(mm) && !is.na(mm)) return(mm)
  NA_character_
}

# patrones generales (ajusta si tus nombres son distintos)
cand_ingreso <- c("ing.*suel", "ing.*sal", "suel.*liqu", "suel.*mono",
                  "salario", "ingreso.*labor")
cand_peso    <- c("^pond$", "ponder", "factor", "fact.*exp", "peso", "w[0-9]*")

pick_first <- function(nms, patterns){
  for (pat in patterns){
    idx <- stringr::str_which(nms, stringr::regex(pat, ignore_case = TRUE))
    if (length(idx)) return(nms[idx[1]])
  }
  NA_character_
}

# ---- 3) Enumerar archivos y filtrar 2008–2022 ----
sav_files <- fs::dir_ls(dir_eod, regexp="\\.sav$", type="file", recurse = FALSE)
stopifnot(length(sav_files) > 0)

files_tbl <- tibble::tibble(path = sav_files) %>%
  mutate(
    year  = purrr::map_int(path, infer_year),
    month = purrr::map_chr(path, infer_month),
    month = ifelse(is.na(month) | month == "", NA_character_, month),
    month = as.integer(month),
    month_chr = ifelse(is.na(month), NA_character_,
                       stringr::str_pad(month, 2, pad = "0")),
    trimestre = ifelse(is.na(month_chr), NA_character_, unname(tri_map[month_chr]))
  ) %>%
  filter(!is.na(year), dplyr::between(year, 2008, 2022)) %>%
  arrange(year, month, path)

if (nrow(files_tbl) == 0) stop("No se encontraron .sav entre 2008–2022 en data/eod/")

# ---- 4) Procesar y extraer columnas ----
out_list <- vector("list", nrow(files_tbl))

for (i in seq_len(nrow(files_tbl))){
  f <- files_tbl$path[i]
  yr <- files_tbl$year[i]
  mm <- files_tbl$month[i]
  
  df <- tryCatch(haven::read_sav(f, user_na = TRUE), error = function(e) NULL)
  if (is.null(df)) next
  
  nms <- names(df)
  var_ing <- pick_first(nms, cand_ingreso)
  var_w   <- pick_first(nms, cand_peso)
  
  if (is.na(var_ing) || is.na(var_w)) next
  
  x <- suppressWarnings(as.numeric(df[[var_ing]]))
  w <- suppressWarnings(as.numeric(df[[var_w]]))
  
  # limpieza mínima: negativos imposibles a NA; pesos <=0 a NA
  x[x < 0] <- NA
  w[w <= 0] <- NA
  
  mm_chr <- if (!is.na(mm)) stringr::str_pad(mm, 2, pad = "0") else NA_character_
  
  # construir tibble largo
  out_list[[i]] <- tibble::tibble(
    year       = yr,
    month      = as.integer(ifelse(is.na(mm), NA, mm)),
    trimestre  = ifelse(is.na(mm_chr), NA_character_, unname(tri_map[mm_chr])),
    archivo    = fs::path_file(f),
    ingsueld   = x,
    factor_hd  = w
  )
}

ings_nom <- dplyr::bind_rows(out_list) %>%
  mutate(
    trimestre = dplyr::coalesce(trimestre, to_trimester(month)),
    year      = as.integer(year),
    month     = as.integer(month)
  ) %>%
  filter(!is.na(ingsueld) | !is.na(factor_hd)) %>%
  arrange(year, month, archivo)

# ---- 5) Guardar resultados ----
f_csv  <- here("auditoria_variables","tablas","ingsueld_factorhd_nominal_2008_2022.csv")
readr::write_csv(ings_nom, f_csv)

# Si el volumen es razonable (< 1,000,000 filas), también a Excel:
if (nrow(ings_nom) <= 1e6) {
  f_xlsx <- here("auditoria_variables","tablas","ingsueld_factorhd_nominal_2008_2022.xlsx")
  writexl::write_xlsx(ings_nom, f_xlsx)
  message("✔ Guardado CSV y XLSX:\n- ", f_csv, "\n- ", f_xlsx)
} else {
  message("✔ Guardado CSV (demasiadas filas para Excel):\n- ", f_csv)
}

# ---- 6) Resumen rápido para chequeo ----
resumen <- ings_nom %>%
  filter(!is.na(ingsueld), !is.na(factor_hd)) %>%
  group_by(year, trimestre) %>%
  summarise(
    n_obs      = dplyr::n(),
    wmean_nom  = sum(ingsueld * factor_hd, na.rm = TRUE) / sum(factor_hd, na.rm = TRUE),
    .groups = "drop"
  )

save_table(resumen, "resumen_nominal_ingsueld_por_trimestre_2008_2022")

message("✔ Listo. Datos nominales exportados a auditoria_variables/tablas/")

# ===============================================================
# Auditoría visual de observaciones: ingsueld & factor_hd (2008–2022)
# Lee:  auditoria_variables/tablas/ingsueld_factorhd_nominal_2008_2022.{xlsx,csv}
# Sale: auditoria_variables/graficos/*.png  + muestras/flags en tablas/
# ===============================================================

# 0) Paquetes + helpers
pkgs <- c("tidyverse","readxl","janitor","scales","here","fs","writexl")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

source(here("scripts","00_helpers_paths.R"))  # save_plot(), save_table(), here()

# 1) Entradas -------------------------------------------------------------------
dir_tab <- here("auditoria_variables","tablas")
fs::dir_create(dir_tab)

f_xlsx <- here("auditoria_variables","tablas","ingsueld_factorhd_nominal_2008_2022.xlsx")
f_csv  <- here("auditoria_variables","tablas","ingsueld_factorhd_nominal_2008_2022.csv")

stopifnot(file.exists(f_xlsx) || file.exists(f_csv))

raw <- if (file.exists(f_xlsx)) {
  readxl::read_xlsx(f_xlsx)
} else {
  readr::read_csv(f_csv, show_col_types = FALSE)
} %>% 
  janitor::clean_names()

# 2) Limpieza mínima ------------------------------------------------------------
# Asegura tipos y crea índice temporal
to_trimester <- function(m) paste0("T", ceiling(as.integer(m)/3))
q_num <- function(tri) match(tri, c("T1","T2","T3","T4"))

dat <- raw %>%
  mutate(
    year      = as.integer(year),
    month     = as.integer(month),
    trimestre = dplyr::coalesce(trimestre, to_trimester(month)),
    ingsueld  = suppressWarnings(as.numeric(ingsueld)),
    factor_hd = suppressWarnings(as.numeric(factor_hd)),
    t_index   = year + q_num(trimestre)/10
  ) %>%
  filter(year >= 2008, year <= 2022,
         !is.na(ingsueld), !is.na(factor_hd),
         factor_hd > 0, ingsueld >= 0)

# 3) Chequeos rápidos -----------------------------------------------------------
# a) conteos por año-trimestre
cont <- dat %>% count(year, trimestre, name = "n_obs") %>% arrange(year, trimestre)
save_table(cont, "conteos_por_trimestre_2008_2022")

# b) medias ponderadas vs simples por trimestre (para ver diferencias)
res_trimestral <- dat %>%
  group_by(year, trimestre) %>%
  summarise(
    media_pond = sum(ingsueld * factor_hd, na.rm = TRUE) / sum(factor_hd, na.rm = TRUE),
    media_simple = mean(ingsueld, na.rm = TRUE),
    p50 = median(ingsueld, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(t_index = year + q_num(trimestre)/10)

save_table(res_trimestral, "medias_por_trimestre_nominal_2008_2022")

# 4) Gráficos de observaciones --------------------------------------------------
# 4.1 Histograma/densidad (ponderada)
g_hist <- ggplot(dat, aes(ingsueld)) +
  geom_histogram(aes(weight = factor_hd), bins = 80, fill = "#2E86AB", alpha = .7) +
  scale_x_continuous(labels = comma) +
  labs(title = "Distribución de ingsueld (ponderada por factor_hd)",
       x = "Ingreso por sueldo (pesos nominales)", y = "Frecuencia ponderada") +
  theme_minimal(12)
save_plot(g_hist, "obs_dist_ingsueld_hist_ponderado")

g_dens <- ggplot(dat, aes(ingsueld, weight = factor_hd)) +
  geom_density(color = "#2E86AB", linewidth = 1) +
  scale_x_continuous(labels = comma) +
  labs(title = "Densidad ponderada de ingsueld", x = "Pesos", y = "Densidad ponderada") +
  theme_minimal(12)
save_plot(g_dens, "obs_densidad_ingsueld_ponderada")

# 4.2 Lo mismo en escala log (para colas)
g_hist_log <- ggplot(dat, aes(ingsueld)) +
  geom_histogram(aes(weight = factor_hd), bins = 80, fill = "#6C757D", alpha=.8) +
  scale_x_log10(labels = comma) +
  labs(title = "Distribución de ingsueld (escala log, ponderada)",
       x = "Ingreso (log10)", y = "Frecuencia ponderada") +
  theme_minimal(12)
save_plot(g_hist_log, "obs_dist_ingsueld_hist_log_ponderado")

# 4.3 Dispersión de pesos vs ingreso (para ver pesos extremos)
g_scatter_w <- ggplot(dat, aes(factor_hd, ingsueld)) +
  geom_point(alpha = .15, color = "#C0392B") +
  scale_x_log10(labels = comma) + scale_y_log10(labels = comma) +
  labs(title = "Scatter factor_hd vs ingsueld (log-log)",
       x = "factor_hd (log)", y = "ingsueld (log)") +
  theme_minimal(12)
save_plot(g_scatter_w, "obs_scatter_factor_vs_ingsueld_log")

# 4.4 Boxplots por año (sin ponderar, útil para forma de la distribución)
g_box_year <- dat %>%
  mutate(year = factor(year)) %>%
  ggplot(aes(year, ingsueld)) +
  geom_boxplot(outlier.alpha = .2, fill = "#2E86AB") +
  scale_y_log10(labels = comma) +
  labs(title = "Boxplots de ingsueld por año (escala log)",
       x = "Año", y = "Ingreso por sueldo (log)") +
  theme_minimal(12)
save_plot(g_box_year, "obs_boxplots_anuales_log")

# 4.5 Serie temporal: media ponderada vs simple
g_ts_means <- res_trimestral %>%
  select(t_index, media_pond, media_simple) %>%
  pivot_longer(-t_index, names_to = "serie", values_to = "valor") %>%
  mutate(serie = recode(serie,
                        media_pond = "Media ponderada",
                        media_simple = "Media simple")) %>%
  ggplot(aes(t_index, valor, color = serie)) +
  geom_line(linewidth = .9) +
  scale_y_continuous(labels = comma) +
  labs(title = "Ingreso nominal: media ponderada vs media simple",
       x = "Año (trimestre)", y = "Pesos", color = NULL) +
  theme_minimal(12) +
  theme(legend.position = "bottom")
save_plot(g_ts_means, "obs_media_pond_vs_simple")

# 5) Outliers y muestras --------------------------------------------------------
# Umbrales por año (p1–p99) para marcar posibles outliers
thr <- dat %>%
  group_by(year) %>%
  summarise(p01 = quantile(ingsueld, .01, na.rm = TRUE),
            p99 = quantile(ingsueld, .99, na.rm = TRUE),
            .groups = "drop")

dat_flag <- dat %>%
  left_join(thr, by = "year") %>%
  mutate(flag_outlier = ingsueld < p01 | ingsueld > p99)

# Conteo de outliers por año
flag_count <- dat_flag %>%
  group_by(year) %>%
  summarise(
    n_total = n(),
    n_out   = sum(flag_outlier, na.rm = TRUE),
    pct_out = 100*n_out/n_total,
    .groups = "drop"
  )
save_table(flag_count, "flags_outliers_p01_p99_por_anio")

# Gráfico de % outliers por año
g_out <- ggplot(flag_count, aes(year, pct_out)) +
  geom_col(fill = "#C0392B") +
  labs(title = "% de observaciones fuera de [p1, p99] por año",
       x = "Año", y = "% de outliers (p1–p99)") +
  theme_minimal(12)
save_plot(g_out, "obs_porcentaje_outliers_anual")

# 6) Muestras para inspección manual --------------------------------
set.seed(123)

# 6.1) 0.5% de la base completa
muestra_05 <- dat %>%
  dplyr::slice_sample(prop = 0.005)

# 6.2) Hasta 5.000 observaciones de outliers (SIN agrupar)
muestra_05_flag <- dat_flag %>%
  dplyr::filter(flag_outlier) %>%
  { dplyr::slice_sample(., n = min(5000L, nrow(.))) }

# Guardar
save_table(muestra_05,      "muestra_0p5_por_ciento_observaciones")
save_table(muestra_05_flag, "muestra_outliers_hasta_5000")

# 7) Heatmap de conteos por año-mes (cobertura de archivos) ---------------------
heat_counts <- dat %>%
  count(year, month) %>%
  mutate(month = factor(month, levels = 1:12)) %>%
  ggplot(aes(month, factor(year), fill = n)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Cobertura de observaciones por año–mes",
       x = "Mes", y = "Año", fill = "N obs") +
  theme_minimal(12)
save_plot(heat_counts, "obs_heatmap_cobertura_anio_mes")

message("✔ Auditoría visual lista. Revisa auditoria_variables/{graficos,tablas}.")
