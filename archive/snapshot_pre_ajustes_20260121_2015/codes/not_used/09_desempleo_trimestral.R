# ===============================================================
# 09_desempleo_trimestral.R
# Serie de tasa de desempleo trimestral (EOD 2008–2022)
# ===============================================================

library(dplyr)
library(purrr)
library(stringr)
library(haven)
library(janitor)
library(fs)
library(here)

# Si tienes helpers de rutas, los cargamos (no es obligatorio)
if (file.exists(here("scripts", "00_helpers_paths.R"))) {
  source(here("scripts", "00_helpers_paths.R"))
}

# ---------------------------------------------------------------
# 0) Directorio de EOD
# ---------------------------------------------------------------
dir_eod <- here("data", "eod")
stopifnot(dir_exists(dir_eod))

eod_files <- dir_ls(
  dir_eod,
  regexp  = "\\.(sav|dta)$",
  type    = "file",
  recurse = FALSE
)
stopifnot(length(eod_files) > 0)

# ---------------------------------------------------------------
# 1) Helpers (muy parecidos a los de 07_recalculo_masa_salarial)
# ---------------------------------------------------------------

cand_peso <- c("^factor_hd$", "factor", "pond", "ponder", "w[0-9]*")
cand_mes  <- c(
  "^mes$", "mes_lev$", "meslev$", "mes_enc$",
  "mes_muestra$", "mes_levantamiento$"
)
cand_year <- c(
  "^year$", "^agno$", "^ano$", "^anio$",
  "year_enc$", "agno_enc$", "ano_enc$", "anio_enc$"
)
cand_sit  <- c("^sitocup1$")  # en tu diccionario aparece siempre así

pick_first <- function(nms, patterns) {
  for (p in patterns) {
    hit <- str_which(nms, regex(p, ignore_case = TRUE))
    if (length(hit)) return(nms[hit[1]])
  }
  NA_character_
}

read_sav_safely <- function(f) {
  base <- basename(f)
  df1 <- try(read_sav(f, user_na = TRUE), silent = TRUE)
  if (!inherits(df1, "try-error")) return(df1)
  
  df2 <- try(read_sav(f, user_na = TRUE, encoding = "latin1"),
             silent = TRUE)
  if (!inherits(df2, "try-error")) {
    message("✔ ", base, " leído con encoding = 'latin1'")
    return(df2)
  }
  
  message("✖ No se pudo leer: ", base)
  NULL
}

tri_from_month <- function(mm) {
  mm <- suppressWarnings(as.integer(mm))
  ifelse(is.na(mm), NA_character_, paste0("T", ceiling(mm / 3)))
}

q_num <- function(tri) match(toupper(tri), c("T1", "T2", "T3", "T4"))

infer_month_from_filename <- function(fname) {
  f <- tolower(basename(fname))
  # buscar 01..12 en el nombre
  mm <- str_match(f, "([01][0-9])")[, 2]
  if (!is.na(mm) && mm %in% sprintf("%02d", 1:12)) return(mm)
  
  # códigos t1..t4 como último recurso
  if (str_detect(f, "t1")) return("03")
  if (str_detect(f, "t2")) return("06")
  if (str_detect(f, "t3")) return("09")
  if (str_detect(f, "t4")) return("12")
  NA_character_
}

infer_month_vec <- function(x) vapply(x, infer_month_from_filename, character(1))

# ---------------------------------------------------------------
# 2) Extracción mínima: sitocup1 + factor_hd + año/mes
# ---------------------------------------------------------------

extract_desemp <- function(f) {
  base <- basename(f)
  message("Leyendo: ", base)
  ext  <- tolower(path_ext(f))
  
  df <- if (ext == "sav") {
    read_sav_safely(f)
  } else if (ext == "dta") {
    read_dta(f)
  } else {
    message("  → extensión no soportada: ", ext)
    return(NULL)
  }
  if (is.null(df)) return(NULL)
  
  nms   <- names(df)
  v_sit <- pick_first(nms, cand_sit)
  v_w   <- pick_first(nms, cand_peso)
  v_mes <- pick_first(nms, cand_mes)
  v_y   <- pick_first(nms, cand_year)
  
  if (is.na(v_sit) || is.na(v_w)) {
    message("  → se omite (falta sitocup1 o factor_hd): ", base)
    return(NULL)
  }
  
  # Año desde columna + nombre archivo
  year_col_raw <- if (!is.na(v_y)) df[[v_y]] else NA
  year_lab_chr <- suppressWarnings(as.character(as_factor(year_col_raw)))
  year_lab_num <- suppressWarnings(readr::parse_integer(
    year_lab_chr, na = c("", "NA")
  ))
  year_num <- suppressWarnings(as.numeric(year_col_raw))
  year_col <- dplyr::coalesce(year_lab_num, year_num)
  year_col[year_col < 1900 | year_col > 2100] <- NA_integer_
  
  year_file <- suppressWarnings(
    as.integer(str_extract(base, "(19|20)\\d{2}"))
  )
  
  mes_val <- suppressWarnings(
    as.integer(if (!is.na(v_mes)) df[[v_mes]] else NA)
  )
  
  tibble(
    archivo   = base,
    sitocup1  = suppressWarnings(as.numeric(df[[v_sit]])),
    factor_hd = suppressWarnings(as.numeric(df[[v_w]])),
    mes       = mes_val,
    year      = dplyr::coalesce(year_col, year_file)
  )
}

desemp_nom <- map_dfr(eod_files, extract_desemp) %>%
  clean_names()

message("✔ Extracción lista: ", nrow(desemp_nom), " filas")

# ---------------------------------------------------------------
# 3) Normalizar mes / trimestre y recortar 2008–2022
# ---------------------------------------------------------------

desemp0 <- desemp_nom %>%
  mutate(
    archivo   = tolower(archivo),
    year_file = suppressWarnings(
      as.integer(str_extract(archivo, "(19|20)\\d{2}"))
    ),
    year_final = coalesce(
      if_else(between(year, 1900, 2100), year, NA_integer_),
      year_file
    ),
    month_chr = ifelse(
      !is.na(mes),
      stringr::str_pad(mes, 2, pad = "0"),
      infer_month_vec(archivo)
    ),
    month    = suppressWarnings(as.integer(month_chr)),
    tri_fix  = tri_from_month(month)
  ) %>%
  filter(
    !is.na(factor_hd), factor_hd > 0,
    !is.na(sitocup1),
    !is.na(year_final), between(year_final, 2008, 2022),
    !is.na(tri_fix)
  )

# ---------------------------------------------------------------
# 4) Clasificación laboral y tasa de desempleo
# ---------------------------------------------------------------
# Códigos (según exploración):
# 1  En trabajo
# 8  Trabajador sin remuneración        → ocupados
# 2  Cesante
# 7  Busca trabajo por primera vez      → desocupados
# 3,4,5,6,9  inactivos / fuera fuerza de trabajo

desemp_tri <- desemp0 %>%
  mutate(
    ocupado    = sitocup1 %in% c(1, 8),
    desocupado = sitocup1 %in% c(2, 7)
  ) %>%
  group_by(year_final, tri_fix) %>%
  summarise(
    ocupados    = sum(ocupado    * factor_hd, na.rm = TRUE),
    desocupados = sum(desocupado * factor_hd, na.rm = TRUE),
    fuerza_trab = ocupados + desocupados,
    tasa_desemp = 100 * desocupados / fuerza_trab,
    n_obs       = n(),
    .groups     = "drop"
  ) %>%
  mutate(
    t_index = year_final + q_num(tri_fix) / 10
  ) %>%
  arrange(year_final, t_index)

# ---------------------------------------------------------------
# 5) Guardar resultados y gráfico simple
# ---------------------------------------------------------------

dir_out <- here("auditoria_variables", "tablas")
dir_create(dir_out)

readr::write_csv(
  desemp_tri,
  file = path(dir_out, "resumen_trimestral_desempleo_2008_2022.csv")
)

# Gráfico rápido (puedes pulirlo luego)
library(ggplot2)
library(scales)

df_desemp <- read.csv("auditoria_variables/tablas/resumen_trimestral_desempleo_2008_2022.csv")

ggplot(df_desemp, aes(t_index, tasa_desemp)) +
  geom_line(color = "#D55E00", linewidth = 1.2) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Tasa de desempleo trimestral (EOD, 2008–2022)",
    x = "Año (trimestre)",
    y = "Porcentaje"
  ) +
  theme_minimal(base_size = 14)

p_desemp <- df_desemp %>% 
  ggplot(aes(t_index, tasa_desemp)) +
  geom_line(color = "#D55E00", linewidth = 1.2) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Tasa de desempleo trimestral (EOD, 2008–2022)",
    x = "Año (trimestre)", y = "Porcentaje"
  ) +
  theme_minimal(base_size = 14)


save_plot(
  p_desemp,
  "tasa_desempleo_trimestral_2008_2022"
)


