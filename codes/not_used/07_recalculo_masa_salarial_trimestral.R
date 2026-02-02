# ===============================================================
# Extracción + Trimestralización + Winsorización (p99.5) 2008–2022
# ===============================================================

# ---- 0) Paquetes y helpers -----------------------------------
pkgs <- c(
  "tidyverse", "haven", "janitor", "fs", "here", "writexl",
  "stringr", "scales"
)
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install)) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
invisible(lapply(pkgs, library, character.only = TRUE))

# save_plot/save_table (usa helpers si existen)
if (file.exists(here("scripts", "00_helpers_paths.R"))) {
  source(here("scripts", "00_helpers_paths.R"))
} else {
  save_plot <- function(p, name,
                        dir = here("auditoria_variables", "graficos"),
                        width = 10, height = 5, dpi = 150,
                        device = c("png", "pdf")) {
    device <- match.arg(device)
    fs::dir_create(dir)
    f <- fs::path(
      dir,
      paste0(
        gsub("[^a-zA-Z0-9_-]+", "_", tolower(name)),
        ".",
        device
      )
    )
    ggsave(f, p, width = width, height = height,
           dpi = dpi, units = "in", device = device)
    invisible(f)
  }
  
  save_table <- function(x, name,
                         dir = here("auditoria_variables", "tablas")) {
    fs::dir_create(dir)
    f <- fs::path(
      dir,
      paste0(
        gsub("[^a-zA-Z0-9_-]+", "_", tolower(name)),
        ".csv"
      )
    )
    readr::write_csv(x, f)
    invisible(f)
  }
}

dir_eod <- here("data", "eod")
fs::dir_create(here("auditoria_variables", "tablas"))
fs::dir_create(here("auditoria_variables", "graficos"))
stopifnot(fs::dir_exists(dir_eod))

# ---- 1) Detectores de columnas en EOD -------------------------
cand_ingreso <- c(
  "ing.*suel", "ing.*sal", "suel.*liqu", "suel.*mono",
  "salario", "ingreso.*labor"
)
cand_peso <- c("^pond$", "ponder", "factor", "fact.*exp", "peso", "w[0-9]*")
cand_mes <- c(
  "^mes$", "mes_lev$", "meslev$", "mes_enc$", "meslevantamiento$",
  "mes_muestra$", "mes_entrevista$", "mes_levantamiento$"
)
cand_year <- c(
  "^year$", "^ano$", "^agno$", "^anio$",
  "year_enc$", "ano_enc$", "agno_enc$", "anio_enc$",
  "year_var$", "ano_var$", "agno_var$", "anio_var$"
)

pick_first <- function(nms, patterns) {
  for (p in patterns) {
    hit <- stringr::str_which(nms, regex(p, ignore_case = TRUE))
    if (length(hit)) return(nms[hit[1]])
  }
  NA_character_
}

# ---- 1bis) Lectura robusta de .sav (encoding) ----------------
read_sav_safely <- function(f) {
  base <- basename(f)
  msg_pref <- paste0("Archivo ", base, ": ")
  
  # intento 1: encoding por defecto
  df1 <- try(haven::read_sav(f, user_na = TRUE), silent = TRUE)
  if (!inherits(df1, "try-error")) return(df1)
  message("⚠ ", msg_pref, "falló con encoding por defecto: ",
          as.character(df1))
  
  # intento 2: latin1 (necesario para sept/dic 2018)
  df2 <- try(haven::read_sav(f, user_na = TRUE, encoding = "latin1"),
             silent = TRUE)
  if (!inherits(df2, "try-error")) {
    message("✔ ", msg_pref, "leído con encoding = 'latin1'")
    return(df2)
  }
  
  message("✖ ", msg_pref, "no se pudo leer ni con 'latin1'")
  return(NULL)
}

# ---- 2) Extraer de los archivos (.sav y .dta) ----------------
eod_files <- fs::dir_ls(
  dir_eod,
  regexp  = "\\.(sav|dta)$",
  type    = "file",
  recurse = FALSE
)
stopifnot(length(eod_files) > 0)

extract_one <- function(f) {
  base <- basename(f)
  message("Leyendo: ", base)
  ext  <- tolower(fs::path_ext(f))
  
  df <- tryCatch(
    {
      if (ext == "sav") {
        read_sav_safely(f)
      } else if (ext == "dta") {
        haven::read_dta(f)
      } else {
        message("  → extensión no soportada: ", ext)
        NULL
      }
    },
    error = function(e) {
      message("✖ Error crítico leyendo ", base, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(df)) return(NULL)
  
  nms  <- names(df)
  v_ing <- pick_first(nms, cand_ingreso)
  v_w   <- pick_first(nms, cand_peso)
  v_mes <- pick_first(nms, cand_mes)
  v_y   <- pick_first(nms, cand_year)
  
  if (is.na(v_ing) || is.na(v_w)) {
    message("  → se omite ", base,
            " (no se encontró ingreso o factor de expansión)")
    return(NULL)
  }
  
  # Año: combinamos etiqueta y valor numérico
  year_col_raw <- if (!is.na(v_y)) df[[v_y]] else NA
  year_lab_chr <- suppressWarnings(as.character(haven::as_factor(year_col_raw)))
  year_lab_num <- suppressWarnings(readr::parse_integer(
    year_lab_chr, na = c("", "NA")
  ))
  year_num     <- suppressWarnings(as.numeric(year_col_raw))
  year_col     <- dplyr::coalesce(year_lab_num, year_num)
  year_col[year_col < 1900 | year_col > 2100] <- NA_integer_
  
  year_file <- suppressWarnings(
    as.integer(stringr::str_extract(base, "(19|20)\\d{2}"))
  )
  
  mes_val <- suppressWarnings(
    as.integer(if (!is.na(v_mes)) df[[v_mes]] else NA)
  )
  
  tibble(
    archivo   = base,
    ingsueld  = suppressWarnings(as.numeric(df[[v_ing]])),
    factor_hd = suppressWarnings(as.numeric(df[[v_w]])),
    mes       = mes_val,
    year      = dplyr::coalesce(year_col, year_file),
    trimestre = ifelse(!is.na(mes_val),
                       paste0("T", ceiling(mes_val / 3)),
                       NA_character_)
  )
}

ings_nom <- purrr::map_dfr(eod_files, extract_one) %>%
  janitor::clean_names()

# Guardamos extracción “cruda”
save_table(ings_nom, "ingsueld_factorhd_nominal_2008_2022")

if (nrow(ings_nom) < 1e6) {
  writexl::write_xlsx(
    ings_nom,
    here("auditoria_variables", "tablas",
         "ingsueld_factorhd_nominal_2008_2022.xlsx")
  )
}

message("✔ Extracción lista: ", nrow(ings_nom), " filas")

# ---- 3) Normalización meses y trimestres ----------------------
mes_map <- c(
  "enero"="01","febrero"="02","marzo"="03","abril"="04","mayo"="05","junio"="06",
  "julio"="07","agosto"="08","septiembre"="09","setiembre"="09","octubre"="10",
  "noviembre"="11","diciembre"="12","ene"="01","feb"="02","mar"="03","abr"="04",
  "may"="05","jun"="06","jul"="07","ago"="08","sep"="09","oct"="10","nov"="11","dic"="12"
)

tri_from_month <- function(mm) {
  mm <- suppressWarnings(as.integer(mm))
  ifelse(is.na(mm), NA_character_, paste0("T", ceiling(mm / 3)))
}

q_num <- function(tri) match(toupper(tri), c("T1", "T2", "T3", "T4"))

# Inferir mes desde el nombre de archivo
infer_month_from_filename <- function(fname){
  f <- tolower(basename(fname))
  
  # 1) nombre de mes en texto
  pat   <- paste(names(mes_map), collapse = "|")
  m_chr <- stringr::str_extract(f, pat)
  if (!is.na(m_chr)) {
    return(unname(mes_map[m_chr]))
  }
  
  # 2) patrón numérico 01–12
  mm <- stringr::str_match(f, "([01][0-9])")[, 2]
  if (!is.na(mm) && mm %in% sprintf("%02d", 1:12)) {
    return(mm)
  }
  
  # 3) códigos T1..T4 en el nombre
  if (stringr::str_detect(f, "t1")) return("03")
  if (stringr::str_detect(f, "t2")) return("06")
  if (stringr::str_detect(f, "t3")) return("09")
  if (stringr::str_detect(f, "t4")) return("12")
  
  NA_character_
}

infer_month_vec <- function(x) vapply(x, infer_month_from_filename, character(1))

# ---- 3b) Construcción de ing0 con limpieza de outliers --------
ing0 <- ings_nom %>%
  mutate(
    archivo   = tolower(archivo),
    
    # *** Paso CLAVE: eliminar top-codes / errores de ingsueld ***
    # Sólo dejamos ingresos mensuales < 10 millones
    ingsueld  = ifelse(ingsueld > 10000000 | ingsueld < 0,
                       NA_real_, as.numeric(ingsueld)),
    
    factor_hd = suppressWarnings(as.numeric(factor_hd)),
    
    year_col  = suppressWarnings(as.integer(year)),
    year_file = suppressWarnings(
      as.integer(stringr::str_extract(archivo, "(19|20)\\d{2}"))
    ),
    year_final = dplyr::coalesce(
      dplyr::if_else(
        dplyr::between(year_col, 1900L, 2100L),
        year_col,
        NA_integer_
      ),
      year_file
    ),
    month_chr = ifelse(
      !is.na(mes),
      stringr::str_pad(mes, 2, pad = "0"),
      infer_month_vec(archivo)
    ),
    month      = suppressWarnings(as.integer(month_chr)),
    tri_direct = toupper(trimestre),
    tri_from_m = tri_from_month(month),
    tri_final  = dplyr::coalesce(tri_direct, tri_from_m),
    t_index    = year_final + q_num(tri_final) / 10
  )

# ---- 4) Ventana temporal principal (2008–2022) ----------------
dat <- ing0 %>%
  filter(
    !is.na(ingsueld), ingsueld >= 0,
    !is.na(factor_hd), factor_hd > 0,
    !is.na(year_final),
    dplyr::between(year_final, 2008, 2022)
  ) %>%
  arrange(year_final, t_index)

stopifnot(nrow(dat) > 0)

# ===============================================================
# 5) BLOQUE DE DIAGNÓSTICOS (mes y trimestre, 1957–2022)
# ===============================================================

# 5.1 Por archivo
auditoria_mes_trimestre_por_archivo <- ing0 %>%
  group_by(archivo, year_final) %>%
  summarise(
    n_obs       = n(),
    meses_orig  = list(sort(unique(mes[!is.na(mes)]))),
    meses_norm  = list(sort(unique(month[!is.na(month)]))),
    tris_final  = list(sort(unique(tri_final[!is.na(tri_final)]))),
    .groups     = "drop"
  )
save_table(auditoria_mes_trimestre_por_archivo,
           "auditoria_mes_trimestre_por_archivo")

# 5.2 Calendario esperado por año
expected_months <- tibble(year_final = 1957L:2022L) %>%
  rowwise() %>%
  mutate(
    mes = list(
      if (year_final < 1979L) {
        6L                        # un levantamiento (junio)
      } else if (dplyr::between(year_final, 1980L, 1996L)) {
        c(3L, 6L)                 # marzo, junio
      } else {
        c(3L, 6L, 9L, 12L)        # marzo, junio, sept, dic
      }
    )
  ) %>%
  tidyr::unnest_longer(mes) %>%
  mutate(tri_esp = tri_from_month(mes))

# 5.3 Meses efectivamente observados (usando ing0 completo)
actual_months <- ing0 %>%
  filter(!is.na(year_final), !is.na(month)) %>%
  distinct(year_final, month) %>%
  rename(mes = month) %>%
  mutate(tri_obs = tri_from_month(mes))

auditoria_mes_esperado_vs_observado <- full_join(
  expected_months %>% mutate(esperado = TRUE),
  actual_months    %>% mutate(observado = TRUE),
  by = c("year_final", "mes")
) %>%
  mutate(
    esperado  = dplyr::coalesce(esperado,  FALSE),
    observado = dplyr::coalesce(observado, FALSE),
    estado = dplyr::case_when(
      esperado & observado ~ "OK",
      esperado & !observado ~ "FALTA_EN_DATOS",
      !esperado & observado ~ "EXTRA_EN_DATOS",
      TRUE ~ "NINGUNO"
    )
  ) %>%
  arrange(year_final, mes)

save_table(auditoria_mes_esperado_vs_observado,
           "auditoria_mes_esperado_vs_observado")

# 5.4 Auditoría por trimestre
auditoria_trimestre_esperado_vs_observado <- auditoria_mes_esperado_vs_observado %>%
  mutate(trimestre = tri_from_month(mes)) %>%
  group_by(year_final, trimestre) %>%
  summarise(
    esperado  = any(estado %in% c("OK", "FALTA_EN_DATOS")),
    observado = any(estado %in% c("OK", "EXTRA_EN_DATOS")),
    estado = dplyr::case_when(
      esperado & observado ~ "OK",
      esperado & !observado ~ "FALTA_EN_DATOS",
      !esperado & observado ~ "EXTRA_EN_DATOS",
      TRUE ~ "NINGUNO"
    ),
    .groups = "drop"
  ) %>%
  arrange(year_final, trimestre)

save_table(auditoria_trimestre_esperado_vs_observado,
           "auditoria_trimestre_esperado_vs_observado")

# ===============================================================
# 6) Trimestre robusto común (2008–2022)
# ===============================================================

# 6.1 Recuperar columnas de mes / trimestre desde `dat`
cols_mes <- intersect(c("month", "mes", "month_chr"), names(dat))
cols_tri <- intersect(
  c("tri_final", "trimestre", "tri", "tri_fix", "tri_aud", "tri_det"),
  names(dat)
)

if (length(cols_mes) == 0) {
  mes_chr <- rep(NA_character_, nrow(dat))
} else {
  mes_chr <- dplyr::coalesce(
    !!!lapply(cols_mes, \(nm) as.character(dat[[nm]]))
  )
}

if (length(cols_tri) == 0) {
  tri_raw_vec <- rep(NA_character_, nrow(dat))
} else {
  tri_raw_vec <- dplyr::coalesce(
    !!!lapply(cols_tri, \(nm) as.character(dat[[nm]]))
  )
}

dat_0822 <- dat %>%
  mutate(
    mes_fix = suppressWarnings(as.integer(mes_chr)),
    tri_raw = tri_raw_vec,
    tri_fix = toupper(dplyr::coalesce(
      tri_raw,
      tri_from_month(mes_fix),
      tri_from_month(infer_month_vec(archivo))
    ))
  )

# Base limpia común a ambas versiones (con y sin winsor)
dat_ok <- dat_0822 %>%
  filter(
    !is.na(ingsueld), ingsueld >= 0,
    !is.na(factor_hd), factor_hd > 0,
    !is.na(year_final), !is.na(tri_fix)
  )

# ===============================================================
# 7) Versión A: CON winsor p99.5 por año
# ===============================================================

thr <- dat_ok %>%
  group_by(year_final) %>%
  summarise(
    p995 = quantile(ingsueld, 0.995, na.rm = TRUE),
    .groups = "drop"
  )

dat_w <- dat_ok %>%
  left_join(thr, by = "year_final") %>%
  mutate(ingsueld_w = pmin(ingsueld, p995))

res_tri_w <- dat_w %>%
  group_by(year = year_final, trimestre = tri_fix) %>%
  summarise(
    n_obs         = n(),
    masa_salarial = sum(ingsueld_w * factor_hd),
    suma_pesos    = sum(factor_hd),
    media_pond    = masa_salarial / suma_pesos,
    media_simple  = mean(ingsueld_w),
    p99_5         = first(p995),
    .groups       = "drop"
  ) %>%
  mutate(
    t_index  = year + q_num(trimestre) / 10,
    variante = "winsor_p99_5"
  ) %>%
  arrange(year, t_index)

# Guardar versión winsorizada (como venías trabajando)
out_csv_w <- here("auditoria_variables", "tablas",
                  "resumen_trimestral_winsor_2008_2022.csv")
readr::write_csv(res_tri_w, out_csv_w)
save_table(res_tri_w, "resumen_trimestral_winsor_2008_2022")

# ===============================================================
# 8) Versión B: SIN winsor p99.5 (solo tope 10 millones)
# ===============================================================

res_tri_now <- dat_ok %>%
  group_by(year = year_final, trimestre = tri_fix) %>%
  summarise(
    n_obs         = n(),
    masa_salarial = sum(ingsueld * factor_hd),
    suma_pesos    = sum(factor_hd),
    media_pond    = masa_salarial / suma_pesos,
    media_simple  = mean(ingsueld),
    .groups       = "drop"
  ) %>%
  mutate(
    t_index  = year + q_num(trimestre) / 10,
    variante = "sin_winsor"
  ) %>%
  arrange(year, t_index)

out_csv_now <- here("auditoria_variables", "tablas",
                    "resumen_trimestral_sin_winsor_2008_2022.csv")
readr::write_csv(res_tri_now, out_csv_now)
save_table(res_tri_now, "resumen_trimestral_sin_winsor_2008_2022")

# ===============================================================
# 9) Comparación de resultados CON vs SIN winsor
# ===============================================================

res_tri_comp <- bind_rows(res_tri_w, res_tri_now)

# 9.1 Gráfico: medias ponderadas trimestrales (comparación)
p_medias_comp <- res_tri_comp %>%
  ggplot(aes(t_index, media_pond, color = variante)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Ingreso nominal mensual: medias ponderadas trimestrales\ncon y sin winsor p99.5",
    x = "Año (trimestre)", y = "Pesos", color = "Versión"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(
  here("auditoria_variables", "graficos",
       "medias_trimestrales_pond_winsor_vs_sinwinsor.png"),
  p_medias_comp, width = 10, height = 5, dpi = 150
)

# 9.2 Gráfico: masa salarial trimestral (solo versión winsorizada)
p_masa_w <- ggplot(res_tri_w, aes(t_index, masa_salarial)) +
  geom_line(color = "#2E86AB", linewidth = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Masa salarial trimestral (winsorizada al p99.5)",
    x = "Año (trimestre)", y = "Pesos"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  here("auditoria_variables", "graficos",
       "masa_salarial_trimestral_winsorizada.png"),
  p_masa_w, width = 10, height = 5, dpi = 150
)

# 9.3 Gráfico: distribución de ingresos con y sin winsor
set.seed(123)

ingresos_long <- dat_w %>%
  select(year_final, tri_fix, factor_hd, ingsueld, ingsueld_w) %>%
  pivot_longer(
    cols      = c(ingsueld, ingsueld_w),
    names_to  = "variante",
    values_to = "valor"
  ) %>%
  mutate(
    variante = recode(variante,
                      ingsueld   = "sin_winsor",
                      ingsueld_w = "winsor_p99_5")
  )

# Muestra para que el gráfico no sea tan pesado
ingresos_muestra <- ingresos_long %>%
  sample_n(size = min(200000, nrow(.)))

p_dens <- ingresos_muestra %>%
  ggplot(aes(x = valor, colour = variante)) +
  geom_density(adjust = 1.5) +
  scale_x_continuous(
    labels = scales::comma,
    limits = c(0, NA)
  ) +
  labs(
    title = "Distribución de ingresos mensuales\ncon y sin winsor p99.5",
    x = "Ingreso mensual (pesos)",
    y = "Densidad",
    colour = "Versión"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(
  here("auditoria_variables", "graficos",
       "densidad_ingresos_winsor_vs_sinwinsor.png"),
  p_dens, width = 10, height = 5, dpi = 150
)

message("✔ Tablas y gráficos generados para: con y sin winsor p99.5")

# ===============================================================
# 8) INCORPORACIÓN DEL IPC Y CÁLCULO DE SALARIOS REALES
# ===============================================================

library(readxl)
library(lubridate)

# --- 8.1 Leer archivo IPC mensual ------------------------------

ipc_raw <- read_excel(
  here("data", "ipc", "PEM_VAR_IPC_2018_HIST.xlsx"),
  skip = 0
)

# Estructura real del archivo:
# - Columna "Periodo": fecha mensual (POSIXct/Date) 1997-01-01, 1997-02-01, ...
# - Columna "1. IPC General (empalme BCCh)": variación mensual en %

# --- 8.2 Normalización básica ----------------------------------

ipc <- ipc_raw %>%
  # Renombramos columnas a algo manejable
  rename(
    periodo     = Periodo,
    var_mensual = `1. IPC General (empalme BCCh)`
  ) %>%
  mutate(
    # Aseguramos tipo Date
    periodo     = as.Date(periodo),
    year        = year(periodo),
    month       = month(periodo),
    var_mensual = as.numeric(var_mensual)
  ) %>%
  filter(!is.na(year), !is.na(month)) %>%
  arrange(year, month)

# --- 8.3 Construir índice IPC acumulado con base dic-2018 = 100 --

# 1) convertir variación mensual en factor multiplicativo
ipc <- ipc %>%
  mutate(factor = 1 + var_mensual / 100)

# 2) producto acumulado hasta cada mes (orden ya está por year, month)
ipc <- ipc %>%
  mutate(indice_bruto = 100 * cumprod(factor))

# 3) re-escalar para que diciembre 2018 = 100
ipc_base <- ipc %>%
  filter(year == 2018, month == 12) %>%
  pull(indice_bruto)

ipc <- ipc %>%
  mutate(IPC = indice_bruto / ipc_base * 100)

# Guardamos IPC limpio (toda la serie 1997–2022)
save_table(ipc, "ipc_mensual_base2018_100")


# ===============================================================
# 9) UNIR IPC A LOS DATOS INDIVIDUALES Y CALCULAR INGRESO REAL
# ===============================================================

# dat_ok: datos individuales sin winsor
# dat_w : datos individuales con winsor (contiene ingsueld_w)

# --- 9.1 Unir IPC mensual por mes y año ------------------------

dat_real <- dat_ok %>%
  left_join(
    ipc %>% select(year, month, IPC),
    by = c("year_final" = "year", "month" = "month")
  )

dat_real_w <- dat_w %>%
  left_join(
    ipc %>% select(year, month, IPC),
    by = c("year_final" = "year", "month" = "month")
  )

# --- 9.2 Calcular ingreso real mensual -------------------------

dat_real <- dat_real %>%
  mutate(
    ing_real = ingsueld * (100 / IPC)
  )

dat_real_w <- dat_real_w %>%
  mutate(
    ing_real = ingsueld_w * (100 / IPC)
  )


# ===============================================================
# 10) AGREGADOS TRIMESTRALES REALES (winsor y sin winsor)
# ===============================================================

# -- 10.1 sin winsor --
res_tri_real <- dat_real %>%
  group_by(year = year_final, trimestre = tri_fix) %>%
  summarise(
    n_obs              = n(),
    masa_salarial_real = sum(ing_real * factor_hd),
    suma_pesos         = sum(factor_hd),
    media_pond_real    = masa_salarial_real / suma_pesos,
    media_simple_real  = mean(ing_real),
    .groups            = "drop"
  ) %>%
  mutate(t_index = year + q_num(trimestre) / 10)

# -- 10.2 con winsor --
res_tri_real_w <- dat_real_w %>%
  group_by(year = year_final, trimestre = tri_fix) %>%
  summarise(
    n_obs              = n(),
    masa_salarial_real = sum(ing_real * factor_hd),
    suma_pesos         = sum(factor_hd),
    media_pond_real    = masa_salarial_real / suma_pesos,
    media_simple_real  = mean(ing_real),
    .groups            = "drop"
  ) %>%
  mutate(t_index = year + q_num(trimestre) / 10)

# Guardamos tablas
save_table(res_tri_real,   "resumen_trimestral_real_sinwinsor_2008_2022")
save_table(res_tri_real_w, "resumen_trimestral_real_winsor_2008_2022")


# ===============================================================
# 11) GRÁFICOS COMPARATIVOS REAL (winsor vs sin winsor)
# ===============================================================

# --- 11.1 media ponderada real ---
p_real_pond <- bind_rows(
  res_tri_real   %>% mutate(version = "sin_winsor"),
  res_tri_real_w %>% mutate(version = "winsor_p99_5")
) %>%
  ggplot(aes(t_index, media_pond_real, color = version)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Ingreso real mensual: medias ponderadas trimestrales",
    x = "Año (trimestre)", y = "Pesos"
  ) +
  theme_minimal(base_size = 12)

save_plot(p_real_pond, "medias_trimestrales_reales_comparacion")

# --- 11.2 masa salarial real ---
p_masa_real <- res_tri_real_w %>%
  ggplot(aes(t_index, masa_salarial_real)) +
  geom_line(color = "#2E86AB", linewidth = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Masa salarial real trimestral (winsor p99.5)",
    x = "Año (trimestre)", y = "Pesos"
  ) +
  theme_minimal(base_size = 12)

save_plot(p_masa_real, "masa_salarial_real_trimestral_winsor")

# ===============================================================
# 12) Varianza y desviación estándar de los salarios reales
# ===============================================================

res_tri_real <- dat_real %>%
  group_by(year = year_final, trimestre = tri_fix) %>%
  summarise(
    n_obs = n(),
    masa_salarial_real = sum(ing_real * factor_hd),
    suma_pesos = sum(factor_hd),
    media_pond_real = masa_salarial_real / suma_pesos,
    media_simple_real = mean(ing_real),
    
    # 🔹 Nuevos estadísticos
    var_real = var(ing_real, na.rm = TRUE),
    sd_real  = sd(ing_real, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(t_index = year + q_num(trimestre)/10)

res_tri_real_w <- dat_real_w %>%
  group_by(year = year_final, trimestre = tri_fix) %>%
  summarise(
    n_obs = n(),
    masa_salarial_real = sum(ing_real * factor_hd),
    suma_pesos = sum(factor_hd),
    media_pond_real = masa_salarial_real / suma_pesos,
    media_simple_real = mean(ing_real),
    
    # 🔹 Nuevos estadísticos
    var_real = var(ing_real, na.rm = TRUE),
    sd_real  = sd(ing_real, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(t_index = year + q_num(trimestre)/10)

save_table(res_tri_real,   "resumen_trimestral_real_sinwinsor_2008_2022")
save_table(res_tri_real_w, "resumen_trimestral_real_winsor_2008_2022")

# ===============================================================
# 13) Gráficos de varianza y desviación estándar (real, winsor vs sin winsor)
# ===============================================================

library(ggplot2)
library(scales)

# ---- 13.1 Armar base larga con ambas versiones ----
disp_full <- bind_rows(
  res_tri_real   %>% mutate(version = "sin_winsor"),
  res_tri_real_w %>% mutate(version = "winsor_p99_5")
)

# ---- 13.2 Desviación estándar trimestral ----
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

# si tienes save_plot():
save_plot(p_sd_real, "desviacion_estandar_trimestral_real")

# ---- 13.3 Varianza trimestral ----
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

# ---- 13.4 Ribbon ±1 DE alrededor de la media (winsor p99.5) ----
plot_ribbon_real_w <- res_tri_real_w %>%
  ggplot(aes(t_index, media_pond_real)) +
  geom_ribbon(aes(ymin = media_pond_real - sd_real,
                  ymax = media_pond_real + sd_real),
              fill = "#1f77b4", alpha = 0.18) +
  geom_line(color = "#1f77b4", linewidth = 1.1) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Ingreso real promedio y banda ±1 DE\n(winsorizado al p99.5)",
    x = "Año (trimestre)",
    y = "Pesos"
  ) +
  theme_minimal(base_size = 13)

save_plot(plot_ribbon_real_w, "ribbon_ingreso_real_winsor")

# ---- 13.5 Ribbon ±1 DE alrededor de la media SIN winsor ----
plot_ribbon_real <- res_tri_real %>%
  ggplot(aes(t_index, media_pond_real)) +
  geom_ribbon(aes(ymin = media_pond_real - sd_real,
                  ymax = media_pond_real + sd_real),
              fill = "#1f77b4", alpha = 0.18) +
  geom_line(color = "#1f77b4", linewidth = 1.1) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Ingreso real promedio y banda ±1 DE (sin winsor)",
    x = "Año (trimestre)",
    y = "Pesos"
  ) +
  theme_minimal(base_size = 13)

# Para verlo en el panel de RStudio
plot_ribbon_real

# Y para guardarlo:
save_plot(plot_ribbon_real, "ribbon_ingreso_real_sinwinsor")


# ===============================================================
# 14) Boxplots anuales de salarios reales (robustez)
# ===============================================================

# ---- 14.1 Boxplot anual SIN winsor --------------------------------
box_real_year <- dat_real %>%
  filter(
    year_final >= 2008, year_final <= 2022,
    !is.na(ing_real)
  ) %>%
  ggplot(aes(x = factor(year_final), y = ing_real)) +
  geom_boxplot(
    fill = "#1f77b4",
    colour = "grey30",
    outlier.alpha = 0.15,
    outlier.size  = 0.8
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Distribución anual del ingreso real (sin winsor)",
    x = "Año",
    y = "Ingreso real mensual (pesos)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

box_real_year
save_plot(box_real_year, "boxplot_ingreso_real_anual_sinwinsor")

# ---- 14.2 Boxplot anual CON winsor p99.5 --------------------------
box_real_year_w <- dat_real_w %>%
  filter(
    year_final >= 2008, year_final <= 2022,
    !is.na(ing_real)
  ) %>%
  ggplot(aes(x = factor(year_final), y = ing_real)) +
  geom_boxplot(
    fill = "#E67E22",
    colour = "grey30",
    outlier.alpha = 0.15,
    outlier.size  = 0.8
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Distribución anual del ingreso real (winsor p99.5)",
    x = "Año",
    y = "Ingreso real mensual (pesos)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

box_real_year_w
save_plot(box_real_year_w, "boxplot_ingreso_real_anual_winsor")

# ===============================================================
# Gráfico 1: Ingreso real promedio trimestral SIN winsor
# ===============================================================

p_ingreso_real_sin <- res_tri_real %>%
  ggplot(aes(x = t_index, y = media_pond_real)) +
  geom_line(linewidth = 1.1) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Ingreso real mensual promedio (sin winsor)",
    subtitle = "Media ponderada trimestral, 2008–2022",
    x = "Año (trimestre)",
    y = "Ingreso real promedio (pesos de 2018)"
  ) +
  theme_minimal(base_size = 13)

# Ver en pantalla
p_ingreso_real_sin

# Guardar
save_plot(p_ingreso_real_sin, "ingreso_real_promedio_trimestral_sin_winsor")


# ===============================================================
# Gráfico 2: Ingreso real promedio trimestral CON winsor p99.5
# ===============================================================

p_ingreso_real_w <- res_tri_real_w %>%
  ggplot(aes(x = t_index, y = media_pond_real)) +
  geom_line(linewidth = 1.1) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Ingreso real mensual promedio (winsor p99.5)",
    subtitle = "Media ponderada trimestral, 2008–2022",
    x = "Año (trimestre)",
    y = "Ingreso real promedio (pesos de 2018)"
  ) +
  theme_minimal(base_size = 13)

# Ver en pantalla
p_ingreso_real_w

# Guardar
save_plot(p_ingreso_real_w, "ingreso_real_promedio_trimestral_winsor")
