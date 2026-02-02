# codes/11_imacec_quarterly_indices.R
# ===============================================================
# 11) IMACEC mensual -> trimestral + índice base (año base = 100)
# - Lee: CFG$files$imacec_excel (sheet CFG$files$imacec_sheet)
# - Produce:
#   * data/processed/imacec_quarterly.rds
#   * out_tables/imacec_quarterly_indices_baseXXXX_100.csv
#   * out_tables/audits_11_*.csv (auditorías)
# ===============================================================

source(here::here("codes","00_setup.R"))
message("\n== 11) IMACEC trimestral (índices) ==\n")

stopifnot(file.exists(CFG$files$imacec_excel))

imacec_raw <- readxl::read_excel(
  CFG$files$imacec_excel,
  sheet = CFG$files$imacec_sheet
)

# ---- checks mínimos de columnas esperadas (tal como vienen en el Excel) ----
req_cols <- c("Periodo", "1. Imacec", "7. Imacec no minero")
stopifnot(all(req_cols %in% names(imacec_raw)))

# ---- parser robusto de Periodo (Date / POSIX / texto / Excel numeric) ----
parse_periodo <- function(x) {
  # vectorizado, devuelve Date
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  
  # Excel serial date (número)
  if (is.numeric(x)) {
    out <- as.Date(x, origin = "1899-12-30")
    return(out)
  }
  
  # texto: probar varios formatos típicos
  xx <- as.character(x)
  xx <- stringr::str_trim(xx)
  xx[xx == ""] <- NA_character_
  
  # casos tipo "2020m01" o "2020M01"
  xx <- stringr::str_replace_all(xx, "(\\d{4})[mM](\\d{2})", "\\1-\\2-01")
  
  # si viene "YYYY-MM" o "YYYY/MM" o "YYYY.MM" => agregar día 01
  xx <- ifelse(stringr::str_detect(xx, "^\\d{4}[-/.]\\d{1,2}$"),
               paste0(xx, "-01"),
               xx)
  
  # parse con lubridate (varios órdenes)
  out <- suppressWarnings(lubridate::parse_date_time(
    xx,
    orders = c("Ymd", "Y-m-d", "Y/m/d", "Y.m.d",
               "dmY", "d-m-Y", "d/m/Y", "d.m.Y",
               "my", "mY", "Ym", "Y-m", "Y/m", "Y.m"),
    exact = FALSE
  ))
  as.Date(out)
}

# ---- normalización mensual ----
imacec_mensual <- imacec_raw %>%
  dplyr::rename(
    periodo          = `Periodo`,
    imacec           = `1. Imacec`,
    imacec_no_minero = `7. Imacec no minero`
  ) %>%
  dplyr::mutate(
    fecha = parse_periodo(periodo),
    imacec = suppressWarnings(as.numeric(imacec)),
    imacec_no_minero = suppressWarnings(as.numeric(imacec_no_minero)),
    year  = lubridate::year(fecha),
    month = lubridate::month(fecha)
  ) %>%
  dplyr::filter(
    !is.na(fecha),
    dplyr::between(year, CFG$params$year_min, CFG$params$year_max)
  ) %>%
  dplyr::arrange(fecha)

stopifnot(nrow(imacec_mensual) > 0)

# ===============================================================
# Auditorías mensuales (para trazabilidad)
# ===============================================================

# 1) Duplicados por (year, month)
aud_dup_month <- imacec_mensual %>%
  dplyr::count(year, month, name = "n") %>%
  dplyr::filter(n > 1) %>%
  dplyr::arrange(year, month)

# 2) Cobertura mensual esperada vs observada (huecos)
exp_months <- tidyr::expand_grid(
  year = CFG$params$year_min:CFG$params$year_max,
  month = 1:12
)

aud_missing_month <- exp_months %>%
  dplyr::anti_join(
    imacec_mensual %>% dplyr::distinct(year, month),
    by = c("year", "month")
  ) %>%
  dplyr::arrange(year, month)

# 3) NA de series clave en mensual
aud_na_mensual <- imacec_mensual %>%
  dplyr::summarise(
    n = dplyr::n(),
    pct_na_imacec = mean(is.na(imacec)) * 100,
    pct_na_nm     = mean(is.na(imacec_no_minero)) * 100
  )

save_table_csv(aud_dup_month,     "audits_11_month_duplicates", dir = CFG$dirs$out_tables)
save_table_csv(aud_missing_month, "audits_11_month_missing",    dir = CFG$dirs$out_tables)
save_table_csv(aud_na_mensual,    "audits_11_month_na_summary", dir = CFG$dirs$out_tables)

# ---- mensual -> trimestral ----
imacec_tri <- imacec_mensual %>%
  dplyr::mutate(trimestre = lubridate::quarter(fecha)) %>%
  dplyr::group_by(year, trimestre) %>%
  dplyr::summarise(
    n_months = dplyr::n_distinct(month),
    imacec           = mean(imacec, na.rm = TRUE),
    imacec_no_minero = mean(imacec_no_minero, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    t_index = year + trimestre / 10
  ) %>%
  dplyr::arrange(t_index)

stopifnot(nrow(imacec_tri) > 0)

# 4) Auditoría: trimestres con menos de 3 meses
aud_quarter_months <- imacec_tri %>%
  dplyr::count(n_months, name = "n_quarters") %>%
  dplyr::arrange(n_months)

aud_quarters_lt3 <- imacec_tri %>%
  dplyr::filter(n_months < 3) %>%
  dplyr::select(year, trimestre, t_index, n_months, imacec, imacec_no_minero)

save_table_csv(aud_quarter_months, "audits_11_quarter_months_distribution", dir = CFG$dirs$out_tables)
save_table_csv(aud_quarters_lt3,   "audits_11_quarters_with_lt3_months",    dir = CFG$dirs$out_tables)

# ---- base año = 100 (promedio trimestral del año base) ----
base_year <- CFG$params$base_year

base_total <- imacec_tri %>%
  dplyr::filter(year == base_year) %>%
  dplyr::summarise(base = mean(imacec, na.rm = TRUE)) %>%
  dplyr::pull(base)

base_nm <- imacec_tri %>%
  dplyr::filter(year == base_year) %>%
  dplyr::summarise(base = mean(imacec_no_minero, na.rm = TRUE)) %>%
  dplyr::pull(base)

stopifnot(length(base_total) == 1, is.finite(base_total), base_total > 0)
stopifnot(length(base_nm)    == 1, is.finite(base_nm),    base_nm > 0)

imacec_tri <- imacec_tri %>%
  dplyr::mutate(
    imacec_indice           = imacec / base_total * 100,
    imacec_no_minero_indice = imacec_no_minero / base_nm * 100
  )

# ---- check fuerte: en el año base el promedio debe ser 100 ----
chk_base <- imacec_tri %>%
  dplyr::filter(year == base_year) %>%
  dplyr::summarise(
    mean_base_total = mean(imacec_indice, na.rm = TRUE),
    mean_base_nm    = mean(imacec_no_minero_indice, na.rm = TRUE),
    n_quarters      = dplyr::n()
  )

save_table_csv(chk_base, "audits_11_base_year_means_should_be_100", dir = CFG$dirs$out_tables)
stopifnot(abs(chk_base$mean_base_total - 100) < 1e-8)
stopifnot(abs(chk_base$mean_base_nm    - 100) < 1e-8)

# ---- outputs ----
save_rds(imacec_tri, CFG$paths$imacec_quarterly)

save_table_csv(
  imacec_tri %>%
    dplyr::select(year, trimestre, t_index, imacec_indice, imacec_no_minero_indice, n_months),
  paste0("imacec_quarterly_indices_base", base_year, "_100"),
  dir = CFG$dirs$out_tables
)

message("✅ 11 listo: IMACEC trimestral base ", base_year, " = 100 guardado + auditorías.")
