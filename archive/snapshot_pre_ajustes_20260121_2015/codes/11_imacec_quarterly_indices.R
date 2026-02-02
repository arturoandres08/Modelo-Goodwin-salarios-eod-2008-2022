# codes/11_imacec_quarterly_indices.R
source(here("codes", "00_setup.R"))

stopifnot(file.exists(CFG$files$imacec_excel))

imacec_raw <- readxl::read_excel(
  CFG$files$imacec_excel,
  sheet = CFG$files$imacec_sheet
)

# ---- checks mínimos de columnas esperadas ----
req_cols <- c("Periodo", "1. Imacec", "7. Imacec no minero")
stopifnot(all(req_cols %in% names(imacec_raw)))

# ---- parser robusto de Periodo (Date / POSIX / texto / Excel numeric) ----
parse_periodo <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))  # origen típico Excel
  as.Date(x)
}

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

# ---- mensual -> trimestral ----
imacec_tri <- imacec_mensual %>%
  dplyr::mutate(trimestre = lubridate::quarter(fecha)) %>%
  dplyr::group_by(year, trimestre) %>%
  dplyr::summarise(
    imacec           = mean(imacec, na.rm = TRUE),
    imacec_no_minero = mean(imacec_no_minero, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(t_index = year + trimestre / 10) %>%
  dplyr::arrange(t_index)

# ---- base 2018 = 100 (promedio trimestral del año base) ----
base_total <- imacec_tri %>%
  dplyr::filter(year == CFG$params$base_year) %>%
  dplyr::summarise(base = mean(imacec, na.rm = TRUE)) %>%
  dplyr::pull(base)

base_nm <- imacec_tri %>%
  dplyr::filter(year == CFG$params$base_year) %>%
  dplyr::summarise(base = mean(imacec_no_minero, na.rm = TRUE)) %>%
  dplyr::pull(base)

stopifnot(length(base_total) == 1, is.finite(base_total), base_total > 0)
stopifnot(length(base_nm)    == 1, is.finite(base_nm),    base_nm > 0)

imacec_tri <- imacec_tri %>%
  dplyr::mutate(
    imacec_indice           = imacec / base_total * 100,
    imacec_no_minero_indice = imacec_no_minero / base_nm * 100
  )

# ---- outputs ----
save_rds(imacec_tri, CFG$paths$imacec_quarterly)

save_table_csv(
  imacec_tri %>%
    dplyr::select(year, trimestre, t_index, imacec_indice, imacec_no_minero_indice),
  "imacec_quarterly_indices_base2018_100",
  dir = CFG$dirs$out_tables
)

message("✔ 11 listo: IMACEC trimestral base 2018=100 guardado")
