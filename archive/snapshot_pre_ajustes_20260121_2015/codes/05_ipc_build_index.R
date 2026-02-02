# codes/05_ipc_build_index.R
source(here("codes", "00_setup.R"))

stopifnot(file.exists(CFG$files$ipc_excel))

# ------------------------------------------------------------
# 0) Lectura + detección robusta de columnas
# ------------------------------------------------------------
ipc_raw <- readxl::read_excel(CFG$files$ipc_excel) %>%
  janitor::clean_names()

# Intento 1: nombres esperados (tras clean_names)
# - periodo -> "periodo"
# - var mensual -> algo como "x1_ipc_general_empalme_bcch" o similar
# Por eso uso detección por patrones.
pick_first <- function(nms, patterns) {
  for (p in patterns) {
    hit <- stringr::str_which(nms, stringr::regex(p, ignore_case = TRUE))
    if (length(hit)) return(nms[hit[1]])
  }
  NA_character_
}

nms <- names(ipc_raw)
v_periodo <- pick_first(nms, c("^periodo$", "period"))
v_var     <- pick_first(nms, c("ipc.*general", "var.*mens", "variac", "empalme", "bcch"))

stopifnot(!is.na(v_periodo), !is.na(v_var))

# ------------------------------------------------------------
# 1) Parse robusto de Periodo
# ------------------------------------------------------------
parse_periodo <- function(x) {
  # Date nativo
  if (inherits(x, "Date")) return(x)
  
  # Numérico Excel (días desde 1899-12-30)
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  
  # Texto
  x_chr <- as.character(x)
  x_chr <- stringr::str_trim(x_chr)
  
  # Intentos comunes: "YYYY-MM-DD", "YYYY-MM", "MM/YYYY", etc.
  d <- suppressWarnings(lubridate::ymd(x_chr))
  if (!all(is.na(d))) return(d)
  
  d <- suppressWarnings(lubridate::ym(x_chr))   # "YYYY-MM"
  if (!all(is.na(d))) return(as.Date(d))
  
  d <- suppressWarnings(lubridate::my(x_chr))   # "MM-YYYY" o "MM/YYYY"
  if (!all(is.na(d))) return(as.Date(d))
  
  # Último intento: as.Date directo
  suppressWarnings(as.Date(x_chr))
}

# ------------------------------------------------------------
# 2) Construcción IPC (mensual) + validaciones
# ------------------------------------------------------------
ipc <- ipc_raw %>%
  dplyr::transmute(
    periodo_raw = .data[[v_periodo]],
    var_raw     = .data[[v_var]]
  ) %>%
  dplyr::mutate(
    periodo = parse_periodo(periodo_raw),
    year    = lubridate::year(periodo),
    month   = lubridate::month(periodo),
    # parse_number soporta "1,2", "1.2", "1.2%" (saca el número)
    var_mensual = readr::parse_number(as.character(var_raw), locale = readr::locale(decimal_mark = ","))
  ) %>%
  dplyr::filter(!is.na(year), !is.na(month)) %>%
  dplyr::arrange(year, month)

stopifnot(nrow(ipc) > 0)

# ---- (A) Duplicados year-month ----
dup <- ipc %>%
  dplyr::count(year, month) %>%
  dplyr::filter(n > 1)

if (nrow(dup) > 0) {
  # Guardar diagnóstico y colapsar con promedio (regla estable y reproducible)
  save_table_csv(dup, "validation_ipc_duplicates_year_month", dir = CFG$dirs$validation)
  
  ipc <- ipc %>%
    dplyr::group_by(year, month) %>%
    dplyr::summarise(
      periodo = min(periodo, na.rm = TRUE),
      var_mensual = mean(var_mensual, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(year, month)
}

# ---- (B) Chequeo meses faltantes (importante para cumprod) ----
min_ym <- min(ipc$year * 100 + ipc$month, na.rm = TRUE)
max_ym <- max(ipc$year * 100 + ipc$month, na.rm = TRUE)

start_y <- min(ipc$year, na.rm = TRUE)
start_m <- ipc$month[which.min(ipc$year * 100 + ipc$month)]
end_y   <- max(ipc$year, na.rm = TRUE)
end_m   <- ipc$month[which.max(ipc$year * 100 + ipc$month)]

cal <- tibble::tibble(
  periodo = seq.Date(
    from = as.Date(sprintf("%04d-%02d-01", start_y, start_m)),
    to   = as.Date(sprintf("%04d-%02d-01", end_y, end_m)),
    by   = "month"
  )
) %>%
  dplyr::mutate(
    year  = lubridate::year(periodo),
    month = lubridate::month(periodo)
  )

ipc_chk <- cal %>%
  dplyr::left_join(ipc %>% dplyr::select(year, month, var_mensual), by = c("year", "month"))

missing_months <- ipc_chk %>%
  dplyr::filter(is.na(var_mensual))

if (nrow(missing_months) > 0) {
  save_table_csv(missing_months, "validation_ipc_missing_months", dir = CFG$dirs$validation)
  stop(
    "IPC: faltan meses o var_mensual NA. Revisa data/validation/validation_ipc_missing_months.csv.\n",
    "No se calcula indice para evitar cumprod incorrecto."
  )
}

# Usamos ipc_chk como serie completa (sin huecos)
ipc <- ipc_chk %>%
  dplyr::arrange(year, month) %>%
  dplyr::mutate(
    factor = 1 + var_mensual / 100,
    indice_bruto = 100 * cumprod(factor)
  )

# ------------------------------------------------------------
# 3) Base dic-(base_year)=100
# ------------------------------------------------------------
ipc_base <- ipc %>%
  dplyr::filter(year == CFG$params$base_year, month == CFG$params$base_month_ipc) %>%
  dplyr::pull(indice_bruto)

stopifnot(length(ipc_base) == 1, is.finite(ipc_base))

ipc <- ipc %>%
  dplyr::mutate(IPC = indice_bruto / ipc_base * 100)

# ------------------------------------------------------------
# 4) Guardar outputs
# ------------------------------------------------------------
save_rds(ipc, CFG$paths$ipc_monthly)

save_table_csv(
  ipc %>% dplyr::select(year, month, IPC),
  "ipc_monthly_base2018_100",
  dir = CFG$dirs$out_tables
)

message("✔ 05 listo: IPC mensual base dic-2018=100 guardado + validaciones en data/validation")
