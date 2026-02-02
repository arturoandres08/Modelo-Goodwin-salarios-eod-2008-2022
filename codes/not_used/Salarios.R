# ========================================
# EOD 1997–2022: lectura robusta y resúmenes
# - 1) Resumen por archivo (trimestre)
# - 2) Agregado año–trimestre (wmean_adj) -> audit_ingsueld_trimestral.csv
# - 3) Gráficos de auditoría
# ========================================

# Paquetes
pkgs <- c("tidyverse","haven","janitor","foreign","here")
invisible(lapply(pkgs, function(p) if (!requireNamespace(p, quietly=TRUE)) install.packages(p)))
invisible(lapply(pkgs, library, character.only = TRUE))

# Helpers (rutas + save_plot/save_table)
source(here("scripts","00_helpers_paths.R"))

# ------------------ Utilidades ------------------
infer_year <- function(path) {
  y <- stringr::str_extract(basename(path), "(19|20)\\d{2}")
  as.integer(y)
}

infer_month <- function(path) {
  fname <- tolower(basename(path))
  dplyr::case_when(
    stringr::str_detect(fname, "\\bmar\\b|marzo") ~ 3L,
    stringr::str_detect(fname, "\\bjun\\b|junio") ~ 6L,
    stringr::str_detect(fname, "septiembre|\\bsept\\b|setiembre|\\bset\\b") ~ 9L,
    stringr::str_detect(fname, "diciembre|\\bdic\\b|dici") ~ 12L,
    TRUE ~ NA_integer_
  )
}

read_sav_robust <- function(path) {
  encodings <- c(NA, "UTF-8", "Windows-1252", "Latin1", "ISO-8859-1")
  for (e in encodings) {
    out <- try(haven::read_sav(path, encoding = e), silent = TRUE)
    if (!inherits(out, "try-error")) return(out |> janitor::clean_names())
  }
  for (e in c("latin1","windows-1252","iso-8859-1")) {
    out <- try(
      foreign::read.spss(path, to.data.frame = TRUE,
                         use.value.labels = FALSE, reencode = e),
      silent = TRUE
    )
    if (!inherits(out, "try-error"))
      return(tibble::as_tibble(out) |> janitor::clean_names())
  }
  stop("No se pudo leer el archivo con ningún encoding: ", basename(path))
}

read_any <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
         "sav" = read_sav_robust(path),
         "dta" = haven::read_dta(path) |> janitor::clean_names(),
         stop("Extensión no soportada: ", ext)
  )
}

safe_as_numeric <- function(x) {
  if (inherits(x, "labelled")) x <- haven::zap_labels(x)
  if (is.factor(x)) x <- as.character(x)
  suppressWarnings(as.numeric(x))
}

get_col <- function(df, name_target) {
  hit <- names(df)[tolower(names(df)) == tolower(name_target)]
  if (length(hit) == 1) hit else NA_character_
}

to_trimester <- function(m) dplyr::case_when(
  m %in% 1:3  ~ "T1",
  m %in% 4:6  ~ "T2",
  m %in% 7:9  ~ "T3",
  m %in% 10:12~ "T4",
  TRUE ~ NA_character_
)

multiplier_for_year <- function(y) ifelse(y %in% c(1997, 1998), 100, 1)

# ------------------ Entradas / salidas ------------------
dir_eod   <- here("data","eod")                # carpeta con .sav/.dta
dir_tab   <- here("auditoria_variables","tablas")
dir_graf  <- here("auditoria_variables","graficos")
fs::dir_create(dir_tab); fs::dir_create(dir_graf)

# ------------------ 1) Listado de archivos ------------------
files_9722 <- tibble::tibble(
  path = list.files(dir_eod, pattern = "\\.(sav|dta)$", full.names = TRUE, recursive = FALSE)
) |>
  dplyr::mutate(
    year  = infer_year(path),
    month = infer_month(path)
  ) |>
  dplyr::filter(!is.na(year), dplyr::between(year, 1997, 2022)) |>
  dplyr::arrange(year, month, path)

# ------------------ 2) Resumen por archivo ------------------
resumir_archivo <- function(path, year, month = NA_integer_) {
  try({
    df <- read_any(path) |> janitor::clean_names()
    
    var_wage   <- get_col(df, "ingsueld")
    var_weight <- get_col(df, "factor_hd")
    
    if (is.na(var_wage) || is.na(var_weight)) {
      return(tibble::tibble(
        year, month, file = basename(path),
        ok = FALSE,
        motivo = paste0("Faltan: ",
                        ifelse(is.na(var_wage), "ingsueld ", ""),
                        ifelse(is.na(var_weight), "factor_hd", "")),
        n_total = nrow(df), n_usadas = 0,
        masa_salarial = NA_real_, suma_pesos = NA_real_, salario_promedio = NA_real_
      ))
    }
    
    w   <- safe_as_numeric(df[[var_wage]])
    fac <- safe_as_numeric(df[[var_weight]])
    
    mult    <- multiplier_for_year(year)
    w_pesos <- w * mult
    
    keep <- !is.na(w_pesos) & w_pesos > 0 & !is.na(fac) & fac > 0
    n_total  <- length(w_pesos)
    n_usadas <- sum(keep)
    
    if (n_usadas == 0) {
      return(tibble::tibble(
        year, month, file = basename(path), ok = FALSE,
        motivo = "Sin observaciones válidas (w>0 y factor>0)",
        n_total, n_usadas,
        masa_salarial = NA_real_, suma_pesos = NA_real_, salario_promedio = NA_real_
      ))
    }
    
    masa  <- sum(w_pesos[keep] * fac[keep], na.rm = TRUE)
    pesos <- sum(fac[keep], na.rm = TRUE)
    prom  <- ifelse(pesos > 0, masa / pesos, NA_real_)
    
    tibble::tibble(
      year, month, file = basename(path), ok = TRUE, motivo = NA_character_,
      n_total, n_usadas,
      masa_salarial = masa, suma_pesos = pesos, salario_promedio = prom,
      wage_var = var_wage, weight_var = var_weight, mult_unidades = mult
    )
  }, silent = TRUE)
  
  tibble::tibble(
    year, month, file = basename(path), ok = FALSE,
    motivo = "Fallo en lectura/procesamiento en resumir_archivo()",
    n_total = NA_integer_, n_usadas = 0,
    masa_salarial = NA_real_, suma_pesos = NA_real_, salario_promedio = NA_real_,
    wage_var = NA_character_, weight_var = NA_character_, mult_unidades = multiplier_for_year(year)
  )
}

files_9722 <- files_9722 |> dplyr::mutate(month = ifelse(is.na(month), infer_month(path), month))

resumen_por_archivo <- files_9722 |>
  dplyr::mutate(res = purrr::pmap(list(path, year, month), \(p,y,m) resumir_archivo(p,y,m))) |>
  dplyr::select(res) |>
  tidyr::unnest(res) |>
  dplyr::mutate(trimestre = to_trimester(month)) |>
  dplyr::relocate(year, trimestre, month, file, .before = ok) |>
  dplyr::arrange(year, month)

# Guardar 1:1 archivo–trimestre
save_table(resumen_por_archivo, "resumen_ingsueld_factor_hd_trimestre_por_archivo")

# ------------------ 3) Agregado año–trimestre (insumo salarios) ----
audit_ingsueld_trimestral <- resumen_por_archivo |>
  dplyr::filter(ok, !is.na(salario_promedio)) |>
  dplyr::group_by(year, trimestre) |>
  dplyr::summarise(
    # promedio ponderado con suma de factores como peso total de archivo
    wmean_adj = weighted.mean(salario_promedio, w = suma_pesos, na.rm = TRUE),
    n_obs     = sum(n_usadas, na.rm = TRUE),
    .groups   = "drop"
  )

save_table(audit_ingsueld_trimestral, "audit_ingsueld_trimestral")

# ------------------ 4) Gráficos de auditoría -----------------------
# Dispersión (log) de salario_promedio por mes
g_disp <- resumen_por_archivo |>
  dplyr::filter(ok, !is.na(salario_promedio)) |>
  dplyr::mutate(fecha = lubridate::make_date(year, month, 1)) |>
  ggplot(aes(fecha, salario_promedio)) +
  geom_point(alpha = 0.6) +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Dispersión de promedios por archivo (escala log)",
       x = "Fecha (mes del levantamiento)", y = "Salario promedio (log)") +
  theme_minimal(base_size = 12)
save_plot(g_disp, "audit_ingsueld_dispersion_log")

# Promedio anual de los promedios por archivo (log)
g_media_anual <- resumen_por_archivo |>
  dplyr::filter(ok, !is.na(salario_promedio)) |>
  dplyr::group_by(year) |>
  dplyr::summarise(media_anual = mean(salario_promedio, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(year, media_anual)) +
  geom_line() + geom_point() +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Media anual de promedios por archivo (log)",
       x = "Año", y = "Salario promedio (log)") +
  theme_minimal(base_size = 12)
save_plot(g_media_anual, "audit_ingsueld_media_log_promedios_anuales")

message("✔ Listo. Archivos en auditoria_variables/{tablas,graficos}.")

