# codes/06_real_income_and_quarterly.R
# ===============================================================
# 06) Salarios reales trimestrales (2008–2022)
# - Insumos:
#   * data/processed/nominal_quarterly_2008_2022.rds   (desde 04)
#   * data/processed/ipc_monthly_base2018_100.rds      (desde 05)
#   * (opcional pero recomendado) data/interim/eod_individual_2008_2022_clean.rds
#                                data/interim/eod_individual_2008_2022_winsor.rds
# - Salidas:
#   * data/processed/real_quarterly_2008_2022.rds
#   * tables/real_quarterly_2008_2022.csv
#   * data/validation/audit_06_*.csv  + auditoria_variables/validation/audit_06_*.csv
# ===============================================================

source(here::here("codes","00_setup.R"))

# ---- Aliases (compatibilidad con scripts antiguos) ----
CFGpaths  <- CFG$paths
CFGdirs   <- CFG$dirs
CFGparams <- CFG$params

message("== 06) Salarios reales trimestrales ==")

# ===============================================================
# Helpers
# ===============================================================

norm_trim_int <- function(tri_raw, month = NA_integer_) {
  # Devuelve trimestre como entero 1..4
  out <- suppressWarnings(as.integer(tri_raw))
  if (!all(is.na(out))) return(out)
  
  tri_chr <- toupper(as.character(tri_raw))
  out2 <- suppressWarnings(as.integer(stringr::str_extract(tri_chr, "[1-4]")))
  if (!all(is.na(out2))) return(out2)
  
  mm <- suppressWarnings(as.integer(month))
  ifelse(is.na(mm), NA_integer_, ceiling(mm / 3))
}

q_num <- function(tri_int) suppressWarnings(as.integer(tri_int))

pick_first_existing <- function(nms, candidates) {
  hit <- intersect(candidates, nms)
  if (length(hit) == 0) NA_character_ else hit[1]
}

safe_div <- function(a, b) {
  out <- rep(NA_real_, length(a))
  ok <- !is.na(a) & !is.na(b) & b != 0
  out[ok] <- a[ok] / b[ok]
  out
}

save_validation_both <- function(df, filename) {
  fs::dir_create(CFGdirs$validation)
  fs::dir_create(CFGdirs$aud_val)
  p1 <- fs::path(CFGdirs$validation, filename)
  p2 <- fs::path(CFGdirs$aud_val, filename)
  readr::write_csv(df, p1)
  readr::write_csv(df, p2)
  invisible(list(data_validation = p1, aud_validation = p2))
}

# ===============================================================
# 1) Cargar nominal trimestral (output 04)
# ===============================================================

stopifnot(file.exists(CFGpaths$nominal_quarterly))
nom <- load_rds(CFGpaths$nominal_quarterly) %>%
  tibble::as_tibble()

# Normaliza llaves
nom <- nom %>%
  dplyr::mutate(
    year          = as.integer(year),
    trimestre_raw = trimestre,
    trimestre     = norm_trim_int(trimestre, month = month %||% NA_integer_)
  )

# version (si no existe, crea)
if (!"version" %in% names(nom)) {
  nom <- nom %>% dplyr::mutate(version = "sin_winsor")
}

# llaves mínimas
req_nom <- c("year","trimestre","version")
stopifnot(all(req_nom %in% names(nom)))

# Duplicados (auditar y resolver)
keys <- c("version","year","trimestre")
dups <- nom %>%
  dplyr::count(dplyr::across(dplyr::all_of(keys)), name = "n") %>%
  dplyr::filter(n > 1)

if (nrow(dups) > 0) {
  save_validation_both(dups, "audit_06_duplicates_in_nominal.csv")
  # regla simple: quedarse con la primera fila por llave
  nom <- nom %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
}

# ===============================================================
# 2) Cargar IPC mensual (output 05) y construir IPC trimestral
# ===============================================================

stopifnot(file.exists(CFGpaths$ipc_monthly))
ipc_m <- load_rds(CFGpaths$ipc_monthly) %>%
  tibble::as_tibble()

# Detecta columna IPC (según 05)
ipc_col <- pick_first_existing(
  names(ipc_m),
  c("ipc_base2018_100","ipc","IPC","ipc_2018_100","indice_ipc","indice")
)
if (is.na(ipc_col)) {
  stop("No pude detectar la columna de IPC en ipc_monthly. Esperaba algo como 'ipc_base2018_100'.")
}

# year/month deben existir o ser derivables desde 'periodo'
if (!all(c("year","month") %in% names(ipc_m))) {
  if ("periodo" %in% names(ipc_m)) {
    ipc_m <- ipc_m %>%
      dplyr::mutate(
        periodo = as.Date(periodo),
        year = lubridate::year(periodo),
        month = lubridate::month(periodo)
      )
  } else {
    stop("ipc_monthly no tiene (year, month) ni 'periodo' para derivarlos.")
  }
}

ipc_m <- ipc_m %>%
  dplyr::mutate(
    year = as.integer(year),
    month = as.integer(month),
    ipc_m = as.numeric(.data[[ipc_col]]),
    trimestre = norm_trim_int(NA, month = month)
  ) %>%
  dplyr::filter(!is.na(year), !is.na(month), !is.na(trimestre)) %>%
  dplyr::arrange(year, month)

# Cobertura IPC por trimestre (audit)
ipc_cov <- ipc_m %>%
  dplyr::group_by(year, trimestre) %>%
  dplyr::summarise(
    n_months = dplyr::n(),
    months = paste(sort(unique(month)), collapse = ","),
    ipc_min = min(ipc_m, na.rm = TRUE),
    ipc_max = max(ipc_m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(year, trimestre)

save_validation_both(ipc_cov, "audit_06_ipc_quarter_coverage.csv")

# IPC trimestral promedio (simple)
ipc_q <- ipc_m %>%
  dplyr::group_by(year, trimestre) %>%
  dplyr::summarise(
    ipc_q_avg = mean(ipc_m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(deflator_q = ipc_q_avg / 100)

# Base esperada (informativo)
base_row <- ipc_q %>%
  dplyr::filter(year == CFGparams$base_year, trimestre == 4) %>%
  dplyr::slice(1)

if (nrow(base_row) == 1) {
  message("✓ Base usada: ", CFGparams$base_year, "-T4 | ipc_q_avg=",
          round(base_row$ipc_q_avg, 6), " | deflator_q=", round(base_row$deflator_q, 6))
}

# ===============================================================
# 3) Unir nominal trimestral con IPC trimestral (real)
# ===============================================================

rea <- nom %>%
  dplyr::mutate(
    year = as.integer(year),
    trimestre = as.integer(trimestre)
  ) %>%
  dplyr::left_join(ipc_q, by = c("year","trimestre"))

# Checks duros: join no puede dejar IPC/deflator NA dentro del rango
audit_join <- rea %>%
  dplyr::summarise(
    n = dplyr::n(),
    n_na_ipc = sum(is.na(ipc_q_avg)),
    n_na_def = sum(is.na(deflator_q)),
    def_min = suppressWarnings(min(deflator_q, na.rm = TRUE)),
    def_max = suppressWarnings(max(deflator_q, na.rm = TRUE))
  )

save_validation_both(audit_join, "audit_06_join_ipc_summary.csv")

if (audit_join$n_na_def > 0) {
  miss <- rea %>%
    dplyr::filter(is.na(deflator_q)) %>%
    dplyr::distinct(version, year, trimestre) %>%
    dplyr::arrange(version, year, trimestre)
  save_validation_both(miss, "audit_06_missing_ipc_after_join.csv")
  stop("Faltan trimestres con deflator_q (revisa audit_06_missing_ipc_after_join.csv).")
}

# ===============================================================
# 4) Variables reales (desde agregados nominales + deflator_q)
#    + compatibilidad con 07 y checks
# ===============================================================

# Asegura que existan estas columnas nominales (si no, quedan NA)
for (cc in c("media_pond","media_simple","p99_5","masa_salarial",
             "sd_nominal","var_nominal","cv_nominal","n_obs","suma_pesos","t_index")) {
  if (!cc %in% names(rea)) rea[[cc]] <- NA_real_
}

rea <- rea %>%
  dplyr::mutate(
    # índices/llaves
    trimestre = as.integer(trimestre),
    t_index = dplyr::coalesce(as.numeric(t_index), as.numeric(year) + as.numeric(trimestre)/10),
    
    # real (deflactado base 2018=100)
    media_pond_real   = safe_div(as.numeric(media_pond),   deflator_q),
    media_simple_real = safe_div(as.numeric(media_simple), deflator_q),
    
    # nombres esperados por otros scripts/checks
    mean_nominal = as.numeric(media_pond),
    mean_real    = media_pond_real,
    
    masa_salarial_real = safe_div(as.numeric(masa_salarial), deflator_q),
    p99_5_real         = safe_div(as.numeric(p99_5), deflator_q)
  )

# ===============================================================
# 5) SD / VAR / CV reales (desde microdatos EOD, si están)
# ===============================================================

build_dispersion_from_micro <- function(df, version_label, income_candidates) {
  df <- tibble::as_tibble(df)
  
  year_col <- pick_first_existing(names(df), c("year_final","year","anio","ano"))
  mon_col  <- pick_first_existing(names(df), c("month","mes_int","mes","mes_num"))
  tri_col  <- pick_first_existing(names(df), c("tri_fix","trimestre","tri_raw"))
  w_col    <- pick_first_existing(names(df), c("factor_hd","pond","weight","w"))
  inc_col  <- pick_first_existing(names(df), income_candidates)
  
  if (any(is.na(c(year_col, mon_col, w_col, inc_col)))) {
    return(tibble::tibble(
      version = character(0),
      year = integer(0),
      trimestre = integer(0),
      sd_real_new = numeric(0),
      var_real_new = numeric(0),
      cv_real_new = numeric(0)
    ))
  }
  
  tmp <- df %>%
    dplyr::mutate(
      year_i = as.integer(.data[[year_col]]),
      month_i = as.integer(.data[[mon_col]]),
      trimestre_i = if (!is.na(tri_col)) norm_trim_int(.data[[tri_col]], month = month_i) else norm_trim_int(NA, month = month_i),
      w = suppressWarnings(as.numeric(.data[[w_col]])),
      inc_nom = suppressWarnings(as.numeric(.data[[inc_col]]))
    ) %>%
    dplyr::filter(
      !is.na(year_i),
      dplyr::between(year_i, CFGparams$year_min, CFGparams$year_max),
      !is.na(month_i),
      month_i >= 1, month_i <= 12,
      !is.na(trimestre_i), trimestre_i %in% 1:4,
      !is.na(inc_nom),
      !is.na(w),
      w > 0,
      inc_nom >= 0
    ) %>%
    dplyr::left_join(
      ipc_m %>% dplyr::select(year, month, ipc_m),
      by = c("year_i" = "year", "month_i" = "month")
    ) %>%
    dplyr::mutate(
      def_m = ipc_m / 100,
      ing_real = safe_div(inc_nom, def_m)
    ) %>%
    dplyr::filter(!is.na(ing_real))
  
  tmp %>%
    dplyr::group_by(year = year_i, trimestre = trimestre_i) %>%
    dplyr::summarise(
      sd_real_new  = sd(ing_real, na.rm = TRUE),
      var_real_new = var(ing_real, na.rm = TRUE),
      mean_simple_real_new = mean(ing_real, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      version = version_label,
      cv_real_new = safe_div(sd_real_new, mean_simple_real_new)
    ) %>%
    dplyr::select(version, year, trimestre, sd_real_new, var_real_new, cv_real_new)
}

disp_list <- list()

if (file.exists(CFGpaths$eod_clean_2008_2022)) {
  df_clean <- load_rds(CFGpaths$eod_clean_2008_2022)
  disp_list[["sin_winsor"]] <- build_dispersion_from_micro(
    df_clean,
    version_label = "sin_winsor",
    income_candidates = c("ingsueld","ingreso","ing_sueldo","income")
  )
}

if (file.exists(CFGpaths$eod_winsor_2008_2022)) {
  df_w <- load_rds(CFGpaths$eod_winsor_2008_2022)
  disp_list[["winsor_p99_5"]] <- build_dispersion_from_micro(
    df_w,
    version_label = "winsor_p99_5",
    income_candidates = c("ingsueld_w","ingsueld","ingreso_w","income_w")
  )
}

disp_real <- dplyr::bind_rows(disp_list) %>%
  dplyr::mutate(
    year = as.integer(year),
    trimestre = as.integer(trimestre)
  )

# Join de dispersión al agregado
if (nrow(disp_real) > 0) {
  rea <- rea %>%
    dplyr::left_join(disp_real, by = c("version","year","trimestre"))
} else {
  rea$sd_real_new  <- NA_real_
  rea$var_real_new <- NA_real_
  rea$cv_real_new  <- NA_real_
}

# --- Blindaje: si no existen columnas “viejas”, créalas como NA ---
for (cc in c("sd_real","var_real","cv_real")) {
  if (!cc %in% names(rea)) rea[[cc]] <- NA_real_
}

# Consolidar (coalesce) + limpiar _new
rea <- rea %>%
  dplyr::mutate(
    sd_real  = dplyr::coalesce(sd_real,  sd_real_new),
    var_real = dplyr::coalesce(var_real, var_real_new),
    cv_real  = dplyr::coalesce(cv_real,  cv_real_new)
  ) %>%
  dplyr::select(-dplyr::any_of(c("sd_real_new","var_real_new","cv_real_new")))

# Si CV aún NA pero ya hay sd_real y mean_real, intenta definirlo con mean_real
rea <- rea %>%
  dplyr::mutate(
    cv_real = dplyr::coalesce(cv_real, safe_div(sd_real, mean_real))
  )

# ===============================================================
# 6) Auditorías finales (NA por versión + checks de llaves)
# ===============================================================

audit_na <- rea %>%
  dplyr::group_by(version) %>%
  dplyr::summarise(
    n = dplyr::n(),
    na_sd_real  = sum(is.na(sd_real)),
    na_var_real = sum(is.na(var_real)),
    na_cv_real  = sum(is.na(cv_real)),
    na_mean_real = sum(is.na(mean_real)),
    na_ipc_q_avg = sum(is.na(ipc_q_avg)),
    na_deflator_q = sum(is.na(deflator_q)),
    .groups = "drop"
  )

save_validation_both(audit_na, "audit_06_deflated_columns.csv")

# llaves únicas
dups_final <- rea %>%
  dplyr::count(version, year, trimestre, name = "n") %>%
  dplyr::filter(n > 1)

if (nrow(dups_final) > 0) {
  save_validation_both(dups_final, "audit_06_duplicates_in_real.csv")
  stop("Duplicados en real_quarterly (revisa audit_06_duplicates_in_real.csv).")
}

# completitud 2008–2022 (60 por versión)
exp_grid <- tidyr::expand_grid(
  version = sort(unique(rea$version)),
  year = CFGparams$year_min:CFGparams$year_max,
  trimestre = 1:4
)

faltan <- exp_grid %>%
  dplyr::anti_join(rea %>% dplyr::distinct(version, year, trimestre),
                   by = c("version","year","trimestre"))

if (nrow(faltan) > 0) {
  save_validation_both(faltan, "audit_06_missing_quarters_real.csv")
  stop("Faltan trimestres en real_quarterly (revisa audit_06_missing_quarters_real.csv).")
}

# ===============================================================
# 7) Guardar outputs finales
# ===============================================================

save_rds(rea, CFGpaths$real_quarterly)
save_paper_csv(rea, "real_quarterly_2008_2022", dir = CFGdirs$out_tables)

message("✓ 06 listo: salarios reales trimestrales guardados en processed + tables + auditorías en validation.")
