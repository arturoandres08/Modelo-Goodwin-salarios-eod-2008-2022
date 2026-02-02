# codes/08_eod_extract_unemployment_individuals.R
# ===============================================================
# 08) Extrae individuos para desempleo (sitocup*) desde EOD
# - Lee: data/raw/eod/*.sav|*.dta
# - Guarda:
#   * data/interim/eod_unemployment_individual_raw_like.rds
#   * data/interim/eod_unemployment_individual_2008_2022.rds   (CFG$paths$unemp_individual_2008_2022)
#   * tables/ (auditorías + extract_like)
#   * data/validation/ + auditoria_variables/validation/ (auditorías)
# ===============================================================

source(here::here("codes","00_setup.R"))

message("\n== 08) Extract desempleo individual (EOD) ==\n")

dir_eod <- CFG$dirs$data_eod
stopifnot(fs::dir_exists(dir_eod))

eod_files <- fs::dir_ls(
  dir_eod,
  regexp  = "\\.(sav|dta)$",
  type    = "file",
  recurse = FALSE
)
stopifnot(length(eod_files) > 0)

# -------------------- helpers guardado auditorías --------------------
save_validation_dual <- function(df, name) {
  # data/validation
  fs::dir_create(CFG$dirs$validation)
  readr::write_csv(df, fs::path(CFG$dirs$validation, paste0(name, ".csv")))
  # auditoria_variables/validation
  save_audit_validation_csv(df, name)
  invisible(TRUE)
}

# -------------------- detectores de columnas --------------------
cand_peso <- c("^factor_hd$", "^pond$", "factor", "ponder", "^w[0-9]*$")
cand_mes  <- c("^mes$", "mes_lev", "meslev", "mes_enc", "mes_muestra", "mes_levantamiento")
cand_year <- c("^year$", "^agno$", "^ano$", "^anio$", "year_enc", "agno_enc", "ano_enc", "anio_enc")

# 👇 más flexible que solo "^sitocup1$"
cand_sit  <- c("^sitocup1$", "^sitocup$", "sitocup", "sit_ocup", "situacion.*ocup", "sit.*ocup")

pick_first <- function(nms, patterns) {
  for (p in patterns) {
    hit <- stringr::str_which(nms, stringr::regex(p, ignore_case = TRUE))
    if (length(hit)) return(nms[hit[1]])
  }
  NA_character_
}

# -------------------- lectura robusta .sav / .dta --------------------
read_sav_safely <- function(f) {
  base <- basename(f)
  df1 <- try(haven::read_sav(f, user_na = TRUE), silent = TRUE)
  if (!inherits(df1, "try-error")) return(df1)
  
  df2 <- try(haven::read_sav(f, user_na = TRUE, encoding = "latin1"), silent = TRUE)
  if (!inherits(df2, "try-error")) {
    message("✔ ", base, " leído con encoding='latin1'")
    return(df2)
  }
  message("✖ No se pudo leer: ", base)
  NULL
}

read_dta_safely <- function(f) {
  base <- basename(f)
  df1 <- try(haven::read_dta(f), silent = TRUE)
  if (!inherits(df1, "try-error")) return(df1)
  
  df2 <- try(haven::read_dta(f, encoding = "latin1"), silent = TRUE)
  if (!inherits(df2, "try-error")) {
    message("✔ ", base, " leído .dta con encoding='latin1'")
    return(df2)
  }
  message("✖ No se pudo leer: ", base)
  NULL
}

# -------------------- inferir mes desde nombre archivo --------------------
infer_month_from_filename <- function(fname) {
  f <- tolower(basename(fname))
  
  # 1) _01_ / -01- / .01.
  mm <- stringr::str_match(f, "(?:^|[_\\-.])(0[1-9]|1[0-2])(?:[_\\-.]|$)")[, 2]
  if (!is.na(mm)) return(mm)
  
  # 2) m01 / mes01
  mm2 <- stringr::str_match(f, "(?:m|mes)(0[1-9]|1[0-2])")[, 2]
  if (!is.na(mm2)) return(mm2)
  
  # 3) fallback por trimestre en nombre
  if (stringr::str_detect(f, "t1")) return("03")
  if (stringr::str_detect(f, "t2")) return("06")
  if (stringr::str_detect(f, "t3")) return("09")
  if (stringr::str_detect(f, "t4")) return("12")
  
  NA_character_
}
infer_month_vec <- function(x) vapply(x, infer_month_from_filename, character(1))

# -------------------- extracción --------------------
extract_desemp <- function(f) {
  base <- basename(f)
  message("Leyendo: ", base)
  ext  <- tolower(fs::path_ext(f))
  
  df <- if (ext == "sav") {
    read_sav_safely(f)
  } else if (ext == "dta") {
    read_dta_safely(f)
  } else {
    NULL
  }
  if (is.null(df)) return(NULL)
  
  # limpia nombres para detección consistente
  df <- janitor::clean_names(df)
  nms <- names(df)
  
  v_sit <- pick_first(nms, cand_sit)
  v_w   <- pick_first(nms, cand_peso)
  v_mes <- pick_first(nms, cand_mes)
  v_y   <- pick_first(nms, cand_year)
  
  if (is.na(v_sit) || is.na(v_w)) {
    message("  → se omite (falta sitocup* o factor_hd): ", base)
    return(NULL)
  }
  
  # año: desde columna o desde nombre archivo
  year_col_raw <- if (!is.na(v_y)) df[[v_y]] else NA
  year_lab_chr <- suppressWarnings(as.character(haven::as_factor(year_col_raw)))
  year_lab_num <- suppressWarnings(readr::parse_integer(year_lab_chr, na = c("", "NA")))
  year_num     <- suppressWarnings(as.numeric(year_col_raw))
  year_col     <- dplyr::coalesce(year_lab_num, year_num)
  year_col[year_col < 1900 | year_col > 2100] <- NA_integer_
  
  year_file <- suppressWarnings(as.integer(stringr::str_extract(base, "(19|20)\\d{2}")))
  mes_val   <- suppressWarnings(as.integer(if (!is.na(v_mes)) df[[v_mes]] else NA))
  
  tibble::tibble(
    archivo        = tolower(base),
    sitocup1       = suppressWarnings(as.numeric(df[[v_sit]])),      # nombre estándar
    sitocup1_lbl   = suppressWarnings(as.character(haven::as_factor(df[[v_sit]]))), # para auditar
    factor_hd      = suppressWarnings(as.numeric(df[[v_w]])),
    mes            = mes_val,
    year           = dplyr::coalesce(year_col, year_file)
  )
}

desemp_nom <- purrr::map_dfr(eod_files, extract_desemp)
stopifnot(nrow(desemp_nom) > 0)

# Guardar raw-like (trazabilidad)
save_rds(desemp_nom, fs::path(CFG$dirs$interim, "eod_unemployment_individual_raw_like.rds"))

# -------------------- normalización + filtro ventana --------------------
desemp0 <- desemp_nom %>%
  dplyr::mutate(
    year_file  = suppressWarnings(as.integer(stringr::str_extract(archivo, "(19|20)\\d{2}"))),
    year_final = dplyr::coalesce(
      dplyr::if_else(dplyr::between(as.integer(year), 1900L, 2100L), as.integer(year), NA_integer_),
      year_file
    ),
    month_chr = ifelse(!is.na(mes),
                       stringr::str_pad(as.integer(mes), 2, pad = "0"),
                       infer_month_vec(archivo)),
    month = suppressWarnings(as.integer(month_chr)),
    month = dplyr::if_else(month %in% 1:12, month, NA_integer_),
    tri_fix = tri_from_month(month),
    trimestre = suppressWarnings(as.integer(stringr::str_remove(tri_fix, "T")))
  ) %>%
  dplyr::filter(
    !is.na(factor_hd), factor_hd > 0,
    !is.na(sitocup1),
    !is.na(year_final),
    dplyr::between(year_final, CFG$params$year_min, CFG$params$year_max),
    !is.na(tri_fix),
    trimestre %in% 1:4
  )

stopifnot(nrow(desemp0) > 0)

# -------------------- auditorías (validation + paper tables) --------------------
aud_q <- desemp0 %>%
  dplyr::count(year_final, tri_fix, name = "n_obs") %>%
  dplyr::arrange(year_final, tri_fix)

aud_sit <- desemp0 %>%
  dplyr::count(sitocup1, sitocup1_lbl, name = "n_obs") %>%
  dplyr::arrange(dplyr::desc(n_obs))

aud_cov_month <- desemp0 %>%
  dplyr::count(year_final, month, name = "n_obs") %>%
  dplyr::arrange(year_final, month)

# Guardar auditorías en data/validation y auditoria_variables/validation
save_validation_dual(aud_q,        "audit_08_unemployment_counts_by_year_quarter")
save_validation_dual(aud_sit,      "audit_08_unemployment_sitocup_distribution")
save_validation_dual(aud_cov_month,"audit_08_unemployment_counts_by_year_month")

# También a tables/ (paper)
save_table_csv(aud_q,   "audit_unemployment_counts_by_year_quarter", dir = CFG$dirs$out_tables)
save_table_csv(aud_sit, "audit_unemployment_sitocup_distribution",  dir = CFG$dirs$out_tables)

# -------------------- guardar outputs --------------------
save_rds(desemp0, CFG$paths$unemp_individual_2008_2022)
save_table_csv(desemp0, "eod_unemployment_individual_2008_2022_extract_like", dir = CFG$dirs$out_tables)

message("✔ 08 listo: base individual desempleo guardada (interim) + auditorías (tables + validation).")
