# codes/02_eod_extract_individuals.R
source(here::here("codes","00_setup.R"))

dir_eod <- CFG$dirs$data_eod
stopifnot(fs::dir_exists(dir_eod))

cand_ingreso <- c("ing.*suel", "ing.*sal", "suel.*liqu", "suel.*mono", "salario", "ingreso.*labor")
cand_peso    <- c("^pond$", "ponder", "factor", "fact.*exp", "peso", "w[0-9]*")
cand_mes     <- c("^mes$", "mes_lev$", "meslev$", "mes_enc$", "meslevantamiento$", "mes_muestra$",
                  "mes_entrevista$", "mes_levantamiento$")
cand_year    <- c("^year$", "^ano$", "^agno$", "^anio$", "year_enc$", "ano_enc$", "agno_enc$",
                  "anio_enc$", "year_var$", "ano_var$", "agno_var$", "anio_var$")

pick_first <- function(nms, patterns) {
  for (p in patterns) {
    hit <- stringr::str_which(nms, stringr::regex(p, ignore_case = TRUE))
    if (length(hit)) return(nms[hit[1]])
  }
  NA_character_
}

read_sav_safely <- function(f) {
  base <- basename(f)
  df1 <- try(haven::read_sav(f, user_na = TRUE), silent = TRUE)
  if (!inherits(df1, "try-error")) return(df1)
  
  message("⚠ Archivo ", base, ": falló encoding default. Intentando latin1...")
  df2 <- try(haven::read_sav(f, user_na = TRUE, encoding = "latin1"), silent = TRUE)
  if (!inherits(df2, "try-error")) {
    message("✔ Archivo ", base, ": leído con latin1")
    return(df2)
  }
  
  message("✖ Archivo ", base, ": no se pudo leer.")
  NULL
}

# OJO: recurse = TRUE por seguridad (si hay subcarpetas por año/mes)
eod_files <- fs::dir_ls(
  dir_eod,
  regexp  = "\\.(sav|dta)$",
  type    = "file",
  recurse = TRUE
)
stopifnot(length(eod_files) > 0)

extract_one <- function(f) {
  base <- basename(f)
  ext  <- tolower(fs::path_ext(f))
  message("Leyendo: ", base)
  
  df <- tryCatch(
    {
      if (ext == "sav") read_sav_safely(f)
      else if (ext == "dta") haven::read_dta(f)
      else NULL
    },
    error = function(e) {
      message("✖ Error leyendo ", base, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(df)) return(NULL)
  
  nms   <- names(df)
  v_ing <- pick_first(nms, cand_ingreso)
  v_w   <- pick_first(nms, cand_peso)
  v_mes <- pick_first(nms, cand_mes)
  v_y   <- pick_first(nms, cand_year)
  
  if (is.na(v_ing) || is.na(v_w)) {
    message("  → se omite ", base, " (no ingreso o no factor)")
    return(NULL)
  }
  
  year_col_raw <- if (!is.na(v_y)) df[[v_y]] else NA
  year_lab_chr <- suppressWarnings(as.character(haven::as_factor(year_col_raw)))
  year_lab_num <- suppressWarnings(readr::parse_integer(year_lab_chr, na = c("", "NA")))
  year_num     <- suppressWarnings(as.numeric(year_col_raw))
  year_col     <- dplyr::coalesce(year_lab_num, year_num)
  year_col[year_col < 1900 | year_col > 2100] <- NA_integer_
  
  year_file <- suppressWarnings(as.integer(stringr::str_extract(base, "(19|20)\\d{2}")))
  mes_val   <- suppressWarnings(as.integer(if (!is.na(v_mes)) df[[v_mes]] else NA))
  
  tibble::tibble(
    archivo   = tolower(base),
    ingsueld  = suppressWarnings(as.numeric(df[[v_ing]])),
    factor_hd = suppressWarnings(as.numeric(df[[v_w]])),
    mes       = mes_val,
    year      = dplyr::coalesce(year_col, year_file),
    trimestre = ifelse(!is.na(mes_val), paste0("T", ceiling(mes_val / 3)), NA_character_),
    
    # meta para auditoría (liviano)
    v_ing = v_ing,
    v_w   = v_w,
    v_mes = v_mes,
    v_y   = v_y,
    n_raw = nrow(df)
  )
}

ings_nom_all <- purrr::map_dfr(eod_files, extract_one) %>%
  janitor::clean_names()

stopifnot(nrow(ings_nom_all) > 0)

# Auditoría liviana (por archivo)
aud_02 <- ings_nom_all %>%
  dplyr::distinct(archivo, v_ing, v_w, v_mes, v_y, n_raw) %>%
  dplyr::arrange(archivo)

save_table_csv(aud_02, "audit_02_detected_columns_by_file", dir = CFG$dirs$validation)

# Guardar dataset sin las columnas meta (deja lo esencial para el pipeline)
ings_nom <- ings_nom_all %>%
  dplyr::select(archivo, ingsueld, factor_hd, mes, year, trimestre)

save_rds(ings_nom, CFG$paths$eod_individual_raw)

# Si este CSV queda gigante, lo comentas y te quedas con el RDS + auditoría
save_table_csv(ings_nom, "eod_extract_individual_raw_like", dir = CFG$dirs$out_tables)

message("✔ 02 listo: extracción individual guardada en interim")
message("✔ Auditoría 02 guardada en data/validation")
