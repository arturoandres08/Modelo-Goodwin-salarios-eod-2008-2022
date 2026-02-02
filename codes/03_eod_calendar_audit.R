# codes/03_eod_calendar_audit.R
source(here::here("codes","00_setup.R"))

ings_nom <- load_rds(CFG$paths$eod_individual_raw)

mes_map <- c(
  "enero"="01","febrero"="02","marzo"="03","abril"="04","mayo"="05","junio"="06",
  "julio"="07","agosto"="08","septiembre"="09","setiembre"="09","octubre"="10",
  "noviembre"="11","diciembre"="12",
  "ene"="01","feb"="02","mar"="03","abr"="04","may"="05","jun"="06",
  "jul"="07","ago"="08","sep"="09","oct"="10","nov"="11","dic"="12"
)

infer_month_from_filename <- function(fname){
  f <- tolower(basename(fname))
  
  pat   <- paste(names(mes_map), collapse = "|")
  m_chr <- stringr::str_extract(f, pat)
  if (!is.na(m_chr)) return(unname(mes_map[m_chr]))
  
  mm <- stringr::str_match(f, "([01][0-9])")[,2]
  if (!is.na(mm) && mm %in% sprintf("%02d", 1:12)) return(mm)
  
  if (stringr::str_detect(f, "t1")) return("03")
  if (stringr::str_detect(f, "t2")) return("06")
  if (stringr::str_detect(f, "t3")) return("09")
  if (stringr::str_detect(f, "t4")) return("12")
  
  NA_character_
}
infer_month_vec <- function(x) vapply(x, infer_month_from_filename, character(1))

# ------------------------------------------------------------
# 1) Normalización robusta: month + trimestre + t_index
# ------------------------------------------------------------
ing0 <- ings_nom %>%
  dplyr::mutate(
    # regla fuerte de limpieza (tu regla)
    ingsueld = dplyr::if_else(
      ingsueld > CFG$params$income_cap_nominal | ingsueld < 0,
      NA_real_, suppressWarnings(as.numeric(ingsueld))
    ),
    factor_hd = suppressWarnings(as.numeric(factor_hd)),
    
    # year: columna (si viene) y fallback por nombre de archivo
    year_col  = suppressWarnings(as.integer(year)),
    year_file = suppressWarnings(as.integer(stringr::str_extract(archivo, "(19|20)\\d{2}"))),
    year_final = dplyr::coalesce(
      dplyr::if_else(dplyr::between(year_col, 1900L, 2100L), year_col, NA_integer_),
      year_file
    ),
    
    # ---------- MES robusto ----------
    # 1) intenta mes numérico 1..12
    mes_int = suppressWarnings(as.integer(mes)),
    mes_int = dplyr::if_else(dplyr::between(mes_int, 1L, 12L), mes_int, NA_integer_),
    
    # 2) si mes viene etiquetado/texto: as_factor -> map a 01..12 si aplica
    mes_lab = suppressWarnings(tolower(trimws(as.character(haven::as_factor(mes))))),
    mes_lab = dplyr::if_else(!is.na(mes_lab) & mes_lab %in% names(mes_map), unname(mes_map[mes_lab]), mes_lab),
    
    # 3) extrae número 1..12 desde texto si viene “3”, “03”, etc.
    mes_from_text = stringr::str_match(mes_lab, "(?:^|\\D)(1[0-2]|0?[1-9])(?:$|\\D)")[,2],
    mes_from_text = dplyr::if_else(!is.na(mes_from_text), stringr::str_pad(mes_from_text, 2, pad = "0"), NA_character_),
    
    # 4) construir month_chr con coalesce: num -> texto -> filename
    month_chr = dplyr::coalesce(
      dplyr::if_else(!is.na(mes_int), stringr::str_pad(mes_int, 2, pad = "0"), NA_character_),
      mes_from_text,
      infer_month_vec(archivo)
    ),
    month = suppressWarnings(as.integer(month_chr)),
    
    # ---------- TRIMESTRE robusto ----------
    tri_raw = toupper(trimws(as.character(trimestre))),
    tri_raw = dplyr::case_when(
      stringr::str_detect(tri_raw, "^T[1-4]$") ~ tri_raw,
      stringr::str_detect(tri_raw, "^[1-4]$")  ~ paste0("T", tri_raw),
      TRUE ~ NA_character_
    ),
    tri_fix = toupper(dplyr::coalesce(
      tri_raw,
      tri_from_month(month),
      tri_from_month(infer_month_vec(archivo))
    )),
    
    # índice temporal
    t_index = year_final + q_num(tri_fix) / 10
  )

save_rds(ing0, CFG$paths$eod_calendar_normalized)

# ------------------------------------------------------------
# 2) Auditoría por archivo (SIN list-columns en CSV)
#    -> recomendado: data/validation (no paper)
# ------------------------------------------------------------
aud_archivo <- ing0 %>%
  dplyr::group_by(archivo, year_final) %>%
  dplyr::summarise(
    n_obs      = dplyr::n(),
    meses_orig = paste(sort(unique(mes[!is.na(mes)])), collapse = ","),
    meses_norm = paste(sort(unique(month[!is.na(month)])), collapse = ","),
    tris_final = paste(sort(unique(tri_fix[!is.na(tri_fix)])), collapse = ","),
    .groups = "drop"
  ) %>%
  dplyr::arrange(archivo, year_final)

save_table_csv(aud_archivo, "audit_month_quarter_by_file", dir = CFG$dirs$validation)

# ------------------------------------------------------------
# 3) Calendario esperado vs observado (1957–year_max)
#    -> data/validation
# ------------------------------------------------------------
expected_months <- tibble::tibble(year_final = 1957L:CFG$params$year_max) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    mes = list(
      if (year_final < 1979L) 6L
      else if (dplyr::between(year_final, 1980L, 1996L)) c(3L, 6L)
      else c(3L, 6L, 9L, 12L)
    )
  ) %>%
  tidyr::unnest_longer(mes) %>%
  dplyr::mutate(tri_esp = tri_from_month(mes))

actual_months <- ing0 %>%
  dplyr::filter(!is.na(year_final), !is.na(month)) %>%
  dplyr::distinct(year_final, mes = month) %>%
  dplyr::mutate(tri_obs = tri_from_month(mes))

aud_mes <- dplyr::full_join(
  expected_months %>% dplyr::mutate(esperado = TRUE),
  actual_months   %>% dplyr::mutate(observado = TRUE),
  by = c("year_final", "mes")
) %>%
  dplyr::mutate(
    esperado  = dplyr::coalesce(esperado,  FALSE),
    observado = dplyr::coalesce(observado, FALSE),
    estado = dplyr::case_when(
      esperado & observado ~ "OK",
      esperado & !observado ~ "FALTA_EN_DATOS",
      !esperado & observado ~ "EXTRA_EN_DATOS",
      TRUE ~ "NINGUNO"
    )
  ) %>%
  dplyr::arrange(year_final, mes)

save_table_csv(aud_mes, "audit_expected_vs_observed_months", dir = CFG$dirs$validation)

aud_tri <- aud_mes %>%
  dplyr::mutate(trimestre = tri_from_month(mes)) %>%
  dplyr::group_by(year_final, trimestre) %>%
  dplyr::summarise(
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
  dplyr::arrange(year_final, trimestre)

save_table_csv(aud_tri, "audit_expected_vs_observed_quarters", dir = CFG$dirs$validation)

message("✔ 03 listo: calendario normalizado + auditorías (guardadas en data/validation)")
