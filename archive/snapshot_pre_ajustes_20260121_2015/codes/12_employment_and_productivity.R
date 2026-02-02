# codes/12_employment_and_productivity_indices.R
source(here("codes", "00_setup.R"))

desemp_tri <- load_rds(CFG$paths$unemp_quarterly)
imacec_tri <- load_rds(CFG$paths$imacec_quarterly)

# ---------------------------------------------------------------
# 0) Normalizadores (robustos)
# ---------------------------------------------------------------
to_q_int <- function(x) {
  # acepta "T1", "t2", "1", 1L, etc.
  if (is.numeric(x)) return(as.integer(x))
  as.integer(stringr::str_extract(toupper(as.character(x)), "[1-4]"))
}

# ---------------------------------------------------------------
# 1) Empleo trimestral (ocupados) + índice base 2018=100
# ---------------------------------------------------------------
empleo_tri <- desemp_tri %>%
  dplyr::mutate(
    trimestre = to_q_int(trimestre),
    year = as.integer(year),
    t_index = as.numeric(t_index)
  ) %>%
  dplyr::transmute(
    year, trimestre, t_index,
    ocupados = as.numeric(ocupados)
  ) %>%
  dplyr::filter(
    dplyr::between(year, CFG$params$year_min, CFG$params$year_max),
    !is.na(trimestre), trimestre %in% 1:4,
    !is.na(ocupados)
  ) %>%
  dplyr::arrange(t_index)

base_emp <- empleo_tri %>%
  dplyr::filter(year == CFG$params$base_year) %>%
  dplyr::summarise(base = mean(ocupados, na.rm = TRUE)) %>%
  dplyr::pull(base)

stopifnot(length(base_emp) == 1, is.finite(base_emp), base_emp > 0)

empleo_tri <- empleo_tri %>%
  dplyr::mutate(empleo_indice = ocupados / base_emp * 100)

save_rds(empleo_tri, CFG$paths$employment_quarterly)

# ---------------------------------------------------------------
# 2) Productividad = IMACEC índice / empleo índice
#    y re-base 2018=100
# ---------------------------------------------------------------
imacec_key_total <- imacec_tri %>%
  dplyr::mutate(
    year = as.integer(year),
    trimestre = to_q_int(trimestre),
    t_index = as.numeric(t_index)
  ) %>%
  dplyr::select(year, trimestre, t_index, imacec_indice)

prod_total <- imacec_key_total %>%
  dplyr::left_join(
    empleo_tri %>% dplyr::select(year, trimestre, t_index, empleo_indice),
    by = c("year", "trimestre", "t_index")
  ) %>%
  dplyr::mutate(
    prod_raw = dplyr::if_else(
      !is.na(empleo_indice) & empleo_indice > 0,
      imacec_indice / empleo_indice,
      NA_real_
    )
  ) %>%
  dplyr::arrange(t_index)

# Chequeo: ¿se cayó el join?
share_na_join <- mean(is.na(prod_total$empleo_indice))
if (is.finite(share_na_join) && share_na_join > 0.05) {
  warning("Más del 5% de las filas quedaron sin empleo_indice tras el join (NA). Revisa trimestre/t_index.")
}

base_prod_total <- prod_total %>%
  dplyr::filter(year == CFG$params$base_year) %>%
  dplyr::summarise(base = mean(prod_raw, na.rm = TRUE)) %>%
  dplyr::pull(base)

stopifnot(length(base_prod_total) == 1, is.finite(base_prod_total), base_prod_total > 0)

prod_total <- prod_total %>%
  dplyr::mutate(prod_indice = prod_raw / base_prod_total * 100)

# ---- No minero ----
imacec_key_nm <- imacec_tri %>%
  dplyr::mutate(
    year = as.integer(year),
    trimestre = to_q_int(trimestre),
    t_index = as.numeric(t_index)
  ) %>%
  dplyr::select(year, trimestre, t_index, imacec_no_minero_indice)

prod_no_minero <- imacec_key_nm %>%
  dplyr::left_join(
    empleo_tri %>% dplyr::select(year, trimestre, t_index, empleo_indice),
    by = c("year", "trimestre", "t_index")
  ) %>%
  dplyr::mutate(
    prod_raw = dplyr::if_else(
      !is.na(empleo_indice) & empleo_indice > 0,
      imacec_no_minero_indice / empleo_indice,
      NA_real_
    )
  ) %>%
  dplyr::arrange(t_index)

base_prod_nm <- prod_no_minero %>%
  dplyr::filter(year == CFG$params$base_year) %>%
  dplyr::summarise(base = mean(prod_raw, na.rm = TRUE)) %>%
  dplyr::pull(base)

stopifnot(length(base_prod_nm) == 1, is.finite(base_prod_nm), base_prod_nm > 0)

prod_no_minero <- prod_no_minero %>%
  dplyr::mutate(prod_indice = prod_raw / base_prod_nm * 100)

# ---------------------------------------------------------------
# 3) Outputs
# ---------------------------------------------------------------
save_rds(prod_total,     CFG$paths$prod_total_quarterly)
save_rds(prod_no_minero, CFG$paths$prod_nonmining_quarterly)

save_table_csv(empleo_tri,     "employment_quarterly_base2018_100", dir = CFG$dirs$out_tables)
save_table_csv(prod_total,     "productivity_total_quarterly_base2018_100", dir = CFG$dirs$out_tables)
save_table_csv(prod_no_minero, "productivity_nonmining_quarterly_base2018_100", dir = CFG$dirs$out_tables)

message("✔ 12 listo: empleo y productividad guardados")
