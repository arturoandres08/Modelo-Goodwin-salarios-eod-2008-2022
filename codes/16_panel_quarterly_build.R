# codes/16_panel_quarterly_build.R
# ===============================================================
# 16) Panel final (2008Q1–2022Q4)
# Panel base (nombres fijos):
#   quarter, g_imacec, inflation, g_salario_real, u
#
# Panel extendido (con mediana):
#   quarter, g_imacec, inflation, g_salario_real, g_salario_real_mediana, u
#
# Nota:
# - g_imacec / inflation / g_salario_real / g_salario_real_mediana son Δln * 100 (QoQ).
# - El primer trimestre (2008Q1) típicamente queda NA en tasas por falta de lag.
#   Para estimación SIN NA se crean panel_estimation_* con drop_na().
# ===============================================================

source(here::here("codes","00_setup.R"))

message("\n== 16) Construcción panel trimestral ==\n")

# --------------------------
# Helpers robustos
# --------------------------
to_q_int <- function(x) {
  if (is.numeric(x)) return(as.integer(x))
  as.integer(stringr::str_extract(toupper(as.character(x)), "[1-4]"))
}

quarter_str <- function(year, q) paste0(as.integer(year), "Q", as.integer(q))

# --------------------------
# 0) Cargar inputs procesados (desde CFG)
# --------------------------
stopifnot(
  file.exists(CFG$paths$imacec_quarterly),
  file.exists(CFG$paths$ipc_monthly),
  file.exists(CFG$paths$real_quarterly),
  file.exists(CFG$paths$unemp_quarterly)
)

imacec <- load_rds(CFG$paths$imacec_quarterly) %>% janitor::clean_names()
ipc_m  <- load_rds(CFG$paths$ipc_monthly) %>% janitor::clean_names()
wage   <- load_rds(CFG$paths$real_quarterly) %>% janitor::clean_names()
unemp  <- load_rds(CFG$paths$unemp_quarterly) %>% janitor::clean_names()

# --------------------------
# 1) IMACEC: g_imacec = 100*Δln(imacec_indice)
# --------------------------
stopifnot(all(c("year","trimestre","imacec_indice") %in% names(imacec)))

imacec_q <- imacec %>%
  dplyr::mutate(
    year = as.integer(year),
    trimestre = to_q_int(trimestre),
    quarter = quarter_str(year, trimestre),
    imacec_indice = as.numeric(imacec_indice)
  ) %>%
  dplyr::filter(
    dplyr::between(year, CFG$params$year_min, CFG$params$year_max),
    trimestre %in% 1:4,
    !is.na(imacec_indice)
  ) %>%
  dplyr::arrange(year, trimestre) %>%
  dplyr::mutate(
    g_imacec = 100 * (log(imacec_indice) - log(dplyr::lag(imacec_indice)))
  ) %>%
  dplyr::select(year, trimestre, quarter, g_imacec)

# --------------------------
# 2) IPC mensual -> IPC trimestral (promedio) -> inflation = 100*Δln(ipc_q)
# --------------------------
ipc_idx_candidates <- c("ipc_base2018_100","ipc","cpi","index","ipc_index","cpi_index")
ipc_idx_col <- ipc_idx_candidates[ipc_idx_candidates %in% names(ipc_m)][1]
stopifnot(!is.na(ipc_idx_col))

if (!all(c("year","month") %in% names(ipc_m))) {
  stop("IPC mensual no trae year/month. Revisa el output de 05 (debería tenerlos).")
}

ipc_q <- ipc_m %>%
  dplyr::mutate(
    year  = as.integer(year),
    month = as.integer(month),
    trimestre = dplyr::case_when(
      month %in% 1:3   ~ 1L,
      month %in% 4:6   ~ 2L,
      month %in% 7:9   ~ 3L,
      month %in% 10:12 ~ 4L,
      TRUE ~ NA_integer_
    ),
    ipc_index = as.numeric(.data[[ipc_idx_col]]),
    quarter = quarter_str(year, trimestre)
  ) %>%
  dplyr::filter(
    dplyr::between(year, CFG$params$year_min, CFG$params$year_max),
    !is.na(trimestre), trimestre %in% 1:4,
    !is.na(ipc_index)
  ) %>%
  dplyr::group_by(year, trimestre, quarter) %>%
  dplyr::summarise(ipc_q = mean(ipc_index, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(year, trimestre) %>%
  dplyr::mutate(
    inflation = 100 * (log(ipc_q) - log(dplyr::lag(ipc_q)))
  ) %>%
  dplyr::select(year, trimestre, quarter, inflation)

# --------------------------
# 3) Salario real:
#    - g_salario_real = 100*Δln(mean_real)
#    - g_salario_real_mediana = 100*Δln(median_real)
#    (usamos versión winsor_p99_5 por defecto)
# --------------------------
stopifnot(all(c("version","year","trimestre","mean_real","median_real") %in% names(wage)))

wage_q <- wage %>%
  dplyr::mutate(
    version = as.character(version),
    year = as.integer(year),
    trimestre = to_q_int(trimestre),
    mean_real = as.numeric(mean_real),
    median_real = as.numeric(median_real),
    quarter = quarter_str(year, trimestre)
  ) %>%
  dplyr::filter(
    dplyr::between(year, CFG$params$year_min, CFG$params$year_max),
    trimestre %in% 1:4,
    version == "winsor_p99_5",
    !is.na(mean_real), mean_real > 0,
    !is.na(median_real), median_real > 0
  ) %>%
  dplyr::arrange(year, trimestre) %>%
  dplyr::mutate(
    g_salario_real = 100 * (log(mean_real) - log(dplyr::lag(mean_real))),
    g_salario_real_mediana = 100 * (log(median_real) - log(dplyr::lag(median_real)))
  ) %>%
  dplyr::select(year, trimestre, quarter, g_salario_real, g_salario_real_mediana)

# --------------------------
# 4) Desempleo: u = tasa_desemp (en %)
# --------------------------
stopifnot(all(c("year","trimestre","tasa_desemp") %in% names(unemp)))

unemp_q <- unemp %>%
  dplyr::mutate(
    year = as.integer(year),
    trimestre = to_q_int(trimestre),
    quarter = quarter_str(year, trimestre),
    u = as.numeric(tasa_desemp)
  ) %>%
  dplyr::filter(
    dplyr::between(year, CFG$params$year_min, CFG$params$year_max),
    trimestre %in% 1:4,
    !is.na(u)
  ) %>%
  dplyr::arrange(year, trimestre) %>%
  dplyr::select(year, trimestre, quarter, u)

# --------------------------
# 5) Merge final
# --------------------------
panel_all <- dplyr::left_join(imacec_q, ipc_q,  by = c("year","trimestre","quarter")) %>%
  dplyr::left_join(wage_q,  by = c("year","trimestre","quarter")) %>%
  dplyr::left_join(unemp_q, by = c("year","trimestre","quarter")) %>%
  dplyr::arrange(year, trimestre)

# Panel base (5 columnas, compatible con lo anterior)
panel_full <- panel_all %>%
  dplyr::select(quarter, g_imacec, inflation, g_salario_real, u)

# Panel extendido (6 columnas, con mediana)
panel_full_with_median <- panel_all %>%
  dplyr::select(quarter, g_imacec, inflation, g_salario_real, g_salario_real_mediana, u)

# --------------------------
# 6) Checks “profe” (sobre el panel extendido, que es el más exigente)
# --------------------------
expected <- tidyr::expand_grid(
  year = CFG$params$year_min:CFG$params$year_max,
  trimestre = 1:4
) %>%
  dplyr::mutate(quarter = quarter_str(year, trimestre)) %>%
  dplyr::select(quarter)

stopifnot(nrow(panel_full_with_median) == nrow(expected))
stopifnot(all(expected$quarter %in% panel_full_with_median$quarter))

na_counts <- colSums(is.na(panel_full_with_median))
print(na_counts)

first_q <- "2008Q1"
na_rows <- panel_full_with_median %>%
  dplyr::mutate(any_na = dplyr::if_any(dplyr::everything(), is.na)) %>%
  dplyr::filter(any_na) %>%
  dplyr::pull(quarter)

stopifnot(all(na_rows %in% first_q))

# Paneles para estimación SIN NA (robustos)
panel_estimation <- panel_full %>%
  tidyr::drop_na(g_imacec, inflation, g_salario_real, u)

panel_estimation_with_median <- panel_full_with_median %>%
  tidyr::drop_na(g_imacec, inflation, g_salario_real, g_salario_real_mediana, u)

stopifnot(sum(is.na(panel_estimation)) == 0)
stopifnot(sum(is.na(panel_estimation_with_median)) == 0)

# --------------------------
# 7) Guardar outputs
# --------------------------
# Base (como antes)
save_rds(panel_full, fs::path(CFG$dirs$processed, "panel_quarterly_2008Q1_2022Q4.rds"))
save_table_csv(panel_full, "panel_quarterly_2008Q1_2022Q4", dir = CFG$dirs$out_tables)

save_rds(panel_estimation, fs::path(CFG$dirs$processed, "panel_estimation_2008Q2_2022Q4_noNA.rds"))
save_table_csv(panel_estimation, "panel_estimation_2008Q2_2022Q4_noNA", dir = CFG$dirs$out_tables)

# Extendido (nuevo)
save_rds(panel_full_with_median, fs::path(CFG$dirs$processed, "panel_quarterly_with_median_2008Q1_2022Q4.rds"))
save_table_csv(panel_full_with_median, "panel_quarterly_with_median_2008Q1_2022Q4", dir = CFG$dirs$out_tables)

save_rds(panel_estimation_with_median, fs::path(CFG$dirs$processed, "panel_estimation_with_median_2008Q2_2022Q4_noNA.rds"))
save_table_csv(panel_estimation_with_median, "panel_estimation_with_median_2008Q2_2022Q4_noNA", dir = CFG$dirs$out_tables)

message("✔ 16 listo: panel_full (base) + panel_full_with_median (extendido) y sus versiones sin NA guardadas.")