# codes/17_svar_core_inputs_build.R
# ==========================================================
# 17) Inputs SVAR núcleo — Chile 2008–2022
# Construye dataset de niveles para SVAR: (ga, gw, v)
# Output:
#   processed/svar_core_inputs_2008Q1_2022Q4.rds
#   tables/svar_core_inputs_2008Q1_2022Q4.csv
# ==========================================================

source(here::here("codes","00_setup.R"))
message("\n== 17) Construcción inputs SVAR núcleo ==\n")

# ---- helpers ----
to_q_int <- function(x) {
  if (is.numeric(x)) return(as.integer(x))
  as.integer(stringr::str_extract(toupper(as.character(x)), "[1-4]"))
}
quarter_str <- function(year, q) paste0(as.integer(year), "Q", as.integer(q))

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

# ---- 0) cargar fuentes mínimas ----
stopifnot(
  file.exists(CFG$paths$imacec_quarterly),
  file.exists(CFG$paths$unemp_quarterly),
  file.exists(CFG$paths$real_quarterly)
)

imacec <- load_rds(CFG$paths$imacec_quarterly) %>% janitor::clean_names()
unemp  <- load_rds(CFG$paths$unemp_quarterly)  %>% janitor::clean_names()
wage   <- load_rds(CFG$paths$real_quarterly)   %>% janitor::clean_names()

# ---- 1) IMACEC nivel (índice) ----
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
  dplyr::select(year, trimestre, quarter, imacec_indice) %>%
  dplyr::arrange(year, trimestre)

# ---- 2) Empleo (índice) desde output de tu 12 ----
# Si tienes una ruta explícita en CFG, úsala.
emp_path <- NULL
if (!is.null(CFG$paths$employment_quarterly) && file.exists(CFG$paths$employment_quarterly)) {
  emp_path <- CFG$paths$employment_quarterly
} else {
  # si no existe en CFG, busca en processed algo “employment/empleo”
  candidates <- fs::dir_ls(CFG$dirs$processed, glob = "*.rds")
  hits <- candidates[stringr::str_detect(tolower(basename(candidates)), "employment|empleo")]
  if (length(hits) >= 1) emp_path <- hits[1]
}

if (is.null(emp_path)) {
  stop("No encontré output de empleo del script 12 en processed/. ",
       "Solución: define CFG$paths$employment_quarterly apuntando al RDS correcto, ",
       "o renombra el archivo para que contenga 'employment' o 'empleo'.")
}

emp <- load_rds(emp_path) %>% janitor::clean_names()
stopifnot(all(c("year","trimestre") %in% names(emp)))

L_col <- pick_first_existing(names(emp), c(
  "employment_indice","empleo_indice","employment_index","empleo_index","l_indice","l_index"
))
if (is.na(L_col)) {
  stop("No pude detectar la columna índice de empleo en el output del 12. ",
       "Muéstrame names(emp) y lo ajusto exacto.")
}

emp_q <- emp %>%
  dplyr::mutate(
    year = as.integer(year),
    trimestre = to_q_int(trimestre),
    quarter = quarter_str(year, trimestre),
    L_indice = as.numeric(.data[[L_col]])
  ) %>%
  dplyr::filter(
    dplyr::between(year, CFG$params$year_min, CFG$params$year_max),
    trimestre %in% 1:4,
    !is.na(L_indice)
  ) %>%
  dplyr::select(year, trimestre, quarter, L_indice) %>%
  dplyr::arrange(year, trimestre)

# ---- 3) Productividad (índice) ----
prod_q <- imacec_q %>%
  dplyr::left_join(emp_q, by = c("year","trimestre","quarter")) %>%
  dplyr::mutate(a_prod = safe_div(imacec_indice, L_indice))

if (any(is.na(prod_q$a_prod))) stop("a_prod quedó con NA: mismatch de llaves IMACEC vs empleo.")

# ---- 4) Desempleo: u (%) y v ----
stopifnot(all(c("year","trimestre","tasa_desemp") %in% names(unemp)))

unemp_q <- unemp %>%
  dplyr::mutate(
    year = as.integer(year),
    trimestre = to_q_int(trimestre),
    quarter = quarter_str(year, trimestre),
    u = as.numeric(tasa_desemp),
    v = 1 - (u / 100)
  ) %>%
  dplyr::filter(
    dplyr::between(year, CFG$params$year_min, CFG$params$year_max),
    trimestre %in% 1:4,
    !is.na(u), !is.na(v)
  ) %>%
  dplyr::select(year, trimestre, quarter, u, v) %>%
  dplyr::arrange(year, trimestre)

# ---- 5) Salarios reales (niveles) para robustez ----
stopifnot(all(c("version","year","trimestre","mean_real","median_real") %in% names(wage)))

w_base <- wage %>%
  dplyr::mutate(
    version = as.character(version),
    year = as.integer(year),
    trimestre = to_q_int(trimestre),
    quarter = quarter_str(year, trimestre),
    mean_real = as.numeric(mean_real),
    median_real = as.numeric(median_real)
  ) %>%
  dplyr::filter(
    dplyr::between(year, CFG$params$year_min, CFG$params$year_max),
    trimestre %in% 1:4
  )

w_winsor <- w_base %>%
  dplyr::filter(version == "winsor_p99_5") %>%
  dplyr::select(
    year, trimestre, quarter,
    w_real_mean_winsor = mean_real,
    w_real_median      = median_real
  )

w_nowins <- w_base %>%
  dplyr::filter(version == "sin_winsor") %>%
  dplyr::select(
    year, trimestre, quarter,
    w_real_mean_nowinsor = mean_real
  )

# ---- 6) merge final ----
svar_inputs <- prod_q %>%
  dplyr::left_join(unemp_q, by = c("year","trimestre","quarter")) %>%
  dplyr::left_join(w_winsor, by = c("year","trimestre","quarter")) %>%
  dplyr::left_join(w_nowins, by = c("year","trimestre","quarter")) %>%
  dplyr::arrange(year, trimestre) %>%
  dplyr::select(
    year, trimestre, quarter,
    imacec_indice, L_indice, a_prod,
    u, v,
    w_real_mean_winsor, w_real_mean_nowinsor, w_real_median
  )

# ---- 7) checks duros (60 trimestres, sin NA) ----
expected <- tidyr::expand_grid(
  year = CFG$params$year_min:CFG$params$year_max,
  trimestre = 1:4
) %>%
  dplyr::mutate(quarter = quarter_str(year, trimestre))

stopifnot(nrow(svar_inputs) == nrow(expected))
stopifnot(all(expected$quarter %in% svar_inputs$quarter))

na_counts <- colSums(is.na(svar_inputs))
print(na_counts)
if (any(na_counts > 0)) stop("Hay NA en svar_inputs. Revisa joins/columnas antes de SVAR.")

# ---- 8) guardar ----
out_rds <- fs::path(CFG$dirs$processed, "svar_core_inputs_2008Q1_2022Q4.rds")
save_rds(svar_inputs, out_rds)
save_table_csv(svar_inputs, "svar_core_inputs_2008Q1_2022Q4", dir = CFG$dirs$out_tables)

message("✔ 17 listo: inputs SVAR guardados en processed/ y tables/")