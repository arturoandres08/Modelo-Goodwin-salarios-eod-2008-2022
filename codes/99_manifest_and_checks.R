# codes/99_manifest_and_checks.R
source(here::here("codes","00_setup.R"))

message("\n== 99) CIERRE: checks + manifest + sessionInfo ==\n")

# ------------------------------------------------------------
# A) CHECKS DE ARCHIVOS “CRÍTICOS” (outputs esperados)
# ------------------------------------------------------------
expected_files <- c(
  # ---- RDS processed clave ----
  CFG$paths$real_quarterly,
  CFG$paths$unemp_quarterly,
  CFG$paths$imacec_quarterly,
  CFG$paths$employment_quarterly,
  CFG$paths$prod_total_quarterly,
  CFG$paths$prod_nonmining_quarterly,
  CFG$paths$cv_adjusted_sinwinsor,
  CFG$paths$cv_adjusted_winsor,
  
  # ---- FIGURES clave (según tus scripts actuales) ----
  fs::path(CFG$dirs$out_figures, "wages_real_mean_with_sd.png"),
  fs::path(CFG$dirs$out_figures, "wages_real_mean_with_sd.pdf"),
  fs::path(CFG$dirs$out_figures, "wages_real_sd.png"),
  fs::path(CFG$dirs$out_figures, "wages_real_sd.pdf"),
  
  fs::path(CFG$dirs$out_figures, "unemployment_rate_quarterly_2008_2022.png"),
  fs::path(CFG$dirs$out_figures, "unemployment_rate_quarterly_2008_2022.pdf"),
  
  fs::path(CFG$dirs$out_figures, "productivity_total_base2018_100.png"),
  fs::path(CFG$dirs$out_figures, "productivity_total_base2018_100.pdf"),
  fs::path(CFG$dirs$out_figures, "productivity_nonmining_base2018_100.png"),
  fs::path(CFG$dirs$out_figures, "productivity_nonmining_base2018_100.pdf"),
  fs::path(CFG$dirs$out_figures, "productivity_total_vs_nonmining_base2018_100.png"),
  fs::path(CFG$dirs$out_figures, "productivity_total_vs_nonmining_base2018_100.pdf"),
  fs::path(CFG$dirs$out_figures, "imacec_total_vs_employment_base2018_100.png"),
  fs::path(CFG$dirs$out_figures, "imacec_total_vs_employment_base2018_100.pdf"),
  
  fs::path(CFG$dirs$out_figures, "cv_adjusted_mean_comparison_real_quarterly_2008_2022.png"),
  fs::path(CFG$dirs$out_figures, "cv_adjusted_mean_comparison_real_quarterly_2008_2022.pdf")
)

missing <- expected_files[!file.exists(expected_files)]
if (length(missing) > 0) {
  message("❌ FALTAN outputs esperados:\n- ", paste(missing, collapse = "\n- "))
  stop("Cierre falló: faltan archivos. Revisa qué script no generó outputs.")
} else {
  message("✅ Outputs críticos encontrados (OK).")
}

# ------------------------------------------------------------
# B) CHECKS DE INTEGRIDAD (RDS clave)
#     (son checks “duros” para errores obvios + warnings suaves)
# ------------------------------------------------------------

# helper
check_has <- function(df, cols) stopifnot(all(cols %in% names(df)))

# 1) real_quarterly
rea <- load_rds(CFG$paths$real_quarterly)
rea <- rea %>% janitor::clean_names()

check_has(rea, c("version","year","trimestre","t_index"))
if (!("mean_real" %in% names(rea)) && ("media_pond_real" %in% names(rea))) {
  warning("real_quarterly: no veo mean_real, pero sí media_pond_real. (Ok si tu pipeline usa media_pond_real).")
}
if ("sd_real" %in% names(rea)) {
  if (any(rea$sd_real < 0, na.rm = TRUE)) stop("real_quarterly: hay sd_real < 0 (imposible).")
} else {
  warning("real_quarterly: no veo sd_real. Si lo necesitas para 07/14/15, revisa el 06.")
}

# versiones esperadas
vers_ok <- all(c("sin_winsor","winsor_p99_5") %in% unique(rea$version))
if (!vers_ok) warning("real_quarterly: faltan versiones (esperaba sin_winsor y winsor_p99_5).")

# 2) desempleo trimestral
u <- load_rds(CFG$paths$unemp_quarterly)
check_has(u, c("year","trimestre","t_index","tasa_desemp","fuerza_trab"))
if (any(u$tasa_desemp < 0 | u$tasa_desemp > 100, na.rm = TRUE)) stop("unemp_quarterly: tasa_desemp fuera de [0,100].")
if (any(u$fuerza_trab <= 0, na.rm = TRUE)) warning("unemp_quarterly: hay fuerza_trab <= 0 (revisar aud_zero_force).")

# 3) IMACEC trimestral índices
im <- load_rds(CFG$paths$imacec_quarterly)
check_has(im, c("year","trimestre","t_index","imacec_indice","imacec_no_minero_indice"))
# base 2018 ≈ 100
base_check_im <- im %>% dplyr::filter(year == CFG$params$base_year) %>%
  dplyr::summarise(
    m_total = mean(imacec_indice, na.rm = TRUE),
    m_nm    = mean(imacec_no_minero_indice, na.rm = TRUE)
  )
if (is.finite(base_check_im$m_total) && abs(base_check_im$m_total - 100) > 1) warning("IMACEC total: base 2018 no está ~100 (ojo).")
if (is.finite(base_check_im$m_nm)    && abs(base_check_im$m_nm    - 100) > 1) warning("IMACEC no minero: base 2018 no está ~100 (ojo).")

# 4) empleo índice
emp <- load_rds(CFG$paths$employment_quarterly)
check_has(emp, c("year","trimestre","t_index","empleo_indice"))
base_emp <- emp %>% dplyr::filter(year == CFG$params$base_year) %>%
  dplyr::summarise(m = mean(empleo_indice, na.rm = TRUE)) %>% dplyr::pull(m)
if (is.finite(base_emp) && abs(base_emp - 100) > 1) warning("Empleo: base 2018 no está ~100 (ojo).")

# 5) productividad índices
pt <- load_rds(CFG$paths$prod_total_quarterly)
pn <- load_rds(CFG$paths$prod_nonmining_quarterly)
check_has(pt, c("year","trimestre","prod_indice"))
check_has(pn, c("year","trimestre","prod_indice"))

base_pt <- pt %>% dplyr::filter(year == CFG$params$base_year) %>%
  dplyr::summarise(m = mean(prod_indice, na.rm = TRUE)) %>% dplyr::pull(m)
base_pn <- pn %>% dplyr::filter(year == CFG$params$base_year) %>%
  dplyr::summarise(m = mean(prod_indice, na.rm = TRUE)) %>% dplyr::pull(m)
if (is.finite(base_pt) && abs(base_pt - 100) > 1) warning("Prod total: base 2018 no está ~100 (ojo).")
if (is.finite(base_pn) && abs(base_pn - 100) > 1) warning("Prod no minero: base 2018 no está ~100 (ojo).")

message("✅ Checks de integridad RDS: OK (warnings pueden ser normales si están justificados).")

# ------------------------------------------------------------
# C) MANIFEST DE OUTPUTS (lista completa + md5)
# ------------------------------------------------------------
dirs_to_manifest <- c(
  CFG$dirs$processed,
  CFG$dirs$out_tables,
  CFG$dirs$out_figures,
  CFG$dirs$aud_tab,
  CFG$dirs$aud_fig,
  CFG$dirs$aud_val,
  CFG$dirs$logs
)

files_all <- unlist(lapply(dirs_to_manifest, function(d) {
  if (fs::dir_exists(d)) fs::dir_ls(d, recurse = TRUE, type = "file") else character(0)
}))

manifest <- tibble::tibble(
  path = files_all,
  size_bytes = fs::file_size(files_all),
  modified = fs::file_info(files_all)$modification_time,
  md5 = as.character(tools::md5sum(files_all))
) %>%
  dplyr::arrange(path)

ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
manifest_path <- fs::path(CFG$dirs$logs, paste0("manifest_outputs_", ts, ".csv"))
readr::write_csv(manifest, manifest_path)
message("✅ Manifest guardado en: ", manifest_path)

# ------------------------------------------------------------
# D) sessionInfo (reproducibilidad)
# ------------------------------------------------------------
sess_path <- fs::path(CFG$dirs$logs, paste0("sessionInfo_", ts, ".txt"))
writeLines(c(capture.output(sessionInfo())), sess_path)
message("✅ sessionInfo guardado en: ", sess_path)

# ------------------------------------------------------------
# E) (Opcional) Snapshot ZIP de outputs (para no perder nada)
# ------------------------------------------------------------
snap_dir <- here::here("snapshots")
fs::dir_create(snap_dir)
zip_path <- fs::path(snap_dir, paste0("snapshot_outputs_", ts, ".zip"))

to_zip <- files_all[fs::path_ext(files_all) %in% c("rds","csv","png","pdf","txt","xlsx")]
if (length(to_zip) > 0) {
  old <- getwd()
  setwd(CFG$project$root)
  on.exit(setwd(old), add = TRUE)
  
  # paths relativos para que el zip quede “limpio”
  rel <- fs::path_rel(to_zip, start = CFG$project$root)
  utils::zip(zipfile = zip_path, files = rel)
  message("✅ Snapshot ZIP guardado en: ", zip_path)
} else {
  warning("No había archivos para zippear (raro).")
}

message("\n✔ CIERRE COMPLETO.\n")
