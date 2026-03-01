# codes/19d_var_lag_grid_diagnostics.R
# ==========================================================
# 19d) Grid diagnóstico VAR por rezagos (baseline)
# - Prueba p=1..6 (o el rango que definas)
# - Reporta estabilidad y autocorrelación residual
# - Output:
#   tables/svar_core/lag_grid/var_lag_grid_baseline.csv
# ==========================================================

source(here::here("codes","00_setup.R"))
message("\n== 19d) Grid diagnóstico VAR por rezagos (baseline) ==\n")

pkgs <- c("zoo", "vars")
to_install <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(zoo)
library(vars)

source(here::here("codes","18_svar_core_functions.R"))  # usa build_core()

# Output dir
out_dir <- as.character(fs::path(CFG$dirs$out_tables, "svar_core", "lag_grid"))
fs::dir_create(out_dir)

# Inputs
in_path <- fs::path(CFG$dirs$processed, "svar_core_inputs_2008Q1_2022Q4.rds")
stopifnot(file.exists(in_path))
df <- load_rds(in_path) %>%
  dplyr::mutate(q = zoo::as.yearqtr(paste0(year, " Q", trimestre), format = "%Y Q%q")) %>%
  dplyr::arrange(q)

# Construir core baseline
X <- build_core(df, wage_col = "w_real_mean_winsor", a_col = "a_prod")
Xmat <- X %>% dplyr::select(ga, gw, v)

# Grid de p
p_grid <- 1:6  # <- cambia a 1:5 si quieres exactamente 1..5

one_p <- function(p) {
  var_fit <- tryCatch(
    vars::VAR(Xmat, p = as.integer(p), type = "const"),
    error = function(e) NULL
  )
  if (is.null(var_fit)) {
    return(tibble::tibble(
      p = as.integer(p),
      stable = NA,
      max_root_mod = NA_real_,
      serial_p = NA_real_,
      arch_p = NA_real_
    ))
  }
  
  roots <- vars::roots(var_fit)
  stable <- all(Mod(roots) < 1)
  max_root_mod <- max(Mod(roots))
  
  serial_p <- tryCatch(
    vars::serial.test(var_fit, lags.pt = 12, type = "PT.asymptotic")$serial$p.value,
    error = function(e) NA_real_
  )
  
  arch_p <- tryCatch(
    vars::arch.test(var_fit, lags.multi = 12)$arch.mul$p.value,
    error = function(e) NA_real_
  )
  
  tibble::tibble(
    p = as.integer(p),
    stable = stable,
    max_root_mod = max_root_mod,
    serial_p = serial_p,
    arch_p = arch_p
  )
}

res <- purrr::map_dfr(p_grid, one_p) %>%
  dplyr::arrange(p)

# Recomendación: elegir p estable con mayor serial_p (y más chico si empata)
cand <- res %>%
  dplyr::filter(stable == TRUE, !is.na(serial_p)) %>%
  dplyr::arrange(dplyr::desc(serial_p), p)

rec <- if (nrow(cand) > 0) cand[1,] else tibble::tibble()

# Guardar
out_csv <- file.path(out_dir, "var_lag_grid_baseline.csv")
readr::write_csv(res, out_csv)

message("✔ Guardado: ", out_csv)
if (nrow(rec) > 0) {
  message("★ Recomendación (baseline): p=", rec$p,
          " | serial_p=", signif(rec$serial_p, 3),
          " | max_root_mod=", signif(rec$max_root_mod, 3))
} else {
  message("⚠ No encontré ningún p estable con serial_p definido en el rango.")
}

print(res)

