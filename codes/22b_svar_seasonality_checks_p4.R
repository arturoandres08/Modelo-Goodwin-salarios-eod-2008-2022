# codes/22b_svar_seasonality_checks_p4.R
# ==========================================================
# 22b) Evidencia de estacionalidad (SVAR core, p=4, 3 specs salariales)
# - Tests: y ~ dummies trimestrales (Q2-Q4) + test conjunto
# - Gráficos: boxplot por trimestre (ga, gw, v) facet por spec
# Outputs:
#   tables/svar_core/p4_fixed/seasonality/seasonality_dummies_tests.csv
#   figures/svar_core/p4_fixed/seasonality/boxplot_ga_by_qtr.pdf/.png
#   figures/svar_core/p4_fixed/seasonality/boxplot_gw_by_qtr.pdf/.png
#   figures/svar_core/p4_fixed/seasonality/boxplot_v_by_qtr.pdf/.png
# ==========================================================

source(here::here("codes","00_setup.R"))
message("\n== 22b) Checks estacionalidad (dummies trimestrales) ==\n")

# funciones core
source(here::here("codes","18_svar_core_functions.R"))

# dirs
dir_tbl <- as.character(fs::path(CFG$dirs$out_tables,  "svar_core", "p4_fixed", "seasonality"))
dir_fig <- as.character(fs::path(CFG$dirs$out_figures, "svar_core", "p4_fixed", "seasonality"))
fs::dir_create(dir_tbl)
fs::dir_create(dir_fig)

# inputs base
in_path <- fs::path(CFG$dirs$processed, "svar_core_inputs_2008Q1_2022Q4.rds")
stopifnot(file.exists(in_path))

df0 <- load_rds(in_path) %>%
  dplyr::mutate(q = zoo::as.yearqtr(paste0(year, " Q", trimestre), format = "%Y Q%q")) %>%
  dplyr::arrange(q)

specs <- tibble::tibble(
  spec = c("Baseline (mean winsor)", "Robustez (mean no-winsor)", "Robustez (median)"),
  wage_col = c("w_real_mean_winsor", "w_real_mean_nowinsor", "w_real_median")
)

get_qtr <- function(q) as.integer(format(q, "%q"))

seasonality_test_one <- function(y, qtr_factor) {
  # Test conjunto: modelo con dummies vs intercept-only
  m0 <- stats::lm(y ~ 1)
  m1 <- stats::lm(y ~ qtr_factor)
  a  <- stats::anova(m0, m1)
  p_joint <- a$`Pr(>F)`[2]
  
  # medias por trimestre
  means <- tapply(y, qtr_factor, mean, na.rm = TRUE)
  tibble::tibble(
    p_joint = as.numeric(p_joint),
    mean_Q1 = as.numeric(means["1"]),
    mean_Q2 = as.numeric(means["2"]),
    mean_Q3 = as.numeric(means["3"]),
    mean_Q4 = as.numeric(means["4"])
  )
}

rows <- list()
plot_data <- list()

for (i in seq_len(nrow(specs))) {
  sp  <- specs$spec[i]
  wcol <- specs$wage_col[i]
  
  X <- build_core(df0, wage_col = wcol, a_col = "a_prod") %>%
    dplyr::mutate(qtr = get_qtr(q), qtr_f = factor(qtr, levels = 1:4, labels = c("Q1","Q2","Q3","Q4")))
  
  # guardar data para boxplots
  plot_data[[sp]] <- X %>% dplyr::mutate(spec = sp)
  
  # tests por variable
  for (vn in c("ga","gw","v")) {
    tmp <- seasonality_test_one(X[[vn]], X$qtr_f)
    rows[[length(rows)+1]] <- tmp %>%
      dplyr::mutate(spec = sp, variable = vn, T = nrow(X)) %>%
      dplyr::select(spec, variable, T, dplyr::everything())
  }
}

out <- dplyr::bind_rows(rows) %>%
  dplyr::arrange(spec, factor(variable, levels = c("ga","gw","v")))

readr::write_csv(out, file.path(dir_tbl, "seasonality_dummies_tests.csv"))
message("✔ Guardado: ", file.path(dir_tbl, "seasonality_dummies_tests.csv"))

# ---- Boxplots (una figura por variable) ----
plot_df <- dplyr::bind_rows(plot_data)

plot_box <- function(df, var, title, ylab, fname) {
  p <- ggplot2::ggplot(df, ggplot2::aes(x = qtr_f, y = .data[[var]])) +
    ggplot2::geom_boxplot(outlier.size = 0.8) +
    ggplot2::facet_wrap(~spec, ncol = 1) +
    ggplot2::labs(title = title, x = "Trimestre", y = ylab) +
    ggplot2::theme_bw(base_size = 12)
  
  ggplot2::ggsave(file.path(dir_fig, paste0(fname, ".pdf")), p, width = 8, height = 8)
  ggplot2::ggsave(file.path(dir_fig, paste0(fname, ".png")), p, width = 8, height = 8, dpi = 300)
}

plot_box(plot_df, "ga", "Distribución de ga por trimestre (Q1–Q4)", "ga", "boxplot_ga_by_qtr")
plot_box(plot_df, "gw", "Distribución de gw por trimestre (Q1–Q4)", "gw", "boxplot_gw_by_qtr")
plot_box(plot_df, "v",  "Distribución de v por trimestre (Q1–Q4)",  "v",  "boxplot_v_by_qtr")

message("✔ 22b listo: ver tablas/figuras en p4_fixed/seasonality")