####=========================================================####
####   Identification Strategy Figures (F1–F7)
####   Output: here("output", "id_strategy")
####=========================================================####

#### 0) Packages ####
pkgs <- c(
  "here","readxl","dplyr","tidyr","ggplot2","zoo",
  "patchwork","stats","scales","strucchange"
)
invisible(lapply(pkgs, require, character.only = TRUE))

#### 1) Paths, helpers, data ####
source(paste0(here("codes"), "/0_functions.R"))  # you already have this

data_path   <- here("data/ddbb_cu_US_kgr.xlsx")
output_path <- here("output", "id_strategy")
ensure_dirs(output_path)
set_seed_deterministic()

ddbb_us <- readxl::read_excel(data_path, sheet = "us_data")

#### 2) Build variables ####
dfv <- ddbb_us |>
  transmute(
    year      = year,
    K         = KGCRcorp,
    Y         = Yrgdp,
    e         = e,
    e2        = e^2,
    yk        = Yrgdp / KGCRcorp,
    log_y     = log(Yrgdp),
    log_k     = log(KGCRcorp),
    log_yk    = log(yk),
    e_logk    = e * log_k,
    e2_logk    = e*e* log_k
  )

#### 3) Regime splits ####
fordist     <- dfv |> filter(year <= 1973)
postfordist <- dfv |> filter(year >= 1974)

####=========================================================####
#### F1 — Core Time-Series Diagnostics (Panels A & B)
#### Output: here("output", "id_strategy")
####=========================================================####

## ------------------------------------------------------------
## PANEL A — log(Y/K), e, e²
## ------------------------------------------------------------

# A1 — log(Y/K)
pA_1 <- ggplot(dfv, aes(year, log_yk)) +
  geom_line(color = "black") +
  geom_vline(xintercept = 1973, color = "red", linetype = "dashed") +
  labs(title = "log(Y/K)", x = NULL, y = "log(Y/K)") +
  theme_minimal(base_size = 12)

save_plot_dual(pA_1, file.path(output_path, "F1A1_logYK_ts"))

# A2 — e
pA_2 <- ggplot(dfv, aes(year, e)) +
  geom_line(color = "black") +
  geom_vline(xintercept = 1973, color = "red", linetype = "dashed") +
  labs(title = "Exploitation Rate (e)", x = NULL, y = "e") +
  theme_minimal(base_size = 12)

save_plot_dual(pA_2, file.path(output_path, "F1A2_e_ts"))


# Combined Panel A
panel_A <- pA_1 / pA_2

save_plot_dual(
  panel_A,
  file.path(output_path, "F1A_panel_combined")
)


## ------------------------------------------------------------
## PANEL B — e·logK, e²·logK
## ------------------------------------------------------------


# B2 — e·logK
pB_2 <- ggplot(dfv, aes(year, e_logk)) +
  geom_line(color = "black") +
  geom_vline(xintercept = 1973, color = "red", linetype = "dashed") +
  labs(title = expression(e*log(K)), x = NULL, y = expression(e*log(K))) +
  theme_minimal(base_size = 12)

# B3 — e²·logK
pB_3 <- ggplot(dfv, aes(year, e2_logk)) +
  geom_line(color = "black") +
  geom_vline(xintercept = 1973, color = "red", linetype = "dashed") +
  labs(title = expression(e^2*log(K)), x = "Year", y = expression(e^2*log(K))) +
  theme_minimal(base_size = 12)


# Combined Panel B
panel_B <-  pB_2 / pB_3

save_plot_dual(
  panel_B,
  file.path(output_path, "F1B_panel_combined")
)


####=========================================================####
#### F2 — log(Y/K) Densities by Regime (Clean, Consistent Colors)
####=========================================================####

dfv$regime <- ifelse(dfv$year <= 1973, 
                     "Fordist (1925–1973)", 
                     "Post-Fordist (1974–2023)")

## ---------------------- F2a ---------------------- ##
## Overlaid kernel densities (clean lines, no fill) ##
p_F2a <- ggplot(dfv, aes(x = log_yk, color = regime)) +
  geom_density(size = 1.2) +
  scale_color_manual(values = c(
    "Fordist (1925–1973)" = "steelblue",
    "Post-Fordist (1974–2023)" = "darkgreen"
  )) +
  labs(
    title = "log(Y/K): Kernel Densities by Regime",
    x = "log(Y/K)", y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

save_plot_dual(
  p_F2a,
  file.path(output_path, "F2a_logYK_density_overlay")
)


## ---------------------- F2b ---------------------- ##
## Faceted kernel densities (clean lines)          ##
p_F2b <- ggplot(dfv, aes(x = log_yk, color = regime)) +
  geom_density(size = 1.2) +
  facet_wrap(~ regime, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c(
    "Fordist (1925–1973)" = "steelblue",
    "Post-Fordist (1974–2023)" = "darkgreen"
  )) +
  labs(
    title = "log(Y/K): Faceted Kernel Densities",
    x = "log(Y/K)", y = "Density"
  ) +
  theme_minimal(base_size = 13)

save_plot_dual(
  p_F2b,
  file.path(output_path, "F2b_logYK_density_facets")
)


## ---------------------- F2c ---------------------- ##
## Ridgeline (joyplot) — requires fill aesthetic   ##
library(ggridges)

p_F2c <- ggplot(dfv, aes(x = log_yk, y = regime, fill = regime)) +
  geom_density_ridges(scale = 1.15, alpha = 0.75, color = "black") +
  scale_fill_manual(values = c(
    "Fordist (1925–1973)" = "steelblue",
    "Post-Fordist (1974–2023)" = "darkgreen"
  )) +
  labs(
    title = "log(Y/K): Ridgeline Density (Joyplot)",
    x = "log(Y/K)", y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

save_plot_dual(
  p_F2c,
  file.path(output_path, "F2c_logYK_joyplot")
)


####=========================================================####
#### F3 — Exploitation Rate e — Densities by Regime
####=========================================================####

## ---------------------- F3a ---------------------- ##
## Overlaid kernel densities (clean lines, no fill) ##
p_F3a <- ggplot(dfv, aes(x = e, color = regime)) +
  geom_density(size = 1.2) +
  scale_color_manual(values = c(
    "Fordist (1925–1973)" = "steelblue",
    "Post-Fordist (1974–2023)" = "darkgreen"
  )) +
  labs(
    title = "e: Kernel Densities by Regime",
    x = "e (exploitation rate)", y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

save_plot_dual(
  p_F3a,
  file.path(output_path, "F3a_e_density_overlay")
)


## ---------------------- F3b ---------------------- ##
## Faceted kernel densities (clean lines)          ##
p_F3b <- ggplot(dfv, aes(x = e, color = regime)) +
  geom_density(size = 1.2) +
  facet_wrap(~ regime, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c(
    "Fordist (1925–1973)" = "steelblue",
    "Post-Fordist (1974–2023)" = "darkgreen"
  )) +
  labs(
    title = "e: Faceted Kernel Densities",
    x = "e (exploitation rate)", y = "Density"
  ) +
  theme_minimal(base_size = 13)

save_plot_dual(
  p_F3b,
  file.path(output_path, "F3b_e_density_facets")
)


## ---------------------- F3c ---------------------- ##
## Ridgeline (joyplot)                              ##
p_F3c <- ggplot(dfv, aes(x = e, y = regime, fill = regime)) +
  geom_density_ridges(scale = 1.15, alpha = 0.75, color = "black") +
  scale_fill_manual(values = c(
    "Fordist (1925–1973)" = "steelblue",
    "Post-Fordist (1974–2023)" = "darkgreen"
  )) +
  labs(
    title = "e: Ridgeline Density (Joyplot)",
    x = "e (exploitation rate)", y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

save_plot_dual(
  p_F3c,
  file.path(output_path, "F3c_e_joyplot")
)


####=========================================================####
#### F4 — Scatter: log(Y/K) vs e (LOESS) by regime
####=========================================================####

p4a <- ggplot(fordist, aes(e, log_yk)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "loess", se = TRUE, color = "steelblue") +
  labs(title = "Fordist (1925–1973)", x = "e", y = "log(Y/K)") +
  theme_minimal(base_size = 12)

p4b <- ggplot(postfordist, aes(e, log_yk)) +
  geom_point(alpha = 0.4, color = "darkgreen") +
  geom_smooth(method = "loess", se = TRUE, color = "darkgreen") +
  labs(title = "Post-Fordist (1974–2023)", x = "e", y = "log(Y/K)") +
  theme_minimal(base_size = 12)

p_F4 <- p4a + p4b + plot_layout(ncol = 2)
save_plot_dual(p_F4, file.path(output_path, "F4_logYK_vs_e_scatter_regimes"))

####=========================================================####
#### F5 — Scatter: logY vs e*logK (LOESS + linear) by regime
####=========================================================####

p5a <- ggplot(fordist, aes(e_logk, log_y)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  geom_smooth(method = "loess", se = TRUE, color = "steelblue") +
  labs(title = "Fordist (1925–1973)", x = "e * log(K)", y = "log(Y)") +
  theme_minimal(base_size = 12)

p5b <- ggplot(postfordist, aes(e_logk, log_y)) +
  geom_point(alpha = 0.4, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  geom_smooth(method = "loess", se = TRUE, color = "darkgreen") +
  labs(title = "Post-Fordist (1974–2023)", x = "e * log(K)", y = "log(Y)") +
  theme_minimal(base_size = 12)

p_F5 <- p5a + p5b + plot_layout(ncol = 2)
save_plot_dual(p_F5, file.path(output_path, "F5_logY_vs_eLogK_scatter_regimes"))

####=========================================================####
#### F6 — Recursive Estimation: Forward, Centered, Backward #####
####=========================================================####

win <- 20   # window length
N <- nrow(dfv)

##############################################################
### 1. FORWARD RECURSION  (existing pipeline, cleaned)
##############################################################

theta0_fwd <- theta1_fwd <- theta2_fwd <- rep(NA, N)

for (t in seq_len(N)) {
  start_idx <- max(1, t - win + 1)
  end_idx   <- t
  
  df_win <- dfv[start_idx:end_idx,
                c("log_y", "log_k", "e_logk", "e2_logk")]
  if (nrow(df_win) < win) next
  
  fit <- lm(log_y ~ log_k + e_logk + e2_logk, data=df_win)
  theta0_fwd[t] <- coef(fit)[["log_k"]]
  theta1_fwd[t] <- coef(fit)[["e_logk"]]
  theta2_fwd[t] <- coef(fit)[["e2_logk"]]
}

df_fwd <- data.frame(
  year   = dfv$year,
  theta0 = theta0_fwd,
  theta1 = theta1_fwd,
  theta2 = theta2_fwd
)
df_fwd$theta_e <- df_fwd$theta0 +
  df_fwd$theta1 * dfv$e +
  df_fwd$theta2 * (dfv$e^2)



##############################################################
### 2. CENTERED RECURSION  (new)
##############################################################

theta0_mid <- theta1_mid <- theta2_mid <- rep(NA, N)

halfwin <- floor(win/2)

for (t in seq_len(N)) {
  
  # ideal symmetric window
  start_idx <- t - halfwin
  end_idx   <- t + halfwin
  
  # boundary correction: shift entire window, do NOT collapse it
  if (start_idx < 1) {
    end_idx   <- end_idx + (1 - start_idx)
    start_idx <- 1
  }
  if (end_idx > N) {
    start_idx <- start_idx - (end_idx - N)
    end_idx   <- N
  }
  
  # if window still not full length, skip
  if ((end_idx - start_idx + 1) < win) next
  
  df_win <- dfv[start_idx:end_idx,
                c("log_y", "log_k", "e_logk", "e2_logk")]
  
  fit <- lm(log_y ~ log_k + e_logk + e2_logk, data=df_win)
  
  theta0_mid[t] <- coef(fit)[["log_k"]]
  theta1_mid[t] <- coef(fit)[["e_logk"]]
  theta2_mid[t] <- coef(fit)[["e2_logk"]]
}

df_mid <- data.frame(
  year   = dfv$year,
  theta0 = theta0_mid,
  theta1 = theta1_mid,
  theta2 = theta2_mid
)

df_mid$theta_e <- df_mid$theta0 +
  df_mid$theta1 * dfv$e +
  df_mid$theta2 * (dfv$e^2)



##############################################################
### 3. BACKWARD RECURSION  (new, reverse window)
##############################################################

theta0_back <- theta1_back <- theta2_back <- rep(NA, N)

for (t in seq_len(N)) {
  
  # ending year goes backward in time
  end_idx   <- N - t + 1
  start_idx <- end_idx - win + 1
  
  # skip if window would go below 1
  if (start_idx < 1) next
  
  df_win <- dfv[start_idx:end_idx,
                c("log_y", "log_k", "e_logk", "e2_logk")]
  
  fit <- lm(log_y ~ log_k + e_logk + e2_logk, data = df_win)
  
  # store into SAME year-index so plots line up properly
  theta0_back[start_idx] <- coef(fit)[["log_k"]]
  theta1_back[start_idx] <- coef(fit)[["e_logk"]]
  theta2_back[start_idx] <- coef(fit)[["e2_logk"]]
}

df_back <- data.frame(
  year   = dfv$year,
  theta0 = theta0_back,
  theta1 = theta1_back,
  theta2 = theta2_back
)
df_back$theta_e <- df_back$theta0 +
  df_back$theta1 * dfv$e +
  df_back$theta2 * (dfv$e^2)



##############################################################
### 4. EXPORT PLOTS FOR EACH RECURSION MODE
##############################################################

plot_rec <- function(df, var, label, fname_suffix) {
  p <- ggplot(df, aes(year, .data[[var]])) +
    geom_line(color="black") +
    geom_vline(xintercept = 1973, linetype="dashed", color="red") +
    labs(
      title = paste0(label, " — ", fname_suffix),
      x = NULL, y = label
    ) +
    theme_minimal(base_size = 12)
  
  save_plot_dual(p, file.path(output_path, paste0("F6_", var, "_", fname_suffix)))
}


### Forward:
plot_rec(df_fwd,  "theta0", expression(theta[0]), "fwdRec")
plot_rec(df_fwd,  "theta1", expression(theta[1]), "fwdRec")
plot_rec(df_fwd,  "theta2", expression(theta[2]), "fwdRec")
plot_rec(df_fwd,  "theta_e", expression(theta(e)), "fwdRec")

### Centered (midRec):
plot_rec(df_mid,  "theta0", expression(theta[0]), "midRec")
plot_rec(df_mid,  "theta1", expression(theta[1]), "midRec")
plot_rec(df_mid,  "theta2", expression(theta[2]), "midRec")
plot_rec(df_mid,  "theta_e", expression(theta(e)), "midRec")

### Backward:
plot_rec(df_back, "theta0", expression(theta[0]), "backRec")
plot_rec(df_back, "theta1", expression(theta[1]), "backRec")
plot_rec(df_back, "theta2", expression(theta[2]), "backRec")
plot_rec(df_back, "theta_e", expression(theta(e)), "backRec")

cat("Completed forward, centered, and backward recursive estimations.\n")

####=========================================================####
#### F7 — Quadratic Fit: log(Y/K) ~ e + e^2 (overlay regimes)
####=========================================================####

# Build regime label
dfv$regime <- ifelse(dfv$year <= 1973, "Fordist (1925–1973)", 
                     "Post-Fordist (1974–2023)")

# Fit models
fit_ford <- lm(log_yk ~ e + I(e^2), data = subset(dfv, year <= 1973))
fit_post <- lm(log_yk ~ e + I(e^2), data = subset(dfv, year >= 1974))

# Grid of e values for smooth curves
e_grid <- seq(min(dfv$e, na.rm=TRUE),
              max(dfv$e, na.rm=TRUE),
              length.out = 400)

df_pred <- data.frame(
  e = e_grid,
  ford = predict(fit_ford, newdata = data.frame(e = e_grid)),
  post = predict(fit_post, newdata = data.frame(e = e_grid))
)

p_F7 <- ggplot() +
  
  # 1. Scatter: regime identifiable
  geom_point(data = dfv, 
             aes(x = e, y = log_yk, color = regime),
             alpha = 0.55, size = 2) +
  
  # 2. Quadratic fits
  geom_line(data = df_pred, aes(x = e, y = ford, 
                                color = "Fordist (fit)"), size = 1.4) +
  geom_line(data = df_pred, aes(x = e, y = post, 
                                color = "Post-Fordist (fit)"), size = 1.4) +
  
  # Colors: points and lines must align
  scale_color_manual(
    values = c(
      "Fordist (1925–1973)" = "steelblue",
      "Post-Fordist (1974–2023)" = "darkgreen",
      "Fordist (fit)" = "steelblue",
      "Post-Fordist (fit)" = "darkgreen"
    ),
    breaks = c("Fordist (1925–1973)", "Post-Fordist (1974–2023)",
               "Fordist (fit)", "Post-Fordist (fit)"),
    name = NULL
  ) +
  
  labs(
    title = "Quadratic Fit: log(Y/K) ~ e + e² (Regime-Identifiable Scatter)",
    x = "e (exploitation rate)",
    y = "log(Y/K)"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

save_plot_dual(
  p_F7,
  file.path(output_path, "F7_logYK_vs_e_quadratic_regimes_points")
)

####=========================================================####
#### Save session info
####=========================================================####

write_session_info(file.path(output_path, "session_info.txt"))
cat("F1–F7 completed. Output in: ", output_path, "\n")
