# codes/18_svar_core_functions.R
# ==========================================================
# 18) Funciones SVAR núcleo (ga, gw, v)
# Respeta teoría A0:
#   A0 = [[1,0,0],[0,1,0],[NA,NA,1]]
# Fixes operativos para bootstrap (vars::irf boot=TRUE):
#   - VAR construido con p literal (evita "p_bic no encontrado")
#   - SVAR construido con Amat/Bmat literales (evita "Amat no encontrado")
# ==========================================================

build_core <- function(data, wage_col, a_col = "a_prod") {
  stopifnot(wage_col %in% names(data))
  stopifnot(a_col %in% names(data))
  stopifnot(all(c("q","v") %in% names(data)))
  
  out <- data %>%
    dplyr::transmute(
      q = q,
      v = as.numeric(v),
      a = as.numeric(.data[[a_col]]),
      w = as.numeric(.data[[wage_col]])
    ) %>%
    dplyr::mutate(
      ga = c(NA_real_, diff(log(a))),
      gw = c(NA_real_, diff(log(w)))
    ) %>%
    dplyr::filter(!is.na(ga), !is.na(gw), !is.na(v)) %>%
    dplyr::select(q, ga, gw, v)
  
  if (nrow(out) < 30) stop("Quedaron muy pocas observaciones. Revisa datos/NA.")
  out
}

unit_root_table <- function(x, name) {
  adf <- tseries::adf.test(x, alternative = "stationary")
  kps <- tseries::kpss.test(x, null = "Level")
  tibble::tibble(
    variable  = name,
    adf_stat  = unname(adf$statistic),
    adf_p     = adf$p.value,
    kpss_stat = unname(kps$statistic),
    kpss_p    = kps$p.value
  )
}

# ---- VAR select + fit (p literal para que bootstrap no busque símbolos) ----
select_lags_and_fit <- function(X, lag_max = 8) {
  Xmat <- X %>% dplyr::select(ga, gw, v)
  
  sel <- vars::VARselect(Xmat, lag.max = lag_max, type = "const")
  p_bic <- max(1L, as.integer(sel$selection[["SC(n)"]]))
  p_aic <- max(1L, as.integer(sel$selection[["AIC(n)"]]))
  
  # ✅ clave: p se inyecta como número literal
  var_bic <- eval(bquote(vars::VAR(Xmat, p = .(p_bic), type = "const")))
  
  list(sel = sel, p_bic = p_bic, p_aic = p_aic, var_bic = var_bic)
}

diagnostics_var <- function(var_model) {
  roots <- vars::roots(var_model)
  stable <- all(Mod(roots) < 1)
  serial <- vars::serial.test(var_model, lags.pt = 12, type = "PT.asymptotic")
  arch   <- vars::arch.test(var_model, lags.multi = 12)
  
  tibble::tibble(
    stable = stable,
    max_root_mod = max(Mod(roots)),
    serial_p = serial$serial$p.value,
    arch_p   = arch$arch.mul$p.value
  )
}

# ---- SVAR A0 teórico (Amat/Bmat literales para bootstrap) ----
fit_svar_A0 <- function(var_model,
                        method_primary = "direct",
                        method_fallback = "scoring") {
  
  # A0 teórico: [[1,0,0],[0,1,0],[NA,NA,1]]
  Amat <- diag(3)
  Amat[1,2] <- 0; Amat[1,3] <- 0
  Amat[2,1] <- 0; Amat[2,3] <- 0
  Amat[3,1] <- NA
  Amat[3,2] <- NA
  
  # helper: inyecta x/A/B/method como literales
  svar_eval <- function(x, A, B, method) {
    eval(bquote(vars::SVAR(.(x), Amat = .(A), Bmat = .(B), method = .(method))))
  }
  
  # 1) B = I (normalización shocks var=1)
  B_id <- diag(3)
  
  out <- tryCatch(
    {
      tryCatch(
        svar_eval(var_model, Amat, B_id, method_primary),
        error = function(e) svar_eval(var_model, Amat, B_id, method_fallback)
      )
    },
    error = function(e) NULL
  )
  
  if (!is.null(out)) {
    attr(out, "svar_spec") <- list(B = "I", method = method_primary)
    return(out)
  }
  
  # 2) Fallback numérico (misma teoría): B diagonal libre (escala shocks)
  B_diag <- matrix(0, 3, 3)
  diag(B_diag) <- NA
  
  out2 <- tryCatch(
    {
      tryCatch(
        svar_eval(var_model, Amat, B_diag, method_primary),
        error = function(e) svar_eval(var_model, Amat, B_diag, method_fallback)
      )
    },
    error = function(e) stop("SVAR falló incluso con B diagonal libre: ", conditionMessage(e))
  )
  
  attr(out2, "svar_spec") <- list(B = "diag_free", method = method_primary)
  out2
}

tidy_irf <- function(irf_obj, impulse_name, response_name) {
  irf_mat   <- irf_obj$irf[[impulse_name]]
  lower_mat <- irf_obj$Lower[[impulse_name]]
  upper_mat <- irf_obj$Upper[[impulse_name]]
  
  if (is.null(colnames(irf_mat)) || !response_name %in% colnames(irf_mat)) {
    stop("No encontré response_name='", response_name, "' en irf_mat. Colnames: ",
         paste(colnames(irf_mat), collapse = ", "))
  }
  
  tibble::tibble(
    h = 0:(nrow(irf_mat)-1),
    irf   = irf_mat[, response_name],
    lower = lower_mat[, response_name],
    upper = upper_mat[, response_name]
  )
}

plot_irf <- function(df_irf, title, ylab) {
  ggplot2::ggplot(df_irf, ggplot2::aes(x = h, y = irf)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.2) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::labs(title = title, x = "Horizonte (trimestres)", y = ylab) +
    ggplot2::theme_bw(base_size = 12)
}

cum_multiplier <- function(df_irf, H_vec = c(4,8,12)) {
  purrr::map_dfr(H_vec, function(H) {
    tibble::tibble(
      H = H,
      M       = sum(df_irf$irf[df_irf$h <= H], na.rm = TRUE),
      M_lower = sum(df_irf$lower[df_irf$h <= H], na.rm = TRUE),
      M_upper = sum(df_irf$upper[df_irf$h <= H], na.rm = TRUE)
    )
  })
}