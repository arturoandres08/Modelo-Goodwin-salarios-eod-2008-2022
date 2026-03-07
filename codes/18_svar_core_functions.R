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

unit_root_table <- function(x, name,
                            adf_type = "drift",        # "none", "drift", "trend"
                            adf_lag_select = "AIC",    # "AIC" o "BIC"
                            kpss_type = "mu",          # "mu" (nivel) o "tau" (tendencia)
                            kpss_lags = "short") {     # "short","long" o número
  
  if (!requireNamespace("urca", quietly = TRUE)) install.packages("urca")
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  Tn <- length(x)
  
  # -----------------------
  # ADF (H0: raíz unitaria)
  # -----------------------
  adf <- urca::ur.df(x, type = adf_type, selectlags = adf_lag_select)
  
  # en ur.df: teststat es matriz; la fila 1 corresponde al tau-test relevante
  adf_stat <- as.numeric(adf@teststat[1])
  adf_cv10 <- as.numeric(adf@cval[1, "10pct"])
  adf_cv5  <- as.numeric(adf@cval[1, "5pct"])
  adf_cv1  <- as.numeric(adf@cval[1, "1pct"])
  
  # Rechazo ADF si stat es más negativo que el crítico (stat < cv)
  adf_reject_5 <- adf_stat < adf_cv5
  
  # --- KPSS: H0 = estacionaria (nula invertida) ---
  kpss <- urca::ur.kpss(x, type = kpss_type, lags = kpss_lags)
  
  kpss_stat <- as.numeric(kpss@teststat)
  
  cval <- kpss@cval
  
  get_cv <- function(level) {
    # level: "10", "5", "1"
    keys <- c(paste0(level, "pct"), paste0(level, "%"))
    
    if (is.null(cval)) return(NA_real_)
    
    # Caso matriz/data.frame: buscar en colnames
    if (is.matrix(cval) || is.data.frame(cval)) {
      cn <- colnames(cval)
      for (k in keys) {
        if (!is.null(cn) && k %in% cn) return(as.numeric(cval[1, k]))
      }
      return(NA_real_)
    }
    
    # Caso vector nombrado
    nm <- names(cval)
    for (k in keys) {
      if (!is.null(nm) && k %in% nm) return(as.numeric(cval[[k]]))
    }
    
    NA_real_
  }
  
  kpss_cv10 <- get_cv("10")
  kpss_cv5  <- get_cv("5")
  kpss_cv1  <- get_cv("1")
  
  kpss_reject_5 <- kpss_stat > kpss_cv5  # rechazo si el estadístico supera el crítico
  
  tibble::tibble(
    variable = name,
    T = Tn,
    
    ADF_type = adf_type,
    ADF_stat = adf_stat,
    ADF_cv_10 = adf_cv10,
    ADF_cv_5  = adf_cv5,
    ADF_cv_1  = adf_cv1,
    ADF_reject_5pct = adf_reject_5,
    
    KPSS_type = kpss_type,
    KPSS_stat = kpss_stat,
    KPSS_cv_10 = kpss_cv10,
    KPSS_cv_5  = kpss_cv5,
    KPSS_cv_1  = kpss_cv1,
    KPSS_reject_5pct = kpss_reject_5
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