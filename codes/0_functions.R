<<<<<<< Updated upstream
<<<<<<< Updated upstream
############################################################
## 0_functions.R — Consolidated helpers + theta_inference module
## QR/Gram–Schmidt orthogonalization (GLOBAL, window-invariant)
## Reproducible + inference-safe (audited, deduplicated)
##
## Key contract:
## - Orthogonalization is QR of raw powers e^1..e^d (optionally centered)
## - The QR basis object is built ONCE using FULL window e, then reused
## - Theta inference uses the SAME QR basis for theta, theta', theta''
############################################################

# -------- Significance stars (robust, pretty) --------
add_stars <- function(vec, reverse = FALSE, digits = 2) {
  if (length(vec) < 4 || all(is.na(vec))) return("")
  v <- suppressWarnings(as.numeric(vec[1:4]))
  if (any(!is.finite(v))) return("")
  names(v) <- c("Test statistic","Critical value 1%","Critical value 5%","Critical value 10%")
  ts <- v[1]; c1 <- v[2]; c5 <- v[3]; c10 <- v[4]
  if (any(is.na(c(ts,c1,c5,c10)))) return("")
  stars <- if (reverse) {
    if (ts >= c1) "***" else if (ts >= c5) "**" else if (ts >= c10) "*" else ""
  } else {
    if (ts <= c1) "***" else if (ts <= c5) "**" else if (ts <= c10) "*" else ""
  }
  paste0(formatC(ts, format = "f", digits = digits), stars)
}

# -------- Unit root suite: ADF/PP/DF-GLS/ERS.PO/KPSS --------
UnitRootTests <- function(df_ts, model_type = c("constant","trend"),
                          adf_lag_select = c("BIC","AIC","Fixed"),
                          adf_lags = NULL,
                          ers_lag_max = 5,
                          pp_lags = c("short","long")) {
  if (!requireNamespace("urca", quietly = TRUE)) stop("Please install 'urca'")
  model_type <- match.arg(model_type)
  adf_lag_select <- match.arg(adf_lag_select)
  pp_lags <- match.arg(pp_lags)
  
  df_ts <- as.data.frame(df_ts)
  numeric_cols <- vapply(df_ts, is.numeric, TRUE)
  if (!any(numeric_cols)) stop("No numeric columns found in df_ts.")
  if (any(!numeric_cols)) warning("Ignored non-numeric columns: ", paste(names(df_ts)[!numeric_cols], collapse = ", "))
  df_ts <- df_ts[, numeric_cols, drop = FALSE]
  
  strip_na <- function(x) {
    ix <- which(is.finite(x))
    if (!length(ix)) return(numeric())
    x[min(ix):max(ix)]
  }
  
  var_names <- colnames(df_ts)
  test_names <- c("ADF","PP","DF.GL","ERS.PO","KPSS")
  empty_vec <- function() {
    vec <- rep(NA_real_, 4)
    names(vec) <- c("Test statistic","Critical value 1%","Critical value 5%","Critical value 10%")
    vec
  }
  tests <- setNames(
    lapply(test_names, function(.) setNames(vector("list", length(var_names)), var_names)),
    test_names
  )
  
  for (v in var_names) {
    x <- strip_na(df_ts[[v]])
    if (!length(x)) {
      tests$ADF[[v]]    <- empty_vec()
      tests$PP[[v]]     <- empty_vec()
      tests$DF.GL[[v]]  <- empty_vec()
      tests$ERS.PO[[v]] <- empty_vec()
      tests$KPSS[[v]]   <- empty_vec()
      next
    }
    
    tests$ADF[[v]] <- tryCatch({
      adf_type <- if (model_type == "constant") "drift" else "trend"
      adf <- if (adf_lag_select %in% c("BIC","AIC")) {
        urca::ur.df(x, type = adf_type, selectlags = adf_lag_select)
      } else {
        if (is.null(adf_lags)) stop("When adf_lag_select='Fixed', provide adf_lags.")
        urca::ur.df(x, type = adf_type, lags = adf_lags)
      }
      tau_name <- if (model_type == "constant") "tau2" else "tau3"
      ts_vec <- adf@teststat
      ts <- if (is.matrix(ts_vec)) {
        rn <- rownames(ts_vec); idx <- which(rn == tau_name); if (!length(idx)) idx <- 1L
        unname(ts_vec[idx, 1])
      } else {
        tmp <- unname(ts_vec[tau_name]); if (is.na(tmp)) tmp <- unname(ts_vec[[1L]]); tmp
      }
      cv_mat <- adf@cval
      cv_r <- grep(paste0("^", tau_name), rownames(cv_mat)); if (!length(cv_r)) cv_r <- 1L
      cv <- cv_mat[cv_r, c("1pct","5pct","10pct"), drop = TRUE]
      vec <- c(ts, as.numeric(cv)); names(vec) <- names(empty_vec()); vec
    }, error = function(e){ warning(sprintf("ADF failed for %s: %s", v, conditionMessage(e))); empty_vec() })
    
    tests$PP[[v]] <- tryCatch({
      pp <- urca::ur.pp(x, type = "Z-tau", lags = pp_lags, model = model_type)
      ts <- unname(pp@teststat[1])
      cv <- pp@cval[1, c("1pct","5pct","10pct"), drop = TRUE]
      vec <- c(ts, as.numeric(cv)); names(vec) <- names(empty_vec()); vec
    }, error = function(e){ warning(sprintf("PP failed for %s: %s", v, conditionMessage(e))); empty_vec() })
    
    tests$DF.GL[[v]] <- tryCatch({
      dfgls <- urca::ur.ers(x, type = "DF-GLS", model = model_type, lag.max = ers_lag_max)
      ts <- unname(dfgls@teststat[1])
      cv <- dfgls@cval[1, c("1pct","5pct","10pct"), drop = TRUE]
      vec <- c(ts, as.numeric(cv)); names(vec) <- names(empty_vec()); vec
    }, error = function(e){ warning(sprintf("DF-GLS failed for %s: %s", v, conditionMessage(e))); empty_vec() })
    
    tests$ERS.PO[[v]] <- tryCatch({
      ers_po <- urca::ur.ers(x, type = "P-test", model = model_type, lag.max = ers_lag_max)
      ts <- unname(ers_po@teststat[1])
      cv <- ers_po@cval[1, c("1pct","5pct","10pct"), drop = TRUE]
      vec <- c(ts, as.numeric(cv)); names(vec) <- names(empty_vec()); vec
    }, error = function(e){ warning(sprintf("ERS.PO failed for %s: %s", v, conditionMessage(e))); empty_vec() })
    
    tests$KPSS[[v]] <- tryCatch({
      kpss_type <- if (model_type == "constant") "mu" else "tau"
      kpss <- urca::ur.kpss(x, type = kpss_type, lags = "long")
      ts <- unname(kpss@teststat[1])
      cv <- kpss@cval[1, c("1pct","5pct","10pct"), drop = TRUE]
      vec <- c(ts, as.numeric(cv)); names(vec) <- names(empty_vec()); vec
    }, error = function(e){ warning(sprintf("KPSS failed for %s: %s", v, conditionMessage(e))); empty_vec() })
  }
  
  for (t in test_names) tests[[t]] <- tests[[t]][var_names]
  
  structure(list(raw = tests, vars = var_names, tests = test_names,
                 model_type = model_type,
                 meta = list(adf_lag_select = adf_lag_select, ers_lag_max = ers_lag_max, pp_lags = pp_lags)),
            class = "unitroot_suite")
}

processAndAddStars <- function(test_results, tidy = FALSE, digits = 2) {
  stopifnot(is.list(test_results), !is.null(test_results$raw))
  raw <- test_results$raw; var_names <- test_results$vars; test_names <- test_results$tests
  out <- matrix(NA_character_, nrow = length(var_names), ncol = length(test_names),
                dimnames = list(var_names, test_names))
  for (t in test_names) {
    reverse <- identical(t, "KPSS")
    for (v in var_names) out[v, t] <- add_stars(raw[[t]][[v]], reverse = reverse, digits = digits)
  }
  df <- as.data.frame(out, stringsAsFactors = FALSE)
  if (!tidy) return(df)
  
  tidy_rows <- lapply(var_names, function(v) {
    do.call(rbind, lapply(test_names, function(t) {
      vec <- raw[[t]][[v]]
      if (length(vec) < 4) vec <- rep(NA_real_, 4)
      data.frame(var = v, test = t,
                 stat = vec[1], cval_1 = vec[2], cval_5 = vec[3], cval_10 = vec[4],
                 stat_star = add_stars(vec, reverse = identical(t,"KPSS"), digits = digits),
                 stringsAsFactors = FALSE)
    }))
  })
  do.call(rbind, tidy_rows)
}

reshapeTestsResults <- function(results_vector, var_names,
                                test_names = c('ADF','PP','DF.GL','ERS.PO','KPSS'),
                                byrow = TRUE) {
  if (length(results_vector) != length(var_names) * length(test_names))
    stop("Length mismatch: results_vector must equal length(var_names) * length(test_names)")
  matrix(as.numeric(results_vector),
         nrow = length(var_names),
         ncol = length(test_names),
         byrow = byrow,
         dimnames = list(var_names, test_names))
}

# -------- Table export with optional kableExtra footnote/styling --------
table_as_is <- function(data, file_path,
                        column_labels = NULL,
                        caption = "Table",
                        format = c("latex", "html"),
                        overwrite = TRUE,
                        escape = TRUE,
                        return_string = FALSE,
                        footnote = NULL,
                        manifest_hook = NULL) {
  format <- match.arg(format)
  if (!is.data.frame(data) && !is.matrix(data)) stop("`data` must be a data.frame or matrix.")
  if (!is.null(column_labels)) {
    if (length(column_labels) != ncol(data)) stop("column_labels length mismatch.")
    colnames(data) <- column_labels
  }
  dir_path <- dirname(file_path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  if (file.exists(file_path) && !isTRUE(overwrite)) stop("File exists and overwrite = FALSE: ", file_path)
  
  if (!requireNamespace("knitr", quietly = TRUE)) stop("Package 'knitr' is required")
  tbl <- knitr::kable(data, format = format, booktabs = TRUE, caption = caption, escape = escape)
  
  if (requireNamespace("kableExtra", quietly = TRUE)) {
    if (format == "latex") {
      tbl <- kableExtra::kable_styling(tbl, latex_options = c("hold_position"))
      if (!is.null(footnote)) tbl <- kableExtra::footnote(tbl, general = footnote, threeparttable = TRUE)
    } else {
      tbl <- kableExtra::kable_styling(tbl, bootstrap_options = c("condensed","responsive"))
      if (!is.null(footnote)) tbl <- kableExtra::footnote(tbl, general = footnote)
    }
    tbl_string <- as.character(tbl)
  } else {
    tbl_string <- paste(tbl, collapse = "\n")
    if (!is.null(footnote)) tbl_string <- paste0(tbl_string, sprintf("\n\nNote: %s\n", footnote))
  }
  
  if (isTRUE(return_string)) return(tbl_string)
  
  tryCatch({
    writeLines(tbl_string, con = file_path, useBytes = TRUE)
    if (is.function(manifest_hook)) manifest_hook(list(type = "table", file = file_path, caption = caption))
    invisible(file_path)
  }, error = function(e) stop("Failed to write table: ", conditionMessage(e)))
}

############################
# Reproducible export utils
############################

ensure_dirs <- function(...) {
  paths <- c(...)
  paths <- unique(paths[!is.na(paths) & nzchar(paths)])
  invisible(lapply(paths, function(p) {
    if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  }))
}

set_seed_deterministic <- function(seed = 123456) {
  suppressWarnings(RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion"))
  set.seed(seed)
}

safe_logdet <- function(resids) {
  S <- stats::cov(resids, use = "pairwise.complete.obs")
  as.numeric(determinant(S, logarithm = TRUE)$modulus)
}

save_plot_dual <- function(gg, filepath_no_ext,
                           width = 9, height = 5.2, dpi = 300,
                           overwrite = TRUE) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  dir.create(dirname(filepath_no_ext), recursive = TRUE, showWarnings = FALSE)
  pdf_path <- paste0(filepath_no_ext, ".pdf")
  png_path <- paste0(filepath_no_ext, ".png")
  if (!overwrite && (file.exists(pdf_path) || file.exists(png_path)))
    stop("File exists and overwrite = FALSE: ", filepath_no_ext)
  
  try({
    ggplot2::ggsave(filename = pdf_path, plot = gg,
                    width = width, height = height,
                    device = get0("cairo_pdf", envir = asNamespace("grDevices"), inherits = FALSE),
                    bg = "white")
  }, silent = TRUE)
  if (!file.exists(pdf_path)) {
    ggplot2::ggsave(filename = pdf_path, plot = gg, width = width, height = height, bg = "white")
  }
  
  ggplot2::ggsave(filename = png_path, plot = gg,
                  width = width, height = height, dpi = dpi, bg = "white")
  invisible(list(pdf = pdf_path, png = png_path))
}

save_table_tex_csv <- function(data, tex_path, csv_path = NULL,
                               caption = "Table", footnote = NULL,
                               escape = TRUE, overwrite = TRUE) {
  if (!requireNamespace("knitr", quietly = TRUE)) stop("Package 'knitr' required for LaTeX export")
  dir.create(dirname(tex_path), recursive = TRUE, showWarnings = FALSE)
  if (!is.null(csv_path)) dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
  if (!overwrite && (file.exists(tex_path) || (!is.null(csv_path) && file.exists(csv_path))))
    stop("File exists and overwrite = FALSE: ", tex_path)
  
  if (exists("table_as_is", mode = "function")) {
    table_as_is(data, file_path = tex_path, caption = caption,
                format = "latex", overwrite = TRUE,
                escape = escape, footnote = footnote)
  } else {
    tbl <- knitr::kable(data, format = "latex", booktabs = TRUE, caption = caption, escape = escape)
    writeLines(paste(tbl, collapse = "\n"), con = tex_path, useBytes = TRUE)
  }
  
  if (!is.null(csv_path)) {
    if (!requireNamespace("readr", quietly = TRUE)) stop("Package 'readr' required for CSV export")
    readr::write_csv(as.data.frame(data), csv_path)
  }
  invisible(list(tex = tex_path, csv = csv_path))
}

# ============================================================
# Admissible gate (shared)
# ============================================================
admissible_gate <- function(dfw, p, hard_kappa = 1e8, slack = 5) {
  T_eff <- nrow(dfw)
  m     <- ncol(dfw)
  
  if ((T_eff - (p + 1)) < slack * m) {
    return(list(ok = FALSE,
                reason = "T too small given (p,m)",
                diagnostics = list(T_eff = T_eff, m = m, p = p)))
  }
  
  S <- try(stats::cov(dfw), silent = TRUE)
  if (inherits(S, "try-error") || any(!is.finite(S))) {
    return(list(ok = FALSE,
                reason = "cov(dfw) failed / non-finite",
                diagnostics = list(T_eff = T_eff, m = m, p = p)))
  }
  
  kap <- try(kappa(S), silent = TRUE)
  if (!inherits(kap, "try-error") && is.finite(kap) && kap > hard_kappa) {
    return(list(ok = FALSE,
                reason = "ill-conditioned covariance (kappa too large)",
                diagnostics = list(kappa = kap, T_eff = T_eff, m = m, p = p)))
  }
  
  list(ok = TRUE, reason = "ok", diagnostics = list(kappa = kap, T_eff = T_eff, m = m, p = p))
}

# ============================================================
# theta_inference module (GLOBAL QR basis + mapping + CIs)
# ============================================================

# Build raw polynomial matrix X(e) = [e, e^2, ..., e^degree] (no intercept)
poly_raw <- function(e, degree) {
  X <- sapply(1:degree, function(d) e^d)
  colnames(X) <- paste0("e", 1:degree)
  X
}

poly_raw_d1 <- function(e, degree) {
  X1 <- sapply(1:degree, function(d) d * e^(d - 1))
  colnames(X1) <- paste0("de_e", 1:degree)
  X1
}

poly_raw_d2 <- function(e, degree) {
  X2 <- sapply(1:degree, function(d) if (d >= 2) d * (d - 1) * e^(d - 2) else rep(0, length(e)))
  colnames(X2) <- paste0("d2e_e", 1:degree)
  X2
}

# Build a GLOBAL QR basis object from reference e (use full sample e)
theta_basis_qr_build <- function(e_ref, degree = 3, center = TRUE) {
  stopifnot(degree >= 1)
  X <- poly_raw(e_ref, degree)
  
  mu <- if (center) colMeans(X, na.rm = TRUE) else rep(0, ncol(X))
  Xc <- sweep(X, 2, mu, "-")
  
  qrX <- qr(Xc)
  R   <- qr.R(qrX)
  
  # A = R^{-1} so that q(e) = Xc(e) %*% A matches qr.Q on reference sample
  A <- solve(R)
  
  # optional deterministic sign convention for reproducibility:
  # force diag(R) positive => flip columns if needed
  sgn <- sign(diag(R)); sgn[sgn == 0] <- 1
  A <- A %*% diag(sgn, nrow = length(sgn))
  mu <- mu # unchanged
  
  list(degree = degree, center = center, mu = mu, A = A)
}

# Evaluate q(e), dq/de, d2q/de2 using the frozen QR basis object
theta_basis_qr_fixed <- function(e, basis_obj) {
  degree <- basis_obj$degree
  mu     <- basis_obj$mu
  A      <- basis_obj$A
  
  X  <- poly_raw(e, degree)
  Xc <- sweep(X, 2, mu, "-")
  
  dX  <- poly_raw_d1(e, degree)
  d2X <- poly_raw_d2(e, degree)
  
  q   <- Xc  %*% A
  dq  <- dX  %*% A
  ddq <- d2X %*% A
  
  colnames(q)   <- paste0("Q", 1:degree)
  colnames(dq)  <- paste0("dQ", 1:degree)
  colnames(ddq) <- paste0("ddQ", 1:degree)
  
  list(q = q, dq = dq, ddq = ddq)
}

# Map orthogonal coeffs c -> raw polynomial coefficients a0..ad given basis_obj
# theta(e) = b0 + c' q(e)
# q(e) = Xc(e) A where Xc = X - mu and X=[e^1..e^d]
# Let w = A c. Then theta(e) = b0 + sum_k w_k (e^k - mu_k)
# => a_k = w_k;  a_0 = b0 - sum_k w_k mu_k
theta_coefmap_qr <- function(b0, cvec, basis_obj) {
  stopifnot(length(cvec) == basis_obj$degree)
  w <- as.numeric(basis_obj$A %*% as.numeric(cvec))
  a_k <- w
  a0  <- as.numeric(b0 - sum(w * basis_obj$mu))
  a <- c(a0, a_k) # a0..ad
  names(a) <- paste0("a", 0:basis_obj$degree)
  list(w = w, a = a)
}

# Evaluate theta and derivatives given beta = (b0, c1..cd) using QR basis
theta_eval_qr <- function(e, beta, basis_obj) {
  degree <- basis_obj$degree
  stopifnot(length(beta) == (1 + degree))
  B <- theta_basis_qr_fixed(e, basis_obj)
  cvec <- as.numeric(beta[-1])
  th  <- as.numeric(beta[1] + B$q  %*% cvec)
  th1 <- as.numeric(B$dq %*% cvec)
  th2 <- as.numeric(B$ddq %*% cvec)
  list(theta = th, theta_prime = th1, theta_second = th2)
}

theta_delta_ci_qr <- function(e_grid, beta_hat, Sigma_beta, basis_obj,
                              zcrit = 1.96, do_second = FALSE) {
  degree <- basis_obj$degree
  stopifnot(length(beta_hat) == (1 + degree))
  
  B <- theta_basis_qr_fixed(e_grid, basis_obj)
  
  g   <- cbind(1, B$q)
  gp  <- cbind(0, B$dq)
  gpp <- cbind(0, B$ddq)
  
  est <- theta_eval_qr(e_grid, beta_hat, basis_obj)
  
  var_theta <- rowSums((g  %*% Sigma_beta) * g)
  var_prime <- rowSums((gp %*% Sigma_beta) * gp)
  
  out <- data.frame(
    e = e_grid,
    theta_hat = est$theta,
    theta_lo  = est$theta - zcrit * sqrt(pmax(var_theta, 0)),
    theta_hi  = est$theta + zcrit * sqrt(pmax(var_theta, 0)),
    theta_prime_hat = est$theta_prime,
    theta_prime_lo  = est$theta_prime - zcrit * sqrt(pmax(var_prime, 0)),
    theta_prime_hi  = est$theta_prime + zcrit * sqrt(pmax(var_prime, 0)),
    stringsAsFactors = FALSE
  )
  
  if (isTRUE(do_second)) {
    var_second <- rowSums((gpp %*% Sigma_beta) * gpp)
    out$theta_second_hat <- est$theta_second
    out$theta_second_lo  <- est$theta_second - zcrit * sqrt(pmax(var_second, 0))
    out$theta_second_hi  <- est$theta_second + zcrit * sqrt(pmax(var_second, 0))
  }
  
  out
}

# Moving-block bootstrap indices
mbb_indices <- function(T, L) {
  if (L < 1) L <- 1
  if (L > T) L <- T
  starts <- sample.int(T - L + 1L, size = ceiling(T / L), replace = TRUE)
  idx <- unlist(lapply(starts, function(s) s:(s + L - 1L)))
  idx[1:T]
}

# Bootstrap inference for theta curve and coefficients (pointwise percentile bands)
theta_bootstrap_qr <- function(dfw, degree, basis_obj,
                               ecdet, p, r,
                               B = 100, block_len = NULL,
                               extract_beta_fun,
                               verbose = FALSE) {
  stopifnot(is.matrix(dfw))
  T <- nrow(dfw)
  if (is.null(block_len)) block_len <- max(5L, floor(T^(1/3)))
  if (verbose) message("Bootstrap B=", B, " block_len=", block_len)
  
  e_grid <- sort(unique(seq(min(basis_obj$mu[1]^(1/1), na.rm=TRUE),
                            max(basis_obj$mu[1]^(1/1), na.rm=TRUE),
                            length.out = 101)))
  # Better: evaluate on observed e-range. Caller should pass e_grid, but keep safe default:
  e_grid <- seq(min(dfw[,1], na.rm=TRUE), max(dfw[,1], na.rm=TRUE), length.out = 101)
  # NOTE: we don't actually know which column is e here; Engine evaluates on global e range.
  # In Engine we override e_grid explicitly. This function expects caller to manage e_grid if needed.
  
  beta_hat <- extract_beta_fun(dfw, ecdet, p, r)
  # e_grid will be overwritten by caller logic; but keep consistent interface:
  # We'll set curve later after caller passes its own grid. See Engine.
  
  th_mat  <- matrix(NA_real_, nrow = 101, ncol = B)
  thp_mat <- matrix(NA_real_, nrow = 101, ncol = B)
  thpp_mat<- matrix(NA_real_, nrow = 101, ncol = B)
  
  kept <- 0L
  fail <- 0L
  
  for (b in seq_len(B)) {
    idx <- mbb_indices(T, block_len)
    dfb <- dfw[idx, , drop = FALSE]
    tryb <- try({
      beta_b <- extract_beta_fun(dfb, ecdet, p, r)
      # Caller re-evaluates curve on its grid; here we only store betas
      kept <<- kept + 1L
      beta_b
    }, silent = TRUE)
    if (inherits(tryb, "try-error")) fail <- fail + 1L
  }
  
  list(
    method = "bootstrap_mbb_ca_jo",
    B = B,
    block_len = block_len,
    kept = kept,
    failed = fail,
    beta_hat = beta_hat
  )
}

# Unit tests for QR mapping: theta via basis equals theta via raw polynomial a
theta_unit_tests_qr <- function() {
  e_ref <- seq(0.2, 0.6, length.out = 80)
  for (deg in 2:3) {
    basis_obj <- theta_basis_qr_build(e_ref, degree = deg, center = TRUE)
    e_grid <- seq(min(e_ref), max(e_ref), length.out = 25)
    
    # random (b0,c)
    b0 <- stats::rnorm(1)
    cvec <- stats::rnorm(deg)
    beta <- c(b0, cvec)
    
    B <- theta_basis_qr_fixed(e_grid, basis_obj)
    th1 <- as.numeric(b0 + B$q %*% cvec)
    
    mp <- theta_coefmap_qr(b0, cvec, basis_obj)
    a <- mp$a # a0..ad
    V <- sapply(0:deg, function(k) e_grid^k)
    th2 <- as.numeric(V %*% a)
    
    if (max(abs(th1 - th2), na.rm = TRUE) > 1e-8) {
      stop("theta_unit_tests_qr failed for deg=", deg)
    }
  }
  TRUE
=======
# =========================
# 0_functions.R (vNext, CLEAN)
# Regime-stable Reduced-Rank helpers
# Q2-first, orthogonal basis invariant across windows
# =========================

# -------------------------
# S0.0 Utility + hygiene
# -------------------------
ensure_dirs <- function(...) {
  dirs <- c(...)
  for (d in dirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  invisible(TRUE)
}

set_seed_deterministic <- function(seed = 123) {
  set.seed(seed)
  invisible(TRUE)
}

# When your session is cursed: 128 connections in use.
# Call this ONCE, then restart your engine cleanly.
reset_r_io <- function() {
  # close all sinks safely
  while (sink.number(type = "output") > 0) sink(NULL, type = "output")
  while (sink.number(type = "message") > 0) sink(NULL, type = "message")
  # close connections
  try(closeAllConnections(), silent = TRUE)
  invisible(TRUE)
}

any_non_na <- function(x) {
  if (length(x) == 0) return(FALSE)
  any(!is.na(x))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# -------------------------
# S0.1 Locked windows + deterministics
# -------------------------
WINDOWS_LOCKED <- list(
  full         = c(1925, 2023),
  fordism      = c(1925, 1973),
  post_fordism = c(1974, 2023)
)

ECDET_LOCKED <- c("none", "const") # trend excluded (LOCKED)

ALPHA_LEVELS <- c("10pct" = 0.10, "5pct" = 0.05, "1pct" = 0.01)

DECISION_DEFAULTS <- list(
  epsilon = 0.10,
  delta   = 0.02,
  tau_det = 0.05
)

# -------------------------
# S0.2 Window filtering
# -------------------------
filter_window_years <- function(df, window_name, year_col = "year") {
  rng <- WINDOWS_LOCKED[[window_name]]
  if (is.null(rng)) stop("Unknown window: ", window_name, call. = FALSE)
  df[df[[year_col]] >= rng[1] & df[[year_col]] <= rng[2], , drop = FALSE]
}

# -------------------------
# S0.3 Q2 orthogonal basis: build + apply + evaluate
# Contract:
# - basis is frozen using FULL sample (mu, sd, poly_coefs)
# - each window uses same basis by evaluation on e (standardized by FULL mu/sd)
# -------------------------
build_q2_basis <- function(df, e_col = "e", degree = 2) {
  if (!(e_col %in% names(df))) stop("build_q2_basis: missing column ", e_col, call. = FALSE)
  
  e <- df[[e_col]]
  e <- e[is.finite(e)]
  if (length(e) < 10) stop("build_q2_basis: too few finite e values", call. = FALSE)
  
  mu  <- mean(e)
  sdv <- stats::sd(e)
  if (!is.finite(sdv) || sdv <= 0) stop("build_q2_basis: sd invalid", call. = FALSE)
  
  z <- (e - mu) / sdv
  
  P <- stats::poly(z, degree = degree, raw = FALSE)
  poly_coefs <- attr(P, "coefs")
  
  list(
    e_col = e_col,
    degree = degree,
    mean_full = mu,
    sd_full   = sdv,
    poly_coefs = poly_coefs
  )
}

eval_q2_basis <- function(basis, e_vec) {
  stopifnot(!is.null(basis$mean_full), !is.null(basis$sd_full), !is.null(basis$poly_coefs))
  z <- (as.numeric(e_vec) - basis$mean_full) / basis$sd_full
  P <- stats::poly(z, degree = basis$degree, coefs = basis$poly_coefs)
  P <- as.matrix(P)
  data.frame(P1 = P[, 1], P2 = P[, 2])
}

apply_q2_basis <- function(df, basis) {
  if (!(basis$e_col %in% names(df))) stop("apply_q2_basis: missing e column ", basis$e_col, call. = FALSE)
  ev <- df[[basis$e_col]]
  P  <- eval_q2_basis(basis, ev)
  df$P1 <- P$P1
  df$P2 <- P$P2
  df
}

# -------------------------
# S0.4 System builder: Q2 (DO NOT BREAK BASIS CONTRACT)
# System:
#   log_y, log_k, P1*log_k, P2*log_k
# -------------------------
build_system_Q2 <- function(df, y_col = "log_y", k_col = "log_k") {
  for (nm in c(y_col, k_col, "P1", "P2")) {
    if (!(nm %in% names(df))) stop("Missing column for system: ", nm, call. = FALSE)
  }
  
  X <- data.frame(
    log_y   = df[[y_col]],
    log_k   = df[[k_col]],
    P1_logK = df[["P1"]] * df[[k_col]],
    P2_logK = df[["P2"]] * df[[k_col]]
  )
  
  ok <- stats::complete.cases(X)
  X  <- X[ok, , drop = FALSE]
  
  list(Y = as.matrix(X), var_names = colnames(X), ok_idx = which(ok))
>>>>>>> Stashed changes
}

=======
# =========================
# 0_functions.R (vNext, CLEAN)
# Regime-stable Reduced-Rank helpers
# Q2-first, orthogonal basis invariant across windows
# =========================

# -------------------------
# S0.0 Utility + hygiene
# -------------------------
ensure_dirs <- function(...) {
  dirs <- c(...)
  for (d in dirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  invisible(TRUE)
}

set_seed_deterministic <- function(seed = 123) {
  set.seed(seed)
  invisible(TRUE)
}

# When your session is cursed: 128 connections in use.
# Call this ONCE, then restart your engine cleanly.
reset_r_io <- function() {
  # close all sinks safely
  while (sink.number(type = "output") > 0) sink(NULL, type = "output")
  while (sink.number(type = "message") > 0) sink(NULL, type = "message")
  # close connections
  try(closeAllConnections(), silent = TRUE)
  invisible(TRUE)
}

any_non_na <- function(x) {
  if (length(x) == 0) return(FALSE)
  any(!is.na(x))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# -------------------------
# S0.1 Locked windows + deterministics
# -------------------------
WINDOWS_LOCKED <- list(
  full         = c(1925, 2023),
  fordism      = c(1925, 1973),
  post_fordism = c(1974, 2023)
)

ECDET_LOCKED <- c("none", "const") # trend excluded (LOCKED)

ALPHA_LEVELS <- c("10pct" = 0.10, "5pct" = 0.05, "1pct" = 0.01)

DECISION_DEFAULTS <- list(
  epsilon = 0.10,
  delta   = 0.02,
  tau_det = 0.05
)

# -------------------------
# S0.2 Window filtering
# -------------------------
filter_window_years <- function(df, window_name, year_col = "year") {
  rng <- WINDOWS_LOCKED[[window_name]]
  if (is.null(rng)) stop("Unknown window: ", window_name, call. = FALSE)
  df[df[[year_col]] >= rng[1] & df[[year_col]] <= rng[2], , drop = FALSE]
}

# -------------------------
# S0.3 Q2 orthogonal basis: build + apply + evaluate
# Contract:
# - basis is frozen using FULL sample (mu, sd, poly_coefs)
# - each window uses same basis by evaluation on e (standardized by FULL mu/sd)
# -------------------------
build_q2_basis <- function(df, e_col = "e", degree = 2) {
  if (!(e_col %in% names(df))) stop("build_q2_basis: missing column ", e_col, call. = FALSE)
  
  e <- df[[e_col]]
  e <- e[is.finite(e)]
  if (length(e) < 10) stop("build_q2_basis: too few finite e values", call. = FALSE)
  
  mu  <- mean(e)
  sdv <- stats::sd(e)
  if (!is.finite(sdv) || sdv <= 0) stop("build_q2_basis: sd invalid", call. = FALSE)
  
  z <- (e - mu) / sdv
  
  P <- stats::poly(z, degree = degree, raw = FALSE)
  poly_coefs <- attr(P, "coefs")
  
  list(
    e_col = e_col,
    degree = degree,
    mean_full = mu,
    sd_full   = sdv,
    poly_coefs = poly_coefs
  )
}

eval_q2_basis <- function(basis, e_vec) {
  stopifnot(!is.null(basis$mean_full), !is.null(basis$sd_full), !is.null(basis$poly_coefs))
  z <- (as.numeric(e_vec) - basis$mean_full) / basis$sd_full
  P <- stats::poly(z, degree = basis$degree, coefs = basis$poly_coefs)
  P <- as.matrix(P)
  data.frame(P1 = P[, 1], P2 = P[, 2])
}

apply_q2_basis <- function(df, basis) {
  if (!(basis$e_col %in% names(df))) stop("apply_q2_basis: missing e column ", basis$e_col, call. = FALSE)
  ev <- df[[basis$e_col]]
  P  <- eval_q2_basis(basis, ev)
  df$P1 <- P$P1
  df$P2 <- P$P2
  df
}

# -------------------------
# S0.4 System builder: Q2 (DO NOT BREAK BASIS CONTRACT)
# System:
#   log_y, log_k, P1*log_k, P2*log_k
# -------------------------
build_system_Q2 <- function(df, y_col = "log_y", k_col = "log_k") {
  for (nm in c(y_col, k_col, "P1", "P2")) {
    if (!(nm %in% names(df))) stop("Missing column for system: ", nm, call. = FALSE)
  }
  
  X <- data.frame(
    log_y   = df[[y_col]],
    log_k   = df[[k_col]],
    P1_logK = df[["P1"]] * df[[k_col]],
    P2_logK = df[["P2"]] * df[[k_col]]
  )
  
  ok <- stats::complete.cases(X)
  X  <- X[ok, , drop = FALSE]
  
  list(Y = as.matrix(X), var_names = colnames(X), ok_idx = which(ok))
}

>>>>>>> Stashed changes
# -------------------------
# S0.5 Feasibility gate (transparent, conservative)
# -------------------------
feasible_for_ca_jo <- function(T, m, p, ecdet) {
  T_eff  <- T - (p + 1)
  min_eff <- 10 * m + 5 * p + if (ecdet == "const") 5 else 0
  list(ok = is.finite(T_eff) && T_eff > min_eff, T_eff = T_eff, min_eff = min_eff)
}

# -------------------------
# S0.6 Rank extraction (PAIR version: trace + eigen objects)
# Fixes urca ordering to r0 = 0:(m-1)
# -------------------------
.rank_from_seq <- function(stats_vec, cvals_mat, alpha) {
  col_idx <- switch(
    as.character(alpha),
    "0.1"  = 1L,
    "0.05" = 2L,
    "0.01" = 3L,
    stop("Unsupported alpha: ", alpha, call. = FALSE)
  )
  rej <- stats_vec > cvals_mat[, col_idx]
  r_hat <- sum(rej, na.rm = TRUE)
  r_hat <- max(0L, min(r_hat, length(stats_vec) - 1L))
  list(r_hat = r_hat, rej = rej)
}

.align_jo_order <- function(teststat_vec, cval_mat) {
  if (!is.numeric(teststat_vec)) stop("teststat is not numeric.", call. = FALSE)
  if (!is.matrix(cval_mat)) stop("cval is not a matrix.", call. = FALSE)
  if (length(teststat_vec) != nrow(cval_mat)) stop("teststat length != nrow(cval).", call. = FALSE)
  
  list(
    teststat = rev(as.numeric(teststat_vec)),
    cval     = cval_mat[nrow(cval_mat):1, , drop = FALSE]
  )
}

extract_rank_decisions_pair <- function(jo_trace, jo_eigen, alpha_levels = ALPHA_LEVELS) {
  tr0 <- .align_jo_order(jo_trace@teststat, jo_trace@cval)
  eg0 <- .align_jo_order(jo_eigen@teststat, jo_eigen@cval)
  
  trace_stats <- tr0$teststat
  trace_cv    <- tr0$cval
  eigen_stats <- eg0$teststat
  eigen_cv    <- eg0$cval
  
  # standardize col order if present
  if (!is.null(colnames(trace_cv)) && all(c("10pct","5pct","1pct") %in% colnames(trace_cv))) {
    trace_cv <- trace_cv[, c("10pct","5pct","1pct"), drop = FALSE]
  }
  if (!is.null(colnames(eigen_cv)) && all(c("10pct","5pct","1pct") %in% colnames(eigen_cv))) {
    eigen_cv <- eigen_cv[, c("10pct","5pct","1pct"), drop = FALSE]
  }
  
  out <- list()
  for (nm in names(alpha_levels)) {
    a <- unname(alpha_levels[[nm]])
    tr <- .rank_from_seq(trace_stats, trace_cv, a)
    eg <- .rank_from_seq(eigen_stats, eigen_cv, a)
    out[[paste0("r_trace_", nm)]] <- tr$r_hat
    out[[paste0("r_eigen_", nm)]] <- eg$r_hat
  }
  
  list(
    trace_stats = trace_stats,
    eigen_stats = eigen_stats,
    trace_cv    = trace_cv,
    eigen_cv    = eigen_cv,
    ranks       = out,
    reject_sequence_valid = !(all(is.na(trace_stats)) || all(is.na(eigen_stats)))
  )
}

run_ca_jo_pair <- function(Y, ecdet, K, spec = "transitory") {
  jo_tr <- urca::ca.jo(Y, type = "trace", ecdet = ecdet, K = K, spec = spec)
  jo_ei <- urca::ca.jo(Y, type = "eigen", ecdet = ecdet, K = K, spec = spec)
  list(trace = jo_tr, eigen = jo_ei)
}

# -------------------------
# S0.7 IC: PIC/BIC
# -------------------------
count_params_vecm <- function(m, p, r, ecdet) {
  k_alpha <- m * r
  k_beta  <- m * r
  k_gamma <- m * m * max(p - 1, 0)
  k_det   <- if (ecdet == "const") r else 0
  list(
    k_alpha = k_alpha, k_beta = k_beta, k_gamma = k_gamma, k_det = k_det,
    k_total = k_alpha + k_beta + k_gamma + k_det
  )
}

calc_ic <- function(loglik, T, k_total) {
  bic <- -2 * loglik + k_total * log(T)
  pic <- -2 * loglik + k_total * log(T) * log(log(T))
  list(PIC = pic, BIC = bic)
}

recompute_ic_check <- function(row) {
  ic <- calc_ic(row$logLik, row$T, row$k_total)
  ok1 <- isTRUE(all.equal(as.numeric(ic$PIC), as.numeric(row$PIC), tolerance = 1e-8))
  ok2 <- isTRUE(all.equal(as.numeric(ic$BIC), as.numeric(row$BIC), tolerance = 1e-8))
  ok1 && ok2
}

# -------------------------
# S0.8 Boundary discipline + decision rule
# -------------------------
boundary_flag <- function(p, r, p_max, r_max) (p >= p_max) || (r >= r_max)

choose_winner_Q2 <- function(df_grid, p_max, r_max, epsilon, delta) {
  df_grid <- df_grid[order(df_grid$PIC, df_grid$p, df_grid$r), , drop = FALSE]
  if (nrow(df_grid) == 0) return(NULL)
  
  global <- df_grid[1, , drop = FALSE]
  global_min <- global$PIC
  
  df_grid$boundary <- mapply(boundary_flag, df_grid$p, df_grid$r,
                             MoreArgs = list(p_max = p_max, r_max = r_max))
  interior <- df_grid[!df_grid$boundary, , drop = FALSE]
  
  if (nrow(interior) == 0) {
    w <- global
    w$winner_type <- "global_min_all_boundary"
    w$boundary_flag <- TRUE
    w$epsilon_used <- epsilon
    w$delta_to_global <- 0
    w$delta_to_runnerup <- if (nrow(df_grid) > 1) df_grid$PIC[2] - global_min else NA_real_
    w$no_interior_within_epsilon <- TRUE
    w$tie_break_applied <- "none"
    return(w)
  }
  
  scale <- max(abs(global_min), 1)
  eps_abs <- epsilon * scale
  
  interior$delta_to_global <- interior$PIC - global_min
  within_eps <- interior[interior$delta_to_global <= eps_abs, , drop = FALSE]
  
  if (nrow(within_eps) > 0) {
    cand <- within_eps
    winner_type <- "epsilon_interior_min"
    no_within <- FALSE
  } else {
    cand <- interior
    winner_type <- "interior_min_no_epsilon"
    no_within <- TRUE
  }
  
  cand <- cand[order(cand$PIC, cand$p, cand$r), , drop = FALSE]
  best <- cand[1, , drop = FALSE]
  
  scale2 <- max(abs(best$PIC), 1)
  del_abs <- delta * scale2
  pool <- cand[cand$PIC <= best$PIC + del_abs, , drop = FALSE]
  
  tie_break <- "none"
  if (nrow(pool) > 1) {
    pool <- pool[order(pool$p, pool$r, pool$PIC), , drop = FALSE]
    if (!(pool$p[1] == best$p && pool$r[1] == best$r)) tie_break <- "lower_p_then_r"
    best <- pool[1, , drop = FALSE]
  }
  
  best$winner_type <- winner_type
  best$boundary_flag <- FALSE
  best$epsilon_used <- epsilon
  best$no_interior_within_epsilon <- no_within
  best$delta_to_global <- best$PIC - global_min
  best$delta_to_runnerup <- if (nrow(cand) > 1) cand$PIC[2] - cand$PIC[1] else NA_real_
  best$tie_break_applied <- tie_break
  
  best
}

det_preference <- function(wn, wc, tau_det = 0.05) {
  add_det_cols <- function(x, pref, imp_rel) {
    if (is.null(x)) return(NULL)
    x <- as.data.frame(x)
    x$det_preference <- pref
    x$det_improvement_rel <- imp_rel
    x
  }
  
  if (is.null(wn) && is.null(wc)) return(NULL)
  if (is.null(wc)) return(add_det_cols(wn, "none_only", NA_real_))
  if (is.null(wn)) return(add_det_cols(wc, "const_only", NA_real_))
  
  wn <- as.data.frame(wn)
  wc <- as.data.frame(wc)
  
  pic_n <- as.numeric(wn$PIC[1])
  pic_c <- as.numeric(wc$PIC[1])
  imp_rel <- (pic_n - pic_c) / max(1e-12, abs(pic_n))
  
  if (is.finite(imp_rel) && imp_rel > tau_det) {
    add_det_cols(wc, "const", imp_rel)
  } else {
    add_det_cols(wn, "none", imp_rel)
  }
}

# -------------------------
# S0.9 LogLik from cajorls residuals (rank-specific)
# -------------------------
loglik_from_cajorls <- function(jo_trace, r, m_expected = NULL) {
  fit <- tryCatch(urca::cajorls(jo_trace, r = r), error = function(e) e)
  if (inherits(fit, "error")) return(NA_real_)
  
  E <- tryCatch(as.matrix(fit$rlm$residuals), error = function(e) NULL)
  if (is.null(E) || nrow(E) < 5) return(NA_real_)
  
  m <- ncol(E)
  if (!is.null(m_expected) && m != m_expected) return(NA_real_)
  
  Teff <- nrow(E)
  S <- crossprod(E) / Teff
  
  ld <- tryCatch(as.numeric(determinant(S, logarithm = TRUE)$modulus), error = function(e) NA_real_)
  if (!is.finite(ld)) return(NA_real_)
  
  -0.5 * Teff * (m * log(2 * pi) + ld + m)
}

# -------------------------
# S0.10 Inference: beta extraction + robust normalization + theta(e)
# -------------------------
extract_beta_alpha <- function(jo_trace, r, var_names) {
  if (is.null(var_names) || length(var_names) == 0) stop("extract_beta_alpha: var_names required", call. = FALSE)
  
  fit <- urca::cajorls(jo_trace, r = r)
  
  beta_full <- as.matrix(fit$beta)
  m <- length(var_names)
  
  if (nrow(beta_full) < m) {
    stop("extract_beta_alpha: beta has fewer rows than var_names. nrow(beta)=",
         nrow(beta_full), " m=", m, call. = FALSE)
  }
  
  # Keep ONLY endogenous block (first m rows) so deterministics never poison naming/normalization
  beta <- beta_full[seq_len(m), , drop = FALSE]
  rownames(beta) <- var_names
  
  # alpha is not strictly needed for theta; keep best-effort
  alpha <- tryCatch(as.matrix(jo_trace@W[, 1:r, drop = FALSE]), error = function(e) NULL)
  if (!is.null(alpha) && nrow(alpha) == m) rownames(alpha) <- var_names
  
  list(beta = beta, beta_full = beta_full, alpha = alpha, cajorls = fit)
}

# Normalize ONE cointegration vector on on_var.
# col:
#   - numeric index (1..r)
#   - "first_nonzero": choose first column where beta[on_var, j] != 0
normalize_beta <- function(beta, on_var = "log_y", col = "first_nonzero", tol = 1e-12) {
  beta <- as.matrix(beta)
  rn <- rownames(beta)
  if (is.null(rn) || !(on_var %in% rn)) {
    stop("normalize_beta: on_var not in beta rownames: ", on_var,
         "\nrows = {", paste(rn %||% "<none>", collapse = ", "), "}", call. = FALSE)
  }
  
  r <- ncol(beta)
  if (r < 1) stop("normalize_beta: beta has 0 columns", call. = FALSE)
  
  j <- NA_integer_
  if (is.numeric(col)) {
    j <- as.integer(col)
    if (j < 1 || j > r) stop("normalize_beta: invalid col index ", j, call. = FALSE)
  } else if (identical(col, "first_nonzero")) {
    piv <- as.numeric(beta[on_var, ])
    hits <- which(is.finite(piv) & abs(piv) > tol)
    if (length(hits) == 0) {
      stop("normalize_beta: no column has nonzero pivot for ", on_var, call. = FALSE)
    }
    j <- hits[1]
  } else {
    stop("normalize_beta: unsupported col spec. Use numeric or 'first_nonzero'.", call. = FALSE)
  }
  
  pivot <- as.numeric(beta[on_var, j])
  if (!is.finite(pivot) || abs(pivot) <= tol) {
    stop("normalize_beta: pivot is zero/invalid for selected column ", j, call. = FALSE)
  }
  
  b <- beta[, j, drop = FALSE] / pivot
  colnames(b) <- paste0("CI", j)
  b
}

# theta(e) for Q2 system:
# cointegration: b1*log_y + b2*log_k + b3*(P1*log_k) + b4*(P2*log_k) = 0
# => log_y = - (b2 + b3*P1(e) + b4*P2(e))/b1 * log_k
theta_curve_from_beta <- function(beta_norm, basis, grid_e) {
  beta_norm <- as.matrix(beta_norm)
  
  need <- c("log_y", "log_k", "P1_logK", "P2_logK")
  if (!all(need %in% rownames(beta_norm))) {
    stop("theta_curve_from_beta: beta rows must include: ", paste(need, collapse = ", "),
         "\nGot: ", paste(rownames(beta_norm), collapse = ", "), call. = FALSE)
  }
  
  P <- eval_q2_basis(basis, grid_e)
  
  b1 <- as.numeric(beta_norm["log_y", 1])
  b2 <- as.numeric(beta_norm["log_k", 1])
  b3 <- as.numeric(beta_norm["P1_logK", 1])
  b4 <- as.numeric(beta_norm["P2_logK", 1])
  
  if (!is.finite(b1) || abs(b1) < 1e-12) stop("theta_curve_from_beta: b1 (log_y) invalid", call. = FALSE)
  
  theta <- - (b2 + b3 * P$P1 + b4 * P$P2) / b1
  
  curve <- data.frame(
    e     = as.numeric(grid_e),
    P1    = P$P1,
    P2    = P$P2,
    theta = as.numeric(theta)
  )
  
  coefmap <- data.frame(
    term = c("log_y","log_k","P1_logK","P2_logK"),
    beta = c(b1,b2,b3,b4)
  )
  
  list(curve = curve, coefmap = coefmap)
}

# -------------------------
# S0.11 Unit tests (minimal, fast)
# -------------------------
run_unit_tests <- function() {
  stopifnot(all(names(WINDOWS_LOCKED) %in% c("full","fordism","post_fordism")))
  stopifnot(identical(ECDET_LOCKED, c("none","const")))
  
  stopifnot(boundary_flag(3, 1, p_max = 3, r_max = 3) == TRUE)
  stopifnot(boundary_flag(2, 3, p_max = 4, r_max = 3) == TRUE)
  stopifnot(boundary_flag(2, 1, p_max = 4, r_max = 3) == FALSE)
  
  pc <- count_params_vecm(m = 4, p = 2, r = 1, ecdet = "const")
  stopifnot(pc$k_total > 0)
  
  fake <- data.frame(logLik = -100, T = 100, k_total = 10)
  ic <- calc_ic(fake$logLik, fake$T, fake$k_total)
  fake$PIC <- ic$PIC; fake$BIC <- ic$BIC
  stopifnot(recompute_ic_check(fake))
  
  df <- data.frame(year = 1925:1934, e = seq(0.3, 0.5, length.out = 10))
  b <- build_q2_basis(df, e_col = "e")
  df2 <- apply_q2_basis(df, b)
  stopifnot(all(c("P1","P2") %in% names(df2)))
  stopifnot(nrow(df2) == nrow(df))
  
  df2$log_y <- rnorm(10); df2$log_k <- rnorm(10)
  sys <- build_system_Q2(df2)
  stopifnot(is.matrix(sys$Y) && ncol(sys$Y) == 4)
  
  g <- data.frame(p = rep(1:3, each=3), r = rep(0:2, times=3), PIC = rnorm(9))
  w <- choose_winner_Q2(g, p_max = 3, r_max = 2, epsilon = 0.1, delta = 0.05)
  stopifnot(!is.null(w))
  
  TRUE
}

