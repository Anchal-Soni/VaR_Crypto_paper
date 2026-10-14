# =============================================================================
# transfer_entropy.R
#
# Shannon (group) transfer entropy for discretised time series, following
#   Dimpfl, T. and Peter, F. J. (2018). Analyzing volatility transmission
#   using group transfer entropy. Energy Economics 75, 368-376.
#   Dimpfl, T. and Peter, F. J. (2019). Group transfer entropy with an
#   application to cryptocurrencies. Physica A 516, 543-551.
#
# Re-implementation in plain base R (no parallel back-end, so it also runs on
# Windows). Discretisation, shuffle correction and block bootstrap follow the
# authors' original procedure (KodStp / TransEnt / Shuffled_TE / bootstrap
# loop in their "main" file).
# =============================================================================

# ---- Discretisation -----------------------------------------------------------
# Codes each column into length(quant)+1 classes using sample quantiles
# (type 8, as in the original KodStp): class 1 = x <= q1, class 2 = q1 < x <= q2, ...
te_discretise <- function(x, quant = c(10, 90)) {
  x <- as.matrix(x)
  apply(x, 2, function(col) {
    q <- stats::quantile(col, probs = quant / 100, type = 8, names = FALSE)
    findInterval(col, q, left.open = TRUE) + 1L
  })
}

# ---- Transfer entropy Y -> X ----------------------------------------------------
# x : integer vector (coded target series)
# y : integer matrix (coded source series; one or several columns = group)
# lx, ly : number of lags of x and of every column of y (default 1, as in the paper)
# Returns TE in bits:
#   sum p(x_{t+1}, x_t^(k), y_t^(l)) log2[ p(x_{t+1} | x_t^(k), y_t^(l)) / p(x_{t+1} | x_t^(k)) ]
te_shannon <- function(x, y, lx = 1, ly = 1) {
  y <- as.matrix(y)
  n <- length(x)
  m <- max(lx, ly)
  t <- (m + 1):n                                   # time index of x_{t+1}
  past <- function(v, l) {                         # string of the last l values
    do.call(paste, c(lapply(seq_len(l), function(k) v[t - k]), sep = ""))
  }
  xf <- x[t]
  xp <- past(x, lx)
  yp <- do.call(paste, c(lapply(seq_len(ncol(y)), function(j) past(y[, j], ly)), sep = "|"))

  n_xyz <- table(paste(xf, xp, yp))                # (x_{t+1}, x_t, y_t)
  key   <- strsplit(names(n_xyz), " ", fixed = TRUE)
  k_xf  <- vapply(key, `[`, "", 1); k_xp <- vapply(key, `[`, "", 2); k_yp <- vapply(key, `[`, "", 3)

  n_xy  <- table(paste(xp, yp))                    # (x_t, y_t)
  n_xx  <- table(paste(xf, xp))                    # (x_{t+1}, x_t)
  n_x   <- table(xp)                               # (x_t)

  # joint weights are divided by the series length n (not by the number of
  # transitions n - m), exactly as in the original TransEnt()/KlustGen() code
  p_xyz <- as.numeric(n_xyz) / n
  ratio <- (as.numeric(n_xyz) / as.numeric(n_xy[paste(k_xp, k_yp)])) /
           (as.numeric(n_xx[paste(k_xf, k_xp)]) / as.numeric(n_x[k_xp]))
  sum(p_xyz * log2(ratio))
}

# ---- Effective (shuffle-corrected) group TE with block-bootstrap s.e. ----------
# dat     : numeric matrix, one column per series (e.g. 5 cryptocurrencies)
# target  : column index of the receiving series X; all other columns form Y
# quant   : quantiles for discretisation, e.g. c(10, 90)
# nrep    : number of shuffles for the small-sample bias correction
# nboot   : number of block-bootstrap replications for the standard error
# blocklen: block length (default round(n^(1/5)), as in the original code)
# shuffle : "original" = as in the authors' Shuffled_TE_n3(): only the FIRST TWO
#                        source series are resampled for the bias correction
#                        (this is the procedure behind the published Table 7);
#           "all"      = every source series is resampled independently.
#           With a single source series both options are identical.
group_te <- function(dat, target, quant = c(10, 90), lx = 1, ly = 1,
                     nrep = 100, nboot = 400, blocklen = NULL,
                     shuffle = c("original", "all")) {
  shuffle <- match.arg(shuffle)
  dat <- as.matrix(dat)
  n   <- nrow(dat)
  if (is.null(blocklen)) blocklen <- round(n^(1 / 5))

  code <- te_discretise(dat, quant)
  x <- code[, target]; y <- code[, -target, drop = FALSE]

  te <- te_shannon(x, y, lx, ly)

  # shuffle (bias) correction: resampled source series lose their time
  # dependence with x; the mean TE over the shuffles is subtracted
  shuffle_te <- mean(replicate(nrep, {
    ys <- if (shuffle == "original" && ncol(y) >= 2) {
      cbind(sample(y[, 1], n, replace = TRUE), sample(y[, 2], n, replace = TRUE))
    } else {
      apply(y, 2, function(col) sample(col, n, replace = TRUE))
    }
    te_shannon(x, ys, lx, ly)
  }))
  ete <- te - shuffle_te

  # moving-block bootstrap of the joint series (re-discretised in each draw)
  boot <- replicate(nboot, {
    starts <- sample.int(n - blocklen, ceiling(n / blocklen) + 1, replace = TRUE)
    idx    <- as.vector(outer(0:(blocklen - 1), starts, `+`))[1:n]
    cb     <- te_discretise(dat[idx, , drop = FALSE], quant)
    te_shannon(cb[, target], cb[, -target, drop = FALSE], lx, ly)
  })
  se <- stats::sd(boot)

  tstat <- ete / se
  c(TE = te, Shuffle = shuffle_te, ETE = ete, SE = se,
    t = tstat, p = 2 * (1 - stats::pnorm(abs(tstat))))
}
