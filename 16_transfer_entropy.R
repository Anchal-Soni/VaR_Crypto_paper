# =============================================================================
# 16_transfer_entropy.R
#
# Purpose : Risk spillovers between the five cryptocurrencies, measured on the
#           1% VaR forecasts of the EGARCH models (Norm, SST, SGED, JSU, PSIV)
#           in each subsample:
#           (a) effective group transfer entropy (EGTE) towards each coin from
#               the other four, for quantile bins 5/95, 10/90 and 15/85, with
#               block-bootstrap standard errors;
#           (b) pairwise effective transfer entropy between every pair of
#               coins, both directions (RTransferEntropy package).
# Inputs  : VR* objects from 03f_var_series_1pct.R;
#           R/functions/transfer_entropy.R
# Outputs : output/tables/var_series/VaR_ss<k>_<dist>.csv  (the VaR series used)
#           output/tables/group_te_results.csv               (all EGTE results)
#           output/tables/pairwise_te_results.csv            (all pairwise results)
#           output/tables/table7_egte_q10.csv + LaTeX printed to the console
#           output/figures/EGTE_JSU_PSIV.eps                 (Figure 4)
#           output/figures/TE_pairwise_plot.eps              (Figure 5)
# Paper   : Table 7, Figure 4, Figure 5
#
# Run after R/00_setup.R and the preceding scripts (order: see README.md).
# =============================================================================

source(file.path("R", "functions", "transfer_entropy.R"))

# ---- Settings (defaults reproduce the published Table 7 / Figures 4-5) ----------
# Series fed into the transfer entropy:
#   "level"     : the 1% VaR forecasts themselves (used for the paper)
#   "logchange" : log(VaR_t / VaR_{t-1})
TE_INPUT <- "level"

# Bias correction for the group TE (see R/functions/transfer_entropy.R):
#   "original" : as in the authors' Shuffled_TE_n3(), which resamples only the
#                first two of the four source series (used for the paper)
#   "all"      : resample every source series
TE_SHUFFLE <- "original"

TE_NREP   <- 100        # shuffles for the bias correction
TE_NBOOT  <- 400        # block-bootstrap replications for the standard errors
TE_QUANTS <- list("5/95" = c(5, 95), "10/90" = c(10, 90), "15/85" = c(15, 85))
TE_TABLE_QUANT    <- "10/90"   # quantile bins used for Table 7
TE_ALPHA_PAIRWISE <- 0.05      # pairwise flows shown in Figure 5 if p < this

# Shuffles and bootstrap draws are random, so the third decimal of the results
# can differ slightly between runs/seeds.
set.seed(12345)

coins <- c("BTC", "ETH", "LTC", "XMR", "ETC")
# position k in the VR<ss><k>_<coin> objects created in 03f_var_series_1pct.R
dists <- c(Norm = 1, SST = 2, SGED = 3, JSU = 4, PSIV = 5)

# ---- 1. Collect the 1% VaR series: one matrix (time x coin) per dist and subsample
get_var_matrix <- function(ss, dist) {
  k <- dists[[dist]]
  m <- sapply(coins, function(cn) as.numeric(get(paste0("VR", ss, k, "_", cn))[, 1]))
  colnames(m) <- coins
  m
}

var_dir <- file.path(TAB_DIR, "var_series")
dir.create(var_dir, showWarnings = FALSE, recursive = TRUE)

te_input <- list()
for (ss in 1:3) for (d in names(dists)) {
  v <- get_var_matrix(ss, d)
  write.csv(v, file.path(var_dir, sprintf("VaR_ss%d_%s.csv", ss, d)), row.names = FALSE)
  te_input[[sprintf("SS%d_%s", ss, d)]] <-
    if (TE_INPUT == "logchange") log(v[-1, ] / v[-nrow(v), ]) else v
}

# ---- 2. Effective group transfer entropy -----------------------------------------
group_res <- list()
for (ss in 1:3) for (d in names(dists)) for (qn in names(TE_QUANTS)) {
  dat <- te_input[[sprintf("SS%d_%s", ss, d)]]
  for (j in seq_along(coins)) {
    r <- group_te(dat, target = j, quant = TE_QUANTS[[qn]],
                  nrep = TE_NREP, nboot = TE_NBOOT, shuffle = TE_SHUFFLE)
    group_res[[length(group_res) + 1]] <-
      data.frame(Subsample = paste0("SS", ss), Distribution = d, Quantiles = qn,
                 Receiver = coins[j], t(r), check.names = FALSE)
  }
}
group_res <- do.call(rbind, group_res)
write.csv(group_res, file.path(TAB_DIR, "group_te_results.csv"), row.names = FALSE)

# ---- 3. Table 7: EGTE (s.e.) at the 10/90 quantiles, SS2 and SS3 -------------------
tab7 <- subset(group_res, Quantiles == TE_TABLE_QUANT & Subsample %in% c("SS2", "SS3"))
tab7$cell <- sprintf("%.3f(%.3f)", tab7$ETE, tab7$SE)
tab7 <- reshape(tab7[, c("Distribution", "Subsample", "Receiver", "cell")],
                idvar = c("Distribution", "Subsample"), timevar = "Receiver",
                direction = "wide")
names(tab7) <- sub("^cell\\.", "", names(tab7))
tab7 <- tab7[order(match(tab7$Distribution, c("JSU", "PSIV", "SST", "SGED", "Norm")),
                   tab7$Subsample), c("Distribution", "Subsample", coins)]
rownames(tab7) <- NULL
write.csv(tab7, file.path(TAB_DIR, "table7_egte_q10.csv"), row.names = FALSE)
print(tab7)
print(xtable(tab7, caption = "EGTE for VaR across residual distributions (10/90 quantiles); standard errors in parentheses."),
      include.rownames = FALSE)

# ---- 4. Figure 4: EGTE with 90% intervals, JSU and PSIV, SS2 and SS3 --------------
fig4 <- subset(group_res, Distribution %in% c("JSU", "PSIV") & Subsample %in% c("SS2", "SS3"))
fig4$x <- match(fig4$Receiver, coins) + ifelse(fig4$Subsample == "SS3", length(coins) + 1, 0)
fig4$Distribution <- factor(fig4$Distribution, levels = c("JSU", "PSIV"))
fig4$Quantiles <- factor(paste0("Quantiles(", sub("/", ",", fig4$Quantiles), ")"),
                         levels = paste0("Quantiles(", sub("/", ",", names(TE_QUANTS)), ")"))
z90 <- qnorm(0.95)

egte_plot <- ggplot(fig4, aes(x = x, y = ETE)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = ETE - z90 * SE, ymax = ETE + z90 * SE), width = 0.3, colour = "blue") +
  geom_point(colour = "blue", size = 2) +
  facet_grid(Distribution ~ Quantiles) +
  scale_x_continuous(breaks = c(1:5, 7:11), labels = rep(coins, 2)) +
  labs(x = "SS2 (left) and SS3 (right)", y = "Effective group transfer entropy") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))

ggsave(filename = file.path(FIG_DIR, "EGTE_JSU_PSIV.eps"), plot = egte_plot,
       width = 300, height = 200, units = "mm")

# ---- 5. Pairwise effective transfer entropy (RTransferEntropy) ------------------
# Same call as in the original code: transfer_entropy(x, y, lx = 1, ly = 1,
# nboot = 400, q = 0.1). Note: q is the Renyi parameter (unused with the default
# Shannon entropy); the discretisation uses the package default quantiles c(5, 95).
# Each call returns both directions, x -> y and y -> x.
library(RTransferEntropy)
pair_res <- list()
for (ss in 1:3) for (d in c("JSU", "PSIV")) {
  dat <- te_input[[sprintf("SS%d_%s", ss, d)]]
  for (a in 1:(length(coins) - 1)) for (b in (a + 1):length(coins)) {
    res <- transfer_entropy(dat[, a], dat[, b], lx = 1, ly = 1, nboot = 400, q = 0.1,
                            quiet = TRUE, seed = 12345)
    cf  <- coef(res)   # rows: X->Y, Y->X; columns: te, ete, se, p-value
    pair_res[[length(pair_res) + 1]] <- data.frame(
      Subsample = paste0("SS", ss), Distribution = d,
      From = c(coins[a], coins[b]), To = c(coins[b], coins[a]),
      TE = cf[, 1], ETE = cf[, 2], SE = cf[, 3], p = cf[, 4], row.names = NULL)
  }
}
pair_res <- do.call(rbind, pair_res)
write.csv(pair_res, file.path(TAB_DIR, "pairwise_te_results.csv"), row.names = FALSE)

# ---- 6. Figure 5: significant pairwise flows ---------------------------------------
fig5 <- pair_res
fig5$ETE_sig <- ifelse(fig5$p < TE_ALPHA_PAIRWISE & fig5$ETE > 0, fig5$ETE, NA)
fig5$From <- factor(fig5$From, levels = coins)
fig5$To   <- factor(fig5$To,   levels = rev(coins))
fig5$Distribution <- factor(fig5$Distribution, levels = c("JSU", "PSIV"))

pair_plot <- ggplot(fig5, aes(x = From, y = To, fill = ETE_sig)) +
  geom_tile(colour = "white") +
  facet_grid(Distribution ~ Subsample) +
  scale_fill_gradient(low = "#c6dbef", high = "#08306b", na.value = "grey95",
                      name = "transfer entropy") +
  labs(x = "from", y = "to") +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = "bottom")

ggsave(filename = file.path(FIG_DIR, "TE_pairwise_plot.eps"), plot = pair_plot,
       width = 300, height = 200, units = "mm")
