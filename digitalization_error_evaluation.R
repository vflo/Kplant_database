library(ggplot2)
library(scales)

#### 1 Generate synthetic plots ####
#
# 
# set.seed(42)
# out_dir <- "plots/synthetic_plots"
# dir.create(out_dir, showWarnings = FALSE)
# 
# true_values <- list()
# y_lab <- expression(K[plant] ~ (kg ~ s^{-1} ~ MPa^{-1}))
# 
# # Y-axis ranges mimicking typical figures in the literature
# y_ranges <- list(
#   c(0, 1.2e-4),
#   c(0, 8e-5),
#   c(0, 5e-5),
#   c(0, 3e-5),
#   c(0, 1e-5)
# )
# 
# # --- 5 Bar charts ---
# for (i in 1:5) {
#   n_bars <- sample(3:6, 1)
#   y_lim <- y_ranges[[i]]
#   vals <- runif(n_bars, y_lim[2] * 0.15, y_lim[2] * 0.85)
#   vals <- signif(vals, 3)
#   df <- data.frame(group = LETTERS[1:n_bars], y = vals)
#   
#   true_values[[paste0("bar_", i)]] <- data.frame(
#     plot = paste0("bar_", i), type = "bar", id = df$group, true_y = vals
#   )
#   
#   p <- ggplot(df, aes(group, y)) +
#     geom_col(fill = "steelblue", width = 0.6) +
#     scale_y_continuous(limits = y_lim, labels = scientific) +
#     labs(title = paste("Bar chart", i), x = "Treatment", y = y_lab) +
#     theme_minimal()
#   
#   ggsave(file.path(out_dir, paste0("bar_", i, ".png")), p,
#          width = 5, height = 4, dpi = 150)
# }
# 
# --- 5 Scatter plots ---
# for (i in 1:5) {
#   n_pts <- sample(8:15, 1)
#   y_lim <- y_ranges[[i]]
#   x_vals <- seq(1,2, length.out =n_pts)
#   y_vals <- runif(n_pts, y_lim[2] * 0.1, y_lim[2] * 0.9)
#   y_vals <- signif(y_vals, 3)
#   df <- data.frame(x = x_vals, y = y_vals)
# 
#   true_values[[paste0("scatter_", i)]] <- data.frame(
#     plot = paste0("scatter_", i), type = "scatter",
#     id = seq_len(n_pts), true_y = y_vals
#   )
# 
#   p <- ggplot(df, aes(x, y)) +
#     geom_point(size = runif(1,0.5,2.5)) +
#     scale_y_continuous(limits = y_lim, labels = scientific) +
#     labs(title = paste("Scatter plot", i),
#          x = expression(Psi[soil] ~ (MPa)), y = y_lab) +
#     theme_minimal()
# 
#   ggsave(file.path(out_dir, paste0("scatter_", i, ".png")), p,
#          width = 5, height = 4, dpi = 150)
# }
# # 
# true_df <- do.call(rbind, true_values)
# write.csv(true_df, "data_github/digitalization_error_true_values.csv", row.names = FALSE)


#### 2 Analysis ####

library(lme4)
library(readr)
df <- read_delim("data_github/digitalization_error_true_values_observed_values.csv", 
                 delim = "\t", escape_double = FALSE, trim_ws = TRUE)

df$diff <- df$measure - df$true_y

# Model
mod <- lmer(diff ~ 1 + (1 | Observer)+ (1|plot), data = df)
summary(mod)

# Fixed intercept (bias) with CI
bias <- fixef(mod)
ci <- confint(mod, parm = "(Intercept)", method = "Wald")

# Variance components
vc <- as.data.frame(VarCorr(mod))
sigma_obs <- vc$sdcor[vc$grp == "Observer"]
sigma_plot <- vc$sdcor[vc$grp == "plot"]
sigma_res <- vc$sdcor[vc$grp == "Residual"]
sigma_total <- sqrt(sigma_obs^2 + sigma_plot^2 + sigma_res^2)

# Bias as % of typical axis range
y_range <- max(df$true_y) - min(df$true_y)
bias_pct <- bias / y_range * 100
sigma_total_pct <- sigma_total / y_range * 100

cat("Mean bias (intercept):", formatC(bias, format = "e", digits = 3), "\n")
cat("95% CI:", formatC(ci[1], format = "e", digits = 3), "to",
    formatC(ci[2], format = "e", digits = 3), "\n")
cat("sigma_observer:", formatC(sigma_obs, format = "e", digits = 3), "\n")
cat("sigma_residual:", formatC(sigma_res, format = "e", digits = 3), "\n")
cat("sigma_total:", formatC(sigma_total, format = "e", digits = 3), "\n")
cat("Bias as % of range:", round(bias_pct, 3), "%\n")
cat("sigma_total as % of range:", round(sigma_total_pct, 3), "%\n")



