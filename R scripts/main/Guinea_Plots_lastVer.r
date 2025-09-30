# ============================================================
# Guinea pig hierarchy, temperature, and cortisol – analysis
# ============================================================

# --- Libraries ------------------------------------------------
library(ggplot2)
library(ggpubr)
library(lattice)
library(dplyr)
library(carData)
library(gridExtra)

# --- Working directory & data load ---------------------------
setwd('/Users/ilsongjeon/Desktop/For_Job_Interviews/Guinea_pig/data')
data_1  <- read.csv('index_cortisol_1st.csv')   # Week 1
data_2  <- read.csv('index_cortisol_2nd.csv')   # Week 2
data_3  <- read.csv('index_cortisol.csv')       # Paired (Index_1, Index_2; Cortisol_1, Cortisol_2)
data_R1 <- read.csv('1st_Rang.csv')             # Week 1 by rank (Rang)
data_R2 <- read.csv('2nd_Rang.csv')             # Week 2 by rank (Rang)

names(data_3)

# --- Quick sanity checks -------------------------------------
# Avoid 'attach/detach'; use explicit data references instead.
mean(data_R1$Index_1, na.rm = TRUE)
sd(data_R1$Index_1, na.rm = TRUE)
summary(data_1)

# ============================================================
# 1) Temperature–Cortisol relationship per week
#    Note: normality checks; correlation; simple regression & plot
# ============================================================

# ---- Week 1 --------------------------------------------------
shapiro.test(data_1$Cortisol)     # Normality of cortisol (W1)
shapiro.test(data_1$Temperatur)   # Normality of temperature (W1)

# Group difference example (if 'Alter' is a grouping factor)
t.test(Temperatur ~ Alter, data = data_1)

# Correlation and linear model
cor.test(data_1$Temperatur, data_1$Cortisol, method = "pearson")
m1 <- lm(Temperatur ~ Cortisol, data = data_1)

ggplot(data_1, aes(x = Temperatur, y = Cortisol)) +
  geom_point() +
  # Use the fitted model rather than hard-coding slope/intercept
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Temperature (°C)", y = "Cortisol (ng/ml)",
       title = "Week 1: Cortisol vs Temperature") +
  theme_minimal()

# ---- Week 2 --------------------------------------------------
shapiro.test(data_2$Cortisol)     # Normality of cortisol (W2)
shapiro.test(data_2$Temperatur)   # Normality of temperature (W2)

# IMPORTANT: t.test(x, y) compares means of two numeric vectors (paired/unpaired),
# not temperature vs cortisol. Keep the groupwise t-test if you have a factor.
# Remove the incorrect 't.test(Temperatur, Cortisol)'.
# t.test(Temperatur, Cortisol)  # <- removed (not meaningful)

cor.test(data_2$Temperatur, data_2$Cortisol, method = "pearson")
m2 <- lm(Temperatur ~ Cortisol, data = data_2)

ggplot(data_2, aes(x = Temperatur, y = Cortisol)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Temperature (°C)", y = "Cortisol (ng/ml)",
       title = "Week 2: Cortisol vs Temperature") +
  theme_minimal()

# ============================================================
# 2) Hierarchy index comparison (Week 1 vs Week 2)
#    Rank correlations; pairwise inversions; paired test
# ============================================================

# Use only subjects present in both weeks
df_idx <- subset(data_3, is.finite(Index_1) & is.finite(Index_2))

# Rank associations
spearman_idx <- cor.test(df_idx$Index_1, df_idx$Index_2, method = "spearman", exact = FALSE)
kendall_idx  <- cor.test(df_idx$Index_1, df_idx$Index_2, method = "kendall",  exact = FALSE)
print(spearman_idx)
print(kendall_idx)

# Pairwise inversion rate (exclude perfect ties)
n  <- nrow(df_idx)
inv <- 0; tot <- 0
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    d1 <- df_idx$Index_1[i] - df_idx$Index_1[j]
    d2 <- df_idx$Index_2[i] - df_idx$Index_2[j]
    if (d1 != 0 && d2 != 0) {
      tot <- tot + 1
      if (d1 * d2 < 0) inv <- inv + 1
    }
  }
}
inv_rate <- inv / tot
cat(sprintf("Pairwise inversions: %d/%d (%.1f%%)\n", inv, tot, 100*inv_rate))

# Paired level shift: pick parametric/nonparametric by normality of differences
if (shapiro.test(df_idx$Index_1 - df_idx$Index_2)$p.value > 0.05) {
  lvl_idx <- t.test(df_idx$Index_1, df_idx$Index_2, paired = TRUE)
} else {
  lvl_idx <- wilcox.test(df_idx$Index_1, df_idx$Index_2, paired = TRUE, exact = FALSE)
}
lvl_idx

# Visualization: bar overlays + week lines
long_idx <- rbind(
  transform(data_3[, c("Code","Index_1")], Week = "Week1", Index = Index_1)[, c("Code","Week","Index")],
  transform(data_3[, c("Code","Index_2")], Week = "Week2", Index = Index_2)[, c("Code","Week","Index")]
)
lvl_code <- unique(c(data_1$Code, data_2$Code, data_3$Code))
long_idx$Code <- factor(long_idx$Code, levels = lvl_code)

ggplot() +
  geom_col(data = data_1, aes(x = Code, y = Index), fill = "red",  alpha = 0.20, width = 0.6) +
  geom_col(data = data_2, aes(x = Code, y = Index), fill = "blue", alpha = 0.20, width = 0.6) +
  geom_line(data = long_idx, aes(x = Code, y = Index, color = Week, group = Week),
            linewidth = 0.9, na.rm = TRUE) +
  geom_point(data = long_idx, aes(x = Code, y = Index, color = Week),
             size = 2.2, na.rm = TRUE) +
  scale_color_manual(values = c(Week1 = "red", Week2 = "blue"), guide = "none") +
  labs(y = "Hierarchy Index", x = "Individuals",
       title = "Hierarchy Index: Week 1 (Red) vs Week 2 (Blue)") +
  theme_minimal()

# ============================================================
# 3) Cortisol comparison (Week 1 vs Week 2)
#    Rank associations; inversion rate; paired test
# ============================================================

df_cor <- subset(data_3, is.finite(Cortisol_1) & is.finite(Cortisol_2))
spearman_cor <- cor.test(df_cor$Cortisol_1, df_cor$Cortisol_2, method = "spearman", exact = FALSE)
kendall_cor  <- cor.test(df_cor$Cortisol_1, df_cor$Cortisol_2, method = "kendall",  exact = FALSE)
print(spearman_cor)
print(kendall_cor)

# Pairwise inversions for cortisol
n  <- nrow(df_cor)
inv <- 0; tot <- 0
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    d1 <- df_cor$Cortisol_1[i] - df_cor$Cortisol_1[j]
    d2 <- df_cor$Cortisol_2[i] - df_cor$Cortisol_2[j]
    if (d1 != 0 && d2 != 0) {
      tot <- tot + 1
      if (d1 * d2 < 0) inv <- inv + 1
    }
  }
}
inv_rate <- inv / tot
cat(sprintf("Pairwise inversions: %d/%d (%.1f%%)\n", inv, tot, 100*inv_rate))

# Paired test for cortisol level shift
if (shapiro.test(df_cor$Cortisol_1 - df_cor$Cortisol_2)$p.value > 0.05) {
  lvl_cor <- t.test(df_cor$Cortisol_1, df_cor$Cortisol_2, paired = TRUE)
} else {
  lvl_cor <- wilcox.test(df_cor$Cortisol_1, df_cor$Cortisol_2, paired = TRUE, exact = FALSE)
}
lvl_cor

# Visualization: cortisol per code/week
long_cor <- rbind(
  transform(data_3[, c("Code","Cortisol_1")], Week = "Week1", Cortisol = Cortisol_1)[, c("Code","Week","Cortisol")],
  transform(data_3[, c("Code","Cortisol_2")], Week = "Week2", Cortisol = Cortisol_2)[, c("Code","Week","Cortisol")]
)
long_cor$Code <- factor(long_cor$Code, levels = lvl_code)

ggplot() +
  geom_col(data = data_1, aes(x = Code, y = Cortisol), fill = "red",  alpha = 0.20, width = 0.6) +
  geom_col(data = data_2, aes(x = Code, y = Cortisol), fill = "blue", alpha = 0.20, width = 0.6) +
  geom_line(data = long_cor, aes(x = Code, y = Cortisol, color = Week, group = Week),
            linewidth = 0.9, na.rm = TRUE) +
  geom_point(data = long_cor, aes(x = Code, y = Cortisol, color = Week),
             size = 2.2, na.rm = TRUE) +
  scale_color_manual(values = c(Week1 = "red", Week2 = "blue"), guide = "none") +
  labs(y = "Cortisol (ng/ml)", x = "Individuals",
       title = "Cortisol Level: Week 1 (Red) vs Week 2 (Blue)") +
  theme_minimal()

# ============================================================
# 4) Week-wise associations of hierarchy with temperature/cortisol
#    Choose parametric or nonparametric based on normality
# ============================================================

## Week 1 – Temperature by Dominance/Index
shapiro.test(data_1$Temperatur)               # Normality (W1 temperature)
t.test(Temperatur ~ Dominanz, data = data_1)  # Group difference (W1)
cor.test(data_1$Temperatur, data_1$Index, method = "pearson")

# Plot: Temperature vs Index (W1)
p_t_w1 <- ggplot(data_1, aes(x = Index, y = Temperatur)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(y = "Temperature (°C)", x = "Hierarchy Index",
       title = "Week 1: Temperature by Hierarchy Index") +
  stat_compare_means(method = "t.test", label.y = max(data_1$Temperatur, na.rm = TRUE)*1.05) +
  theme_minimal()

## Week 2 – Temperature by Dominance/Index
shapiro.test(data_2$Temperatur)               # Normality (W2 temperature)
t.test(Temperatur ~ Dominanz, data = data_2)  # Group difference (W2)
cor.test(data_2$Temperatur, data_2$Index, method = "pearson")

p_t_w2 <- ggplot(data_2, aes(x = Index, y = Temperatur)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(y = "Temperature (°C)", x = "Hierarchy Index",
       title = "Week 2: Temperature by Hierarchy Index") +
  theme_minimal()

grid.arrange(p_t_w1, p_t_w2, ncol = 2)

## Week 1 – Cortisol by Dominance/Index
shapiro.test(data_1$Cortisol)                 # Normality (W1 cortisol)
# Non-normal → Wilcoxon by rank group
wilcox.test(Cortisol ~ Dominanz, data = data_1)
cor.test(data_1$Cortisol, data_1$Index, method = "spearman")

p_c_w1 <- ggplot(data_1, aes(x = Index, y = Cortisol)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(y = "Cortisol (ng/ml)", x = "Hierarchy Index",
       title = "Week 1: Cortisol by Hierarchy Index") +
  theme_minimal()

## Week 2 – Cortisol by Dominance/Index
shapiro.test(data_2$Cortisol)                 # Normality (W2 cortisol)
t.test(Cortisol ~ Dominanz, data = data_2)    # If normal
cor.test(data_2$Cortisol, data_2$Index, method = "pearson")

p_c_w2 <- ggplot(data_2, aes(x = Index, y = Cortisol)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(y = "Cortisol (ng/ml)", x = "Hierarchy Index",
       title = "Week 2: Cortisol by Hierarchy Index") +
  theme_minimal()

grid.arrange(p_c_w1, p_c_w2, ncol = 2)

# ============================================================
# 5) Rank (Rang) vs cortisol/temperature by week
# ============================================================

# Week 1 – Rank vs Cortisol / Temperature
shapiro.test(data_R1$Cortisol_1)                       # Non-normal
wilcox.test(Cortisol_1 ~ Rang, data = data_R1)         # Rank–cortisol association (W1)
cor.test(data_R1$Cortisol_1, data_R1$Index_1, method = "spearman")

g_r1_cor <- ggplot(data_R1, aes(x = Rang, y = Cortisol_1)) +
  geom_boxplot() +
  geom_point() +
  stat_compare_means() +
  labs(y = "Cortisol (ng/ml)", x = "Rank",
       title = "Week 1: Rank vs Cortisol") +
  theme_minimal()

shapiro.test(data_R1$Temperatur)                       # Normal
t.test(Temperatur ~ Rang, data = data_R1)              # Rank–temperature (W1)
cor.test(data_R1$Temperatur, data_R1$Index_1, method = "pearson")

g_r1_temp_box <- ggplot(data_R1, aes(x = Rang, y = Temperatur)) +
  geom_boxplot() +
  geom_point() +
  stat_compare_means(method = 't.test') +
  labs(y = "Temperature (°C)", title = "Week 1: Temperature by Rank") +
  theme_minimal()

g_r1_temp_smooth <- ggplot(data_R1, aes(x = Index_1, y = Temperatur)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(y = "Temperature (°C)", x = "Hierarchy Index", title = "Week 1: Temperature vs Index") +
  theme_minimal()

# Week 2 – Rank vs Cortisol / Temperature
shapiro.test(data_R2$Cortisol_2)                       # Normal
t.test(Cortisol_2 ~ Rang, data = data_R2)

g_r2_cor <- ggplot(data_R2, aes(x = Rang, y = Cortisol_2)) +
  geom_boxplot() +
  geom_point() +
  stat_compare_means() +
  labs(y = "Cortisol (ng/ml)", x = "Rank",
       title = "Week 2: Rank vs Cortisol") +
  theme_minimal()

shapiro.test(data_R2$Temperatur)                       # Normal
t.test(Temperatur ~ Rang, data = data_R2)              # Not significant (as noted)

g_r2_temp_box <- ggplot(data_R2, aes(x = Rang, y = Temperatur)) +
  geom_boxplot() +
  geom_point() +
  stat_compare_means(method = 't.test') +
  labs(y = "Temperature (°C)", title = "Week 2: Temperature by Rank") +
  theme_minimal()

g_r2_temp_smooth <- ggplot(data_R2, aes(x = Index_2, y = Temperatur)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(y = "Temperature (°C)", x = "Hierarchy Index",
       title = "Week 2: Temperature vs Index") +
  theme_minimal()

grid.arrange(g_r1_cor, g_r2_cor, ncol = 2)
grid.arrange(g_r1_temp_box, g_r2_temp_box, ncol = 2)
grid.arrange(g_r1_temp_smooth, g_r2_temp_smooth, ncol = 2)

# ============================================================
# 6) Correlation between Week 1 & Week 2 hierarchy indices
# ============================================================

ggplot(data_3, aes(x = Index_1, y = Index_2)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope = 1.073184, intercept = 0.008567, linetype = "dashed") +
  labs(x = "Hierarchy Index – Week 1", y = "Hierarchy Index – Week 2",
       title = "Correlation between Week 1 and Week 2 Hierarchy Index") +
  theme_minimal()

lm(Index_1 ~ Index_2, data = data_3)
wilcox.test(data_3$Index_1, data_3$Index_2, paired = TRUE)  # Paired nonparametric alternative
cor(data_3$Index_1, data_3$Index_2)                          # Pearson r (no test)
cor.test(data_3$Index_1, data_3$Index_2, paired = TRUE)      # Paired cor. test (Pearson)
