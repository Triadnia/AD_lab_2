library(dplyr)
library(boot)
library(ggplot2)
freq <- read.csv("B:/AD/freMTPL2freq.csv")
sev  <- read.csv("B:/AD/freMTPL2sev.csv")

sev_agg <- sev %>% group_by(IDpol) %>% summarise(TotalClaimAmount = sum(ClaimAmount))
df_full <- inner_join(freq, sev_agg, by = "IDpol") %>% filter(TotalClaimAmount > 0)

cap_995 <- quantile(df_full$TotalClaimAmount, 0.995)
df_full$CappedClaim <- pmin(df_full$TotalClaimAmount, cap_995)

df_A <- df_full %>% filter(Area == "A")
df_F <- df_full %>% filter(Area == "F")
mean_A <- mean(df_A$CappedClaim)
mean_F <- mean(df_F$CappedClaim)
var_A <- var(df_A$CappedClaim)
var_F <- var(df_F$CappedClaim)
n_A <- nrow(df_A)
n_F <- nrow(df_F)

se_H1 <- sqrt(var_A/n_A + var_F/n_F)
ci_A <- c(mean_A - 1.96 * sqrt(var_A/n_A), mean_A + 1.96 * sqrt(var_A/n_A))
ci_F <- c(mean_F - 1.96 * sqrt(var_F/n_F), mean_F + 1.96 * sqrt(var_F/n_F))

W_H1 <- (mean_A - mean_F) / se_H1
pval_H1 <- 1 - pnorm(W_H1) # Односторонній тест (Area A > Area F)

prop_A <- mean(df_A$TotalClaimAmount > 3000)
prop_F <- mean(df_F$TotalClaimAmount > 3000)

se_H3 <- sqrt(prop_A*(1-prop_A)/n_A + prop_F*(1-prop_F)/n_F)
ci_pA <- c(prop_A - 1.96*sqrt(prop_A*(1-prop_A)/n_A), prop_A + 1.96*sqrt(prop_A*(1-prop_A)/n_A))
ci_pF <- c(prop_F - 1.96*sqrt(prop_F*(1-prop_F)/n_F), prop_F + 1.96*sqrt(prop_F*(1-prop_F)/n_F))

W_H3 <- (prop_A - prop_F) / se_H3
pval_H3 <- 1 - pnorm(W_H3)

padj <- p.adjust(c(pval_H1, pval_H3), method = "BH")

spearman_func <- function(data, indices) {
  d <- data[indices, ]
  return(cor(d$Density, d$CappedClaim, method = "spearman"))
}

set.seed(42)
boot_cor <- boot(df_full[, c("Density", "CappedClaim")], spearman_func, R = 2500)
ci_cor <- boot.ci(boot_cor, type = "perc")
get_h_text <- function(p) { if(p < 0.05) return("H0 ВІДХИЛЯЄТЬСЯ") else return("H0 НЕ ВІДХИЛЯЄТЬСЯ") }

cat(sprintf("[Г1.1 Середні]: Area A = %.2f € | Area F = %.2f €\n", mean_A, mean_F))
cat(sprintf("   -> Тест Вальда: W = %.3f, padj = %.5f. %s (Збитки за містом більші)\n\n", W_H1, padj[1], get_h_text(padj[1])))

cat(sprintf("[Г1.3 Частки Катастроф]: Area A = %.2f%% | Area F = %.2f%%\n", prop_A*100, prop_F*100))
cat(sprintf("   -> Тест Вальда: W = %.3f, padj = %.5f. %s (Катастроф за містом більше)\n\n", W_H3, padj[2], get_h_text(padj[2])))

cat(sprintf("[Г1.2 Спірмен]: Кореляція = %.4f\n", boot_cor$t0))
cat(sprintf("   -> Бутстреп ДІ (R=2500): [%.4f; %.4f]\n", ci_cor$percent[4], ci_cor$percent[5]))
if(ci_cor$percent[4] <= 0 & ci_cor$percent[5] >= 0) {
  cat("   -> ВИСНОВОК: Інтервал ПЕРЕТИНАЄ нуль. H0 НЕ ВІДХИЛЯЄТЬСЯ. Стабільного зв'язку немає.\n\n")
} else {
  cat("   -> ВИСНОВОК: Інтервал НЕ ПЕРЕТИНАЄ нуль. H0 ВІДХИЛЯЄТЬСЯ. Зв'язок підтверджено.\n\n")
}

plot_data <- data.frame(
  Area = c("Сільська місцевість (Area A)", "Мегаполіси (Area F)"),
  Mean = c(mean_A, mean_F),
  CI_Low = c(ci_A[1], ci_F[1]),
  CI_High = c(ci_A[2], ci_F[2])
)

g_plot1 <- ggplot(plot_data, aes(x = Area, y = Mean)) +
  geom_point(color = "darkgreen", size = 4) +
  geom_errorbar(aes(ymin = CI_Low, ymax = CI_High), width = 0.2, color = "forestgreen", size = 1) +
  coord_flip() +
  labs(
    title = "Середній розмір типового збитку за типом місцевості",
    subtitle = "Перевірено за допомогою критерію Вальда",
    x = "",
    y = "Середній розмір виплати (Євро)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13), axis.text.y = element_text(size = 11, face = "bold"))

print(g_plot1)

med_0 <- median(df_full$TotalClaimAmount)
all_regions <- unique(df_full$Region)
med_func <- function(data, indices) { return(median(data[indices])) }
results_all <- list()

for(reg in all_regions) {
  reg_data <- df_full %>% filter(Region == reg) %>% pull(TotalClaimAmount)
  if(length(reg_data) < 10) next 
  
  b_reg <- boot(reg_data, med_func, R = 2500)
  ci_reg <- boot.ci(b_reg, type = "perc")
  
  results_all[[reg]] <- data.frame(
    Region = reg,
    Median = b_reg$t0,
    CI_Low = ci_reg$percent[4],
    CI_High = ci_reg$percent[5]
  )
}

df_all_reg <- bind_rows(results_all)
df_all_reg$Region <- factor(df_all_reg$Region, levels = df_all_reg$Region[order(df_all_reg$Median)])

g_plot2 <- ggplot(df_all_reg, aes(x = Region, y = Median)) +
  geom_point(color = "darkblue", size = 3) +
  geom_errorbar(aes(ymin = CI_Low, ymax = CI_High), width = 0.4, color = "blue", size = 0.8) +
  geom_hline(yintercept = med_0, linetype = "dashed", color = "red", size = 1) +
  coord_flip() +
  labs(
    title = "Типовий збиток (Медіана) для всіх регіонів Франції",
    subtitle = paste("Червона пунктирна лінія - національна медіана", round(med_0, 1), "\nОцінка виключно за Бутстреп-інтервалами (без p-value)"),
    x = "Регіон",
    y = "Медіанний розмір виплати (Євро)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.y = element_text(size = 9, face = "bold"),
    panel.grid.minor = element_blank()
  )

print(g_plot2)