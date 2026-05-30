library(data.table)

# 1. Завантаження даних
freq <- fread("freMTPL2freq.csv")
sev <- fread("freMTPL2sev (1).csv")

df <- merge(sev, freq, by = "IDpol", all.x = TRUE)

# ВАЖЛИВО: Додано VehGas у список колонок, щоб зберегти тип палива
df <- na.omit(df[, .(ClaimAmount, VehAge, VehPower, VehBrand, VehGas)])

p95_threshold <- quantile(df$ClaimAmount, probs = 0.95)
cat("Межа для 95% типових збитків:", p95_threshold, "\n")

# Відсікаємо аномально великі збитки
df <- df[ClaimAmount <= p95_threshold]

# Функція для бутстреп тесту Вальда (порівняння медіан)
wald_test_medians_boot <- function(x, y, B = 1000) {
  nx <- length(x); ny <- length(y)
  med_diff <- median(x) - median(y)
  set.seed(42)
  boot_diffs <- replicate(B, {
    median(sample(x, nx, replace = TRUE)) - median(sample(y, ny, replace = TRUE))
  })
  se_diff <- sd(boot_diffs)
  w_stat <- med_diff / se_diff
  pval <- 2 * (1 - pnorm(abs(w_stat)))
  return(list(P_value = pval, Diff = med_diff))
}

# Функція для виведення результатів
print_significance <- function(p_values) {
  res <- data.frame(
    P_value = p_values,
    Significance = ifelse(p_values < 0.05, "Значуща", "Не значуща")
  )
  print(res)
}

# ==============================================================================
cat("\n--- Гіпотеза 1: Вік авто (Всі порівняння медіан) ---\n")
age_0_3  <- df[VehAge <= 3, ClaimAmount]
age_4_10 <- df[VehAge >= 4 & VehAge <= 10, ClaimAmount]
age_11_plus <- df[VehAge >= 11, ClaimAmount]

p_age_1_2 <- wald_test_medians_boot(age_0_3, age_4_10)$P_value
p_age_1_3 <- wald_test_medians_boot(age_0_3, age_11_plus)$P_value
p_age_2_3 <- wald_test_medians_boot(age_4_10, age_11_plus)$P_value

adj_age_p <- p.adjust(c(
  "0-3 vs 4-10" = p_age_1_2, 
  "0-3 vs 11+"  = p_age_1_3, 
  "4-10 vs 11+" = p_age_2_3
), method = "BH")

print_significance(adj_age_p)

# ==============================================================================
cat("\n--- Гіпотеза 2: Потужність авто (Всі порівняння медіан) ---\n")
pow_0_5 <- df[VehPower <= 6, ClaimAmount]
pow_6_8 <- df[VehPower >= 7 & VehPower <= 9, ClaimAmount]
pow_9_plus <- df[VehPower >= 10, ClaimAmount]

p_pow_1_2 <- wald_test_medians_boot(pow_0_5, pow_6_8, B = 1000)$P_value
p_pow_1_3 <- wald_test_medians_boot(pow_0_5, pow_9_plus, B = 1000)$P_value
p_pow_2_3 <- wald_test_medians_boot(pow_6_8, pow_9_plus, B = 1000)$P_value

adj_pow_p <- p.adjust(c(
  "Pow 4-6 vs 7-9" = p_pow_1_2, 
  "Pow 4-6 vs 10+"  = p_pow_1_3, 
  "Pow 7-9 vs 10+"  = p_pow_2_3
), method = "BH")

print_significance(adj_pow_p)

# ==============================================================================
cat("\n--- Гіпотеза 3: Вплив Бренду (Порівняння з B1 за медіаною) ---\n")
brand_base <- df[VehBrand == "B1", ClaimAmount]
brands_to_test <- c("B2", "B3", "B12")

p_brands <- sapply(brands_to_test, function(b) {
  wald_test_medians_boot(brand_base, df[VehBrand == b, ClaimAmount])$P_value
})

adj_brand_p <- p.adjust(p_brands, method = "BH")
names(adj_brand_p) <- paste("B1 vs", brands_to_test)

print_significance(adj_brand_p)

# ==============================================================================
# ОНОВЛЕНО: Гіпотеза 4 (Тільки середні)
# ==============================================================================
cat("\n--- Гіпотеза 4: Тип палива (Дизель vs Бензин за середнім збитком) ---\n")

diesel <- df[VehGas == "Diesel", ClaimAmount]
regular <- df[VehGas == "Regular", ClaimAmount] # У цій базі бензин позначений як 'Regular'

cat(sprintf("Середній збиток (Дизель): %.2f\n", mean(diesel)))
cat(sprintf("Середній збиток (Бензин): %.2f\n\n", mean(regular)))

# Перевірка: Порівняння середніх через T-тест
t_test_gas <- t.test(diesel, regular, var.equal = FALSE)

gas_p <- c(
  "Середні (Welch T-test)" = t_test_gas$p.value
)

cat("Статистична значущість різниці між дизелем та бензином:\n")
print_significance(gas_p)