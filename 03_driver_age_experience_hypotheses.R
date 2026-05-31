
library(dplyr)

freq_data <- read.csv("freMTPL2freq.csv")
sev_data  <- read.csv("freMTPL2sev.csv")

sev_grouped <- sev_data %>%
  group_by(IDpol) %>%
  summarise(TotalClaim = sum(ClaimAmount), .groups = "drop")

insurance_data <- freq_data %>%
  left_join(sev_grouped, by = "IDpol") %>%
  mutate(
    TotalClaim = ifelse(is.na(TotalClaim), 0, TotalClaim),
    
    AgeGroup = case_when(
      DrivAge <= 25 ~ "18-25",
      DrivAge <= 40 ~ "26-40",
      DrivAge <= 60 ~ "41-60",
      TRUE ~ "60+"
    ),
    
    MaxPossibleExperience = pmax(DrivAge - 18, 0),
    
    EstimatedExperience = case_when(
      is.na(BonusMalus) ~ NA_real_,
      BonusMalus < 100 ~ log(BonusMalus / 100) / log(0.95),
      TRUE ~ 0
    ),
    
    EstimatedExperience = pmin(EstimatedExperience, MaxPossibleExperience),
    EstimatedExperience = pmax(EstimatedExperience, 0),
    
    ExperienceGroup = case_when(
      EstimatedExperience < 3 ~ "0-2",
      EstimatedExperience < 6 ~ "3-5",
      EstimatedExperience < 10 ~ "6-9",
      TRUE ~ "10+"
    ),
    
    ExperienceGroup = factor(
      ExperienceGroup,
      levels = c("0-2", "3-5", "6-9", "10+")
    )
  ) %>%
  filter(Exposure > 0, DrivAge >= 18)

claims_only <- insurance_data %>% 
  filter(TotalClaim > 0)

cap_95 <- quantile(claims_only$TotalClaim, 0.95, na.rm = TRUE)

claims_capped <- claims_only %>%
  mutate(CappedClaim = ifelse(TotalClaim > cap_95, cap_95, TotalClaim))


wald_test_means <- function(x, y, alternative = "two.sided") {
  nx <- length(x)
  ny <- length(y)
  diff <- mean(x) - mean(y)
  se <- sqrt(var(x) / nx + var(y) / ny)
  w <- diff / se
  
  if (alternative == "two.sided") {
    p <- 2 * (1 - pnorm(abs(w)))
  } else if (alternative == "greater") {
    p <- 1 - pnorm(w)
  } else if (alternative == "less") {
    p <- pnorm(w)
  }
  
  return(list(Diff = diff, SE = se, Statistic = w, P_value = p))
}




# H0: lambda_young = lambda_others
# H1: lambda_young > lambda_others

young_freq_data <- insurance_data %>%
  filter(AgeGroup == "18-25")

other_freq_data <- insurance_data %>%
  filter(AgeGroup != "18-25")

claims_young <- sum(young_freq_data$ClaimNb)
exposure_young <- sum(young_freq_data$Exposure)

claims_others <- sum(other_freq_data$ClaimNb)
exposure_others <- sum(other_freq_data$Exposure)

lambda_young <- claims_young / exposure_young
lambda_others <- claims_others / exposure_others

diff_h1 <- lambda_young - lambda_others

se_h1 <- sqrt(
  claims_young / exposure_young^2 +
    claims_others / exposure_others^2
)

wald_h1 <- diff_h1 / se_h1
p_h1 <- 1 - pnorm(wald_h1)

res_h1 <- list(
  Diff = diff_h1,
  SE = se_h1,
  Statistic = wald_h1,
  P_value = p_h1,
  Lambda_young = lambda_young,
  Lambda_others = lambda_others
)

cat("\n--- Гіпотеза 1: Молоді водії мають вищу частоту аварій ---\n")
cat(sprintf("Частота молодих водіїв: %.6f\n", res_h1$Lambda_young))
cat(sprintf("Частота інших водіїв: %.6f\n", res_h1$Lambda_others))
cat(sprintf("Різниця частот: %.6f\nSE: %.6f\nСтатистика Волда: %.3f\nP-value: %.6f\n",
            res_h1$Diff, res_h1$SE, res_h1$Statistic, res_h1$P_value))


# H0: mean(CappedClaim young) = mean(CappedClaim others)
# H1: mean(CappedClaim young) > mean(CappedClaim others)

claim_young <- claims_capped %>% 
  filter(AgeGroup == "18-25") %>% 
  pull(CappedClaim)

claim_others <- claims_capped %>% 
  filter(AgeGroup != "18-25") %>% 
  pull(CappedClaim)

res_h2 <- wald_test_means(
  claim_young, 
  claim_others, 
  alternative = "greater"
)

cat("\n--- Гіпотеза 2: Молоді водії мають вищий середній розмір виплати ---\n")
cat(sprintf("Різниця середніх виплат: %.2f\nSE: %.2f\nСтатистика Волда: %.3f\nP-value: %.6f\n",
            res_h2$Diff, res_h2$SE, res_h2$Statistic, res_h2$P_value))



# H0: mean(CappedClaim exp<5) = mean(CappedClaim exp>=5)
# H1: mean(CappedClaim exp<5) > mean(CappedClaim exp>=5)
claim_exp_low <- claims_capped %>% 
  filter(ExperienceGroup == "0-2") %>% 
  pull(CappedClaim)

claim_exp_high <- claims_capped %>% 
  filter(ExperienceGroup != "0-2") %>% 
  pull(CappedClaim)

res_h3 <- wald_test_means(
  claim_exp_low, 
  claim_exp_high, 
  alternative = "greater"
)

cat("\n--- Гіпотеза 3: Найнижчий оцінний стаж 0-2 роки пов'язаний із вищим середнім збитком ---\n")
cat(sprintf("Різниця середніх виплат: %.2f\nSE: %.2f\nСтатистика Волда: %.3f\nP-value: %.6f\n",
            res_h3$Diff, res_h3$SE, res_h3$Statistic, res_h3$P_value))

low_exp_data <- insurance_data %>%
  filter(ExperienceGroup == "0-2")

high_exp_data <- insurance_data %>%
  filter(ExperienceGroup != "0-2")

claims_low_exp <- sum(low_exp_data$ClaimNb)
exposure_low_exp <- sum(low_exp_data$Exposure)

claims_high_exp <- sum(high_exp_data$ClaimNb)
exposure_high_exp <- sum(high_exp_data$Exposure)

lambda_low_exp <- claims_low_exp / exposure_low_exp
lambda_high_exp <- claims_high_exp / exposure_high_exp

diff_h4 <- lambda_low_exp - lambda_high_exp

se_h4 <- sqrt(
  claims_low_exp / exposure_low_exp^2 +
    claims_high_exp / exposure_high_exp^2
)

wald_h4 <- diff_h4 / se_h4
p_h4 <- 1 - pnorm(wald_h4)

res_h4 <- list(
  Diff = diff_h4,
  SE = se_h4,
  Statistic = wald_h4,
  P_value = p_h4,
  Lambda_low_exp = lambda_low_exp,
  Lambda_high_exp = lambda_high_exp
)

cat("\n--- Гіпотеза 4: Найнижчий оцінний стаж 0-2 роки пов'язаний із вищою частотою аварій ---\n")
cat(sprintf("Частота водіїв зі стажем 0-2 роки: %.6f\n", res_h4$Lambda_low_exp))
cat(sprintf("Частота інших водіїв: %.6f\n", res_h4$Lambda_high_exp))
cat(sprintf("Різниця частот: %.6f\nSE: %.6f\nСтатистика Волда: %.3f\nP-value: %.6f\n",
            res_h4$Diff, res_h4$SE, res_h4$Statistic, res_h4$P_value))

raw_pvals <- c(
  H1_Frequency_Young = res_h1$P_value,
  H2_Severity_Young = res_h2$P_value,
  H3_Severity_LowExperience = res_h3$P_value,
  H4_Frequency_LowExperience = res_h4$P_value
)

adjusted_pvals <- p.adjust(raw_pvals, method = "BH")

results <- data.frame(
  Hypothesis = names(raw_pvals),
  Raw_P_Value = raw_pvals,
  Adjusted_P_Value_BH = adjusted_pvals
)

cat("\n--- Корекція p-value методом Benjamini-Hochberg ---\n")
print(results)