library(dplyr)
library(ggplot2)

freq_data <- read.csv("freMTPL2freq.csv")
sev_data  <- read.csv("freMTPL2sev.csv")

# 1. Агрегація виплат по полісу
sev_grouped <- sev_data %>%
  group_by(IDpol) %>%
  summarise(TotalClaim = sum(ClaimAmount), .groups = "drop")

# 2. Об'єднання з основним датасетом
insurance_data <- freq_data %>%
  left_join(sev_grouped, by = "IDpol") %>%
  mutate(
    TotalClaim = ifelse(is.na(TotalClaim), 0, TotalClaim),
    BonusGroup = case_when(
      BonusMalus >= 50 & BonusMalus <= 80  ~ "50-80 (Супер)",
      BonusMalus >= 81 & BonusMalus <= 100 ~ "81-100 (Норма)",
      BonusMalus >= 101 & BonusMalus <= 120 ~ "101-120 (Ризик)",
      BonusMalus >= 121 & BonusMalus <= 200 ~ "121-200 (Погано)",
      TRUE ~ NA_character_
    ),
    BonusGroup = factor(
      BonusGroup,
      levels = c("50-80 (Супер)", "81-100 (Норма)", "101-120 (Ризик)", "121-200 (Погано)")
    )
  ) %>%
  filter(Exposure > 0, !is.na(BonusGroup))

ci_frequency_bonus <- insurance_data %>%
  group_by(BonusGroup) %>%
  summarise(
    TotalPolicies = n(),
    TotalClaims = sum(ClaimNb),
    TotalExposure = sum(Exposure),
    Frequency = TotalClaims / TotalExposure,
    SE = sqrt(TotalClaims) / TotalExposure,
    CI_low = Frequency + qnorm(0.025) * SE,
    CI_high = Frequency + qnorm(0.975) * SE,
    .groups = "drop"
  )

print("95% довірчі інтервали для частоти аварій за Bonus-Malus групами:")
print(ci_frequency_bonus, n = Inf, width = Inf)
write.csv(ci_frequency_bonus, "bonus_frequency_CI_table.csv", row.names = FALSE)

p_freq_bonus <- ggplot(ci_frequency_bonus, aes(x = BonusGroup, y = Frequency)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.12, linewidth = 0.9, color = "steelblue4") +
  geom_point(size = 3, color = "steelblue4") +
  geom_text(aes(label = round(Frequency, 3)), vjust = -1, size = 4, fontface = "bold") +
  labs(
    title = "Частота аварій за групами Bonus-Malus",
    subtitle = "95% довірчі інтервали, Frequency = TotalClaims / TotalExposure",
    x = "Група Bonus-Malus",
    y = "Частота аварій"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

print(p_freq_bonus)
ggsave("bonus_frequency_CI.png", p_freq_bonus, width = 9, height = 5, dpi = 300)

claims_only <- insurance_data %>%
  filter(TotalClaim > 0)

large_threshold_995 <- quantile(claims_only$TotalClaim, 0.995, na.rm = TRUE)

claims_bonus <- claims_only %>%
  mutate(
    LargeClaim = TotalClaim > large_threshold_995
  )

ci_large_bonus <- claims_bonus %>%
  group_by(BonusGroup) %>%
  summarise(
    n = n(),
    LargeClaims = sum(LargeClaim),
    LargeClaimRate = LargeClaims / n,
    SE = sqrt(LargeClaimRate * (1 - LargeClaimRate) / n),
    CI_low = pmax(0, LargeClaimRate + qnorm(0.025) * SE),
    CI_high = pmin(1, LargeClaimRate + qnorm(0.975) * SE),
    .groups = "drop"
  )

print("99.5-й перцентиль TotalClaim для визначення великої виплати:")
print(large_threshold_995)
print("95% довірчі інтервали для частки великих виплат за Bonus-Malus групами:")
print(ci_large_bonus, n = Inf, width = Inf)
write.csv(ci_large_bonus, "bonus_large_claim_rate_CI_table.csv", row.names = FALSE)

p_large_bonus <- ggplot(ci_large_bonus, aes(x = BonusGroup, y = LargeClaimRate)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.12, linewidth = 0.9, color = "firebrick4") +
  geom_point(size = 3, color = "firebrick4") +
  geom_text(aes(label = paste0(round(LargeClaimRate * 100, 2), "%")), vjust = -1, size = 4, fontface = "bold") +
  labs(
    title = "Частка великих виплат за групами Bonus-Malus",
    subtitle = "Велика виплата = верхні 0.5% TotalClaim; 95% довірчі інтервали для частки",
    x = "Група Bonus-Malus",
    y = "Частка великих виплат"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

print(p_large_bonus)
ggsave("bonus_large_claim_rate_CI.png", p_large_bonus, width = 9, height = 5, dpi = 300)

cap_995 <- quantile(claims_only$TotalClaim, 0.995, na.rm = TRUE)

claims_capped <- claims_only %>%
  mutate(
    CappedClaim = ifelse(TotalClaim > cap_995, cap_995, TotalClaim)
  )

ci_severity_bonus <- claims_capped %>%
  group_by(BonusGroup) %>%
  summarise(
    n = n(),
    mean = mean(CappedClaim, na.rm = TRUE),
    sd = sd(CappedClaim, na.rm = TRUE),
    SE = sd / sqrt(n),
    CI_low = mean + qnorm(0.025) * SE,
    CI_high = mean + qnorm(0.975) * SE,
    .groups = "drop"
  )

print("99.5-й перцентиль TotalClaim для capping:")
print(cap_995)
print("95% довірчі інтервали для середньої capped-виплати за Bonus-Malus групами:")
print(ci_severity_bonus, n = Inf, width = Inf)
write.csv(ci_severity_bonus, "bonus_severity_CI_table.csv", row.names = FALSE)

p_sev_bonus <- ggplot(ci_severity_bonus, aes(x = BonusGroup, y = mean)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.12, linewidth = 0.9, color = "darkorange3") +
  geom_point(size = 3, color = "darkorange3") +
  geom_text(aes(label = round(mean, 0)), vjust = -1, size = 4, fontface = "bold") +
  labs(
    title = "Середній розмір виплати за групами Bonus-Malus",
    subtitle = "95% довірчі інтервали, виплати обмежено 99.5-м перцентилем",
    x = "Група Bonus-Malus",
    y = "Середня capped-виплата, євро"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

print(p_sev_bonus)
ggsave("bonus_severity_CI.png", p_sev_bonus, width = 9, height = 5, dpi = 300)
