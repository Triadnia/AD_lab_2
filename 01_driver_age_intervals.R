
library(dplyr)
library(ggplot2)

freq_data <- read.csv("freMTPL2freq.csv")
sev_data  <- read.csv("freMTPL2sev.csv")

sev_grouped <- sev_data %>%
  group_by(IDpol) %>%
  summarise(TotalClaim = sum(ClaimAmount), .groups = "drop")

insurance_data <- freq_data %>%
  left_join(sev_grouped, by = "IDpol") %>%
  mutate(
    TotalClaim = ifelse(is.na(TotalClaim), 0, TotalClaim),
    ClaimFrequency = ClaimNb / Exposure,
    AgeGroup = case_when(
      DrivAge <= 25 ~ "18-25",
      DrivAge <= 40 ~ "26-40",
      DrivAge <= 60 ~ "41-60",
      TRUE ~ "60+"
    )
  ) %>%
  filter(Exposure > 0, DrivAge >= 18)

ci_frequency_age <- insurance_data %>%
  group_by(AgeGroup) %>%
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

print("95% довірчі інтервали для частоти аварій за віковими групами:")
print(ci_frequency_age, n = Inf, width = Inf)

claims_only <- insurance_data %>%
  filter(TotalClaim > 0)

cap_95 <- quantile(claims_only$TotalClaim, 0.95, na.rm = TRUE)

claims_capped <- claims_only %>%
  mutate(CappedClaim = ifelse(TotalClaim > cap_95, cap_95, TotalClaim))

ci_severity_age <- claims_capped %>%
  group_by(AgeGroup) %>%
  summarise(
    n = n(),
    mean = mean(CappedClaim, na.rm = TRUE),
    sd = sd(CappedClaim, na.rm = TRUE),
    SE = sd / sqrt(n),
    CI_low = mean + qnorm(0.025) * SE,
    CI_high = mean + qnorm(0.975) * SE,
    .groups = "drop"
  )

print("95% довірчі інтервали для середнього розміру виплати за віковими групами:")
print(ci_severity_age)

частота аварій з 95% CI
p1 <- ggplot(ci_frequency_age, aes(x = AgeGroup, y = Frequency)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.12, linewidth = 0.9, color = "steelblue4") +
  geom_point(size = 3, color = "steelblue4") +
  geom_text(aes(label = round(Frequency, 3)), vjust = -1, size = 4, fontface = "bold") +
  labs(
    title = "Частота аварій за віковими групами водіїв",
    subtitle = "95% довірчі інтервали, Frequency = TotalClaims / TotalExposure",
    x = "Вікова група водія",
    y = "Частота аварій"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(p1)
ggsave("driver_age_frequency_CI.png", p1, width = 9, height = 5, dpi = 300)

середній збиток з 95% CI
p2 <- ggplot(ci_severity_age, aes(x = AgeGroup, y = mean)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.12, linewidth = 0.9, color = "darkorange3") +
  geom_point(size = 3, color = "darkorange3") +
  geom_text(aes(label = round(mean, 0)), vjust = -1, size = 4, fontface = "bold") +
  labs(
    title = "Середній розмір виплати за віковими групами",
    subtitle = "95% довірчі інтервали, виплати обмежено 95-м перцентилем",
    x = "Вікова група водія",
    y = "Середня виплата, євро"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(p2)
ggsave("driver_age_severity_CI.png", p2, width = 9, height = 5, dpi = 300)
