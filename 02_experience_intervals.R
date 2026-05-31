library(dplyr)
library(ggplot2)

freq_data <- read.csv("freMTPL2freq.csv")
sev_data  <- read.csv("freMTPL2sev.csv")

sev_grouped <- sev_data %>%
  group_by(IDpol) %>%
  summarise(TotalClaim = sum(ClaimAmount), .groups = "drop")

insurance_data <- freq_data %>%
  left_join(sev_grouped, by = "IDpol") %>%
  mutate(TotalClaim = ifelse(is.na(TotalClaim), 0, TotalClaim)) %>%
  filter(Exposure > 0, DrivAge >= 18)

insurance_data <- insurance_data %>%
  mutate(
    MaxPossibleExperience = pmax(DrivAge - 18, 0),
    
    EstimatedExperience = case_when(
      is.na(BonusMalus) ~ NA_real_,
      BonusMalus < 100 ~ log(BonusMalus / 100) / log(0.95),
      TRUE ~ 0
    ),
    
    # Обмеження: оцінний стаж не може бути більшим за фізично можливий
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
  )
ci_frequency_exp <- insurance_data %>%
  group_by(ExperienceGroup) %>%
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

print("95% довірчі інтервали для частоти аварій за групами оцінного стажу:")
print(ci_frequency_exp, n = Inf, width = Inf)

write.csv(ci_frequency_exp, "experience_frequency_CI_table.csv", row.names = FALSE)

p_freq <- ggplot(ci_frequency_exp, aes(x = ExperienceGroup, y = Frequency)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.12, linewidth = 0.9, color = "steelblue4") +
  geom_point(size = 3, color = "steelblue4") +
  geom_text(aes(label = round(Frequency, 3)), vjust = -1, size = 4, fontface = "bold") +
  labs(
    title = "Частота аварій за оцінним стажем водіння",
    subtitle = "95% довірчі інтервали, Frequency = TotalClaims / TotalExposure",
    x = "Оцінний стаж водіння, років",
    y = "Частота аварій"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(p_freq)
ggsave("experience_frequency_CI.png", p_freq, width = 9, height = 5, dpi = 300)

claims_only <- insurance_data %>% 
  filter(TotalClaim > 0)

cap_95 <- quantile(claims_only$TotalClaim, 0.95, na.rm = TRUE)

claims_capped <- claims_only %>%
  mutate(CappedClaim = ifelse(TotalClaim > cap_95, cap_95, TotalClaim))

ci_severity_exp <- claims_capped %>%
  group_by(ExperienceGroup) %>%
  summarise(
    n = n(),
    mean = mean(CappedClaim, na.rm = TRUE),
    sd = sd(CappedClaim, na.rm = TRUE),
    SE = sd / sqrt(n),
    CI_low = mean + qnorm(0.025) * SE,
    CI_high = mean + qnorm(0.975) * SE,
    .groups = "drop"
  )

print("95% довірчі інтервали для середнього розміру виплати за групами стажу:")
print(ci_severity_exp, n = Inf, width = Inf)

write.csv(ci_severity_exp, "experience_severity_CI_table.csv", row.names = FALSE)

p <- ggplot(ci_severity_exp, aes(x = ExperienceGroup, y = mean)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.12, linewidth = 0.9, color = "firebrick4") +
  geom_point(size = 3, color = "firebrick4") +
  geom_text(aes(label = round(mean, 0)), vjust = -1, size = 4, fontface = "bold") +
  labs(
    title = "Середній розмір виплати за оцінним стажем водіння",
    subtitle = "95% довірчі інтервали, виплати обмежено 95-м перцентилем",
    x = "Оцінний стаж водіння, років",
    y = "Середня виплата, євро"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(p)
ggsave("experience_severity_CI.png", p, width = 9, height = 5, dpi = 300)