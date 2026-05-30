library(dplyr)
library(ggplot2)

set.seed(123)

# 1. Завантаження та об'єднання даних
freq_data <- read.csv("freMTPL2freq.csv", sep = ";")
sev_data <- read.csv("freMTPL2sev (1).csv")
insurance_data <- inner_join(sev_data, freq_data, by = "IDpol")

# 2. Розрахунок даних та інтервалів виключно для середнього
ci <- insurance_data %>% 
  filter(ClaimAmount <= quantile(ClaimAmount, 0.95, na.rm = TRUE)) %>%
  group_by(VehGas) %>% 
  summarize(
    mean_val = mean(ClaimAmount, na.rm = TRUE),
    sd_val = sd(ClaimAmount, na.rm = TRUE), 
    n = n(), 
    .groups = "drop"
  ) %>%
  mutate(
    # Класичні межі для середнього (через t-розподіл)
    se = sd_val / sqrt(n),
    t_crit = qt(0.975, df = n - 1),
    LowerCI = mean_val - t_crit * se,
    UpperCI = mean_val + t_crit * se,
    Metric = "Середнє (Аналітично)" # Змінна для красивої легенди на графіку
  ) %>%
  # Відбираємо та перейменовуємо колонки для зручності
  select(VehGas, n, Metric, Estimate = mean_val, LowerCI, UpperCI)

print("Розраховані дані для графіка:")
print(ci, n = Inf, width = Inf)

# 3. Побудова графіка
my_plot <- ggplot(ci, aes(x = Estimate, y = factor(VehGas), color = Metric)) +
  
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI), 
                width = 0.2, linewidth = 1) +
  geom_point(size = 3) +
  
  scale_color_manual(values = c("Середнє (Аналітично)" = "red")) +
  
  labs(
    title = "Середній збиток за типом палива",
    subtitle = "95% довірчі інтервали (відкинуто 5% екстремальних викидів)",
    x = "Сума збитку (Claim Amount)",
    y = "Тип палива (VehGas)",
    color = "Показник:"
  ) +
  
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    title = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 11)
  )

print(my_plot)