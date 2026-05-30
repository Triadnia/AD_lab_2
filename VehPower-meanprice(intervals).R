library(dplyr)
library(ggplot2)

set.seed(2)

freq_data <- read.csv("freMTPL2freq.csv", sep = ";")
sev_data <- read.csv("freMTPL2sev (1).csv")
insurance_data <- inner_join(sev_data, freq_data, by = "IDpol")

ci <- insurance_data %>% 
  filter(ClaimAmount <= quantile(ClaimAmount, 0.95, na.rm = TRUE)) %>%
  
  group_by(VehPower) %>% 
  
  summarize(
    median_val = median(ClaimAmount, na.rm = TRUE),
    n = n(),
    
    boot_medians = list(
      replicate(1000, median(sample(na.omit(ClaimAmount), replace = TRUE)))
    ),
    .groups = "drop"
  ) %>% 

  mutate(
    a = unname(sapply(boot_medians, quantile, probs = 0.025)),
    b = unname(sapply(boot_medians, quantile, probs = 0.975))
  ) %>% 
  
  select(-boot_medians)

print("Розраховані довірчі інтервали (бутстреп медіани):")
print(ci, width = Inf)

my_plot <- ggplot(ci, aes(x = median_val, y = factor(VehPower))) +

  geom_errorbar(aes(xmin = a, xmax = b), width = 0.2, color = "skyblue", linewidth = 1) +

  geom_point(color = "blue", size = 3) +

  labs(
    title = "Медіанний збиток за потужністю авто",
    subtitle = "З 95% довірчими інтервалами (Бутстреп 1000 ітерацій, відкинуто топ 5%)",
    x = "Медіанний збиток (Claim Amount)",
    y = "Потужність авто (VehPower)"
  ) +
  
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    title = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank()
  )

print(my_plot)