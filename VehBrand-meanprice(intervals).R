library(dplyr)
library(ggplot2)

set.seed(2)

freq_data <- read.csv("freMTPL2freq.csv", sep = ";")
sev_data <- read.csv("freMTPL2sev (1).csv")
insurance_data <- inner_join(sev_data, freq_data, by = "IDpol")

ci <- insurance_data %>% 
  filter(ClaimAmount <= quantile(ClaimAmount, 0.95, na.rm = TRUE)) %>%

  group_by(VehBrand) %>% 
  
  summarize(
    median_val = median(ClaimAmount, na.rm = TRUE), 
    n = n(), 
    boot_medians = list(
      replicate(1000, median(sample(na.omit(ClaimAmount), replace = TRUE)))
    ),
    .groups = "drop"
  ) %>%
  
  # Обчислюємо 95% довірчий інтервал з бутстреп-розподілу
  mutate(
    a = unname(sapply(boot_medians, quantile, probs = 0.025)),
    b = unname(sapply(boot_medians, quantile, probs = 0.975))
  ) %>%

  select(-boot_medians) %>%

  arrange(median_val)

print("Розраховані довірчі інтервали (бутстреп, відсортовано за медіаною):")
print(ci, width = Inf)

my_plot <- ggplot(ci, aes(x = median_val, y = reorder(VehBrand, median_val))) +
  
  geom_errorbar(aes(xmin = a, xmax = b), width = 0.2, color = "skyblue", linewidth = 1) +
  
  geom_point(color = "blue", size = 3) +
  
  labs(
    title = "Медіанний збиток за маркою автомобіля (Типові аварії)",
    subtitle = "Відсортовано за зростанням вартості (Бутстреп 1000 ітерацій, відкинуто 5%)",
    x = "Медіанний збиток (Claim Amount)",
    y = "Марка авто (VehBrand)"
  ) +
  
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    title = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank()
  )

print(my_plot)