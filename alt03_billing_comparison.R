# Public example: invented data and generic provider identifiers only.
# Run from the repository root after generate_example_data.R.
if (!file.exists("README.md") || !dir.exists("data")) stop("Run from the project folder after generating example data.")
library(lubridate)
dir.create("results/alt03", recursive = TRUE, showWarnings = FALSE)
dir.create("results/alt03/comparison", recursive = TRUE, showWarnings = FALSE)

library(tidyverse)
library(readxl)
library(writexl)
library(scales)

actual_new <- read_csv("data/synthetic_billing_use.csv")

new_clean <- actual_new %>%
  rename(
    retailer = `Retailer`,
    year     = `Cal. Year`,
    month    = `Data Month`,
    demand   = `Total Water Use`
  ) %>%
  mutate(
    year  = as.integer(year),
    month = as.integer(month)
  ) %>%
  filter(year > 0, year < 2025) %>%
  mutate(retailer = if_else(retailer == "Example_B_Alias", "Example_B", retailer)) %>%
  group_by(retailer, year, month) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

yearly_coverage <- new_clean %>%
  group_by(retailer, year) %>%
  summarise(n_months = n(), .groups = "drop") %>%
  filter(n_months == 12) %>%
  group_by(year) %>%
  summarise(n_complete_retailers = n(), .groups = "drop")

n_retailers <- length(unique(new_clean$retailer))

start_year <- yearly_coverage %>%
  filter(n_complete_retailers == n_retailers) %>%
  pull(year) %>%
  min()

cat("Total retailers:", n_retailers, "\n")
cat("Starting year for analysis:", start_year, "\n")

new_clean <- new_clean %>%
  filter(year >= start_year)

incomplete_combos <- new_clean %>%
  group_by(retailer, year) %>%
  summarise(n_months = n(), .groups = "drop") %>%
  filter(n_months != 12)

cat("Incomplete retailer-year combinations dropped:\n")
print(incomplete_combos)

new_clean <- new_clean %>%
  anti_join(incomplete_combos, by = c("retailer", "year"))

check <- new_clean %>%
  group_by(retailer, year) %>%
  summarise(n_months = n(), .groups = "drop") %>%
  filter(n_months != 12)

if (nrow(check) == 0) {
  message("✅ All retailer-years have complete 12 months of data")
} else {
  warning("Some incomplete data remains:")
  print(check)
}

cat("\nClean data summary:\n")
cat("Retailers:", length(unique(new_clean$retailer)), "\n")
cat("Year range:", min(new_clean$year), "-", max(new_clean$year), "\n")
cat("Total observations:", nrow(new_clean), "\n")

new_clean <- new_clean %>%
  group_by(retailer, year) %>%
  mutate(
    annual_total = sum(demand, na.rm = TRUE),
    fraction     = demand / annual_total
  ) %>%
  ungroup()

message("✅ Data preparation complete!")

read_noaa_sheet <- function(sheet_name, param_name) {
  read_excel("data/synthetic_climate.xlsx",
             sheet = sheet_name) %>%
    pivot_longer(
      cols      = -month,
      names_to  = "year",
      values_to = param_name
    ) %>%
    mutate(
      year  = as.integer(year),
      month = match(month, month.abb)
    )
}

pdsi_long <- read_noaa_sheet("Palmer Drought Severity Index", "pdsi") %>%
  mutate(
    regime = case_when(
      pdsi <= -3 ~ "Drought",
      pdsi >= 2  ~ "Wet",
      TRUE       ~ "Normal"
    )
  )

year_class <- pdsi_long %>%
  filter(year %in% unique(new_clean$year)) %>%
  group_by(year) %>%
  summarise(
    n_drought = sum(regime == "Drought"),
    n_normal  = sum(regime == "Normal"),
    n_wet     = sum(regime == "Wet"),
    .groups   = "drop"
  ) %>%
  mutate(
    year_regime = case_when(
      n_drought >= 7 ~ "Drought",
      n_normal  >= 7 ~ "Normal",
      n_wet     >= 7 ~ "Wet",
      TRUE           ~ "Mixed"
    )
  )

cat("Drought years:", year_class %>% filter(year_regime == "Drought") %>% pull(year), "\n")
cat("Normal years:",  year_class %>% filter(year_regime == "Normal")  %>% pull(year), "\n")
cat("Wet years:",     year_class %>% filter(year_regime == "Wet")     %>% pull(year), "\n")
cat("Mixed years:",   year_class %>% filter(year_regime == "Mixed")   %>% pull(year), "\n")

new_classified <- new_clean %>%
  left_join(year_class %>% select(year, year_regime), by = "year")

monthly_fractions <- new_classified %>%
  filter(year_regime %in% c("Drought", "Normal")) %>%
  group_by(retailer, month, year_regime) %>%
  summarise(
    median_fraction = median(fraction, na.rm = TRUE),
    .groups         = "drop"
  )

drought_scenario <- monthly_fractions %>%
  filter(year_regime == "Drought") %>%
  group_by(retailer) %>%
  mutate(final_fraction = median_fraction / sum(median_fraction)) %>%
  ungroup()

normal_scenario <- monthly_fractions %>%
  filter(year_regime == "Normal") %>%
  group_by(retailer) %>%
  mutate(final_fraction = median_fraction / sum(median_fraction)) %>%
  ungroup()

cat("\nDrought scenario rows:", nrow(drought_scenario), "\n")
cat("Normal scenario rows:",  nrow(normal_scenario),  "\n")

actual_summary <- new_clean %>%
  group_by(retailer, month) %>%
  summarise(
    median_frac = median(fraction, na.rm = TRUE),
    min_frac    = min(fraction, na.rm = TRUE),
    max_frac    = max(fraction, na.rm = TRUE),
    .groups     = "drop"
  )

drought_years_list <- year_class %>% filter(year_regime == "Drought") %>% pull(year)
normal_years_list  <- year_class %>% filter(year_regime == "Normal")  %>% pull(year)
wet_years_list     <- year_class %>% filter(year_regime == "Wet")     %>% pull(year)
mixed_years_list   <- year_class %>% filter(year_regime == "Mixed")   %>% pull(year)

year_colors <- c(
  setNames(colorRampPalette(c("#FFAAAA", "#CC0000"))(length(drought_years_list)), as.character(drought_years_list)),
  setNames(colorRampPalette(c("#AED6F1", "#1A5276"))(length(wet_years_list)),     as.character(wet_years_list)),
  setNames(colorRampPalette(c("#90EE90", "#006400"))(length(normal_years_list)),  as.character(normal_years_list)),
  setNames(rep("#A9A9A9", length(mixed_years_list)),                               as.character(mixed_years_list))
)

retailers_list <- unique(new_clean$retailer)

for (r in retailers_list) {
  
  p <- ggplot() +
    geom_ribbon(data = actual_summary %>% filter(retailer == r),
                aes(x = month, ymin = min_frac, ymax = max_frac),
                fill = "grey80", alpha = 0.3) +
    geom_line(data = actual_summary %>% filter(retailer == r),
              aes(x = month, y = median_frac),
              color = "grey40", linewidth = 1) +
    geom_line(data = new_clean %>% filter(retailer == r),
              aes(x = month, y = fraction, group = year, color = factor(year)),
              linewidth = 0.6, alpha = 0.8) +
    geom_line(data = normal_scenario %>% filter(retailer == r),
              aes(x = month, y = final_fraction),
              color = "black", linewidth = 1.5, linetype = "dashed") +
    geom_line(data = drought_scenario %>% filter(retailer == r),
              aes(x = month, y = final_fraction),
              color = "darkred", linewidth = 1.5, linetype = "dashed") +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_color_manual(values = year_colors) +
    labs(
      title    = paste("Synthetic Monthly Demand Pattern —", r),
      subtitle = "Grey band = min/max | Grey line = median | Red = drought | Green = normal | Blue = wet | Grey lines = mixed\nBlack dashed = WEAP Normal | Red dashed = WEAP Drought",
      x        = NULL,
      y        = "Fraction of Annual Demand",
      color    = "Year"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      legend.position  = "right"
    )
  
  ggsave(
    paste0("results/alt03/plot_",
           gsub(" ", "_", r), ".png"),
    p, width = 12, height = 7, dpi = 150
  )
}

message("✅ Plots saved!")

monthly_fractions_mean <- new_classified %>%
  filter(year_regime %in% c("Drought", "Normal")) %>%
  group_by(retailer, month, year_regime) %>%
  summarise(
    mean_fraction = mean(fraction, na.rm = TRUE),
    .groups       = "drop"
  )

drought_scenario_mean <- monthly_fractions_mean %>%
  filter(year_regime == "Drought") %>%
  group_by(retailer) %>%
  mutate(final_fraction = mean_fraction / sum(mean_fraction)) %>%
  ungroup()

normal_scenario_mean <- monthly_fractions_mean %>%
  filter(year_regime == "Normal") %>%
  group_by(retailer) %>%
  mutate(final_fraction = mean_fraction / sum(mean_fraction)) %>%
  ungroup()

for (r in retailers_list) {
  
  p <- ggplot() +
    geom_ribbon(data = actual_summary %>% filter(retailer == r),
                aes(x = month, ymin = min_frac, ymax = max_frac),
                fill = "grey80", alpha = 0.3) +
    geom_line(data = actual_summary %>% filter(retailer == r),
              aes(x = month, y = median_frac),
              color = "grey40", linewidth = 1) +
    geom_line(data = new_clean %>% filter(retailer == r),
              aes(x = month, y = fraction, group = year, color = factor(year)),
              linewidth = 0.6, alpha = 0.8) +
    geom_line(data = normal_scenario_mean %>% filter(retailer == r),
              aes(x = month, y = final_fraction),
              color = "black", linewidth = 1.5, linetype = "dashed") +
    geom_line(data = drought_scenario_mean %>% filter(retailer == r),
              aes(x = month, y = final_fraction),
              color = "darkred", linewidth = 1.5, linetype = "dashed") +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_color_manual(values = year_colors) +
    labs(
      title    = paste("Synthetic Monthly Demand Pattern (Mean-based) —", r),
      subtitle = "Grey band = min/max | Grey line = median | Black dashed = WEAP Normal (mean) | Red dashed = WEAP Drought (mean)",
      x        = NULL,
      y        = "Fraction of Annual Demand",
      color    = "Year"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      legend.position  = "right"
    )
  
  ggsave(
    paste0("results/alt03/plot_mean_",
           gsub(" ", "_", r), ".png"),
    p, width = 12, height = 7, dpi = 150
  )
}

message("✅ Mean-based plots saved!")

comparison <- monthly_fractions %>%
  left_join(monthly_fractions_mean, by = c("retailer", "month", "year_regime")) %>%
  mutate(difference = abs(median_fraction - mean_fraction))

comparison %>%
  arrange(desc(difference)) %>%
  head(20)

comparison %>%
  summarise(
    avg_diff = mean(difference, na.rm = TRUE),
    max_diff = max(difference, na.rm = TRUE)
  )

outliers <- new_clean %>%
  filter(fraction > 0.2 | fraction < 0.02) %>%
  arrange(retailer, year, month) %>%
  select(retailer, year, month, demand, annual_total, fraction)

print(outliers, n = 100)
if (interactive()) utils::View(outliers)

for (r in retailers_list) {
  
  p <- ggplot() +
    
    geom_line(data = normal_scenario %>% filter(retailer == r),
              aes(x = month, y = final_fraction, color = "Normal - Median"),
              linewidth = 1.5) +
    geom_line(data = normal_scenario_mean %>% filter(retailer == r),
              aes(x = month, y = final_fraction, color = "Normal - Mean"),
              linewidth = 1.5, linetype = "dashed") +
    
    geom_line(data = drought_scenario %>% filter(retailer == r),
              aes(x = month, y = final_fraction, color = "Drought - Median"),
              linewidth = 1.5) +
    geom_line(data = drought_scenario_mean %>% filter(retailer == r),
              aes(x = month, y = final_fraction, color = "Drought - Mean"),
              linewidth = 1.5, linetype = "dashed") +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_color_manual(values = c(
      "Normal - Median"  = "black",
      "Normal - Mean"    = "grey50",
      "Drought - Median" = "darkred",
      "Drought - Mean"   = "salmon"
    )) +
    labs(
      title    = paste("Mean vs Median Scenarios —", r),
      subtitle = "Solid = Median | Dashed = Mean",
      x        = NULL,
      y        = "Fraction of Annual Demand",
      color    = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
  
  ggsave(
    paste0("results/alt03/plot_compare_",
           gsub(" ", "_", r), ".png"),
    p, width = 12, height = 7, dpi = 150
  )
}

message("✅ Comparison plots saved!")

old_drought <- read_excel(
  "results/alt01/scenario_fractions.xlsx",
  sheet = "Drought_Scenario"
) %>%
  pivot_longer(-Retailer, names_to = "month_name", values_to = "old_fraction") %>%
  mutate(month = match(month_name, month.abb)) %>%
  rename(retailer = Retailer) %>%
  select(retailer, month, old_fraction)

old_normal <- read_excel(
  "results/alt01/scenario_fractions.xlsx",
  sheet = "Normal_Scenario"
) %>%
  pivot_longer(-Retailer, names_to = "month_name", values_to = "old_fraction") %>%
  mutate(month = match(month_name, month.abb)) %>%
  rename(retailer = Retailer) %>%
  select(retailer, month, old_fraction)

for (r in retailers_list) {
  
  p <- ggplot() +
    
    geom_line(data = normal_scenario %>% filter(retailer == r),
              aes(x = month, y = final_fraction, color = "Normal - Billing example"),
              linewidth = 1.5) +
    geom_line(data = drought_scenario %>% filter(retailer == r),
              aes(x = month, y = final_fraction, color = "Drought - Billing example"),
              linewidth = 1.5) +
    
    geom_line(data = old_normal %>% filter(retailer == r),
              aes(x = month, y = old_fraction, color = "Normal - Reported example"),
              linewidth = 1.5, linetype = "dashed") +
    geom_line(data = old_drought %>% filter(retailer == r),
              aes(x = month, y = old_fraction, color = "Drought - Reported example"),
              linewidth = 1.5, linetype = "dashed") +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_color_manual(values = c(
      "Normal - Billing example"  = "black",
      "Normal - Reported example"  = "grey50",
      "Drought - Billing example" = "darkred",
      "Drought - Reported example" = "salmon"
    )) +
    labs(
      title    = paste("Synthetic Billing vs Reported Median Scenarios —", r),
      subtitle = "Solid = Synthetic billing | Dashed = Synthetic reported",
      x        = NULL,
      y        = "Fraction of Annual Demand",
      color    = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
  
  ggsave(
    paste0("results/alt03/comparison/plot_",
           gsub(" ", "_", r), ".png"),
    p, width = 12, height = 7, dpi = 150
  )
}

message("✅ Comparison plots saved!")
if (interactive()) utils::View(outliers)
