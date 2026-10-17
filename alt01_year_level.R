# Public example: invented data and generic provider identifiers only.
# Run from the repository root after generate_example_data.R.
if (!file.exists("README.md") || !dir.exists("data")) stop("Run from the project folder after generating example data.")
library(lubridate)
dir.create("results/alt01", recursive = TRUE, showWarnings = FALSE)

library(tidyverse)
library(readxl)

actual_data   <- read_excel("data/synthetic_reported_use.xlsx")
weap_forecast <- read_excel("data/synthetic_reference.xlsx")
drought_data  <- read.csv("data/synthetic_climate_index.csv")

precip_raw <- read_excel(
  "data/synthetic_climate.xlsx",
  sheet = "Precipitation (in)"
)

pdsi_raw <- read_excel(
  "data/synthetic_climate.xlsx",
  sheet = "Palmer Drought Severity Index"
)

actual_data <- actual_data %>%
  mutate(
    Month_date = as.Date(Month, origin = "1899-12-30"),
    Month_num  = month(Month_date),
    Year       = year(Month_date)
  ) %>%
  filter(Year >= 2011, Year <= 2025)

weap_forecast <- weap_forecast %>% filter(!is.na(Retailer))

actual_fractions <- actual_data %>%
  filter(Year >= 2013) %>%
  group_by(Retailer, Year) %>%
  mutate(
    annual_total = sum(`Total Monthly Use`, na.rm = TRUE),
    fraction     = `Total Monthly Use` / annual_total
  ) %>%
  ungroup()

pdsi_long <- pdsi_raw %>%
  pivot_longer(-month, names_to = "Year", values_to = "pdsi") %>%
  mutate(
    Year      = as.integer(Year),
    Month_num = match(month, month.abb),
    regime    = case_when(
      pdsi <= -3 ~ "Drought",
      pdsi >= 2  ~ "Wet",
      TRUE       ~ "Normal"
    )
  ) %>%
  select(Year, Month_num, pdsi, regime) %>%
  filter(Year >= 2013, Year <= 2025)

year_class <- pdsi_long %>%
  group_by(Year) %>%
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

cat("Drought years:", year_class %>% filter(year_regime == "Drought") %>% pull(Year), "\n")
cat("Normal years:",  year_class %>% filter(year_regime == "Normal")  %>% pull(Year), "\n")
cat("Wet years:",     year_class %>% filter(year_regime == "Wet")     %>% pull(Year), "\n")
cat("Mixed years:",   year_class %>% filter(year_regime == "Mixed")   %>% pull(Year), "\n")

actual_fractions_classified <- actual_fractions %>%
  left_join(year_class %>% select(Year, year_regime), by = "Year")

monthly_fractions <- actual_fractions_classified %>%
  filter(year_regime %in% c("Drought", "Normal")) %>%
  group_by(Retailer, Month_num, year_regime) %>%
  summarise(
    median_fraction = median(fraction, na.rm = TRUE),
    .groups         = "drop"
  )

drought_scenario <- monthly_fractions %>%
  filter(year_regime == "Drought") %>%
  group_by(Retailer) %>%
  mutate(final_fraction = median_fraction / sum(median_fraction)) %>%
  ungroup()

normal_scenario <- monthly_fractions %>%
  filter(year_regime == "Normal") %>%
  group_by(Retailer) %>%
  mutate(final_fraction = median_fraction / sum(median_fraction)) %>%
  ungroup()

actual_summary <- actual_fractions %>%
  group_by(Retailer, Month_num) %>%
  summarise(
    median_frac = median(fraction, na.rm = TRUE),
    min_frac    = min(fraction, na.rm = TRUE),
    max_frac    = max(fraction, na.rm = TRUE),
    .groups     = "drop"
  )

all_years_plot <- actual_fractions %>%
  left_join(year_class %>% select(Year, year_regime), by = "Year")

year_colors <- c(
  "2013" = "#E69F00",  
  "2014" = "#D55E00",  
  "2015" = "#CC0000",  
  "2016" = "#56B4E9",  
  "2017" = "#009E73",  
  "2018" = "#0072B2",  
  "2019" = "#3CB371",  
  "2020" = "#A0C4FF",  
  "2021" = "#9400D3",  
  "2022" = "#FF69B4",  
  "2023" = "#00CED1",  
  "2024" = "#66CDAA",  
  "2025" = "#708090"   
)

retailers <- unique(actual_fractions$Retailer)
for (r in retailers) {
  
  y_min <- ifelse(r == "Example_A", 0.02, 0.025)
  
  p <- ggplot() +
    
    geom_ribbon(data = actual_summary %>% filter(Retailer == r),
                aes(x = Month_num, ymin = min_frac, ymax = max_frac),
                fill = "steelblue", alpha = 0.15) +
    
    geom_line(data = all_years_plot %>% filter(Retailer == r),
              aes(x = Month_num, y = fraction, group = Year, color = factor(Year)),
              linewidth = 0.8, alpha = 0.9) +
    
    geom_line(data = normal_scenario %>% filter(Retailer == r),
              aes(x = Month_num, y = final_fraction),
              color = "black", linewidth = 1.5, linetype = "dashed") +
    
    geom_line(data = drought_scenario %>% filter(Retailer == r),
              aes(x = Month_num, y = final_fraction),
              color = "darkred", linewidth = 1.5, linetype = "dashed") +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_y_continuous(limits = c(y_min, 0.15), breaks = seq(0.04, 0.14, by = 0.02)) +
    scale_color_manual(values = year_colors) +
    labs(
      title    = paste("Synthetic Monthly Demand Pattern (Alt01) —", r),
      subtitle = "Blue band = min/max | Colored lines = individual years | Black dashed = WEAP Normal | Red dashed = WEAP Drought",
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
    paste0("results/alt01/plot_",
           gsub(" ", "_", r), ".png"),
    p, width = 12, height = 7, dpi = 150
  )
}

message("✅ Individual retailer plots saved!")

format_scenario <- function(df) {
  df %>%
    select(Retailer, Month_num, final_fraction) %>%
    pivot_wider(names_from = Month_num, values_from = final_fraction) %>%
    rename_with(~ month.abb[as.integer(.)], -Retailer)
}

writexl::write_xlsx(
  list(
    Normal_Scenario  = format_scenario(normal_scenario),
    Drought_Scenario = format_scenario(drought_scenario)
  ),
  "results/alt01/scenario_fractions.xlsx"
)

message("✅ Scenario fractions saved!")

regime_table <- actual_fractions %>%
  left_join(year_class %>% select(Year, year_regime), by = "Year") %>%
  select(Retailer, Year, Month_num, fraction, year_regime) %>%
  arrange(Retailer, Year, Month_num)

write.csv(regime_table,
          "results/alt01/regime_table_Alt01.csv",
          row.names = FALSE)
