# Public example: invented data and generic provider identifiers only.
# Run from the repository root after generate_example_data.R.
if (!file.exists("README.md") || !dir.exists("data")) stop("Run from the project folder after generating example data.")
library(lubridate)
dir.create("results/total_use", recursive = TRUE, showWarnings = FALSE)

library(tidyverse)
library(readxl)
library(scales)

actual_data <- read_excel("data/synthetic_reported_use.xlsx")

actual_data <- actual_data %>%
  mutate(
    Month_date = as.Date(Month, origin = "1899-12-30"),
    Month_num  = month(Month_date),
    Year       = year(Month_date)
  ) %>%
  filter(Year >= 2013, Year <= 2025)

total_summary <- actual_data %>%
  group_by(Retailer, Month_num) %>%
  summarise(
    median_total = median(`Total Monthly Use`, na.rm = TRUE),
    min_total    = min(`Total Monthly Use`, na.rm = TRUE),
    max_total    = max(`Total Monthly Use`, na.rm = TRUE),
    .groups      = "drop"
  )

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

retailers <- unique(actual_data$Retailer)

for (r in retailers) {
  
  p <- ggplot() +
    geom_ribbon(data = total_summary %>% filter(Retailer == r),
                aes(x = Month_num, ymin = min_total, ymax = max_total),
                fill = "steelblue", alpha = 0.15) +
    geom_line(data = total_summary %>% filter(Retailer == r),
              aes(x = Month_num, y = median_total),
              color = "steelblue", linewidth = 1) +
    geom_line(data = actual_data %>% filter(Retailer == r),
              aes(x = Month_num, y = `Total Monthly Use`, group = Year, color = factor(Year)),
              linewidth = 0.8, alpha = 0.9) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_y_continuous(labels = comma) +
    scale_color_manual(values = year_colors) +
    labs(
      title    = paste("Synthetic Monthly Total Demand —", r),
      subtitle = "Blue band = min/max range | Blue line = median | Colored lines = individual years",
      x        = NULL,
      y        = "Total Monthly Use",
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
    paste0("results/total_use/plot_total_",
           gsub(" ", "_", r), ".png"),
    p, width = 12, height = 7, dpi = 150
  )
}

message("✅ Total volume plots saved!")

library(tidyverse)
library(readxl)
library(scales)

actual_data <- read_excel("data/synthetic_reported_use.xlsx")

actual_data <- actual_data %>%
  mutate(
    Month_date = as.Date(Month, origin = "1899-12-30"),
    Month_num  = month(Month_date),
    Year       = year(Month_date)
  ) %>%
  filter(Year >= 2013, Year <= 2025)

total_summary <- actual_data %>%
  group_by(Retailer, Month_num) %>%
  summarise(
    median_total = median(`Total Monthly Use`, na.rm = TRUE),
    min_total    = min(`Total Monthly Use`, na.rm = TRUE),
    max_total    = max(`Total Monthly Use`, na.rm = TRUE),
    .groups      = "drop"
  )

year_colors <- c(
  "2013" = "#FFAAAA",  
  "2014" = "#FF4444",  
  "2015" = "#CC0000",  
  "2016" = "#AED6F1",  
  "2017" = "#5DADE2",  
  "2018" = "#2E86C1",  
  "2019" = "#1A5276",  
  "2020" = "#85C1E9",  
  "2021" = "#FF6666",  
  "2022" = "#FF8888",  
  "2023" = "#00CED1",  
  "2024" = "#AED6F1",  
  "2025" = "#5DADE2"   
)

library(ggrepel)

retailers <- unique(actual_data$Retailer)

for (r in retailers) {
  
  year_labels <- actual_data %>%
    filter(Retailer == r, Month_num == 3)
  
  p <- ggplot() +
    geom_ribbon(data = total_summary %>% filter(Retailer == r),
                aes(x = Month_num, ymin = min_total, ymax = max_total),
                fill = "grey80", alpha = 0.3) +
    geom_line(data = total_summary %>% filter(Retailer == r),
              aes(x = Month_num, y = median_total),
              color = "grey40", linewidth = 1) +
    geom_line(data = actual_data %>% filter(Retailer == r),
              aes(x = Month_num, y = `Total Monthly Use`, group = Year, color = factor(Year)),
              linewidth = 0.8, alpha = 0.9) +
    geom_text_repel(data = year_labels,
                    aes(x = Month_num, y = `Total Monthly Use`,
                        label = Year, color = factor(Year)),
                    size = 3, show.legend = FALSE) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_y_continuous(labels = comma) +
    scale_color_manual(values = year_colors) +
    labs(
      title    = paste("Synthetic Monthly Total Demand —", r),
      subtitle = "Grey band = min/max range | Grey line = median | Year colors are illustrative; see generated climate data",
      x        = NULL,
      y        = "Total Monthly Use",
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
    paste0("results/total_use/plot_total_2_",
           gsub(" ", "_", r), ".png"),
    p, width = 12, height = 7, dpi = 150
  )
}

message("✅ Total volume plots saved!")

