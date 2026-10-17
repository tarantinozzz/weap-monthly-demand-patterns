# All records below are invented, not derived from operational data.
# Generates local Excel/CSV fixtures needed by the five example analyses.
library(writexl)
dir.create("data", showWarnings = FALSE)
example_regime <- function(year) {
  c("Normal", "Drought", "Normal", "Wet")[(year - 1995) %% 4 + 1]
}
example_volume <- function(provider, year, month, source = "reported") {
  # Arbitrary illustrative units; no real demand values or calibrated profiles.
  base <- c(Example_A = 120, Example_B = 170, Example_C = 210)[provider]
  amplitude <- if (example_regime(year) == "Drought") 0.18 else 0.30
  seasonal <- 1 + amplitude * cos(2 * pi * (month - 7) / 12)
  source_factor <- if (source == "billing") 1 + 0.025 * sin(month + year) else 1
  round(base * seasonal * (1 + (year %% 5) * 0.015) * source_factor, 3)
}
providers <- c("Example_A", "Example_B", "Example_C")
reported <- expand.grid(Retailer = providers, Year = 2013:2025,
                        Month_num = 1:12, stringsAsFactors = FALSE)
dates <- as.Date(sprintf("%04d-%02d-01", reported$Year, reported$Month_num))
reported$Month <- as.numeric(dates - as.Date("1899-12-30"))
reported$`Total Monthly Use` <- mapply(example_volume, reported$Retailer,
                                      reported$Year, reported$Month_num)
write_xlsx(reported[c("Retailer", "Month", "Total Monthly Use")],
           "data/synthetic_reported_use.xlsx")
write_xlsx(data.frame(Retailer = providers), "data/synthetic_reference.xlsx")
climate <- data.frame(month = month.abb)
precip <- data.frame(month = month.abb)
for (year in 1995:2025) {
  regime <- example_regime(year)
  pdsi <- switch(regime, Drought = -4, Wet = 3, Normal = 0)
  climate[[as.character(year)]] <- rep(pdsi, 12)
  precip[[as.character(year)]] <- round(2 + cos(2 * pi * (1:12 - 1) / 12), 2)
}
write_xlsx(list(`Palmer Drought Severity Index` = climate,
                `Precipitation (in)` = precip), "data/synthetic_climate.xlsx")
write.csv(data.frame(Year = 2013:2025, ExampleIndex = 0),
          "data/synthetic_climate_index.csv", row.names = FALSE)
billing <- expand.grid(Retailer = providers, year = 1995:2024,
                       month = 1:12, stringsAsFactors = FALSE)
billing$`Cal. Year` <- billing$year
billing$`Data Month` <- billing$month
billing$`Total Water Use` <- mapply(function(p, y, m) example_volume(p, y, m, "billing"),
                                  billing$Retailer, billing$year, billing$month)
# Synthetic alias and missing month exercise the original preparation logic.
billing$Retailer[billing$Retailer == "Example_B" & billing$year < 2005] <- "Example_B_Alias"
billing <- billing[!(billing$Retailer == "Example_A" & billing$year == 2000 & billing$month == 7), ]
write.csv(billing[c("Retailer", "Cal. Year", "Data Month", "Total Water Use")],
          "data/synthetic_billing_use.csv", row.names = FALSE)
message("Synthetic examples created. No operational data were used.")
