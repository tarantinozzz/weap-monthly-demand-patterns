# Climate-Sensitive Monthly Water Demand Patterns for WEAP

This project develops monthly demand distributions for normal and drought conditions from historical water-use records. It combines demand normalization, climate-regime classification and alternative-method comparisons to prepare seasonal demand inputs for WEAP.

## The modeling problem

A fixed monthly demand pattern with an equal percentage reduction in every drought month cannot represent changes in the seasonal shape of consumption. This analysis examines an alternative: estimate separate normal and drought patterns from observed monthly shares of annual use, rather than assume that drought simply scales every month equally.

The analysis concerns **when annual demand occurs**, not a forecast of total annual demand. A twelve-month pattern can be multiplied by a separately specified annual demand to obtain monthly demands.

## Approach

For each provider and year, monthly use is divided by that year's total. Monthly PDSI values classify conditions as Drought (PDSI <= -3), Wet (PDSI >= 2), or Normal (between those thresholds). In the selected year-level approach, seven or more months in a category assign that regime to the whole year; otherwise it is Mixed. Only Normal and Drought years enter the corresponding scenario estimates.

For each provider and calendar month, the median fraction across years in a regime is calculated. The twelve median values are then normalized to sum to one for each provider and regime. Plots compare the resulting scenarios against individual-year patterns and historical ranges.

The project evaluates month-level versus year-level classification, sensitivity to explicit year exclusions, mean versus median estimates, and differences between two distinct sources of water-use data.

## Two distinct data sources

The original Alt00, Alt01 and Alt02 analyses use provider-reported water-use records. Alt03 uses a separate **billing dataset**, with its own preparation steps and a longer analysis period. Alt03 is not simply an extension of the same input series. Its comparison with Alt01 evaluates both a different source and a different record length; differences cannot be attributed to record length alone.

## Scripts

| File | Purpose |
| --- | --- |
| total_use_plots.R | Explore monthly use volumes and year-to-year seasonal variation. |
| alt00_month_level.R | Classify each month independently and estimate regime-specific fractions. |
| alt01_year_level.R | Selected approach in the original methodology: classify whole years and use normalized monthly medians. |
| alt02_filtered_years.R | Sensitivity analysis using the same method with explicit exclusions for two boundary years. |
| alt03_billing_comparison.R | Clean the separate billing source, reconcile an example alias, remove incomplete provider-years, compare mean/median estimates and compare with Alt01. |
| generate_example_data.R | Create every required input locally from invented formulas. |

Keeping complete years together gives a consistent annual-regime interpretation to their twelve fractions. Month-level classification is a different sampling approach, potentially combining months from different annual conditions; the project compares these choices rather than treating them as interchangeable.

## Public example and confidentiality

The original project uses organizational water-use records. **No real input data, billing records, consumption values, empirical scenario fractions, figures, memoranda or internal findings are included here.** This public repository shares the analysis methods using generic provider IDs and invented fixtures. The original organization, geography, provider names, aliases, personal paths and identifying comments have been removed from the code.

The example generator does not read any original dataset. Its volumes, climate values, provider count, alias and missing-data example are invented. Dates and analytical thresholds support the code demonstration; they do not disclose observed use. The generated plots and scenario outputs describe synthetic examples and must not be interpreted as findings about any organization or area. The provider-reported example and billing example have intentionally different series.

## Run the public example

Install R 4.1 or newer and optionally RStudio. In R, install:

```r
install.packages(c("tidyverse", "readxl", "writexl", "lubridate", "scales", "ggrepel"))
```

Set the working directory to this repository folder, then run:

```r
source("generate_example_data.R")
source("alt01_year_level.R")
source("total_use_plots.R")
source("alt00_month_level.R")
source("alt02_filtered_years.R")
source("alt03_billing_comparison.R")
```

Alt01 must run before Alt03 because the latter reads its scenario workbook for comparison. The other analyses are independent once example inputs exist. No WEAP installation is needed to calculate or plot these fractions; using them in a model is a separate task.

Generated Excel and CSV inputs are under data/. Outputs are under results/, separated by analysis. Alt00–Alt02 export scenario workbooks and classification tables; Alt03 creates billing-source and comparison plots and holds its scenario objects in the R session, as in the supplied workflow. Total-use plots use arbitrary example units.

## Preparation details

The monthly-fraction, classification, median/mean and renormalization calculations are retained. File paths and provider-specific identifiers were generalized. Explicit lubridate loading and output-directory creation were added, interactive table viewers were guarded, and Alt03's comparison-sheet references were aligned with Alt01's Normal_Scenario and Drought_Scenario export names. Source comments were removed to avoid identifying details. The total-use plotting script retains both supplied plotting styles. Calendar-year exclusion settings and original exploratory plot styling remain visible as methodology choices; the artificial climate series does not reproduce historical classifications.

Only code and documentation are tracked by the included .gitignore; local inputs and outputs are excluded. No software reuse license has been selected.
