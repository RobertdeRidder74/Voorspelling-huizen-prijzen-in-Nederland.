################################################################################
# PROJECT SETUP
################################################################################

# Required packages
required_packages <- c(
  "tidyverse",
  "readr",
  "tidyr",
  "ggplot2",
  "knitr",
  "dplyr",
  "stringr",
  "missForestPredict",
  "car",
  "corrplot",
  "psych",
  "kableExtra",
  "forecast"
)

# Install missing packages and load libraries
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

################################################################################
# DATA IMPORT
################################################################################

# Dutch housing prices by province and year in Euro. source: CBS
house_prices_raw <- read_csv("data/raw/huizenprijs per provicie per jaar.csv")

# Average income statistics.  gross weighted. (only working people) source: CBS
income_total_raw <- read_csv("data/raw/inkomen per persoon per jaar.csv")

# Income statistics for men.  gross weighted. (only working people) source: CBS
income_men_raw <- read_csv("data/raw/mannen met inkomen.csv")

# Income statistics for women.  gross weighted. (only working people) source: CBS
income_women_raw <- read_csv("data/raw/vrouwen met inkomen.csv")

# Average income statistics by province. gross un weighted (all people) source: allescijfers.nl
income_un_weighted_province <- read_csv("data/raw/gem bruto jaarinkomen per gemeente.csv")

# Mortgage interest statistics. source: DNB
Mortgage_interest <- read_csv("data/raw/hypotheek rente.csv")

# population growth statistics. source: cbs
population_growth <- read_csv("data/raw/Bevolkingsontwikkeling (x 1 000).csv")

# total population statistics. source: cbs
total_population <- read_csv("data/raw/Bevolking (x mln).csv")

# script werkt
