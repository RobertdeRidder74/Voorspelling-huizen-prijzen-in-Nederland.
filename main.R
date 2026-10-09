################################################################################
# MAIN
################################################################################

rm(list = ls())

cat("Start analyse\n")

source("scripts/01_import.R")

source("scripts/02_cleaning.R")

source("scripts/03_eda.R")

source("scripts/04_analyse.R")

source("scripts/05_linear_model.R")

cat("Analyse gereed\n")