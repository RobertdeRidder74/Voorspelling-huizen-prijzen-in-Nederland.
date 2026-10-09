################################################################################
# EDA
################################################################################

# load data
base_data_set_imputed <- readRDS(
  "data/processed/base_data_set_imputed.rds"
)

# first evaluation

overzicht <- data.frame(
  Kenmerk = c(
    "Observaties (N)",
    "Variabelen",
    "Periode",
    "Gemeenten"
  ),
  Waarde = c(
    nrow(base_data_set_imputed),
    ncol(base_data_set_imputed),
    "2009-2024",
    length(unique(base_data_set_imputed$province))
  )
)

overzicht |>
  kbl(
    caption = "Tabel 1. Overzicht van de dataset",
    align = c("l", "r")
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE
  )
################################################################################

cat(
  "Na imputatie bevat de dataset geen ontbrekende waarden meer (",
  sum(is.na(base_data_set_imputed)),
  "ontbrekende waarden)."
)
################################################################################

desc <- describe(base_data_set_imputed)

desc %>%
  select(mean, sd, median, min, max) %>%
  round(2) %>%
  tibble::rownames_to_column("Variable") %>%
  kbl(caption = "Tabel 2. Descriptive statistics") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE
  )

################################################################################
# check for outliers

numeric_data <- base_data_set_imputed %>%
  select(where(is.numeric)) %>%
  pivot_longer(
    cols = -year,
    names_to = "Variable",
    values_to = "Value"
  )

ggplot(numeric_data, aes(y = Value)) +
  geom_boxplot(
    fill = "#2C7FB8",
    colour = "black",
    alpha = 0.8
  ) +
  facet_wrap(
    ~ Variable,
    scales = "free",
    ncol = 3
  ) +
  labs(
    title = "Figuur1. Detectie van potentiële outliers",
    subtitle = "Boxplots voor alle numerieke variabelen",
    y = "Waarde"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold", size = 10),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

cat(
  "Figuur 1 toont boxplots voor alle numerieke variabelen. ",
  "Verschillende variabelen bevatten potentiële outliers volgens de IQR-methode. ",
  "Deze observaties zijn niet verwijderd omdat zij plausibele economische waarden vertegenwoordigen."
)

################################################################################
# verdeling van de data
################################################################################

ggplot(numeric_data, aes(x = Value)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 30,
    fill = "#2C7FB8",
    colour = "black",
    alpha = 0.7
  ) +
  geom_density(
    colour = "darkred",
    linewidth = 0.8
  ) +
  facet_wrap(
    ~ Variable,
    scales = "free",
    ncol = 3
  ) +
  labs(
    title = "Figuur 2. Distributie van de numerieke variabelen",
    x = "Waarde",
    y = "Dichtheid"
  ) +
  theme_bw()

################################################################################
# qq plot

ggplot(numeric_data, aes(sample = Value)) +
  stat_qq() +
  stat_qq_line(colour = "red") +
  facet_wrap(
    ~ Variable,
    scales = "free",
    ncol = 3
  ) +
  theme_bw() +
  labs(
    title = "Figuur 3. Q-Q plots van de numerieke variabelen"
  )

################################################################################
# correlatie
################################################################################
corr_data <- base_data_set_imputed |>
  select(
    house_price,
    mortgage_rate,
    avg_income_working_province,
    natural_pop_growth,
    migration_growth,
    total_growth,
    total_pop_in_mln
  )

corr_matrix <- cor(corr_data)

corrplot(
  corr_matrix,
  method = "color",
  type = "lower",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.7,
  title = "fig 4.Correlatiematrix van numerieke variabelen",
  mar = c(0, 0, 2, 0)
)

################################################################################
# Variance Inflation Factor
################################################################################
model <- lm(
  house_price ~ mortgage_rate +
    avg_income_working_total +
    avg_income_males_working +
    avg_income_females_working +
    avg_income_working_province +
    total_growth +
    migration_growth,
  data = base_data_set_imputed
)

vif(model)

vif_table <- data.frame(
  Variable = names(vif(model)),
  VIF = round(vif(model), 2)
)

vif_table |>
  kbl(
    caption = "Tabel 3. Variance Inflation Factors (VIF)",
    align = c("l", "r")
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE
  )

# extreme multicollineariteit


