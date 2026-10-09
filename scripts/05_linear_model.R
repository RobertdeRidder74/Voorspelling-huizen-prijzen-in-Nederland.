################################################################################
# DATA
################################################################################

housing_data <- readRDS(
  "data/processed/base_data_set_imputed.rds"
)

################################################################################
# MODEL DATA
################################################################################

model_data <- housing_data |>
  select(
    house_price,
    mortgage_rate,
    avg_income_working_province,
    total_growth
  )

################################################################################
# TRAIN TEST SPLIT
################################################################################

train <- housing_data |>
  filter(year <= 2021) |>
  select(
    house_price,
    mortgage_rate,
    avg_income_working_province,
    total_growth
  )

test <- housing_data |>
  filter(year > 2021) |>
  select(
    house_price,
    mortgage_rate,
    avg_income_working_province,
    total_growth
  )

################################################################################
# LINEAIR MODEL
################################################################################

lm_model <- lm(
  house_price ~
    mortgage_rate +
    avg_income_working_province +
    total_growth,
  data = train
)

summary(lm_model)

################################################################################
# MULTICOLLINEARITEIT
################################################################################

car::vif(lm_model)

################################################################################
# TESTSET VOORSPELLING
################################################################################

pred <- predict(
  lm_model,
  newdata = test
)

################################################################################
# MODEL EVALUATIE
################################################################################

rmse <- sqrt(
  mean(
    (test$house_price - pred)^2
  )
)

mae <- mean(
  abs(
    test$house_price - pred
  )
)

r2 <- cor(
  test$house_price,
  pred
)^2

cat(
  "RMSE =", round(rmse,0),
  "\nMAE =", round(mae,0),
  "\nR2 =", round(r2,3)
)

################################################################################
# JAARGEMIDDELDEN
################################################################################

yearly_data <- housing_data |>
  group_by(year) |>
  summarise(
    mortgage_rate =
      mean(
        mortgage_rate,
        na.rm = TRUE
      ),
    
    avg_income_working_province =
      mean(
        avg_income_working_province,
        na.rm = TRUE
      ),
    
    total_growth =
      mean(
        total_growth,
        na.rm = TRUE
      ),
    
    avg_house_price =
      mean(
        house_price,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

################################################################################
# TOEKOMSTIGE JAREN
################################################################################

future_years <- tibble(
  year = 2025:2030
)

################################################################################
# INKOMENSPROGNOSE
################################################################################

income_model <- lm(
  avg_income_working_province ~ year,
  data = yearly_data
)

future_income <- predict(
  income_model,
  newdata = future_years
)

income_interval <- predict(
  income_model,
  newdata = future_years,
  interval = "confidence"
)
################################################################################
# MIGRATIEPROGNOSE
################################################################################

migration_model <- lm(
  total_growth ~ year,
  data = yearly_data
)

future_migration <- predict(
  migration_model,
  newdata = future_years
)

################################################################################
# HYPOTHEEKRENTE (ARIMA)
################################################################################

rate_ts <- ts(
  yearly_data$mortgage_rate,
  start = min(yearly_data$year)
)

mortgage_model <- auto.arima(
  rate_ts
)

mortgage_forecast <- forecast(
  mortgage_model,
  h = 6
)

################################################################################
# FUTURE DATA
################################################################################

future_data <- tibble(
  year = 2025:2030,
  
  mortgage_rate =
    as.numeric(
      mortgage_forecast$mean
    ),
  
  avg_income_working_province =
    future_income,
  
  total_growth =
    future_migration
)

future_data <- future_data |>
  mutate(
    income_lower = income_interval[,2],
    income_upper = income_interval[,3]
  )

################################################################################
# HUISPRIJS FORECAST
################################################################################

prediction_interval <- predict(
  lm_model,
  newdata = future_data,
  interval = "prediction"
)

future_data <- future_data |>
  mutate(
    predicted_house_price =
      prediction_interval[,1],
    
    lower_bound =
      prediction_interval[,2],
    
    upper_bound =
      prediction_interval[,3]
  )

################################################################################
# BETAALBAARHEID
################################################################################

future_data <- future_data |>
  mutate(
    affordability_ratio =
      predicted_house_price /
      avg_income_working_province
  )

################################################################################
# RESULTATEN
################################################################################

future_data |>
  select(
    year,
    predicted_house_price,
    avg_income_working_province,
    affordability_ratio,
    lower_bound,
    upper_bound
  )

summary(lm_model)

rmse
mae
r2

################################################################################
ggplot(
  future_data,
  aes(
    x = year,
    y = predicted_house_price
  )
) +
  geom_ribbon(
    aes(
      ymin = lower_bound,
      ymax = upper_bound
    ),
    fill = "steelblue",
    alpha = 0.20
  ) +
  geom_line(
    colour = "steelblue",
    linewidth = 1.4
  ) +
  geom_point(
    colour = "steelblue",
    size = 3
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = "."
    )
  ) +
  theme_bw() +
  labs(
    title = "Figuur 11. Verwachte ontwikkeling van huizenprijzen",
    subtitle = "Voorspelling op basis van lineaire regressie",
    x = "Jaar",
    y = "Gemiddelde huizenprijs (€)"
  )

################################################################################
ggplot(
  future_data,
  aes(
    x = year,
    y = avg_income_working_province
  )
) +
  geom_ribbon(
    aes(
      ymin = income_lower,
      ymax = income_upper
    ),
    fill = "darkgreen",
    alpha = 0.2
  ) +
  geom_line(
    colour = "darkgreen",
    linewidth = 1.2
  ) +
  geom_point(
    colour = "darkgreen",
    size = 3
  ) +
  theme_bw() +
  labs(
    title = "Figuur 12. Verwachte ontwikkeling van inkomen",
    subtitle = "95% betrouwbaarheidsinterval",
    x = "Jaar",
    y = "Gemiddeld inkomen (€)"
  )


################################################################################

ggplot(
  future_data,
  aes(
    year,
    affordability_ratio
  )
) +
  geom_line(
    colour = "firebrick",
    linewidth = 1.4
  ) +
  geom_point(
    colour = "firebrick",
    size = 3
  ) +
  geom_text(
    aes(
      label = round(
        affordability_ratio,
        1
      )
    ),
    vjust = -0.5
  ) +
  theme_bw() +
  labs(
    title = "Figuur 15. Ontwikkeling van de betaalbaarheid",
    subtitle = "Huizenprijs gedeeld door inkomen",
    x = "Jaar",
    y = "Prijs-inkomenratio"
  )

################################################################################

historical <- housing_data |>
  group_by(year) |>
  summarise(
    house_price =
      mean(house_price)
  ) |>
  mutate(
    type = "Historisch"
  )

forecast <- future_data |>
  transmute(
    year,
    house_price =
      predicted_house_price,
    type = "Voorspelling"
  )

plot_data <- bind_rows(
  historical,
  forecast
)

ggplot(
  plot_data,
  aes(
    year,
    house_price,
    colour = type
  )
) +
  geom_line(
    linewidth = 1.4
  ) +
  geom_point(
    size = 2
  ) +
  theme_bw() +
  labs(
    title = "Figuur 13. Historische en voorspelde huizenprijzen",
    x = "Jaar",
    y = "Gemiddelde huizenprijs (€)"
  )

################################################################################

plot_data <- future_data |>
  transmute(
    year,
    
    house_price_index =
      predicted_house_price /
      first(predicted_house_price) * 100,
    
    income_index =
      avg_income_working_province /
      first(avg_income_working_province) * 100
  ) |>
  pivot_longer(
    -year,
    names_to = "variable",
    values_to = "value"
  )

ggplot(
  plot_data,
  aes(
    x = year,
    y = value,
    colour = variable
  )
) +
  geom_line(
    linewidth = 1.3
  ) +
  geom_point(
    size = 3
  ) +
  theme_bw() +
  labs(
    title = "Figuur 14. Ontwikkeling huizenprijzen en inkomens",
    subtitle = "Index (2025 = 100)",
    x = "Jaar",
    y = "Index",
    colour = ""
  ) +
  scale_colour_manual(
    values = c(
      "house_price_index" = "steelblue",
      "income_index" = "darkgreen"
    ),
    labels = c(
      "Huizenprijs",
      "Inkomen"
    )
  )

################################################################################

model_stats <- data.frame(
  Metriek = c(
    "Observaties",
    "Residual Standard Error",
    "R-squared",
    "Adjusted R-squared",
    "F-statistic"
  ),
  Waarde = c(
    nobs(lm_model),
    round(summary(lm_model)$sigma, 0),
    round(summary(lm_model)$r.squared, 3),
    round(summary(lm_model)$adj.r.squared, 3),
    round(summary(lm_model)$fstatistic[1], 1)
  )
)

model_stats |>
  kbl(
    caption = "Tabel 4. Modelstatistieken",
    align = c("l", "r")
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE
  )
