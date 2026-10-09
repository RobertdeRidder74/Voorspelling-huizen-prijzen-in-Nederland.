################################################################################
# Analyse
################################################################################

# load data
base_data_set_imputed <- readRDS(
  "data/processed/base_data_set_imputed.rds"
)

# create trendline
ggplot(numeric_data, aes(x = year, y = Value)) +
  geom_line(
    colour = "#2C7FB8",
    linewidth = 0.8
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "darkred",
    linewidth = 1.1
  ) +
  facet_wrap(
    ~ Variable,
    scales = "free_y",
    ncol = 3
  ) +
  labs(
    title = "Figuur 5. Ontwikkeling van de variabelen door de tijd",
    subtitle = "Blauw = waargenomen waarden, rood = lineaire trend",
    x = "Jaar",
    y = "Waarde"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

################################################################################
# bereken hoeveel procent van de bevolking werk heeft
################################################################################
prep1 <- base_data_set_imputed |>
  filter(year == 2024) |>
  summarise(
    max_population = max(total_pop_in_mln, na.rm = TRUE),
    max_working_males = max(working_males, na.rm = TRUE),
    max_working_females = max(working_females, na.rm = TRUE)
  ) |>
  mutate(
    max_population = max_population * 10^6,
    total_working = max_working_males + max_working_females)

perct_working <- prep1$total_working / prep1$max_population * 100 # 49.7%
  
################################################################################
# bekijk inkomen per provincie over tijd
################################################################################

  ggplot(
    base_data_set_imputed,
    aes(
      x = year,
      y = avg_income_working_province,
      group = province
    )
  ) +
  geom_line(alpha = 0.3) +
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "line",
    colour = "red",
    linewidth = 1.5
  ) +
  theme_minimal()+
  labs(
    title = "Figuur 6. Salaris ontwikkeling",
    subtitle = "grijs = trend per gemeente, rood = gemiddelde trend",
    x = "Jaar",
    y = "Waarde"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

################################################################################
# woning prijzen over tijd per gemeente
################################################################################

ggplot(
  base_data_set_imputed,
  aes(
    x = year,
    y = house_price,
    group = province
  )
) +
  geom_line(alpha = 0.3) +
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "line",
    colour = "red",
    linewidth = 1.5
  ) +
  theme_minimal()+
  labs(
    title = "Figuur 7. Woning prijs ontwikkeling",
    subtitle = "grijs = trend per gemeente, rood = gemiddelde trend",
    x = "Jaar",
    y = "Waarde"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

################################################################################
# hypothee rente en woning prijzen
################################################################################
plot_data <- base_data_set_imputed |>
  group_by(year) |>
  summarise(
    avg_house_price = mean(house_price, na.rm = TRUE),
    avg_mortgage_rate = mean(mortgage_rate, na.rm = TRUE)
  )

ggplot(plot_data, aes(x = year)) +
  geom_line(
    aes(y = avg_house_price,
        color = "Gemiddelde huizenprijs"),
    linewidth = 1.2
  ) +
  geom_line(
    aes(
      y = avg_mortgage_rate *
        max(avg_house_price) /
        max(avg_mortgage_rate),
      color = "Hypotheekrente"
    ),
    linewidth = 1.2
  ) +
  scale_y_continuous(
    name = "Gemiddelde huizenprijs (€)",
    sec.axis = sec_axis(
      ~ . * max(plot_data$avg_mortgage_rate) /
        max(plot_data$avg_house_price),
      name = "Hypotheekrente (%)"
    )
  ) +
  scale_color_manual(
    values = c(
      "Gemiddelde huizenprijs" = "#2C7FB8",
      "Hypotheekrente" = "#D95F02"
    )
  ) +
  labs(
    title = "Figuur 8. Ontwikkeling van huizenprijzen en hypotheekrente",
    x = "Jaar",
    color = NULL
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

################################################################################
# inkomen versus huisprijzen
################################################################################
plot_data <- base_data_set_imputed |>
  group_by(year) |>
  summarise(
    avg_house_price = mean(house_price, na.rm = TRUE),
    avg_income = mean(avg_income_working_total, na.rm = TRUE)
  ) |>
  mutate(
    house_index = avg_house_price /
      first(avg_house_price) * 100,
    income_index = avg_income /
      first(avg_income) * 100
  )

ggplot(plot_data, aes(x = year)) +
  geom_line(
    aes(y = house_index,
        color = "Huizenprijs"),
    linewidth = 1.2
  ) +
  geom_line(
    aes(y = income_index,
        color = "Inkomen"),
    linewidth = 1.2
  ) +
  labs(
    title = "Figuur 9. Geïndexeerde ontwikkeling van huizenprijzen en inkomen",
    subtitle = "2009 = 100",
    x = "Jaar",
    y = "Index",
    color = NULL
  ) +
  scale_color_manual(
    values = c(
      "Huizenprijs" = "#2C7FB8",
      "Inkomen" = "#31A354"
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

################################################################################
# betaalbaarheid
################################################################################

plot_data <- base_data_set_imputed |>
  group_by(year) |>
  summarise(
    house_price = mean(house_price),
    income = mean(avg_income_working_total)
  ) |>
  mutate(
    affordability_ratio = house_price / income
  )

ggplot(
  plot_data,
  aes(year, affordability_ratio)
) +
  geom_line() +
  geom_point() +
  theme_minimal()+
  labs(
    title = "Figuur 10. betaalbaarheids ratio",
    x = "Jaar",
    y = "ratio"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )
