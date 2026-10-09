################################################################################
# DATA CLEANING AND TRANSFORMATION
################################################################################

# Housing price data. convert csv into df with separate columns.
house_prices <- house_prices_raw |>
  separate(
    col = 1,
    into = c("year", "province", "price"),
    sep = ";"
  ) %>%
  mutate(year = as.numeric(year)) |>
  mutate(price = as.numeric(price))

summary(house_prices)

# Overall income data. convert csv into df with separate columns. remove unwanted
# spaces and characters. select only columns needed.
income_total <- income_total_raw |>
  separate(
    col = 1,
    into = c(
      "Inkomensbegrippen",
      "Inkomensklassen",
      "Geslacht",
      "Kenmerken_van_personen",
      "year",
      "avg_income_x_1000"
    ),
    sep = ";"
  ) |>
  mutate(
    year = gsub("\\*", "", year),
    avg_income_x_1000 = gsub(",", ".", avg_income_x_1000)
  ) |>
  select(year, avg_income_x_1000) |>
  mutate(
    year = as.numeric(year),
    avg_income_x_1000 = as.numeric(avg_income_x_1000)
  )

summary(income_total)

# Male income data. convert csv into df with separate columns. remove unwanted
# spaces and characters. select only columns needed.
income_men <- income_men_raw |>
  separate(
    col = 1,
    into = c(
      "sex",
      "Inkomensbestanddelen",
      "Kenmerken_van_personen",
      "year",
      "amound_male_x_1000",
      "avg_income_male_x_1000"
    ),
    sep = ";"
  ) |>
  mutate(year = gsub("\\*", "", year)) |>
  select(sex, year, amound_male_x_1000, avg_income_male_x_1000) %>%
  mutate(year = as.numeric(year)) |>
  mutate(amound_male_x_1000 = gsub(",", ".", amound_male_x_1000)) |>
  mutate(amound_male_x_1000 = as.numeric(amound_male_x_1000)) |>
  mutate(avg_income_male_x_1000 = gsub(",", ".", avg_income_male_x_1000)) |>
  mutate(avg_income_male_x_1000 = as.numeric(avg_income_male_x_1000))

summary(income_men)

# Female income data. convert csv into df with separate columns. remove unwanted
# spaces and characters. select only columns needed.
income_women <- income_women_raw |>
  separate(
    col = 1,
    into = c(
      "sex",
      "Inkomensbestanddelen",
      "Kenmerken_van_personen",
      "year",
      "amound_female_x_1000",
      "avg_income_female_x_1000"
    ),
    sep = ";"
  ) |>
  mutate(year = gsub("\\*", "", year)) |>
  select(sex, year, amound_female_x_1000, avg_income_female_x_1000) %>%
  mutate(year = as.numeric(year)) |>
  mutate(amound_female_x_1000 = gsub(",", ".", amound_female_x_1000)) |>
  mutate(amound_female_x_1000 = as.numeric(amound_female_x_1000)) |>
  mutate(avg_income_female_x_1000 = gsub(",", ".", avg_income_female_x_1000)) |>
  mutate(avg_income_female_x_1000 = as.numeric(avg_income_female_x_1000))

summary(income_women)

# Overall income data per province un weighted. convert csv into df with separate columns. remove unwanted
# spaces and characters. select only columns needed.
un_weighted_income_per_province <- income_un_weighted_province |>
  separate(
    col = 1,
    into = c(
      "province",
      "2024",
      "2023",
      "2022",
      "2021",
      "2020",
      "2019",
      "2018",
      "2017",
      "2016",
      "2015",
      "2014",
      "2013"
    ),
    sep = ";")

un_weighted_income_per_province <- un_weighted_income_per_province |>
  pivot_longer(
    cols = -province,
    names_to = "year",
    values_to = "income"
  ) |>
  mutate(year = as.numeric(year))

# remove unwanted characters, and make numeric
un_weighted_income_per_province <- un_weighted_income_per_province |>
  mutate(
    province = trimws(province),
    year = as.numeric(year),
    income = gsub("\\.", "", income),
    income = gsub(",", ".", income),
    income = as.numeric(income)
  )

summary(un_weighted_income_per_province)

# Mortgage interest data. convert csv into df with separate columns. remove unwanted
# spaces and characters. select only columns needed.
Mortgage_interest <- Mortgage_interest |> # probleem
  separate(
    col = 1,
    into = c(
      "total",
      "year",
      "interest"
    ),
    sep = ","
  )
# change interest in numeric
Mortgage_interest <- Mortgage_interest %>%
  mutate(
    interest = gsub(",", ".", interest),
    interest = as.numeric(interest)
  )

# remove month an take the average interest.
Mortgage_interest <- Mortgage_interest %>%
  mutate(year = str_extract(year, "\\d{4}")) %>%
  group_by(year) %>%
  mutate(year = as.numeric(year)) |>
  summarise(
    interest = mean(interest, na.rm = TRUE)) |>
ungroup()

summary(Mortgage_interest)

# Population data. convert csv into df with separate columns. remove unwanted
# spaces and characters. select only columns needed.
population_growth <- population_growth |>
  separate(
    col = 1,
    into = c(
      "year",
      "natural_growth_x_1000",
      "migration_x_1000",
      "total_growth_x_1000"
    ),
    sep = ";"
  ) |>
  mutate(year = gsub("\\*", "", year)) |>
  mutate(year = as.numeric(year)) |>
  mutate(natural_growth_x_1000 = gsub(",", ".", natural_growth_x_1000)) |>
  mutate(natural_growth_x_1000 = as.numeric(natural_growth_x_1000)) |>
  mutate(migration_x_1000 = gsub(",", ".", migration_x_1000)) |>
  mutate(migration_x_1000 = as.numeric(migration_x_1000)) |>
  mutate(total_growth_x_1000 = gsub(",", ".", total_growth_x_1000)) |>
  mutate(total_growth_x_1000 = as.numeric(total_growth_x_1000))

summary(population_growth)

# Total Population data. convert csv into df with separate columns. remove unwanted
# spaces and characters. select only columns needed.
total_population <- total_population |>
  separate(
    col = 1,
    into = c(
      "year",
      "people_in_holland"
    ),
    sep = ";"
  ) |>
  mutate(year = gsub("\\*", "", year)) |>
  mutate(year = as.numeric(year)) |>
  mutate(people_in_holland = gsub(",", ".", people_in_holland)) |>
  mutate(people_in_holland = as.numeric(people_in_holland))

summary(total_population)

# join all sets together based on years.
# Merge all datasets using year as key
base_data_set <- house_prices %>%
  left_join(income_total, by = "year") %>%
  left_join(Mortgage_interest, by = "year") |>
  left_join(income_men, by = "year") %>%
  left_join(income_women, by = "year") %>%
  left_join(un_weighted_income_per_province, c("province", "year")) |>
  left_join(population_growth, by = "year") %>%
  left_join(total_population, by = "year") %>%
  filter(year >= 2009 & year <= 2024)

# Remove observations with missing target values
base_data_set <- base_data_set %>%
  filter(!is.na(price))

# final check
names(base_data_set) 
sum(is.na(base_data_set$price))
summary(base_data_set)

################################################################################
# imputeer na's with missforest
################################################################################

# Controleer eerst het aantal missende waarden
colSums(is.na(base_data_set))

# Eventueel karaktervariabelen omzetten naar factor
base_data_set$province <- as.factor(base_data_set$province)
base_data_set$sex.x <- as.factor(base_data_set$sex.x)
base_data_set$sex.y <- as.factor(base_data_set$sex.y)

sapply(base_data_set, class)

set.seed(123)

imputed_data <- missForestPredict::missForest(
  base_data_set,
  maxiter = 10,
  initialization = "median/mode",
  save_models = TRUE,
  num.threads = 2,
  verbose = FALSE
)

# Dataset zonder missings
base_data_set_imputed <- imputed_data$ximp

################################################################################
# calculate income per province for working people
################################################################################
x <- mean(base_data_set_imputed$avg_income_x_1000)
y <- mean(base_data_set_imputed$income)

factor <- x/y * 1000

base_data_set_imputed <- base_data_set_imputed |>
  mutate(incom_converted = income*factor)

################################################################################
# remove unnecessary columns. sex.x and sex.y
################################################################################
base_data_set_imputed <- base_data_set_imputed |>
  select(-c(sex.x, sex.y))

################################################################################
# rename columns.
################################################################################
base_data_set_imputed <- base_data_set_imputed |>
  mutate(house_price = price,
         avg_income_working_total = avg_income_x_1000 * 1000,
         mortgage_rate = interest,
         working_males = amound_male_x_1000 * 1000,
         avg_income_males_working = avg_income_male_x_1000 * 1000,
         working_females = amound_female_x_1000 * 1000,
         avg_income_females_working = avg_income_female_x_1000 * 1000,
         avg_income_incl_non_work = income,
         natural_pop_growth = natural_growth_x_1000 * 1000,
         migration_growth = migration_x_1000 * 1000,
         total_growth = total_growth_x_1000 *1000,
         total_pop_in_mln = people_in_holland,
         avg_income_working_province = incom_converted) |>
  select(
    -price,
    -avg_income_x_1000,
    -interest,
    -amound_male_x_1000,
    -avg_income_male_x_1000,
    -amound_female_x_1000,
    -avg_income_female_x_1000,
    -income,
    -natural_growth_x_1000,
    -migration_x_1000,
    -total_growth_x_1000,
    -people_in_holland,
    -incom_converted
    ) |>
  select(
    year,
    province,
    house_price,
    mortgage_rate,
    avg_income_working_total,
    avg_income_males_working,
    avg_income_females_working,
    avg_income_incl_non_work,
    avg_income_working_province,
    working_males,
    working_females,
    natural_pop_growth,
    migration_growth,
    total_growth,
    total_pop_in_mln
  )
  
summary(base_data_set_imputed)

saveRDS(
  base_data_set_imputed,
  "data/processed/base_data_set_imputed.rds"
)
