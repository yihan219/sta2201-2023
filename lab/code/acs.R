# get variables from 2019 5-year ACS
library(tidycensus)

census_api_key("YOUR KEY HERE") # if you want to play around with this code you will need your own API key, see here: https://walker-data.com/tidycensus/articles/basic-usage.html
v17 <- load_variables(2019, "acs5", cache = TRUE) # to search tables

# extract variables of interest
acs <- get_acs(geography = "county", 
              variables = c(pop_male = "B05003_008", # population males 18+
                            pop_female = "B05003_019", # population females 18+
                            median_age = "B01002_001", # median age
                            median_income = "B19013_001", # median income
                            median_rent = "B25064_001",
                            pop_male_foreign_born = "B05003_010", # foreign males 18+
                            pop_female_foreign_born = "B05003_021",
                            white_pop = "B01001A_001", # total white population
                            total_pop = "B01001_001", # total population
                            total_pop_educ = "B06009_001", # denom for education
                            less_hs = "B06009_002", # population with less than high school
                            high_school = "B06009_003", # high school
                            some_college = "B06009_004", # some college
                            bachelor = "B06009_005", # bachelor degree
                            grad_or_professional = "B06009_006", # grad or professional degree
                            total_pop_employ = "B23025_001", # denom for employment
                            employed = "B23025_004", # number employed
                            unemployed = "B23025_005",
                            nilf = "B23025_007", # not in labor force
                            total_pop_health = "B27020_001", # denom for health insurance
                            health_native = "B27020_003", # native born health insurance
                            health_foreign_nat = "B27020_009", # foreign naturalized health insurance
                            health_foreign_non = "B27020_014", # foreign non citizen health insurance
                            poverty_denom = "B17022_001", # denom for poverty
                            ratio_income_poverty_low = "B17022_002" # ratio less than 1.3
                            ), 
              year = 2019)

# long format

acs_wide <- acs %>% 
  select(-moe) %>% 
  pivot_wider(names_from = "variable", values_from = "estimate")

#tidy up names

acs_wide <- acs_wide %>% 
  rename(fips = GEOID, county_name = NAME)

# create some variables more useful for regression

acs_wide <- acs_wide %>% 
  mutate(prop_white = white_pop/total_pop,
         prop_less_than_hs = less_hs/total_pop_educ,
         prop_bachelor_above = (bachelor + grad_or_professional)/total_pop_educ,
         total_pop_18plus = pop_male+pop_female,
         prop_foreign_born = (pop_male_foreign_born+pop_female_foreign_born)/total_pop_18plus,
         prop_unemployed = unemployed/total_pop_employ,
         prop_nilf = nilf/total_pop_employ,
         prop_health_insurance = (health_native+health_foreign_nat+health_foreign_non)/total_pop_health,
         prop_low_ratio_ip = ratio_income_poverty_low/poverty_denom)

acs_long <- acs_wide %>% 
  select(fips, county_name, total_pop_18plus, 
         prop_white, prop_foreign_born, median_age,
         median_income, median_rent,
         prop_less_than_hs, prop_bachelor_above,
         prop_unemployed, prop_nilf,
         prop_health_insurance, prop_low_ratio_ip) %>% 
  pivot_longer(-(fips:county_name),names_to = "variable", values_to = "value") %>% 
  filter(!str_detect(county_name, "Puerto Rico"))

# check proportions okay
acs_long %>% 
  filter(!str_detect(variable, "median|total")) %>% 
  filter(value>1)

acs_long %>% 
  filter(!str_detect(variable, "median|total")) %>% 
  filter(value<0)
  
write_csv(acs_long, "data/acs.csv")
