
library(tidyverse)
library(here)
library(knitr)
library(brms)
library(brmstools)
library(tidybayes)
library(conflicted)
library(countrycode)
library(psych)
library(parallel)
library(rstan)
library(rstantools)
library(MASS)
library(modelr)

# Read and clean data

# US protest events only
event.list <- c("Protest altruism", "Protest defacement", "Protest demonstrations", "Protest obstruction",
                "Protest procession", "Strikes and boycotts", "Threaten to boycott or embargo")

protest.us <- readxl::read_xlsx(here("Data/Chapter - Protests", "_Revised Extractions and Events.xlsx"), sheet = 2) %>%  
  filter(`Protest Event` %in% event.list) %>% 
  mutate(miltarget = if_else(TgtSector == "<MILI>", 1, 0),
         ustarget = if_else(TgtCountry == "USA", 1, 0),
         antiusmil = if_else(miltarget == 1 & ustarget == 1, 1, 0),
         year = format(as.Date(EventDate, format = "%Y-%m-%d"), "%Y"),
         month = format(as.Date(EventDate, format = "%Y-%m-%d"), "%b")) %>% 
  group_by(Location, year, month, ID) %>% 
  mutate(duplicates = seq()) %>% 
  group_by(Location, year) %>% 
  summarise(anti.us.protest = sum(ustarget, na.rm = TRUE),
            anti.us.mil = sum(antiusmil, na.rm = TRUE)) %>% 
  mutate(year = as.numeric(year),
         ccode = countrycode(Location, "country.name", "cown"))


# All protest events (denomonator)
protest.base <- readxl::read_xlsx(here("Data/Chapter - Protests", "Foreign Protest Monthly Counts.xlsx")) %>% 
  filter(Event %in% event.list) %>% 
  group_by(Country, Year) %>% 
  summarise(protest.count = sum(Count, na.rm = TRUE)) %>%
  mutate(ccode = countrycode(Country, "country.name", "cown")) %>%
  rename("year" = "Year") %>% 
  filter(!is.na(ccode))


# Country-year list
cyear.base <- read_csv(here("Data/Chapter - Protests", "system2016 (2).csv")) %>%
  filter(year >= 1988) %>% 
  select(ccode, year)


# Protest combined 
protest.combined <- cyear.base %>% 
  left_join(protest.base) %>% 
  left_join(protest.us) %>% 
  mutate(protest.count = if_else(is.na(protest.count), 0, protest.count),
         anti.us.protest = if_else(is.na(anti.us.protest), 0, anti.us.protest),
         anti.us.mil = if_else(is.na(anti.us.mil), 0, anti.us.mil),
         country = countrycode(ccode, "cown", "country.name")) %>% 
  select(-c(Location, Country))


# US Military deployment data
troop.data.2014 <- readstata13::read.dta13(here("Data/Chapter - Protests", "troops-update-2014.dta")) %>% 
  filter(year >= 1988 & ccode != 2)

# Troops 2015 (includes active duty only)
troop.data.2015 <- readxl::read_xlsx(here("Data/Chapter - Protests", "DMDC_Website_Location_Report_1512.xlsx"), skip = 62) %>% 
  mutate(ccode = countrycode(`...2`, "country.name", "cown")) %>% 
  mutate(ccode = ifelse(grepl(".*SERBIA.*", `...2`), 345, ccode)) %>% 
  filter(!is.na(ccode)) %>% 
  rename("troops" = 8) %>% 
  mutate(year = 2015) %>% 
  select(ccode, troops, year) %>% 
  arrange(ccode)

# Troops 2016 (includes active duty only)
troop.data.2016 <- readxl::read_xlsx(here("Data/Chapter - Protests", "DMDC_Website_Location_Report_1612.xlsx"), skip = 62) %>% 
  mutate(ccode = countrycode(`...2`, "country.name", "cown")) %>% 
  mutate(ccode = ifelse(grepl(".*SERBIA.*", `...2`), 345, ccode)) %>% 
  filter(!is.na(ccode)) %>% 
  rename("troops" = 8) %>% 
  mutate(year = 2016) %>% 
  select(ccode, troops, year) %>% 
  arrange(ccode)

# Troops 2017 (includes active duty only)
troop.data.2017 <- readxl::read_xlsx(here("Data/Chapter - Protests", "DMDC_Website_Location_Report_1712.xlsx"), skip = 62) %>% 
  mutate(ccode = countrycode(`...2`, "country.name", "cown")) %>% 
  mutate(ccode = ifelse(grepl(".*SERBIA.*", `...2`), 345, ccode)) %>% 
  filter(!is.na(ccode)) %>% 
  rename("troops" = 8) %>% 
  mutate(year = 2017) %>% 
  select(ccode, troops, year) %>% 
  arrange(ccode)

# Troops 2018 (includes active duty only)
troop.data.2018 <- readxl::read_xlsx(here("Data/Chapter - Protests", "DMDC_Website_Location_Report_1812.xlsx"), skip = 61) %>% 
  mutate(ccode = countrycode(`...2`, "country.name", "cown")) %>% 
  mutate(ccode = ifelse(grepl(".*SERBIA.*", `...2`), 345, ccode)) %>% 
  filter(!is.na(ccode)) %>% 
  rename("troops" = 8) %>% 
  mutate(year = 2018) %>% 
  select(ccode, troops, year) %>% 
  arrange(ccode)

# Combine troops data
troops.data.combined <- plyr::rbind.fill(troop.data.2014, troop.data.2015, troop.data.2016, troop.data.2017, troop.data.2018) %>% 
  select(ccode, year, troops) %>% 
  arrange(ccode, year) %>% 
  mutate(lntroops = log(troops + 1))


# World Bank GDP Data
gdp.data <- read_csv(here("Data/Chapter - Protests", "API_NY.GDP.MKTP.KD_DS2_en_csv_v2_820861.csv"), skip = 3) %>% 
  select(c(1,5:63)) %>% 
  pivot_longer(cols = 2:60,
               names_to = "year",
               values_to = "gdp.cons") %>%
  rename("country" = `Country Name`) %>% 
  group_by(country) %>% 
  arrange(country, year) %>% 
  mutate(gdp.growth = ((gdp.cons - lag(gdp.cons)) / lag(gdp.cons)) * 100,
         ccode = countrycode(country, "country.name", "cown"),
         year = as.numeric(year)) %>% 
  filter(!is.na(ccode) & year >= 1988)


# World Bank population data
pop.data <- read_csv(here("Data/Chapter - Protests", "API_SP.POP.TOTL_DS2_en_csv_v2_821007.csv"), skip = 3) %>% 
  select(c(1,5:63)) %>% 
  pivot_longer(cols = 2:60,
               names_to = "year",
               values_to = "pop.total") %>%
  rename("country" = `Country Name`) %>% 
  mutate(ccode = countrycode(country, "country.name", "cown"),
         year = as.numeric(year)) %>% 
  filter(!is.na(ccode) & year >= 1988)


# V-Dem data
vdem.data <- read_csv(here("Data/Chapter - Protests/Country_Year_V-Dem_Core_CSV_v9", "V-Dem-CY-Core-v9.csv")) %>% 
  filter(year >= 1988) %>% 
  rename("ccode" = "COWcode") %>% 
  select(ccode, year, v2x_polyarchy, v2x_libdem, v2x_egaldem, v2x_freexp_altinf, v2x_suffr, v2x_cspart, v2xel_locelec, v2xel_regelec)
  


protest.data.final <- vdem.data %>% 
  left_join(protest.combined) %>% 
  left_join(gdp.data) %>% 
  left_join(pop.data) %>% 
  left_join(troops.data.combined) %>% 
  arrange(ccode,  year) %>% 
  filter(!is.na(country) & !is.na(year))



# Descriptive Figures




# Country-Year Models

# Parallelize machine
options(mc.cores = parallel::detectCores())
ncores <- detectCores()
rstan_options(auto_write = TRUE)

vagueprior <- set_prior("normal(0, 20)", class = "b")

# Base model
cm.1 <- brm(anti.us.protest ~ v2x_libdem + gdp.growth + log(pop.total) + lntroops + v2x_libdem*lntroops + lag(protest.count), 
            data = protest.data.final,
            iter = 8000,
            warmup = 4000,
            prior = vagueprior,
            chains = 1,
            cores = 1,
            seed = 66502,
            family = negbinomial())


data <- protest.data.final %>% 
  data_grid(lntroops = seq_range(lntroops, n = 20),
            v2x_libdem = c(quantile(v2x_libdem, probs = c(0.10), na.rm = TRUE), quantile(v2x_libdem, probs = c(0.90), na.rm = TRUE)),
            gdp.growth = mean(gdp.growth, na.rm = TRUE),
            pop.total = log(mean(pop.total, na.rm = TRUE)),
            protest.count = mean(protest.count, na.rm = TRUE)) %>% 
  add_fitted_draws(cm.1, n = 1000) %>% 
  group_by(lntroops) %>% 
  mutate(medianvalue = median(.value, na.rm = TRUE))


ggplot(data = data, aes(x = lntroops, y = exp(.value), group = .draw)) +
  geom_line(alpha = 0.2) +
  geom_line(aes(y = exp(medianvalue)), color = "red", size = 3) +
  facet_wrap(. ~ v2x_libdem) +
  theme_bw()