library(tidyverse)
library(here)
library(cshapes)
library(countrycode)
library(conflicted)
library(arm)

conflict_prefer("here", "here")
conflict_prefer("filter", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("rescale", "arm")

# Read and clean data

#Please note that the PRIO armed conflict data is available from this website: https://ucdp.uu.se/downloads/
  
# Read and clean data
# US protest events only
event.list <- c("Protest altruism", "Protest defacement", "Protest demonstrations", "Protest obstruction", "Protest procession", "Strikes and boycotts", "Threaten to boycott or embargo")

protest.us <- readxl::read_xlsx(here::here("Raw Data/Chapter-Protests", "_Revised Extractions and Events.xlsx"), sheet = 2) %>%  
  filter(`Protest Event` %in% event.list) %>% 
  mutate(miltarget = if_else(TgtSector == "<MILI>", 1, 0),
         ustarget = if_else(TgtCountry == "USA", 1, 0),
         antiusmil = if_else(miltarget == 1 & ustarget == 1, 1, 0),
         year = format(as.Date(EventDate, format = "%Y-%m-%d"), "%Y"),
         month = format(as.Date(EventDate, format = "%Y-%m-%d"), "%b")) %>% 
  mutate(ccode = countrycode(Location, "country.name", "cown")) %>% 
  group_by(ccode, year, month, ID) %>% 
  mutate(duplicates = seq()) %>% 
  group_by(ccode, year) %>% 
  summarise(anti_us_protest = sum(ustarget, na.rm = TRUE),
            anti_us_mil = sum(antiusmil, na.rm = TRUE)) %>% 
  mutate(year = as.numeric(year))


# All protest events (denomonator)
protest.base <- readxl::read_xlsx(here("Raw Data/Chapter-Protests", "Foreign Protest Monthly Counts.xlsx")) %>% 
  filter(Event %in% event.list) %>% 
  mutate(ccode = countrycode(Country, "country.name", "cown")) %>%
  group_by(ccode, Year) %>% 
  summarise(protest_count = sum(Count, na.rm = TRUE)) %>%
  rename("year" = "Year") %>% 
  filter(!is.na(ccode))


# Country-year list
cyear.base <- read_csv(here("Raw Data/Chapter-Protests", "system2016 (2).csv")) %>%
  filter(year >= 1988) %>% 
  dplyr::select(ccode, year)


# Protest combined 
protest.combined <- cyear.base %>% 
  left_join(protest.base, by = c("ccode", "year")) %>% 
  left_join(protest.us, by = c("ccode", "year")) %>% 
  mutate(protest_count = if_else(is.na(protest_count), 0, protest_count),
         anti_us_protest = if_else(is.na(anti_us_protest), 0, anti_us_protest),
         anti_us_mil = if_else(is.na(anti_us_mil), 0, anti_us_mil),
         country = countrycode(ccode, "cown", "country.name"))


# Penn world tables data
# Includes total population, real gdp per capita 2011 USD, and human capital index
pwt.data <- readxl::read_xlsx(here("Raw Data/Chapter-Protests", "pwt91.xlsx"), sheet = 3) %>%
  mutate(ccode = countrycode(country, "country.name", "cown")) %>% 
  dplyr::select("country", "ccode", "year", "pop", "rgdpe", "hc")


# V-Dem Data
vdem.data <- read_csv(here::here("Raw Data/Chapter-Protests/Country_Year_V-Dem_Core_CSV_v9", "V-Dem-CY-Core-v9.csv")) %>% 
  select("country_name", "year", "v2x_polyarchy", "v2x_libdem", "COWcode") %>% 
  filter(year >= 1988) %>% 
  rename("ccode" = "COWcode") %>% 
  mutate(ccode = ifelse(ccode == 260, 255, ccode)) 



# WARNING!!! for whatever reason the pivot function sometimes doesn't work on every other pass. 
# Make sure this is in long form

# Also, LOTS of stupid names match the Netherlands country codes. Filter those out.
netherlandsfilter <- c("Curaçao, Kingdom of the Netherlands", "Sint Maarten, Kingdom of the Netherlands", "Aruba, Kingdom of the Netherlands")

`%notin%` <- Negate(`%in%`) # create this to get rid of lots of duplicate Netherlands values. Appear as a result of protectorates or territories. 

# IMF DOTS Exports to counterpart countries from US
exports.data <- readxl::read_xlsx(here::here("Raw Data/Chapter-Protests", "Exports_to_Counterpart_Countries.xlsx"), skip = 5) %>% 
  rename("country" = 1) %>% 
  pivot_longer(cols = 2:ncol(exports.data),
               values_to = "exports",
               names_to = "year") %>% 
  filter(country %notin% netherlandsfilter) %>% 
  filter(year >= 1988) %>% 
  mutate(ccode = countrycode(country, "country.name", "cown"),
         ccode = ifelse(country == "Eastern Germany", 265, ccode),
         ccode = ifelse(country == "China, P.R.", 710, ccode),
         ccode = ifelse(country == "Central African Rep.", 482, ccode),
         year = as.numeric(year)) 



# IMF DOTS Imports from counterpart countries into US
imports.data <- readxl::read_xlsx(here("Raw Data/Chapter-Protests", "Imports_from_Counterpart_Countries.xlsx"), skip = 5) %>% 
  rename("country" = 1) %>% 
  pivot_longer(cols = 2:ncol(imports.data),
               values_to = "imports",
               names_to = "year") %>% 
  filter(country %notin% netherlandsfilter) %>% 
  filter(year >= 1988) %>% 
  mutate(ccode = countrycode(country, "country.name", "cown"),
         ccode = ifelse(country == "Eastern Germany", 265, ccode),
         ccode = ifelse(country == "China, P.R.", 710, ccode),
         ccode = ifelse(country == "Central African Rep.", 482, ccode),
         year = as.numeric(year)) 



# Load foreign aid data from US AID Green book
aid.base <- readxl::read_xlsx(here("Raw Data/Chapter-Protests", "us_foreignaid_greenbook.xlsx"), skip = 6)

# Rename the columns
names(aid.base) <- c("year", "region", "country", "category", "pubrow", "agency", "account", "aid.current", "aid.constant")


# Create county-year tibble
time <- tibble(ccode2 = 2:999) %>% 
  mutate(ccode = countrycode(ccode2, "cown", "cown"),
         year = list(seq(from = 1949, to = 2018))) %>% 
  filter(!is.na(ccode) & ccode != 2) %>% 
  select("ccode", "year") %>% 
  unnest(cols = c(year))


# Read in and edit aid data
aid.data <- aid.base %>% 
  mutate(year = ifelse(year == 1976.9, year == 1976, year)) %>% 
  group_by(country, year, category) %>% 
  summarise(aid.constant = sum(aid.constant, na.rm = TRUE),
            aid.current = sum(aid.current, na.rm = TRUE)) %>% 
  dplyr::select(-c(5)) %>% 
  pivot_wider(names_from = category,
              values_from = aid.constant) %>% 
  ungroup() %>% 
  mutate(ccode = countrycode(country, "country.name", "cown")) %>% 
  filter(!is.na(ccode)) %>% 
  full_join(time) %>% 
  mutate(Economic = ifelse(is.na(Economic), 0, Economic),
         Military = ifelse(is.na(Military), 0, Military),
         log_econ = log(Economic + 1),
         log_mil = log(Military + 1)) %>% 
  rename("econaid" = "Economic", "milaid" = "Military") %>% 
  arrange(ccode, year) %>% 
  mutate(country = countrycode(ccode, "cown", "country.name")) %>% 
  filter(year >= 1988) %>% 
  as.data.frame()



# Troop deployment data
# Note these files only use active duty troop numbers. Data on reserve and guard troops not always available.
# US Military deployment data up through 2014
troop.data.2014 <- readstata13::read.dta13(here("Raw Data/Chapter-Protests", "troops-update-2014.dta")) %>% 
  dplyr::filter(ccode != 2) %>% 
  dplyr::filter(year >= 1988)

# Troops 2015 (includes active duty only)
troop.data.2015 <- readxl::read_xlsx(here("Raw Data/Chapter-Protests", "DMDC_Website_Location_Report_1512.xlsx"), skip = 62) %>% 
  mutate(ccode = countrycode(`...2`, "country.name", "cown")) %>% 
  mutate(ccode = ifelse(grepl(".*SERBIA.*", `...2`), 345, ccode)) %>% 
  filter(!is.na(ccode)) %>% 
  rename("troops" = 8) %>% 
  mutate(year = 2015) %>% 
  dplyr::select(ccode, troops, year) %>% 
  arrange(ccode)

# Troops 2016 (includes active duty only)
troop.data.2016 <- readxl::read_xlsx(here("Raw Data/Chapter-Protests", "DMDC_Website_Location_Report_1612.xlsx"), skip = 62) %>% 
  mutate(ccode = countrycode(`...2`, "country.name", "cown")) %>% 
  mutate(ccode = ifelse(grepl(".*SERBIA.*", `...2`), 345, ccode)) %>% 
  filter(!is.na(ccode)) %>% 
  rename("troops" = 8) %>% 
  mutate(year = 2016) %>% 
  dplyr::select(ccode, troops, year) %>% 
  arrange(ccode)

# Troops 2017 (includes active duty only)
troop.data.2017 <- readxl::read_xlsx(here("Raw Data/Chapter-Protests", "DMDC_Website_Location_Report_1712.xlsx"), skip = 62) %>% 
  mutate(ccode = countrycode(`...2`, "country.name", "cown")) %>% 
  mutate(ccode = ifelse(grepl(".*SERBIA.*", `...2`), 345, ccode)) %>% 
  filter(!is.na(ccode)) %>% 
  rename("troops" = 8) %>% 
  mutate(year = 2017) %>% 
  dplyr::select(ccode, troops, year) %>% 
  arrange(ccode)

# Troops 2018 (includes active duty only)
troop.data.2018 <- readxl::read_xlsx(here("Raw Data/Chapter-Protests", "DMDC_Website_Location_Report_1812.xlsx"), skip = 61) %>% 
  mutate(ccode = countrycode(`...2`, "country.name", "cown")) %>% 
  mutate(ccode = ifelse(grepl(".*SERBIA.*", `...2`), 345, ccode)) %>% 
  filter(!is.na(ccode)) %>% 
  rename("troops" = 8) %>% 
  mutate(year = 2018) %>% 
  dplyr::select(ccode, troops, year) %>% 
  arrange(ccode)

# Combine troops data
troops.data.combined <- bind_rows(troop.data.2014, troop.data.2015, troop.data.2016, troop.data.2017, troop.data.2018) %>% 
  dplyr::select(ccode, year, troops) %>% 
  arrange(ccode, year) %>% 
  mutate(ccode = ifelse(ccode == 260, 255, ccode)) %>% 
  group_by(ccode, year) %>% 
  dplyr::summarise(troops = max(troops, na.rm = TRUE)) %>% 
  group_by(ccode) %>% 
  mutate(troops = ifelse(is.finite(troops), troops, NA),
         log_troops = round(log(troops + 1)),
         log_troops = ifelse(is.finite(log_troops), log_troops, NA), # remove NaN and Inf values
         troops_change = ifelse(!is.na(troops) & !is.na(dplyr::lag(troops)), as.numeric(troops - dplyr::lag(troops)), NA),
         troops_change = ifelse(is.finite(troops_change), troops_change, NA))  # remove NaN and Inf values


# Create second data frame for troops data for spatial weighting
troops.sp <- troops.data.combined %>% 
  filter(year >= 1988) %>% 
  dplyr::select("ccode", "year", "troops") %>% 
  dplyr::rename("ccode2" = "ccode")

# Create spatial weighting framework for troops variable
mindist <- list() # Create empty list 
startdate = as.Date("1988-01-01", format = "%Y-%m-%d")
enddate = as.Date("2019-01-01", format = "%Y-%m-%d")
datelist <- tibble(date = as.Date(seq(startdate, enddate, by = "1 year"), format = "%y-%m-%d"))

thedate <- startdate

for (i in 1:32) {
  
  thedate <- datelist$date[i]
  mindist[[i]] <- cshapes::distlist(thedate, type = "mindist", tolerance = 0.1, useGW = FALSE)
  mindist[[i]]$year <- thedate
  
}  


# Create minimum distance dataframe to merge troops data
# Note that the data only go through 2016. Must replicate 2017 and 2018.
mindist.df.1 <- bind_rows(mindist) %>% 
  mutate(year = as.numeric(format(year, "%Y")))

mindist.df.2 <- subset(mindist.df.1, year >= 2015) %>% 
  mutate(year = year + 2)


mindist.df <- bind_rows(mindist.df.1, mindist.df.2) %>% 
  filter(ccode1 != ccode2) %>% 
  mutate(ccode1 = ifelse(ccode1 == 260, 255, ccode1),
         ccode2 = ifelse(ccode2 == 260, 255, ccode2),
         mindist = ifelse(mindist < 1, 0, mindist), # Note: distlist is returning weird values < 1 & > 0. These all appear to be contiguous.
         mindist450k = ifelse(mindist == 0, 1, 
                              ifelse(mindist > 450, 0, mindist))) %>% 
  left_join(troops.sp, by = c("ccode2", "year")) %>% 
  mutate(inversedistance = if_else(mindist450k > 0, (1 / mindist450k), 0),
         troops_w = inversedistance * troops) %>% # Look only at deployments within 450k of the referent state
  arrange(ccode1, ccode2, year) %>% 
  group_by(ccode1, year) %>% 
  dplyr::summarise(troops_w_sum = sum(troops_w, na.rm = TRUE),
                   troops_w_mean = mean(troops_w, na.rm = TRUE)) %>% 
  mutate(troops_w_sum = ifelse(is.finite(troops_w_sum), troops_w_sum, NA),
         troops_w_mean = ifelse(is.finite(troops_w_mean), troops_w_mean, NA)) %>% 
  filter(ccode1 != 2) %>% 
  dplyr::rename("ccode" = "ccode1") 




# Create a time series frame with the total size of the US military by year
# Military size 1954-1993

milsize.1950.2005 <- readxl::read_xls(here::here("Raw Data/Chapter-Protests", "troopMarch2005.xls"), sheet = 2) %>% # Kane's Heritage data.
  slice(6) %>% 
  dplyr::select(-1) %>% 
  mutate_if(is.numeric, as.character) %>% # Some conflicting types after reading data in
  pivot_longer(cols = 2:57,
               values_to = "troops",
               names_to = "year") %>% 
  dplyr::select(year, troops) %>% 
  mutate_each(funs = as.numeric) %>% # Convert all to numeric for append 
  filter(year <= 1993)

# Military size 1994-2012

milsize <- readxl::excel_sheets(here::here("Raw Data/Chapter-Protests", "AD_Strengths_FY1994-FY2012.xlsx"))

milsize.list <- lapply(milsize, function(x) readxl::read_excel(here::here("Raw Data/Chapter-Protests", "AD_Strengths_FY1994-FY2012.xlsx"), sheet = x))

milsize.1.df <- bind_rows(milsize.list) %>% 
  filter(grepl(".*GRAND TOTAL.*", Rank)) %>% 
  dplyr::select("DoD Total") %>% 
  mutate(year = seq(1994, 2012)) %>% 
  dplyr::rename("Total" = "DoD Total") %>% 
  mutate(troops = as.numeric(Total),
         year = as.numeric(year)) %>% 
  dplyr::select(troops, year)

# Military size 2013-2016
milsize <- readxl::excel_sheets(here::here("Raw Data/Chapter-Protests", "AD_Strengths_FY2013-FY2016.xlsx"))

milsize.list <- lapply(milsize, function(x) readxl::read_excel(here::here("Raw Data/Chapter-Protests", "AD_Strengths_FY2013-FY2016.xlsx"), sheet = x))

milsize.2.df <- bind_rows(milsize.list) %>% 
  filter(grepl(".*GRAND TOTAL.*", `Department of Defense`)) %>% 
  dplyr::rename("Total" = 6) %>% 
  dplyr::select("Total") %>% 
  mutate(year = seq(2013, 2016)) %>% 
  mutate(troops = as.numeric(Total),
         year = as.numeric(year)) %>% 
  dplyr::select(troops, year)


# 2017 and 2018 data

# Troops 2017 (includes active duty only)
milsize.2017 <- readxl::read_xlsx(here::here("Raw Data/Chapter-Protests", "DMDC_Website_Location_Report_1712.xlsx"), skip = 5) %>% 
  filter(grepl(".*GRAND.*", LOCATION)) %>% 
  dplyr::select(8) %>% 
  dplyr::rename("troops" = 1) %>% 
  mutate(year = 2017) %>% 
  mutate_each(as.numeric)


# Troops 2018 (includes active duty only)
milsize.2018 <- readxl::read_xlsx(here::here("Raw Data/Chapter-Protests", "DMDC_Website_Location_Report_1812.xlsx"), skip = 5) %>% 
  filter(grepl(".*GRAND.*", LOCATION)) %>% 
  dplyr::select(8) %>% 
  dplyr::rename("troops" = 1) %>% 
  mutate(year = 2018) %>% 
  mutate_each(as.numeric)


milsize.1950.2018 <- bind_rows(milsize.1950.2005, milsize.1.df, milsize.2.df, milsize.2017, milsize.2018) %>% 
  rename("usmilsize" = "troops") %>% 
  filter(year >= 1988) 



# UN Ideal Point Data
undata <- read_delim(here("Raw Data/Chapter-Protests", "Idealpoints.tab"), delim = "\t") %>% # Tab delimited
  dplyr::select(ccode, idealpoint, year) %>% 
  dplyr::filter(year >= 1988) %>% 
  mutate(ccode2 = list(seq(2:999))) %>% 
  unnest(ccode2)

undata.2 <- read_delim(here("Raw Data/Chapter-Protests", "Idealpoints.tab"), delim = "\t") %>% # Tab delimited
  dplyr::select(ccode, idealpoint, year) %>% 
  dplyr::filter(year >= 1988) %>% 
  rename("ccode2" = "ccode")

undata.com <- undata %>% 
  full_join(undata.2, by = c("ccode2", "year")) %>% 
  dplyr::arrange(ccode, ccode2, year) %>% 
  filter(!is.na(idealpoint.y)) %>% 
  filter(ccode2 == 2 | ccode2 == 365) %>% 
  mutate(idealdistance = idealpoint.x - idealpoint.y) %>% 
  filter(ccode, ccode2, year, idealdistance) %>% 
  pivot_wider(id_cols = c(ccode, year),
              values_from = "idealdistance",
              names_from = "ccode2",
              names_prefix = "idealdistance_") %>% 
  filter(ccode != 2) %>% 
  mutate(ccode = ifelse(ccode == 260, 255, ccode)) %>% 
  group_by(ccode, year) %>% 
  arrange(ccode, year) 


# ATOP alliance data
ally.data <- read_csv(here("Raw Data/Chapter-Protests/ATOP V4.01 Data (csv)", "atop4_01ddyr.csv")) %>% 
  select(stateA, stateB, year, defense, offense) %>% 
  filter(stateA == 2,
         year >= 1988) %>% 
  mutate(usally = ifelse(defense == 1 | offense == 1, 1, 0),
         stateB = ifelse(stateB == 260, 255, stateB)) %>% 
  select(stateB, year, usally) %>% 
  rename("ccode" = "stateB") %>% 
  group_by(ccode,  year) %>% 
  summarise(usally = max(usally, na.rm = TRUE)) # Get rid of Germany duplicate


# US Government Composition Data
# The President chamber is missing from the 115th and 116th congresses. Code looks a little wild partly due to this. Also having to generate years.
nom.data <- read_csv(here("Raw Data/Chapter-Protests", "HSall_members.csv")) %>% 
  filter(congress >= 81) %>% 
  group_by(congress, chamber) %>% 
  mutate(seats = n_distinct(paste(state_icpsr, district_code)),
         reppres = ifelse(party_code == 200 & chamber == "President", 1, 0),
         repmem = ifelse(party_code == 200, 1, 0)) %>% 
  summarise(seats = max(seats, na.rm = TRUE),
            repseats = sum(repmem),
            reppres = max(reppres, na.rm = TRUE)) %>% 
  group_by(chamber) %>% 
  mutate(seats = ifelse(chamber == "Senate", seats * 2, seats),
         seats = ifelse(chamber == "House", 435, seats),
         repseatshare = repseats/seats) %>% 
  pivot_wider(values_from = c("seats", "repseats", "repseatshare", "reppres"),
              names_from = "chamber") %>% 
  mutate(session = list(seq(1:2))) %>% 
  unnest(session) %>% 
  mutate(year = seq(1949, 2020, by = 1),
         reppres = reppres_President,
         reppres = ifelse(congress == 114, 0, reppres),
         reppres = ifelse(congress == 116, 1, reppres),
         repseatshare_change_house = repseatshare_House - dplyr::lag(repseatshare_House, order_by = congress),
         repseatshare_change_house = ifelse(repseatshare_change_house == 0, dplyr::lag(repseatshare_change_house), repseatshare_change_house),
         repseatshare_change_senate = repseatshare_Senate - dplyr::lag(repseatshare_Senate, order_by = congress),
         repseatshare_change_senate = ifelse(repseatshare_change_senate == 0, dplyr::lag(repseatshare_change_senate), repseatshare_change_senate)) %>% 
  dplyr::select("congress", "year", "seats_House", "seats_Senate", "repseats_House", "repseats_Senate", "repseatshare_House", "repseatshare_change_house", "repseatshare_change_senate", "repseatshare_Senate", "reppres") %>% 
  rename("seats_house" = "seats_House", "seats_senate" = "seats_Senate", "repseats_house" = "repseats_House", "repseats_senate" = "repseats_Senate", "repseatshare_house" = "repseatshare_House", "repseatshare_senate" = "repseatshare_Senate")  



# PRIO Data
# https://ucdp.uu.se/downloads/
prio.data <- read_csv(here("Raw Data/Chapter-Protests/ucdp-prio-acd-191.csv")) %>% 
  rowwise() %>% 
  select("conflict_id", "year", "intensity_level", "gwno_loc") %>% 
  separate(gwno_loc, sep = ",", into = c("ccode1", "ccode2", "ccode3", "ccode4" ,"ccode5", "ccode6")) %>% 
  pivot_longer(cols = c("ccode1", "ccode2", "ccode3", "ccode4", "ccode5", "ccode6"), 
               names_to = "countrynum", 
               values_to = "ccode", 
               values_drop_na = TRUE,
               names_repair = "minimal") %>% 
  mutate(ccode = as.numeric(trimws(ccode, which = "both")),
         intensity.low = if_else(intensity_level == 1, 1, 0),
         intensity.high = if_else(intensity_level == 2, 1, 0)) %>% 
  group_by(ccode, year) %>% # Gleditsch and Ward location country code and year
  summarise(conflictcount = n_distinct(conflict_id),
            conflictcount_high = sum(intensity.high, na.rm = TRUE),
            conflictcount_low = sum(intensity.low, na.rm = TRUE),
            conflict_dummy = if_else(conflictcount > 0, 1, 0)) %>% 
  filter(year >= 1988)


# Create function to rescale variables by dividing by 1 million
scale1k <- function(x, na.rm = FALSE) (x/1000)
scale1m <- function(x, na.rm = FALSE) (x/1000000)

# Combine data into single data frame
protest.data.final <- vdem.data %>% 
  left_join(aid.data, by = c("ccode", "year")) %>% 
  left_join(pwt.data, by = c("ccode", "year")) %>% 
  left_join(protest.combined, by = c("ccode", "year")) %>% 
  left_join(exports.data, by = c("ccode", "year")) %>% 
  left_join(imports.data, by = c("ccode", "year")) %>% 
  left_join(mindist.df, by = c("ccode", "year")) %>%
  left_join(troops.data.combined, by = c("ccode", "year")) %>% 
  left_join(undata.com, by = c("ccode", "year")) %>% 
  left_join(ally.data, by = c("ccode", "year")) %>% 
  left_join(prio.data, by = c("ccode", "year")) %>% 
  left_join(milsize.1950.2018) %>% 
  left_join(nom.data, by = c("year")) %>% 
  dplyr::select("country", "ccode", "year", names(nom.data), "usally", "usmilsize", "idealdistance_2", "idealdistance_365", "protest_count", "anti_us_protest", "anti_us_mil", "conflictcount", "conflictcount_high", "conflictcount_low", "conflict_dummy", "pop", "rgdpe", "hc", "v2x_polyarchy", "v2x_libdem", "econaid", "milaid", "log_econ", "log_mil", "troops", "troops_change", "log_troops", "troops_w_sum", "troops_w_mean", "exports", "imports") %>% 
  mutate_at(vars("pop", "rgdpe", "exports", "imports", "econaid", "milaid", "troops", "troops_w_sum", "troops_w_mean", "troops_change"),
            list(rescaled = ~scale1m(.))) %>% # Divide by 1000 for running stan models later.
  group_by(ccode) %>% 
  mutate_at(vars("pop","rgdpe", "hc", "v2x_polyarchy", "econaid", "milaid", "v2x_libdem", "exports", "imports", "protest_count", "anti_us_protest", "anti_us_mil", "exports_rescaled", "imports_rescaled", "pop_rescaled", "rgdpe_rescaled", "milaid_rescaled", "econaid_rescaled", "exports_rescaled", "imports_rescaled", "idealdistance_2", "idealdistance_365", "troops_w_sum", "troops_w_mean", "troops_w_mean_rescaled", "troops_w_sum_rescaled"),
            list(change = ~ ifelse(is.finite(.), (.) - dplyr::lag(., order_by = year), NA))) %>% # weed out Inf and NaN values.
  mutate_at(vars("pop","rgdpe", "hc", "v2x_polyarchy", "v2x_polyarchy_change", "econaid", "econaid_change", "milaid", "milaid_change", "troops_rescaled", "v2x_libdem", "v2x_libdem_change", "protest_count", "anti_us_protest", "anti_us_mil", "exports", "exports_change", "imports", "imports_rescaled", "exports_rescaled", "idealdistance_2", "idealdistance_2_change", "idealdistance_365", "idealdistance_365_change", "troops", "troops_change", "troops_change_rescaled", "pop_rescaled", "pop_rescaled_change", "rgdpe_rescaled", "rgdpe_rescaled_change", "exports_rescaled", "exports_rescaled_change", "imports_rescaled", "imports_rescaled_change", "troops_w_sum", "troops_w_sum_change", "troops_w_mean", "troops_w_mean_change", "troops_w_mean_rescaled", "troops_w_sum_rescaled", "troops_w_mean_rescaled_change", "troops_w_sum_rescaled_change"),
            list(c_mean = ~mean(., na.rm = TRUE),
                 c_demeaned = ~(.) - mean(., na.rm = TRUE),
                 lag = ~dplyr::lag(., order_by = year))) %>%
  ungroup() %>% # I want to make sure the standardized demeaned scores, which are done by country, are standardized aacross the entire population of cases. Otherwise massive changes are smushed into the same tiny scale as smaller changes.
  mutate_at(vars(ends_with("c_demeaned"), ends_with("c_mean")),
            list(z = ~rescale(.))) %>% 
  mutate_at(vars("v2x_polyarchy", "v2x_libdem", "v2x_polyarchy_c_mean", "v2x_polyarchy_c_mean_z", "v2x_polyarchy_c_demeaned_z", "v2x_polyarchy_c_demeaned", "v2x_libdem_c_mean", "v2x_libdem_c_mean_z", "v2x_libdem_c_demeaned_z", "v2x_libdem_c_demeaned"),
            list(sq = ~(.)^2)) %>% 
  mutate(usmilsize = usmilsize/1000,
         log_usmilsize = log(usmilsize)) %>% # This is a really big number for some states. Divide by 1,000,000
  mutate(coldwar = if_else(year <= 1989, 1, 0),
         usally = ifelse(is.na(usally), 0, usally), # ATOP data doesn't cover all directed dyad years. Fill in missings with 0.
         region = as.factor(countrycode(ccode, "cown", "region")), # 17 World bank regional groups
         region2 = as.factor(countrycode(ccode, "cown", "continent")), # Five continent regional groups 
         region3 = ifelse(ccode >= 600 & ccode <= 698, "Middle East and North Africa", paste(as.character(region2))),
         region3 = as.factor(region3),
         country = as.factor(country),
         conflictcount = ifelse(is.na(conflictcount), 0, conflictcount),
         conflictcount_low = if_else(is.na(conflictcount_low), 0, conflictcount_low),
         conflictcount_high = if_else(is.na(conflictcount_high), 0, conflictcount_high),
         conflict_dummy = if_else(is.na(conflict_dummy), 0, conflict_dummy),
         ccode = ifelse(ccode == 260, 255, ccode)) %>% 
  filter(ccode > 2) %>%
  filter(year >= 1988) %>% 
  ungroup()

write_csv(protest.data.final, here::here("Data/Chapter-Protests", "protest-data-final.csv"), append = FALSE)

# Note: I've performed checks and the standardized variables and regular demeaned variables are all both either positive or negative. We do not see contradictory signs.