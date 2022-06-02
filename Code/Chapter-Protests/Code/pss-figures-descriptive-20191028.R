

# PSS 2019 Conference
# Descriptive statistics for protest responses

ggplot(data = pss.data %>% filter(!is.na(protest)), aes(y = (..count../sum(..count..)), x = country, fill = protest )) +
  geom_bar(position = "fill", color = "black", size = 0.1) +
  theme_bw() +
  coord_flip() +
  scale_fill_brewer(palette = "YlOrRd", direction = -1) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "",
       y = "Percent",
       fill = "Response",
       title = "Individual anti-US protest experience by country")

ggsave(here("Figures", "pss-figure-opinion-protest-country.png"))



# Protest and Polity scores
ggplot(data = pss.data %>% filter(!is.na(protest)), aes(y = ..count.., x = polity2,  fill = protest)) +
  geom_bar(color = "black", size = 0.1) +
  theme_bw() +
  scale_fill_brewer(palette = "YlOrRd", direction = -1) +
  scale_y_continuous(labels = comma_format()) +
  scale_x_continuous(breaks = seq(-10, 10, 2), limits = c(-11, 11)) +
  labs(x = "Polity",
       y = "Count",
       fill = "Response",
       title = "Individual anti-US protest attendance by Polity score")

ggsave(here("Figures", "pss-figure-opinion-protest-polity.png"))



# Protest and  Education (All protest values)
ggplot(data = pss.data %>% filter(!is.na(protest)), aes(y = ..count.., x = ed,  fill = protest)) +
  geom_bar(color = "black", size = 0.1) +
  theme_bw() +
  scale_fill_brewer(palette = "YlOrRd", direction = -1) +
  scale_y_continuous(labels = comma_format()) +
  scale_x_continuous(breaks = seq(0, 25, 5), limits = c(0, 25)) +
  labs(x = "Education",
       y = "Count",
       fill = "Response",
       title = "Individual anti-US protest attendance by years of education level")

ggsave(here("Figures", "pss-figure-opinion-protest-ed.png"))



# Protest and  Education (Only positive protest values)
ggplot(data = pss.data %>% filter(!is.na(protest) & protest != "Never" & protest != "Don't know/Decline to answer"), aes(y = ..count.., x = ed,  fill = protest)) +
  geom_bar(color = "black", size = 0.1) +
  theme_bw() +
  scale_fill_brewer(palette = "YlOrRd", direction = -1) +
  scale_y_continuous(labels = comma_format()) +
  scale_x_continuous(breaks = seq(0, 25, 5), limits = c(0, 25)) +
  labs(x = "Education",
       y = "Count",
       fill = "Response",
       title = "Individual anti-US protest attendance by years of education level")

ggsave(here("Figures", "pss-figure-opinion-protest-ed-nonzero.png"))



# Protest and  Income (Count)
ggplot(data = pss.data %>% filter(!is.na(protest)), aes(y = (..count..), x = income,  fill = protest)) +
  geom_bar(color = "black", size = 0.1) +
  theme_bw() +
  coord_flip() +
  scale_fill_brewer(palette = "YlOrRd", direction = -1) +
  scale_y_continuous(labels = comma_format()) +
  labs(x = "Income Quantile",
       y = "Count",
       fill = "Response",
       title = "Individual anti-US protest attendance by income quantile")

ggsave(here("Figures", "pss-figure-opinion-protest-income.png"))



# Protest and  Income (Percentage)
ggplot(data = pss.data %>% filter(!is.na(protest)), aes(y = (..count../sum(..count..)), x = income,  fill = protest)) +
  geom_bar(position = "fill", color = "black", size = 0.1) +
  theme_bw() +
  coord_flip() +
  scale_fill_brewer(palette = "YlOrRd", direction = -1) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Income Quantile",
       y = "Percent",
       fill = "Response",
       title = "Individual anti-US protest attendance by income quantile")

ggsave(here("Figures", "pss-figure-opinion-protest-income-percent.png"))



# Protest attendance and individual ideology
ggplot(data = pss.data %>% filter(!is.na(protest)), aes(y = (..count..), x = ideology, fill = protest)) +
  geom_bar(color = "black", size = 0.1) +
  theme_bw() + 
  scale_fill_brewer(palette = "YlOrRd", direction = -1) +
  scale_y_continuous(labels = comma_format()) +
  scale_x_continuous(breaks = seq(1, 10, 1), limits = c(0.5, 10.5)) +
  labs(x = "Ideology",
       y = "Count",
       fill = "Response",
       title = "Individual anti-US protest attendance by ideological self-placement")

ggsave(here("Figures", "pss-figure-opinion-protest-ideology.png"))



# Protest attendance and individual ideology (Omit bottom categories)
ggplot(data = pss.data %>% filter(!is.na(protest) & protest != "Never" & protest != "Don't know/Decline to answer"), aes(y = (..count..), x = ideology, fill = protest)) +
  geom_bar(color = "black", size = 0.1) +
  theme_bw() + 
  scale_fill_brewer(palette = "YlOrRd", direction = -1) +
  scale_y_continuous(labels = comma_format()) +
  scale_x_continuous(breaks = seq(1, 10, 1), limits = c(0.5, 10.5)) +
  labs(x = "Ideology",
       y = "Count",
       fill = "Response",
       title = "Individual anti-US protest attendance by ideological self-placement")

ggsave(here("Figures", "pss-figure-opinion-protest-ideology-nonzero.png"))




# Country protest data

# Troops data from 2017
troops.data <- readstata13::read.dta13(here("Data", "troops-update-2014.dta")) %>% 
  filter(year >= 1990 & ccode != 2)

# Polity data
polity.data <- read.csv(here("Data", "p4v2017.csv")) %>% 
  filter(ccode != 2 & year >= 1990)


# Country-year protest data
pss.data.protest.cy <- readstata13::read.dta13(here("Data", "WorldProtestsMASTER.dta")) %>% 
  group_by(ccode, year) %>% 
  summarise(protestcount = sum(protestcount, na.rm = TRUE),
            milprotests = sum(milprotests, na.rm = TRUE),
            antiusprotests = sum(antiusprotests, na.rm = TRUE)) %>%
  mutate(perc.anti.us = antiusprotests/protestcount,
         perc.mil = milprotests/protestcount,
         perc.mil.anti.us = milprotests/antiusprotests) %>% 
  left_join(troops.data) %>% 
  left_join(polity.data)


# Overall distribution of protest types
ggplot(pss.data.protest.cy) +
  geom_density(aes(x = milprotests, fill = "blue"), alpha = .3) +
  geom_density(aes(x = antiusprotests, fill = "red"), alpha = .3) +
  theme_bw() +
  scale_fill_manual(values = c("blue", "red"), labels = c("Military Protests", "Anti-US Protests")) +
  labs(x = "Count",
       y = "Density",
       fill = "Protest Type",
       title = "Distribution of country-year protest counts by type, 1990-2018")

ggsave(here("Figures", "pss-figure-protestdensity-1990-2018.png"))




# US troops and Anti-US protests
ggplot(pss.data.protest.cy, aes(x = troops, y = antiusprotests, color = (ccode==645))) +
  geom_jitter(width = 2, height = 0.5, alpha = .6, size = 3) +
  theme_bw() +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(labels = comma_format()) +
  labs(x = "Total US Military Personnel",
       y = "Count",
       title = "Anti-US Protest Events and US Military Deployments, 1990-2014")

ggsave(here("Figures", "pss-figure-protest-troops-1990-2014.png"))



# US troops and Anti-US protests (Logged)
ggplot(pss.data.protest.cy, aes(x = log(troops), y = antiusprotests, color = (ccode==645))) +
  geom_jitter(width = 0.0, height = 0.5, alpha = .6, size = 3) +
  theme_bw() +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Set1")
  labs(x = "Total US Military Personnel",
       y = "Count",
       title = "Anti-US Protest Events and US Military Deployments, 1990-2014")

ggsave(here("Figures", "pss-figure-protest-troops-logged-1990-2014.png"))



# Anti-US protests and Polity Scores
ggplot(pss.data.protest.cy, aes(x = polity2, y = antiusprotests)) +
  geom_jitter(width = 2, height = 0.5, alpha = .5, size = 3) +
  theme_bw() +
  scale_x_continuous() +
  labs(x = "Polity Score",
       y = "Count",
       title = "Anti-US Protest Events and Polity Scores, 1990-2017")

ggsave(here("Figures", "pss-figure-protest-polity-1990-2017.png"))




pss.data.protest.c <- pss.data.protest.cy %>% 
  group_by(ccode, country) %>% 
  summarise(antiusprotests = sum(antiusprotests, na.rm = TRUE),
            milprotests = sum(milprotests, na.rm = TRUE))

# Top countries by anti-US protests
ggplot(pss.data.protest.c %>% filter(antiusprotests > 5), aes(x = reorder(country, antiusprotests), y = antiusprotests)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_bw() +
  labs(x = "Country",
       y = "Count",
       title = "Total anti-US protest events by country, 1990-2018",
       caption = "Figure excludes countries with fewer than five recorded protest events.")

ggsave(here("Figures", "pss-figure-protest-antius-top-countries.png"))



# Top countries by anti-military protests
ggplot(pss.data.protest.c %>% filter(milprotests > 0), aes(x = reorder(country, milprotests), y = milprotests)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 9, 1)) +
  labs(x = "Country",
       y = "Count",
       title = "Total anti-military protest events by country, 1990-2018",
       caption = "Figure excludes countries with fewer than five recorded protest events.")

ggsave(here("Figures", "pss-figure-protest-antimil-top-countries.png"))




# Protest events by year
pss.data.protest <- readstata13::read.dta13(here("Data", "WorldProtestsMASTER.dta")) %>% 
  group_by(year) %>% 
  summarise(protestcount = sum(protestcount, na.rm = TRUE),
            milprotests = sum(milprotests, na.rm = TRUE),
            antiusprotests = sum(antiusprotests, na.rm = TRUE)) %>%
  mutate(perc.anti.us = antiusprotests/protestcount,
         perc.mil = milprotests/protestcount,
         perc.mil.anti.us = milprotests/antiusprotests) %>% 
pivot_longer(cols = 2:7) 


# All protest types
ggplot(pss.data.protest %>% filter(name == "protestcount"), aes(x = year, y = value)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1990, 2018, 4)) +
  labs(x = "Year",
       y = "Count",
       title = "Global protest event count, 1990-2018")

ggsave(here("Figures", "pss-figure-protest-count-all.png"))




# Anti-US Protests
ggplot(pss.data.protest %>% filter(name == "antiusprotests"), aes(x = year, y = value)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1990, 2018, 4)) +
  labs(x = "Year",
       y = "Count",
       title = "Global anti-US protest event count, 1990-2018")

ggsave(here("Figures", "pss-figure-protest-count-antius.png"))



# Anti-US Protests and Total Protests
ggplot(pss.data.protest %>% filter(name == "antiusprotests" | name == "protestcount"), aes(x = year, y = value, fill = name)) +
  geom_col(color = "black", position = position_dodge(width = .0), size = 0.1, alpha = 1) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1990, 2018, 4)) +
  scale_fill_brewer(palette = "Set2", labels = c("Anti-US Protests", "All Protests")) +
  labs(x = "Year",
       y = "Count",
       title = "Total global protest event and anti-US protest event counts, 1990-2018",
       fill = "Protest Type")

ggsave(here("Figures", "pss-figure-protest-count-antius-overlay.png"))




# Adds image to each row of the data frame. 
pss.data.protest$image <- here("Figures", "mericafile.png")
pss.data.protest$image2 <- here("Figures", "eddie-trooper.png")

# Anti-US protests as percentage of all protests
ggplot(pss.data.protest %>% filter(name == "perc.anti.us"), aes(x = year, y = value)) +
  geom_line(size = 1) +
  geom_image(aes(image = image), size = 0.18) +
  theme_bw() +
  scale_y_continuous(breaks = seq(.05, .45, .05), limits = c(.05, .45), labels = percent_format()) +
  scale_x_continuous(breaks = seq(1990, 2018, 4)) +
  labs(x = "Date",
       y = "Percent",
       title = "Anti-US protest events as a percentage of all protest events, 1990-2018")

ggsave(here("Figures", "pss-figure-protest-count-antius-percent-image.png"))





# Total anti US Military 
ggplot(pss.data.protest %>% filter(name == "milprotests"), aes(x = year, y = value)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0), color = "black", size = 0.1) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 25, 2)) +
  scale_x_continuous(breaks = seq(1990, 2018, 4)) +
  scale_fill_brewer(palette = "Set2", labels = c("Anti-US Protests", "All Protests")) +
  labs(x = "Date",
       y = "Count",
       title = "Anti-US military protest events, 1990-2018")

ggsave(here("Figures", "pss-figure-protest-count-antimilitary-count-solo.png"))



# Total anti US Military protests and
# Anti-US protests as percentage of all protests
ggplot(pss.data.protest %>% filter(name == "milprotests" | name == "protestcount"), aes(x = year, y = value, fill = name)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0), color = "black", size = 0.1) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 250, 25)) +
  scale_x_continuous(breaks = seq(1990, 2018, 4)) +
  scale_fill_brewer(palette = "Set2", labels = c("Anti-US Protests", "All Protests")) +
  labs(x = "Date",
       y = "Count",
       title = "All protest events and anti-US military protest events, 1990-2018",
       fill = "Protest type")

ggsave(here("Figures", "pss-figure-protest-count-antimilitary.png"))




# Anti-US Military protests as percent of all anti-US protests
ggplot(pss.data.protest %>% filter(name == "perc.mil.anti.us"), aes(x = year, y = value)) +
  geom_line() +
  geom_point(size = 2) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1990, 2018, 4)) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Year",
       y = "Percent",
       title = "Anti-US military protests as percent of all anti-US protests, 1990-2018")

ggsave(here("Figures", "pss-figure-protest-anti-us-anti-military-percent.png"))







# Map of protest responses
# Global map showing all anti-US protests

pss.data.map <- readstata13::read.dta13(here("Data", "WorldProtestsMASTER.dta")) %>% 
  group_by(ccode, stateabb) %>% 
  summarise(protestcount = sum(protestcount, na.rm = TRUE),
            milprotests = sum(milprotests, na.rm = TRUE),
            antiusprotests = sum(antiusprotests, na.rm = TRUE))

map.data <- map_data("world") %>% 
  mutate(ccode = countrycode(region, "country.name",  "cown")) %>% 
  left_join(pss.data.map) %>% 
  filter(region != "Antarctica")


# Map of anti-US protests for 1990-2018
ggplot() +
  geom_map(data = map.data, map = map.data, aes(x = long, y = lat, group = group, map_id = region, fill = antiusprotests), color = "black", size = 0.01) +
  theme_void() +
  coord_fixed(ratio = 1.3) +
  scale_fill_distiller(type = "seq", palette = 3, direction = 1) +
  labs(title = "Total Anti-US Protest Events by Country, 1990-2018",
       fill = "Protest count")

ggsave(here("Figures", "pss-figure-map-anti-us-1990-2018.png"))


# Map of anti-US protests for 1990-2018
ggplot() +
  geom_map(data = map.data, map = map.data, aes(x = long, y = lat, group = group, map_id = region, fill = milprotests), color = "black", size = 0.01) +
  theme_void() +
  coord_fixed(ratio = 1.3) +
  scale_fill_distiller(type = "seq", palette = 3, direction = 1) +
  labs(title = "Total Anti-US Military Protest Events by Country, 1990-2018",
       fill = "Protest count")

ggsave(here("Figures", "pss-figure-map-anti-mil-1990-2018.png"))



# Map of anti-US protests from 2016-2018
# Global map showing all anti-US protests

pss.data.map.recent <- readstata13::read.dta13(here("Data", "WorldProtestsMASTER.dta")) %>% 
  filter(year >= 2016) %>% 
  group_by(ccode, stateabb) %>% 
  summarise(protestcount = sum(protestcount, na.rm = TRUE),
            milprotests = sum(milprotests, na.rm = TRUE),
            antiusprotests = sum(antiusprotests, na.rm = TRUE))

map.data.recent <- map_data("world") %>% 
  mutate(ccode = countrycode(region, "country.name",  "cown")) %>% 
  left_join(pss.data.map.recent) %>% 
  filter(region != "Antarctica")


# Map of anti-US protests for 2016-2018
ggplot() +
  geom_map(data = map.data.recent, map = map.data.recent, aes(x = long, y = lat, group = group, map_id = region, fill = antiusprotests), color = "black", size = 0.01) +
  theme_void() +
  coord_fixed(ratio = 1.3) +
  scale_fill_distiller(type = "seq", palette = 3, direction = 1) +
  labs(title = "Total Anti-US Protest Events by Country, 2016-2018",
       fill = "Protest count")

ggsave(here("Figures", "pss-figure-map-anti-us-2016-2018.png"))



# Note: No observations for this time range
# Map of anti-US military protests for 2016-2018
ggplot() +
  geom_map(data = map.data.recent, map = map.data.recent, aes(x = long, y = lat, group = group, map_id = region, fill = milprotests), color = "black", size = 0.01) +
  theme_void() +
  coord_fixed(ratio = 1.3) +
  scale_fill_distiller(type = "seq", palette = 3, direction = 1) +
  labs(title = "Total Anti-US Military Protest Events by Country, 2016-2018",
       fill = "Protest count")

ggsave(here("Figures", "pss-figure-map-anti-mil-2016-2018.png"))

