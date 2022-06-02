#### Numerator and Denominator 

p.data$numerator <- dnorm(x = p.data$log_troops_z, 
                          mean = predict(m.numerator, newdata = p.data, allow_new_levels = TRUE)[, 1], 
                          sd = sd(residuals(m.numerator, newdata = p.data, allow_new_levels = TRUE)[, 1], na.rm = TRUE))

p.data$denominator <- dnorm(x = p.data$log_troops_z, 
                            mean = predict(m.denominator, newdata = p.data, allow_new_levels = TRUE)[, 1], 
                            sd = sd(residuals(m.denominator, newdata = p.data, allow_new_levels = TRUE)[, 1], na.rm = TRUE))

p.data <- p.data %>% 
  mutate(iptw = numerator/denominator) %>% 
  group_by(ccode) %>% 
  mutate(iptw.prod = cumprod(replace(iptw, is.na(iptw), 1)),
         iptw.10 = ifelse(iptw.prod > 10, 10, iptw.prod),
         iptw.50 = ifelse(iptw.prod > 50, 50, iptw.prod),
         iptw.500 = ifelse(iptw.prod > 500, 500, iptw.prod),
         iptw.1000 = ifelse(iptw.prod > 1000, 1000, iptw.prod),
         iptw.5000 = ifelse(iptw.prod > 5000, 5000, iptw.prod),
         iptw.10000 = ifelse(iptw.prod > 10000, 10000, iptw.prod))



iptw.10.df <- p.data %>% dplyr::select(ccode, anti_us_protest, anti_us_mil, log_troops_z, log_troops_cumsum_z, iptw.10) 
iptw.50.df <- p.data %>% dplyr::select(ccode, anti_us_protest, anti_us_mil, log_troops_z, log_troops_cumsum_z, iptw.50) 
iptw.500.df <- p.data %>% dplyr::select(ccode, anti_us_protest, anti_us_mil, log_troops_z, log_troops_cumsum_z, iptw.500) 
iptw.1000.df <- p.data %>% dplyr::select(ccode, anti_us_protest, anti_us_mil, log_troops_z, log_troops_cumsum_z, iptw.1000) 
iptw.5000.df <- p.data %>% dplyr::select(ccode, anti_us_protest, anti_us_mil, log_troops_z, log_troops_cumsum_z, iptw.5000) 
iptw.10000.df <- p.data %>% dplyr::select(ccode, anti_us_protest, anti_us_mil, log_troops_z, log_troops_cumsum_z, iptw.10000) 
#iptw.100000.df <- p.data %>% dplyr::select(ccode, anti_us_protest, anti_us_mil, log_troops_z, log_troops_cumsum_z, iptw.100000) 

iptw.list <- list(iptw.10.df, iptw.50.df, iptw.500.df, iptw.1000.df, iptw.5000.df, iptw.10000.df)

iptw.list <- lapply(iptw.list, function(x) {setNames(x, gsub(".*iptw.*", "iptw", names(x)))})

save(iptw.list, file = here("Data/Chapter-Protests/treatment-weight.RData"))




# Anti-U.S. Protest Events
ate.10 <- us.ate.com %>% 
  gather_draws(b_log_troops_z,
               b_log_troops_cumsum_z,
               n = 10000) %>% 
  mutate(model = "1")

ate.50 <- us.ate.com[[2]] %>% 
  gather_draws(b_log_troops_z,
               b_log_troops_cumsum_z,
               n = 10000) %>% 
  mutate(model = "2")

ate.500 <- us.ate.com[[3]] %>% 
  gather_draws(b_log_troops_z,
               b_log_troops_cumsum_z,
               n = 10000) %>% 
  mutate(model = "3")

ate.1000 <- us.ate.com[[4]] %>% 
  gather_draws(b_log_troops_z,
               b_log_troops_cumsum_z,
               n = 10000) %>% 
  mutate(model = "4")

ate.5000 <- us.ate.com[[5]] %>% 
  gather_draws(b_log_troops_z,
               b_log_troops_cumsum_z,
               n = 10000) %>% 
  mutate(model = "5")

ate.10000 <- us.ate.com[[6]] %>% 
  gather_draws(b_log_troops_z,
               b_log_troops_cumsum_z,
               n = 10000) %>% 
  mutate(model = "6")




# Anti-U.S. Military Protest Events
mil.ate.10 <- mil.ate.com[[1]] %>% 
  gather_draws(b_log_troops_z,
               b_log_troops_cumsum_z,
               n = 10000) %>% 
  mutate(iptw = "10",
         model = "Anti-U.S. Military Protests")

mil.ate.50 <- mil.ate.com[[2]] %>% 
  gather_draws(b_log_troops_z,
               b_log_troops_cumsum_z,
               n = 10000) %>% 
  mutate(iptw = "50",
         model = "Anti-U.S. Military Protests")

mil.ate.500 <- mil.ate.com[[3]] %>% 
  gather_draws(b_log_troops_z,
               b_log_troops_cumsum_z,
               n = 10000) %>% 
  mutate(iptw = "500",
         model = "Anti-U.S. Military Protests")

mil.ate.1000 <- mil.ate.com[[4]] %>% 
  gather_draws(b_log_troops_z,
               b_log_troops_cumsum_z,
               n = 10000) %>% 
  mutate(iptw = "1000",
         model = "Anti-U.S. Military Protests")

mil.ate.5000 <- mil.ate.com[[5]] %>% 
  gather_draws(b_log_troops_z,
               b_log_troops_cumsum_z,
               n = 10000) %>% 
  mutate(iptw = "5000",
         model = "Anti-U.S. Military Protests")

mil.ate.10000 <- mil.ate.com[[6]] %>% 
  gather_draws(b_log_troops_z,
               b_log_troops_cumsum_z,
               n = 10000) %>% 
  mutate(iptw = "10000",
         model = "Anti-U.S. Military Protests")



ate.fig.com <- bind_rows(ate.10, ate.50, ate.500, ate.1000, ate.5000, ate.10000) %>% 
  mutate(varname = factor(.variable, levels = c("b_log_troops_z", "b_log_troops_cumsum_z"), labels = c("Contemporaneous Effect", "Treatment History")),
         group = paste(model, ", ", varname, sep = ""),
         .variable = "")

