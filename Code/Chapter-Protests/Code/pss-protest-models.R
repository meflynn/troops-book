


# Protest participation models

options(mc.cores = parallel::detectCores())
ncores = detectCores()
rstan_options(auto_write = TRUE)

vagueprior <- set_prior("normal(0, 20)", class = "b")
              

# Basic demographics              
m1.cat <- brm(protest.cat ~ gender + ed + age + income + ideology + relig + minority + (1 | country),
              data = pss.data,
              prior = vagueprior,
              iter = 2000,
              warmup = 1000,
              chains = 1,
              cores = ncores,
              seed = 66502,
              file = here("m1.cat.base"),
              family = categorical(link = "logit", refcat = "No"))


# Basic demographics + contact and benefits              
m2.cat <- brm(protest.cat ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + troops_crime_pers + troops_crime_nonpers + gender + ed + age + income + ideology + relig + minority + basescount + spend_toa_combined_w_log + (1 | country),
              data = pss.data,
              prior = vagueprior,
              iter = 2000,
              warmup = 1000,
              chains = 1,
              cores = ncores,
              seed = 66502,
              file = here("m2.cat.base"),
              family = categorical(link = "logit", refcat = "No"))



m1.coef <- m1.cat %>% 
  gather_draws(`b_.*`, regex = TRUE) %>% 
  median_qi(.width = c(0.90, 0.95)) %>% 
  mutate(outcome = ifelse(grepl(".*muNo.*", .variable), "No",
                          ifelse(grepl(".*muYes.*", .variable), "Yes",
                                 ifelse(grepl(".*muDKD.*", .variable), "Don't know/Decline to answer", NA))),
         outcome = factor(outcome, levels = c("Don't know/Decline to answer", "No", "Yes")),
         iv = ifelse(grepl(".*Female.*", .variable), "Gender: Female",
                     ifelse(grepl(".*NonMbinary.*", .variable), "Gender: Non-binary", 
                     ifelse(grepl(".*genderNone.*", .variable), "Gender: None of the above",
                     ifelse(grepl(".*ed.*", .variable), "Education", 
                     ifelse(grepl(".*25to34.*", .variable), "Age: 25-34",
                     ifelse(grepl(".*35to44.*", .variable), "Age: 35-44",
                     ifelse(grepl(".*45to54.*", .variable), "Age: 45-54",
                     ifelse(grepl(".*55to64.*", .variable), "Age: 55-64",
                     ifelse(grepl(".*Age65.*", .variable), "Age: 65 or older",
                     ifelse(grepl(".*income17.*", .variable), "Income: 17%-34%",
                     ifelse(grepl(".*income35.*", .variable), "Income: 35%-50%",
                     ifelse(grepl(".*income51.*", .variable), "Income: 51%-65%",
                     ifelse(grepl(".*income65.*", .variable), "Income: 66%-83%",
                     ifelse(grepl(".*income84.*", .variable), "Income: 84%-100%",
                     ifelse(grepl(".*ideology.*", .variable), "Ideology",
                     ifelse(grepl(".*Buddhism.*", .variable), "Religion: Buddhism",
                     ifelse(grepl(".*Cath.*", .variable), "Religion: Catholic",
                     ifelse(grepl(".*religDec.*", .variable), "Religion: Decline to answer",
                     ifelse(grepl(".*Hindu.*", .variable), "Religion: Hinduism",
                     ifelse(grepl(".*Islam.*", .variable), "Religion: Islam",
                     ifelse(grepl(".*Jud.*", .variable), "Religion: Judaism",
                            ifelse(grepl(".*Local.*", .variable), "Religion: Local Religion",
                                   ifelse(grepl(".*Morm.*", .variable), "Religion: Mormonism",
                                          ifelse(grepl(".*religOther.*", .variable), "Religion: Other",
                                                 ifelse(grepl(".*Protestant.*", .variable), "Religion: Protestant",
                                                        ifelse(grepl(".*Shinto.*", .variable), "Religion: Shinto",
                                                               ifelse(grepl(".*minorityNo.*", .variable), "Minority: No",
                                                                      ifelse(grepl(".*minorityYes.*", .variable), "Minority: Yes", NA)))))))))))))))))))))))))))),
         vargroup = ifelse(grepl(".*Religion.*", iv), "Religion",
                           ifelse(grepl(".*Gender.*", iv), "Gender",
                                  ifelse(grepl(".*Education", iv), "Ed.",
                                         ifelse(grepl(".*Age.*", iv), "Age",
                                                ifelse(grepl(".*Income.*", iv), "Income",
                                                       ifelse(grepl(".*Minority.*", iv), "Minority", 
                                                              ifelse(grepl(".*Ideology.*", iv), "Ide.", NA)))))))) %>%  
  filter(!is.na(iv))



# Basic demographic coefficients from model 1
ggplot(m1.coef %>% filter(.value > -10), aes(x = .value, y = iv, group = outcome, color = outcome)) +
  geom_pointintervalh(position = position_dodgev(height =0.75)) +
  geom_vline(xintercept = 0) +
  facet_grid(vargroup ~ ., scales = "free", switch = "y", space = "free_y") + 
  theme_bw() +
  theme(panel.spacing = unit(0.1, "lines"),
        strip.placement = "outside",
        strip.text = element_text(size = 9)) +
  scale_color_brewer(palette = "Set2", direction = -1) +
  labs(x = "Coefficient Value",
       y = "",
       title = "Coefficients from basic demographic model predicting protest involvement",
       color = "Response")

ggsave(here("Figures", "pss-figure-coefplot-demographics.png"))


m2.diag <- ggs(m2.cat)
ggmcmc(m2.diag)

# Contact, benefits, and crime questions

m2.coef <- m2.cat %>% 
  gather_draws(b_muYes_contact_persDontknowDDeclinetoanswer,        
               b_muYes_contact_persYes,       
               b_muYes_contact_nonpersDontknowDDeclinetoanswer,     
               b_muYes_contact_nonpersYes,                          
               b_muYes_benefit_persDontknowDDeclinetoanswer,        
               b_muYes_benefit_persYes,                             
               b_muYes_benefit_nonpersDontknowDDeclinetoanswer,     
               b_muYes_benefit_nonpersYes,                          
               b_muYes_troops_crime_persDontknowDDeclinetoanswer,   
               b_muYes_troops_crime_persYes,                        
               b_muYes_troops_crime_nonpersDontknowDDeclinetoanswer,
               b_muYes_troops_crime_nonpersYes,
               b_muDKD_contact_persDontknowDDeclinetoanswer,        
               b_muDKD_contact_persYes,                             
               b_muDKD_contact_nonpersDontknowDDeclinetoanswer,     
               b_muDKD_contact_nonpersYes,                          
               b_muDKD_benefit_persDontknowDDeclinetoanswer,        
               b_muDKD_benefit_persYes,                             
               b_muDKD_benefit_nonpersDontknowDDeclinetoanswer,     
               b_muDKD_benefit_nonpersYes,                          
               b_muDKD_troops_crime_persDontknowDDeclinetoanswer,   
               b_muDKD_troops_crime_persYes,                        
               b_muDKD_troops_crime_nonpersDontknowDDeclinetoanswer,
               b_muDKD_troops_crime_nonpersYes) %>% 
  median_qi(.width = c(0.90, 0.95)) %>% 
  mutate(outcome = ifelse(grepl(".*muNo.*", .variable), "No",
                          ifelse(grepl(".*muYes.*", .variable), "Yes",
                                 ifelse(grepl(".*muDKD.*", .variable), "Don't know/Decline to answer", NA))),
         outcome = factor(outcome, levels = c("Don't know/Decline to answer", "No", "Yes")),
         iv = ifelse(grepl(".*contact_pers.*", .variable), "Personal Contact",
                     ifelse(grepl(".*contact_nonpers.*", .variable), "Network Contact",
                            ifelse(grepl(".*benefit_pers.*", .variable), "Personal Benefit",
                                   ifelse(grepl(".*benefit_nonpers.*", .variable), "Network Benefit",
                                                ifelse(grepl(".*crime_pers.*", .variable), "Personal Crime",
                                                       ifelse(grepl(".*crime_nonpers.*", .variable), "Network Crime", NA)))))),
         iv = factor(iv, levels = c("Personal Contact", "Network Contact", "Personal Benefit", "Network Benefit", "Personal Crime", "Network Crime")),
         iv.response = ifelse(grepl(".*persYes.*", .variable), "Yes",
                              ifelse(grepl(".*persDont.*", .variable), "Don't know/Decline to answer", NA)),
         groupvar = ifelse(grepl(".*Benefit.*", iv), "Benefits",
                           ifelse(grepl(".*Contact.*", iv), "Contact",
                                  ifelse(grepl(".*Crime.*", iv), "Crime", NA))),)

         
# Coefficient plot from model 2 containing contact questions       
ggplot(m2.coef, aes(x = .value, y = paste(iv.response), color = outcome, group = outcome)) +
  geom_pointintervalh(position = position_dodgev(height = 0.25)) +
  geom_vline(xintercept = 0) +
  theme_bw() + 
  theme(panel.spacing = unit(0.15, "lines"),
        strip.placement = "outside") +
  facet_grid(iv ~ ., scales = "free", switch = "both") +
  scale_color_brewer(palette = "Set2", direction = -1) +
  labs(x = "Coefficient Value",
       y = "Independent Variable and Response",
       title = "Coefficients from contact, benefit, and crime questions",
       color = "Response")
         
ggsave(here("Figures", "pss-figure-coefplot-contact.png"))



                   