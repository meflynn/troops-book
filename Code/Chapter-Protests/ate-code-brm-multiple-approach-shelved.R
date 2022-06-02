
# Set baseline stuff for loops
# Set number of draws for the length of the simulations
draws <- 2000
# Set the iptw truncation points. Basically generating an expontential shaped distribution
# and sampling from that so we're not grabbing as many extreme values.
iptw.trunc <- 50
# Set the hurdle value. This is the proportion of 0s in the data.
hu.val <- round(length(p.data$troops[p.data.complete$troops==0])/length(p.data.complete$troops), 2)

# Add posterior draws for numerator
post.n <- posterior_epred(m.numerator, newdata = p.data.complete, allow_new_levels = TRUE, ndraws = draws) %>% 
  t() %>% 
  as_tibble(.names_repair = "universal") %>% 
  mutate(log_troops_z = p.data.complete$log_troops_z,
         across(starts_with("V"),
                ~(brms::dhurdle_lognormal(x = log_troops_z,
                                          mu = .x,
                                          sigma = sd((log_troops_z - .x), na.rm = TRUE),
                                          hu = hu.val)),
                .names = "num_{.col}")) %>% 
  dplyr::select(starts_with("num")) # Remove everything except the posterior draws

# Check dimensions. Should be ~5000 rows to match rows in data
dim(post.n)

# Add posterior draws for denominator
post.d <- posterior_epred(m.denominator, newdata = p.data.complete, allow_new_levels = TRUE, ndraws = draws) %>% 
  t() %>% 
  as_tibble(.names_repair = "universal") %>% 
  mutate(log_troops_z = p.data.complete$log_troops_z,
         across(starts_with("V"),
                ~(brms::dhurdle_lognormal(x = log_troops_z,
                                          mu = .x,
                                          sigma = sd((log_troops_z - .x), na.rm = TRUE),
                                          hu = hu.val)),
                .names = "den_{.col}")) %>% 
  dplyr::select(starts_with("den")) # Remove everything except the posterior draws

dim(post.d)


# Create the iptw scores by dividing the numerator values by the denominator values
iptw.mat <- post.n/post.d

# Create empty list
data.list <- list()

# Loop through the list to create separate data sets each with one column of iptw scores
i <- 1

for(i in 1:draws){
  data.list[[i]] <- p.data.complete
  data.list[[i]]$iptw <- iptw.mat[,i]
}

# Go through the list and cap the iptw scores so things don't get crazy with
# huge values
data.list <- lapply(data.list, function(x) {
  
  cap.val <- iptw.trunc # Can use sample(iptw.trunc, 1) if using a vector of iptw values above
  temp <- x %>% 
    mutate(iptw = ifelse(iptw > cap.val, cap.val, iptw)) %>% 
    dplyr::select(ccode, anti_us_protest, anti_us_mil, log_troops_z, log_troops_cumsum_z, iptw) 
}
)

save(data.list, file = here("Data/Chapter-Protests/treatment-weight-20211230.RData"))


