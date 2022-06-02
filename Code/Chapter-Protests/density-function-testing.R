ITER = 5000
WARMUP = 2500
CHAINS = 4
CORES = 4
SEED = 66502

PRIOR <- set_prior("normal(0,2)", class = "b")

tic()

m.denominator <-  brm(bf(troops ~ idealdistance_2_z + us_ally + gdp_z + gdp_growth_z + pop_z + log(protest_other+1) + anti_us_protest_lag + lag(conflict_dummy) + v2x_polyarchy_z + us_war + (1 | ccode),
                         hu ~ region + us_ally + idealdistance_2_z + (1 | ccode)),
                      data = p.data,
                      family = hurdle_gamma(link = "log"),
                      iter = ITER,
                      warmup = WARMUP,
                      chains = CHAINS,
                      cores = CORES,
                      seed = SEED,
                      prior = PRIOR,
                      control = list(adapt_delta = 0.80,
                                     max_treedepth = 12),
                      backend = "cmdstanr")

toc()

check <- brms::pp_check(m.denominator)

check +
  scale_x_continuous(trans = "log1p")

tic()

m.denominator2 <-  brm(bf(troops ~ idealdistance_2_z + us_ally + gdp_z + gdp_growth_z + pop_z + log(protest_other+1) + anti_us_protest_lag + lag(conflict_dummy) + v2x_polyarchy_z + us_war + (1 | ccode),
                         hu ~ region + us_ally + idealdistance_2_z + gdp_z + pop_z + (1 | ccode)),
                      data = p.data,
                      family = hurdle_negbinomial(link = "log"),
                      iter = ITER,
                      warmup = WARMUP,
                      chains = CHAINS,
                      cores = CORES,
                      seed = SEED,
                      prior = PRIOR,
                      control = list(adapt_delta = 0.80,
                                     max_treedepth = 12),
                      backend = "cmdstanr")

toc()

check2 <- brms::pp_check(m.denominator2) 

check2 +
  scale_x_continuous(trans = "log1p")


sims <- 1e4
muval <- 10
thetaval <- 1
pival <- 0.79

simvals <- rhnbinom(sims, mu = muval, theta = thetaval, pi = pival)
hist(simvals, breaks = 100)

x <- dnbinom(10, mu = muval, size = thetaval, log = TRUE)
x2 <- pnbinom(0, mu = muval, size = thetaval, log.p = TRUE)

outcome <- x - x2 + log(pival)
print(exp(outcome))

sum(simvals>10)/1e4



# log normal

sims <- 1e4
muval = 2.8
sdval = 2.54
pival = 0.2


simvals <- rep(NA, sims)
simvals[c(1:2000)] <- rep(0, sims*0.2)
simvals[c(2001:10000)] <- rlnorm(sims*0.8, meanlog = muval, sdlog = sdval)

hist(log1p(simvals), breaks = 200)

dhlnorm <- function(x, meanlog, sdlog, pval) {
  
    if (x > 0) {
    
    value <- dlnorm(x, meanlog = meanlog, sdlog = sdlog, log = FALSE) * (1-pval)
    
    return(value)
    
  } else {
    
    value <- pval
    
    return(value)
  }
  }


dhlnorm(2, meanlog = muval, sdlog = sdval, pval = 0.2)

print(exp(outcome))

dlnorm(8, meanlog = 3, sdlog = sdval)

