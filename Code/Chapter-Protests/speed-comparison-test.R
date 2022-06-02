
path_to_opencl_lib <- "C:/Program Files/NVIDIA GPU Computing Toolkit/CUDA/v11.6/lib/x64"
cpp_options = list(
  paste0("LDFLAGS+= -L\"",path_to_opencl_lib,"\" -lOpenCL")
)
cmdstanr::cmdstan_make_local(cpp_options = cpp_options)
cmdstanr::rebuild_cmdstan()


mingw32-make[1]: Leaving directory 'C:/Users/flynn/Documents/.cmdstanr/cmdstan-2.28.2/stan/lib/stan_math/lib/tbb'

NOTE: Please add C:/Users/flynn/Documents/.cmdstanr/cmdstan-2.28.2/stan/lib/stan_math/lib/tbb to your PATH variable.

C:/Users/flynn/Documents/.cmdstanr/cmdstan-2.28.2/stan/lib/stan_math/lib/tbb

You may call

mingw32-make install-tbb

to automatically update your user configuration.
--- CmdStan v2.28.2 built ---


  
# Parallelize machine
options(mc.cores = parallel::detectCores())
ncores <- parallel::detectCores()
rstan_options(auto_write = TRUE)
VAGUEPRIOR <- c(set_prior("normal(0, 2)", class = "b"),
                set_prior("normal(0, 10)", class = "Intercept"))

ITER <- 10000
WARMUP <-  5000
CORES <- 4
CHAINS <- 4
THIN <- 1
SEED <- 66502


tic()
# Base model
us.1.test <- brm(anti_us_protest ~ log_troops_z +
              v2x_polyarchy_z +
              gdp_z + 
              gdp_growth_z +
              pop_z +
              protest_other_z +
              conflict_dummy +
              us_war + 
              us_war_w_mean +
              us_ally + 
              defburden_w_mean_z +
              anti_us_protest_w_mean_z + 
              (1 | ccode),
            data = p.data,
            iter = ITER,
            warmup = WARMUP,
            prior = VAGUEPRIOR,
            chains = CHAINS,
            cores = CORES,
            thin = THIN,
            seed = SEED,
            family = negbinomial(),
            control = list(adapt_delta = 0.85),
            backend = "cmdstanr")
m1time <- toc()


tic()
# Base model
us.1.test.2 <- brm(anti_us_protest ~ log_troops_z +
                   v2x_polyarchy_z +
                   gdp_z + 
                   gdp_growth_z +
                   pop_z +
                   protest_other_z +
                   conflict_dummy +
                   us_war + 
                   us_war_w_mean +
                   us_ally + 
                   defburden_w_mean_z +
                   anti_us_protest_w_mean_z + 
                   (1 | ccode),
                 data = p.data,
                 iter = ITER,
                 warmup = WARMUP,
                 prior = VAGUEPRIOR,
                 chains = CHAINS,
                 cores = 8,
                 threads = threading(2),
                 thin = THIN,
                 seed = SEED,
                 family = negbinomial(),
                 control = list(adapt_delta = 0.85),
                 backend = "cmdstanr")
m2time <- toc()


tic()
# Base model
us.1.test.3 <- brm(anti_us_protest ~ log_troops_z +
                     v2x_polyarchy_z +
                     gdp_z + 
                     gdp_growth_z +
                     pop_z +
                     protest_other_z +
                     conflict_dummy +
                     us_war + 
                     us_war_w_mean +
                     us_ally + 
                     defburden_w_mean_z +
                     anti_us_protest_w_mean_z + 
                     (1 | ccode),
                   data = p.data,
                   iter = ITER,
                   warmup = WARMUP,
                   prior = VAGUEPRIOR,
                   chains = CHAINS,
                   cores = 16,
                   threads = threading(4),
                   thin = THIN,
                   seed = SEED,
                   family = negbinomial(),
                   control = list(adapt_delta = 0.85),
                   backend = "cmdstanr")
m3time <- toc()


tic()
# Base model
us.1.test.4 <- brm(anti_us_protest ~ log_troops_z +
                     v2x_polyarchy_z +
                     gdp_z + 
                     gdp_growth_z +
                     pop_z +
                     protest_other_z +
                     conflict_dummy +
                     us_war + 
                     us_war_w_mean +
                     us_ally + 
                     defburden_w_mean_z +
                     anti_us_protest_w_mean_z + 
                     (1 | ccode),
                   data = p.data,
                   iter = ITER,
                   warmup = WARMUP,
                   prior = VAGUEPRIOR,
                   chains = CHAINS,
                   cores = 4,
                   opencl = opencl(c(0,0)),
                   thin = THIN,
                   seed = SEED,
                   family = negbinomial(),
                   control = list(adapt_delta = 0.85),
                   backend = "cmdstanr",
                   file = here::here("Output/Chapter-Protests/speed-test"))
m4time <- toc()