// generated with brms 2.16.3
functions {
  void add_iter(); // This is new for the code stuff
  int get_iter(); // This is also new for the custom code stuff
}
data {
  int<lower=1> N;  // total number of observations
  int Y[N];  // response variable
  // vector<lower=0>[N] weights;  // model weights
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  int prior_only;  // should the likelihood be ignored?
  int L; // Number of columns in the weight matrix NEW
  matrix[N, L] IPW; //weights matrix NEW
}
transformed data {
  int Kc = K - 1;
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // population-level effects
  real Intercept;  // temporary intercept for centered predictors
  real<lower=0> shape;  // shape parameter
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];  // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_1;  // actual group-level effects
  r_1_1 = (sd_1[1] * (z_1[1]));
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = Intercept + Xc * b;
    
    int M = get_iter(); // NEW get iteration
    vector[N] weights = IPW[, M]; // NEW Get the weights for the iteration
    
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += r_1_1[J_1[n]] * Z_1_1[n];
    }
    for (n in 1:N) {
      target += weights[n] * (neg_binomial_2_log_lpmf(Y[n] | mu[n], shape));
    }
  }
  // priors including constants
  target += student_t_lpdf(b[1] | 3.5, 0, 1);
  target += student_t_lpdf(b[2] | 3.5, 0, 2);
  target += student_t_lpdf(Intercept | 3.5, 0, 3);
  target += gamma_lpdf(shape | 0.01, 0.01);
  target += student_t_lpdf(sd_1 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_1[1]);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  
  add_iter(); // NEW update the counter each iteration

}
