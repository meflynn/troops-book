// generated with brms 2.16.3
functions {
 /* compute correlated group-level effects
  * Args: 
  *   z: matrix of unscaled group-level effects
  *   SD: vector of standard deviation parameters
  *   L: cholesky factor correlation matrix
  * Returns: 
  *   matrix of scaled group-level effects
  */ 
  matrix scale_r_cor(matrix z, vector SD, matrix L) {
    // r is stored in another dimension order than z
    return transpose(diag_pre_multiply(SD, L) * z);
  }
  /* integer sequence of values
   * Args: 
   *   start: starting integer
   *   end: ending integer
   * Returns: 
   *   an integer sequence from start to end
   */ 
  int[] sequence(int start, int end) { 
    int seq[end - start + 1];
    for (n in 1:num_elements(seq)) {
      seq[n] = n + start - 1;
    }
    return seq; 
  } 
  // compute partial sums of the log-likelihood
  real partial_log_lik_lpmf(int[] seq, int start, int end, data int ncat, data int[] Y, data matrix Xc_mudk, vector b_mudk, real Intercept_mudk, data matrix Xc_muneg, vector b_muneg, real Intercept_muneg, data matrix Xc_mupos, vector b_mupos, real Intercept_mupos, data int[] J_1, data vector Z_1_mudk_1, data vector Z_1_muneg_2, data vector Z_1_mupos_3, vector r_1_mudk_1, vector r_1_muneg_2, vector r_1_mupos_3) {
    real ptarget = 0;
    int N = end - start + 1;
    // initialize linear predictor term
    vector[N] mudk = Intercept_mudk + Xc_mudk[start:end] * b_mudk;
    // initialize linear predictor term
    vector[N] muneg = Intercept_muneg + Xc_muneg[start:end] * b_muneg;
    // initialize linear predictor term
    vector[N] mupos = Intercept_mupos + Xc_mupos[start:end] * b_mupos;
    // linear predictor matrix
    vector[ncat] mu[N];
    for (n in 1:N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      mudk[n] += r_1_mudk_1[J_1[nn]] * Z_1_mudk_1[nn];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      muneg[n] += r_1_muneg_2[J_1[nn]] * Z_1_muneg_2[nn];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      mupos[n] += r_1_mupos_3[J_1[nn]] * Z_1_mupos_3[nn];
    }
    for (n in 1:N) {
      mu[n] = transpose([mudk[n], muneg[n], 0, mupos[n]]);
    }
    for (n in 1:N) {
      int nn = n + start - 1;
      ptarget += categorical_logit_lpmf(Y[nn] | mu[n]);
    }
    return ptarget;
  }
}
data {
  int<lower=1> N;  // total number of observations
  int<lower=2> ncat;  // number of categories
  int Y[N];  // response variable
  int<lower=1> K_mudk;  // number of population-level effects
  matrix[N, K_mudk] X_mudk;  // population-level design matrix
  int<lower=1> K_muneg;  // number of population-level effects
  matrix[N, K_muneg] X_muneg;  // population-level design matrix
  int<lower=1> K_mupos;  // number of population-level effects
  matrix[N, K_mupos] X_mupos;  // population-level design matrix
  int grainsize;  // grainsize for threading
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_mudk_1;
  vector[N] Z_1_muneg_2;
  vector[N] Z_1_mupos_3;
  int<lower=1> NC_1;  // number of group-level correlations
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  int Kc_mudk = K_mudk - 1;
  matrix[N, Kc_mudk] Xc_mudk;  // centered version of X_mudk without an intercept
  vector[Kc_mudk] means_X_mudk;  // column means of X_mudk before centering
  int Kc_muneg = K_muneg - 1;
  matrix[N, Kc_muneg] Xc_muneg;  // centered version of X_muneg without an intercept
  vector[Kc_muneg] means_X_muneg;  // column means of X_muneg before centering
  int Kc_mupos = K_mupos - 1;
  matrix[N, Kc_mupos] Xc_mupos;  // centered version of X_mupos without an intercept
  vector[Kc_mupos] means_X_mupos;  // column means of X_mupos before centering
  int seq[N] = sequence(1, N);
  for (i in 2:K_mudk) {
    means_X_mudk[i - 1] = mean(X_mudk[, i]);
    Xc_mudk[, i - 1] = X_mudk[, i] - means_X_mudk[i - 1];
  }
  for (i in 2:K_muneg) {
    means_X_muneg[i - 1] = mean(X_muneg[, i]);
    Xc_muneg[, i - 1] = X_muneg[, i] - means_X_muneg[i - 1];
  }
  for (i in 2:K_mupos) {
    means_X_mupos[i - 1] = mean(X_mupos[, i]);
    Xc_mupos[, i - 1] = X_mupos[, i] - means_X_mupos[i - 1];
  }
}
parameters {
  vector[Kc_mudk] b_mudk;  // population-level effects
  real Intercept_mudk;  // temporary intercept for centered predictors
  vector[Kc_muneg] b_muneg;  // population-level effects
  real Intercept_muneg;  // temporary intercept for centered predictors
  vector[Kc_mupos] b_mupos;  // population-level effects
  real Intercept_mupos;  // temporary intercept for centered predictors
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  matrix[M_1, N_1] z_1;  // standardized group-level effects
  cholesky_factor_corr[M_1] L_1;  // cholesky factor of correlation matrix
}
transformed parameters {
  matrix[N_1, M_1] r_1;  // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_1] r_1_mudk_1;
  vector[N_1] r_1_muneg_2;
  vector[N_1] r_1_mupos_3;
  // compute actual group-level effects
  r_1 = scale_r_cor(z_1, sd_1, L_1);
  r_1_mudk_1 = r_1[, 1];
  r_1_muneg_2 = r_1[, 2];
  r_1_mupos_3 = r_1[, 3];
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, ncat, Y, Xc_mudk, b_mudk, Intercept_mudk, Xc_muneg, b_muneg, Intercept_muneg, Xc_mupos, b_mupos, Intercept_mupos, J_1, Z_1_mudk_1, Z_1_muneg_2, Z_1_mupos_3, r_1_mudk_1, r_1_muneg_2, r_1_mupos_3);
  }
  // priors including constants
  target += normal_lpdf(b_mudk[1] | 0.56,0.29);
  target += normal_lpdf(b_mudk[2] | -0.71,0.37);
  target += normal_lpdf(b_mudk[3] | -0.7,0.28);
  target += normal_lpdf(b_mudk[4] | -0.66,0.31);
  target += normal_lpdf(b_mudk[5] | 0.44,0.24);
  target += normal_lpdf(b_mudk[6] | -0.43,0.43);
  target += normal_lpdf(b_mudk[7] | 0.34,0.23);
  target += normal_lpdf(b_mudk[8] | -0.47,0.46);
  target += normal_lpdf(b_mudk[9] | -0.16,0.17);
  target += normal_lpdf(b_mudk[10] | -0.41,0.17);
  target += normal_lpdf(b_mudk[11] | -0.36,0.18);
  target += normal_lpdf(b_mudk[12] | -0.75,0.19);
  target += normal_lpdf(b_mudk[13] | -0.98,0.23);
  target += normal_lpdf(b_mudk[14] | 0,2);
  target += normal_lpdf(b_mudk[15] | 0,2);
  target += normal_lpdf(b_mudk[16] | 0,2);
  target += normal_lpdf(b_mudk[17] | 0,2);
  target += normal_lpdf(b_mudk[18] | 0,2);
  target += normal_lpdf(b_mudk[19] | 0,2);
  target += normal_lpdf(b_mudk[20] | 0.08,0.11);
  target += normal_lpdf(b_mudk[21] | -79.98,60.28);
  target += normal_lpdf(b_mudk[22] | 0.08,0.98);
  target += normal_lpdf(b_mudk[23] | -0.28,0.28);
  target += normal_lpdf(b_mudk[24] | 0,2);
  target += normal_lpdf(b_mudk[25] | 0.06,0.39);
  target += normal_lpdf(b_mudk[26] | 0.25,0.19);
  target += normal_lpdf(b_mudk[27] | 0,2);
  target += normal_lpdf(b_mudk[28] | 0.07,0.22);
  target += normal_lpdf(b_mudk[29] | 0.24,0.73);
  target += normal_lpdf(b_mudk[30] | 0.21,0.31);
  target += normal_lpdf(b_mudk[31] | 1.05,0.61);
  target += normal_lpdf(b_mudk[32] | -0.66,0.69);
  target += normal_lpdf(b_mudk[33] | 0,2);
  target += normal_lpdf(b_mudk[34] | -80.74,61.19);
  target += normal_lpdf(b_mudk[35] | -0.02,0.22);
  target += normal_lpdf(b_mudk[36] | 0.09,0.21);
  target += normal_lpdf(b_mudk[37] | -79.93,60.96);
  target += normal_lpdf(b_mudk[38] | 0,2);
  target += normal_lpdf(b_mudk[39] | 0,2);
  target += normal_lpdf(b_mudk[40] | 0.55,0.32);
  target += normal_lpdf(b_mudk[41] | -0.56,0.3);
  target += normal_lpdf(b_mudk[42] | -0.13,0.29);
  target += normal_lpdf(b_mudk[43] | -0.25,0.3);
  target += normal_lpdf(b_mudk[44] | 2.12,0.18);
  target += normal_lpdf(b_mudk[45] | 0.64,0.28);
  target += normal_lpdf(b_mudk[46] | 0.33,0.15);
  target += normal_lpdf(b_mudk[47] | 0.24,0.18);
  target += normal_lpdf(b_mudk[48] | 0.48,0.41);
  target += normal_lpdf(b_mudk[49] | 0,2);
  target += normal_lpdf(b_mudk[50] | 0,2);
  target += normal_lpdf(b_mudk[51] | 0,2);
  target += normal_lpdf(b_mudk[52] | 0,2);
  target += normal_lpdf(Intercept_mudk | 0,3);
  target += normal_lpdf(b_muneg[1] | -0.1,0.22);
  target += normal_lpdf(b_muneg[2] | 0.25,0.12);
  target += normal_lpdf(b_muneg[3] | 0.04,0.16);
  target += normal_lpdf(b_muneg[4] | 0.16,0.12);
  target += normal_lpdf(b_muneg[5] | -0.32,0.2);
  target += normal_lpdf(b_muneg[6] | -0.44,0.19);
  target += normal_lpdf(b_muneg[7] | -0.38,0.18);
  target += normal_lpdf(b_muneg[8] | -0.46,0.18);
  target += normal_lpdf(b_muneg[9] | 0.21,0.1);
  target += normal_lpdf(b_muneg[10] | 0.03,0.1);
  target += normal_lpdf(b_muneg[11] | -0.19,0.1);
  target += normal_lpdf(b_muneg[12] | -0.2,0.1);
  target += normal_lpdf(b_muneg[13] | -0.08,0.12);
  target += normal_lpdf(b_muneg[14] | 0,2);
  target += normal_lpdf(b_muneg[15] | 0,2);
  target += normal_lpdf(b_muneg[16] | 0,2);
  target += normal_lpdf(b_muneg[17] | 0,2);
  target += normal_lpdf(b_muneg[18] | 0,2);
  target += normal_lpdf(b_muneg[19] | 0,2);
  target += normal_lpdf(b_muneg[20] | -0.1,0.06);
  target += normal_lpdf(b_muneg[21] | -0.62,0.76);
  target += normal_lpdf(b_muneg[22] | -0.37,0.76);
  target += normal_lpdf(b_muneg[23] | 0.1,0.18);
  target += normal_lpdf(b_muneg[24] | 0,2);
  target += normal_lpdf(b_muneg[25] | -0.52,0.16);
  target += normal_lpdf(b_muneg[26] | -0.45,0.1);
  target += normal_lpdf(b_muneg[27] | 0,2);
  target += normal_lpdf(b_muneg[28] | -0.33,0.12);
  target += normal_lpdf(b_muneg[29] | -0.21,0.47);
  target += normal_lpdf(b_muneg[30] | 0.34,0.18);
  target += normal_lpdf(b_muneg[31] | -0.64,0.61);
  target += normal_lpdf(b_muneg[32] | -0.36,0.3);
  target += normal_lpdf(b_muneg[33] | 0,2);
  target += normal_lpdf(b_muneg[34] | 0.02,0.84);
  target += normal_lpdf(b_muneg[35] | -0.15,0.11);
  target += normal_lpdf(b_muneg[36] | -0.41,0.11);
  target += normal_lpdf(b_muneg[37] | -0.39,0.49);
  target += normal_lpdf(b_muneg[38] | 0,2);
  target += normal_lpdf(b_muneg[39] | 0,2);
  target += normal_lpdf(b_muneg[40] | -0.99,0.25);
  target += normal_lpdf(b_muneg[41] | -0.51,0.18);
  target += normal_lpdf(b_muneg[42] | -0.34,0.17);
  target += normal_lpdf(b_muneg[43] | -0.04,0.17);
  target += normal_lpdf(b_muneg[44] | 0.42,0.17);
  target += normal_lpdf(b_muneg[45] | 1.96,0.13);
  target += normal_lpdf(b_muneg[46] | 1.15,0.07);
  target += normal_lpdf(b_muneg[47] | -0.28,0.1);
  target += normal_lpdf(b_muneg[48] | -0.2,0.24);
  target += normal_lpdf(b_muneg[49] | 0,2);
  target += normal_lpdf(b_muneg[50] | 0,2);
  target += normal_lpdf(b_muneg[51] | 0,2);
  target += normal_lpdf(b_muneg[52] | 0,2);
  target += normal_lpdf(Intercept_muneg | 0,3);
  target += normal_lpdf(b_mupos[1] | -0.4,0.2);
  target += normal_lpdf(b_mupos[2] | 0.58,0.1);
  target += normal_lpdf(b_mupos[3] | 0.13,0.13);
  target += normal_lpdf(b_mupos[4] | 0.21,0.09);
  target += normal_lpdf(b_mupos[5] | -0.26,0.17);
  target += normal_lpdf(b_mupos[6] | -0.09,0.13);
  target += normal_lpdf(b_mupos[7] | -0.2,0.15);
  target += normal_lpdf(b_mupos[8] | 0.54,0.12);
  target += normal_lpdf(b_mupos[9] | -0.08,0.09);
  target += normal_lpdf(b_mupos[10] | -0.06,0.09);
  target += normal_lpdf(b_mupos[11] | -0.15,0.09);
  target += normal_lpdf(b_mupos[12] | 0.06,0.09);
  target += normal_lpdf(b_mupos[13] | 0.22,0.1);
  target += normal_lpdf(b_mupos[14] | 0,2);
  target += normal_lpdf(b_mupos[15] | 0,2);
  target += normal_lpdf(b_mupos[16] | 0,2);
  target += normal_lpdf(b_mupos[17] | 0,2);
  target += normal_lpdf(b_mupos[18] | 0,2);
  target += normal_lpdf(b_mupos[19] | 0,2);
  target += normal_lpdf(b_mupos[20] | -0.05,0.05);
  target += normal_lpdf(b_mupos[21] | -0.41,0.41);
  target += normal_lpdf(b_mupos[22] | -1.46,0.7);
  target += normal_lpdf(b_mupos[23] | 0.04,0.16);
  target += normal_lpdf(b_mupos[24] | 0,2);
  target += normal_lpdf(b_mupos[25] | 0.04,0.14);
  target += normal_lpdf(b_mupos[26] | 0.12,0.09);
  target += normal_lpdf(b_mupos[27] | 0,2);
  target += normal_lpdf(b_mupos[28] | -0.15,0.12);
  target += normal_lpdf(b_mupos[29] | -0.11,0.26);
  target += normal_lpdf(b_mupos[30] | -0.25,0.15);
  target += normal_lpdf(b_mupos[31] | 0.06,0.25);
  target += normal_lpdf(b_mupos[32] | -0.16,0.26);
  target += normal_lpdf(b_mupos[33] | 0,2);
  target += normal_lpdf(b_mupos[34] | 0.16,0.74);
  target += normal_lpdf(b_mupos[35] | -0.12,0.11);
  target += normal_lpdf(b_mupos[36] | 0.19,0.09);
  target += normal_lpdf(b_mupos[37] | -0.1,0.41);
  target += normal_lpdf(b_mupos[38] | 0,2);
  target += normal_lpdf(b_mupos[39] | 0,2);
  target += normal_lpdf(b_mupos[40] | -0.29,0.23);
  target += normal_lpdf(b_mupos[41] | 0.02,0.17);
  target += normal_lpdf(b_mupos[42] | 0.15,0.17);
  target += normal_lpdf(b_mupos[43] | 0.5,0.17);
  target += normal_lpdf(b_mupos[44] | -0.18,0.17);
  target += normal_lpdf(b_mupos[45] | -0.61,0.17);
  target += normal_lpdf(b_mupos[46] | -0.4,0.07);
  target += normal_lpdf(b_mupos[47] | 1.17,0.06);
  target += normal_lpdf(b_mupos[48] | 1.8,0.14);
  target += normal_lpdf(b_mupos[49] | 0,2);
  target += normal_lpdf(b_mupos[50] | 0,2);
  target += normal_lpdf(b_mupos[51] | 0,2);
  target += normal_lpdf(b_mupos[52] | 0,2);
  target += normal_lpdf(Intercept_mupos | 0,3);
  target += gamma_lpdf(sd_1 | 1, 1);
  target += std_normal_lpdf(to_vector(z_1));
  target += lkj_corr_cholesky_lpdf(L_1 | 1);
}
generated quantities {
  // actual population-level intercept
  real b_mudk_Intercept = Intercept_mudk - dot_product(means_X_mudk, b_mudk);
  // actual population-level intercept
  real b_muneg_Intercept = Intercept_muneg - dot_product(means_X_muneg, b_muneg);
  // actual population-level intercept
  real b_mupos_Intercept = Intercept_mupos - dot_product(means_X_mupos, b_mupos);
  // compute group-level correlations
  corr_matrix[M_1] Cor_1 = multiply_lower_tri_self_transpose(L_1);
  vector<lower=-1,upper=1>[NC_1] cor_1;
  // extract upper diagonal of correlation matrix
  for (k in 1:M_1) {
    for (j in 1:(k - 1)) {
      cor_1[choose(k - 1, 2) + j] = Cor_1[j, k];
    }
  }
}
