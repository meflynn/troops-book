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
  real partial_log_lik_lpmf(int[] seq, int start, int end, data int ncat, data int[] Y, data matrix Xc_mudk, vector b_mudk, real Intercept_mudk, data matrix Xc_muneg, vector b_muneg, real Intercept_muneg, data matrix Xc_mupos, vector b_mupos, real Intercept_mupos, data int[] J_1, data vector Z_1_mudk_1, data vector Z_1_mudk_2, data vector Z_1_mudk_3, data vector Z_1_mudk_4, data vector Z_1_mudk_5, data vector Z_1_mudk_6, data vector Z_1_mudk_7, data vector Z_1_mudk_8, data vector Z_1_mudk_9, data vector Z_1_muneg_10, data vector Z_1_muneg_11, data vector Z_1_muneg_12, data vector Z_1_muneg_13, data vector Z_1_muneg_14, data vector Z_1_muneg_15, data vector Z_1_muneg_16, data vector Z_1_muneg_17, data vector Z_1_muneg_18, data vector Z_1_mupos_19, data vector Z_1_mupos_20, data vector Z_1_mupos_21, data vector Z_1_mupos_22, data vector Z_1_mupos_23, data vector Z_1_mupos_24, data vector Z_1_mupos_25, data vector Z_1_mupos_26, data vector Z_1_mupos_27, vector r_1_mudk_1, vector r_1_mudk_2, vector r_1_mudk_3, vector r_1_mudk_4, vector r_1_mudk_5, vector r_1_mudk_6, vector r_1_mudk_7, vector r_1_mudk_8, vector r_1_mudk_9, vector r_1_muneg_10, vector r_1_muneg_11, vector r_1_muneg_12, vector r_1_muneg_13, vector r_1_muneg_14, vector r_1_muneg_15, vector r_1_muneg_16, vector r_1_muneg_17, vector r_1_muneg_18, vector r_1_mupos_19, vector r_1_mupos_20, vector r_1_mupos_21, vector r_1_mupos_22, vector r_1_mupos_23, vector r_1_mupos_24, vector r_1_mupos_25, vector r_1_mupos_26, vector r_1_mupos_27) {
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
      mudk[n] += r_1_mudk_1[J_1[nn]] * Z_1_mudk_1[nn] + r_1_mudk_2[J_1[nn]] * Z_1_mudk_2[nn] + r_1_mudk_3[J_1[nn]] * Z_1_mudk_3[nn] + r_1_mudk_4[J_1[nn]] * Z_1_mudk_4[nn] + r_1_mudk_5[J_1[nn]] * Z_1_mudk_5[nn] + r_1_mudk_6[J_1[nn]] * Z_1_mudk_6[nn] + r_1_mudk_7[J_1[nn]] * Z_1_mudk_7[nn] + r_1_mudk_8[J_1[nn]] * Z_1_mudk_8[nn] + r_1_mudk_9[J_1[nn]] * Z_1_mudk_9[nn];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      muneg[n] += r_1_muneg_10[J_1[nn]] * Z_1_muneg_10[nn] + r_1_muneg_11[J_1[nn]] * Z_1_muneg_11[nn] + r_1_muneg_12[J_1[nn]] * Z_1_muneg_12[nn] + r_1_muneg_13[J_1[nn]] * Z_1_muneg_13[nn] + r_1_muneg_14[J_1[nn]] * Z_1_muneg_14[nn] + r_1_muneg_15[J_1[nn]] * Z_1_muneg_15[nn] + r_1_muneg_16[J_1[nn]] * Z_1_muneg_16[nn] + r_1_muneg_17[J_1[nn]] * Z_1_muneg_17[nn] + r_1_muneg_18[J_1[nn]] * Z_1_muneg_18[nn];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      mupos[n] += r_1_mupos_19[J_1[nn]] * Z_1_mupos_19[nn] + r_1_mupos_20[J_1[nn]] * Z_1_mupos_20[nn] + r_1_mupos_21[J_1[nn]] * Z_1_mupos_21[nn] + r_1_mupos_22[J_1[nn]] * Z_1_mupos_22[nn] + r_1_mupos_23[J_1[nn]] * Z_1_mupos_23[nn] + r_1_mupos_24[J_1[nn]] * Z_1_mupos_24[nn] + r_1_mupos_25[J_1[nn]] * Z_1_mupos_25[nn] + r_1_mupos_26[J_1[nn]] * Z_1_mupos_26[nn] + r_1_mupos_27[J_1[nn]] * Z_1_mupos_27[nn];
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
  vector[N] Z_1_mudk_2;
  vector[N] Z_1_mudk_3;
  vector[N] Z_1_mudk_4;
  vector[N] Z_1_mudk_5;
  vector[N] Z_1_mudk_6;
  vector[N] Z_1_mudk_7;
  vector[N] Z_1_mudk_8;
  vector[N] Z_1_mudk_9;
  vector[N] Z_1_muneg_10;
  vector[N] Z_1_muneg_11;
  vector[N] Z_1_muneg_12;
  vector[N] Z_1_muneg_13;
  vector[N] Z_1_muneg_14;
  vector[N] Z_1_muneg_15;
  vector[N] Z_1_muneg_16;
  vector[N] Z_1_muneg_17;
  vector[N] Z_1_muneg_18;
  vector[N] Z_1_mupos_19;
  vector[N] Z_1_mupos_20;
  vector[N] Z_1_mupos_21;
  vector[N] Z_1_mupos_22;
  vector[N] Z_1_mupos_23;
  vector[N] Z_1_mupos_24;
  vector[N] Z_1_mupos_25;
  vector[N] Z_1_mupos_26;
  vector[N] Z_1_mupos_27;
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
  vector[N_1] r_1_mudk_2;
  vector[N_1] r_1_mudk_3;
  vector[N_1] r_1_mudk_4;
  vector[N_1] r_1_mudk_5;
  vector[N_1] r_1_mudk_6;
  vector[N_1] r_1_mudk_7;
  vector[N_1] r_1_mudk_8;
  vector[N_1] r_1_mudk_9;
  vector[N_1] r_1_muneg_10;
  vector[N_1] r_1_muneg_11;
  vector[N_1] r_1_muneg_12;
  vector[N_1] r_1_muneg_13;
  vector[N_1] r_1_muneg_14;
  vector[N_1] r_1_muneg_15;
  vector[N_1] r_1_muneg_16;
  vector[N_1] r_1_muneg_17;
  vector[N_1] r_1_muneg_18;
  vector[N_1] r_1_mupos_19;
  vector[N_1] r_1_mupos_20;
  vector[N_1] r_1_mupos_21;
  vector[N_1] r_1_mupos_22;
  vector[N_1] r_1_mupos_23;
  vector[N_1] r_1_mupos_24;
  vector[N_1] r_1_mupos_25;
  vector[N_1] r_1_mupos_26;
  vector[N_1] r_1_mupos_27;
  // compute actual group-level effects
  r_1 = scale_r_cor(z_1, sd_1, L_1);
  r_1_mudk_1 = r_1[, 1];
  r_1_mudk_2 = r_1[, 2];
  r_1_mudk_3 = r_1[, 3];
  r_1_mudk_4 = r_1[, 4];
  r_1_mudk_5 = r_1[, 5];
  r_1_mudk_6 = r_1[, 6];
  r_1_mudk_7 = r_1[, 7];
  r_1_mudk_8 = r_1[, 8];
  r_1_mudk_9 = r_1[, 9];
  r_1_muneg_10 = r_1[, 10];
  r_1_muneg_11 = r_1[, 11];
  r_1_muneg_12 = r_1[, 12];
  r_1_muneg_13 = r_1[, 13];
  r_1_muneg_14 = r_1[, 14];
  r_1_muneg_15 = r_1[, 15];
  r_1_muneg_16 = r_1[, 16];
  r_1_muneg_17 = r_1[, 17];
  r_1_muneg_18 = r_1[, 18];
  r_1_mupos_19 = r_1[, 19];
  r_1_mupos_20 = r_1[, 20];
  r_1_mupos_21 = r_1[, 21];
  r_1_mupos_22 = r_1[, 22];
  r_1_mupos_23 = r_1[, 23];
  r_1_mupos_24 = r_1[, 24];
  r_1_mupos_25 = r_1[, 25];
  r_1_mupos_26 = r_1[, 26];
  r_1_mupos_27 = r_1[, 27];
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, ncat, Y, Xc_mudk, b_mudk, Intercept_mudk, Xc_muneg, b_muneg, Intercept_muneg, Xc_mupos, b_mupos, Intercept_mupos, J_1, Z_1_mudk_1, Z_1_mudk_2, Z_1_mudk_3, Z_1_mudk_4, Z_1_mudk_5, Z_1_mudk_6, Z_1_mudk_7, Z_1_mudk_8, Z_1_mudk_9, Z_1_muneg_10, Z_1_muneg_11, Z_1_muneg_12, Z_1_muneg_13, Z_1_muneg_14, Z_1_muneg_15, Z_1_muneg_16, Z_1_muneg_17, Z_1_muneg_18, Z_1_mupos_19, Z_1_mupos_20, Z_1_mupos_21, Z_1_mupos_22, Z_1_mupos_23, Z_1_mupos_24, Z_1_mupos_25, Z_1_mupos_26, Z_1_mupos_27, r_1_mudk_1, r_1_mudk_2, r_1_mudk_3, r_1_mudk_4, r_1_mudk_5, r_1_mudk_6, r_1_mudk_7, r_1_mudk_8, r_1_mudk_9, r_1_muneg_10, r_1_muneg_11, r_1_muneg_12, r_1_muneg_13, r_1_muneg_14, r_1_muneg_15, r_1_muneg_16, r_1_muneg_17, r_1_muneg_18, r_1_mupos_19, r_1_mupos_20, r_1_mupos_21, r_1_mupos_22, r_1_mupos_23, r_1_mupos_24, r_1_mupos_25, r_1_mupos_26, r_1_mupos_27);
  }
  // priors including constants
  target += normal_lpdf(b_mudk[1] | -0.28,0.39);
  target += normal_lpdf(b_mudk[2] | 0,0.38);
  target += normal_lpdf(b_mudk[3] | 0.29,0.33);
  target += normal_lpdf(b_mudk[4] | 0.05,0.37);
  target += normal_lpdf(b_mudk[5] | 0.24,0.32);
  target += normal_lpdf(b_mudk[6] | 0.44,0.41);
  target += normal_lpdf(b_mudk[7] | 0.07,0.3);
  target += normal_lpdf(b_mudk[8] | -0.57,0.49);
  target += normal_lpdf(b_mudk[9] | -0.17,0.26);
  target += normal_lpdf(b_mudk[10] | 0.09,0.25);
  target += normal_lpdf(b_mudk[11] | 0.1,0.26);
  target += normal_lpdf(b_mudk[12] | 0.05,0.27);
  target += normal_lpdf(b_mudk[13] | -0.17,0.35);
  target += normal_lpdf(b_mudk[14] | 0,2);
  target += normal_lpdf(b_mudk[15] | 0,2);
  target += normal_lpdf(b_mudk[16] | 0,2);
  target += normal_lpdf(b_mudk[17] | 0,2);
  target += normal_lpdf(b_mudk[18] | 0,2);
  target += normal_lpdf(b_mudk[19] | 0,2);
  target += normal_lpdf(b_mudk[20] | -0.1,0.16);
  target += normal_lpdf(b_mudk[21] | 1.7,0.93);
  target += normal_lpdf(b_mudk[22] | -79.93,60.03);
  target += normal_lpdf(b_mudk[23] | -0.26,0.34);
  target += normal_lpdf(b_mudk[24] | 0,2);
  target += normal_lpdf(b_mudk[25] | 0.14,0.55);
  target += normal_lpdf(b_mudk[26] | 0.56,0.33);
  target += normal_lpdf(b_mudk[27] | 0,2);
  target += normal_lpdf(b_mudk[28] | 0.86,0.34);
  target += normal_lpdf(b_mudk[29] | 0.63,0.81);
  target += normal_lpdf(b_mudk[30] | 0.52,0.44);
  target += normal_lpdf(b_mudk[31] | 1.8,0.66);
  target += normal_lpdf(b_mudk[32] | 1.43,0.61);
  target += normal_lpdf(b_mudk[33] | 0,2);
  target += normal_lpdf(b_mudk[34] | -77.41,61.07);
  target += normal_lpdf(b_mudk[35] | 0.38,0.39);
  target += normal_lpdf(b_mudk[36] | 0.19,0.38);
  target += normal_lpdf(b_mudk[37] | 1.96,0.95);
  target += normal_lpdf(b_mudk[38] | 0,2);
  target += normal_lpdf(b_mudk[39] | 0,2);
  target += normal_lpdf(b_mudk[40] | 0.14,0.38);
  target += normal_lpdf(b_mudk[41] | -1.63,0.42);
  target += normal_lpdf(b_mudk[42] | -0.82,0.35);
  target += normal_lpdf(b_mudk[43] | -0.98,0.38);
  target += normal_lpdf(b_mudk[44] | 2.05,0.25);
  target += normal_lpdf(b_mudk[45] | 1.49,0.47);
  target += normal_lpdf(b_mudk[46] | 0.38,0.29);
  target += normal_lpdf(b_mudk[47] | -0.05,0.27);
  target += normal_lpdf(b_mudk[48] | 1.01,0.46);
  target += normal_lpdf(b_mudk[49] | 0,2);
  target += normal_lpdf(b_mudk[50] | 0,2);
  target += normal_lpdf(b_mudk[51] | 0,2);
  target += normal_lpdf(b_mudk[52] | 0,2);
  target += normal_lpdf(Intercept_mudk | 0,3);
  target += normal_lpdf(b_muneg[1] | -0.7,0.21);
  target += normal_lpdf(b_muneg[2] | 0.07,0.11);
  target += normal_lpdf(b_muneg[3] | -0.1,0.15);
  target += normal_lpdf(b_muneg[4] | 0.32,0.11);
  target += normal_lpdf(b_muneg[5] | -0.52,0.18);
  target += normal_lpdf(b_muneg[6] | -0.21,0.16);
  target += normal_lpdf(b_muneg[7] | -0.12,0.16);
  target += normal_lpdf(b_muneg[8] | -0.3,0.15);
  target += normal_lpdf(b_muneg[9] | -0.19,0.1);
  target += normal_lpdf(b_muneg[10] | -0.15,0.1);
  target += normal_lpdf(b_muneg[11] | -0.12,0.1);
  target += normal_lpdf(b_muneg[12] | 0.01,0.1);
  target += normal_lpdf(b_muneg[13] | 0.17,0.11);
  target += normal_lpdf(b_muneg[14] | 0,2);
  target += normal_lpdf(b_muneg[15] | 0,2);
  target += normal_lpdf(b_muneg[16] | 0,2);
  target += normal_lpdf(b_muneg[17] | 0,2);
  target += normal_lpdf(b_muneg[18] | 0,2);
  target += normal_lpdf(b_muneg[19] | 0,2);
  target += normal_lpdf(b_muneg[20] | 0.02,0.05);
  target += normal_lpdf(b_muneg[21] | 0.44,0.6);
  target += normal_lpdf(b_muneg[22] | -0.03,0.65);
  target += normal_lpdf(b_muneg[23] | -0.07,0.16);
  target += normal_lpdf(b_muneg[24] | 0,2);
  target += normal_lpdf(b_muneg[25] | -0.55,0.16);
  target += normal_lpdf(b_muneg[26] | -0.34,0.1);
  target += normal_lpdf(b_muneg[27] | 0,2);
  target += normal_lpdf(b_muneg[28] | -0.48,0.12);
  target += normal_lpdf(b_muneg[29] | -0.71,0.41);
  target += normal_lpdf(b_muneg[30] | 0.35,0.18);
  target += normal_lpdf(b_muneg[31] | -1.43,0.55);
  target += normal_lpdf(b_muneg[32] | -1.03,0.3);
  target += normal_lpdf(b_muneg[33] | 0,2);
  target += normal_lpdf(b_muneg[34] | 2.3,1.44);
  target += normal_lpdf(b_muneg[35] | -0.12,0.11);
  target += normal_lpdf(b_muneg[36] | -0.46,0.11);
  target += normal_lpdf(b_muneg[37] | -1.41,0.58);
  target += normal_lpdf(b_muneg[38] | 0,2);
  target += normal_lpdf(b_muneg[39] | 0,2);
  target += normal_lpdf(b_muneg[40] | -0.72,0.22);
  target += normal_lpdf(b_muneg[41] | -0.12,0.17);
  target += normal_lpdf(b_muneg[42] | -0.09,0.17);
  target += normal_lpdf(b_muneg[43] | 0.08,0.17);
  target += normal_lpdf(b_muneg[44] | 0.28,0.15);
  target += normal_lpdf(b_muneg[45] | 2.79,0.2);
  target += normal_lpdf(b_muneg[46] | 1.58,0.08);
  target += normal_lpdf(b_muneg[47] | -0.4,0.07);
  target += normal_lpdf(b_muneg[48] | -0.61,0.21);
  target += normal_lpdf(b_muneg[49] | 0,2);
  target += normal_lpdf(b_muneg[50] | 0,2);
  target += normal_lpdf(b_muneg[51] | 0,2);
  target += normal_lpdf(b_muneg[52] | 0,2);
  target += normal_lpdf(Intercept_muneg | 0,3);
  target += normal_lpdf(b_mupos[1] | -0.51,0.21);
  target += normal_lpdf(b_mupos[2] | 0.11,0.11);
  target += normal_lpdf(b_mupos[3] | 0.18,0.15);
  target += normal_lpdf(b_mupos[4] | 0.23,0.11);
  target += normal_lpdf(b_mupos[5] | 0.04,0.18);
  target += normal_lpdf(b_mupos[6] | 0.32,0.13);
  target += normal_lpdf(b_mupos[7] | -0.06,0.16);
  target += normal_lpdf(b_mupos[8] | 0.08,0.13);
  target += normal_lpdf(b_mupos[9] | -0.06,0.1);
  target += normal_lpdf(b_mupos[10] | 0.06,0.1);
  target += normal_lpdf(b_mupos[11] | -0.03,0.1);
  target += normal_lpdf(b_mupos[12] | 0.04,0.1);
  target += normal_lpdf(b_mupos[13] | 0.15,0.11);
  target += normal_lpdf(b_mupos[14] | 0,2);
  target += normal_lpdf(b_mupos[15] | 0,2);
  target += normal_lpdf(b_mupos[16] | 0,2);
  target += normal_lpdf(b_mupos[17] | 0,2);
  target += normal_lpdf(b_mupos[18] | 0,2);
  target += normal_lpdf(b_mupos[19] | 0,2);
  target += normal_lpdf(b_mupos[20] | 0.03,0.05);
  target += normal_lpdf(b_mupos[21] | -0.14,0.5);
  target += normal_lpdf(b_mupos[22] | -0.79,0.73);
  target += normal_lpdf(b_mupos[23] | 0.27,0.18);
  target += normal_lpdf(b_mupos[24] | 0,2);
  target += normal_lpdf(b_mupos[25] | 0.04,0.15);
  target += normal_lpdf(b_mupos[26] | -0.05,0.11);
  target += normal_lpdf(b_mupos[27] | 0,2);
  target += normal_lpdf(b_mupos[28] | -0.19,0.14);
  target += normal_lpdf(b_mupos[29] | -0.32,0.27);
  target += normal_lpdf(b_mupos[30] | -0.31,0.16);
  target += normal_lpdf(b_mupos[31] | 0.09,0.26);
  target += normal_lpdf(b_mupos[32] | 0.01,0.29);
  target += normal_lpdf(b_mupos[33] | 0,2);
  target += normal_lpdf(b_mupos[34] | 1.97,1.39);
  target += normal_lpdf(b_mupos[35] | -0.01,0.13);
  target += normal_lpdf(b_mupos[36] | 0.01,0.11);
  target += normal_lpdf(b_mupos[37] | 0.06,0.41);
  target += normal_lpdf(b_mupos[38] | 0,2);
  target += normal_lpdf(b_mupos[39] | 0,2);
  target += normal_lpdf(b_mupos[40] | -0.84,0.29);
  target += normal_lpdf(b_mupos[41] | 0.27,0.2);
  target += normal_lpdf(b_mupos[42] | 0.1,0.2);
  target += normal_lpdf(b_mupos[43] | 0.56,0.2);
  target += normal_lpdf(b_mupos[44] | -0.26,0.2);
  target += normal_lpdf(b_mupos[45] | 0.28,0.26);
  target += normal_lpdf(b_mupos[46] | -0.01,0.11);
  target += normal_lpdf(b_mupos[47] | 1.39,0.07);
  target += normal_lpdf(b_mupos[48] | 2.28,0.14);
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
