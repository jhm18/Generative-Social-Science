data {
  int < lower = 1 > N;                              // number of observations
  int < lower = 1 > U;                              // number of unpenalized columns in X matrix
  int < lower = 1 > N_indivs;                       // number of individuals
  int < lower = 1 > indiv[N];                       // subscripts indexing individuals
  real priormean_icept;                             // prior mean of intercept
  real < lower=0 > scale_icept ;                    // sd of prior on intercept
  int < lower=0 > y[ N];                            // count of events
  matrix[N, U] X;                                   // X matrix without ones for intercept in column 1 
}

transformed data { 
  matrix[N, U] Q_ast;                               // QR reparameterization
  matrix[U, U] R_ast;
  matrix[U, U] R_ast_inverse;

  // thin and scale the QR decomposition
  Q_ast = qr_Q(X)[, 1:U] * sqrt(N - 1);
  R_ast = qr_R(X)[1:U, ] / sqrt(N - 1);
  R_ast_inverse = inverse(R_ast);
}

parameters {
  real alpha;                                       // coefficient of variation of random effects
  real < lower=0 > sigma_indiv;                     // SD of random effects
  vector[U] theta;                                  // coefficients on Q_ast
  vector[N_indivs] xi;                              // random effects with standard normal prior
}

transformed parameters {
  vector[N] eta;                                    // linear predictor on scale of log hazard rate
  vector[N_indivs] w;                               // individual random effects 
  eta = Q_ast * theta; 
  w = alpha + xi * sigma_indiv;                     // construct w by scaling xi by sigma_indiv and shifting by alpha
}

model {
  sigma_indiv ~ cauchy(0, 5);                       // half-cauchy prior
  alpha ~ normal(0, 10);
  theta ~ normal(0, scale_icept);


  xi ~ normal(0, 1);
  for(i in 1:N) {
    y[i] ~ poisson_log(eta[i] + w[indiv[i]]);
  }
}

generated quantities {
  vector[U] beta;
  beta = R_ast_inverse * theta; // coefficients on x
}
