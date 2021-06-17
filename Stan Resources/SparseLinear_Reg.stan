// Sparse linear regression in Stan
// Joris Chau
// 16 June 2021

functions{
  #include haar.stan
}

data {
  int<lower=1> N;
  vector[N] y;        // input vector
  real<lower = 0> m0; // expected fraction of non-zero coefficients
}

transformed data{
  int M = (N / 2) - 1;                               // # estimated coefficients
  vector[N] ywd = fwt(y);                            // wavelet coefficients input
  real sigma0 = sd(ywd[1 : (N / 2)]);                // initial estimate sigma
  real tau0 = m0 / (1 - m0) * sigma0 / sqrt(N - 1);  // irrelevance scale
}

parameters {
  real<lower=0> sigma;        // noise standard deviation
  real<lower=0> tau;          // global scale horseshoe
  vector[M] z;                // unscaled estimated coefficients
  vector<lower=0>[M] lambda;  // local scales horseshoe
}

transformed parameters {
  // regularized (sparse) wavelet coefficients
  vector[N] fwd = rep_vector(0.0, N);
  fwd[(N - M) : (N - 1)] = (tau * lambda ./ sqrt(1 + square(tau * lambda))) .* z;
  fwd[N] = ywd[N];
}

model {  
  // (sparse) priors
  lambda ~ cauchy(0, 1);
  sigma ~ normal(sigma0, 5 * sigma0);
  tau ~ normal(0, tau0);
  z ~ std_normal();
  
  // likelhood contributions
  ywd[1 : (N - 1)] ~ normal(fwd[1 : (N - 1)], sigma);
}

generated quantities {
  // back-transformed coefficients
  vector[N] f = iwt(fwd);
}