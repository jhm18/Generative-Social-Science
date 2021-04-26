//  Example Periodic Function
//  Martin Modrak: https://discourse.mc-stan.org/t/mathy-folks-thoughts-on-the-asymptotic-representation-of-this-k-hmm-model-for-periodic-signals/22038/24
//  25 April 2021

data{
	int n ;
	vector[n] y ;
	vector[n] x ;
	int<lower=0> max_k;
	real x_fixed[2];
}

transformed data {
  real log_prior[max_k + 1] = rep_array(-log(max_k + 1.0), max_k + 1); //prior prob 1/(max_k + 1)
}

parameters{
	real<lower=0> noise ;
	real<lower=0> amplitude;
	real<lower=-1, upper=1> z[2];
}

transformed parameters{
  real frequency[2, 2, max_k + 1];
  real phase[2, 2, max_k + 1];
  real log_lik[2, 2, max_k + 1];
  {
    real asinz[2] = asin(z);
    real w[2,2];
    w[1,]= { asinz[1], pi() - asinz[1]};
    w[2,]= { asinz[2], pi() - asinz[2]};
    for(i in 1:2) {
      for(j in 1:2) {
        for(k in 1:(max_k + 1)) {
          frequency[i, j, k] = (w[2, i] + 2 * (k-1) * pi() - w[1, j]) / (x_fixed[2] - x_fixed[1]);
          phase[i, j , k] = w[1,i] - frequency[i, j, k] * x_fixed[1];
          {
            vector[n] f = amplitude * sin(frequency[i, j, k] * x + phase[i, j, k]);
            log_lik[i, j, k] = normal_lpdf(y | f, noise)
              + log_prior[k]
              -log(2.0) //Uniform prior over the 2 options
              ;
          }
        }
      }
    }
  }
}

model{
	noise ~ weibull(2,1) ; //peaked at ~.8, zero-at-zero, ~2% mass >2
	amplitude ~ weibull(2,1) ; //ditto
	target += log_sum_exp(to_array_1d(log_lik));
}

generated quantities {
  // Discrete sampling is inefficient, directly computing weights
  // And taking weighted expectation values would work better
  int index = categorical_logit_rng(to_vector(to_array_1d(log_lik)));
  real freq_chosen = to_array_1d(frequency)[index];
  real phase_chosen = to_array_1d(phase)[index];
  //Force phase to lie between -pi + pi the stupid way
  while(phase_chosen < -pi()) {
    phase_chosen += 2 * pi();
  }
  while(phase_chosen > pi()) {
    phase_chosen -= 2 * pi();
  }
}
