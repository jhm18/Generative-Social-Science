data {
    int<lower=0> nObs;                      //Number of Observations
    int<lower=0> d;                         //Number of dimensions (Pedictors)
    vector[nObs] y;                         //Outcome Variable
    vector[nObs] x;                         //Explanatory Variable
    real <lower=0> scale_incept;            //Prior STD for the intercept
    real <lower=0> scale_global;            //Scale for the half-t prior for tau
    real <lower=1> nu_global;               //Degrees of freedom for the half-t priors for lambda 
    real <lower=1> nu_local;                //Degrees of freedom for the half-t priors for lambdas
    real <lower=0> slab_scale;              //Slab scale for the regularized horseshoe
    real <lower=0> slab_df;                 //Slab degress of freedom for the regularzed horseshoe
}

parameters {
    real logsigma;
    real a;
    vector[d] z;
    real<lower=0> aux1_global;
    real<lower=0> aux2_global;
    vector<lower=0>[d] aux1_local;
    vector<lower=0>[d] aux2_local;
    real<lower=0> caux;
}

transformed parameters {
    real<lower=0> sigma;                    //Noise STD
    real<lower=0> tau;                      //Global shrinkage parameter
    vector<lower=0>[d] lambda;              //Local shrinkage parameter
    vector<lower=0>[d] lambda_tilde;        //Truncated local shrinkage parameter
    real<lower=0> c;                        //Slab scale
    vector[d] b;                            //Regression coefficients
    vector[nObs] f;                            //Latent function values
    sigma = exp(logsigma);                  
    lambda = aux1_local .* sqrt(aux2_local);
    tau = aux1_global * sqrt(aux2_global) * scale_global*sigma;
    c = slab_scale * sqrt(caux);
    lambda_tilde = sqrt( c^2 * square(lambda) ./ (c^2 + tau^2*square(lambda)) );
    b = z .* lambda_tilde*tau;
    f = a + x*b;
}

model {
    //Half-t priors for lambdas and tau, and inverse-gamma for c^2
    z ∼ normal(0, 1);
    aux1_local ∼ normal(0, 1);
    aux2_local ∼ inv_gamma (0.5* nu_local , 0.5* nu_local );
    aux1_global ∼ normal(0, 1);
    aux2_global ∼ inv_gamma (0.5* nu_global , 0.5* nu_global );
    caux ∼ inv_gamma (0.5* slab_df , 0.5* slab_df );
    a ∼ normal(0, scale_incept);

    //Likelihood
    y ∼ normal(f, sigma);
}