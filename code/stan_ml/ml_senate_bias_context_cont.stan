data {
  
  int<lower=1> N;                           // number of obs. (polls) 
  int<lower=1> SY;                          // number of elections (state crossed election year)
  
  vector<lower=0, upper=1>[N] poll;         // two-party poll share
  vector<lower=0, upper=1>[SY] vote;        // two -party vote share

  vector<lower=0, upper=1>[N] t;            // days to election
  vector<lower=1>[N] sample_size;           // poll sample size
  vector[SY] feature;                       // context feature

  int<lower=1, upper=SY> sy_id[N];          // election id

} 

transformed data {
  
  vector[SY] logit_vote;
  logit_vote = logit(vote); // transform to logit scale
  
}

parameters {
  
  // hyper parameters: election specific bias
  real mu_alpha;
  real<lower=0> sig_alpha;
  vector[SY] alpha_sc;
  
  // hyper parameters: time trend
  real mu_beta1;
  real<lower=0> sig_beta1;
  vector[SY] beta1_sc;
  
  // parameter: context factor
  real beta2; 

  // hyper parameters: excess variance
  real<lower=0> mu_phi2;
  real<lower=0> sig_phi2;
  vector<lower=-mu_phi2/sig_phi2>[SY] phi2_sc;
  
  
} 

transformed parameters {
  
  vector[SY] alpha;        // election specific bias
  vector[SY] beta1;        // time trend coefficiant
  vector<lower=0>[SY] phi2; // excess variance
  
  // non-centered parametrization
  alpha = mu_alpha + beta2*feature + sig_alpha * alpha_sc;
  beta1 = mu_beta1 + sig_beta1 * beta1_sc;
  phi2 = mu_phi2 + sig_phi2 * phi2_sc;

}

model {
  
  vector[N] logit_p;
  vector[N] p;
  vector[N] sigma2;
  
  // hyper priors
  mu_alpha ~ normal(0,0.2);
  sig_alpha ~ normal(0,0.2) T[0,];
  alpha_sc ~ normal(0,1);  
  
  mu_beta1 ~ normal(0,0.2);
  sig_beta1 ~ normal(0,0.2) T[0,]; 
  beta1_sc ~ normal(0,1);
  
  beta2 ~ normal(0, 0.2);  
  
  mu_phi2 ~ normal(0,0.05);
  sig_phi2 ~ normal(0,0.05) T[0,];
  for(i in 1:SY) phi2_sc[i] ~ normal(0,1) T[-mu_phi2/sig_phi2,];

  // mean model
  for(i in 1:N){
    logit_p[i] = logit_vote[sy_id[i]] + 
      alpha[sy_id[i]] + 
      beta1[sy_id[i]] * t[i]; 
  }
  p = inv_logit(logit_p); // rescale mean
  
  // variance model
  for(i in 1:N){
    sigma2[i] = (p[i]*(1-p[i])/sample_size[i]) + 
      phi2[sy_id[i]];    
  }

  // poll estimate  
  poll ~ normal(p, sqrt(sigma2));
  
}
