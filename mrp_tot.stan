
data {
  int<lower=0> N;
  array[N] int<lower=1, upper=31> state;
  array[N] int<lower=1, upper=4> age;
  array[N] int<lower=1, upper=2> gender, urban;
  array[N] int<lower=0, upper=1> y;
  array[31] real income, edu;
  array[31] int<lower=1, upper=3> area;
  array[31,4,2,2] int<lower=1> P;
}

parameters {
  real alpha;
  real psi;
  real eta;
  real<lower=1e-10> sigma_beta, sigma_gamma, sigma_delta, sigma_epsilon, sigma_zeta;
  vector[31] beta;
  vector[4] gamma;
  vector[1] delta_raw;  // 改为 raw 参数
  vector[1] epsilon_raw;  // 改为 raw 参数
  vector[3] zeta;
}

transformed parameters {
  vector[2] delta;
  vector[2] epsilon;
  
  // 中心化约束
  delta[1] = delta_raw[1];
  delta[2] = -delta_raw[1];
  
  epsilon[1] = epsilon_raw[1];
  epsilon[2] = -epsilon_raw[1];
}

model {
  vector[N] linear_predictor;
  
  for (i in 1:N) {
    linear_predictor[i] = alpha + gamma[age[i]] +
                         delta[gender[i]] + epsilon[urban[i]] +
                         income[state[i]] * psi + edu[state[i]] * eta + 
                         zeta[area[state[i]]];
  }
  
  y ~ bernoulli_logit(beta[state] + linear_predictor);
  
  // 先验
  alpha ~ normal(0, 2);
  psi ~ normal(0, 2);
  eta ~ normal(0, 2);
  
  beta ~ normal(0, sigma_beta);
  gamma ~ normal(0, sigma_gamma);
  zeta ~ normal(0, sigma_zeta);
  delta_raw ~ normal(0, sigma_delta);  // 对 raw 参数
  epsilon_raw ~ normal(0, sigma_epsilon);  // 对 raw 参数
  
  // sigma 先验使用 cauchy
  sigma_beta ~ cauchy(0, 1);
  sigma_gamma ~ cauchy(0, 1);
  sigma_delta ~ cauchy(0, 1);
  sigma_epsilon ~ cauchy(0, 1);
  sigma_zeta ~ cauchy(0, 1);
}

generated quantities {
  real expect_pos_tot = 0;
  real total_tot = 0;
  real phi_tot;

  for (b in 1:31) {
    for (c in 1:4) {
      for (d in 1:2) {
        for (e in 1:2) {
          total_tot += P[b, c, d, e];
          expect_pos_tot += P[b, c, d, e] * inv_logit(alpha + beta[b] + gamma[c] + 
                                                    delta[d] + epsilon[e] + income[b] * psi + 
                                                    edu[b] * eta + zeta[area[b]]);
        }
      }
    }
  }
  
  phi_tot = (total_tot > 0) ? (expect_pos_tot / total_tot) : 0;
}


