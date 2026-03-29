
data {
  int<lower=0> N;
  array[N] int<lower=1, upper=31> state;
  array[N] int<lower=1, upper=3> age;
  array[N] int<lower=1, upper=2> gender, urban;
  array[N] int<lower=0, upper=1> y;
  array[31] real income, edu;
  array[31] int<lower=1, upper=3> area;
  array[31,3,2,2] int<lower=1> P;
}

parameters {
  real alpha;
  real psi;
  real eta;
  real<lower=1e-10> sigma_beta, sigma_gamma, sigma_delta, sigma_epsilon, sigma_zeta;
  vector[31] beta;
  vector[3] gamma;
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
  alpha ~ normal(0,1);
  psi ~ normal(0,1);
  eta ~ normal(0,1);
  
  beta ~ normal(0, sigma_beta);
  gamma ~ normal(0, sigma_gamma);
  zeta ~ normal(0, sigma_zeta);
  delta_raw ~ normal(0, sigma_delta);  // 对 raw 参数
  epsilon_raw ~ normal(0, sigma_epsilon);  // 对 raw 参数
  
  // sigma 先验使用 cauchy
  sigma_beta ~ normal(0, 1);
  sigma_gamma ~ normal(0, 1);
  sigma_delta ~ normal(0, 1);
  sigma_epsilon ~ normal(0, 1);
  sigma_zeta ~ normal(0, 1);
}

generated quantities {
  array[31] real expect_pos;  // 存储每个州的期望阳性数
  array[31] real total;       // 存储每个州的总数
  // 初始化数组
  for (s in 1:31) {
    expect_pos[s] = 0;
    total[s] = 0;
  }
  
  // 计算每个州的期望阳性数和总数
  for (b in 1:31) {
    for (c in 1:3) {
      for (d in 1:2) {
        for (e in 1:2) {
          total[b] += P[b, c, d, e];
          expect_pos[b] += P[b, c, d, e] * inv_logit(alpha + beta[b] + gamma[c]  + 
                                                    delta[d] + epsilon[e] + income[b]*psi + 
                                                    edu[b]*eta + zeta[area[b]]);
        }
      }
    }
  }
  
  // 计算每个州的比例
  array[31] real<lower=0, upper=1> phi;
  for (s in 1:31) {
    if (total[s] > 0) {
      phi[s] = expect_pos[s] / total[s];
    } else {
      phi[s] = 0;  // 避免除以零
    }
  }
real tot = sum(expect_pos)/sum(total);  
}


