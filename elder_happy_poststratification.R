rm(list = ls())

# setwd('D:/cgss/幸福感/elder')
library(cmdstanr);library(posterior);library(tidyverse)

psframe <- read.csv('census.csv')
df <- read.csv('survey_happy.csv')
province <- read.csv('province.csv') 

four_way_array <- array(data = psframe$freq,
                        dim = c(31, 3, 2, 2),
                        dimnames = list(
                          state = 1:31,age = 1:3,
                          gender = c("1", "2"),
                          urban = c("1", "2") ))

data_for_stan <- list(N = nrow(df),state = df$unit,
                      age = df$age,gender = df$gender,
                      urban = df$urban,y = df$happy,
                      income = province$gdp,edu = province$weighted_avg,
                      area = province$area,
                      P = four_way_array)




# by province

mrp <- '
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

'
write(mrp, file = 'mrp.stan')

library(cmdstanr)
# 编译模型（如果已经编译过，可以跳过）
mod <- cmdstan_model("mrp.stan")
fit.bb <- mod$sample(data = data_for_stan)

draws <- fit.bb$draws()  # 返回 draws_array 对象

summary_stats <- summarise_draws(draws) 
write.csv(summary_stats[summary_stats$variable=='tot',],'tot.csv')

fig1 <- summary_stats[grep("phi",
                                  summary_stats$variable), ]

fig1$province <- c("Anhui", "Beijing", "Fujian", "Gansu", "Guangdong", 
                   "Guangxi", "Guizhou", "Hainan", "Hebei","Henan", 
                   "Heilongjiang", "Hubei", "Hunan", "Jilin", "Jiangsu", 
                   "Jiangxi", "Liaoning", "Inner Mongolia","Ningxia", "Qinghai", 
                   "Shandong", "Shanxi", "Shaanxi", "Shanghai", "Sichuan",
                   "Tianjin", "Xinjiang","Yunnan", "Zhejiang", 
                   "Chongqing", "Tibet")

fig1_sorted <- fig1 %>%
  arrange(mean) %>%
  mutate(province = factor(province, levels = province))

  write.csv(fig1_sorted,'fig1_sorted.csv')
