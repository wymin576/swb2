rm(list = ls())
# setwd('D:/cgss/mobility')
library(cmdstanr);library(posterior);library(tidyverse)

psframe <- read.csv('census.csv')
df <- read.csv('survey_happy.csv')
province <- read.csv('province.csv') 

mrp_sen <- '
data {
  int<lower=0> N;
  array[N] int<lower=1, upper=31> state;
  array[N] int<lower=1, upper=3> age;
  array[N] int<lower=1, upper=2> gender, urban;
  array[N] int<lower=0, upper=1> y;
  array[31] real income, edu;
  array[31] int<lower=1, upper=3> area;
  array[31,3,2,2] int<lower=1> P;
  real tau1,tau2;
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
  alpha ~ normal(0, tau1);
  psi ~ normal(0, tau1);
  eta ~ normal(0, tau1);
  
  beta ~ normal(0, sigma_beta);
  gamma ~ normal(0, sigma_gamma);
  zeta ~ normal(0, sigma_zeta);
  delta_raw ~ normal(0, sigma_delta);  // 对 raw 参数
  epsilon_raw ~ normal(0, sigma_epsilon);  // 对 raw 参数
  
  // sigma 先验使用 cauchy
  sigma_beta ~ cauchy(0, tau2);
  sigma_gamma ~ cauchy(0, tau2);
  sigma_delta ~ cauchy(0, tau2);
  sigma_epsilon ~ cauchy(0, tau2);
  sigma_zeta ~ cauchy(0, tau2);
}

generated quantities {
  real expect_pos_tot = 0;
  real total_tot = 0;
  real phi;

  for (b in 1:31) {
    for (c in 1:3) {
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
  
  phi = (total_tot > 0) ? (expect_pos_tot / total_tot) : 0;
}

'
write(mrp_sen, file = 'mrp_sen.stan')

library(cmdstanr)
# 编译模型（如果已经编译过，可以跳过）
four_way_array <- array(data = psframe$freq,
                        dim = c(31, 3, 2, 2),
                        dimnames = list(
                          state = 1:31,age = 1:3,
                          gender = c("1", "2"),
                          urban = c("1", "2") ))

data <- list(N = nrow(df),state = df$unit,
                      age = df$age,gender = df$gender,
                      urban = df$urban,y = df$happy,
                      income = province$gdp,edu = province$weighted_avg,
                      area = province$area,
                      P = four_way_array)

model <- cmdstan_model("mrp_sen.stan")

ribbon_df <- data.frame(sigma_1 = c(), sigma_2 = c(),
                        prev05 = c(), prev50 = c(), prev95 = c())


sigma_1s <- (1:10)*0.5
sigma_2s <- (1:10)*0.5

# 初始化数据框
ribbon_df <- data.frame()

for (sigma_1 in sigma_1s) {
  for (sigma_2 in sigma_2s) {
    print(c(sigma_1, sigma_2))
    data2 <- append(data, list(tau1 = sigma_1,
                               tau2 = sigma_2))
    fit <-  model$sample(data = data2, 
                         iter_warmup = 200,iter_sampling = 1000)
    pis <- as.vector(fit$draws()[,,"phi"])
    ribbon_df <- rbind(ribbon_df,
                       data.frame(sigma_1 = paste('τ1', "=", sigma_1),
                                  sigma_2 = sigma_2,
                                  prev05 = quantile(pis, 0.05),
                                  prev50 = quantile(pis, 0.5),
                                  prev95 = quantile(pis, 0.95)))
  }
}

# 修改后的绘图代码
plot_ribbon <- ggplot(ribbon_df, aes(x = sigma_2)) +
  facet_wrap(~ sigma_1, nrow = 2) +
  geom_ribbon(aes(ymin = prev05, ymax = prev95), fill = "gray80", alpha = 0.7) +
  geom_line(aes(y = prev50), size = 0.5) +
  scale_x_continuous(
    expand = c(0.02, 0.02), 
    limits = c(0.5, 5.0),  # 修改为实际的数据范围
    breaks = seq(0.5, 5.5, by = 1)  # 设置合适的刻度
  ) +
  scale_y_continuous(
    limits = c(0.6, 1.0),  # 根据您的数据范围调整
    breaks = seq(0.6, 1.0, by = 0.1)
  ) +
  ylab("Support for happiness") +
  xlab(expression(tau[2])) +  # 使用下标表示tau2
  theme_bw() +
  theme(
    panel.spacing = unit(0.5, "lines"),  # 增加分面间距
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(margin = margin(0.1, 0, 0.1, 0, "cm")),  # 调整分面标签边距
    axis.text.x = element_text(angle = 0, vjust = 0.5)  # 确保x轴标签可读
  )

# 保存图片，调整尺寸和分辨率
ggsave("happy-tau1-tau2-tot.jpg", plot_ribbon, 
       width = 10, height = 6, dpi = 300)





