rm(list = ls())
# setwd('D:/cgss/幸福感/elder')
library(cmdstanr);library(tidyverse)
# data preparation
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

# bayesian logistic model

mrp <- '
data {
  int<lower=0> N;
  array[N] int<lower=1, upper=31> state;
  array[N] int<lower=1, upper=3> age;
  array[N] int<lower=1, upper=2> gender, urban;
  array[N] int<lower=0, upper=1> y;
  array[31] real income, edu;
  array[31] int<lower=1, upper=3> area;
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
  real hukou_eff = epsilon[2] - epsilon[1];
  real gender_eff = delta[2]  - delta[1];
  vector[N] predictor_sim;
  array[N] int<lower=0, upper=1> y_sim;
  
  for (i in 1:N) {
    predictor_sim[i] = alpha + beta[state[i]] + gamma[age[i]] + 
      delta[gender[i]] + epsilon[urban[i]] + 
      income[state[i]] * psi + edu[state[i]] * eta + 
      zeta[area[state[i]]];
  }
  
  y_sim = bernoulli_logit_rng(predictor_sim);
  int y_sum = sum(y_sim);
  int<lower=0, upper=1> y_gt = sum(y_sim) >= sum(y);
}

'
write(mrp, file = 'mrp.stan')

mod <- cmdstan_model("mrp.stan")

fit <- mod$sample(
  data = data_for_stan)


library(posterior)
draws <- fit$draws()  # 返回 draws_array 对象

# 计算所有参数的汇总统计,表1
options(scipen = 999)
summary_stats <- summarise_draws(draws) %>% 
  select(-median,-mad,-ess_tail) 
tab1 <- summary_stats %>% 
  slice(1:54) %>%  
  mutate(across(where(is.numeric), \(x) round(x, 2)))

write.csv(tab1,'happy_tab1.csv')

# 给出summary plot:
result <- fit$summary(extra_quantiles = ~ quantile2(.,probs = c(.05,0.25,0.5,0.75,.95)))
# result$variable[1:54]
data <-  result[c(3:4,41:43,46:48,53:54),] %>% 
  mutate(var.name = case_when(variable == "psi"~ 'provincial GDP per capita',   
                              variable == "eta"~ 'Provincial-level average years of schooling', 
                              variable == "gamma[1]" ~ 'Aged 60–64',
                              variable == "gamma[2]" ~ 'Aged 65–69',
                              variable == "gamma[3]" ~ 'Aged 70–74',
                              variable == "zeta[1]" ~ 'East',
                              variable == "zeta[2]" ~ 'Middle',
                              variable == "zeta[3]" ~ 'West',
                              variable == "hukou_eff" ~ 'Hukou',
                              variable == "gender_eff" ~ 'Gender')
  )

fig1 <- ggplot(data, aes(y = reorder(var.name, -seq_along(var.name)))) +
  # 水平误差线（第2.5和第97.5百分位数）- 使用geom_errorbar并指定方向
  geom_errorbar(
    aes(xmin = q5, xmax = q95, y = var.name),
    width = 0,
    color = "black",
    orientation = "y"  # 明确指定水平方向
  ) + 
  # 水平误差线（第25和第75百分位数）
  geom_errorbar(
    aes(xmin = q25, xmax = q75, y = var.name),
    width = 0,
    color = "black",
    linewidth = 1,  # 用linewidth替代size
    orientation = "y"
  ) +
  # 中位数的点
  geom_point(
    aes(x = q50),
    color = "black",
    size = 1  # 点的size参数仍有效
  ) +
  # x 轴范围
  scale_x_continuous(
    name = NULL,
    limits = c(-2.5, 2),
    breaks = seq(-2.5, 1, 0.5),
    sec.axis = dup_axis(name = "")
  ) +
  # 在 x = 0 处添加一条实线
  geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +  # 用linewidth替代size
  # 主题设置
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(hjust = 1),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.5),  # 用linewidth替代size
    axis.line.x.top = element_line(color = "black", linewidth = 0.5)     # 用linewidth替代size
  )

# 增加图表高度并优化显示
ggsave("happy_fig1.jpg", 
       width = 10,      # 增加宽度
       height = 8,      # 增加高度
       dpi = 600,       # 提高分辨率
       bg = "white")    # 确保背景为白色



# goodness of fit
# 打印后验摘要
options(digits = 3) 
result_ppc <- fit$summary() %>% as.data.frame()

# 提取后验样本
draws_df <- fit$draws(variables = c("y_sum", "y_gt"), format = "data.frame")

# 计算y_gt的后验概率
posterior_prob_y_gt <- mean(draws_df$y_gt)

# 绘制y_sum的直方图并添加概率文本
ggplot(draws_df, aes(x = y_sum)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  geom_vline(aes(xintercept = sum(data_for_stan$y)), linetype = "dashed", color = "red")+
  labs(title = paste0("p = ", round(posterior_prob_y_gt, 3)),
    x = "T(y)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5), 
    axis.text.y = element_blank(),           
    axis.ticks.y = element_blank() )     
ggsave('happy_ppc.jpg',
       width = 5,      # 增加宽度
       height = 5,      # 增加高度
       dpi = 600,       # 提高分辨率
       bg = "white")
