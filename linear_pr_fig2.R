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
  vector[N] linear_predictor;
  for (i in 1:N) {
    linear_predictor[i] = alpha + gamma[age[i]] +
                         delta[gender[i]] + epsilon[urban[i]] +
                         income[state[i]] * psi + edu[state[i]] * eta + 
                         zeta[area[state[i]]];
  }
}

model {
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
  array[20, 31] real a_state;     // beta的模拟值
  // 1. 生成观测层面的预测
  vector[N] linear_predictor_rep = linear_predictor;
  
  // 2. 生成beta的模拟（每个state生成n_sim_beta个模拟值）
  for (s in 1:31) {
    for (sim in 1:20) {
      // 从后验预测分布生成新的beta
      a_state[sim, s] = normal_rng(beta[s], sigma_beta);
    }
  }
}

'
write(mrp, file = 'mrp.stan')

mod <- cmdstan_model("mrp.stan")

fit <- mod$sample(
  data = data_for_stan,iter_warmup = 500)


# 使用更高效的计算方法
library(posterior);library(dplyr);library(tidyr)
# 提取 linear_predictor_rep 的均值
linpred <- summarise_draws(
  subset_draws(as_draws_array(fit), variable = "linear_predictor_rep"),
  mean
)$mean

# 获取汇总统计
fit_summary <- fit$summary()

# 筛选 a_state 参数
a_state_summary <- fit_summary[grep("^a_state", fit_summary$variable), ]

# 解析索引
a_state_summary <- a_state_summary %>%
  mutate(
    sim = as.numeric(gsub("a_state\\[([0-9]+),[ ]*([0-9]+)\\]", "\\1", variable)),
    state = as.numeric(gsub("a_state\\[([0-9]+),[ ]*([0-9]+)\\]", "\\2", variable))
  ) %>%
  arrange(sim, state)

# 创建 20×31 矩阵
a_state <- matrix(
  a_state_summary$mean,
  nrow = 20,
  ncol = 31,
  byrow = TRUE
)

invlogit <- function(x) 1 / (1 + exp(-x))

# plot the 8 states

n <- length(df$happy)
y <- df$happy
y.jitter <- y + ifelse (y==0, runif (n, 0, .1), runif (n, -.1, 0))
state.name.all <- c("Anhui", "Beijing", "Fujian", "Gansu", "Guangdong", 
                    "Guangxi", "Guizhou", "Hainan", "Hebei","Henan", 
                    "Heilongjiang", "Hubei", "Hunan", "Jilin", "Jiangsu", 
                    "Jiangxi", "Liaoning", "Inner Mongolia","Ningxia", "Qinghai", 
                    "Shandong", "Shanxi", "Shaanxi", "Shanghai", "Sichuan",
                    "Tianjin", "Xinjiang","Yunnan", "Zhejiang", "Chongqing", "Tibet")

jpeg("state_plots.jpg", 
     width = 12,        # 12英寸宽
     height = 8,        # 8英寸高
     units = "in",      # 指定单位为英寸
     res = 600,         # 分辨率：300 DPI
     pointsize = 8)     # 字号大小
# 直接保存到PDF（不会出现边距问题）
par(mfrow = c(7, 4),  # 7行4列
    mar = c(3, 3, 2, 0.5),    # 增加底部和左侧边距给坐标轴标签更多空间
    mgp = c(1.5, 0.5, 0),     # 调整坐标轴标签和刻度的位置
    oma = c(2, 2, 2, 2),      # 增加外部边距
    tcl = -0.3)               # 缩短刻度线

state_vec <- df$unit
for (j in setdiff(1:31, c(8, 27, 31))) {
  plot(0, 0, xlim = range(linpred), ylim = c(0.6, 0.95), yaxs = "i", pch = 20,
       xlab = "", ylab = "", main = state.name.all[j], type = "n", cex.main = 0.8)
  
  for (s in 1:20) {
    curve(invlogit(a_state[s, j] + x), lwd = 0.3, add = TRUE, col = "gray60")
  }
  
  curve(invlogit(median(a_state[, j]) + x), lwd = 1.2, add = TRUE, col = "black")
  
  if (sum(state_vec == j) > 0) {
    points(linpred[state_vec == j], y.jitter[state_vec == j], 
           cex = 0.4, pch = 16, col = "red")
  }
  
  # 添加坐标轴标签
  mtext("linear predictor", side = 1, line = 1.5, cex = 0.5)
  mtext("Pr(support)", side = 2, line = 1.5, cex = 0.5)
}
dev.off()

