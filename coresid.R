rm(list = ls())
pkgs <- c('tidyverse','haven','mice','labelled')
lapply(pkgs, library,character.only = T)
setwd("D:\\CSS\\CSS_data")
# CSS2006和2008没有提供同吃同住的信息。从CSS11开始才有了相关信息。
# 受访者与成年子女
# CSS11
# 省市名字不统一
# ---------------------- 1. 定义核心参数（末尾数字匹配的2-16配对） ----------------------
# 同吃同住列（4=不同住）
css11 <- haven::read_dta("css2011.dta") %>%
  rename_all(., .funs = tolower) %>% 
  filter(age > 60 & age <= 74) 


id_nums <- 2:16  # 配对序号：2-16
aa_vars <- paste0("a1aa", id_nums)       # 家庭成员身份列
am_vars <- paste0("a1am1", id_nums)      # 同吃同住列

df11 <- css11 %>%
  rowwise() %>%  # 按行处理
  transmute(province = v41 ,
            live_with_child = any(c_across(all_of(aa_vars)) == 2 & 
                                    c_across(all_of(am_vars)) != 4, na.rm = TRUE)
  ) %>% ungroup()


df11$prov <- gsub("(省|市|自治区|回族|回族自治区|维吾尔自治区|是)$",
                  "", df11$province)
df11$prov[df11$prov == '新建'] <- '新疆'

labor11 <- df11 %>% group_by(prov) %>% 
  summarise(num = n(), 
            coresid = sum(live_with_child))

# CSS13
id_nums <- 6:20
aa_vars <- paste0("a1ab", id_nums,'z')       # 亲属身份列
am_vars <- paste0("a1am1", id_nums)      # 同吃同住列（4=不同住）


css13 <- haven::read_dta("css2013.dta") %>%
  rename_all(., .funs = tolower) %>% 
  filter(age > 60 & age <= 74) %>% 
  select(prov_code,all_of(aa_vars),
         all_of(am_vars)) 


df13 <- css13 %>%
  rowwise() %>%  # 按行处理
  transmute(prov_code = prov_code ,
            live_with_child = any(c_across(all_of(aa_vars)) == 2L & 
                                    c_across(all_of(am_vars)) != 4, na.rm = TRUE)
  ) %>% ungroup()


labor13 <- df13 %>% group_by(prov_code) %>% 
  summarise(num = n(),
            coresid = sum(live_with_child))
# 根据国家统计局编码，改成名称
province_code <- c(
  "11" = "北京", "12" = "天津", "13" = "河北", "14" = "山西", 
  "15" = "内蒙古", "21" = "辽宁", "22" = "吉林", "23" = "黑龙江", 
  "31" = "上海", "32" = "江苏", "33" = "浙江", "34" = "安徽", 
  "35" = "福建", "36" = "江西", "37" = "山东", "41" = "河南", 
  "42" = "湖北", "43" = "湖南", "44" = "广东", "45" = "广西", 
  "46" = "海南", "50" = "重庆", "51" = "四川", "52" = "贵州", 
  "53" = "云南", "54" = "西藏", "61" = "陕西", "62" = "甘肃", 
  "63" = "青海", "64" = "宁夏", "65" = "新疆"
)

# 应用到labor13数据集
labor13$prov <- province_code[as.character(labor13$prov_code)]

get_variable_labels(haven::read_dta("css2015.dta")) %>% unlist %>% 
  write.csv('css2015_lab.csv')
# CSS15
aa_vars <- paste0("a1", sprintf("%02d", 2:25), "b")  # 生成a101b, a102b,...,a125b
am_vars <- paste0("a1", sprintf("%02d", 2:25), "m1") 
css15 <- haven::read_dta("css2015.dta") %>% 
  rename_all(.,.funs = tolower) %>% 
  mutate(province = n41,
         age = 2015 - a101d) %>% 
  filter(age > 60 & age <= 74) %>% 
  select(province,all_of(aa_vars),all_of(am_vars))

df15 <- css15 %>%
  rowwise() %>%  # 按行处理
  transmute(province = province,
            live_with_child = any(c_across(all_of(aa_vars)) == 2 & c_across(all_of(am_vars)) != 4, na.rm = TRUE)
  ) %>% ungroup()


labor15 <- df15 %>% group_by(province) %>% 
  summarise(num = n(), 
            coresid = sum(live_with_child))
labor15$prov <- gsub("(省|市|自治区|回族|回族自治区|维吾尔自治区|是|壮族自治区)$",
                     "", labor15$province)



# CSS17
aa_vars <- paste0("a1a1a_",  c(1:24,30:38))  # 生成a101b, a102b,...,a125b
am_vars <- paste0("a1a1j1_", c(1:24,30:38)) 

css17 <- haven::read_dta("css2017.dta") %>% 
  rename_all(.,.funs = tolower) %>% 
  filter(2017 - ra1d > 60 & 2017 - ra1d < 75) %>% 
  select(province,contains(c('a1a1a_','a1a1j1_')))

# 第一次：省份出现缺失值
table(css17$province,useNA = 'always')

df17 <- css17 %>%
  rowwise() %>%  # 按行处理
  transmute(province = province,
            live_with_child = any(c_across(all_of(aa_vars)) == 2 & c_across(all_of(am_vars)) != 4, na.rm = TRUE)
  ) %>% ungroup()

labor17 <- df17 %>% group_by(province) %>% 
  summarise(num = n(), 
            coresid = sum(live_with_child)) %>% na.omit()
labor17$prov <- province_code[as.character(labor17$province)] 


# CSS19
css19 <- haven::read_dta("css2019.dta") %>% 
  rename_all(.,.funs = tolower) %>% 
  filter(2019 - a1_1_a > 60 & 2019 - a1_1_a < 75) %>% 
  select(province,contains(c('a1a','a1g')))


aa_vars <- paste0("a1a",c(2:30))  # 生成a101b, a102b,...,a125b
am_vars <- paste0("a1g",c(2:30)) 

df19 <- css19 %>%
  rowwise() %>%  # 按行处理
  transmute(province = province,
            live_with_child = any(c_across(all_of(aa_vars)) == 2 & c_across(all_of(am_vars)) != 4, na.rm = TRUE)
  ) %>% ungroup()

labor19 <- df19 %>% group_by(province) %>% 
  summarise(num = n(), 
            coresid = sum(live_with_child))

labor19$prov <- province_code[as.character(labor19$province)]


# CSS21
css21 <- haven::read_dta("css2021.dta") %>% 
  rename_all(.,.funs = tolower) %>%  
  mutate(province = v3_1,
         age = 2021 - a1c1) %>% 
  filter(age >= 60 & age < 75) %>% 
  select(province,contains(c('1a','a1g')))
aa_vars <- paste0("a1a",c(1:30))  # 生成a101b, a102b,...,a125b
am_vars <- paste0("a1g",c(1:30)) 

df21 <- css21 %>%
  rowwise() %>%  # 按行处理
  transmute(province = province,
            live_with_child = any(c_across(all_of(aa_vars)) == 2 & c_across(all_of(am_vars)) != 4, na.rm = TRUE)
  ) %>% ungroup()

labor21 <- df21 %>% group_by(province) %>% 
  summarise(num = n(), 
            coresid = sum(live_with_child))

labor21$prov <- province_code[as.character(labor21$province)]
# 合并6个年份的汇总表
lapply(list(labor11,
            labor13,labor15,labor17,
            labor19,labor21),names)

vars <- c("prov","num","coresid")

labor11 <- data.frame(labor11[,vars],wave = 2011)
labor13 <- data.frame(labor13[,vars],wave = 2013)
labor15 <- data.frame(labor15[,vars],wave = 2015)
labor17 <- data.frame(labor17[,vars],wave = 2017)
labor19 <- data.frame(labor19[,vars],wave = 2019)
labor21 <- data.frame(labor21[,vars],wave = 2021)
df <- reduce(list(labor11,
                  labor13,labor15,labor17,
                  labor19,labor21),rbind) 
df$prov[df$prov == '内蒙'] <- '内蒙古'

df <- df %>% mutate(coresid/num) %>% 
  arrange(prov,wave) %>% na.omit()

df_sum <- df %>% group_by(prov) %>% 
  summarise(num_tot = sum(num),
            co_tot = sum(coresid),
            rate = co_tot/num_tot)
write.csv(df,'D:/cgss/幸福感/elder/revision to plosone/df_coresid.csv')
write.csv(df_sum,'D:/cgss/幸福感/elder/revision to plosone/coresid.csv')

