rm(list = ls())
path <- 'D:\\CSS\\CSS_data\\dta'
setwd(path)
pkgs <- c('haven','tidyverse');
lapply(pkgs, library, character.only = TRUE)
# CSS13
table(css13$prov_code)
css13 <- read_dta("CSS2013.dta") %>% 
  rename_all(., .funs = tolower) %>% 
  filter(age > 60 & age <= 74) %>% 
  transmute (province = prov_code, 
             social_life_satif = ifelse(e33 > 5 ,1,0))

dat13_lab <- labelled::get_variable_labels(css13) %>% unlist()
dat13_lab[grep('社交',dat13_lab)]
with(css13,table(social_life_satif),useNA = 'always')

labor13 <- css13 %>% group_by(province) %>% 
  summarise(num = n(),
            life_satisf = sum(social_life_satif,na.rm = T))
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
labor13$prov <- province_code[as.character(labor13$province)]

# CSS15
table(css15$N41)
css15 <- haven::read_dta("css2015.dta") %>% 
  rename_all(.,.funs = tolower) %>% 
  filter(2015 - a101d > 60 & 2015 - a101d <= 74, 
         d6_2 > 0) %>% 
  transmute(province = n41,
         social_life_satif = ifelse(d6_2 > 5 ,1,0))

dat15_lab <- labelled::get_variable_labels(css15) %>% unlist()
dat15_lab[grep('社交',dat15_lab)]
with(css15,table(social_life_satif),useNA = 'always')
labor15 <- css15 %>% group_by(province) %>% 
  summarise(num = n(),
            life_satisf = sum(social_life_satif,na.rm = T))

labor15$prov <- gsub("(省|市|自治区|回族|回族自治区|维吾尔自治区|是|壮族自治区)$",
                     '',labor15$province)
# CSS17
table(css17$province)
css17 <- haven::read_dta("css2017.dta") %>% 
  rename_all(.,.funs = tolower) %>%  
  filter(2017 - ra1d > 60 & 2017 - ra1d < 75,d4_2 > 0) %>% 
  transmute(province = province,
            social_life_satif = ifelse(d4_2 > 5 ,1,0))
dat17_lab <- labelled::get_variable_labels(css17) %>% unlist()
dat17_lab[grep('社交',dat17_lab)]
with(css17,table(social_life_satif),useNA = 'always')

labor17 <- css17 %>% group_by(province) %>% 
  summarise(num = n(),
            life_satisf = sum(social_life_satif,na.rm = T)) %>% 
  na.omit()
labor17$prov <- province_code[as.character(labor17$province)] 
# CSS19
table(css19$province)
css19 <- haven::read_dta("css2019.dta") %>% 
  rename_all(.,.funs = tolower) %>% 
  filter(2019 - a1_1_a > 60 & 2019 - a1_1_a < 75) %>% 
  transmute(province = province,
         social_life_satif = ifelse(d2a_5 > 5 ,1,0))
dat19_lab <- labelled::get_variable_labels(css19) %>% unlist()
dat19_lab[grep('社交',dat19_lab)]
with(css19,table(social_life_satif),useNA = 'always')

labor19 <- css19 %>% group_by(province) %>% 
  summarise(num = n(),
            life_satisf = sum(social_life_satif,na.rm = T)) %>% 
  na.omit()
labor19$prov <- province_code[as.character(labor19$province)] 
# CSS21
table(css21$v3_1)
css21 <- haven::read_dta("css2021.dta") %>% 
  rename_all(.,.funs = tolower) %>% 
  filter(2021 - a1c1 >= 60 & 2021 - a1c1 < 75) %>% 
  transmute(province = v3_1, 
         social_life_satif = ifelse(d3a5 > 5 ,1,0))
dat21_lab <- labelled::get_variable_labels(css21) %>% 
  unlist()
dat21_lab[grep('社交',dat21_lab)]
with(css21,table(social_life_satif),useNA = 'always')

labor21 <- css21 %>% group_by(province) %>% 
  summarise(num = n(),
            life_satisf = sum(social_life_satif,na.rm = T)) %>% 
  na.omit()
labor21$prov <- province_code[as.character(labor21$province)]

vars <- c("prov","num","life_satisf")
labor13 <- data.frame(labor13[,vars],wave = 2013)
labor15 <- data.frame(labor15[,vars],wave = 2015)
labor17 <- data.frame(labor17[,vars],wave = 2017)
labor19 <- data.frame(labor19[,vars],wave = 2019)
labor21 <- data.frame(labor21[,vars],wave = 2021)
df <- reduce(list(
                  labor13,labor15,labor17,
                  labor19,labor21),rbind) 
df$prov[df$prov == '内蒙'] <- '内蒙古'

df <- df %>% mutate(life_satisf/num) %>% 
  arrange(prov,wave) %>% na.omit()

df_sum <- df %>% group_by(prov) %>% 
  summarise(num_tot = sum(num),
            co_tot = sum(life_satisf),
            rate = co_tot/num_tot)

dimnames(table(df_sum$prov))
