rm(list = ls())
library(tidyverse);library(haven);library(mice)
library(labelled)
# healthcare accessibility:22-10住院情况
# elderly support security:pension replacement rate,
# urban-rural income gap:收入比
# 6-24城镇人均可支配收入，6-30农村人均可支配收入
# and environmental quality:8-21森林覆盖率
# 以上数据来自2020年统计年鉴
setwd('D:\\cgss\\幸福感\\elder\\revision to plosone')

outpatient <- readxl::read_excel('门诊服务20.xlsx',
                                 range = 'A3:I36') %>% 
  select(1,9) %>% slice(-c(1:2)) %>% 
  setNames(c('prov','num_of_visitor'))
outpatient$prov <- gsub(' ','',outpatient$prov)

ind_urban <- readxl::read_excel('城镇收入20.xlsx',
                                 range = 'A4:I37') %>% 
  select(1,9) %>% slice(-c(1:2)) %>% 
  setNames(c('prov','ind_urban'))
ind_urban$prov <- gsub(' ','',ind_urban$prov)

ind_rural <- readxl::read_excel('农村收入20.xlsx',
                                 range = 'A4:I37') %>% 
  select(1,9) %>% slice(-c(1:2)) %>% 
  setNames(c('prov','ind_rural'))
ind_rural$prov <- gsub(' ','',ind_rural$prov)

df1 <- reduce(list(outpatient,ind_urban,ind_rural),
       function(x,y) merge(x,y,by = 'prov',all = T))



# 劳动参与率，基于CSS数据
cgss18 <- haven::read_dta("D:\\cgss\\2018\\CGSS2018.dta") %>% 
  rename_all(., .funs = tolower) %>% filter(a31 <= 2018-60 )
names(cgss18) %>% head()
apply(cgss18 %>% select(a53,a58),2,table,useNA='always')
cgss18$iswork <- ifelse(cgss18$a58 >=4,1,0)

labor_participation <- prop.table(with(cgss18,table(iswork,s41)),2)[2, ]
str(labor_participation)
table(cgss18$s41) %>% length()
# 逻辑链如下：
# 缺少14(新疆)，20(海南)，and 25(西藏)，考虑用CSS数据
# 策略：合并历年60-75岁的老年人，确定其劳动参与率
# 调查员读出以下对于“工作”的解释：
# 这里所说的工作是指：
# 1、最近一周以来从事过1小时以上有收入的工作；
# 2、在自己/自己家庭或家族拥有的
# 企业/机构中工作，虽然没报酬，但每周工作在15小时以上或每天工作3小时以上； 
# 3、参加农业生产劳动。
# 符合上述三者之一，即算作有工作。
# 4、学生的勤工俭学不算参加工作。
setwd("D:\\CSS\\CSS_data")
css06 <- read_dta("css2006.dta") %>% 
  rename_all(., .funs = tolower) 
lab06 <- labelled::get_variable_labels(css06) %>% 
  tibble::enframe(name = "variable_name",
                  value = "variable_label")
labelled::get_value_labels(css06$qe02_04)
table(css06$qe02_04,useNA = 'always')
lab06$variable_label[grep('环境',lab06$variable_label)]

df06 <- css06 %>% 
  select(qs2a,qa02,qb1,qe02_04) %>% 
  filter (qa02 >= 60 & qa02 < 75) %>% 
  transmute(province = labelled::to_factor(qs2a),
            age = qa02,
            iswork = ifelse(qb1 != 1,1,0),
            envir_proc = ifelse(qe02_04 %in% c(3,4),1,0))
df06$prov <- gsub("(省|市|自治区)$", "", df06$province)

# 不回答 
# -9901 
# 没有工作,1;有工作,2;离/退休后再工作,3 
apply(df06,2,table,useNA = 'always')

labor06 <- df06 %>% group_by(prov) %>% 
  summarise(num = n(),
            participation = sum(iswork),
             rate = mean(iswork))

# CSS08
# 目前的就业状况 目前的就业状况【【【出示卡片 【出示卡片 2】】】 】
# 01.全职工作；02.半职工作；03.临时性工作（无合同、非稳定的工作）；
# 04.全职务农；05.兼业务农；06.离退休，目前无工作； 
# 07.失业/下岗，目前无工作；08.以前工作（务农）过，但现在无工作；
# 09.从未工作过；10.在学且无工作

css08 <- haven::read_dta("css2008.dta") %>% 
  rename_all(., .funs = tolower) 
lab08 <- labelled::get_variable_labels(css08) %>% 
  tibble::enframe(name = "variable_name",
                  value = "variable_label")
labelled::get_value_labels(css08$b1)

df08 <- css08 %>% 
   select(province,age1,b1) %>% 
  filter(age1 >= 60 & age1 < 75) %>%  
  transmute(prov = province,
            age = age1,
            iswork = ifelse(b1 != 3,1,0))


labor08 <- df08 %>% group_by(prov) %>% 
  summarise(num = n(), 
            participation = sum(iswork),
            rate = mean(iswork))

# CSS11
# 省市名字不统一
css11 <- haven::read_dta("css2011.dta") %>% 
  rename_all(., .funs = tolower) 

lab11 <- labelled::get_variable_labels(css11) %>% 
  tibble::enframe(name = "variable_name",
                  value = "variable_label")
df11 <- css11 %>% 
  select(v41,age,b1) %>% 
  filter(age >= 60 & age < 75) %>% 
  na.omit() %>% transmute( province = v41,
                           age = age,
                           iswork = ifelse(b1 != 3,1,0)) 

df11$prov <- gsub("(省|市|自治区|回族|回族自治区|维吾尔自治区|是)$",
                  "", df11$province)
df11$prov[df11$prov == '新建'] <- '新疆'
table(df11$prov,useNA = 'always')

var_11 <- labelled::get_variable_labels(css11) %>% 
  tibble::enframe(name = "variable_name",
                  value = "variable_label") 
var_11$variable_label[grep('省',var_11$variable_label)]

labor11 <- df11 %>% group_by(prov) %>% 
  summarise(num = n(), 
            participation = sum(iswork),
            rate = mean(iswork))

# CSS13
css13 <- haven::read_dta("css2013.dta") %>% 
  rename_all(., .funs = tolower) 
df13 <- css13 %>% 
  select(prov_code,age,b1) %>% 
  transmute(province = prov_code,
            age = age,
            iswork = ifelse(b1 != 3,1,0)) %>% 
  filter(age >= 60 & age < 75)
labelled::get_value_labels(css13$b1)
str(df13)
# 有工作,1 
# 有工作，但目前休假、学习，或临时停工、歇业,2 
# 没有工作,3 
var_13 <- labelled::get_variable_labels(css13) %>% 
  tibble::enframe(name = "variable_name",
                  value = "variable_label") 

apply(css13,2,table,useNA='always')

labelled::get_value_labels(css13$b1) 


labor13 <- df13 %>% group_by(province) %>% 
  summarise(num = n(), 
            participation = sum(iswork),
            rate = mean(iswork))
# 编码改成名称
# 创建统计局编码到省份名称的映射
# 只保留省份核心名称的映射表
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
css15 <- haven::read_dta("css2015.dta") %>% 
  rename_all(.,.funs = tolower) 
var_15 <- labelled::get_variable_labels(css15) %>% 
  tibble::enframe(name = "variable_name",
                  value = "variable_label") 
labelled::get_value_labels(css15$b1)
df15 <- css15  %>% 
  select(n41,a101d,b1) %>% 
  mutate(province = n41,
         age = 2015 - a101d,
         iswork = ifelse(b1 != 3,1,0)) %>% 
  select(province,age,iswork) %>%  
  filter(age >= 60 & age < 75)


var_15 <- labelled::get_variable_labels(css15) %>% 
  tibble::enframe(name = "variable_name",
                  value = "variable_label") 

var_15$variable_label[grep('目前的工作',var_15$variable_label)]

labor15 <- df15 %>% group_by(province) %>% 
  summarise(num = n(), 
            participation = sum(iswork),
              rate = mean(iswork))
labor15$prov <- gsub("(省|市|自治区|回族|回族自治区|维吾尔自治区|是|壮族自治区)$",
                     "", labor15$province)
# CSS17

css17_sav <- haven::read_sav("CSS2017/CSS2017SPSS版本数据.sav") %>% 
  rename_all(.,.funs = tolower)  
table(css17_sav$province,useNA = 'always')
 
css17 <- haven::read_dta("css2017.dta") %>% 
  rename_all(.,.funs = tolower)  
# 第一次：省份出现缺失值
table(css17$province,useNA = 'always')

write.csv(css17,'css17.csv')
df17 <- css17 %>% 
  mutate(age = 2017 - ra1d,
          iswork = ifelse(b1 !=3,1,0)) %>% 
  select(province,age,iswork) %>% 
  filter(age >= 60 & age < 75)


var_17 <- labelled::get_variable_labels(css17) %>% 
  tibble::enframe(name = "variable_name",
                  value = "variable_label") 


labor17 <- df17 %>% group_by(province) %>% 
  summarise(num = n(), 
            participation = sum(iswork),
              rate = mean(iswork))
labor17$prov <- province_code[as.character(labor17$province)] 


# CSS19
css19 <- haven::read_dta("css2019.dta") 
df19 <- css19 %>% 
  select(province,a1_1_a,b1) %>% 
  mutate(age = 2019 - a1_1_a,
         iswork = ifelse(b1 !=3,1,0) ) %>% 
  filter(age >= 60 & age < 75) %>% 
  select(province,age,iswork) %>% na.omit()

lab_19 <- labelled::get_variable_labels(css19) %>% 
  tibble::enframe(name = "variable_name", 
                  value = "label")

labor19 <- df19 %>% group_by(province) %>% 
  summarise(num = n(), 
            participation = sum(iswork),
            rate = mean(iswork))

labor19$prov <- province_code[as.character(labor19$province)]


# CSS21
css21 <- haven::read_dta("css2021.dta") 
df21 <- css21 %>% 
  select(v3_1,a1c1,b1) %>% 
  transmute(province = v3_1,
            age = 2021 - a1c1, 
            iswork = ifelse(b1 != 3,1,0)) %>% 
  filter(age >= 60 & age < 75)

labelled::get_value_labels(css21$iswork)

var_21 <- labelled::get_variable_labels(css21)
lab_21 <- data.frame(
  variable_name = names(var_21),
  label = as.character(unlist(var_21)),
  stringsAsFactors = FALSE
)
lab_21$label[grep('您',lab_21$label)]

labor21 <- df21 %>% group_by(province) %>% 
  summarise(num = n(), 
             participation = sum(iswork),
             rate = mean(iswork))

labor21$prov <- province_code[as.character(labor21$province)]
# 合并8个年份的汇总表
lapply(list(labor06,labor08,labor11,
            labor13,labor15,labor17,
            labor19,labor21),names)
vars <- c("prov","num","participation")
labor06 <- data.frame(labor06[,vars],wave = 2006)
labor08 <- data.frame(labor08[,vars],wave = 2008)
labor11 <- data.frame(labor11[,vars],wave = 2011)
labor13 <- data.frame(labor13[,vars],wave = 2013)
labor15 <- data.frame(labor15[,vars],wave = 2015)
labor17 <- data.frame(labor17[,vars],wave = 2017)
labor19 <- data.frame(labor19[,vars],wave = 2019)
labor21 <- data.frame(labor21[,vars],wave = 2021)
df <- reduce(list(labor06,labor08,labor11,
            labor13,labor15,labor17,
            labor19,labor21),rbind) %>% 
  arrange(prov) %>% na.omit()
df$prov[df$prov == '内蒙'] <- '内蒙古'

df_sum <- df %>% group_by(prov) %>% 
  summarise(num_tot = sum(num),
            par_tot = sum(participation),
            rate = par_tot/num_tot)
write.csv(df_sum,'D:/cgss/幸福感/elder/revision to plosone/labor_participation.csv')

