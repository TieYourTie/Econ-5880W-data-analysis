###############################################################################
# Machine Learning Practice: Data Cleaning and Variable Selection
# 本文件用于机器学习实践，主要进行数据清洗和变量筛选
###############################################################################

# 清除环境中所有对象
rm(list = ls())

# 载入所需 R 套件（确保已安装）
library(lubridate)    
library(fredr)        
library(mFilter)      
library(neverhpfilter)
library(tsbox)
library(RColorBrewer)
library(plotly)
library(wesanderson)
library(writexl)
library(tidyverse)
library(readr)
library(haven)

###############################################################################
# 1.0 数据加载与预处理
###############################################################################

# 1.1 读取数据并初步筛选
canadian_election  <- read_dta("Google Drive/我的云端硬盘/Mac things/2025 winter/Econ 5880W/5880 project/Canadian election/2021 Canadian Election Study v2.0.dta")
View(X2021_Canadian_Election_Study_v2_0)

# 找出不重要的列名：点击信息、文本回答、同意条款、验证码、邮寄选票、"pr" 字样
remove_cols <- colnames(canadian_election)[grepl("First_Click|Last_Click|t_Click|TEXT|consent|captcha|mail-in ballot|\\bpr\\b", 
                                                 colnames(canadian_election))]

# 删除这些列
cecc  <- canadian_election[, !(colnames(canadian_election) %in% remove_cols)]

# 删除全是缺失值的列
empty_cols <- names(cecc)[colSums(is.na(cecc)) == nrow(cecc)]
cecc <- cecc[, !(colnames(cecc) %in% empty_cols)]

# 1.2 手动处理一些必要的变量
# 数据质量筛选：仅保留 data_quality 为 0 的行（表示有效数据）
cecc <- cecc %>%
  filter(cps21_data_quality == 0, 
         pes21_data_quality == 0)

# 保留加拿大公民（citizenship == 1）
cecc <- cecc %>%
  filter(cps21_citizenship == 1)

# 保留对民主满意度回答非缺失（pes21_dem_sat != 5）者
cecc <- cecc %>%
  filter(pes21_dem_sat != 5)


###############################################################################
#The Variable processing
###############################################################################
# 语言处理：创建语言虚拟变量（英语为 1，其他为 0）
cecc <- cecc %>%
  mutate(UserLanguage_dummy = ifelse(UserLanguage == "EN", 1, 0))


# 年龄处理：出生年份是“自 1919 年起的编码”，实际年份 = 编码 + 1919
# 年龄 = 2021 - 出生年份
cecc<- cecc %>%
  mutate(
    birth_year = cps21_yob + 1919,
    age = 2021 - birth_year
  )


##############transgender_dummy 
# Create the transgender dummy variable and remove genderid == 4
cecc <- cecc %>%
  mutate(transgender = if_else(cps21_trans == 1, 1, 0)) %>%  # 1 if transgender, 0 otherwise
  filter(cps21_genderid != 4) %>%  # Remove rows where gender identity is 4 (another gender) 
  filter(cps21_trans!= 3)



###############################################################################
# Machine Learning Practice: Data Cleaning and Variable Selection
# 本文件用于机器学习实践，主要进行数据清洗和变量筛选
###############################################################################

# 清除环境中所有对象
rm(list = ls())

# 载入所需 R 套件（确保已安装）
library(lubridate)    
library(fredr)        
library(mFilter)      
library(neverhpfilter)
library(tsbox)
library(RColorBrewer)
library(plotly)
library(wesanderson)
library(writexl)
library(tidyverse)
library(readr)
library(haven)

###############################################################################
# 1.0 数据加载与预处理
###############################################################################

# 1.1 读取数据并初步筛选
canadian_election  <- read_dta("Google Drive/我的云端硬盘/Mac things/2025 winter/Econ 5880W/5880 project/Canadian election/2021 Canadian Election Study v2.0.dta")
View(X2021_Canadian_Election_Study_v2_0)

# 找出不重要的列名：点击信息、文本回答、同意条款、验证码、邮寄选票、"pr" 字样
remove_cols <- colnames(canadian_election)[grepl("First_Click|Last_Click|t_Click|TEXT|consent|captcha|mail-in ballot|\\bpr\\b", 
                                                 colnames(canadian_election))]

# 删除这些列
cecc  <- canadian_election[, !(colnames(canadian_election) %in% remove_cols)]

# 删除全是缺失值的列
empty_cols <- names(cecc)[colSums(is.na(cecc)) == nrow(cecc)]
cecc <- cecc[, !(colnames(cecc) %in% empty_cols)]

# 1.2 手动处理一些必要的变量
# 数据质量筛选：仅保留 data_quality 为 0 的行（表示有效数据）
cecc <- cecc %>%
  filter(cps21_data_quality == 0, 
         pes21_data_quality == 0)

# 保留加拿大公民（citizenship == 1）
cecc <- cecc %>%
  filter(cps21_citizenship == 1)

# 保留对民主满意度回答非缺失（pes21_dem_sat != 5）者
cecc <- cecc %>%
  filter(pes21_dem_sat != 5)


###############################################################################
#The Variable processing
###############################################################################
# 语言处理：创建语言虚拟变量（英语为 1，其他为 0）
cecc <- cecc %>%
  mutate(UserLanguage_dummy = ifelse(UserLanguage == "EN", 1, 0))


# 年龄处理：出生年份是“自 1919 年起的编码”，实际年份 = 编码 + 1919
# 年龄 = 2021 - 出生年份
cecc<- cecc %>%
  mutate(
    birth_year = cps21_yob + 1919,
    age = 2021 - birth_year
  )


##############transgender_dummy 
# Create the transgender dummy variable and remove genderid == 4
cecc <- cecc %>%
  mutate(transgender = if_else(cps21_trans == 1, 1, 0)) %>%  # 1 if transgender, 0 otherwise
  filter(cps21_genderid != 4) %>%  # Remove rows where gender identity is 4 (another gender) 
  filter(cps21_trans!= 3)
###############################################################################



################################################################################
#select the only important variable
################################################################################

# 提取指定变量
cecc_selected <- cecc %>%
  select(
    pes21_votechoice2021,
    # 自定义变量
    UserLanguage_dummy,
    age,
    transgender,
  
    # 人口统计
    cps21_genderid,
    cps21_province,
    cps21_education,
    
    # 政治兴趣与态度
    cps21_demsat,
    cps21_interest_gen_1,
    cps21_interest_elxn_1,
    
    # 支出偏好
    cps21_spend_env,
    cps21_spend_imm_min,
    cps21_spend_rec_indi,
    cps21_spend_afford_h,
    
    # 群体热度感知（温度计）
    cps21_groups_therm_1,
    cps21_groups_therm_2,
    cps21_groups_therm_7,
    cps21_groups_therm_3,
    cps21_groups_therm_4,
    cps21_groups_therm_6,
    
    # 疫情相关
    cps21_covid_liberty,
    cps21_most_seats_1,
    cps21_most_seats_2,
    cps21_most_seats_3,
    cps21_most_seats_4,
    cps21_most_seats_5,
    cps21_outcome_most,
    
    # 疫苗态度
    cps21_vaccine_mandat_1,
    cps21_vaccine_mandat_2,
    cps21_vaccine_mandat_3,
    cps21_vaccine1,
    
    # 选后民主态度
    pes21_dem_sat,
    pes21_losetouch,
    pes21_govtcare,
    
    # 民粹/价值观等
    pes21_hatespeech,
    pes21_envirojob,
    pes21_discfam,
    pes21_famvalues,
    pes21_equalrights,
    pes21_fitin,
    pes21_immigjobs,
  )



#################################################################
#2. The Variable selection and cleaning
#################################################################
#1 The Y varable
cecc_selected <- cecc_selected %>%
  mutate(
    pes21_dem_sat = na_if(pes21_dem_sat, 5),
    y = cps21_demsat - pes21_dem_sat
  ) %>%
  drop_na(y)


#######################################################
#2.1 Gender
#######################################################
#cps21_genderid
cecc_selected <- dummy_cols(
  cecc_selected,
    select_columns = "cps21_genderid",
    remove_first_dummy = FALSE,
    remove_most_frequent_dummy = FALSE,
    ignore_na = TRUE,
    split = NULL,
    remove_selected_columns = TRUE,
    omit_colname_prefix = FALSE,
    return_generated_variables = FALSE
  )

#rename the new dummy variable 
cecc_selected <- cecc_selected %>% 
  rename("cps21_genderid_1" = "gender_man",
         "cps21_genderid_2" = "gender_woman",
         "cps21_genderid_3" = "gender_nonbinary")


#cps21_genderid
cecc_evil <- cecc_selected %>%
  mutate(
    gender_man = ifelse(cps21_genderid == 1, 1, 0),
    gender_woman = ifelse(cps21_genderid == 2, 1, 0),
    gender_nonbinary = ifelse(cps21_genderid == 3, 1, 0)
  )


#######################################################
#2.2 province
#######################################################

# 创建 dummy
cecc_selected <- dummy_cols(
  cecc_selected,
  select_columns = "cps21_province",
  remove_first_dummy = TRUE,
  remove_selected_columns = TRUE
)

# 重命名列名为省名
cecc_selected <- cecc_selected %>%
  rename(
    province_AB = cps21_province_1,
    province_BC = cps21_province_2,
    province_Manitoba = cps21_province_3,
    province_NB = cps21_province_4,
    province_NL = cps21_province_5,
    province_NWT = cps21_province_6,
    province_NS = cps21_province_7,
    province_Nunavut = cps21_province_8,
    province_Ontario = cps21_province_9,
    province_PEI = cps21_province_10,
    province_Quebec = cps21_province_11,
    province_Saskatchewan = cps21_province_12,
    province_Yukon = cps21_province_13
  )

#######################################################
#2.3 education
#######################################################
#education
cecc_selected <- cecc_selected %>%
  mutate(
    edu_noschooling = ifelse(cps21_education == 1, 1, 0),
    edu_elementary_some = ifelse(cps21_education == 2, 1, 0),
    edu_elementary_done = ifelse(cps21_education == 3, 1, 0),
    edu_highschool_some = ifelse(cps21_education == 4, 1, 0),
    edu_highschool_done = ifelse(cps21_education == 5, 1, 0),
    edu_college_some = ifelse(cps21_education == 6, 1, 0),
    edu_college_done = ifelse(cps21_education == 7, 1, 0),
    edu_university_some = ifelse(cps21_education == 8, 1, 0),
    edu_bachelor = ifelse(cps21_education == 9, 1, 0),
    edu_master = ifelse(cps21_education == 10, 1, 0),
    edu_phd = ifelse(cps21_education == 11, 1, 0),
    edu_dk_pna = ifelse(cps21_education == 12, 1, 0)
  ) %>%
  select(-cps21_education)  # 可选：删除原始变量

#interested in the polical 
cecc_selected <- cecc_selected %>%
  mutate(
    political_interest = na_if(cps21_interest_gen_1, -99)
  ) %>%
  select(-cps21_interest_gen_1)  # 可选：删除原始变量

#cecc_selected$cps21_v_likely (remove this variable) 
cecc_selected <-cecc_selected %>%
  select(-cps21_v_likely)

#######################################################
#2.4 confortable to vote During the Covid
#######################################################

#comfort to vote in covid
cecc_selected <- cecc_selected %>%
  mutate(
    comfort_vote_raw = coalesce(cps21_comfort1, cps21_comfort2, cps21_comfort3)
  )

cecc_selected <- cecc_selected %>%
  mutate(
    comfort_vote = na_if(comfort_vote_raw, 5)
  ) %>%
  select(-comfort_vote_raw, -cps21_comfort1, -cps21_comfort2, -cps21_comfort3)

# 处理 spending variables 为 ordered
cecc_selected <- cecc_selected %>%
  mutate(
    spend_env = ordered(cps21_spend_env, levels = c(1, 2, 3)),
    spend_imm_min = ordered(cps21_spend_imm_min, levels = c(1, 2, 3)),
    spend_rec_indi = ordered(cps21_spend_rec_indi, levels = c(1, 2, 3)),
    spend_afford_h = ordered(cps21_spend_afford_h, levels = c(1, 2, 3))
  )


#######################################################
#2.5 How do you feel the following group
#######################################################

#温度计 
cecc_selected <- cecc_selected %>%
  mutate(across(starts_with("cps21_groups_therm_"), 
                ~ na_if(., -99)))

#######################################################
#2.6 Covid Liberty
#######################################################
#cps21_covid_liberty The public health recommendations aimed at slowing the spread
#of the COVID-19 virus are threatening my liberty.

#covid
cecc_selected <- cecc_selected %>%
  mutate(
    covid_liberty = na_if(cps21_covid_liberty, 6),
    covid_liberty = ordered(covid_liberty, levels = c(1, 2, 3, 4, 5))
  )

#######################################################
#2.7 cps21_most_seats
#######################################################
# cps21_most_seats For each of the parties below, how likely is each party to win the
# most seats in the House of Commons?
  


cecc_selected <- cecc_selected %>%
  mutate(across(starts_with("cps21_most_seats_"), ~ na_if(., -99))) %>%
  mutate(across(starts_with("cps21_most_seats_"), scale, .names = "{.col}_z"))



#vaccine
cecc_selected <- cecc_selected %>%
  mutate(across(starts_with("cps21_vaccine_mandat_"), 
                ~ ordered(na_if(., 5), levels = 1:4)))

cecc_selected <- cecc_selected %>%
  mutate(vaccine_status = ordered(cps21_vaccine1, levels = c(3, 2, 1),
                                  labels = c("No", "OneDose", "TwoOrMore")))

cecc_selected <- cecc_selected %>%
  mutate(losetouch = na_if(pes21_losetouch, 6)) %>%
  mutate(losetouch = ordered(losetouch, levels = 1:5))


cecc_selected <- cecc_selected %>%
  mutate(outcome_most = na_if(cps21_outcome_most, 8)) %>%
  mutate(outcome_most = factor(outcome_most,
                               levels = 1:7,
                               labels = c("Lib_Maj", "Con_Maj", "NDP_Maj",
                                          "Lib_Min", "Con_Min", "NDP_Min", "Other")))

cecc_selected <- dummy_cols(cecc_selected,
                            select_columns = "outcome_most",
                            remove_first_dummy = TRUE,  # 防止完全共线性
                            remove_selected_columns = TRUE)




# 移除 'Don't know / Prefer not to answer' 的观测
cecc_selected  <- cecc_selected  %>%
  filter(
    pes21_govtcare != 6,
    pes21_hatespeech != 6,
    pes21_envirojob != 6,
    pes21_discfam != 5
  )

# 创建 dummy variables
cecc_selected  <- cecc_selected  %>%
  mutate(
    ## pes21_govtcare - The government does not care much about what people like me think
    govtcare_sd = ifelse(pes21_govtcare == 1, 1, 0),
    govtcare_swd = ifelse(pes21_govtcare == 2, 1, 0),
    govtcare_neutral = ifelse(pes21_govtcare == 3, 1, 0),
    govtcare_swa = ifelse(pes21_govtcare == 4, 1, 0),
    govtcare_sa = ifelse(pes21_govtcare == 5, 1, 0),
    
    ## pes21_hatespeech - It should be illegal to say hateful things publicly
    hatespeech_sd = ifelse(pes21_hatespeech == 1, 1, 0),
    hatespeech_swd = ifelse(pes21_hatespeech == 2, 1, 0),
    hatespeech_neutral = ifelse(pes21_hatespeech == 3, 1, 0),
    hatespeech_swa = ifelse(pes21_hatespeech == 4, 1, 0),
    hatespeech_sa = ifelse(pes21_hatespeech == 5, 1, 0),
    
    ## pes21_envirojob - When there's a conflict between jobs and environment
    envirojob_sd = ifelse(pes21_envirojob == 1, 1, 0),
    envirojob_swd = ifelse(pes21_envirojob == 2, 1, 0),
    envirojob_neutral = ifelse(pes21_envirojob == 3, 1, 0),
    envirojob_swa = ifelse(pes21_envirojob == 4, 1, 0),
    envirojob_sa = ifelse(pes21_envirojob == 5, 1, 0),
    
    ## pes21_discfam - Discuss politics with friends/family
    discfam_never = ifelse(pes21_discfam == 1, 1, 0),
    discfam_sometimes = ifelse(pes21_discfam == 2, 1, 0),
    discfam_often = ifelse(pes21_discfam == 3, 1, 0),
    discfam_alltime = ifelse(pes21_discfam == 4, 1, 0)
  )




# 移除无效值
cecc_selected  <- cecc_selected  %>%
  filter(
    pes21_famvalues != 6,
    pes21_equalrights != 6,
    pes21_fitin != 6,
    pes21_immigjobs != 6
  )

# 创建 dummy variables
cecc_selected  <- cecc_selected  %>%
  mutate(
    # pes21_famvalues
    famvalues_sd = ifelse(pes21_famvalues == 1, 1, 0),
    famvalues_swd = ifelse(pes21_famvalues == 2, 1, 0),
    famvalues_neutral = ifelse(pes21_famvalues == 3, 1, 0),
    famvalues_swa = ifelse(pes21_famvalues == 4, 1, 0),
    famvalues_sa = ifelse(pes21_famvalues == 5, 1, 0),
    
    # pes21_equalrights
    equalrights_sd = ifelse(pes21_equalrights == 1, 1, 0),
    equalrights_swd = ifelse(pes21_equalrights == 2, 1, 0),
    equalrights_neutral = ifelse(pes21_equalrights == 3, 1, 0),
    equalrights_swa = ifelse(pes21_equalrights == 4, 1, 0),
    equalrights_sa = ifelse(pes21_equalrights == 5, 1, 0),
    
    # pes21_fitin
    fitin_sd = ifelse(pes21_fitin == 1, 1, 0),
    fitin_swd = ifelse(pes21_fitin == 2, 1, 0),
    fitin_neutral = ifelse(pes21_fitin == 3, 1, 0),
    fitin_swa = ifelse(pes21_fitin == 4, 1, 0),
    fitin_sa = ifelse(pes21_fitin == 5, 1, 0),
    
    # pes21_immigjobs
    immigjobs_sd = ifelse(pes21_immigjobs == 1, 1, 0),
    immigjobs_swd = ifelse(pes21_immigjobs == 2, 1, 0),
    immigjobs_neutral = ifelse(pes21_immigjobs == 3, 1, 0),
    immigjobs_swa = ifelse(pes21_immigjobs == 4, 1, 0),
    immigjobs_sa = ifelse(pes21_immigjobs == 5, 1, 0)
  )

cecc_selected <- cecc_selected %>%
  filter(pes21_votechoice2021 != 9) %>%  # 移除 'Don't know / Prefer not to answer'
  mutate(
    treatment = ifelse(pes21_votechoice2021 == 1, 1, 0)  # Liberal = 1, others = 0
  )


# 查看各变量缺失比例
missing_proportions <- cecc_selected %>%
  summarise(across(everything(), ~mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_rate")

# 标记高缺失率变量（>30%）
high_missing_vars <- missing_proportions %>% 
  filter(missing_rate > 0.3) %>% 
  pull(variable)

# 建议删除这些高缺失变量
cecc_reduced <- cecc_selected %>% 
  select(-all_of(high_missing_vars))


#####now its the time to start to build the DDML




# 保留 treatment, y, 和所有相关变量（根据上面逻辑）
ddml_data <- cecc_reduced %>%
  select(
    y, treatment,
    starts_with("gender_"),
    starts_with("province_"),
    starts_with("edu_"),
    political_interest,
    comfort_vote,
    starts_with("vaccine_"),
    starts_with("cps21_vaccine_mandat_"),
    starts_with("govtcare_"),
    starts_with("hatespeech_"),
    starts_with("envirojob_"),
    starts_with("discfam_"),
    starts_with("famvalues_"),
    starts_with("equalrights_"),
    starts_with("fitin_"),
    starts_with("immigjobs_"),
    starts_with("outcome_most_"),
    starts_with("cps21_most_seats_")
  )



ddml_data <- ddml_data %>%
  mutate(vaccine_binary = ifelse(vaccine_status == "TwoOrMore", 1, 0))

ddml_data <- ddml_data %>%
  select(-vaccine_status)

table(ddml_data$vaccine_binary)

ddml_data <- ddml_data %>%
  mutate(
    mandate_travel = ifelse(cps21_vaccine_mandat_1 == "Agree", 1, 0),
    mandate_restaurant = ifelse(cps21_vaccine_mandat_2 == "Agree", 1, 0),
    mandate_hospital = ifelse(cps21_vaccine_mandat_3 == "Agree", 1, 0)
  )

ddml_data <- ddml_data %>%
  select(-cps21_vaccine_mandat_1, -cps21_vaccine_mandat_2, -cps21_vaccine_mandat_3)

ddml_data <- ddml_data %>%
  select(-comfort_vote)

ddml_data <- ddml_data %>%
  mutate(political_interest = as.numeric(political_interest))


#check the structure


# y = outcome variable
y <- -ddml_data$y

# d = treatment (Liberal vote)
d <- ddml_data$treatment

# x = covariates (drop y and treatment)
x <- ddml_data %>%
  select(-y, -treatment) %>%
  as.data.frame()

#########

library(mlr3)
library(mlr3learners)
library(mlr3verse)

library(tidyr)

ddml_data_clean <- ddml_data %>%
  drop_na()

ddml_data_clean <- as.data.frame(ddml_data_clean)

df_doubleml <- DoubleMLData$new(
  data = ddml_data_clean,
  y_col = "y",
  d_cols = "treatment",
  x_cols = setdiff(names(ddml_data_clean), c("y", "treatment"))
)


learner <- lrn("regr.cv_glmnet")

dml_plr_obj <- DoubleMLPLR$new(
  data = df_doubleml,
  ml_l = learner,     # 注意是 ml_l（旧版叫 ml_g）
  ml_m = learner,
  n_folds = 5
)

# 拟合模型
dml_plr_obj$fit()

# 查看结果
dml_plr_obj$summary()

######

ddml_data_clean <- as.data.frame(ddml_data_clean)


# 构建 DoubleML 数据对象
df_doubleml <- DoubleMLData$new(
  data = ddml_data_clean,
  y_col = "y",
  d_cols = "treatment",
  x_cols = setdiff(names(ddml_data_clean), c("y", "treatment"))
)

# 设置随机森林学习器（开启变量重要性）
learner_rf <- lrn(
  "regr.ranger",
  num.trees = 1000,             # 更多的树
  mtry = floor(sqrt(ncol(ddml_data_clean) - 2)),  # sqrt(p) 通常是默认经验值
  min.node.size = 5,            # 每个叶节点的最小样本数
  sample.fraction = 0.8,        # 80% bootstrap采样
  max.depth = 15,               # 限制最大深度，防止过拟合
  importance = "impurity",      # 启用变量重要性
  num.threads = parallel::detectCores()  # 最大化 CPU 使用
)

# 构建 PLR 模型
dml_rf <- DoubleMLPLR$new(
  data = df_doubleml,
  ml_l = learner_rf,  # Learner for E[Y|X]
  ml_m = learner_rf,  # Learner for E[D|X]
  n_folds = 5
)

# 拟合模型
dml_rf$fit()
# 查看结果
dml_rf$summary()


#####
param_set <- paradox::ps(
  num.trees = paradox::p_int(lower = 200, upper = 1000),
  mtry = paradox::p_int(lower = 5, upper = floor(ncol(ddml_data_clean) / 3)),
  min.node.size = paradox::p_int(lower = 1, upper = 10),
  max.depth = paradox::p_int(lower = 5, upper = 20)
)

# 定义 resampling 策略（比如 3 折交叉验证）
resampling <- mlr3::rsmp("cv", folds = 3)

# 选择评估指标（R-squared）
measure <- mlr3::msr("regr.rsq")

# 设置调参算法：随机搜索
tuner <- mlr3tuning::tnr("random_search")

# 设置调参实例
instance <- mlr3tuning::TuningInstanceSingleCrit$new(
  task = mlr3::TaskRegr$new(id = "ddml_rf_tune", backend = ddml_data_clean, target = "y"),
  learner = learner_rf_tune,
  resampling = resampling,
  measure = measure,
  search_space = param_set,
  terminator = mlr3tuning::trm("evals", n_evals = 30)
)




# 构建 DoubleMLPLR 模型，使用调参后的随机森林作为 ml_l 和 ml_m
dml_rf_tuned <- DoubleMLPLR$new(
  data = df_doubleml,
  ml_l = learner_rf,  # E[Y|X]
  ml_m = learner_rf,  # E[D|X]
  n_folds = 5
)

# 拟合模型
dml_rf_tuned$fit()

# 输出 ATE 估计和显著性结果
dml_rf_tuned$summary()


# 拟合模型
dml_rf_tuned$fit()

# 输出 ATE 估计和显著性结果
dml_rf_tuned$summary()






