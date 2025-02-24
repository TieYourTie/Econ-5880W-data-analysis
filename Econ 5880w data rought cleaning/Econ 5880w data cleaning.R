rm(list = ls())
#make sure everthing will be fine.)

#Step one Lode the all package that necessary. 
library (lubridate)    
library (cansim)       
library (OECD)        
library (WDI)          
library (fredr)        
library (mFilter)      
library (neverhpfilter)
library (tsbox)
library (RColorBrewer) #so sad they do not have colorful black 
library(plotly)
library(wesanderson)
library(writexl)
library(tidyverse)
library(readr)
library(haven)


################################################################################################

#lode the data
canadian_election <- read_dta("H:/我的云端硬盘/Mac things/2025 winter/Econ 5880W/5880 project/Canadian election/2021 Canadian Election Study v2.0.dta")
dictnoary <- read_dta ("H:/我的云端硬盘/Mac things/2025 winter/Econ 5880W/5880 project/Canadian election/CES21_dictionarycoding_public_release_final.dta")
################################################################################################





# Define a vector containing all the variables
variables <- c(
  # Survey Weights & Data Quality Variables
  "cps21_weight_general_restricted",
  "pes21_weight_general_restricted",
  "cps21_data_quality",
  "pes21_data_quality",
  
  # Demographic Variables
  "cps21_genderid",
  #"cps21_genderid_4_TEXT", # removed
  #"cps21_trans", # removed
  "cps21_province",
  "cps21_citizenship",
  "cps21_education",
  
  # Pre-Election Interest & Political Views
  "cps21_demsat",          # Satisfaction with democracy
  "cps21_interest_gen_1",  # Interest in politics generally
  "cps21_interest_elxn_1", # Interest in federal election
  
  # Voting Likelihood & Behavior
  "cps21_v_likely",   # Likelihood of voting
  "cps21_comfort1",   # Comfort level voting in person during the pandemic
  "cps21_comfort2",   # Comfort level with COVID-19 restrictions during voting
  "cps21_comfort3",   # Comfort level with election procedures
  
  # Voting Choice & Party Preference
  "cps21_votechoice",      # Which party do you plan to vote for?
  #"cps21_vote_unlike_pr",  # removed
  "cps21_v_advance",       # For which party did you vote?
  #"cps21_vote_lean",       # removed
  #"cps21_2nd_choice",      # removed
  
  # Political Ideology & Spending Preferences
  "cps21_spend_env",       # Government spending on the environment
  "cps21_spend_imm_min",   # Government spending on immigrants/minorities
  "cps21_spend_rec_indi",  # Government spending on Indigenous reconciliation
  "cps21_spend_afford_h",  # Government spending on affordable housing
  "cps21_groups_therm_1",  # Feelings toward racial minorities
  "cps21_groups_therm_2",  # Feelings toward immigrants
  "cps21_groups_therm_7",  # Feelings toward Americans
  "cps21_groups_therm_3",  # Feelings toward Francophones
  "cps21_groups_therm_4",  # Feelings toward Indigenous peoples
  "cps21_groups_therm_6",  # Feelings toward feminists
  
  # COVID-19 & Elections
  "cps21_covid_liberty",   # COVID-19 restrictions as a threat to liberty
  "cps21_most_seats_1",    # Likelihood of Liberal Party winning most seats
  "cps21_most_seats_2",    # Likelihood of Conservative Party winning most seats
  "cps21_most_seats_3",    # Likelihood of NDP winning most seats
  "cps21_most_seats_4",    # Likelihood of Bloc Québécois winning most seats
  "cps21_most_seats_5",    # Likelihood of Green Party winning most seats
  "cps21_outcome_most",    # Preferred election outcome
  
  # Vaccine Attitudes
  "cps21_vaccine_mandat_1", # Vaccination required for air/rail travel
  "cps21_vaccine_mandat_2", # Vaccination required for bars/restaurants
  "cps21_vaccine_mandat_3", # Vaccination required for hospital work
  "cps21_vaccine1",         # Vaccination status
  
  # Post-Election Trust Variables
  "pes21_dem_sat",     # Post-election satisfaction with democracy
  "pes21_losetouch",   # Do elected officials lose touch with people?
  "pes21_govtcare",    # Does the government care about people like me?
  
  # Political Extremity & Populism
  "pes21_hatespeech",   # Should hateful speech be illegal?
  "pes21_envirojob",    # Jobs vs. environmental protection trade-off
  "pes21_discfam",      # Frequency of political discussion with family/friends
  "pes21_famvalues",    # Support for traditional family values
  "pes21_equalrights",  # Have equal rights gone too far?
  "pes21_fitin",        # Do recent immigrants integrate into Canadian society?
  "pes21_immigjobs",    # Do immigrants take jobs from Canadians?
  "pes21_govtprograms"  # Support for government assistance programs
)

# Print the list of selected variables
print(variables)
################################################################################################
#important variable

ce_p <- canadian_election %>%
  select(all_of(variables))

#keep the only high quality of row
ce_p <-ce_p %>%
  filter(
    cps21_data_quality == 0 & pes21_data_quality == 0)

ce_p <- ce_p %>%
  filter(!is.na(cps21_weight_general_restricted) & !is.na(pes21_weight_general_restricted))

#keep only the canadian citizen
ce_p <- ce_p  %>%
  filter(cps21_citizenship == 1)

# Remove unwanted columns
ce_p <- ce_p %>%
  select( -cps21_citizenship)

#education 
# Remove rows where cps21_education == 12 but keep NA values
ce_p <- ce_p %>%
  filter(cps21_education != 12)

# Remove rows where cps21_genderid == 4 but keep NA values
ce_p <- ce_p %>%
  filter(cps21_genderid != 4 )

# Remove unwanted columns
ce_p <- ce_p %>%
  select( -cps21_data_quality, -pes21_data_quality)


# Remove rows where cps21_v_advance is 6 or 7 but keep NA values
ce_p <- ce_p %>%
  filter(!cps21_votechoice %in% c(6, 7) | is.na(cps21_votechoice))

ce_p <- ce_p %>%
  filter(!cps21_v_advance %in% c(6, 7) | is.na(cps21_v_advance))

# Remove rows where cps21_genderid == 4 but keep NA values
ce_p <- ce_p %>%
  filter(cps21_genderid != 4 )

# Remove rows where cps21_genderid == 4 but keep NA values
ce_p <- ce_p %>%
  filter(cps21_interest_gen_1 != -99 )

# Remove rows where cps21_genderid == 4 but keep NA values
ce_p <- ce_p %>%
  filter(cps21_interest_elxn_1 != -99 )

#combine covid_voting feeling together
ce_p <- ce_p %>%
  mutate(cps21_comfort_combined = coalesce(cps21_comfort1, cps21_comfort2))

# Remove unwanted columns
ce_p <- ce_p %>%
  select( -cps21_comfort1, -cps21_comfort2)

#which you vote for 
ce_p <- ce_p %>%
  mutate(voting_choise = coalesce(cps21_votechoice, cps21_v_advance))


# Remove unwanted columns
ce_p <- ce_p %>%
  select( -cps21_votechoice, -cps21_v_advance)


ce_p <- ce_p %>%
  filter(!is.na(voting_choise))


################################################################################################












# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Filter and clean data
democracy_satisfaction <- ce_p %>%
  select(cps21_demsat, pes21_dem_sat) %>%
  drop_na()

# Convert to factor for proper ordering
democracy_satisfaction$cps21_demsat <- factor(democracy_satisfaction$cps21_demsat, levels = sort(unique(democracy_satisfaction$cps21_demsat)))
democracy_satisfaction$pes21_dem_sat <- factor(democracy_satisfaction$pes21_dem_sat, levels = sort(unique(democracy_satisfaction$pes21_dem_sat)))

# Count occurrences for each satisfaction level
satisfaction_counts <- democracy_satisfaction %>%
  pivot_longer(cols = c(cps21_demsat, pes21_dem_sat),
               names_to = "Election_Phase",
               values_to = "Satisfaction_Level") %>%
  group_by(Election_Phase, Satisfaction_Level) %>%
  summarise(Count = n(), .groups = "drop")

# Rename for better labeling
satisfaction_counts$Election_Phase <- recode(satisfaction_counts$Election_Phase,
                                             "cps21_demsat" = "Pre-Election",
                                             "pes21_dem_sat" = "Post-Election")

# Plot: Side-by-side Bar Chart
ggplot(satisfaction_counts, aes(x = Satisfaction_Level, y = Count, fill = Election_Phase)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Change in Satisfaction with Democracy",
       x = "Satisfaction Level",
       y = "Number of Respondents",
       fill = "Election Phase") +
  theme_minimal()

# Plot: Line Chart to Show Trend
ggplot(satisfaction_counts, aes(x = Satisfaction_Level, y = Count, group = Election_Phase, color = Election_Phase)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Trend in Satisfaction with Democracy",
       x = "Satisfaction Level",
       y = "Number of Respondents",
       color = "Election Phase") +
  theme_minimal()

####################################################


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(broom)
library(MASS)      # For ordered logistic regression
library(survey)    # For survey weights

# Step 1: Define mappings for categorical variables

# Mapping for provinces
province_mapping <- c(
  "cps21_province1"  = "Alberta",
  "cps21_province2"  = "British Columbia",
  "cps21_province3"  = "Manitoba",
  "cps21_province4"  = "New Brunswick",
  "cps21_province5"  = "Newfoundland and Labrador",
  "cps21_province6"  = "Northwest Territories",
  "cps21_province7"  = "Nova Scotia",
  "cps21_province8"  = "Nunavut",
  "cps21_province9"  = "Ontario",
  "cps21_province10" = "Prince Edward Island",
  "cps21_province11" = "Quebec",
  "cps21_province12" = "Saskatchewan",
  "cps21_province13" = "Yukon"
)

# Mapping for gender categories
gender_mapping <- c(
  "cps21_genderid1" = "Man",
  "cps21_genderid2" = "Woman",
  "cps21_genderid3" = "Non-binary"
)

# Mapping for political parties
party_mapping <- c(
  "voting_choise1" = "Liberal Party",
  "voting_choise2" = "Conservative Party",
  "voting_choise3" = "NDP",
  "voting_choise4" = "Bloc Québécois",
  "voting_choise5" = "Green Party"
)

# Mapping for education levels
education_mapping <- c(
  "cps21_education1"  = "No schooling",
  "cps21_education2"  = "Some elementary school",
  "cps21_education3"  = "Completed elementary school",
  "cps21_education4"  = "Some secondary/high school",
  "cps21_education5"  = "Completed secondary/high school",
  "cps21_education6"  = "Some technical/community college",
  "cps21_education7"  = "Completed technical/community college",
  "cps21_education8"  = "Some university",
  "cps21_education9"  = "Bachelor's degree",
  "cps21_education10" = "Master's degree",
  "cps21_education11" = "Professional degree or doctorate",
  "cps21_education12" = "Don't know/Prefer not to answer"
)

# Step 2: Create the dependent variable (ordinal outcome)
ce_p <- ce_p %>%
  mutate(democracy_change = as.factor(pes21_dem_sat - cps21_demsat))


# Step 1: Remove rows with NA or -99 values in relevant variables
ce_p <- ce_p %>%
  filter(
    !is.na(democracy_change) &
      !is.na(cps21_genderid) & cps21_genderid != -99 &
      !is.na(cps21_province) & cps21_province != -99 &
      !is.na(voting_choise) & voting_choise != -99 &
      !is.na(cps21_education) & cps21_education != -99
  )

# Step 2: Re-run the survey design and ordered logistic regression
design <- svydesign(ids = ~1, data = ce_p, weights = ~avg_weight)

ordered_logit_model_weighted <- svyolr(
  democracy_change ~  cps21_province + voting_choise + cps21_education,
  design = design
)

# Step 3: Extract regression results
logit_results_weighted <- broom::tidy(ordered_logit_model_weighted, conf.int = TRUE)


# Step 4: Compute the average survey weight for each respondent
ce_p <- ce_p %>%
  mutate(avg_weight = (cps21_weight_general_restricted + pes21_weight_general_restricted) / 2)

# Step 5: Define survey design with weights
design <- svydesign(ids = ~1, data = ce_p, weights = ~avg_weight)

# Step 6: Run Ordered Logistic Regression with survey weights
ordered_logit_model_weighted <- svyolr(democracy_change ~  + cps21_province + voting_choise + cps21_education, 
                                       design = design)

# Step 7: Extract regression results
logit_results_weighted <- broom::tidy(ordered_logit_model_weighted, conf.int = TRUE)

# Step 8: Replace codes with actual category names
logit_results_weighted <- logit_results_weighted %>%
  mutate(term = recode(term, !!!province_mapping, !!!gender_mapping, !!!party_mapping, !!!education_mapping))

# Step 9: Display summary of the weighted model
summary(ordered_logit_model_weighted)


logit_results_filtered <- logit_results_weighted %>%
  filter(!grepl("\\|", term))  # Remove rows where term contains "|", which represent intercepts

# Plot only relevant coefficients
ggplot(logit_results_filtered, aes(x = estimate, y = term)) +
  geom_point(color = "red", size = 3) +  # Red dots for coefficients
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.3) +  # Confidence intervals
  labs(title = "Ordered Logistic Regression Coefficients (Excluding Intercepts)",
       x = "Coefficient Estimate",
       y = "Variable") +
  theme_minimal()






1. 选举结果与选民期望
选举结果是否符合选民期望，往往会影响选民对民主制度的满意度。

🔹 支持的政党赢得选举 → 满意度上升
🔹 支持的政党失败 → 满意度下降

例如：

萨斯喀彻温省（Saskatchewan）选民满意度上升

保守党（Conservative Party）在此地赢得14个席位，几乎完全主导该省。
该省选民可能倾向于保守党，而保守党在全国选举中获得较多选票（尽管席位较少），这可能让选民觉得他们的声音得到了体现。
安大略省（Ontario）和纽芬兰与拉布拉多省（Newfoundland & Labrador）选民满意度下降

自由党虽然赢得选举，但在这些省份失去了一些席位（安大略-1席，纽芬兰与拉布拉多-1席）。
选民可能对自由党政策有所不满，或对选举结果不满意，因此民主满意度降低。
魁北克省（Quebec）选民满意度变化不大

**魁北克党（Bloc Québécois）**保住了大部分席位（32席），自由党赢得35席，选举结果相对稳定。
选民对选举结果的预期基本符合实际，因此满意度变化较小。
2. 经济和社会因素
经济状况和社会政策往往影响选民对政府的态度，而这也会反映在他们对民主制度的满意度变化上。

🔹 经济表现较弱 → 满意度下降
安大略（Ontario）和爱德华王子岛（Prince Edward Island）：
2021年选举时，安大略的房价飙升、生活成本上升，自由党在该省的支持度下降。
爱德华王子岛疫情影响严重，选民可能对政府的经济恢复政策不满。
这些地区选民认为政府未能有效解决经济问题，因此民主满意度下降。
🔹 经济表现较好 → 满意度上升
萨斯喀彻温省（Saskatchewan）和阿尔伯塔省（Alberta）：
这两个省份依赖石油、天然气和农业产业，而保守党在此地占主导地位。
选民认为保守党的经济政策更有利，选举后对民主制度的满意度提升。
3. 疫情管理与政策
COVID-19 政策 在选举中起到了重要作用，不同省份对政府的防疫政策有不同的看法。

🔹 政府疫情管理受欢迎 → 满意度上升
新不伦瑞克省（New Brunswick）、新斯科舍省（Nova Scotia）：
大西洋省份（Atlantic Canada）严格封锁，防疫措施成功，选民对政府表现较满意。
选举后满意度变化较小或上升。
🔹 反对防疫措施的地区 → 满意度下降
阿尔伯塔（Alberta）和萨斯喀彻温省（Saskatchewan）：
这两个省份中部分选民反对政府的封锁政策，认为自由党政府过度干预经济和个人自由。
选举后，他们可能对民主制度仍然持怀疑态度。
4. 选举制度的不满
加拿大的选举采用**"单一选区多数制"（First-Past-The-Post, FPTP）**，意味着：

得票最多的候选人当选，而不是按全国总票数分配席位。
可能导致选民认为选票“无效”，特别是支持小党的选民。
举例：

保守党全国总得票数高于自由党，但席位更少（119 vs. 160）

在选举制度下，保守党的选票更多地集中在西部省份，而东部省份的自由党选民较分散，但因选区优势赢得更多席位。
这可能导致保守党选民对选举制度不满，从而对民主制度满意度下降。
绿党（Green Party）和新民主党（NDP）支持者

绿党获得2.3%全国选票但仅赢得2个席位，而魁北克党获得7.7%选票却赢得32个席位。
支持小党的选民可能对民主制度不满意，因为他们的选票“未能转化为席位”。






