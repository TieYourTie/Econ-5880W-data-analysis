
#############################################################################
rm(list = ls())

## Load the package#############################################################
#Step one Lode the all package that necessary. 
library (lubridate)    
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

## load the data##############################################################################################

canadian_election <- read_dta("C:/Users/tcyub/OneDrive/桌面/MA Course/2021 Canadian Election Study v2.0.dta")
dictnoary <- read_dta ("C:/Users/tcyub/OneDrive/桌面/MA Course/CES21_dictionarycoding_public_release_final.dta")



############ 1.1 lode the data and do a first step filtering#################

#filter out the cols that is not important
remove_cols <- colnames(canadian_election)[grepl("First_Click|Last_Click|t_Click|TEXT|consent|captcha|mail-in ballot|\\bpr\\b", colnames(canadian_election))]

# filter it out
cecc <- canadian_election[, !(colnames(canadian_election) %in% remove_cols)]

#remove the the empty cols
empty_cols <- names(cecc)[colSums(is.na(cecc)) == nrow(cecc)]


############ 1.2 manual processing some necessary variable#################

############ Data Quality###########
cecc <-cecc %>%
  filter(
    cps21_data_quality == 0 & pes21_data_quality == 0)


#############################################################################
#                           Data Cleaning 
#############################################################################

############## Canadian Citizen###########
cecc <- cecc  %>%
  filter(cps21_citizenship == 1)

############## democratic satification###########
cecc <- cecc %>%
  filter(pes21_dem_sat != 5 & pes21_data_quality != 5)

cecc <- cecc %>%
  mutate(
    cps21_demsat = case_when(
      cps21_demsat == 1 ~ 4,
      cps21_demsat == 2 ~ 3,
      cps21_demsat == 3 ~ 2,
      cps21_demsat == 4 ~ 1,
      cps21_demsat == 5 ~ NA_real_  # 'Don't know' becomes NA
    )
  )

cecc <- cecc %>%
  mutate(
    pes21_dem_sat = case_when(
      pes21_dem_sat == 1 ~ 4,
      pes21_dem_sat == 2 ~ 3,
      pes21_dem_sat == 3 ~ 2,
      pes21_dem_sat == 4 ~ 1,
      pes21_dem_sat == 5 ~ NA_real_  # 'Don't know' becomes NA
    )
  )

############## language used###########
cecc$UserLanguage_dummy <- ifelse(cecc$UserLanguage == "EN", 1, 0)

############## cps21_marital###########

# Step 1: Drop respondents with value 7
cecc <- cecc %>%
  filter(cps21_marital != 7)

#Step 1: Convert cps21_marital to a factor
cecc$cps21_marital <- as.factor(cecc$cps21_marital)
#step 2: Create dummy variables
marital_dummies <- model.matrix(~ cps21_marital - 1, data = cecc)
#step 3: Add dummies back to your dataset
cecc <- cbind(cecc, marital_dummies)
#step 4: Drop the original cps21_marital variable
cecc <- cecc %>% select(-cps21_marital)
#step 5: Check the new dummies
grep("^cps21_marital", colnames(cecc), value = TRUE)

############## province###########

#backup this variable, before being set as dummy (for later use in subgroup)
cecc$cps21_new_province <- cecc$cps21_province

# Step 1: Create region variable from province
cecc$region <- dplyr::case_when(
  cecc$cps21_province %in% c(5, 10, 7, 4) ~ "Atlantic",         # NL, PEI, NS, NB
  cecc$cps21_province == 11              ~ "Quebec",           # Quebec
  cecc$cps21_province == 9               ~ "Ontario",          # Ontario
  cecc$cps21_province %in% c(3, 12, 1)   ~ "Prairies",         # MB, SK, AB
  cecc$cps21_province == 2               ~ "British_Columbia", # BC
  cecc$cps21_province %in% c(13, 6, 8)   ~ "Territories",      # YK, NWT, NU
  TRUE ~ NA_character_
)

# Step 2: Convert to factor
cecc$region <- as.factor(cecc$region)

# Step 3: Create dummy variables
region_dummies <- model.matrix(~ region - 1, data = cecc)

# Step 4: Add dummies and drop the original region variable
cecc <- cbind(cecc, region_dummies)
cecc <- cecc %>% select(-region)
cecc <- cecc %>% select(-Region)
# Step 5: Drop Territories dummy (low sample)
cecc <- cecc %>% select(-matches("^regionTerritories$"))

# Step 6: Check new dummy columns
grep("^region", names(cecc), value = TRUE)

############## age###########
cecc <- cecc %>%
  mutate(birth_year = cps21_yob + 1919,   # Convert coded value to real year
         age = 2021 - birth_year)         # Compute age
############## transgender_dummy################
# Create the transgender dummy variable (we remove this cause data is inbalance)

############## gender##########
#we'll remove the non-binary dummy, due to data inbalance
cecc <- cecc %>% filter(!(cps21_genderid %in% c(3, 4)))

#backup this variable, before being set as dummy (for later use in subgroup)
cecc$cps21_new_genderid <- cecc$cps21_genderid

######set as dummy
# Step 1: Convert to factor
cecc$cps21_genderid <- as.factor(cecc$cps21_genderid)

# Optional: Check the levels (to ensure they look right)
levels(cecc$cps21_genderid)

# Step 2: Create dummy variables
gender_dummies <- model.matrix(~ cps21_genderid - 1, data = cecc)

# Step 3: Add dummies back to dataset
cecc <- cbind(cecc, gender_dummies)

#removes only the original column, keeping the dummy variables
cecc <- cecc %>% select(-cps21_genderid)

#step 5: Check the new dummy variables
grep("^cps21_genderid", colnames(cecc), value = TRUE)

############## race ##########
#white, black, asian, idigenous and others.(we'll only look at "white" & "non-white" dummy)
cecc <- cecc %>%
  mutate(
    vismin_arab = ifelse(cps21_vismin_1 == 1, 1, 0),
    vismin_asian = ifelse(cps21_vismin_2 == 1, 1, 0),
    vismin_black = ifelse(cps21_vismin_3 == 1, 1, 0),
    vismin_indigenous = ifelse(cps21_vismin_4 == 1, 1, 0),
    vismin_latino = ifelse(cps21_vismin_5 == 1, 1, 0),
    vismin_south_asian = ifelse(cps21_vismin_6 == 1, 1, 0),
    vismin_southeast_asian = ifelse(cps21_vismin_7 == 1, 1, 0),
    vismin_west_asian = ifelse(cps21_vismin_8 == 1, 1, 0),
    vismin_white = ifelse(cps21_vismin_9 == 1, 1, 0),
    vismin_other = ifelse(cps21_vismin_10 == 1, 1, 0),
    vismin_none = ifelse(cps21_vismin_11 == 1, 1, 0),
    vismin_prefer_not_answer = ifelse(cps21_vismin_12 == 1, 1, 0)
  )

cecc <- cecc %>%
  mutate(cps21_vismin_9 = ifelse(cps21_vismin_9 == -99, 0, cps21_vismin_9))

#remove not answered
cecc <- cecc %>%
  select(-cps21_vismin_12)

############## per_party_choice###########

# Combine vote-related variables into `party_choice`
cecc <- cecc %>%
  mutate(
    post_party_choice = coalesce(pes21_votechoice2021)
  )
# To remove values 7, 8, 9, and NA
cecc <- cecc %>%
  filter(post_party_choice %in% 1:6)

############## Economy scale##############
cecc <- cecc %>%
  filter(cps21_own_fin_retro != 4 & cps21_ownfinanc_fed != 4 & cps21_econ_retro != 4,5 & cps21_econ_fed_bette != 4)

#To switch the values 2 and 3 along with their labels in cps21_econ_fed_bette
cecc <- cecc %>%
  mutate(cps21_econ_fed_bette = case_when(
    cps21_econ_fed_bette == 2 ~ 3,  # Change 2 → 3
    cps21_econ_fed_bette == 3 ~ 2,  # Change 3 → 2
    TRUE ~ cps21_econ_fed_bette  # Keep other values unchanged
  ))
#reassign labels for values 2 and 3
cecc$cps21_econ_fed_bette <- labelled(cecc$cps21_econ_fed_bette, 
                                       c("Better" = 1, 
                                         "Not made much difference" = 2,  # Previously 3, now 2
                                         "Worse" = 3,                     # Previously 2, now 3
                                         "Don't know/ Prefer not to answer" = 4)
)

# Swap values for cps21_ownfinanc_fed
cecc <- cecc %>%
  mutate(cps21_ownfinanc_fed = case_when(
    cps21_ownfinanc_fed == 2 ~ 3,  # Change 2 → 3
    cps21_ownfinanc_fed == 3 ~ 2,  # Change 3 → 2
    TRUE ~ cps21_ownfinanc_fed  # Keep other values unchanged
  ))

# Update labels accordingly
cecc$cps21_ownfinanc_fed <- labelled(cecc$cps21_ownfinanc_fed, 
                                      c("Better" = 1, 
                                        "Not made much difference" = 2,  # Previously 3, now 2
                                        "Worse" = 3,                     # Previously 2, now 3
                                        "Don't know/ Prefer not to answer" = 4)
)


############## cps21_bornin_canada########
cecc <- cecc %>% filter(cps21_bornin_canada != 3)

#make dummy
cecc <- cecc %>%
  mutate(
    born_in_canada = ifelse(cps21_bornin_canada == 1, 1, 0)
  )


############## Cleaning Sexuality (cps21_sexuality) ##########
cecc <- cecc %>%
  filter(!cps21_sexuality %in% c(5, 6, 7, 8))

#Dummy = straight vs not straight
cecc <- cecc %>%
  mutate(
    sexuality_straight = ifelse(cps21_sexuality == 1, 1, 0)
  )

############## Cleaning Expected Future Financial Situation (cps21_own_fin_future) ##########
cecc <- cecc %>%
  filter(cps21_own_fin_future %in% c(1, 2, 3))

############## Cleaning Religion Identification (cps21_religion) ##########

#drop 22/23, Dummy = religion or not
cecc <- cecc %>%
  filter(!cps21_religion %in% c(22, 23)) %>%
  mutate(
    religion_None = ifelse(cps21_religion == 1, 1, 0)
  )


############## Cleaning Economic Evaluations ##########
cecc <- cecc %>%
  filter(!cps21_econ_retro %in% c(4, 5))
############## Cleaning Political Interest (cps21_interest_gen_1 & cps21_interest_elxn_1) ##########

cecc <- cecc %>%
  filter(cps21_interest_gen_1 != -99)
cecc <- cecc %>%
  filter(cps21_interest_elxn_1 != -99)

############## Cleaning Employment Status (cps21_employment) ##########
# Step 1: Remove unwanted categories (12 = Other, 13 = DK/PNA)
#create dummy = in the workforce or not
cecc <- cecc %>%
  filter(!cps21_employment %in% c(12, 13)) %>%
  mutate(
    employment_group = case_when(
      cps21_employment %in% c(4, 6, 7, 8) ~ "Group_A",  # Combine 4,6,7,8
      TRUE ~ "Group_B"                                 # All others except 12,13
    ),
    employment_dummy = ifelse(employment_group == "Group_B", 1, 0)
  )


############## Cleaning Education (cps21_education) ##########

cecc <- cecc %>% filter(cps21_education != 12)

#backup this variable, before being set as dummy (for later use in subgroup)
cecc$cps21_new_education <- cecc$cps21_education

# classify education groups
cecc <- cecc %>%
  mutate(education_group = case_when(
    cps21_education %in% c(1, 2, 3, 4) ~ "Less than high school",
    cps21_education == 5              ~ "High school",
    cps21_education %in% c(6, 7)      ~ "College/Technical",
    cps21_education %in% c(8, 9, 10, 11) ~ "University",
    TRUE ~ NA_character_
  ))

#Convert to factor
cecc$cps21_education <- as.factor(cecc$education_group)
#Create dummy variables
education_dummies <- model.matrix(~ cps21_education - 1, data = cecc)
#Bind dummies back to the dataset
cecc <- cbind(cecc, education_dummies)
#(Optional) Drop the original column
cecc <- cecc %>% select(-cps21_education)
#check dummys
grep("^education_group", names(cecc), value = TRUE)
#drop original variable
cecc <- cecc %>% select(-education_group)


############## Cleaning Democracy Satisfaction (cps21_demsat) ##########
cecc <- cecc %>% filter(cps21_demsat != 5)


############## Clean Covid satisfaction #################
cecc <- cecc %>% filter(cps21_covid_sat_1 != 5 & cps21_covid_sat_2 != 5 & cps21_covid_sat_3 != 5)

#backup this variable, before being set as dummy (for later use in subgroup)
cecc$cps21_new_covid_sat_1 <- cecc$cps21_covid_sat_1

############## taken Vaccine ##########
#Step 1: Filter out responses with value 4
#set Dummy = vaccined or not
cecc <- cecc %>%
  filter(cps21_vaccine1 != 4) %>%
  mutate(
    vaccine_accept = ifelse(cps21_vaccine1 %in% c(1, 2), 1, 0)
  )


############## vaccine mandate#############
cecc <- cecc %>%
  filter(cps21_vaccine_mandat_1 != 5)
cecc <- cecc %>%
  filter(cps21_vaccine_mandat_2 != 5)
cecc <- cecc %>%
  filter(cps21_vaccine_mandat_3 != 5)

############## Demographic##############
cecc <- cecc %>% select(-cps21_children)

############## Attitude & satisfaction#############
cecc <- cecc %>%
  filter(
    pes21_govtcare != 6,
    pes21_famvalues != 6,
    pes21_equalrights != 6,
    pes21_envirojob != 6,
    pes21_discfam != 5,       # Drop "Don't know/Prefer not to answer"
    pes21_fitin != 6,         # Drop DK/PNA
    pes21_immigjobs != 6      # Drop DK/PNA
  )
############## Spending preference#############
cecc <- cecc %>%
  filter(
    cps21_spend_educ != 4,
    cps21_spend_env != 4,
    cps21_spend_defence != 4,
    cps21_spend_imm_min != 4,
    cps21_spend_rec_indi != 4,
    cps21_spend_just_law != 4
  )


#############################################################################
#                            Model for Liberal
#############################################################################
#### packages install#####
install.packages("mlr3extralearners")
install.packages("mlr3")

#### Open Library #####
library(mlr3extralearners)
library(DoubleML)
library(mlr3)
library(mlr3learners)
library(dplyr)
library(tidyr)
library(data.table)  # Load data.table package
library(haven)

cecc <- cecc %>%
  mutate(across(where(is.labelled), zap_labels))


####################
set.seed(1234)
#####################################################################
#                          DDML setup for Liberal
#####################################################################
######### setup ####
cecc_dml <- cecc %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)))


cecc_dml <- as.data.table(cecc_dml)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml), c(y_col, d_cols))  # Control variables

dml_data <- DoubleMLData$new(data = cecc_dml, 
                             y_col = y_col, 
                             d_cols = d_cols, 
                             x_cols = x_cols)
#####################################################################
                           #Different LASSO
#####################################################################

############# Lasso learners using lambda.min #######
ml_l <- lrn("regr.cv_glmnet", s = "lambda.min")  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = "lambda.min") # Lasso classification for D ~ X

# Make sure they retain the fitted model
ml_l$param_set$values$keep_model <- TRUE
ml_m$param_set$values$keep_model <- TRUE

# Train DML model (LASSO)
dml_multi <- DoubleMLPLR$new(dml_data, ml_l = ml_l, ml_m = ml_m)
dml_multi$fit()

# Output results (LASSO)
dml_multi$summary()

########### Access the ml_l and ml_m models used (find the lambda value)
# 1. Get the ml_l model (for Y ~ X)
lasso_model_Y <- dml_multi$models$ml_l[[1]]$model

# 2. Extract lambda.min correctly from the $fit object
lambda_min_Y <- lasso_model_Y$fit$lambda.min
print(lambda_min_Y)

# 1. Get the ml_m model (for D ~ X)
lasso_model_D <- dml_multi$models$ml_m[[1]]$model

# 2. Extract lambda.min correctly from the $fit object
lambda_min_D <- lasso_model_D$fit$lambda.min
print(lambda_min_D)


############# Lasso learners using lambda.1se #####
ml_l <- lrn("regr.cv_glmnet", s = "lambda.1se")  # More regularized regression
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = "lambda.1se")  # Classification

# Train DML model
dml_multi_1se <- DoubleMLPLR$new(dml_data, ml_l = ml_l, ml_m = ml_m)
dml_multi_1se$fit()

# Output results
dml_multi_1se$summary()

# 1. Get the ml_l model (for Y ~ X)
lasso_model_Y <- dml_multi_1se$models$ml_l[[1]]$model

# 2. Extract lambda.1se and lambda.min
lambda_1se_Y <- lasso_model_Y$fit$lambda.1se
lambda_min_Y <- lasso_model_Y$fit$lambda.min
cat("lambda.1se (Y ~ X):", lambda_1se_Y, "\n")
cat("lambda.min  (Y ~ X):", lambda_min_Y, "\n")

# 3. Get the ml_m model (for D ~ X)
lasso_model_D <- dml_multi_1se$models$ml_m[[1]]$model

# 4. Extract lambda.1se and lambda.min
lambda_1se_D <- lasso_model_D$fit$lambda.1se
lambda_min_D <- lasso_model_D$fit$lambda.min
cat("lambda.1se (D ~ X):", lambda_1se_D, "\n")
cat("lambda.min  (D ~ X):", lambda_min_D, "\n")

############# Lasso learners using manually adjusted lumbda ######
library(mlr3)
library(mlr3learners)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

ml_l <- lrn("regr.glmnet", s = my_lambda)  # Manual lambda (regression)
ml_m <- lrn("classif.glmnet", predict_type = "prob", s = my_lambda)  # Manual lambda (classification)

# Train DML model
dml_multi_custom <- DoubleMLPLR$new(dml_data, ml_l = ml_l, ml_m = ml_m)
dml_multi_custom$fit()

# Output results
dml_multi_custom$summary()


#####################################################################
#                           Other Models
#####################################################################
############# Check which control variables Lasso selected (for Y model)######

library(glmnet)

# Step 1: Prepare X and y
X <- as.matrix(cecc_dml[, ..x_cols])  # Control variables
y <- cecc_dml[[y_col]]                # Outcome: democratic satisfaction

# Step 2: Fit Lasso model
lasso_outcome <- glmnet(X, y, alpha = 1)  # alpha = 1 = Lasso

# Step 3: Extract non-zero coefficients (variables kept by Lasso)
coef_outcome <- coef(lasso_outcome, s = my_lambda)
selected_vars_outcome <- rownames(coef_outcome)[which(coef_outcome != 0)]

# View selected variables
selected_vars_outcome


############# Check which control variables Lasso selected (for D model)########

d_liberal <- cecc_dml[["liberal"]]  # Treatment: Liberal supporter

lasso_treat_lib <- cv.glmnet(X, d_liberal, alpha = 1, family = "binomial")
coef_treat_lib <- coef(lasso_treat_lib, s = my_lambda)
selected_vars_treat_lib <- rownames(coef_treat_lib)[which(coef_treat_lib != 0)]

selected_vars_treat_lib

############# Compare: Shared Variables Between Y & D Models##########

intersect(selected_vars_outcome, selected_vars_treat_lib)


############# Look at coefficient magnitudes from LASSO ########

coef(lasso_outcome, s = my_lambda)




############# Past-LASSO OLS ########
set.seed(1234)
# Step 1: Combine treatment, outcome, and selected controls (remove intercept)
ols_vars <- unique(c(y_col, d_cols, selected_vars_outcome))
ols_vars <- setdiff(ols_vars, "(Intercept)")

# Step 2: Subset the data
ols_data <- cecc_dml[, ..ols_vars]

# Step 3: Build the OLS formula
ols_formula <- as.formula(
  paste(y_col, "~", paste(setdiff(ols_vars, y_col), collapse = " + "))
)

# Step 4: Run OLS regression
ols_model <- lm(ols_formula, data = as.data.frame(ols_data))

# Step 5: View results
summary(ols_model)




############# Ridge learners#######
set.seed(1234)
ml_lR <- lrn("regr.cv_glmnet", s = "lambda.min", alpha = 0)  # Ridge regression for Y ~ X
ml_mR <- lrn("classif.cv_glmnet", predict_type = "prob", s = "lambda.min", alpha = 0) # Ridge classification for D ~ X

# Train DML model (Ridge)
dml_multiR <- DoubleMLPLR$new(dml_data, ml_l = ml_lR, ml_m = ml_mR)
dml_multiR$fit()

# Output results (Ridge)
dml_multiR$summary()


############# Random Forest learners########
set.seed(1234)
ml_lRF <- lrn("regr.ranger") # RF for Y ~ X
ml_mRF <- lrn("classif.ranger", predict_type = "prob") # RF for D ~ X

# Train DML model (Random Forest)
dml_multiRF <- DoubleMLPLR$new(dml_data, ml_l = ml_lRF, ml_m = ml_mRF)
dml_multiRF$fit()

# Output results (Random Forest)
dml_multiRF$summary()



#############################################################################
#                        model for conservative 
#############################################################################

set.seed(1234)

############ DDML setup for conservative#####
cecc_dml_cons <- cecc %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)))


cecc_dml_cons <- as.data.table(cecc_dml_cons)  # Convert to data.table (required by DoubleML)

# Set variables
y_col_cons <- "dem_sat_change"
d_cols_cons <- "conservative"
x_cols_cons <- setdiff(names(cecc_dml_cons), c(y_col_cons, d_cols_cons))  # Control variables

dml_data_cons <- DoubleMLData$new(data = cecc_dml_cons,
                                  y_col = y_col_cons,
                                  d_cols = d_cols_cons,
                                  x_cols = x_cols_cons)

############ Lasso learners using manually adjusted lumbda######

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

ml_l_cons <- lrn("regr.glmnet", s = my_lambda)  # Manual lambda (regression)
ml_m_cons <- lrn("classif.glmnet", predict_type = "prob", s = my_lambda)  # Manual lambda (classification)

# Train DML model
dml_model_cons<- DoubleMLPLR$new(dml_data_cons, ml_l = ml_l_cons, ml_m = ml_m_cons)
dml_model_cons$fit()

# Output results
dml_model_cons$summary()

############ Check which control variables Lasso selected (for Y model)######

library(glmnet)

# Step 1: Drop NA rows in both X and y
complete_rows <- complete.cases(cecc_dml_cons[, c(x_cols_cons, y_col_cons), with = FALSE])

X_cons <- as.matrix(cecc_dml_cons[complete_rows, ..x_cols_cons])  # Controls
y_cons <- cecc_dml_cons[complete_rows][[y_col_cons]]              # Outcome

# Step 2: Fit Lasso for outcome model
lasso_outcome_cons <- glmnet(X_cons, y_cons, alpha = 1)

# Step 3: Get non-zero coefficients at lambda = my_lambda
coef_outcome_cons <- coef(lasso_outcome_cons, s = my_lambda)
selected_vars_outcome_cons <- rownames(coef_outcome_cons)[which(coef_outcome_cons != 0)]

############ Check which control variables Lasso selected (for D model) ########

# Step 4: Align X and D
complete_rows_d <- complete.cases(cecc_dml_cons[, c(x_cols_cons, "conservative"), with = FALSE])

X_cons_d <- as.matrix(cecc_dml_cons[complete_rows_d, ..x_cols_cons])
d_cons <- cecc_dml_cons[complete_rows_d][["conservative"]]

# Step 5: Fit Lasso for treatment model
lasso_treat_cons <- glmnet(X_cons_d, d_cons, alpha = 1, family = "binomial")

# Step 6: Get non-zero coefficients
coef_treat_cons <- coef(lasso_treat_cons, s = my_lambda)
selected_vars_treat_cons <- rownames(coef_treat_cons)[which(coef_treat_cons != 0)]


############ Compare: Shared Variables Between Y & D Models##########

intersect(selected_vars_outcome_cons, selected_vars_treat_cons)


############ Look at coefficient magnitudes from LASSO ########

coef(lasso_outcome_cons, s = my_lambda)




############ Past-LASSO OLS ########
set.seed(1234)
# Step 1: Combine treatment, outcome, and selected controls (remove intercept)
ols_vars_cons <- unique(c(y_col_cons, d_cols_cons, selected_vars_outcome_cons))
ols_vars_cons <- setdiff(ols_vars_cons, "(Intercept)")

# Step 2: Subset the data
ols_data_cons <- cecc_dml_cons[, ..ols_vars_cons]

# Step 3: Build the OLS formula
ols_formula_cons <- as.formula(
  paste(y_col_cons, "~", paste(setdiff(ols_vars_cons, y_col_cons), collapse = " + "))
)

# Step 4: Run OLS regression
ols_model_cons <- lm(ols_formula_cons, data = as.data.frame(ols_data_cons))

# Step 5: View results
summary(ols_model_cons)





############ Ridge learners######
set.seed(1234)
ml_lR_cons <- lrn("regr.cv_glmnet", s = "lambda.min", alpha = 0)  # Ridge regression for Y ~ X
ml_mR_cons <- lrn("classif.cv_glmnet", predict_type = "prob", s = "lambda.min", alpha = 0) # Ridge classification for D ~ X

# Train DML model (Ridge)
dml_multiR_cons <- DoubleMLPLR$new(dml_data_cons, ml_l = ml_lR_cons, ml_m = ml_mR_cons)
dml_multiR_cons$fit()

# Output results (Ridge)
dml_multiR_cons$summary()


############ Random Forest learners######
set.seed(1234)
ml_lRF_cons <- lrn("regr.ranger") # RF for Y ~ X
ml_mRF_cons <- lrn("classif.ranger", predict_type = "prob") # RF for D ~ X

# Train DML model (Random Forest)
dml_multiRF_cons <- DoubleMLPLR$new(dml_data_cons, ml_l = ml_lRF_cons, ml_m = ml_mRF_cons)
dml_multiRF_cons$fit()

# Output results (Random Forest)
dml_multiRF_cons$summary()




#############################################################################
#             model for Liberal (Quebec,French subgroup) 
#############################################################################

set.seed(1234)

############ Subgroup Quebec & French #####

# Subset data for French-speaking Quebecers
quebec_french <- cecc %>%
  filter(regionQuebec == 1 & UserLanguage_dummy == 0)

############ DDML setup for Liberal#####

cecc_dml_qf_lib <- quebec_french %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)))


cecc_dml_qf_lib <- as.data.table(cecc_dml_qf_lib)  # Convert to data.table (required by DoubleML)

# Set variables
y_col_qf_lib <- "dem_sat_change"
d_cols_qf_lib <- "liberal"
x_cols_qf_lib <- setdiff(names(cecc_dml_qf_lib), c(y_col_qf_lib, d_cols_qf_lib))  # Control variables

dml_data_qf_lib <- DoubleMLData$new(data = cecc_dml_qf_lib,
                                  y_col = y_col_qf_lib,
                                  d_cols = d_cols_qf_lib,
                                  x_cols = x_cols_qf_lib)

############ Lasso learners using manually adjusted lumbda######

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

ml_l_qf_lib <- lrn("regr.glmnet", s = my_lambda)  # Manual lambda (regression)
ml_m_qf_lib <- lrn("classif.glmnet", predict_type = "prob", s = my_lambda)  # Manual lambda (classification)

# Train DML model
dml_model_qf_lib<- DoubleMLPLR$new(dml_data_qf_lib, ml_l = ml_l_qf_lib, ml_m = ml_m_qf_lib)
dml_model_qf_lib$fit()

# Output results
dml_model_qf_lib$summary()

############ Check which control variables Lasso selected (for Y model)######

library(glmnet)

# Step 1: Drop NA rows in both X and y
complete_rows <- complete.cases(cecc_dml_qf_lib[, c(x_cols_qf_lib, y_col_qf_lib), with = FALSE])

X_qf_lib <- as.matrix(cecc_dml_qf_lib[complete_rows, ..x_cols_qf_lib])  # Controls
y_qf_lib <- cecc_dml_qf_lib[complete_rows][[y_col_qf_lib]]              # Outcome

# Step 2: Fit Lasso for outcome model
lasso_outcome_qf_lib <- glmnet(X_qf_lib, y_qf_lib, alpha = 1)

# Step 3: Get non-zero coefficients at lambda = my_lambda
coef_outcome_qf_lib <- coef(lasso_outcome_qf_lib, s = my_lambda)
selected_vars_outcome_qf_lib <- rownames(coef_outcome_qf_lib)[which(coef_outcome_qf_lib != 0)]

############ Check which control variables Lasso selected (for D model) ########

# Step 4: Align X and D
complete_rows_d <- complete.cases(cecc_dml_qf_lib[, c(x_cols_qf_lib, "liberal"), with = FALSE])

X_qf_lib_d <- as.matrix(cecc_dml_qf_lib[complete_rows_d, ..x_cols_qf_lib])
d_qf_lib <- cecc_dml_qf_lib[complete_rows_d][["liberal"]]

# Step 5: Fit Lasso for treatment model
lasso_treat_qf_lib <- glmnet(X_qf_lib_d, d_qf_lib, alpha = 1, family = "binomial")

# Step 6: Get non-zero coefficients
coef_treat_qf_lib <- coef(lasso_treat_qf_lib, s = my_lambda)
selected_vars_treat_qf_lib <- rownames(coef_treat_qf_lib)[which(coef_treat_qf_lib != 0)]


############ Compare: Shared Variables Between Y & D Models##########

intersect(selected_vars_outcome_qf_lib, selected_vars_treat_qf_lib)


############ Look at coefficient magnitudes from LASSO ########

coef(lasso_outcome_qf_lib, s = my_lambda)




############ Past-LASSO OLS ########
set.seed(1234)
# Step 1: Combine treatment, outcome, and selected controls (remove intercept)
ols_vars_qf_lib <- unique(c(y_col_qf_lib, d_cols_qf_lib, selected_vars_outcome_qf_lib))
ols_vars_qf_lib <- setdiff(ols_vars_qf_lib, "(Intercept)")

# Step 2: Subset the data
ols_data_qf_lib <- cecc_dml_qf_lib[, ..ols_vars_qf_lib]

# Step 3: Build the OLS formula
ols_formula_qf_lib <- as.formula(
  paste(y_col_qf_lib, "~", paste(setdiff(ols_vars_qf_lib, y_col_qf_lib), collapse = " + "))
)

# Step 4: Run OLS regression
ols_model_qf_lib <- lm(ols_formula_qf_lib, data = as.data.frame(ols_data_qf_lib))

# Step 5: View results
summary(ols_model_qf_lib)





############ Ridge learners######
set.seed(1234)
ml_lR_qf_lib <- lrn("regr.cv_glmnet", s = "lambda.min", alpha = 0)  # Ridge regression for Y ~ X
ml_mR_qf_lib <- lrn("classif.cv_glmnet", predict_type = "prob", s = "lambda.min", alpha = 0) # Ridge classification for D ~ X

# Train DML model (Ridge)
dml_multiR_qf_lib <- DoubleMLPLR$new(dml_data_qf_lib, ml_l = ml_lR_qf_lib, ml_m = ml_mR_qf_lib)
dml_multiR_qf_lib$fit()

# Output results (Ridge)
dml_multiR_qf_lib$summary()


############ Random Forest learners######
set.seed(1234)
ml_lRF_qf_lib <- lrn("regr.ranger") # RF for Y ~ X
ml_mRF_qf_lib <- lrn("classif.ranger", predict_type = "prob") # RF for D ~ X

# Train DML model (Random Forest)
dml_multiRF_qf_lib <- DoubleMLPLR$new(dml_data_qf_lib, ml_l = ml_lRF_qf_lib, ml_m = ml_mRF_qf_lib)
dml_multiRF_qf_lib$fit()

# Output results (Random Forest)
dml_multiRF_qf_lib$summary()





#############################################################################
#             model for Bloc Quebec (Quebec,French subgroup) 
#############################################################################

set.seed(1234)

############ DDML setup for Bloc Quebec#####

cecc_dml_qf_qb <- quebec_french %>%
  mutate(
    quebec = ifelse(post_party_choice == 4, 1, 0),        # Dummy: 1 if Quebec supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "quebec", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)))


cecc_dml_qf_qb <- as.data.table(cecc_dml_qf_qb)  # Convert to data.table (required by DoubleML)

# Set variables
y_col_qf_qb <- "dem_sat_change"
d_cols_qf_qb <- "quebec"
x_cols_qf_qb <- setdiff(names(cecc_dml_qf_qb), c(y_col_qf_qb, d_cols_qf_qb))  # Control variables

dml_data_qf_qb <- DoubleMLData$new(data = cecc_dml_qf_qb,
                                    y_col = y_col_qf_qb,
                                    d_cols = d_cols_qf_qb,
                                    x_cols = x_cols_qf_qb)

############ Lasso learners using manually adjusted lumbda######

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

ml_l_qf_qb <- lrn("regr.glmnet", s = my_lambda)  # Manual lambda (regression)
ml_m_qf_qb <- lrn("classif.glmnet", predict_type = "prob", s = my_lambda)  # Manual lambda (classification)

# Train DML model
dml_model_qf_qb<- DoubleMLPLR$new(dml_data_qf_qb, ml_l = ml_l_qf_qb, ml_m = ml_m_qf_qb)
dml_model_qf_qb$fit()

# Output results
dml_model_qf_qb$summary()

############ Check which control variables Lasso selected (for Y model)######

library(glmnet)

# Step 1: Drop NA rows in both X and y
complete_rows <- complete.cases(cecc_dml_qf_qb[, c(x_cols_qf_qb, y_col_qf_qb), with = FALSE])

X_qf_qb <- as.matrix(cecc_dml_qf_qb[complete_rows, ..x_cols_qf_qb])  # Controls
y_qf_qb <- cecc_dml_qf_qb[complete_rows][[y_col_qf_qb]]              # Outcome

# Step 2: Fit Lasso for outcome model
lasso_outcome_qf_qb <- glmnet(X_qf_qb, y_qf_qb, alpha = 1)

# Step 3: Get non-zero coefficients at lambda = my_lambda
coef_outcome_qf_qb <- coef(lasso_outcome_qf_qb, s = my_lambda)
selected_vars_outcome_qf_qb <- rownames(coef_outcome_qf_qb)[which(coef_outcome_qf_qb != 0)]

############ Check which control variables Lasso selected (for D model) ########

# Step 4: Align X and D
complete_rows_d <- complete.cases(cecc_dml_qf_qb[, c(x_cols_qf_qb, "quebec"), with = FALSE])

X_qf_qb_d <- as.matrix(cecc_dml_qf_qb[complete_rows_d, ..x_cols_qf_qb])
d_qf_qb <- cecc_dml_qf_qb[complete_rows_d][["quebec"]]

# Step 5: Fit Lasso for treatment model
lasso_treat_qf_qb <- glmnet(X_qf_qb_d, d_qf_qb, alpha = 1, family = "binomial")

# Step 6: Get non-zero coefficients
coef_treat_qf_qb <- coef(lasso_treat_qf_qb, s = my_lambda)
selected_vars_treat_qf_qb <- rownames(coef_treat_qf_qb)[which(coef_treat_qf_qb != 0)]


############ Compare: Shared Variables Between Y & D Models##########

intersect(selected_vars_outcome_qf_qb, selected_vars_treat_qf_qb)


############ Look at coefficient magnitudes from LASSO ########

coef(lasso_outcome_qf_qb, s = my_lambda)




############ Past-LASSO OLS ########
set.seed(1234)
# Step 1: Combine treatment, outcome, and selected controls (remove intercept)
ols_vars_qf_qb <- unique(c(y_col_qf_qb, d_cols_qf_qb, selected_vars_outcome_qf_qb))
ols_vars_qf_qb <- setdiff(ols_vars_qf_qb, "(Intercept)")

# Step 2: Subset the data
ols_data_qf_qb <- cecc_dml_qf_qb[, ..ols_vars_qf_qb]

# Step 3: Build the OLS formula
ols_formula_qf_qb <- as.formula(
  paste(y_col_qf_qb, "~", paste(setdiff(ols_vars_qf_qb, y_col_qf_qb), collapse = " + "))
)

# Step 4: Run OLS regression
ols_model_qf_qb <- lm(ols_formula_qf_qb, data = as.data.frame(ols_data_qf_qb))

# Step 5: View results
summary(ols_model_qf_qb)





############ Ridge learners######
set.seed(1234)
ml_lR_qf_qb <- lrn("regr.cv_glmnet", s = "lambda.min", alpha = 0)  # Ridge regression for Y ~ X
ml_mR_qf_qb <- lrn("classif.cv_glmnet", predict_type = "prob", s = "lambda.min", alpha = 0) # Ridge classification for D ~ X

# Train DML model (Ridge)
dml_multiR_qf_qb <- DoubleMLPLR$new(dml_data_qf_qb, ml_l = ml_lR_qf_qb, ml_m = ml_mR_qf_qb)
dml_multiR_qf_qb$fit()

# Output results (Ridge)
dml_multiR_qf_qb$summary()


############ Random Forest learners######
set.seed(1234)
ml_lRF_qf_qb <- lrn("regr.ranger") # RF for Y ~ X
ml_mRF_qf_qb <- lrn("classif.ranger", predict_type = "prob") # RF for D ~ X

# Train DML model (Random Forest)
dml_multiRF_qf_qb <- DoubleMLPLR$new(dml_data_qf_qb, ml_l = ml_lRF_qf_qb, ml_m = ml_mRF_qf_qb)
dml_multiRF_qf_qb$fit()

# Output results (Random Forest)
dml_multiRF_qf_qb$summary()




#############################################################################
#                       DDML (consider subgroup)  
#############################################################################
####Install package####
install.packages("fastDummies")
library(fastDummies)
#####reproduction####
set.seed(1234)
#####################################################################
#                         Subgroup Classification
#####################################################################

######### Income class data cleaning#######

# Step 1: Remove NAs
income_clean <- na.omit(cecc$cps21_income_number)

# Step 2: Define outliers (using IQR method)
Q1 <- quantile(income_clean, 0.25)
Q3 <- quantile(income_clean, 0.75)
IQR <- Q3 - Q1

# Step 3: Filter out outliers
income_no_outliers <- income_clean[income_clean >= (Q1 - 1.5 * IQR) &
                                     income_clean <= (Q3 + 1.5 * IQR)]
#plot density
plot(density(income_no_outliers, na.rm = TRUE),
     main = "Income Density",
     xlab = "Income Number",
     col = "darkblue",
     lwd = 2)

#plot histogram
hist(income_no_outliers,
     main = "Income Frequency",
     xlab = "Income Number",
     col = "lightblue",
     border = "white",
     freq = TRUE) 

######### Income class data classify######

#Step 1: Get data bounds
min_income <- min(income_no_outliers)
max_income <- max(income_no_outliers)

#Step 2: Define class breaks
breaks <- c(min_income,         # lower bound of Lower Class
            53359,
            106717,
            235675,
            max_income)         # upper bound of Upper Class

#Step 3: Define class labels
labels <- c("Lower Class",
            "Middle Class",
            "Upper Middle Class",
            "Upper Class")

#Step 4: Categorize incomes
income_class <- cut(income_no_outliers,
                    breaks = breaks,
                    labels = labels,
                    include.lowest = TRUE,
                    right = FALSE)

#Step 5: Check the distribution
table(income_class)
barplot(table(income_class),
        col = "lightblue",
        main = "Income Class Distribution",
        ylab = "Frequency")

#Create income_class from cecc$cps21_income_number
cecc <- cecc %>%
  mutate(income_class = cut(
    cps21_income_number,
    breaks = c(min(cps21_income_number, na.rm = TRUE),
               53359, 106717, 235675,
               max(cps21_income_number, na.rm = TRUE)),
    labels = c("Lower Class", "Middle Class", "Upper Middle Class", "Upper Class"),
    include.lowest = TRUE,
    right = FALSE
  ))
#Filter income base data, for >1000 OBS variables
cecc_mi <- cecc %>% filter(income_class == "Middle Class")
cecc_upmi <- cecc %>% filter(income_class == "Upper Middle Class")
cecc_lo <- cecc %>% filter(income_class == "Lower Class")

library(ggplot2)

# Create a data frame for income class distribution
income_df <- data.frame(income_class = income_class)

# Minimalist plot
ggplot(income_df, aes(x = income_class)) +
  geom_bar(fill = "#4C78A8", width = 0.6) +
  labs(title = "Income Class Distribution",
       x = "Income Class",
       y = "Count") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),  # Remove vertical gridlines
    panel.grid.minor = element_blank()
  )


######### Age group data cleaning #######

# Step 1: Remove NAs
age_clean <- na.omit(cecc$age)

# Step 2: Define outliers (using IQR method)
Q1_age <- quantile(age_clean, 0.25)
Q3_age <- quantile(age_clean, 0.75)
IQR_age <- Q3_age - Q1_age

# Step 3: Filter out outliers
age_no_outliers <- age_clean[age_clean >= (Q1_age - 1.5 * IQR_age) &
                               age_clean <= (Q3_age + 1.5 * IQR_age)]

# Plot density
plot(density(age_no_outliers, na.rm = TRUE),
     main = "Age Density",
     xlab = "Age",
     col = "darkgreen",
     lwd = 2)

# Plot histogram
hist(age_no_outliers,
     main = "Age Frequency",
     xlab = "Age",
     col = "lightgreen",
     border = "white",
     freq = TRUE)


######### Age group data classify #######

# Define breaks using your data's actual min and max
min_age <- min(age_no_outliers)
max_age <- max(age_no_outliers)

# Age group breakpoints
age_breaks <- c(min_age, 15, 25, 65, max_age)

# Group labels
age_labels <- c("Children", "Youth", "Adults", "Seniors")

# Create factor variable with age groups
age_group <- cut(age_no_outliers,
                 breaks = age_breaks,
                 labels = age_labels,
                 include.lowest = TRUE,
                 right = FALSE)

library(ggplot2)

# Age group classification
age_df <- data.frame(
  group = cut(age_no_outliers,
              breaks = c(min(age_no_outliers), 15, 25, 65, max(age_no_outliers)),
              labels = c("Children", "Youth", "Adults", "Seniors"),
              include.lowest = TRUE,
              right = FALSE)
)

# Plot bar chart with monotone color
ggplot(age_df, aes(x = group)) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(title = "Age Group Frequency",
       x = "Age Group",
       y = "Count") +
  theme_minimal()

#the Distribution of OBS, exclude <1000 OBS variables 
table(age_df$group)

#Create age_group from cecc$age, not just age_no_outliers
cecc <- cecc %>%
  mutate(age_group = cut(
    age,
    breaks = c(min(age, na.rm = TRUE), 15, 25, 65, max(age, na.rm = TRUE)),
    labels = c("Children", "Youth", "Adults", "Seniors"),
    include.lowest = TRUE,
    right = FALSE
  ))
#Filter age base data, for >1000 OBS variables
cecc_yo <- cecc %>% filter(age_group == "Youth")
cecc_ad <- cecc %>% filter(age_group == "Adults")
cecc_se <- cecc %>% filter(age_group == "Seniors")

ggplot(age_df, aes(x = group)) +
  geom_bar(fill = "#4C78A8", width = 0.6) +
  labs(title = "Age Group Distribution",
       x = "Age Group",
       y = "Count") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )


######### Edu group data classify#######

# Then recode education groups
cecc <- cecc %>%
  mutate(education_group = case_when(
    cps21_new_education %in% c(1, 2, 3, 4) ~ "Less than high school",
    cps21_new_education == 5              ~ "High school",
    cps21_new_education %in% c(6, 7)      ~ "College/Technical",
    cps21_new_education %in% c(8, 9, 10, 11) ~ "University",
    TRUE ~ NA_character_
  ))

table(cecc$education_group)

library(ggplot2)

ggplot(cecc, aes(x = education_group)) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(title = "Education Level Distribution",
       x = "Education Group",
       y = "Count") +
  theme_minimal()

cecc_uni <- cecc %>% filter(education_group == "University")
cecc_hs  <- cecc %>% filter(education_group == "High school")
cecc_ct  <- cecc %>% filter(education_group == "College/Technical")
cecc_Lhs  <- cecc %>% filter(education_group == "Less than high school")

######### Province data classify to Regions ############

cecc$region_ <- dplyr::case_when(
  cecc$cps21_new_province %in% c(5, 10, 7, 4) ~ "Atlantic",        # Newfoundland, PEI, NS, NB
  cecc$cps21_new_province == 11              ~ "Quebec",          # Quebec
  cecc$cps21_new_province == 9               ~ "Ontario",         # Ontario
  cecc$cps21_new_province %in% c(3, 12, 1)   ~ "Prairies",        # Manitoba, Saskatchewan, Alberta
  cecc$cps21_new_province == 2               ~ "British Columbia",# BC
  cecc$cps21_new_province %in% c(13, 6, 8)   ~ "Territories",     # Yukon, NWT, Nunavut
  TRUE ~ NA_character_
)

library(ggplot2)

ggplot(cecc, aes(x = region_)) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(title = "Respondent Count by Region",
       x = "Region",
       y = "Count") +
  theme_minimal()

#the Distribution of OBS, exclude <1000 OBS variables 
table(cecc$region_)

#Filter region base data, for >1000 OBS variables
cecc_on <- cecc %>% filter(region_ == "Ontario")
cecc_qu <- cecc %>% filter(region_ == "Quebec")
cecc_pr <- cecc %>% filter(region_ == "Prairies")
cecc_at <- cecc %>% filter(region_ == "Atlantic")
cecc_bc <- cecc %>% filter(region_ == "British Columbia")


######### Gender data ############
library(dplyr)

cecc <- cecc %>%
  mutate(gender_label = case_when(
    cps21_new_genderid == 1 ~ "Man",
    cps21_new_genderid == 2 ~ "Woman",
    cps21_new_genderid == 3 ~ "Non-binary",
    TRUE ~ NA_character_
  ))

#plot Hist
library(ggplot2)

ggplot(cecc, aes(x = gender_label)) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(title = "Gender Distribution",
       x = "Gender",
       y = "Count") +
  theme_minimal()

#the Distribution of OBS, exclude <1000 OBS variables 
table(cecc$gender_label)


#Filter gender base data, for >1000 OBS variables
cecc_man <- cecc %>%
  filter(cps21_new_genderid == 1)

cecc_woman <- cecc %>%
  filter(cps21_new_genderid == 2)



######### Covid_sat data ############

#Flip the numeric scale, and labeling
cecc <- cecc %>%
  mutate(
    cps21_covid_sat_1_new = case_when(
      cps21_new_covid_sat_1 == 1 ~ 4,
      cps21_new_covid_sat_1 == 2 ~ 3,
      cps21_new_covid_sat_1 == 3 ~ 2,
      cps21_new_covid_sat_1 == 4 ~ 1,
      TRUE ~ NA_real_
    ),
    cps21_new_covid_sat_1 = factor(
      cps21_new_covid_sat_1,
      levels = c(1, 2, 3, 4),
      labels = c("Not at all satisfied",
                 "Not very satisfied",
                 "Fairly satisfied",
                 "Very satisfied")
    )
  )

#drop the Haven_label
cecc <- cecc %>%
  mutate(cps21_new_covid_sat_1 = as.numeric(cps21_new_covid_sat_1))

#Distribution
table(cecc$cps21_new_covid_sat_1)

cecc_ver_sat <- cecc %>% filter(cps21_new_covid_sat_1 == 4)
cecc_Fair_sat <- cecc %>% filter(cps21_new_covid_sat_1 == 3)
cecc_Nver_sat <- cecc %>% filter(cps21_new_covid_sat_1 == 2)
cecc_Not_sat <- cecc %>% filter(cps21_new_covid_sat_1 == 1)

##############################################
#         Compare Liberal w/ gender  
##############################################
######### For Man ########
set.seed(1234)
cecc_dml_man <- cecc_man %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)


cecc_dml_man <- as.data.table(cecc_dml_man)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_man), c(y_col, d_cols))  # Control variables

dml_data_man <- DoubleMLData$new(data = cecc_dml_man, 
                             y_col = y_col, 
                             d_cols = d_cols, 
                             x_cols = x_cols)
# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_man <- DoubleMLPLR$new(dml_data_man, ml_l = ml_l, ml_m = ml_m)
dml_multi_man$fit()

# Output results (LASSO)
dml_multi_man$summary()




######### For Woman #######
set.seed(1234)
cecc_dml_woman <- cecc_woman %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)


cecc_dml_woman <- as.data.table(cecc_dml_woman)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_woman), c(y_col, d_cols))  # Control variables

dml_data_woman <- DoubleMLData$new(data = cecc_dml_woman, 
                                 y_col = y_col, 
                                 d_cols = d_cols, 
                                 x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_woman <- DoubleMLPLR$new(dml_data_woman, ml_l = ml_l, ml_m = ml_m)
dml_multi_woman$fit()

# Output results (LASSO)
dml_multi_woman$summary()



##############################################
#          Compare Liberal w/ Age 
##############################################
######### For Youth ########
set.seed(1234)
cecc_dml_yo <- cecc_yo %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)))


cecc_dml_yo <- as.data.table(cecc_dml_yo)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_yo), c(y_col, d_cols))  # Control variables

dml_data_yo <- DoubleMLData$new(data = cecc_dml_yo, 
                                y_col = y_col, 
                                d_cols = d_cols, 
                                x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_yo <- DoubleMLPLR$new(dml_data_yo, ml_l = ml_l, ml_m = ml_m)
dml_multi_yo$fit()

# Output results (LASSO)
dml_multi_yo$summary()

######### For Adults ########
set.seed(1234)
cecc_dml_ad <- cecc_ad %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)))


cecc_dml_ad <- as.data.table(cecc_dml_ad)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_ad), c(y_col, d_cols))  # Control variables

dml_data_ad <- DoubleMLData$new(data = cecc_dml_ad, 
                                 y_col = y_col, 
                                 d_cols = d_cols, 
                                 x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_ad <- DoubleMLPLR$new(dml_data_ad, ml_l = ml_l, ml_m = ml_m)
dml_multi_ad$fit()

# Output results (LASSO)
dml_multi_ad$summary()

######### For Seniors ########
set.seed(1234)
cecc_dml_se <- cecc_se %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)))


cecc_dml_se <- as.data.table(cecc_dml_se)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_se), c(y_col, d_cols))  # Control variables

dml_data_se <- DoubleMLData$new(data = cecc_dml_se, 
                                y_col = y_col, 
                                d_cols = d_cols, 
                                x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_se <- DoubleMLPLR$new(dml_data_se, ml_l = ml_l, ml_m = ml_m)
dml_multi_se$fit()

# Output results (LASSO)
dml_multi_se$summary()




##############################################
#         Compare Liberal w/ Income  
##############################################
######### For Lower Class ########
set.seed(1234)
cecc_dml_lo <- cecc_lo %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)))


cecc_dml_lo <- as.data.table(cecc_dml_lo)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_lo), c(y_col, d_cols))  # Control variables

dml_data_lo <- DoubleMLData$new(data = cecc_dml_lo, 
                                y_col = y_col, 
                                d_cols = d_cols, 
                                x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_lo <- DoubleMLPLR$new(dml_data_lo, ml_l = ml_l, ml_m = ml_m)
dml_multi_lo$fit()

# Output results (LASSO)
dml_multi_lo$summary()

######### For Middle Class ########
set.seed(1234)
cecc_dml_mi <- cecc_mi %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)))


cecc_dml_mi <- as.data.table(cecc_dml_mi)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_mi), c(y_col, d_cols))  # Control variables

dml_data_mi <- DoubleMLData$new(data = cecc_dml_mi, 
                                y_col = y_col, 
                                d_cols = d_cols, 
                                x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_mi <- DoubleMLPLR$new(dml_data_mi, ml_l = ml_l, ml_m = ml_m)
dml_multi_mi$fit()

# Output results (LASSO)
dml_multi_mi$summary()

######### For Upper-Middle Class ########
set.seed(1234)
cecc_dml_upmi <- cecc_upmi %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)))


cecc_dml_upmi <- as.data.table(cecc_dml_upmi)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_upmi), c(y_col, d_cols))  # Control variables

dml_data_upmi <- DoubleMLData$new(data = cecc_dml_upmi, 
                                y_col = y_col, 
                                d_cols = d_cols, 
                                x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_upmi <- DoubleMLPLR$new(dml_data_upmi, ml_l = ml_l, ml_m = ml_m)
dml_multi_upmi$fit()

# Output results (LASSO)
dml_multi_upmi$summary()



##############################################
#        Compare Liberal w/ Region 
##############################################
######### For Atlantic ########
set.seed(1234)
cecc_dml_at <- cecc_at %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)


cecc_dml_at <- as.data.table(cecc_dml_at)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_at), c(y_col, d_cols))  # Control variables

dml_data_at <- DoubleMLData$new(data = cecc_dml_at, 
                                y_col = y_col, 
                                d_cols = d_cols, 
                                x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_at <- DoubleMLPLR$new(dml_data_at, ml_l = ml_l, ml_m = ml_m)
dml_multi_at$fit()

# Output results (LASSO)
dml_multi_at$summary()

######### For BC ########
set.seed(1234)
cecc_dml_bc <- cecc_bc %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)


cecc_dml_bc <- as.data.table(cecc_dml_bc)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_bc), c(y_col, d_cols))  # Control variables

dml_data_bc <- DoubleMLData$new(data = cecc_dml_bc, 
                                y_col = y_col, 
                                d_cols = d_cols, 
                                x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_bc <- DoubleMLPLR$new(dml_data_bc, ml_l = ml_l, ml_m = ml_m)
dml_multi_bc$fit()

# Output results (LASSO)
dml_multi_bc$summary()

######### For Ontario ########
set.seed(1234)
cecc_dml_on <- cecc_on %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)


cecc_dml_on <- as.data.table(cecc_dml_on)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_on), c(y_col, d_cols))  # Control variables

dml_data_on <- DoubleMLData$new(data = cecc_dml_on, 
                                y_col = y_col, 
                                d_cols = d_cols, 
                                x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.05

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_on <- DoubleMLPLR$new(dml_data_on, ml_l = ml_l, ml_m = ml_m)
dml_multi_on$fit()

# Output results (LASSO)
dml_multi_on$summary()

######### For Quebec ########
set.seed(1234)
cecc_dml_qu <- cecc_qu %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)


cecc_dml_qu <- as.data.table(cecc_dml_qu)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_qu), c(y_col, d_cols))  # Control variables

dml_data_qu <- DoubleMLData$new(data = cecc_dml_qu, 
                                y_col = y_col, 
                                d_cols = d_cols, 
                                x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda )  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda ) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_qu <- DoubleMLPLR$new(dml_data_qu, ml_l = ml_l, ml_m = ml_m)
dml_multi_qu$fit()

# Output results (LASSO)
dml_multi_qu$summary()

######### For Prairies ########
set.seed(1234)
cecc_dml_pr <- cecc_pr %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)


cecc_dml_pr <- as.data.table(cecc_dml_pr)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_pr), c(y_col, d_cols))  # Control variables

dml_data_pr <- DoubleMLData$new(data = cecc_dml_pr, 
                                y_col = y_col, 
                                d_cols = d_cols, 
                                x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.05

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_pr <- DoubleMLPLR$new(dml_data_pr, ml_l = ml_l, ml_m = ml_m)
dml_multi_pr$fit()

# Output results (LASSO)
dml_multi_pr$summary()


##############################################
#       Compare Liberal w/ Education
##############################################
######### For Less than High School ########
set.seed(1234)
cecc_dml_Lhs <- cecc_Lhs %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)

cecc_dml_Lhs <- as.data.table(cecc_dml_Lhs)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_Lhs), c(y_col, d_cols))  # Control variables

dml_data_Lhs <- DoubleMLData$new(data = cecc_dml_Lhs, 
                                y_col = y_col, 
                                d_cols = d_cols, 
                                x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_Lhs <- DoubleMLPLR$new(dml_data_Lhs, ml_l = ml_l, ml_m = ml_m)
dml_multi_Lhs$fit()

# Output results (LASSO)
dml_multi_Lhs$summary()

######### For High School ########
set.seed(1234)
cecc_dml_hs <- cecc_hs %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)

cecc_dml_hs <- as.data.table(cecc_dml_hs)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_hs), c(y_col, d_cols))  # Control variables

dml_data_hs <- DoubleMLData$new(data = cecc_dml_hs, 
                                y_col = y_col, 
                                d_cols = d_cols, 
                                x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda )  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda ) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_hs <- DoubleMLPLR$new(dml_data_hs, ml_l = ml_l, ml_m = ml_m)
dml_multi_hs$fit()

# Output results (LASSO)
dml_multi_hs$summary()

######### For College/Technical ########
set.seed(1234)
cecc_dml_ct <- cecc_ct %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)


cecc_dml_ct <- as.data.table(cecc_dml_ct)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_ct), c(y_col, d_cols))  # Control variables

dml_data_ct <- DoubleMLData$new(data = cecc_dml_ct, 
                                y_col = y_col, 
                                d_cols = d_cols, 
                                x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_ct <- DoubleMLPLR$new(dml_data_ct, ml_l = ml_l, ml_m = ml_m)
dml_multi_ct$fit()

# Output results (LASSO)
dml_multi_ct$summary()


######### For University ########
set.seed(1234)
cecc_dml_uni <- cecc_uni %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)


cecc_dml_uni <- as.data.table(cecc_dml_uni)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_uni), c(y_col, d_cols))  # Control variables

dml_data_uni <- DoubleMLData$new(data = cecc_dml_uni, 
                                 y_col = y_col, 
                                 d_cols = d_cols, 
                                 x_cols = x_cols)
# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_uni <- DoubleMLPLR$new(dml_data_uni, ml_l = ml_l, ml_m = ml_m)
dml_multi_uni$fit()

# Output results (LASSO)
dml_multi_uni$summary()

##############################################
#        Compare Liberal w/ Covid_sat
##############################################
######### For Very sat ########
set.seed(1234)
cecc_dml_ver_sat <- cecc_ver_sat %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)

cecc_dml_ver_sat <- as.data.table(cecc_dml_ver_sat)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_ver_sat), c(y_col, d_cols))  # Control variables

dml_data_ver_sat <- DoubleMLData$new(data = cecc_dml_ver_sat, 
                                 y_col = y_col, 
                                 d_cols = d_cols, 
                                 x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_ver_sat <- DoubleMLPLR$new(dml_data_ver_sat, ml_l = ml_l, ml_m = ml_m)
dml_multi_ver_sat$fit()

# Output results (LASSO)
dml_multi_ver_sat$summary()

######### For Fairly sat ########
set.seed(1234)
cecc_dml_Fair_sat <- cecc_Fair_sat %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)

cecc_dml_Fair_sat <- as.data.table(cecc_dml_Fair_sat)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_Fair_sat), c(y_col, d_cols))  # Control variables

dml_data_Fair_sat <- DoubleMLData$new(data = cecc_dml_Fair_sat, 
                                     y_col = y_col, 
                                     d_cols = d_cols, 
                                     x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_Fair_sat <- DoubleMLPLR$new(dml_data_Fair_sat, ml_l = ml_l, ml_m = ml_m)
dml_multi_Fair_sat$fit()

# Output results (LASSO)
dml_multi_Fair_sat$summary()



######### For Not very sat ########
set.seed(1234)
cecc_dml_Nver_sat <- cecc_Nver_sat %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)

cecc_dml_Nver_sat <- as.data.table(cecc_dml_Nver_sat)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_Nver_sat), c(y_col, d_cols))  # Control variables

dml_data_Nver_sat <- DoubleMLData$new(data = cecc_dml_Nver_sat, 
                                     y_col = y_col, 
                                     d_cols = d_cols, 
                                     x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_Nver_sat <- DoubleMLPLR$new(dml_data_Nver_sat, ml_l = ml_l, ml_m = ml_m)
dml_multi_Nver_sat$fit()

# Output results (LASSO)
dml_multi_Nver_sat$summary()

######### For Not at all sat ########
set.seed(1234)
cecc_dml_Not_sat <- cecc_Not_sat %>%
  mutate(
    liberal = ifelse(post_party_choice == 1, 1, 0),        # Dummy: 1 if Liberal supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "liberal", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)

cecc_dml_Not_sat <- as.data.table(cecc_dml_Not_sat)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "liberal"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_Not_sat), c(y_col, d_cols))  # Control variables

dml_data_Not_sat <- DoubleMLData$new(data = cecc_dml_Not_sat, 
                                     y_col = y_col, 
                                     d_cols = d_cols, 
                                     x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_Not_sat <- DoubleMLPLR$new(dml_data_Not_sat, ml_l = ml_l, ml_m = ml_m)
dml_multi_Not_sat$fit()

# Output results (LASSO)
dml_multi_Not_sat$summary()


##############################################
#       Compare Conservative w/ gender
##############################################
######### For Man ########
set.seed(1234)
cecc_dml_man_con <- cecc_man %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)

cecc_dml_man_con <- as.data.table(cecc_dml_man_con)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "conservative"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_man_con), c(y_col, d_cols))  # Control variables

dml_data_man_con <- DoubleMLData$new(data = cecc_dml_man_con, 
                                 y_col = y_col, 
                                 d_cols = d_cols, 
                                 x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_man_con <- DoubleMLPLR$new(dml_data_man_con, ml_l = ml_l, ml_m = ml_m)
dml_multi_man_con$fit()

# Output results (LASSO)
dml_multi_man_con$summary()


######### For Woman #######
set.seed(1234)
cecc_dml_woman_con <- cecc_woman %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)

cecc_dml_woman_con <- as.data.table(cecc_dml_woman_con)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "conservative"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_woman_con), c(y_col, d_cols))  # Control variables

dml_data_woman_con <- DoubleMLData$new(data = cecc_dml_woman_con, 
                                   y_col = y_col, 
                                   d_cols = d_cols, 
                                   x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_woman_con <- DoubleMLPLR$new(dml_data_woman_con, ml_l = ml_l, ml_m = ml_m)
dml_multi_woman_con$fit()

# Output results (LASSO)
dml_multi_woman_con$summary()



##############################################
#          Compare Conservative w/ Age 
##############################################
######### For Youth ########
set.seed(1234)
cecc_dml_yo_con <- cecc_yo %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )

cecc_dml_yo_con <- as.data.table(cecc_dml_yo_con)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "conservative"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_yo_con), c(y_col, d_cols))  # Control variables

dml_data_yo_con <- DoubleMLData$new(data = cecc_dml_yo_con, 
                                    y_col = y_col, 
                                    d_cols = d_cols, 
                                    x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_yo_con <- DoubleMLPLR$new(dml_data_yo_con, ml_l = ml_l, ml_m = ml_m)
dml_multi_yo_con$fit()

# Output results (LASSO)
dml_multi_yo_con$summary()

######### For Adults ########
set.seed(1234)
cecc_dml_ad_con <- cecc_ad %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )

cecc_dml_ad_con <- as.data.table(cecc_dml_ad_con)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "conservative"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_ad_con), c(y_col, d_cols))  # Control variables

dml_data_ad_con <- DoubleMLData$new(data = cecc_dml_ad_con, 
                                y_col = y_col, 
                                d_cols = d_cols, 
                                x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_ad_con <- DoubleMLPLR$new(dml_data_ad_con, ml_l = ml_l, ml_m = ml_m)
dml_multi_ad_con$fit()

# Output results (LASSO)
dml_multi_ad_con$summary()

######### For Seniors ########
set.seed(1234)
cecc_dml_se_con <- cecc_se %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )

cecc_dml_se_con <- as.data.table(cecc_dml_se_con)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "conservative"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_se_con), c(y_col, d_cols))  # Control variables

dml_data_se_con <- DoubleMLData$new(data = cecc_dml_se_con, 
                                y_col = y_col, 
                                d_cols = d_cols, 
                                x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_se_con <- DoubleMLPLR$new(dml_data_se_con, ml_l = ml_l, ml_m = ml_m)
dml_multi_se_con$fit()

# Output results (LASSO)
dml_multi_se_con$summary()




##############################################
#         Compare Conservative w/ Income 
##############################################
######### For Lower Class ########
set.seed(1234)
cecc_dml_lo_con <- cecc_lo %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )

cecc_dml_lo_con <- as.data.table(cecc_dml_lo_con)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "conservative"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_lo_con), c(y_col, d_cols))  # Control variables

dml_data_lo_con <- DoubleMLData$new(data = cecc_dml_lo_con, 
                                y_col = y_col, 
                                d_cols = d_cols, 
                                x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_lo_con <- DoubleMLPLR$new(dml_data_lo_con, ml_l = ml_l, ml_m = ml_m)
dml_multi_lo_con$fit()

# Output results (LASSO)
dml_multi_lo_con$summary()

######### For Middle Class ########
set.seed(1234)
cecc_dml_mi_con <- cecc_mi %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )


cecc_dml_mi_con <- as.data.table(cecc_dml_mi_con)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "conservative"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_mi_con), c(y_col, d_cols))  # Control variables

dml_data_mi_con <- DoubleMLData$new(data = cecc_dml_mi_con, 
                                y_col = y_col, 
                                d_cols = d_cols, 
                                x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_mi_con <- DoubleMLPLR$new(dml_data_mi_con, ml_l = ml_l, ml_m = ml_m)
dml_multi_mi_con$fit()

# Output results (LASSO)
dml_multi_mi_con$summary()

######### For Upper-Middle Class ########
set.seed(1234)
cecc_dml_upmi_con <- cecc_upmi %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )


cecc_dml_upmi_con <- as.data.table(cecc_dml_upmi_con)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "conservative"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_upmi_con), c(y_col, d_cols))  # Control variables

dml_data_upmi_con <- DoubleMLData$new(data = cecc_dml_upmi_con, 
                                  y_col = y_col, 
                                  d_cols = d_cols, 
                                  x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda )  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda ) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_upmi_con <- DoubleMLPLR$new(dml_data_upmi_con, ml_l = ml_l, ml_m = ml_m)
dml_multi_upmi_con$fit()

# Output results (LASSO)
dml_multi_upmi_con$summary()



##############################################
#         Compare Conservative w/ Region 
##############################################
######### For Atlantic ########
set.seed(1234)
cecc_dml_at_con <- cecc_at %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)


cecc_dml_at_con <- as.data.table(cecc_dml_at_con)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "conservative"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_at_con), c(y_col, d_cols))  # Control variables

dml_data_at_con <- DoubleMLData$new(data = cecc_dml_at_con, 
                                    y_col = y_col, 
                                    d_cols = d_cols, 
                                    x_cols = x_cols)


# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_at_con <- DoubleMLPLR$new(dml_data_at_con, ml_l = ml_l, ml_m = ml_m)
dml_multi_at_con$fit()

# Output results (LASSO)
dml_multi_at_con$summary()

######### For BC ########
set.seed(1234)
cecc_dml_bc_con <- cecc_bc %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)


cecc_dml_bc_con <- as.data.table(cecc_dml_bc_con)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "conservative"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_bc_con), c(y_col, d_cols))  # Control variables

dml_data_bc_con <- DoubleMLData$new(data = cecc_dml_bc_con, 
                                    y_col = y_col, 
                                    d_cols = d_cols, 
                                    x_cols = x_cols)


# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_bc_con <- DoubleMLPLR$new(dml_data_bc_con, ml_l = ml_l, ml_m = ml_m)
dml_multi_bc_con$fit()

# Output results (LASSO)
dml_multi_bc_con$summary()

######### For Ontario ########
set.seed(1234)
cecc_dml_on_con <- cecc_on %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)


cecc_dml_on_con <- as.data.table(cecc_dml_on_con)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "conservative"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_on_con), c(y_col, d_cols))  # Control variables

dml_data_on_con <- DoubleMLData$new(data = cecc_dml_on_con, 
                                y_col = y_col, 
                                d_cols = d_cols, 
                                x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.05

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_on_con <- DoubleMLPLR$new(dml_data_on_con, ml_l = ml_l, ml_m = ml_m)
dml_multi_on_con$fit()

# Output results (LASSO)
dml_multi_on_con$summary()

######### For Quebec ########
set.seed(1234)
cecc_dml_qu_con <- cecc_qu %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)


cecc_dml_qu_con <- as.data.table(cecc_dml_qu_con)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "conservative"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_qu_con), c(y_col, d_cols))  # Control variables

dml_data_qu_con <- DoubleMLData$new(data = cecc_dml_qu_con, 
                                y_col = y_col, 
                                d_cols = d_cols, 
                                x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_qu_con <- DoubleMLPLR$new(dml_data_qu_con, ml_l = ml_l, ml_m = ml_m)
dml_multi_qu_con$fit()

# Output results (LASSO)
dml_multi_qu_con$summary()

######### For Prairies ########
set.seed(1234)
cecc_dml_pr_con <- cecc_pr %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)


cecc_dml_pr_con <- as.data.table(cecc_dml_pr_con)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "conservative"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_pr_con), c(y_col, d_cols))  # Control variables

dml_data_pr_con <- DoubleMLData$new(data = cecc_dml_pr_con, 
                                y_col = y_col, 
                                d_cols = d_cols, 
                                x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.05

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_pr_con <- DoubleMLPLR$new(dml_data_pr_con, ml_l = ml_l, ml_m = ml_m)
dml_multi_pr_con$fit()

# Output results (LASSO)
dml_multi_pr_con$summary()


##############################################
#        Compare Conservative w/ Education 
##############################################
######### For Less than High School ########
set.seed(1234)
cecc_dml_Lhs_con <- cecc_Lhs %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)


cecc_dml_Lhs_con <- as.data.table(cecc_dml_Lhs_con)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "conservative"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_Lhs_con), c(y_col, d_cols))  # Control variables

dml_data_Lhs_con <- DoubleMLData$new(data = cecc_dml_Lhs_con, 
                                    y_col = y_col, 
                                    d_cols = d_cols, 
                                    x_cols = x_cols)


# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_Lhs_con <- DoubleMLPLR$new(dml_data_Lhs_con, ml_l = ml_l, ml_m = ml_m)
dml_multi_Lhs_con$fit()

# Output results (LASSO)
dml_multi_Lhs_con$summary()

######### For High School ########
set.seed(1234)
cecc_dml_hs_con <- cecc_hs %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)


cecc_dml_hs_con <- as.data.table(cecc_dml_hs_con)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "conservative"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_hs_con), c(y_col, d_cols))  # Control variables

dml_data_hs_con <- DoubleMLData$new(data = cecc_dml_hs_con, 
                                y_col = y_col, 
                                d_cols = d_cols, 
                                x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_hs_con <- DoubleMLPLR$new(dml_data_hs_con, ml_l = ml_l, ml_m = ml_m)
dml_multi_hs_con$fit()

# Output results (LASSO)
dml_multi_hs_con$summary()

######### For College/Technical ########
set.seed(1234)
cecc_dml_ct_con <- cecc_ct %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)


cecc_dml_ct_con <- as.data.table(cecc_dml_ct_con)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "conservative"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_ct_con), c(y_col, d_cols))  # Control variables

dml_data_ct_con <- DoubleMLData$new(data = cecc_dml_ct_con, 
                                    y_col = y_col, 
                                    d_cols = d_cols, 
                                    x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_ct_con <- DoubleMLPLR$new(dml_data_ct_con, ml_l = ml_l, ml_m = ml_m)
dml_multi_ct_con$fit()

# Output results (LASSO)
dml_multi_ct_con$summary()


######### For University ########
set.seed(1234)
cecc_dml_uni_con <- cecc_uni %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)

cecc_dml_uni_con <- as.data.table(cecc_dml_uni_con)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "conservative"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_uni_con), c(y_col, d_cols))  # Control variables

dml_data_uni_con <- DoubleMLData$new(data = cecc_dml_uni_con, 
                                 y_col = y_col, 
                                 d_cols = d_cols, 
                                 x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_uni_con <- DoubleMLPLR$new(dml_data_uni_con, ml_l = ml_l, ml_m = ml_m)
dml_multi_uni_con$fit()

# Output results (LASSO)
dml_multi_uni_con$summary()


##############################################
#       Compare Conserv w/ Covid_sat
##############################################
######### For Very sat ########
set.seed(1234)
cecc_dml_ver_sat_con <- cecc_ver_sat %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)


cecc_dml_ver_sat_con <- as.data.table(cecc_dml_ver_sat_con)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "conservative"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_ver_sat_con), c(y_col, d_cols))  # Control variables

dml_data_ver_sat_con <- DoubleMLData$new(data = cecc_dml_ver_sat_con, 
                                     y_col = y_col, 
                                     d_cols = d_cols, 
                                     x_cols = x_cols)


# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda )  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda ) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_ver_sat_con <- DoubleMLPLR$new(dml_data_ver_sat_con, ml_l = ml_l, ml_m = ml_m)
dml_multi_ver_sat_con$fit()

# Output results (LASSO)
dml_multi_ver_sat_con$summary()

######### For Fairly sat ########
set.seed(1234)
cecc_dml_Fair_sat_con <- cecc_Fair_sat %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)

cecc_dml_Fair_sat_con <- as.data.table(cecc_dml_Fair_sat_con)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "conservative"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_Fair_sat_con), c(y_col, d_cols))  # Control variables

dml_data_Fair_sat_con <- DoubleMLData$new(data = cecc_dml_Fair_sat_con, 
                                      y_col = y_col, 
                                      d_cols = d_cols, 
                                      x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda )  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda ) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_Fair_sat_con <- DoubleMLPLR$new(dml_data_Fair_sat_con, ml_l = ml_l, ml_m = ml_m)
dml_multi_Fair_sat_con$fit()

# Output results (LASSO)
dml_multi_Fair_sat_con$summary()



######### For Not very sat ########
set.seed(1234)
cecc_dml_Nver_sat_con <- cecc_Nver_sat %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)


cecc_dml_Nver_sat_con <- as.data.table(cecc_dml_Nver_sat_con)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "conservative"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_Nver_sat_con), c(y_col, d_cols))  # Control variables

dml_data_Nver_sat_con <- DoubleMLData$new(data = cecc_dml_Nver_sat_con, 
                                      y_col = y_col, 
                                      d_cols = d_cols, 
                                      x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_Nver_sat_con <- DoubleMLPLR$new(dml_data_Nver_sat_con, ml_l = ml_l, ml_m = ml_m)
dml_multi_Nver_sat_con$fit()

# Output results (LASSO)
dml_multi_Nver_sat_con$summary()

######### For Not at all sat ########
set.seed(1234)
cecc_dml_Not_sat_con <- cecc_Not_sat %>%
  mutate(
    conservative = ifelse(post_party_choice == 2, 1, 0),  # Dummy: 1 if Conservative supporter
    dem_sat_change = pes21_dem_sat - cps21_demsat  # Post - Pre democratic satisfaction
  ) %>%
  select(
    # Treatment & Outcome
    "conservative", "dem_sat_change",
    
    # Demographics
    "UserLanguage_dummy", "age",
    starts_with("born_in_canada"), 
    starts_with("cps21_marital"),
    "religion_None","cps21_vismin_9",
    starts_with("cps21_genderid"),
    "sexuality_straight",
    starts_with("race"),
    starts_with("region"),
    starts_with("education_group"), "employment_dummy",
    
    # Finances & Economy
    "cps21_income_number",
    "cps21_own_fin_retro", "cps21_own_fin_future",
    "cps21_ownfinanc_fed", "cps21_econ_fed_bette",
    "cps21_econ_retro",
    
    # Political Interest & Vote Intentions
    "cps21_interest_gen_1", "cps21_interest_elxn_1",
    "post_party_choice",
    
    # Attitudes & Satisfaction
    "pes21_govtcare", "pes21_famvalues", "pes21_equalrights",
    "pes21_envirojob", "pes21_discfam", "pes21_fitin", "pes21_immigjobs",
    
    # Group Thermometers
    "cps21_groups_therm_1", "cps21_groups_therm_2", "cps21_groups_therm_3",
    "cps21_groups_therm_4", "cps21_groups_therm_6", "cps21_groups_therm_7",
    
    # Spending Preferences
    "cps21_spend_educ", "cps21_spend_env", "cps21_spend_defence",
    "cps21_spend_imm_min", "cps21_spend_rec_indi", "cps21_spend_just_law",
    
    # COVID-19
    starts_with("cps21_covid_sat_1"),
    "cps21_vaccine_mandat_1", "cps21_vaccine_mandat_2", "cps21_vaccine_mandat_3", "vaccine_accept"
  ) %>%
  drop_na() %>%
  mutate(
    across(where(~ inherits(.x, "haven_labelled")), ~ as.numeric(.x)),
    across(where(is.character), as.factor)
  )%>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE)

cecc_dml_Not_sat_con <- as.data.table(cecc_dml_Not_sat_con)  # Convert to data.table (required by DoubleML)

# Set variables
y_col <- "dem_sat_change"                  # Outcome variable
d_cols <- "conservative"   # Treatment variables
x_cols <- setdiff(names(cecc_dml_Not_sat_con), c(y_col, d_cols))  # Control variables

dml_data_Not_sat_con <- DoubleMLData$new(data = cecc_dml_Not_sat_con, 
                                     y_col = y_col, 
                                     d_cols = d_cols, 
                                     x_cols = x_cols)

# Manually define lambda (e.g., 0.01 or based on your cv.glmnet plot)
my_lambda <- 0.025

# Lasso learners
ml_l <- lrn("regr.cv_glmnet", s = my_lambda)  # Lasso regression for Y ~ X
ml_m <- lrn("classif.cv_glmnet", predict_type = "prob", s = my_lambda) # Lasso classification for D ~ X

# Train DML model (LASSO)
dml_multi_Not_sat_con <- DoubleMLPLR$new(dml_data_Not_sat_con, ml_l = ml_l, ml_m = ml_m)
dml_multi_Not_sat_con$fit()

# Output results (LASSO)
dml_multi_Not_sat_con$summary()




##############################################
#     Generate graph for easy comparison
##############################################
library(tidyverse)
library(ggplot2)
###### Load .csv table & plot graph#########
df <- read_csv("C:/Users/tcyub/OneDrive/桌面/MA Course/Subgroup_Comparison_Table.csv")

# Reshape data to long format
df_long <- df %>%
  pivot_longer(
    cols = c(Estimate_con, Estimate_lib),
    names_to = "Treatment",
    names_prefix = "Estimate_",
    values_to = "Estimate"
  ) %>%
  mutate(
    Significance = case_when(
      Treatment == "con" ~ Significance_con,
      Treatment == "lib" ~ Significance_lib
    ),
    Treatment = recode(Treatment,
                       "con" = "Conservative",
                       "lib" = "Liberal"),
    Subgroup = factor(Subgroup, levels = unique(Subgroup))
  )%>%
  mutate(
    Significance = str_extract(Significance, "\\*{1,3}|\\.|")
  )

# Plot
ggplot(df_long, aes(x = Subgroup, y = Estimate, fill = Treatment)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = Significance),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Liberal" = "#FDBE34", "Conservative" = "#E65C3A")) +
  labs(
    title = "DDML LASSO Estimates by Subgroup and Political Affiliation (Significance: *, **, ***)",
    x = "Subgroup",
    y = "Estimated Effect on Democratic Satisfaction"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )
