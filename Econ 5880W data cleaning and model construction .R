
###########################################################################
#This R code file has the code for practice and learning machine learning.
#more or less about the data cleaning and other method. 
#note: this file is doing is to selected the variable
###########################################################################



#fist_step#####################################################################
rm(list = ls())
#make sure everthing will be fine.)
#Lode the package#############################################################
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

#lode the data#################################################################

#lode the data if youa run this code in the 
canadian_election <- read_dta("Canadian election/2021 Canadian Election Study v2.0.dta")

###########################################################################
#1.0 Data processing
###########################################################################
###########1.1 lode the data and do a first step filtering#################

#filter out the cols that is not important
remove_cols <- colnames(canadian_election)[grepl("First_Click|Last_Click|t_Click|TEXT|consent|captcha|mail-in ballot|\\bpr\\b", colnames(canadian_election))]

# filter it out
cecc <- canadian_election[, !(colnames(canadian_election) %in% remove_cols)]

#remove the the empty cols
empty_cols <- names(cecc)[colSums(is.na(cecc)) == nrow(cecc)]


###########################################################################
############1.2 manual processing some necessary variable#################
##############Data Quality###########
cecc <-cecc %>%
  filter(
    cps21_data_quality == 0 & pes21_data_quality == 0)

##############personal_information###########
#Canadian Citizen###########
cecc <- cecc  %>%
  filter(cps21_citizenship == 1)

#democratic statstification###########
cecc <- cecc %>%
  filter(pes21_dem_sat != 5 & pes21_data_quality != 5)

#lanauge used###########
cecc$UserLanguage_dummy <- ifelse(cecc$UserLanguage == "EN", 1, 0)


##############age###########
cecc <- cecc %>%
  mutate(birth_year = cps21_yob + 1919,   # Convert coded value to real year
         age = 2021 - birth_year)         # Compute age

##############per_party_choice###########
# # Combine vote-related variables into `party_choice`
# cecc <- cecc %>%
#   mutate(
#     per_party_choice = coalesce(cps21_v_advance, cps21_votechoice, cps21_vote_lean, cps21_vote_unlikely)
#   )
# 


# Combine vote-related variables into `party_choice`
cecc <- cecc %>%
  mutate(
    post_party_choice = coalesce(pes21_votechoice2021)
  )

# # filter out rows where post_party_choice and per_party_choice have different values
# cecc1 <- cecc %>%
#   filter(post_party_choice == per_party_choice)

# # Store the original number of rows
# original_row_count <- nrow(cecc)





##############Economy scale##############
cecc2 <- cecc2 %>%
  filter(cps21_own_fin_retro != 4 & cps21_ownfinanc_fed != 4 & cps21_econ_retro != 4,5 & cps21_econ_fed_bette != 4)

#To switch the values 2 and 3 along with their labels in cps21_econ_fed_bette
cecc2 <- cecc2 %>%
  mutate(cps21_econ_fed_bette = case_when(
    cps21_econ_fed_bette == 2 ~ 3,  # Change 2 → 3
    cps21_econ_fed_bette == 3 ~ 2,  # Change 3 → 2
    TRUE ~ cps21_econ_fed_bette  # Keep other values unchanged
  ))
#reassign labels for values 2 and 3
cecc2$cps21_econ_fed_bette <- labelled(cecc2$cps21_econ_fed_bette, 
                                       c("Better" = 1, 
                                         "Not made much difference" = 2,  # Previously 3, now 2
                                         "Worse" = 3,                     # Previously 2, now 3
                                         "Don't know/ Prefer not to answer" = 4)
)

# Swap values for cps21_ownfinanc_fed
cecc2 <- cecc2 %>%
  mutate(cps21_ownfinanc_fed = case_when(
    cps21_ownfinanc_fed == 2 ~ 3,  # Change 2 → 3
    cps21_ownfinanc_fed == 3 ~ 2,  # Change 3 → 2
    TRUE ~ cps21_ownfinanc_fed  # Keep other values unchanged
  ))

# Update labels accordingly
cecc2$cps21_ownfinanc_fed <- labelled(cecc2$cps21_ownfinanc_fed, 
                                      c("Better" = 1, 
                                        "Not made much difference" = 2,  # Previously 3, now 2
                                        "Worse" = 3,                     # Previously 2, now 3
                                        "Don't know/ Prefer not to answer" = 4)
)

#store the aggregate value as economy_scale (4=better~12=worse)
cecc2 <- cecc2 %>%
  mutate(economy_scale = cps21_own_fin_retro + cps21_ownfinanc_fed + cps21_econ_retro + cps21_econ_fed_bette)

#Rescale economy_scale from 4-12 to 1-9 (1=better~9=worse)
cecc2 <- cecc2 %>%
  mutate(economy_scale = economy_scale - 3)

library(ggplot2)  # Load ggplot2 for plotting

ggplot(cecc2, aes(x = economy_scale)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Economy Scale",
       x = "Economy Scale",
       y = "Count") +
  theme_minimal()

##############Cynicism scale##############

cecc2 <- cecc2 %>%
  filter(pes21_losetouch != 6 & pes21_govtcare != 6 & !(pes21_keepromises %in% c(5, 6)))

#store the aggregate value as Cynicism_scale (3=not cynical~14=very cynical)
cecc2 <- cecc2 %>%
  mutate(cynicism_scale = pes21_losetouch + pes21_govtcare + pes21_keepromises)

#Rescale Cynicism_scale from 3-14 to 1-12 (1=not cynical~12=very cynical)
cecc2 <- cecc2 %>%
  mutate(cynicism_scale = cynicism_scale - 2)

ggplot(cecc2, aes(x = cynicism_scale)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Cynicism Scale",
       x = "Cynicism Scale",
       y = "Count") +
  theme_minimal()

##############per_are_you_vote###########
cecc <- cecc %>%
  filter(cps21_v_likely != 5) %>%  # Remove respondents who are not eligible to vote
  mutate(per_are_you_vote = case_when(
    cps21_v_likely %in% c(1, 2, 6) ~ 1,  # Voted or likely to vote → 1
    cps21_v_likely %in% c(3, 4, 7) ~ 0   # Unlikely to vote → 0
  ))

##############transgender_dummy 
# Create the transgender dummy variable and remove genderid == 4
cecc <- cecc %>%
  mutate(transgender = if_else(cps21_trans == 1, 1, 0)) %>%  # 1 if transgender, 0 otherwise
  filter(cps21_genderid != 4) %>%  # Remove rows where gender identity is 4 (another gender) 
  filter(cps21_trans!= 3)



#remove the unecrssary col
cecc <- cecc %>%
  select(-c(cps21_StartDate, 
            cps21_EndDate,
            Duration__in_seconds_, 
            DistributionChannel,  
            cps21_citizenship, 
            cps21_yob ))
















##############cps21_bornin_canada########
cecc <- cecc %>% filter(cps21_bornin_canada != 3)
########## Cleaning Employment Status (cps21_employment) ##########
cecc <- cecc %>%
  mutate(
    employment_status = case_when(
      cps21_employment == 1 ~ "Full-time",
      cps21_employment == 2 ~ "Part-time",
      cps21_employment == 3 ~ "Self-employed",
      cps21_employment == 4 ~ "Retired",
      cps21_employment == 5 ~ "Unemployed",
      cps21_employment == 6 ~ "Student",
      cps21_employment == 7 ~ "Caring for family",
      cps21_employment == 8 ~ "Disabled",
      cps21_employment == 9 ~ "Student and working",
      cps21_employment == 10 ~ "Caring and working",
      cps21_employment == 11 ~ "Retired and working",
      cps21_employment == 12 ~ "Other",
      cps21_employment == 13 ~ NA_character_
    )
  )

########## Cleaning Education Level (cps21_education) ##########
cecc <- cecc %>%
  mutate(
    education_level = case_when(
      cps21_education == 1 ~ "No schooling",
      cps21_education == 2 ~ "Some elementary",
      cps21_education == 3 ~ "Completed elementary",
      cps21_education == 4 ~ "Some high school",
      cps21_education == 5 ~ "Completed high school",
      cps21_education == 6 ~ "Some college",
      cps21_education == 7 ~ "Completed college",
      cps21_education == 8 ~ "Some university",
      cps21_education == 9 ~ "Bachelor's degree",
      cps21_education == 10 ~ "Master's degree",
      cps21_education == 11 ~ "Doctorate",
      cps21_education == 12 ~ NA_character_
    )
  )

########## Cleaning Province (cps21_province) ##########
cecc <- cecc %>%
  mutate(
    province = case_when(
      cps21_province == 1 ~ "Alberta",
      cps21_province == 2 ~ "British Columbia",
      cps21_province == 3 ~ "Manitoba",
      cps21_province == 4 ~ "New Brunswick",
      cps21_province == 5 ~ "Newfoundland and Labrador",
      cps21_province == 6 ~ "Northwest Territories",
      cps21_province == 7 ~ "Nova Scotia",
      cps21_province == 8 ~ "Nunavut",
      cps21_province == 9 ~ "Ontario",
      cps21_province == 10 ~ "Prince Edward Island",
      cps21_province == 11 ~ "Quebec",
      cps21_province == 12 ~ "Saskatchewan",
      cps21_province == 13 ~ "Yukon"
    )
  )

########## Cleaning Democracy Satisfaction (cps21_demsat) ##########
cecc <- cecc %>%
  mutate(
    democracy_satisfaction = case_when(
      cps21_demsat == 1 ~ "Very satisfied",
      cps21_demsat == 2 ~ "Fairly satisfied",
      cps21_demsat == 3 ~ "Not very satisfied",
      cps21_demsat == 4 ~ "Not at all satisfied",
      cps21_demsat == 5 ~ NA_character_
    )
  )

########## Cleaning Political Interest (cps21_interest_gen_1 & cps21_interest_elxn_1) ##########
cecc <- cecc %>%
  mutate(
    political_interest = as.numeric(cps21_interest_gen_1),
    election_interest = as.numeric(cps21_interest_elxn_1)
  )

########## Cleaning Sexuality (cps21_sexuality) ##########
cecc <- cecc %>%
  mutate(
    sexuality = case_when(
      cps21_sexuality == 1 ~ "Straight",
      cps21_sexuality == 2 ~ "Gay/Lesbian",
      cps21_sexuality == 3 ~ "Bisexual",
      cps21_sexuality == 4 ~ "Queer",
      cps21_sexuality == 5 ~ "Other",
      cps21_sexuality == 6 ~ "Not sure",
      cps21_sexuality == 7 ~ "Don't understand",
      cps21_sexuality == 8 ~ NA_character_
    )
  )

########## Cleaning Economic Evaluations ##########
cecc <- cecc %>%
  mutate(
    economy_past_year = case_when(
      cps21_econ_retro == 1 ~ "Got better",
      cps21_econ_retro == 2 ~ "Stayed the same",
      cps21_econ_retro == 3 ~ "Got worse",
      cps21_econ_retro == 4 ~ NA_character_,
      cps21_econ_retro == 5 ~ NA_character_
    ),
    economy_fed_policy = case_when(
      cps21_econ_fed_bette == 1 ~ "Improved",
      cps21_econ_fed_bette == 2 ~ "Worsened",
      cps21_econ_fed_bette == 3 ~ "No effect",
      cps21_econ_fed_bette == 4 ~ NA_character_
    )
  )

########## Cleaning Party Perception (cps21_most_seats) ##########
cecc <- cecc %>%
  rename(
    most_seats_lib = cps21_most_seats_1,
    most_seats_con = cps21_most_seats_2,
    most_seats_ndp = cps21_most_seats_3,
    most_seats_bloc = cps21_most_seats_4,
    most_seats_green = cps21_most_seats_5
  )

########## Cleaning Preferred Candidate in Riding (cps21__candidateref) ##########
cecc <- cecc %>%
  mutate(
    preferred_candidate = case_when(
      cps21__candidateref == 1 ~ "Liberal",
      cps21__candidateref == 2 ~ "Conservative",
      cps21__candidateref == 3 ~ "NDP",
      cps21__candidateref == 4 ~ "Bloc",
      cps21__candidateref == 5 ~ "Green",
      cps21__candidateref == 6 ~ "Other",
      cps21__candidateref == 7 ~ NA_character_
    )
  )
#conservative vs liveral 
#liberal vs dpn
#rest of shit

########## Cleaning Preferred Election Outcome (cps21_outcome_most & cps21_outcome_least) ##########
cecc <- cecc %>%
  mutate(
    outcome_most = case_when(
      cps21_outcome_most == 1 ~ "Liberal Majority",
      cps21_outcome_most == 2 ~ "Conservative Majority",
      cps21_outcome_most == 3 ~ "NDP Majority",
      cps21_outcome_most == 4 ~ "Liberal Minority",
      cps21_outcome_most == 5 ~ "Conservative Minority",
      cps21_outcome_most == 6 ~ "NDP Minority",
      cps21_outcome_most == 7 ~ "Other",
      cps21_outcome_most == 8 ~ NA_character_
    ),
    outcome_least = case_when(
      cps21_outcome_least == 1 ~ "Liberal Majority",
      cps21_outcome_least == 2 ~ "Conservative Majority",
      cps21_outcome_least == 3 ~ "NDP Majority",
      cps21_outcome_least == 4 ~ "Liberal Minority",
      cps21_outcome_least == 5 ~ "Conservative Minority",
      cps21_outcome_least == 6 ~ "NDP Minority",
      cps21_outcome_least == 7 ~ "Other",
      cps21_outcome_least == 8 ~ NA_character_
    )
  )

# ########## Cleaning Views on Minority Government (cps21_minority_gov) ##########
# cecc <- cecc %>%
#   mutate(
#     minority_gov = case_when(
#       cps21_minority_gov == 1 ~ "Good",
#       cps21_minority_gov == 2 ~ "Bad",
#       cps21_minority_gov == 3 ~ "Not Sure",
#       cps21_minority_gov == 4 ~ NA_character_
#     )
#   )


#cps21_most_seats For each of the parties below, how likely is each party to win the most seats in the House of Commons?
#cps21_win_local For each of the parties below, how likely is each party to win the seat in your own local riding?
#cps21__candidateref Which candidate do you want to win the seat in your riding?


########## Cleaning Visible Minority Identification (cps21_vismin) ##########
########## race ##########
#white, black, asian, idigenous and others.
cecc <- cecc %>%
  mutate(
    vismin_arab = ifelse(cps21_vismin_1 == 1, 1, 0),
    vismin_asian = ifelse(cps21_vismin_2 == 1, 1, 0),#
    vismin_black = ifelse(cps21_vismin_3 == 1, 1, 0),
    vismin_indigenous = ifelse(cps21_vismin_4 == 1, 1, 0),
    vismin_latino = ifelse(cps21_vismin_5 == 1, 1, 0),
    vismin_south_asian = ifelse(cps21_vismin_6 == 1, 1, 0),# 
    vismin_southeast_asian = ifelse(cps21_vismin_7 == 1, 1, 0),#
    vismin_west_asian = ifelse(cps21_vismin_8 == 1, 1, 0 #
    vismin_white = ifelse(cps21_vismin_9 == 1, 1, 0),
    vismin_other = ifelse(cps21_vismin_10 == 1, 1, 0),
    vismin_none = ifelse(cps21_vismin_11 == 1, 1, 0),
    vismin_prefer_not_answer = ifelse(cps21_vismin_12 == 1, 1, 0)
  )
########## Cleaning Religion Identification (cps21_religion) ##########
#lets lasso delete it 
cecc <- cecc %>%
  mutate(
    religion_none = ifelse(cps21_religion == 1, 1, 0),
    religion_agnostic = ifelse(cps21_religion == 2, 1, 0),
    religion_buddhist = ifelse(cps21_religion == 3, 1, 0),
    religion_hindu = ifelse(cps21_religion == 4, 1, 0), 
    religion_jewish = ifelse(cps21_religion == 5, 1, 0),
    religion_muslim = ifelse(cps21_religion == 6, 1, 0),
    religion_sikh = ifelse(cps21_religion == 7, 1, 0),
    religion_anglican = ifelse(cps21_religion == 8, 1, 0),
    religion_baptist = ifelse(cps21_religion == 9, 1, 0),
    religion_catholic = ifelse(cps21_religion == 10, 1, 0),
    religion_orthodox = ifelse(cps21_religion == 11, 1, 0),
    religion_jehovah_witness = ifelse(cps21_religion == 12, 1, 0),
    religion_lutheran = ifelse(cps21_religion == 13, 1, 0),
    religion_mormon = ifelse(cps21_religion == 14, 1, 0),
    religion_pentecostal = ifelse(cps21_religion == 15, 1, 0),
    religion_presbyterian = ifelse(cps21_religion == 16, 1, 0),
    religion_protestant = ifelse(cps21_religion == 17, 1, 0),
    religion_united_church = ifelse(cps21_religion == 18, 1, 0),
    religion_christian_reformed = ifelse(cps21_religion == 19, 1, 0),
    religion_salvation_army = ifelse(cps21_religion == 20, 1, 0),
    religion_mennonite = ifelse(cps21_religion == 21, 1, 0),
    religion_other = ifelse(cps21_religion == 22, 1, 0),
    religion_prefer_not_answer = ifelse(cps21_religion == 23, 1, 0)
  )




########## Cleaning Preferred Election Outcome (cps21_outcome_most) ##########
cecc <- cecc %>%
  mutate(
    outcome_most_liberal_majority = ifelse(cps21_outcome_most == 1, 1, 0),
    outcome_most_conservative_majority = ifelse(cps21_outcome_most == 2, 1, 0),
    outcome_most_ndp_majority = ifelse(cps21_outcome_most == 3, 1, 0),
    outcome_most_liberal_minority = ifelse(cps21_outcome_most == 4, 1, 0),
    outcome_most_conservative_minority = ifelse(cps21_outcome_most == 5, 1, 0),
    outcome_most_ndp_minority = ifelse(cps21_outcome_most == 6, 1, 0),
    outcome_most_other = ifelse(cps21_outcome_most == 7, 1, 0),
    outcome_most_prefer_not_answer = ifelse(cps21_outcome_most == 8, 1, 0)
  )



########## Cleaning Past Financial Situation (cps21_own_fin_retro) ##########
cecc <- cecc %>%
  mutate(
    own_fin_retro_better = ifelse(cps21_own_fin_retro == 1, 1, 0),
    own_fin_retro_same = ifelse(cps21_own_fin_retro == 2, 1, 0),
    own_fin_retro_worse = ifelse(cps21_own_fin_retro == 3, 1, 0),
    own_fin_retro_dk_pna = ifelse(cps21_own_fin_retro == 4, 1, 0) # Don't know/Prefer not to answer
  )

########## Cleaning Impact of Federal Policies on Personal Finances (cps21_ownfinanc_fed) ##########
cecc <- cecc %>%
  mutate(
    own_fin_fed_better = ifelse(cps21_ownfinanc_fed == 1, 1, 0),
    own_fin_fed_worse = ifelse(cps21_ownfinanc_fed == 2, 1, 0),
    own_fin_fed_no_change = ifelse(cps21_ownfinanc_fed == 3, 1, 0),
    own_fin_fed_dk_pna = ifelse(cps21_ownfinanc_fed == 4, 1, 0) # Don't know/Prefer not to answer
  )

########## Cleaning Expected Future Financial Situation (cps21_own_fin_future) ##########
cecc <- cecc %>%
  mutate(
    own_fin_future_better = ifelse(cps21_own_fin_future == 1, 1, 0),
    own_fin_future_same = ifelse(cps21_own_fin_future == 2, 1, 0),
    own_fin_future_worse = ifelse(cps21_own_fin_future == 3, 1, 0),
    own_fin_future_dk_pna = ifelse(cps21_own_fin_future == 4, 1, 0) # Don't know/Prefer not to answer
  )
########## Cleaning Voting Likelihood (cps21_v_likely) ##########
cecc <- cecc %>%
  mutate(
    v_likely_certain = ifelse(cps21_v_likely == 1, 1, 0),  # Certain to vote
    v_likely_likely = ifelse(cps21_v_likely == 2, 1, 0),   # Likely to vote
    v_likely_unlikely = ifelse(cps21_v_likely == 3, 1, 0), # Unlikely to vote
    v_likely_not_vote = ifelse(cps21_v_likely == 4, 1, 0), # Certain not to vote
    v_likely_not_eligible = ifelse(cps21_v_likely == 5, 1, 0), # Not eligible to vote
    v_likely_already_voted = ifelse(cps21_v_likely == 6, 1, 0), # Already voted
    v_likely_dk_pna = ifelse(cps21_v_likely == 7, 1, 0)  # Don't know/Prefer not to answer
  )







#variable list####################################################################################
# Step 14: Keep Only Relevant Variables
final_variables <- c(
  
  # Custom Variables Created for Analysis
  "UserLanguage_dummy",  # Dummy variable: 1 if UserLanguage == "EN", 0 otherwise
  "are_you_vote",        # Dummy variable: per-election 1 if respondent likely voted, 0 otherwise
  "party_choice",        # Combined variable for per-election vote choice (advance vote, preference, lean)
  "age",                 # Computed from cps21_yob (Year of Birth) as 2021 - birth year
  "transgender",         # Dummy variable: 1 if respondent is transgender, 0 otherwise
  
  # Survey Weights & Data Quality
  "cps21_data_quality",  # Data quality indicator for pre-election responses
  "pes21_data_quality",  # Data quality indicator for post-election responses
  
  # Demographics
  "cps21_genderid",  # Gender identity of the respondent (after filtering out "Other" category)
  "cps21_province",  # Province of residence (used for regional analysis)
  "cps21_education", # Education level of respondent (important for political analysis)
  
  # Pre-Election Interest & Political Views
  "cps21_demsat",         # Satisfaction with democracy before the election
  "cps21_interest_gen_1", # General interest in politics
  "cps21_interest_elxn_1",# Interest in the 2021 federal election
  
  # Voting Likelihood & Behavior
  "cps21_v_likely",  # Likelihood of voting in the election (before filtering ineligible voters)
  "cps21_comfort1",  # Comfort level with in-person voting during the pandemic
  "cps21_comfort2",  # Comfort level with COVID-19 restrictions while voting
  "cps21_comfort3",  # Comfort level with election procedures in general
  
  # Political Ideology & Spending Preferences
  "cps21_spend_env",       # Support for government spending on environmental policies
  "cps21_spend_imm_min",   # Support for government spending on immigrants and minorities
  "cps21_spend_rec_indi",  # Support for government spending on Indigenous reconciliation
  "cps21_spend_afford_h",  # Support for government spending on affordable housing
  "cps21_groups_therm_1",  # Feelings toward racial minorities (thermometer scale)
  "cps21_groups_therm_2",  # Feelings toward immigrants (thermometer scale)
  "cps21_groups_therm_7",  # Feelings toward Americans (thermometer scale)
  "cps21_groups_therm_3",  # Feelings toward Francophones (thermometer scale)
  "cps21_groups_therm_4",  # Feelings toward Indigenous peoples (thermometer scale)
  "cps21_groups_therm_6",  # Feelings toward feminists (thermometer scale)
  
  # COVID-19 & Elections
  "cps21_covid_liberty", # Whether COVID-19 restrictions were seen as a threat to personal liberty
  "cps21_most_seats_1",  # Perceived likelihood that the Liberal Party would win most seats
  "cps21_most_seats_2",  # Perceived likelihood that the Conservative Party would win most seats
  "cps21_most_seats_3",  # Perceived likelihood that the NDP would win most seats
  "cps21_most_seats_4",  # Perceived likelihood that the Bloc Québécois would win most seats
  "cps21_most_seats_5",  # Perceived likelihood that the Green Party would win most seats
  "cps21_outcome_most",  # Respondent's preferred election outcome
  
  # Vaccine Attitudes
  "cps21_vaccine_mandat_1", # Support for mandatory vaccination for air/rail travel
  "cps21_vaccine_mandat_2", # Support for mandatory vaccination for bars/restaurants
  "cps21_vaccine_mandat_3", # Support for mandatory vaccination for hospital workers
  "cps21_vaccine1",         # Respondent's own vaccination status
  
  # Post-Election Trust Variables
  "pes21_dem_sat",  # Satisfaction with democracy after the election
  "pes21_losetouch",# Perception of whether elected officials lose touch with the public
  "pes21_govtcare", # Perception of whether the government cares about people like the respondent
  
  # Political Extremity & Populism
  "pes21_hatespeech",   # Should hateful speech be illegal?
  "pes21_envirojob",    # Trade-off between jobs and environmental protection
  "pes21_discfam",      # Frequency of discussing politics with family/friends
  "pes21_famvalues",    # Support for traditional family values
  "pes21_equalrights",  # Perception of whether equal rights have "gone too far"
  "pes21_fitin",        # Perception of whether recent immigrants integrate into Canadian society
  "pes21_immigjobs",    # Perception of whether immigrants take jobs from Canadians
  "c"  # Support for government assistance programs
)






# Ensure factors are correctly formatted
cecc <- cecc %>%
  mutate(
    # Treatment: If the respondent's vote choice matches the winning party
    treatment = if_else(pes21_votechoice2021 == cps21_votechoice, 1, 0),
    
    # Expected Winner variable
    expected_winner = case_when(
      cps21_votechoice == 1 & cps21_most_seats_1 >= 50 ~ "Liberal Expected",
      cps21_votechoice == 2 & cps21_most_seats_2 >= 50 ~ "Conservative Expected",
      cps21_votechoice == 3 & cps21_most_seats_3 >= 50 ~ "NDP Expected",
      cps21_votechoice == 4 & cps21_most_seats_4 >= 50 ~ "Bloc Expected",
      cps21_votechoice == 5 & cps21_most_seats_5 >= 50 ~ "Green Expected",
      TRUE ~ "Did Not Expect Win"
    )











