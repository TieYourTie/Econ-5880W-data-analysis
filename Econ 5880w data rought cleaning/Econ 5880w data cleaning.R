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

canadian_election$cps21_covidrelief

################################################################################################
#1. we need to remove the low quality row
################################################################################################

#filter out those people 
canadian_election_quality <- canadian_election %>%
  filter(canadian_election$cps21_data_quality == 0)

################################################################################################


cps21_education
cps21_demsat
cps21_imp_iss_party
cps21_imp_loc_iss_p

#remove the people who cannot vote
cps21_v_likely

#are you most likely to vote?
cps21_howvote1


#Regardless of how you voted, how comfortable were you with the idea of voting in person during the coronavirus (COVID-19) pandemic?
cps21_comfort3

#Which party do you think you will vote for?
cps21_votechoice
cps21_v_advance
cps21_vote_lean
cps21_vote_lean_pr


cps21_2nd_choice
cps21_2nd_choice_pr
cps21_not_vote_for

#alternative option
#federal party
cps21_party_rating

#federal leader 
cps21_lead_rating


#How satisfied are you with the performance of the federal government under Justin Trudeau?
cps21_fed_gov_sat


#How much should the federal government spend on education?
#cps21_spend_educ


#cps21_spend_env How much should the federal government spend on the environment?

#cps21_spend_just_law How much should the federal government spend on

#cps21_spend_defence How much should the federal government spend on defence?

#cps21_spend_imm_min How much should the federal government spend on immigrants and minorities?

#cps21_spend_rec_indi How much should the federal government spend on reconciliation with Indigenous Peoples?

#cps21_spend_afford_h How much should the federal government spend on affordable housing?

#cps21_covid_liberty The public health recommendations aimed at slowing the spread of the COVID-19 virus are threatening my liberty.


####
#cps21_most_seats For each of the parties below, how likely is each party to win the most seats in the House of Commons?
#####


####
#cps21__candidateref Which candidate do you want to win the seat in your riding?
#####


#####
#cps21_candidate_imag Imagine you were the only voter in the election. Which candidate would you want to win in your riding?
####

###
#cps21_outcome_most Which election outcome would you most prefer?
###

###
#pes21_dem_sat
###









