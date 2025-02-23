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
#Step_one: keep only the high quality dataset 
################################################################################################
#filter out those people 
canadian_election_quality <- canadian_election %>%
  filter(cps21_data_quality == 0 & pes21_data_quality == 0)

################################################################################################
################################################################################################
#2. Personal_information 
################################################################################################
#2.1 keep only canadian citizen 
################################################################################################
ce_p <- canadian_election_quality  %>%
  filter(cps21_citizenship == 1)
################################################################################################
#2.2 gender
################################################################################################
#remove weird answer
ce_p <- ce_p %>% 
  filter(cps21_genderid != 4)
#remove cps21_genderid_4_TEXT
ce_p <- ce_p %>%
  select(-cps21_genderid_4_TEXT)

#2.2.1 transgender
ce_p <- ce_p %>%
  filter(cps21_trans != 3)
################################################################################################
#2.3 province
################################################################################################
print(unique(ce_p$cps21_province))

################################################################################################
#2.4 cps21_education What is the highest level of education that you have completed?
################################################################################################
ce_p <- ce_p %>% 
  filter(cps21_education != 12)

################################################################################################
#3. Pre-Election Interest & Political Views
################################################################################################
#3.1   "cps21_demsat" - Satisfaction with democracy
ce_pp <- ce_p %>% 
  filter(cps21_demsat != 5)

#3.2  "cps21_interest_gen_1",  # Interest in politics generally
ce_pp <- ce_pp %>% 
  filter(cps21_interest_gen_1 != -99)

#3.3 cps21_interest_elxn_1 How interested are you in this federal election? Set the slider to a number from 0 to 10, where 0 means no interest at all, and 10 means a great deal of interest.
ce_pp <- ce_pp %>% 
  filter(cps21_interest_elxn_1 != -99)


################################################################################################
#4. Voting Likelihood & Behavior
################################################################################################
#4.1 "cps21_v_likely",   # Likelihood of voting (filter out 4 and 5)
ce_pp <- ce_pp %>% 
 filter(cps21_v_likely != 7)

#4.2 "cps21_comfort1",   # Comfort level voting in person during the pandemic
#ce_pp <- ce_pp %>% 
  #filter(cps21_comfort1 != 5)

#4.3 "cps21_comfort2",   # Comfort level with COVID-19 restrictions during voting
#ce_pp <- ce_pp %>% 
 # filter(cps21_comfort2 != 5)

#4.4 "cps21_comfort3",   # Comfort level with election procedures
#ce_pp <- ce_pp %>% 
 # filter(cps21_comfort3 != 5)


################################################################################################
#5. Voting Choice & Party Preference
################################################################################################
#"cps21_votechoice",      # Which party do you plan to vote for?
#"cps21_vote_unlike_pr",  # If you could vote, which party would you choose?
#  "cps21_v_advance",       # For which party did you vote?
# "cps21_vote_lean",       # Party leaning preference
#  "cps21_2nd_choice",      # Second-choice party

  
  # Political Ideology & Spending Preferences
  #"cps21_spend_env",       # Government spending on the environment
  #"cps21_spend_imm_min",   # Government spending on immigrants/minorities
  # "cps21_spend_rec_indi",  # Government spending on Indigenous reconciliation
  # "cps21_spend_afford_h",  # Government spending on affordable housing
  # "cps21_groups_therm",    # Feelings toward different social groups
      #(cps21_groups_therm_1) Racial minorities
      #(cps21_groups_therm_2) mmigrants
      #(cps21_groups_therm_7) Americans
      #(cps21_groups_therm_3) Francophones
      #(cps21_groups_therm_4) Indigenous peoples
      #(cps21_groups_therm_6) Feminists



  #"cps21_covid_liberty",   # COVID-19 restrictions as a threat to liberty
  
  # Election Outcome Expectations
  #"cps21_most_seats",   # Likelihood of each party winning most seats
      #(cps21_most_seats_1)  Liberal Party
      #(cps21_most_seats_2)  Conservative Party
      #(cps21_most_seats_3)  NDP
      #(cps21_most_seats_4)  Bloc Québécois
      #(Green Party)    (cps21_most_seats_5)

  #cps21_outcome_most Which election outcome would you most prefer? 
    #remove 7 

#candidateref




  # Vaccine Attitudes (Proxy for Political Polarization)
    #cps21_vaccine_mandat Should vaccination be required to:
        #(cps21_vaccine_mandat_1)  Travel by air or rail in Canada
        #(cps21_vaccine_mandat_2) Go to a bar or restaurant
        #(cps21_vaccine_mandat_3) Work in a hospital


 #cps21_vaccine1 Have you been vaccinated?








  
  # Post-Election Trust Variables
  #"pes21_dem_sat",     # Post-election satisfaction with democracy
  #"pes21_losetouch",   # Do elected officials lose touch with people?
  #"pes21_govtcare",    # Does the government care about people like me?
  
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







