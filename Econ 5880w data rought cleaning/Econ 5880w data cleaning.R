rm(list = ls())
#make sure everthing will be fine.)

# List of required packages
required_packages <- c(
  "lubridate", "cansim", "OECD", "WDI", "fredr", 
  "mFilter", "neverhpfilter", "tsbox", "RColorBrewer", 
  "plotly", "wesanderson", "writexl", "tidyverse", 
  "readr", "haven"
)

# Install missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if(length(missing_packages)) install.packages(missing_packages)

# Load the packages
lapply(required_packages, library, character.only = TRUE)

fredr_set_key("YOUR_FRED_API_KEY")
installed.packages()[, "Package"]
install.packages("dplyr")


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
library(dplyr)





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
####################################################################################
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

############################ Pie Chart for Province ###################################################################

install.packages("ggrepel")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggrepel)  # For better text placement

# Count the occurrences of each province
province_counts <- data.frame(
  Province = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", 
               "Newfoundland and Labrador", "Northwest Territories", "Nova Scotia", 
               "Nunavut", "Ontario", "Prince Edward Island", "Quebec", 
               "Saskatchewan", "Yukon"),
  Count = c(
    sum(canadian_election$cps21_province == 1, na.rm = TRUE),
    sum(canadian_election$cps21_province == 2, na.rm = TRUE),
    sum(canadian_election$cps21_province == 3, na.rm = TRUE),
    sum(canadian_election$cps21_province == 4, na.rm = TRUE),
    sum(canadian_election$cps21_province == 5, na.rm = TRUE),
    sum(canadian_election$cps21_province == 6, na.rm = TRUE),
    sum(canadian_election$cps21_province == 7, na.rm = TRUE),
    sum(canadian_election$cps21_province == 8, na.rm = TRUE),
    sum(canadian_election$cps21_province == 9, na.rm = TRUE),
    sum(canadian_election$cps21_province == 10, na.rm = TRUE),
    sum(canadian_election$cps21_province == 11, na.rm = TRUE),
    sum(canadian_election$cps21_province == 12, na.rm = TRUE),
    sum(canadian_election$cps21_province == 13, na.rm = TRUE)
  )
)

# Get total number of rows
num_rows <- nrow(canadian_election)

# Calculate percentage
province_counts$Percentage <- (province_counts$Count / num_rows) * 100

# Add percentage labels formatted to 1 decimal place
province_counts$Label <- paste0(round(province_counts$Percentage, 1), "%")

ggplot(province_counts, aes(x = "", y = Percentage, fill = Province)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") +
  geom_text(aes(label = Label), 
            position = position_stack(vjust = 0.3), # Adjust label position
            size = 4, 
            color = "black") +  # Make text more readable
  labs(title = "Province Distribution in Canadian Election Data", fill = "Province") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())

############################ Pie Chart for Gender ######################################

# Count the occurrences of each gender
gender_counts <- data.frame(
  Gender = c("Man", "Woman", "Non-binary", "Another gender"),
  Count = c(
    sum(canadian_election$cps21_genderid == 1, na.rm = TRUE),
    sum(canadian_election$cps21_genderid == 2, na.rm = TRUE),
    sum(canadian_election$cps21_genderid == 3, na.rm = TRUE),
    sum(canadian_election$cps21_genderid == 4, na.rm = TRUE)
  )
)

# Calculate percentage
gender_counts$Percentage <- (gender_counts$Count / num_rows) * 100

# Add percentage labels formatted to 1 decimal place
gender_counts$Label <- paste0(round(gender_counts$Percentage, 1), "%")

ggplot(gender_counts, aes(x = "", y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") +
  geom_text(aes(label = Label), 
            position = position_stack(vjust = 0.3), # Adjust label position
            size = 4, 
            color = "black") +  # Make text more readable
  labs(title = "Gender Distribution in Canadian Election Data", fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())

############################ Pie Chart for vote_Choice ###############################################

# Count the occurrences of each party people voted for (excluding NA)
party_counts <- data.frame(
  Party = c("Liberal", "Conservative", "NDP", "Bloc Québécois", "Green", "Another party", "Prefer not to answer"),
  Count = c(
    sum(ce_p$voting_choise == 1, na.rm = TRUE),
    sum(ce_p$voting_choise == 2, na.rm = TRUE),
    sum(ce_p$voting_choise == 3, na.rm = TRUE),
    sum(ce_p$voting_choise == 4, na.rm = TRUE),
    sum(ce_p$voting_choise == 5, na.rm = TRUE),
    sum(ce_p$voting_choise == 6, na.rm = TRUE),
    sum(ce_p$voting_choise == 7, na.rm = TRUE)
  )
)

# Remove parties with zero votes to avoid them appearing in the chart
party_counts <- party_counts %>%
  filter(Count > 0)

# Get total valid votes (excluding NA)
num_valid_votes <- sum(party_counts$Count)

# Calculate percentage
party_counts$Percentage <- (party_counts$Count / num_valid_votes) * 100

# Add percentage labels formatted to 1 decimal place
party_counts$Label <- paste0(round(party_counts$Percentage, 1), "%")

# Create pie chart
ggplot(party_counts, aes(x = "", y = Percentage, fill = Party)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") +
  geom_text(aes(label = Label), 
            position = position_stack(vjust = 0.3), # Adjust label position
            size = 4, 
            color = "black") +  # Make text more readable
  labs(title = "Party Distribution in Canadian Election Data", fill = "Party") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())  # Remove x-axis text

############################ Bar Chart for Satisfaction (Libral)###########################################

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)  # For reshaping data

# Select the political party to analyze (change this value as needed)
selected_party <- "Liberal"

# Convert numerical satisfaction values into meaningful labels
ce_p <- ce_p %>%
  mutate(
    cps21_demsat_label = factor(cps21_demsat, 
                                levels = c(1, 2, 3, 4, 5), 
                                labels = c("Very satisfied", "Fairly satisfied", 
                                           "Not very satisfied", "Not satisfied at all", "NA")),
    
    pes21_dem_sat_label = factor(pes21_dem_sat, 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Very satisfied", "Fairly satisfied", 
                                            "Not very satisfied", "Not satisfied at all", "NA")),
    
    voting_choise_label = factor(voting_choise, 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Liberal", "Conservative", "NDP", 
                                            "Bloc Québécois", "Green"))
  )

# Filter only respondents who voted for the selected party
filtered_data <- ce_p %>%
  filter(voting_choise_label == selected_party) %>%
  select(cps21_demsat_label, pes21_dem_sat_label)  # Keep only relevant variables

# Reshape data: Merge pre-election & post-election satisfaction into a single column
long_data <- filtered_data %>%
  pivot_longer(cols = c(cps21_demsat_label, pes21_dem_sat_label), 
               names_to = "Satisfaction_Type", 
               values_to = "Satisfaction_Level")

# Rename "Satisfaction_Type" to indicate Pre-election and Post-election
long_data <- long_data %>%
  mutate(Satisfaction_Type = recode(Satisfaction_Type, 
                                    cps21_demsat_label = "Pre-election",
                                    pes21_dem_sat_label = "Post-election"))

# Create a bar chart comparing pre-election vs. post-election satisfaction
ggplot(long_data, aes(x = Satisfaction_Level, fill = Satisfaction_Type)) +
  geom_bar(position = position_dodge2(reverse = TRUE)) +
  labs(title = paste("Pre-election vs. Post-election Satisfaction for", selected_party),
       x = "Satisfaction Level", 
       y = "Count",
       fill = "Election Phase") +
  theme_minimal()

############################ Bar Chart for Satisfaction (Conservative)###########################################

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)  # For reshaping data

# Select the political party to analyze (change this value as needed)
selected_party <- "Conservative"

# Convert numerical satisfaction values into meaningful labels
ce_p <- ce_p %>%
  mutate(
    cps21_demsat_label = factor(cps21_demsat, 
                                levels = c(1, 2, 3, 4, 5), 
                                labels = c("Very satisfied", "Fairly satisfied", 
                                           "Not very satisfied", "Not satisfied at all", "NA")),
    
    pes21_dem_sat_label = factor(pes21_dem_sat, 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Very satisfied", "Fairly satisfied", 
                                            "Not very satisfied", "Not satisfied at all", "NA")),
    
    voting_choise_label = factor(voting_choise, 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Liberal", "Conservative", "NDP", 
                                            "Bloc Québécois", "Green"))
  )

# Filter only respondents who voted for the selected party
filtered_data <- ce_p %>%
  filter(voting_choise_label == selected_party) %>%
  select(cps21_demsat_label, pes21_dem_sat_label)  # Keep only relevant variables

# Reshape data: Merge pre-election & post-election satisfaction into a single column
long_data <- filtered_data %>%
  pivot_longer(cols = c(cps21_demsat_label, pes21_dem_sat_label), 
               names_to = "Satisfaction_Type", 
               values_to = "Satisfaction_Level")

# Rename "Satisfaction_Type" to indicate Pre-election and Post-election
long_data <- long_data %>%
  mutate(Satisfaction_Type = recode(Satisfaction_Type, 
                                    cps21_demsat_label = "Pre-election",
                                    pes21_dem_sat_label = "Post-election"))

# Create a bar chart comparing pre-election vs. post-election satisfaction
ggplot(long_data, aes(x = Satisfaction_Level, fill = Satisfaction_Type)) +
  geom_bar(position = position_dodge2(reverse = TRUE)) +
  labs(title = paste("Pre-election vs. Post-election Satisfaction for", selected_party),
       x = "Satisfaction Level", 
       y = "Count",
       fill = "Election Phase") +
  theme_minimal()

############################ Bar Chart for Satisfaction (NDP)###########################################

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)  # For reshaping data

# Select the political party to analyze (change this value as needed)
selected_party <- "NDP"

# Convert numerical satisfaction values into meaningful labels
ce_p <- ce_p %>%
  mutate(
    cps21_demsat_label = factor(cps21_demsat, 
                                levels = c(1, 2, 3, 4, 5), 
                                labels = c("Very satisfied", "Fairly satisfied", 
                                           "Not very satisfied", "Not satisfied at all", "NA")),
    
    pes21_dem_sat_label = factor(pes21_dem_sat, 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Very satisfied", "Fairly satisfied", 
                                            "Not very satisfied", "Not satisfied at all", "NA")),
    
    voting_choise_label = factor(voting_choise, 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Liberal", "Conservative", "NDP", 
                                            "Bloc Québécois", "Green"))
  )

# Filter only respondents who voted for the selected party
filtered_data <- ce_p %>%
  filter(voting_choise_label == selected_party) %>%
  select(cps21_demsat_label, pes21_dem_sat_label)  # Keep only relevant variables

# Reshape data: Merge pre-election & post-election satisfaction into a single column
long_data <- filtered_data %>%
  pivot_longer(cols = c(cps21_demsat_label, pes21_dem_sat_label), 
               names_to = "Satisfaction_Type", 
               values_to = "Satisfaction_Level")

# Rename "Satisfaction_Type" to indicate Pre-election and Post-election
long_data <- long_data %>%
  mutate(Satisfaction_Type = recode(Satisfaction_Type, 
                                    cps21_demsat_label = "Pre-election",
                                    pes21_dem_sat_label = "Post-election"))

# Create a bar chart comparing pre-election vs. post-election satisfaction
ggplot(long_data, aes(x = Satisfaction_Level, fill = Satisfaction_Type)) +
  geom_bar(position = position_dodge2(reverse = TRUE)) +
  labs(title = paste("Pre-election vs. Post-election Satisfaction for", selected_party),
       x = "Satisfaction Level", 
       y = "Count",
       fill = "Election Phase") +
  theme_minimal()

############################ Bar Chart for Satisfaction (Bloc Québécois)###########################################

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)  # For reshaping data

# Select the political party to analyze (change this value as needed)
selected_party <- "Bloc Québécois"

# Convert numerical satisfaction values into meaningful labels
ce_p <- ce_p %>%
  mutate(
    cps21_demsat_label = factor(cps21_demsat, 
                                levels = c(1, 2, 3, 4, 5), 
                                labels = c("Very satisfied", "Fairly satisfied", 
                                           "Not very satisfied", "Not satisfied at all", "NA")),
    
    pes21_dem_sat_label = factor(pes21_dem_sat, 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Very satisfied", "Fairly satisfied", 
                                            "Not very satisfied", "Not satisfied at all", "NA")),
    
    voting_choise_label = factor(voting_choise, 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Liberal", "Conservative", "NDP", 
                                            "Bloc Québécois", "Green"))
  )

# Filter only respondents who voted for the selected party
filtered_data <- ce_p %>%
  filter(voting_choise_label == selected_party) %>%
  select(cps21_demsat_label, pes21_dem_sat_label)  # Keep only relevant variables

# Reshape data: Merge pre-election & post-election satisfaction into a single column
long_data <- filtered_data %>%
  pivot_longer(cols = c(cps21_demsat_label, pes21_dem_sat_label), 
               names_to = "Satisfaction_Type", 
               values_to = "Satisfaction_Level")

# Rename "Satisfaction_Type" to indicate Pre-election and Post-election
long_data <- long_data %>%
  mutate(Satisfaction_Type = recode(Satisfaction_Type, 
                                    cps21_demsat_label = "Pre-election",
                                    pes21_dem_sat_label = "Post-election"))

# Create a bar chart comparing pre-election vs. post-election satisfaction
ggplot(long_data, aes(x = Satisfaction_Level, fill = Satisfaction_Type)) +
  geom_bar(position = position_dodge2(reverse = TRUE)) +
  labs(title = paste("Pre-election vs. Post-election Satisfaction for", selected_party),
       x = "Satisfaction Level", 
       y = "Count",
       fill = "Election Phase") +
  theme_minimal()

############################ Bar Chart for Satisfaction (Green)###########################################

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)  # For reshaping data

# Select the political party to analyze (change this value as needed)
selected_party <- "Green"

# Convert numerical satisfaction values into meaningful labels
ce_p <- ce_p %>%
  mutate(
    cps21_demsat_label = factor(cps21_demsat, 
                                levels = c(1, 2, 3, 4, 5), 
                                labels = c("Very satisfied", "Fairly satisfied", 
                                           "Not very satisfied", "Not satisfied at all", "NA")),
    
    pes21_dem_sat_label = factor(pes21_dem_sat, 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Very satisfied", "Fairly satisfied", 
                                            "Not very satisfied", "Not satisfied at all", "NA")),
    
    voting_choise_label = factor(voting_choise, 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Liberal", "Conservative", "NDP", 
                                            "Bloc Québécois", "Green"))
  )

# Filter only respondents who voted for the selected party
filtered_data <- ce_p %>%
  filter(voting_choise_label == selected_party) %>%
  select(cps21_demsat_label, pes21_dem_sat_label)  # Keep only relevant variables

# Reshape data: Merge pre-election & post-election satisfaction into a single column
long_data <- filtered_data %>%
  pivot_longer(cols = c(cps21_demsat_label, pes21_dem_sat_label), 
               names_to = "Satisfaction_Type", 
               values_to = "Satisfaction_Level")

# Rename "Satisfaction_Type" to indicate Pre-election and Post-election
long_data <- long_data %>%
  mutate(Satisfaction_Type = recode(Satisfaction_Type, 
                                    cps21_demsat_label = "Pre-election",
                                    pes21_dem_sat_label = "Post-election"))

# Create a bar chart comparing pre-election vs. post-election satisfaction
ggplot(long_data, aes(x = Satisfaction_Level, fill = Satisfaction_Type)) +
  geom_bar(position = position_dodge2(reverse = TRUE)) +
  labs(title = paste("Pre-election vs. Post-election Satisfaction for", selected_party),
       x = "Satisfaction Level", 
       y = "Count",
       fill = "Election Phase") +
  theme_minimal()


############################ New Theme_Pie Chart for Gender #########################


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggthemes)
library(haven)  # Required for haven-labelled data

# Convert cps21_genderid to numeric (handling labelled data)
canadian_election <- canadian_election %>%
  mutate(cps21_genderid = as.numeric(as.character(cps21_genderid)))  # Ensure numeric format

# Count the occurrences of each gender
gender_counts <- data.frame(
  Gender = c("Man", "Woman", "Non-binary", "Another gender"),
  Count = c(
    sum(canadian_election$cps21_genderid == 1, na.rm = TRUE),
    sum(canadian_election$cps21_genderid == 2, na.rm = TRUE),
    sum(canadian_election$cps21_genderid == 3, na.rm = TRUE),
    sum(canadian_election$cps21_genderid == 4, na.rm = TRUE)
  )
)

# Calculate percentage
gender_counts$Percentage <- (gender_counts$Count / sum(gender_counts$Count)) * 100

# Add percentage labels formatted to 1 decimal place
gender_counts$Label <- paste0(round(gender_counts$Percentage, 1), "%")

# Create pie chart with manually set Economist theme
ggplot(gender_counts, aes(x = "", y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") +
  geom_text(aes(label = Label), 
            position = position_stack(vjust = 0.5), # Center labels
            size = 5, 
            fontface = "bold", 
            color = "white") +  # Make text readable
  labs(title = "Gender Distribution in Canadian Election Data", fill = "Gender") +
  theme_minimal(base_size = 14) +  # Using minimal theme for clean look
  theme(
    panel.background = element_rect(fill = "white"),  # Ensure white background
    plot.background = element_rect(fill = "white"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right"
  ) +
  scale_fill_manual(values = c("#e3120b", "#00688D", "#FDBF6F", "#33A02C"))  # Economist color scheme

############################ New Theme_Pie Chart for vote_Choice ###############
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggthemes)
library(haven)  # Required if voting_choise is haven-labelled

# Convert voting_choise to numeric (if it's a labelled variable)
ce_p <- ce_p %>%
  mutate(voting_choise = as.numeric(as.character(voting_choise)))  # Ensure numeric format

# Count occurrences of each party people voted for (excluding NA)
party_counts <- data.frame(
  Party = c("Liberal", "Conservative", "NDP", "Bloc Québécois", "Green", "Another party", "Prefer not to answer"),
  Count = c(
    sum(ce_p$voting_choise == 1, na.rm = TRUE),
    sum(ce_p$voting_choise == 2, na.rm = TRUE),
    sum(ce_p$voting_choise == 3, na.rm = TRUE),
    sum(ce_p$voting_choise == 4, na.rm = TRUE),
    sum(ce_p$voting_choise == 5, na.rm = TRUE),
    sum(ce_p$voting_choise == 6, na.rm = TRUE),
    sum(ce_p$voting_choise == 7, na.rm = TRUE)
  )
)

# Remove parties with zero votes
party_counts <- party_counts %>%
  filter(Count > 0)

# Get total valid votes
num_valid_votes <- sum(party_counts$Count)

# Calculate percentage
party_counts$Percentage <- (party_counts$Count / num_valid_votes) * 100

# Add percentage labels formatted to 1 decimal place
party_counts$Label <- paste0(round(party_counts$Percentage, 1), "%")

# Create pie chart with manually set Economist theme
ggplot(party_counts, aes(x = "", y = Percentage, fill = Party)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") +
  geom_text(aes(label = Label), 
            position = position_stack(vjust = 0.5), # Center labels
            size = 5, 
            fontface = "bold", 
            color = "white") +  # Improve readability
  labs(title = "Party Distribution in Canadian Election Data", fill = "Party") +
  theme_minimal(base_size = 14) +  # Clean layout
  theme(
    panel.background = element_rect(fill = "white"),  # Set white background
    plot.background = element_rect(fill = "white"),
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks = element_blank(),
    legend.position = "right"  # Place legend on the right
  ) +
  scale_fill_manual(values = c("#e3120b", "#00688D", "#FDBF6F", "#33A02C", "#A6CEE3", "#FB9A99", "#B15928"))  # Economist-style colors

############################ New Theme_Pie Chart for Province ################
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggthemes)
library(haven)
library(ggrepel)  # Prevents label overlap and adds background color

# Convert cps21_province to numeric (if it's a labelled variable)
canadian_election <- canadian_election %>%
  mutate(cps21_province = as.numeric(as.character(cps21_province)))

# Count occurrences of each province
province_counts <- data.frame(
  Province = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", 
               "Newfoundland and Labrador", "Northwest Territories", "Nova Scotia", 
               "Nunavut", "Ontario", "Prince Edward Island", "Quebec", 
               "Saskatchewan", "Yukon"),
  Count = c(
    sum(canadian_election$cps21_province == 1, na.rm = TRUE),
    sum(canadian_election$cps21_province == 2, na.rm = TRUE),
    sum(canadian_election$cps21_province == 3, na.rm = TRUE),
    sum(canadian_election$cps21_province == 4, na.rm = TRUE),
    sum(canadian_election$cps21_province == 5, na.rm = TRUE),
    sum(canadian_election$cps21_province == 6, na.rm = TRUE),
    sum(canadian_election$cps21_province == 7, na.rm = TRUE),
    sum(canadian_election$cps21_province == 8, na.rm = TRUE),
    sum(canadian_election$cps21_province == 9, na.rm = TRUE),
    sum(canadian_election$cps21_province == 10, na.rm = TRUE),
    sum(canadian_election$cps21_province == 11, na.rm = TRUE),
    sum(canadian_election$cps21_province == 12, na.rm = TRUE),
    sum(canadian_election$cps21_province == 13, na.rm = TRUE)
  )
)

# Remove provinces with zero votes
province_counts <- province_counts %>%
  filter(Count > 0)

# Calculate percentages
num_valid_responses <- sum(province_counts$Count)
province_counts$Percentage <- (province_counts$Count / num_valid_responses) * 100
province_counts$Label <- paste0(round(province_counts$Percentage, 1), "%")

# Define colors for provinces (same for fill and label background)
province_colors <- c("#e3120b", "#00688D", "#FDBF6F", "#33A02C", 
                     "#A6CEE3", "#FB9A99", "#B15928", "#1F78B4", 
                     "#B2DF8A", "#FF7F00", "#6A3D9A", "#CAB2D6", "#FF69B4")

# Create pie chart with label background color matching the slice
ggplot(province_counts, aes(x = "", y = Percentage, fill = Province)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  
  # Use geom_label_repel for colored background labels
  geom_label_repel(aes(label = Label, fill = Province),  # Fill matches slice color
                   position = position_stack(vjust = 0.5),  # Correct label placement
                   size = 5, fontface = "bold", color = "white",  # White text for readability
                   force = 2, max.overlaps = Inf, segment.color = "grey50") +  
  
  labs(title = "Province Distribution in Canadian Election Data", fill = "Province") +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right"
  ) +
  scale_fill_manual(values = province_colors)  # Ensures label background matches slices

############################ New Theme_Bar Chart for Satisfaction (Green) ##############################

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)  # For reshaping data
library(ggthemes)

# Select the political party to analyze (change this value as needed)
selected_party <- "Green"

# Convert numerical satisfaction values into meaningful labels
ce_p <- ce_p %>%
  mutate(
    cps21_demsat_label = factor(cps21_demsat, 
                                levels = c(1, 2, 3, 4, 5), 
                                labels = c("Very satisfied", "Fairly satisfied", 
                                           "Not very satisfied", "Not satisfied at all", "NA")),
    
    pes21_dem_sat_label = factor(pes21_dem_sat, 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Very satisfied", "Fairly satisfied", 
                                            "Not very satisfied", "Not satisfied at all", "NA")),
    
    voting_choise_label = factor(voting_choise, 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Liberal", "Conservative", "NDP", 
                                            "Bloc Québécois", "Green"))
  )

# Filter only respondents who voted for the selected party
filtered_data <- ce_p %>%
  filter(voting_choise_label == selected_party) %>%
  select(cps21_demsat_label, pes21_dem_sat_label)  # Keep only relevant variables

# Reshape data: Merge pre-election & post-election satisfaction into a single column
long_data <- filtered_data %>%
  pivot_longer(cols = c(cps21_demsat_label, pes21_dem_sat_label), 
               names_to = "Satisfaction_Type", 
               values_to = "Satisfaction_Level")

# Rename "Satisfaction_Type" to indicate Pre-election and Post-election
long_data <- long_data %>%
  mutate(Satisfaction_Type = recode(Satisfaction_Type, 
                                    cps21_demsat_label = "Pre-election",
                                    pes21_dem_sat_label = "Post-election"))

# Create a bar chart comparing pre-election vs. post-election satisfaction
ggplot(long_data, aes(x = Satisfaction_Level, fill = Satisfaction_Type)) +
  geom_bar(position = position_dodge2(reverse = TRUE)) +
  labs(title = paste("Pre-election vs. Post-election Satisfaction for", selected_party),
       x = "Satisfaction Level", 
       y = "Count",
       fill = "Election Phase") +
  theme_minimal(base_size = 14) +  # 使用簡潔風格
  theme(
    panel.background = element_rect(fill = "white"),  # 背景設定為白色
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray80"),  # 模仿 Economist 的網格線
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  ) +
  scale_fill_manual(values = c("#e3120b", "#00688D"))  # Economist 紅 & 藍

############################ New Theme_Bar Chart for Satisfaction (Bloc Québécois) ##############################

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)  # For reshaping data
library(ggthemes)

# Select the political party to analyze (change this value as needed)
selected_party <- "Bloc Québécois"

# Convert numerical satisfaction values into meaningful labels
ce_p <- ce_p %>%
  mutate(
    cps21_demsat_label = factor(cps21_demsat, 
                                levels = c(1, 2, 3, 4, 5), 
                                labels = c("Very satisfied", "Fairly satisfied", 
                                           "Not very satisfied", "Not satisfied at all", "NA")),
    
    pes21_dem_sat_label = factor(pes21_dem_sat, 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Very satisfied", "Fairly satisfied", 
                                            "Not very satisfied", "Not satisfied at all", "NA")),
    
    voting_choise_label = factor(voting_choise, 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Liberal", "Conservative", "NDP", 
                                            "Bloc Québécois", "Green"))
  )

# Filter only respondents who voted for the selected party
filtered_data <- ce_p %>%
  filter(voting_choise_label == selected_party) %>%
  select(cps21_demsat_label, pes21_dem_sat_label)  # Keep only relevant variables

# Reshape data: Merge pre-election & post-election satisfaction into a single column
long_data <- filtered_data %>%
  pivot_longer(cols = c(cps21_demsat_label, pes21_dem_sat_label), 
               names_to = "Satisfaction_Type", 
               values_to = "Satisfaction_Level")

# Rename "Satisfaction_Type" to indicate Pre-election and Post-election
long_data <- long_data %>%
  mutate(Satisfaction_Type = recode(Satisfaction_Type, 
                                    cps21_demsat_label = "Pre-election",
                                    pes21_dem_sat_label = "Post-election"))

# Create a bar chart comparing pre-election vs. post-election satisfaction
ggplot(long_data, aes(x = Satisfaction_Level, fill = Satisfaction_Type)) +
  geom_bar(position = position_dodge2(reverse = TRUE)) +
  labs(title = paste("Pre-election vs. Post-election Satisfaction for", selected_party),
       x = "Satisfaction Level", 
       y = "Count",
       fill = "Election Phase") +
  theme_minimal(base_size = 14) +  # 使用簡潔風格
  theme(
    panel.background = element_rect(fill = "white"),  # 背景設定為白色
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray80"),  # 模仿 Economist 的網格線
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  ) +
  scale_fill_manual(values = c("#e3120b", "#00688D"))  # Economist 紅 & 藍

############################ New Theme_Bar Chart for Satisfaction (NDP) ##############################

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)  # For reshaping data
library(ggthemes)

# Select the political party to analyze (change this value as needed)
selected_party <- "NDP"

# Convert numerical satisfaction values into meaningful labels
ce_p <- ce_p %>%
  mutate(
    cps21_demsat_label = factor(cps21_demsat, 
                                levels = c(1, 2, 3, 4, 5), 
                                labels = c("Very satisfied", "Fairly satisfied", 
                                           "Not very satisfied", "Not satisfied at all", "NA")),
    
    pes21_dem_sat_label = factor(pes21_dem_sat, 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Very satisfied", "Fairly satisfied", 
                                            "Not very satisfied", "Not satisfied at all", "NA")),
    
    voting_choise_label = factor(voting_choise, 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Liberal", "Conservative", "NDP", 
                                            "Bloc Québécois", "Green"))
  )

# Filter only respondents who voted for the selected party
filtered_data <- ce_p %>%
  filter(voting_choise_label == selected_party) %>%
  select(cps21_demsat_label, pes21_dem_sat_label)  # Keep only relevant variables

# Reshape data: Merge pre-election & post-election satisfaction into a single column
long_data <- filtered_data %>%
  pivot_longer(cols = c(cps21_demsat_label, pes21_dem_sat_label), 
               names_to = "Satisfaction_Type", 
               values_to = "Satisfaction_Level")

# Rename "Satisfaction_Type" to indicate Pre-election and Post-election
long_data <- long_data %>%
  mutate(Satisfaction_Type = recode(Satisfaction_Type, 
                                    cps21_demsat_label = "Pre-election",
                                    pes21_dem_sat_label = "Post-election"))

# Create a bar chart comparing pre-election vs. post-election satisfaction
ggplot(long_data, aes(x = Satisfaction_Level, fill = Satisfaction_Type)) +
  geom_bar(position = position_dodge2(reverse = TRUE)) +
  labs(title = paste("Pre-election vs. Post-election Satisfaction for", selected_party),
       x = "Satisfaction Level", 
       y = "Count",
       fill = "Election Phase") +
  theme_minimal(base_size = 14) +  # 使用簡潔風格
  theme(
    panel.background = element_rect(fill = "white"),  # 背景設定為白色
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray80"),  # 模仿 Economist 的網格線
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  ) +
  scale_fill_manual(values = c("#e3120b", "#00688D"))  # Economist 紅 & 藍

############################ New Theme_Bar Chart for Satisfaction (Conservative) ##############################

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)  # For reshaping data
library(ggthemes)

# Select the political party to analyze (change this value as needed)
selected_party <- "Conservative"

# Convert numerical satisfaction values into meaningful labels
ce_p <- ce_p %>%
  mutate(
    cps21_demsat_label = factor(cps21_demsat, 
                                levels = c(1, 2, 3, 4, 5), 
                                labels = c("Very satisfied", "Fairly satisfied", 
                                           "Not very satisfied", "Not satisfied at all", "NA")),
    
    pes21_dem_sat_label = factor(pes21_dem_sat, 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Very satisfied", "Fairly satisfied", 
                                            "Not very satisfied", "Not satisfied at all", "NA")),
    
    voting_choise_label = factor(voting_choise, 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Liberal", "Conservative", "NDP", 
                                            "Bloc Québécois", "Green"))
  )

# Filter only respondents who voted for the selected party
filtered_data <- ce_p %>%
  filter(voting_choise_label == selected_party) %>%
  select(cps21_demsat_label, pes21_dem_sat_label)  # Keep only relevant variables

# Reshape data: Merge pre-election & post-election satisfaction into a single column
long_data <- filtered_data %>%
  pivot_longer(cols = c(cps21_demsat_label, pes21_dem_sat_label), 
               names_to = "Satisfaction_Type", 
               values_to = "Satisfaction_Level")

# Rename "Satisfaction_Type" to indicate Pre-election and Post-election
long_data <- long_data %>%
  mutate(Satisfaction_Type = recode(Satisfaction_Type, 
                                    cps21_demsat_label = "Pre-election",
                                    pes21_dem_sat_label = "Post-election"))

# Create a bar chart comparing pre-election vs. post-election satisfaction
ggplot(long_data, aes(x = Satisfaction_Level, fill = Satisfaction_Type)) +
  geom_bar(position = position_dodge2(reverse = TRUE)) +
  labs(title = paste("Pre-election vs. Post-election Satisfaction for", selected_party),
       x = "Satisfaction Level", 
       y = "Count",
       fill = "Election Phase") +
  theme_minimal(base_size = 14) +  # 使用簡潔風格
  theme(
    panel.background = element_rect(fill = "white"),  # 背景設定為白色
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray80"),  # 模仿 Economist 的網格線
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  ) +
  scale_fill_manual(values = c("#e3120b", "#00688D"))  # Economist 紅 & 藍

############################ New Theme_Bar Chart for Satisfaction (Liberal) ##############################

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)  # For reshaping data
library(ggthemes)

# Select the political party to analyze (change this value as needed)
selected_party <- "Liberal"

# Convert numerical satisfaction values into meaningful labels
ce_p <- ce_p %>%
  mutate(
    cps21_demsat_label = factor(cps21_demsat, 
                                levels = c(1, 2, 3, 4, 5), 
                                labels = c("Very satisfied", "Fairly satisfied", 
                                           "Not very satisfied", "Not satisfied at all", "NA")),
    
    pes21_dem_sat_label = factor(pes21_dem_sat, 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Very satisfied", "Fairly satisfied", 
                                            "Not very satisfied", "Not satisfied at all", "NA")),
    
    voting_choise_label = factor(voting_choise, 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Liberal", "Conservative", "NDP", 
                                            "Bloc Québécois", "Green"))
  )

# Filter only respondents who voted for the selected party
filtered_data <- ce_p %>%
  filter(voting_choise_label == selected_party) %>%
  select(cps21_demsat_label, pes21_dem_sat_label)  # Keep only relevant variables

# Reshape data: Merge pre-election & post-election satisfaction into a single column
long_data <- filtered_data %>%
  pivot_longer(cols = c(cps21_demsat_label, pes21_dem_sat_label), 
               names_to = "Satisfaction_Type", 
               values_to = "Satisfaction_Level")

# Rename "Satisfaction_Type" to indicate Pre-election and Post-election
long_data <- long_data %>%
  mutate(Satisfaction_Type = recode(Satisfaction_Type, 
                                    cps21_demsat_label = "Pre-election",
                                    pes21_dem_sat_label = "Post-election"))

# Create a bar chart comparing pre-election vs. post-election satisfaction
ggplot(long_data, aes(x = Satisfaction_Level, fill = Satisfaction_Type)) +
  geom_bar(position = position_dodge2(reverse = TRUE)) +
  labs(title = paste("Pre-election vs. Post-election Satisfaction for", selected_party),
       x = "Satisfaction Level", 
       y = "Count",
       fill = "Election Phase") +
  theme_minimal(base_size = 14) +  # 使用簡潔風格
  theme(
    panel.background = element_rect(fill = "white"),  # 背景設定為白色
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray80"),  # 模仿 Economist 的網格線
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  ) +
  scale_fill_manual(values = c("#e3120b", "#00688D"))  # Economist 紅 & 藍



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




