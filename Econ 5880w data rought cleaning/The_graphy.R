# Load necessary packages
library(ggdag)
library(dagitty)
library(ggplot2)

# Define DAG structure
dag <- dagitty("
dag {
  'Province'
  'Education' 
  'Vaccine' 
  'Political View' 
  'Di: Preferred Party Win or Not' 
  'Change in Democracy' 
  'Interest in Politics' 
  'Interest in Elections' 
  'Hate Speech Illegal?' 
  'Jobs vs. Environment' 
  'Political Discussions' 
  'Family Values' 
  'Equal Rights'
  'Immigrant'

  'Province' -> 'Political View'
  'Province' -> 'Education'
  'Province' -> 'Di: Preferred Party Win or Not'
  'Education' -> 'Political View'
  'Education' -> 'Change in Democracy'
  'Political View' -> 'Change in Democracy'
  'Di: Preferred Party Win or Not' -> 'Change in Democracy'
  'Political View' -> 'Di: Preferred Party Win or Not'
  'Interest in Politics' -> 'Political View'
  'Interest in Elections' -> 'Di: Preferred Party Win or Not'
  'Vaccine' -> 'Political View'
  'Hate Speech Illegal?' -> 'Political View'
  'Jobs vs. Environment' -> 'Political View'
  'Political Discussions' -> 'Political View'
  'Family Values' -> 'Political View'
  'Equal Rights' -> 'Political View'
  'Immigrant' -> 'Political View'
}
")

# Convert to a tidy DAG for ggdag
tidy_dag <- tidy_dagitty(dag)

# Plot the DAG
ggdag(tidy_dag, text = TRUE) +
  theme_minimal() +
  labs(title = "Causal Diagram for Political Attitudes & Democracy")
