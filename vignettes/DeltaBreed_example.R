install.packages("remotes")
remotes::install_github("icarda-git/QBMS")

library(QBMS)

set_qbms_config(url = 'https://sandbox.breedinginsight.net/', 
                path = 'v1/programs/025a8e6e-15fc-4488-8d26-41eb16107a95', 
                engine = '', brapi_ver = 'v2')

set_token(readline('token:'))

# list all breeding programs in the selected crop
list_programs()

# select a breeding program by name
set_program("Public (PUBL)")

# list all studies/trials in the selected program
list_trials()

# select a specific study/trial by name
set_trial("Performance Trial 2023B")

# list all environments/locations information in the selected study/trial
list_studies()

# select a specific environment/location by name
set_study("Parlier FA23")

# retrieve general information, data, and germplasm list 
# of the selected environment/location
info <- get_study_info()
data <- get_study_data()
germplasm <- get_germplasm_list()

# get observation variable ontology in the selected study/trial
ontology <- get_trial_obs_ontology()

# retrieve multi-environment trial data of the selected study/trial
MET <- get_trial_data()
