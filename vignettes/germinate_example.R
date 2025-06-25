# Why Germinate:
# - CGIAR use it to host fingerprint data
# - LMF project use it

# install the latest development version of QBMS from the GitHub repository
if (!require("remotes")) install.packages("remotes")
remotes::install_github("icarda-git/QBMS")

# load the QBMS library
library(QBMS)

# https://germinate.hutton.ac.uk/demo/api/brapi/v2/serverinfo

# configure your Germinate server connection
set_qbms_config(url = "https://germinate.hutton.ac.uk/demo/#/home", engine = "germinate", no_auth = TRUE, page_size = 9999)

# login("username", "password")
# login()

# dump <- debug_qbms()
# dump$config
# dump$state

# list existing crops on the current server
list_crops()

# select a crop by name
set_crop("Cactuar")

# list all breeding programs in the selected crop
list_programs()

# select a breeding program by name
set_program("Germinate")

# list all trials in the selected program
list_trials()

# select a specific trial by name
set_trial("GWAS data")

# list all studies (environments/locations) in the selected trial
list_studies()


### Phenotypic Data ############################################################

# select a specific study by name
set_study("Sample Phenotype Data")

# retrieve general information and metadata for the selected study
info <- get_study_info()

# retrieve the germplasm list for the selected study
geno <- get_germplasm_list()

# retrieve attributes for a specified germplasm
attr <- get_germplasm_attributes("CACTUAR-3")

# Germinate does not implement the /observation/table BrAPI endpoint yet
# TO DO: use the /search/observations endpoint as a replacement
#
# https://germinate.hutton.ac.uk/demo/api/brapi/v2/observations/table?studyDbId=5
# header Accept application/json
data <- get_study_data()

# still need to test
ontology <- get_trial_obs_ontology()


### Genotypic Data #############################################################

set_study("Sample Genotype Data Subset 2")

list_variantsets()

set_variantset("Sample Genotype Data Subset 2")

# snps <- get_variants()
snps <- get_variantset()


### Pedigree Data ##############################################################

# Germinate dose not support collection, familyCode, binomialName, programDbId, 
# includeSiblings, externalReferenceId and externalReferenceSource
# /pedigree?germplasmDbId=1&includeFullTree=true&includeParents=true&includeProgeny=true

# /pedigree?trialDbId=8&includeFullTree=true&includeParents=true
set_trial("Default pedigree experiment")

# TODO
ped <- get_trial_pedigree()

# BrAPI v1.3 pedigree endpoint
# /germplasm/{germplasmDbId}/pedigree
# https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI/1.3#/Germplasm/get_germplasm__germplasmDbId__pedigree
