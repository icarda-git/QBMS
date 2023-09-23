# ICARDA BMS server
set_qbms_config('https://bms.icarda.org/ibpworkbench/controller/auth/login')

login_bms()

list_crops()
set_crop('wheat')

list_programs()
set_program('Spring Bread Wheat')

(trials <- list_trials())
set_trial(trials[1, 'trialName'])

(studies <- list_studies())
set_study(studies[1, 'studyName'])

bms_germplasm <- get_germplasm_list()
bms_data <- get_study_data()
bms_info <- get_study_info()

# https://cbbrapi-qa.ebsproject.org/
# https://cbbrapi-qa.ebsproject.org/v2/docs#/

ebs <- list()

ebs$config <- list(crop      = '', 
                   server    = 'https://cbbrapi-qa.ebsproject.org', 
                   path      = '', 
                   page_size = 1000, 
                   time_out  = 120, 
                   base_url  = 'https://cbbrapi-qa.ebsproject.org',
                   engine    = 'ebs',
                   brapi_ver = 'v2',
                   varbose   = TRUE)

ebs$state  <- list(token = '', 
                   user = '', 
                   expires_in = as.numeric(Sys.time()) + 3600)

# https://cbbrapi-qa.ebsproject.org/brapi/v2/auth/login
ebs$state$token <- readline('token:')

set_qbms_connection(ebs)

list_programs()
set_program('BW Wheat Breeding Program')

# startDate is always null in /brapi/v2/trials?programDbId=
list_trials()
set_trial('CORB-5128-1553')

list_studies()
set_study('CORB-5128-1553-001')

# does not restrict by study id /brapi/v2/germplasm?studyDbId=
germplasm <- get_germplasm_list()

# calculated trait name included in the "observationVariables"
# but calculated trait values not exists in the "data" (e.g., Plant height_AVG)
data <- get_study_data()

