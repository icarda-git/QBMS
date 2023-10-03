install.packages("remotes")
remotes::install_github("icarda-git/QBMS")

library(QBMS)

# https://cb-qa.ebsproject.org/
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
set_program('Irrigated South-East Asia')

# startDate is always null in /brapi/v2/trials?programDbId=
list_trials()
set_trial('CORB-5272 -test1')

list_studies()
set_study('CORB-5272 -test1-001')

info <- get_study_info()

# calculated trait name included in the "observationVariables"
# but calculated trait values not exists in the "data" (e.g., Plant height_AVG)
# https://ebsproject.atlassian.net/servicedesk/customer/portal/2/ESD-4612
data <- get_study_data()

# does not restrict by study id /brapi/v2/germplasm?studyDbId=
# https://ebsproject.atlassian.net/servicedesk/customer/portal/2/ESD-4613
germplasm <- get_germplasm_list()

# temp fix to get the study germplasm list
germplasm <- germplasm[germplasm$germplasmDbId %in% unique(data$germplasmDbId),]

MET <- get_trial_data()

#' e.g., /brapi/v2/observations/table?studyDbId=3793
#' {...,
#'  "result": {
#'    "data": [...],
#'    "headerRow": [...],
#'    "observationVariables": [
#'      {
#'        "observationVariableDbId": "397",
#'        "observationVariableName": "Actual plot yield in grams"
#'      }, 
#'      {
#'        "observationVariableDbId": "212",
#'        "observationVariableName": "Plant Height"
#'      },...]
#'    }
#'  }
#'  
#' /brapi/v2/variables?observationVariableDbId=397 (OK)
#' /brapi/v2/variables?observationVariableDbId=212 (data[], Oops!)
#' 
#' when call get_study_data() function, QBMS should cache:
#' - the unique(data$germplasmDbId) to restrict/fix the study germplasm list
#' - the observationVariables table, to get the traits ontology
# ontology <- get_trial_obs_ontology()

#' implement the /brapi/v2/locations endpoint
# list_locations()
# get_program_studies()

#' implement the /brapi/v2/germplasm?germplasmName= endpoint
# get_germplasm_id()
# get_germplasm_attributes()

#' call the /brapi/v2/observations/table?germplasmDbId= endpoint generate error 500
# get_germplasm_data()


################################################################################
# EBS OAuth 2.0
# https://cbbrapi-qa.ebsproject.org/brapi/v2/auth/login
# https://ebsproject.atlassian.net/servicedesk/customer/portal/2/ESD-4615

app <- httr::oauth_app(appname = 'qbms_app', key = 'QBMS', secret = NULL)

endpoint <- httr::oauth_endpoint(authorize = 'https://cbbrapi-qa.ebsproject.org/brapi/v2/auth/authorize',
                                 access = 'https://cbbrapi-qa.ebsproject.org/brapi/v2/auth/access_token')

token <- httr::oauth2.0_token(endpoint, app)
