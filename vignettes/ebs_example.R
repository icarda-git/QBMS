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
# https://ebsproject.atlassian.net/browse/ESD-4612
# The requested feature will be available at the end of October or November at the latest.
data <- get_study_data()

# does not restrict by study id /brapi/v2/germplasm?studyDbId=
# https://ebsproject.atlassian.net/servicedesk/customer/portal/2/ESD-4613
# https://ebsproject.atlassian.net/browse/ESD-4613
# The requested feature will be available at the end of October or November at the latest.
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
# https://ebsproject.atlassian.net/browse/ESD-4615

#' Package maintainers might want to build this app in as a fallback, possibly 
#' taking some measures to obfuscate the client ID and secret and limit its use 
#' to your package.
#' 
#' Note that three-legged OAuth always requires the involvement of a user, so 
#' the word “secret” here can be somewhat confusing. It is not a secret in the 
#' same sense as a password or token. But you probably still want to store it 
#' in an opaque way, so that someone else cannot easily “borrow” it and present 
#' an OAuth consent screen that impersonates your package.
#' 
#' The Client ID is a public identifier of your application, it is not a secret 
#' anyway and any end user can see what it is when the app redirects them to 
#' sign in (e.g., if they use browser tools to view the HTTP request).

ebs$state$client_id <- readline('Client ID:')

app <- httr::oauth_app(appname = 'EBS', key = ebs$state$client_id, secret = NULL)

endpoint <- httr::oauth_endpoint(authorize = 'https://auth.ebsproject.org/oauth2/authorize',
                                 access = 'https://auth.ebsproject.org/oauth2/token')

token <- httr::oauth2.0_token(endpoint, app)
