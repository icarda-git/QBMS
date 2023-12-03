install.packages("remotes")
remotes::install_github("icarda-git/QBMS")

library(QBMS)

# https://cb-qa.ebsproject.org/
# https://cbbrapi-qa.ebsproject.org/
# https://cbbrapi-qa.ebsproject.org/v2/docs#/

set_qbms_config(url = 'https://cbbrapi-qa.ebsproject.org', engine = 'ebs', brapi_ver = 'v2')

# set_token(readline('token:'))

################################################################################
#' EBS OAuth 2.0
#'
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
#' 
#' JSON Web Tokens are an open, industry standard RFC 7519 method for representing 
#' claims securely between two parties.
#'
#' https://jwt.io/ allows you to decode, verify and generate JWT.

oauth2_login(authorize_url = 'https://auth-dev.ebsproject.org/oauth2/authorize', 
             access_url    = 'https://auth-dev.ebsproject.org/oauth2/token', 
             client_id     = '5crahiqorgj0lppt3n9dkulkst', 
             client_secret = '1sf4tipbp4arj3d5cncjmrvk9c2cu30gor5618hnh8rgkp6v5fs')

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

germplasm <- get_germplasm_list()

ontology <- get_trial_obs_ontology()

MET <- get_trial_data()

#' implement the /brapi/v2/locations endpoint
# list_locations()
# get_program_studies()

#' implement the /brapi/v2/germplasm?germplasmName= endpoint
# get_germplasm_id()
# get_germplasm_attributes()

#' call the /brapi/v2/observations/table?germplasmDbId= endpoint generate error 500
# get_germplasm_data()
