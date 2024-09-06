# /brapi/{brapi_ver}/{brapi_call}
brapi_map <- data.frame(func_name  = character(), 
                        brapi_ver  = character(),
                        brapi_call = character())

# need to be deprecated in favor of list_dbs() and set_db() functions
# https://github.com/plantbreeding/BrAPI/issues/495
brapi_map <- rbind(brapi_map, c("list_crops", "v1", "crops"))
brapi_map <- rbind(brapi_map, c("list_crops", "v2", "commoncropnames"))

brapi_map <- rbind(brapi_map, c("list_programs", "v1", "programs"))
brapi_map <- rbind(brapi_map, c("list_programs", "v2", "programs"))

brapi_map <- rbind(brapi_map, c("get_program_trials", "v1", "trials?programDbId={programDbId}"))
brapi_map <- rbind(brapi_map, c("get_program_trials", "v2", "trials?programDbId={programDbId}"))

brapi_map <- rbind(brapi_map, c("list_studies", "v1", "studies?trialDbId={trialDbId}"))
brapi_map <- rbind(brapi_map, c("list_studies", "v2", "studies?trialDbId={trialDbId}"))

brapi_map <- rbind(brapi_map, c("get_study_info", "v1", "studies/{studyDbId}"))
brapi_map <- rbind(brapi_map, c("get_study_info", "v2", "studies/{studyDbId}"))

brapi_map <- rbind(brapi_map, c("get_study_data", "v1", "studies/{studyDbId}/table"))
brapi_map <- rbind(brapi_map, c("get_study_data", "v2", "observations/table?studyDbId={studyDbId}"))

brapi_map <- rbind(brapi_map, c("get_germplasm_list", "v1", "studies/{studyDbId}/germplasm"))
brapi_map <- rbind(brapi_map, c("get_germplasm_list", "v2", "germplasm?studyDbId={studyDbId}"))

brapi_map <- rbind(brapi_map, c("list_locations", "v1", "locations"))
brapi_map <- rbind(brapi_map, c("list_locations", "v2", "locations"))

brapi_map <- rbind(brapi_map, c("get_trial_obs_ontology", "v1", "variables"))
brapi_map <- rbind(brapi_map, c("get_trial_obs_ontology", "v2", "search/variables"))

brapi_map <- rbind(brapi_map, c("get_germplasm_id", "v1", "germplasm?germplasmName={germplasmName}"))
brapi_map <- rbind(brapi_map, c("get_germplasm_id", "v2", "germplasm?germplasmName={germplasmName}"))

# POST: germplasmDbIds, observationLevel = "PLOT"
brapi_map <- rbind(brapi_map, c("get_germplasm_data", "v1", "phenotypes-search"))

brapi_map <- rbind(brapi_map, c("get_germplasm_attributes", "v1", "germplasm/{germplasmDbId}/attributes"))
brapi_map <- rbind(brapi_map, c("get_germplasm_attributes", "v2", "attributes?germplasmDbId={germplasmDbId}"))

# gigwa brapi_get_call(s)
brapi_map <- rbind(brapi_map, c("gigwa_list_dbs", "v2", "programs"))
brapi_map <- rbind(brapi_map, c("gigwa_list_projects", "v2", "studies?programDbId={programDbId}"))

# gigwa brapi_post_search_call(s)
brapi_map <- rbind(brapi_map, c("gigwa_list_runs", "v2", "search/variantsets"))
brapi_map <- rbind(brapi_map, c("gigwa_get_samples", "v2", "search/samples"))
brapi_map <- rbind(brapi_map, c("gigwa_get_sequences", "v2", "search/references"))
brapi_map <- rbind(brapi_map, c("gigwa_get_allelematrix", "v2", "search/allelematrix"))
brapi_map <- rbind(brapi_map, c("gigwa_get_markers", "v2", "search/variants"))
brapi_map <- rbind(brapi_map, c("gigwa_get_metadata", "v2", "search/attributevalues"))

colnames(brapi_map) <- c("func_name", "brapi_ver", "brapi_call")


#' Get the BrAPI Endpoint URL for a given QBMS function
#'
#' @param func_name (string) Function name
#'
#' @return
#' The BrAPI endpoint URL to call
#'
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}

get_brapi_url <- function(func_name) {
  call_url <- paste0(qbms_globals$config$base_url, 
                     ifelse(qbms_globals$config$crop == "", "", paste0("/", qbms_globals$config$crop)), 
                     "/brapi/", qbms_globals$config$brapi_ver, "/", 
                     brapi_map[brapi_map$func_name == func_name & brapi_map$brapi_ver == qbms_globals$config$brapi_ver, "brapi_call"])
  return(call_url)
}


#' Scan BrAPI Endpoints
#'
#' @description
#' Scan available BrAPI endpoints on the configured source server.
#'
#' @param programDbId (numeric) ProgramDbId used for BrAPI endpoints scanning. Default is 0.
#' @param trialDbId (numeric) TrialDbId used for BrAPI endpoints scanning. Default is 0.
#' @param studyDbId (numeric) StudyDbId used for BrAPI endpoints scanning. Default is 0.
#'
#' @return
#' A data frame listing the QBMS function, BrAPI endpoint URL, and status.
#'
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#'
#' @export

scan_brapi_endpoints <- function(programDbId = 0, trialDbId = 0, studyDbId = 0) {
  if (is.null(qbms_globals$config$base_url)) {
    stop("No server has been defined yet! You have to set your server configurations first using the `set_qbms_config()` function")
  }
  
  call_url <- paste0(qbms_globals$config$base_url, 
                     ifelse(qbms_globals$config$crop == "", "", paste0("/", qbms_globals$config$crop)), 
                     "/brapi/", brapi_map$brapi_ver, "/", 
                     brapi_map$brapi_call)
  
  call_url <- sub("\\{programDbId\\}", programDbId, call_url)
  call_url <- sub("\\{trialDbId\\}", trialDbId, call_url)
  call_url <- sub("\\{studyDbId\\}", studyDbId, call_url)
  call_url <- sub("\\{.*\\}", "1", call_url)
  
  # ensure getting the minimum data while scanning BrAPI endpoints
  call_url <- ifelse(grepl("\\?", call_url),
                     paste0(call_url, "&pageSize=1"),
                     paste0(call_url, "?pageSize=1"))
  
  scan_result <- !sapply(call_url, function (url) httr::http_error(httr::GET(url, httr::add_headers(brapi_headers()))))
  scan_result <- as.data.frame(cbind(brapi_map$func_name, call_url, scan_result))
  
  rownames(scan_result) <- NULL
  colnames(scan_result) <- c("QBMS Function", "BrAPI endpoint", "Available")
  
  return(scan_result)
}


# List of non-BrAPI calls in QBMS functions
#
# get_program_studies()
# /crops/{cropName}/programs/{programUUID}/studies/{studyId}/entries/metadata (BMS: get study entries metadata)
#
# get_germplasm_list()
# /crops/{cropName}/programs/{programUUID}/studies/{studyId}/entries (BMS: get entry type) (POST: body = "")
#
# gigwa_get_variants()
# /ga4gh/variants/search 
# 
# dancing steps: 
# - searchMode = 0 to get total
# - then searchMode = 3 to request actual results
# - keep checking progress status /gigwa/progress
# - then call the same /ga4gh/variants/search to get the ready results
# 
# GA4GH: https://rest.ensembl.org/documentation/info/gavariants
# BrAPI: https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.1#/Allele%20Matrix


# Internal state variables/lists
qbms_globals <- new.env()
qbms_globals$config <- list(crop = NULL)
qbms_globals$state  <- list(token = NULL)


#' Debug Internal QBMS Status Object
#'
#' @description
#' Returns the internal QBMS status object for debugging purposes.
#'
#' @return
#' An environment object containing package configuration and status.
#'
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#'
#' @examples
#' obj <- debug_qbms()
#' obj$config
#' obj$state
#'
#' @export

debug_qbms <- function() {
  return(qbms_globals)
}


#' Get the QBMS Connection
#'
#' @description
#' Retrieves the QBMS connection object from the current environment.
#'
#' @return
#' A list containing the current connection configuration and status.
#'
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#'
#' @seealso \code{\link{set_qbms_connection}}
#'
#' @examples
#' if(interactive()) {
#'   # Configure your server connection
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'
#'   # Login using your account (interactive mode)
#'   # You can pass your username and password as parameters (batch mode)
#'   login_bms()
#'
#'   # Select a crop by name
#'   set_crop("wheat")
#'
#'   # Select a breeding program by name
#'   set_program("Wheat International Nurseries")
#'      
#'   # Get germplasm data
#'   df1 <- get_germplasm_data("Jabal")
#'   
#'   # Save the current connection (phenotypic server)
#'   con1 <- get_qbms_connection()
#'   
#'   # Configure QBMS to connect to the genotypic server
#'   set_qbms_config("https://gigwa.southgreen.fr/gigwa/", engine = "gigwa", no_auth = TRUE)
#'   
#'   # Set the db, project, and run
#'   gigwa_set_db("DIVRICE_NB")
#'   gigwa_set_project("refNB")
#'   gigwa_set_run("03052022")
#'   
#'   # Get associated metadata
#'   df2 <- gigwa_get_metadata()
#'   
#'   # Save the current connection (before switch)
#'   con2 <- get_qbms_connection()
#'   
#'   # Load the saved phenotypic server connection
#'   set_qbms_connection(con1)
#'   
#'   # Continue retrieving germplasm attributes from the phenotypic server
#'   df3 <- get_germplasm_attributes("Jabal")
#' }
#'
#' @export

get_qbms_connection <- function() {
  return(as.list(qbms_globals))
}


#' Set the QBMS Connection
#'
#' @description
#' Sets the QBMS connection object to the current environment.
#'
#' @param env A list containing the connection configuration and status to load.
#'
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#'
#' @seealso \code{\link{get_qbms_connection}}
#'
#' @examples
#' if(interactive()) {
#'   # Configure your server connection
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'
#'   # Login using your account (interactive mode)
#'   # You can pass your username and password as parameters (batch mode)
#'   login_bms()
#'
#'   # Select a crop by name
#'   set_crop("wheat")
#'
#'   # Select a breeding program by name
#'   set_program("Wheat International Nurseries")
#'      
#'   # Get germplasm data
#'   df1 <- get_germplasm_data("Jabal")
#'   
#'   # Save the current connection (phenotypic server)
#'   con1 <- get_qbms_connection()
#'   
#'   # Configure QBMS to connect to the genotypic server
#'   set_qbms_config("https://gigwa.southgreen.fr/gigwa/", engine = "gigwa", no_auth = TRUE)
#'   
#'   # Set the db, project, and run
#'   gigwa_set_db("DIVRICE_NB")
#'   gigwa_set_project("refNB")
#'   gigwa_set_run("03052022")
#'   
#'   # Get associated metadata
#'   df2 <- gigwa_get_metadata()
#'   
#'   # Save the current connection (before switch)
#'   con2 <- get_qbms_connection()
#'   
#'   # Load the saved phenotypic server connection
#'   set_qbms_connection(con1)
#'   
#'   # Continue retrieving germplasm attributes from the phenotypic server
#'   df3 <- get_germplasm_attributes("Jabal")
#' }
#'
#' @export

set_qbms_connection <- function(env) {
  qbms_globals$config <- list(crop = NULL)
  qbms_globals$state  <- list(token = NULL)
  
  if (!is.null(env$config)) {
    qbms_globals$config <- env$config
  }
  
  if (!is.null(env$state)) {
    qbms_globals$state <- env$state
  }
}


#' Configure BMS Server Settings
#'
#' @description
#' Sets the connection configuration of the BMS server.
#'
#' @param url URL of the BMS login page. Default is "http://localhost/ibpworkbench/".
#' @param path API path. Default is NULL.
#' @param page_size Page size. Default is 1000.
#' @param time_out Number of seconds to wait for a response until giving up. Default is 10.
#' @param no_auth TRUE if the server doesn't require authentication/login. Default is FALSE.
#' @param engine Backend database (bms default, breedbase, gigwa, ebs).
#' @param brapi_ver BrAPI version (v1 or v2).
#' @param verbose Logical indicating if progress bar will display on the console when retrieving data from API. TRUE by default.
#'
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#'
#' @return
#' No return value.
#'
#' @examples
#' set_qbms_config("https://bmsdev-brapi.ibp.services/ibpworkbench")
#'
#' @export

set_qbms_config <- function(url = "http://localhost/ibpworkbench/controller/auth/login",
                            path = NULL, page_size = 1000, time_out = 120, no_auth = FALSE, 
                            engine = "bms", brapi_ver = "v1", verbose = TRUE) {
  
  if (is.null(path)) {
    if (engine == "bms") { path = "bmsapi" }
    if (engine == "breedbase") { path = "" }
    if (engine == "gigwa") { path = "gigwa/rest"; brapi_ver = "v2"}
    if (engine == "ebs") { path = "" }
  }
  
  qbms_globals$config <- list(crop = "")
  qbms_globals$state  <- list(token = NULL)
  
  if (engine == "bms") { qbms_globals$config$crop <- NULL }
  
  qbms_globals$config$server    <- regmatches(url, regexpr("^(?://|[^/]+)*", url))
  qbms_globals$config$path      <- path
  qbms_globals$config$page_size <- page_size
  qbms_globals$config$time_out  <- time_out
  qbms_globals$config$base_url  <- paste0(qbms_globals$config$server, ifelse(path == "", "", paste0("/", path)))
  qbms_globals$config$engine    <- engine
  qbms_globals$config$brapi_ver <- brapi_ver
  qbms_globals$config$verbose   <- verbose
  qbms_globals$config$sleep     <- 1
  
  if (no_auth == TRUE) {
    qbms_globals$state$token <- NA
  }
  
  qbms_globals$state$crops <- NULL
}
