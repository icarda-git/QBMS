
#' Retrieve Supported Crops from the Server
#'
#' @description
#' Retrieves the list of crops supported by the connected server. If the crop 
#' list is cached in the internal state, it returns the cached data; otherwise, it 
#' sends a BrAPI GET request to fetch the crop list.
#'
#' @return
#' A character vector containing the names of supported crops.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}} to configure and set the current active crop.
#'
#' @examples
#' if (interactive()) {
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'   login_bms()  # Log in to the server
#'   list_crops()  # Retrieve list of supported crops
#' }
#' 
#' @export

list_crops <- function() {
  if (is.null(qbms_globals$state$token)) {
    stop("No server has been connected yet! You have to connect a server first using the `login_bms()` function")
  }

  if (!is.null(qbms_globals$state$crops)) {
    bms_crops <- qbms_globals$state$crops
  } else {
    call_url  <- get_brapi_url("list_crops")

    bms_crops <- brapi_get_call(call_url)$data

    qbms_globals$state$crops <- bms_crops
  }

  return(bms_crops)
}


#' Set the Current Active Crop
#'
#' @description
#' Updates the internal configuration to set the selected crop as the active one. This 
#' must be called before performing crop-specific operations such as retrieving breeding programs.
#'
#' @param crop_name A string specifying the name of the crop to set as active.
#'
#' @return
#' No return value. The function updates the global state with the selected crop.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{list_crops}} to validate and retrieve the list of supported crops.
#'
#' @examples
#' if (interactive()) {
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'   login_bms()  # Log in to the server
#'   set_crop("wheat")  # Set "wheat" as the active crop
#' }
#' 
#' @export

set_crop <- function(crop_name) {
  valid_crops <- list_crops()

  if (!crop_name %in% valid_crops) {
    stop("Your crop name is not supported in this connected BMS server! You may use the `list_crops()` function to check the available crops")
  }

  if (qbms_globals$config$engine == "bms") {
    qbms_globals$config$crop <- crop_name
    
    qbms_globals$state$programs  <- NULL
    qbms_globals$state$locations <- NULL
    qbms_globals$state$variables <- NULL
  }
}


#' Retrieve Breeding Programs for the Active Crop
#'
#' @description
#' Retrieves the list of breeding programs available for the currently selected crop. 
#' The crop must be set using the \code{\link{set_crop}} function prior to calling this.
#'
#' @return
#' A data frame containing the names of breeding programs available for the active crop.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{list_crops}} for managing server connection and crop selection.
#'
#' @examples
#' if (interactive()) {
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'   login_bms()  # Log in to the server
#'   set_crop("wheat")  # Set "wheat" as the active crop
#'   list_programs()  # Retrieve breeding programs for the active crop
#' }
#' 
#' @export

list_programs <- function() {
  if (is.null(qbms_globals$state$token)) {
    stop("No server has been connected yet! You have to connect a server first using the `login_bms()` function")
  }

  if (is.null(qbms_globals$config$crop)) {
    stop("No crop has been selected yet! You have to set your crop first using the `set_crop()` function")
  }

  if (is.null(qbms_globals$state$programs)) {
    call_url <- get_brapi_url("list_programs")

    results <- brapi_get_call(call_url)$data
    
    bms_programs <- results[c("programName")]

    qbms_globals$state$programs <- cbind(bms_programs, results[c("programDbId")])
  }
  
  return(subset(qbms_globals$state$programs, select = "programName"))
}


#' Set the Current Active Breeding Program
#'
#' @description
#' Updates the internal state to set the selected breeding program as active using the 
#' associated programDbId. This allows subsequent operations to be carried out 
#' within the context of this program.
#'
#' @param program_name A string specifying the name of the breeding program to set as active.
#'
#' @return
#' No return value. The internal state is updated with the selected program.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{list_programs}} for related operations in the crop and program selection process.
#'
#' @examples
#' if (interactive()) {
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'   login_bms()  # Log in to the server
#'   set_crop("wheat")  # Set crop
#'   set_program("Wheat International Nurseries")  # Set breeding program
#' }
#' 
#' @export

set_program <- function(program_name) {
  valid_programs <- list_programs()

  if (!program_name %in% valid_programs[, 1]) {
    stop("Your breeding program name is not exists in this crop database! You may use the `list_programs()` function to check the available breeding programs")
  }

  program_row <- which(qbms_globals$state$programs[,1] == program_name)

  qbms_globals$state$program_db_id <- qbms_globals$state$programs[program_row, "programDbId"]
  
  qbms_globals$state$trials <- NULL
}


#' Retrieve the List of Trials for the Active Breeding Program
#'
#' @description
#' This internal function retrieves the list of trials for the currently active breeding program 
#' and crop combination. The crop and program must be set using \code{\link{set_crop}} and 
#' \code{\link{set_program}} prior to calling this function.
#'
#' @return
#' A data frame containing information on trials for the active breeding program.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{list_trials}}

get_program_trials <- function() {
  if (!is.null(qbms_globals$state$trials)) {
    bms_program_trials <- qbms_globals$state$trials
  } else {
    call_url <- get_brapi_url("get_program_trials")
    call_url <- sub("\\{programDbId\\}", qbms_globals$state$program_db_id, call_url)
    
    bms_program_trials <- brapi_get_call(call_url, FALSE)$data
    
    qbms_globals$state$trials <- bms_program_trials
  }
  
  return(bms_program_trials)
}


#' List Trials in the Current Active Breeding Program
#'
#' @description
#' Retrieves the list of trials for the current active breeding program. Optionally, 
#' filters trials by their starting year if specified.
#'
#' @param year Numeric. An optional parameter to filter trials by their starting year. 
#'             If not provided, all trials for the active program are returned.
#'
#' @return
#' A data frame containing the names of trials for the active breeding program. If no 
#' trials match the query, a warning is issued, and NA is returned.
#'
#' @note
#' The year filter is only supported for BMS databases.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}} for related operations involving crop and program selection.
#'
#' @examples
#' if (interactive()) {
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'   login_bms()  # Log in to the server
#'   set_crop("wheat")  # Set crop
#'   set_program("Wheat International Nurseries")  # Set breeding program
#'   list_trials()  # List trials
#'   list_trials(2022)  # List trials starting in 2022
#' }
#' 
#' @export

list_trials <- function(year = NULL) {
  if (is.null(qbms_globals$state$program_db_id)) {
    stop("No breeding program has been selected yet! You have to set your breeding program first using the `set_program()` function")
  }

  if (!is.null(year) && !is.numeric(year)) {
    stop("Year parameter should be numeric")
  }

  if (!is.null(year) && qbms_globals$config$engine != "bms") {
    stop("Year parameter is not supported in this database!")
  }

  bms_trials <- get_program_trials()

  if (nrow(bms_trials) > 0) {
    # startDate format in bms_trials is "yyyy-mm-dd"
    if (!is.null(year)) {
      bms_trials <- bms_trials[gsub("-\\d{2}-\\d{2}", "", bms_trials$startDate) == year, ]
    }
    
    trials <- unique(bms_trials[c("trialName")])
    
    if (length(trials$trialName) == 0) {
      warning("No single trial fit your query parameters!")
      trials <- NA
    }
  } else {
    warning("No trials in this program!")
    trials <- NA
  }

  return(trials)
}


#' Set the Current Active Trial
#'
#' @description
#' Updates the internal state to set the selected trial as the current active trial using the 
#' associated trialDbId. This enables operations to be carried out within the context of the selected trial.
#'
#' @param trial_name A string specifying the name of the trial to set as active.
#'
#' @return
#' No return value. The internal state is updated with the selected trial.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{list_trials}} for operations involving crops, programs, and trials.
#'
#' @examples
#' if (interactive()) {
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'   login_bms()  # Log in to the server
#'   set_crop("wheat")  # Set crop
#'   set_program("Wheat International Nurseries")  # Set breeding program
#'   set_trial("IDYT39")  # Set trial
#' }
#' 
#' @export

set_trial <- function(trial_name) {
  valid_trials <- list_trials()

  if (!trial_name %in% valid_trials$trialName) {
    stop("Your trial name is not exists in this breeding program! You may use the `list_trials()` function to check the available trials")
  }

  bms_trials <- get_program_trials()

  trial_row <- which(bms_trials$trialName == trial_name)[1]

  qbms_globals$state$trial_db_id <- as.character(bms_trials[trial_row, c("trialDbId")])
  
  qbms_globals$state$studies <- NULL
  qbms_globals$state$observationVariableDbIds <- NULL
}


#' Get the List of Studies in the Current Active Trial
#'
#' @description
#' Retrieves a list of studies (and associated locations) for the currently active trial, as configured 
#' using the \code{\link{set_trial}} function.
#'
#' @return
#' A data frame containing study names and associated location names. If no studies are available, an error is thrown.
#' 
#' @note
#' This function must be called after a trial has been set using \code{\link{set_trial}}.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{set_trial}} for related operations on crops, programs, and trials.
#'
#' @examples
#' if (interactive()) {
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'   login_bms()  # Log in to the server
#'   set_crop("wheat")  # Set crop
#'   set_program("Wheat International Nurseries")  # Set breeding program
#'   set_trial("IDYT39")  # Set trial
#'   list_studies()  # List studies
#' }
#' 
#' @export

list_studies <- function() {
  if (is.null(qbms_globals$state$trial_db_id)) {
    stop("No trial has been selected yet! You have to set your trial first using the `set_trial()` function")
  }
  
  if (!is.null(qbms_globals$state$studies)) {
    studies <- qbms_globals$state$studies
  } else {
    call_url <- get_brapi_url("list_studies")
    call_url <- sub("\\{trialDbId\\}", qbms_globals$state$trial_db_id, call_url)
    
    trial_studies <- brapi_get_call(call_url, FALSE)$data
    
    if (nrow(trial_studies) == 0) {
      stop("No studies in the selected trial! Please check what you have set in the `set_trial()` function")
    }
    
    studies <- trial_studies[, c("studyName", "locationName", "studyDbId")]

    qbms_globals$state$studies <- studies
  }
  
  return(studies[, c("studyName", "locationName")])
}


#' Set the Current Active Study
#'
#' @description
#' Updates the internal state to set the selected study as the current active study using the 
#' associated studyDbId. This allows operations to be performed within the context of the selected study.
#'
#' @param study_name A string specifying the name of the study to set as active.
#'
#' @return
#' No return value. The internal state is updated with the selected study.
#'  
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{set_trial}}, \code{\link{list_studies}} for related operations on crops, programs, trials, and studies.
#'
#' @examples
#' if (interactive()) {
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'   login_bms()  # Log in to the server
#'   set_crop("wheat")  # Set crop
#'   set_program("Wheat International Nurseries")  # Set breeding program
#'   set_trial("IDYT39")  # Set trial
#'   set_study("IDYT39 Environment Number 9")  # Set study
#' }
#'  
#' @export

set_study <- function(study_name) {
  valid_studies <- list_studies()

  if (!study_name %in% valid_studies$studyName) {
    stop("Your study name is not exists in this trial! You may use the `list_studies()` function to check the available study names")
  }

  study_db_id <- qbms_globals$state$studies[qbms_globals$state$studies$studyName == study_name, "studyDbId"]

  qbms_globals$state$study_db_id <- as.character(study_db_id)
}


#' Get the Details/Metadata of the Current Active Study
#'
#' @description
#' Retrieves detailed metadata for the currently active study. The study must be 
#' set using the \code{\link{set_study}} function, and its details will be fetched 
#' from the BrAPI server.
#'
#' @return
#' A data frame containing the metadata of the active study. Returns \code{NULL} 
#' if no study metadata is available.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, 
#' \code{\link{set_trial}}, \code{\link{set_study}} for related crop and study management.
#'
#' @examples
#' if (interactive()) {
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'   login_bms()
#'   set_crop("wheat")
#'   set_program("Wheat International Nurseries")
#'   set_trial("IDYT39")
#'   set_study("IDYT39 Environment Number 9")
#'   info <- get_study_info()
#' }
#' 
#' @export

get_study_info <- function() {
  if (is.null(qbms_globals$state$study_db_id)) {
    stop("No study has been selected yet! You have to set your study first using the `set_study()` function")
  }
  
  call_url <- get_brapi_url("get_study_info")
  call_url <- sub("\\{studyDbId\\}", qbms_globals$state$study_db_id, call_url)

  study_info <- brapi_get_call(call_url)
  
  if (is.null(study_info)) {
    study_info_df <- NULL
  } else {
    study_info_df <- as.data.frame(t(as.matrix(do.call(c, unlist(study_info, recursive = FALSE)))))
  }

  return(study_info_df)
}


#' Get the Observations Data of the Current Active Study
#'
#' @description
#' Retrieves the observations data (e.g., measurements, variables) for the active study, 
#' which must be set using the \code{\link{set_study}} function.
#'
#' @return
#' A data frame containing the observation data for the active study, or \code{NULL} 
#' if no data is available.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, 
#' \code{\link{set_trial}}, \code{\link{set_study}} for related study operations.
#'
#' @examples
#' if (interactive()) {
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'   login_bms()
#'   set_crop("wheat")
#'   set_program("Wheat International Nurseries")
#'   set_trial("IDYT39")
#'   set_study("IDYT39 Environment Number 9")
#'   data <- get_study_data()
#'   head(data)
#' }
#' 
#' @export

get_study_data <- function() {
  if (is.null(qbms_globals$state$study_db_id)) {
    stop("No study has been selected yet! You have to set your study first using the `set_study()` function")
  }
  
  call_url <- get_brapi_url("get_study_data")
  call_url <- sub("\\{studyDbId\\}", qbms_globals$state$study_db_id, call_url)

  study_result <- brapi_get_call(call_url)
  
  if (qbms_globals$config$brapi_ver == "v1") {
    qbms_globals$state$observationVariableDbIds <- study_result$observationVariableDbIds
  } else if (qbms_globals$config$brapi_ver == "v2") {
    qbms_globals$state$observationVariableDbIds <- study_result$observationVariables$observationVariableDbId
  }
  
  study_data   <- as.data.frame(study_result$data)
  
  if (qbms_globals$config$brapi_ver == "v1") {
    study_header <- c(study_result$headerRow, 
                      study_result$observationVariableNames)
    
  } else if (qbms_globals$config$brapi_ver == "v2") {
    study_header <- c(study_result$headerRow, 
                      study_result$observationVariables$observationVariableName)
  }
  
  if (nrow(study_data) > 0) {
    colnames(study_data) <- study_header
  } else {
    study_data <- NULL
  }

  return(study_data)
}


#' Get the Germplasm List of the Current Active Study
#'
#' @description
#' Retrieves the list of germplasm (genetic material) used in the currently active study, 
#' which must be set using the \code{\link{set_study}} function.
#'
#' @return
#' A data frame containing the germplasm list for the active study.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, 
#' \code{\link{set_trial}}, \code{\link{set_study}} for related operations on crops and studies.
#'
#' @examples
#' if (interactive()) {
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'   login_bms()
#'   set_crop("wheat")
#'   set_program("Wheat International Nurseries")
#'   set_trial("IDYT39")
#'   set_study("IDYT39 Environment Number 9")
#'   germplasm <- get_germplasm_list()
#'   head(germplasm)
#' }
#' 
#' @export

get_germplasm_list <- function() {
  if (is.null(qbms_globals$state$trial_db_id)) {
    stop("No trial has been selected yet! You have to set your trial first using the `set_trial()` function")
  }
  
  if (qbms_globals$config$brapi_ver == "v2" & is.null(qbms_globals$state$study_db_id)) {
    stop("No study has been selected yet! You have to set your study first using the `set_study()` function")
  }

  call_url <- get_brapi_url("get_germplasm_list")
  call_url <- sub("\\{studyDbId\\}", qbms_globals$state$study_db_id, call_url)

  germplasm_list <- brapi_get_call(call_url, nested = FALSE)$data

  if (qbms_globals$config$engine == "ebs") {
    germplasm_list$check <- 0
    
    nested_lists <- c("synonyms", "donors", "externalReferences", "germplasmOrigin",
                      "storageTypes", "taxonIds", "documentationURL", "additionalInfo")

    germplasm_list[, nested_lists] <- NULL
    germplasm_list <- germplasm_list[, colSums(is.na(germplasm_list)) != nrow(germplasm_list)]
  }
  
  if (qbms_globals$config$engine == "breedbase") {
    germplasm_list$check <- NA
    germplasm_list[, c("synonyms")] <- list(NULL)
  }
  
  if (nrow(germplasm_list) > 0 & qbms_globals$config$engine == "bms") {
    # BMS POST /crops/{cropName}/programs/{programUUID}/studies/{studyId}/entries to extract entry type (test or check)
    call_url <- paste0(qbms_globals$config$base_url, "/crops/", qbms_globals$config$crop,
                       "/programs/", qbms_globals$state$program_db_id,
                       "/studies/", qbms_globals$state$trial_db_id, "/entries")

    req <- httr2::request(utils::URLencode(call_url))
    req <- httr2::req_method(req, "POST")
    req <- httr2::req_timeout(req, qbms_globals$config$time_out)
    req <- httr2::req_headers(req, "Accept-Encoding" = "gzip, deflate")
    req <- httr2::req_headers(req, "X-Auth-Token" = qbms_globals$state$token)

    response <- httr2::req_perform(req)
    results  <- jsonlite::fromJSON(httr2::resp_body_string(response), flatten = TRUE)

    germplasm_list <- merge(germplasm_list, results[, c("entryNumber", "properties.8255.value", "gid")], by = "entryNumber")

    germplasm_list$check <- ifelse(germplasm_list$properties.8255.value == "C", 1, 0)

    germplasm_list[, c("synonyms", "typeOfGermplasmStorageCode", "taxonIds", "donors", "properties.8255.value")] <- list(NULL)
  }

  return(germplasm_list)
}


#' Get the Observations Data of the Current Active Trial
#'
#' @description
#' Retrieves the combined observations data (including all studies) for the current 
#' active trial, as configured in the internal state object using the \code{\link{set_trial}} function. 
#' This function iterates over all studies within the active trial and aggregates 
#' their observation data.
#'
#' @return
#' A data frame containing the combined observations data from all studies in the active trial.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{set_trial}}
#'
#' @examples
#' if (interactive()) {
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'   login_bms()
#'   set_crop("wheat")
#'   set_program("Wheat International Nurseries")
#'   set_trial("IDYT39")
#'   MET <- get_trial_data()
#'   head(MET)
#' }
#' 
#' @export

get_trial_data <- function() {
  env <- list_studies()

  for (i in env$studyName) {
    set_study(i)
    study_data <- get_study_data()
    
    if (!exists('trial_data')) {
      trial_data <- study_data
    } else {
      trial_data <- rbindx(trial_data, study_data)
    }
  }

  return(trial_data)
}


#' Get the Traits Ontology/Metadata of the Current Active Trial
#'
#' @description
#' Retrieves the traits ontology or metadata for the current active trial, which 
#' includes detailed information about the observation variables used in the trial.
#'
#' @return
#' A data frame containing the traits ontology or metadata, filtered by the observation 
#' variables used in the current trial.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{set_trial}}, 
#' \code{\link{get_study_data}} for retrieving study observations.
#'
#' @examples
#' if (interactive()) {
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'   login_bms()
#'   set_crop("wheat")
#'   set_program("Wheat International Nurseries")
#'   set_trial("IDYT39")
#'   ontology <- get_trial_obs_ontology()
#' }
#' 
#' @export

get_trial_obs_ontology <- function() {
  if (is.null(qbms_globals$state$observationVariableDbIds)) {
    stop("No data has been imported yet! You have to set get study data first using the `get_study_data()` function")
  }

  if (!is.null(qbms_globals$state$variables)) {
    ontology <- qbms_globals$state$variables
  } else {
    call_url <- get_brapi_url("get_trial_obs_ontology")
    
    if (qbms_globals$config$brapi_ver == "v1") {
      ontology <- brapi_get_call(call_url)$data
      
      ontology <- ontology[ontology$observationVariableDbId %in% qbms_globals$state$observationVariableDbIds, ]

      qbms_globals$state$variables <- ontology
    } else {
      call_body <- paste0('{"observationVariableDbIds": [', 
                          paste0('"', paste0(qbms_globals$state$observationVariableDbIds, collapse = '","'), '"'),
                          ']}')
      
      results <- brapi_post_search_call(call_url, call_body, FALSE)
      
      ontology <- as.data.frame(results$result$data)

      qbms_globals$state$variables <- ontology
    }
  }

  return(ontology)
}


#' Get the List of Locations Information of the Current Selected Crop
#'
#' @description
#' Retrieves a list of locations associated with the current active crop, as 
#' configured in the internal state object using the \code{\link{set_crop}} function.
#'
#' @return
#' A data frame containing information about locations relevant to the current crop.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}} for related crop operations.

list_locations <- function() {
  if (is.null(qbms_globals$config$crop)) {
    stop("No crop has been selected yet! You have to set your crop first using the `set_crop()` function")
  }
  
  if (!is.null(qbms_globals$state$locations)) {
    location_list <- qbms_globals$state$locations
  } else {
    call_url <- get_brapi_url("list_locations")

    location_list <- brapi_get_call(call_url, FALSE)$data

    qbms_globals$state$locations <- location_list
  }

  return(location_list)
}


#' Get the List of Trials, Studies, and Locations Information for the Current Selected Program
#'
#' @description
#' Retrieves comprehensive information about the trials, studies, and environments/locations
#' within the current active breeding program, as configured in the internal state object using 
#' the \code{\link{set_program}} function. This includes test and check entry counts for each study.
#'
#' @return
#' A data frame containing detailed information for each study within the program's trials, 
#' including trial names, study names, location information, and entry counts.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#'
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}
#'
#' @examples
#' if (interactive()) {
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'   login_bms()
#'   set_crop("wheat")
#'   set_program("Wheat International Nurseries")
#'   program_studies <- get_program_studies()
#'   head(program_studies)
#' }
#'
#' @export

get_program_studies <- function() {
  if (qbms_globals$config$engine != "bms") {
    stop("This function is not supported yet in this database!")
  }

  if (is.null(qbms_globals$state$program_db_id)) {
    stop("No breeding program has been selected yet! You have to set your breeding program first using the `set_program()` function")
  }

  all_trials <- get_program_trials()
  program_trials <- all_trials[all_trials$programDbId == qbms_globals$state$program_db_id, ]

  colnames(program_trials) <- gsub("additionalInfo.", "", colnames(program_trials))

  for (row in 1:nrow(program_trials)) {
    trial <- program_trials[row, ]
    trial_studies <- rbindlistx(program_trials[row, "studies"])
    if (nrow(trial_studies) > 0) {
      if (row == 1) {
        studies <- cbind(trial, trial_studies, row.names = NULL)
      } else {
        studies <- rbind(studies, cbind(trial, trial_studies, row.names = NULL))
      }
    }
  }

  # remove locationDbId, active, studies, and locationName columns coming from the trial data.frame
  studies <- studies[, -c(6:8, ncol(studies))]

  crop_locations <- list_locations()

  studies <- merge(studies, crop_locations, by = "locationDbId", all.x = TRUE, all.y = FALSE)

  studies$testEntriesCount <- 0

  crop_url <- paste0(qbms_globals$config$base_url, "/crops/", qbms_globals$config$crop)
  
  all_trials <- unique(studies$trialDbId)
  num_trials <- length(all_trials)
  
  pb <- utils::txtProgressBar(min = 0, max = num_trials, initial = 0, style = 3) 
  pb_step <- round(num_trials/100)

  for (i in 1:num_trials) {
    call_url <- paste0(crop_url, "/programs/", qbms_globals$state$program_db_id, "/studies/", all_trials[i], "/entries/metadata")

    req <- httr2::request(utils::URLencode(call_url))
    req <- httr2::req_method(req, "GET")
    req <- httr2::req_timeout(req, qbms_globals$config$time_out)
    req <- httr2::req_headers(req, "Accept-Encoding" = "gzip, deflate")
    req <- httr2::req_headers(req, "X-Auth-Token" = qbms_globals$state$token)

    response <- httr2::req_perform(req)
    metadata <- jsonlite::fromJSON(httr2::resp_body_string(response), flatten = TRUE)
    
    studies[studies$trialDbId == all_trials[i], "testEntriesCount"] <- metadata$testEntriesCount
    studies[studies$trialDbId == all_trials[i], "checkEntriesCount"] <- metadata$checkEntriesCount
    
    utils::setTxtProgressBar(pb, i)
  }
  
  utils::setTxtProgressBar(pb, num_trials)
  close(pb)

  return(studies)
}


#' Get Germplasm ID for a Specified Germplasm Name
#'
#' @description 
#' Retrieves the unique germplasm ID associated with the specified germplasm name for the current active crop.
#'
#' @param germplasm_name The name of the germplasm.
#' 
#' @return 
#' A string representing the germplasm's unique ID (germplasmDbId).
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso 
#' \code{\link{set_crop}}, \code{\link{get_germplasm_data}}, \code{\link{get_germplasm_attributes}}

get_germplasm_id <- function(germplasm_name = "") {
  if (germplasm_name == "") {
    stop("The germplasm name parameter value is missing!")
  }
  
  if (is.null(qbms_globals$config$crop)) {
    stop("No crop has been selected yet! You have to set your crop first using the `set_crop()` function")
  }
  
  call_url <- get_brapi_url("get_germplasm_id")
  call_url <- sub("\\{germplasmName\\}", germplasm_name, call_url)

  # this BrAPI call return all germplasm records start with the given name NOT exactly match!
  results <- brapi_get_call(call_url, FALSE)$data
  
  if (length(results) == 0) {
    stop("No germplasm in this crop database start with your filtering name!")
  }
  
  germplasm_db_id <- results[results$germplasmName == germplasm_name, "germplasmDbId"]
  
  if (length(germplasm_db_id) == 0) {
    stop("No germplasm in this crop database match your filtering name!")
  }
  
  return(germplasm_db_id)
}


#' Retrieve Observations Data for a Specified Germplasm.
#'
#' @description
#' Retrieves all available observations data for the given germplasm in the current active crop.
#' This data is aggregated across all trials in the crop database.
#'
#' @param germplasm_name The name of the germplasm.
#' 
#' @return 
#' A data frame containing all available observations data for the specified germplasm.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso 
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{get_germplasm_attributes}}
#' 
#' @examples
#' if (interactive()) {
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'   login_bms()
#'   set_crop("wheat")
#'   germplasm_observations <- get_germplasm_data("Jabal")
#'   head(germplasm_observations)
#' }
#' 
#' @export

get_germplasm_data <- function(germplasm_name = "") {
  if (qbms_globals$config$engine != "bms") {
    stop("This function is not supported yet in this database!")
  }
  
  germplasm_db_id <- get_germplasm_id(germplasm_name)

  # https://github.com/plantbreeding/API/blob/V1.2/Specification/Phenotypes/PhenotypesSearch_POST.md
  # Note 1: It does not work with germplasm name (BrAPI specifications): e.g. {"germplasmDbIds": ["ILC 3279"]}
  # Note 2: Return "Invalid request body" if we search for one germplasm_db_id!

  crop_url  <- paste0(qbms_globals$config$base_url, "/", qbms_globals$config$crop, "/brapi/v1")
  call_url  <- paste0(crop_url, "/phenotypes-search")
  call_body <- list(germplasmDbIds = c(germplasm_db_id, ""), observationLevel = "PLOT")

  req <- httr2::request(utils::URLencode(call_url))
  req <- httr2::req_method(req, "POST")
  req <- httr2::req_body_json(req, call_body)
  req <- httr2::req_timeout(req, qbms_globals$config$time_out)
  req <- httr2::req_headers(req, "Accept-Encoding" = "gzip, deflate")
  req <- httr2::req_headers(req, "Authorization" = paste0("Bearer ", qbms_globals$state$token))

  response <- httr2::req_perform(req)
  flatten_results <- jsonlite::fromJSON(httr2::resp_body_string(response), flatten = TRUE)$result$data

  if (length(flatten_results) == 0) {
    stop("No observation data available in this crop database for the given germplasm!")
  }

  unlisted_observations    <- rbindx(flatten_results$observations[[1]])
  unlisted_observations$id <- 1

  for (i in 2:length(flatten_results$observations)) {
    obs <- rbindx(flatten_results$observations[[i]])
    if (nrow(obs) == 0) next
    obs$id <- i
    unlisted_observations <- rbind(unlisted_observations, obs)
  }

  # create same id in remaining data frame
  flatten_results$id <- seq.int(nrow(flatten_results))

  # join data frame with unlisted list
  flatten_results <- merge(flatten_results, unlisted_observations, by = "id", all.x = TRUE)

  # get rid of unnecessary columns
  flatten_results$observations <- NULL
  flatten_results$id <- NULL

  # we still need to filter out unnecessary columns
  results_df <- data.frame(matrix(nrow = dim(flatten_results)[1], ncol = dim(flatten_results)[2]))
  colnames(results_df) <- colnames(flatten_results)

  for (i in 1:ncol(flatten_results)) {
    temp <- flatten_results[, i]
    temp[sapply(temp, function(x) { return(length(x) == 0) })] <- NA
    temp[sapply(temp, is.null)] <- NA
    results_df[, i] <- unlist(temp)
  }

  crop_locations <- list_locations()
  results_df <- merge(results_df, crop_locations, by.x = "studyLocationDbId", by.y = "locationDbId", all.x = TRUE)

  return(results_df)
}


#' Retrieve Attributes for a Specified Germplasm
#'
#' @description
#' Retrieves a detailed list of attributes for a given germplasm, such as its origin, donors, and taxonomic information.
#'
#' @param germplasm_name The name of the germplasm.
#' 
#' @return 
#' A data frame containing the attributes associated with the specified germplasm.
#' 
#' @author 
#' Johan Steven Aparicio, \email{j.aparicio@cgiar.org}
#' 
#' @seealso 
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{get_germplasm_data}}
#' 
#' @examples
#' if (interactive()) {
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'   login_bms()
#'   set_crop("wheat")
#'   germplasm_attributes <- get_germplasm_attributes("Jabal")
#' }
#' 
#' @export

get_germplasm_attributes <- function(germplasm_name = "") {
  germplasm_db_id <- get_germplasm_id(germplasm_name)
  
  if (length(germplasm_db_id) > 1) { germplasm_db_id <- germplasm_db_id[1] }
  
  call_url <- get_brapi_url("get_germplasm_attributes")
  call_url <- sub("\\{germplasmDbId\\}", germplasm_db_id, call_url)
  
  results <- brapi_get_call(call_url)$data

  return(results)
}
