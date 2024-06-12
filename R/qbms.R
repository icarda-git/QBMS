
#' Get the List of Supported Crops
#'
#' @description
#' Retrieves the list of supported crops.
#'
#' @return
#' A list of supported crops.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}
#' 
#' @examples
#' if(interactive()) {
#'   # Configure your BMS connection
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'
#'   # Login using your BMS account (interactive mode)
#'   # You can pass the BMS username and password as parameters (batch mode)
#'   login_bms()
#'
#'   # List supported crops in the BMS server
#'   list_crops()
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
#' Updates the current active crop in the internal configuration object (including 
#' the BrAPI connection object).
#'
#' @param crop_name The name of the crop.
#' 
#' @return
#' No return value.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{list_crops}}
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
#' }
#' 
#' @export

set_crop <- function(crop_name) {
  valid_crops <- list_crops()

  if (!crop_name %in% valid_crops) {
    stop("Your crop name is not supported in this connected BMS server! You may use the `list_crops()` function to check the available crops")
  }

  qbms_globals$config$crop <- crop_name
  
  qbms_globals$state$programs  <- NULL
  qbms_globals$state$locations <- NULL
  qbms_globals$state$variables <- NULL
}


#' Get the List of Breeding Programs Names
#'
#' @description
#' Retrieves the breeding programs list from the current active crop as configured 
#' in the internal configuration object using `set_crop()` function.
#'
#' @return
#' A list of breeding programs names.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}
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
#'   # List existing breeding programs
#'   list_programs()
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
    
    if (qbms_globals$config$engine == "bms") {
      bms_programs <- results[c("name")]
      colnames(bms_programs) <- c("programName")
    } else {
      bms_programs <- results[c("programName")]
    }

    qbms_globals$state$programs <- cbind(bms_programs, results[c("programDbId")])
  }
  
  return(subset(qbms_globals$state$programs, select = "programName"))
}


#' Set the Current Active Breeding Program
#'
#' @description
#' Updates the current active breeding program in the internal state object 
#' using the programDbId which is associated with the given program_name parameter.
#'
#' @param program_name The name of the breeding program.
#' 
#' @return
#' No return value.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{list_programs}}
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


#' Internal Function Used to Retrieve the Rough List of Trials
#'
#' @description
#' This function is created for *internal use only* to retrieve the raw list of trials
#' from the pre-selected (i.e., currently active) crop and breeding program combination
#' as already configured in the internal state object using `set_crop()` and `set_program()`
#' functions, respectively.
#'
#' @return
#' A list of trials information.
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


#' Get the List of Trials in the Current Active Breeding Program
#'
#' @description
#' Retrieves the trials list from the current active breeding program as configured 
#' in the internal state object using `set_program()` function.
#'
#' @param year The starting year to filter the list of trials (optional, default is NULL).
#' 
#' @return
#' A list of trials names.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}
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
#'   # List all studies/trials in the selected program
#'   list_trials()
#'
#'   # Filter listed studies/trials by year
#'   list_trials(2022)
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

  # startDate format in bms_trials is "yyyy-mm-dd"
  if (!is.null(year)) {
    bms_trials <- bms_trials[gsub("-\\d{2}-\\d{2}", "", bms_trials$startDate) == year, ]
  }

  trials <- unique(bms_trials[c("trialName")])

  if (length(trials$trialName) == 0) {
    warning("No single trial fit your query parameters!")
    trials <- NA
  }

  return(trials)
}


#' Set the Current Active Trial
#'
#' @description
#' Updates the current active trial in the internal state object using the 
#' trialDbId which is associated with the given trial_name parameter.
#'
#' @param trial_name The name of the trial.
#' 
#' @return
#' No return value.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{list_trials}}
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
#'   # Select a specific study/trial by name
#'   set_trial("IDYT39")
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
#' Retrieves the studies list from the current active trial as configured in the 
#' internal state object using `set_trial()` function.
#'
#' @return
#' A list of study and location names.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{set_trial}}
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
#'   # Select a specific study/trial by name
#'   set_trial("IDYT39")
#'
#'   # List all environments/locations information in the selected study/trial
#'   list_studies()
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
    
    # handle the case of BreedBase trials (studies) listed in the root program folder (trial)
    if (qbms_globals$config$engine == "breedbase" && qbms_globals$state$trial_db_id == qbms_globals$state$program_db_id) {
      call_url <- sub("\\?trialDbId\\=", '?programDbId=', call_url)
    }

    bms_trial_studies <- brapi_get_call(call_url, FALSE)$data
    
    # handle the case of BreedBase trials (studies) listed in the root program folder (trial)
    if (qbms_globals$config$engine == "breedbase" && qbms_globals$state$trial_db_id == qbms_globals$state$program_db_id) {
      bms_trial_studies <- bms_trial_studies[is.na(bms_trial_studies$trialName), ]
      rownames(bms_trial_studies) <- NULL
    }
    
    if (nrow(bms_trial_studies) == 0) {
      stop("No studies in the selected trial! Please check what you have set in the `set_trial()` function")
    }
    
    studies <- bms_trial_studies[, c("studyName", "locationName", "studyDbId")]

    qbms_globals$state$studies <- studies
  }
  
  return(studies[, c("studyName", "locationName")])
}


#' Set the Current Active Study
#'
#' @description
#' Updates the current active study in the internal state object using the 
#' studyDbId, which is associated with the given study_name 
#' parameter.
#'
#' @param study_name The name of the study.
#' 
#' @return
#' No return value.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{set_trial}}, \code{\link{list_studies}}
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
#'   # Select a specific study/trial by name
#'   set_trial("IDYT39")
#'
#'   # Select a specific environment/location dataset
#'   set_study("IDYT39 Environment Number 9")
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
#' Retrieves the details/metadata of the current active study as configured in 
#' the internal state object using `set_study()` function.
#'
#' @return
#' A data frame of the study details/metadata.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{set_trial}}, \code{\link{set_study}}
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
#'   # Select a specific study/trial by name
#'   set_trial("IDYT39")
#'
#'   # Select a specific environment/location dataset
#'   set_study("IDYT39 Environment Number 9")
#'
#'   # Retrieve the general information of the selected environment/location
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
#' Retrieves the observations data of the current active study as configured in 
#' the internal state object using `set_study()` function.
#'
#' @return
#' A data frame of the study observations data.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{set_trial}}, \code{\link{set_study}}
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
#'   # Select a specific study/trial by name
#'   set_trial("IDYT39")
#'
#'   # Select a specific environment/location dataset
#'   set_study("IDYT39 Environment Number 9")
#'
#'   # Retrieve the data of the selected environment/location
#'   data <- get_study_data()
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
  
  if (qbms_globals$config$engine == "breedbase") {
    study_header <- study_data[1, ]
    study_data   <- study_data[-1, ]
    
  } else if (qbms_globals$config$brapi_ver == "v1") {
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
#' Retrieves the germplasm list of the current active study as configured in the 
#' internal state object using `set_study()` function.
#'
#' @return
#' A data frame of the study germplasm list.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{set_trial}}, \code{\link{set_study}}
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
#'   # Select a specific study/trial by name
#'   set_trial("IDYT39")
#'
#'   # Select a specific environment/location dataset
#'   set_study("IDYT39 Environment Number 9")
#'
#'   # Retrieve the germplasm list of the selected environment/location
#'   germplasm <- get_germplasm_list()
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

    response <- httr::POST(url = utils::URLencode(call_url), body = "", encode = "json",
                           httr::add_headers(c("X-Auth-Token" = qbms_globals$state$token), "Accept-Encoding" = "gzip, deflate"),
                           httr::timeout(qbms_globals$config$time_out))

    results <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)

    germplasm_list <- merge(germplasm_list, results[, c("entryNumber", "properties.8255.value", "gid")], by = "entryNumber")

    germplasm_list$check <- ifelse(germplasm_list$properties.8255.value == 10180, 1, 0)

    germplasm_list[, c("synonyms", "typeOfGermplasmStorageCode", "taxonIds", "donors", "properties.8255.value")] <- list(NULL)
  }

  return(germplasm_list)
}


#' Get the Observations Data of the Current Active Trial
#'
#' @description
#' Retrieves the observations data of the current active trial (i.e., including 
#' all studies within) as configured in the internal state object using 
#' `set_trial()` function.
#'
#' @return
#' A data frame of the trial observations data.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{set_trial}}
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
#'   # Select a specific study/trial by name
#'   set_trial("IDYT39")
#'
#'   # Retrieve multi-environment trial data
#'   MET <- get_trial_data()
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
#' Retrieves the traits ontology/metadata of the current active trial as 
#' configured in the internal state object using the `set_trial()` function.
#'
#' @return
#' A data frame of the traits ontology/metadata.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{set_trial}}
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
#'   # Select a specific study/trial by name
#'   set_trial("IDYT39")
#'
#'   # Get observation variable ontology
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
#' Retrieves the locations information of the current active crop as configured 
#' in the internal state object using the `set_crop()` function.
#'
#' @return
#' A data frame of the locations information.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}

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


#' Get the List of Trials/Studies/Locations Information of the Current Selected Program
#'
#' @description
#' Retrieves all environments/locations information of the trials studies in the 
#' current active program as configured in the internal state object using the 
#' `set_program()` function.
#'
#' @return
#' A data frame of locations information for each study in the program trials.
#'
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#'
#' @seealso
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}
#'
#' @examples
#' if(interactive()) {
#'   # config your server connection
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'
#'   # login using your account (interactive mode)
#'   # you can pass your username and password as parameters (batch mode)
#'   login_bms()
#'
#'   # Select a crop by name
#'   set_crop("wheat")
#'
#'   # select a breeding program by name
#'   set_program("Wheat International Nurseries")
#'
#'   # retrieve all environments/locations information in the selected program studies/trials
#'   program_studies <- get_program_studies()
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

    response <- httr::GET(url = utils::URLencode(call_url),
                          httr::add_headers("X-Auth-Token" = qbms_globals$state$token, "Accept-Encoding" = "gzip, deflate"),
                          httr::timeout(qbms_globals$config$time_out))
    
    metadata <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)

    studies[studies$trialDbId == all_trials[i], "testEntriesCount"] <- metadata$testEntriesCount
    studies[studies$trialDbId == all_trials[i], "checkEntriesCount"] <- metadata$checkEntriesCount
    
    # update the progress bar
    utils::setTxtProgressBar(pb, i)
  }
  
  utils::setTxtProgressBar(pb, num_trials)
  close(pb)

  return(studies)
}


#' Get Germplasm ID
#'
#' @description 
#' Retrieves the germplasm ID for the given germplasm name in the current crop.
#'
#' @param germplasm_name The name of the germplasm.
#' 
#' @return 
#' A string of the germplasm ID.
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
#' Retrieves all available observations data for a given germplasm in the current 
#' crop database, regardless of the nested structure of programs/trials.
#'
#' @param germplasm_name The name of the germplasm.
#' 
#' @return 
#' A data frame containing the aggregated observations data for the germplasm from all trials.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso 
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{get_germplasm_attributes}}
#' 
#' @examples
#' if (interactive()) {
#'   # Configure server connection
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'   
#'   # Log in using your account (interactive mode)
#'   # You can pass your username and password as parameters (batch mode)
#'   login_bms()
#'   
#'   # Select a crop
#'   set_crop("wheat")
#'   
#'   # Select a breeding program by name
#'   set_program("Wheat International Nurseries")
#'   
#'   # Retrieve observations data for a specified germplasm aggregated from all trials
#'   germplasm_observations <- get_germplasm_data("Jabal")
#' }
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
  auth_code <- paste0("Bearer ", qbms_globals$state$token)

  response <- httr::POST(url = utils::URLencode(call_url), body = call_body, encode = "json",
                         httr::add_headers(c("Authorization" = auth_code, "Accept-Encoding" = "gzip, deflate")),
                         httr::timeout(qbms_globals$config$time_out))

  results <- httr::content(response)$result$data

  flatten_results <- jsonlite::fromJSON(jsonlite::toJSON(results), flatten = TRUE)
  
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
#' @param germplasm_name The name of the germplasm.
#' 
#' @return 
#' A data frame containing the attributes of the specified germplasm.
#' 
#' @author 
#' Johan Steven Aparicio, \email{j.aparicio@cgiar.org}
#' 
#' @seealso 
#' \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{get_germplasm_data}}
#' 
#' @examples
#' if (interactive()) {
#'   # Configure your server connection
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'
#'   # Log in using your account (interactive mode)
#'   # You can pass your username and password as parameters (batch mode)
#'   login_bms()
#'
#'   # Select a crop
#'   set_crop("wheat")
#'
#'   # Select a breeding program by name
#'   set_program("Wheat International Nurseries")
#'
#'   # Retrieve attribute data of a specified germplasm in a crop
#'   germplasm_attributes <- get_germplasm_attributes("Jabal")
#' }
#' @export

get_germplasm_attributes <- function(germplasm_name = "") {
  germplasm_db_id <- get_germplasm_id(germplasm_name)
  
  if (length(germplasm_db_id) > 1) { germplasm_db_id <- germplasm_db_id[1] }
  
  call_url <- get_brapi_url("get_germplasm_attributes")
  call_url <- sub("\\{germplasmDbId\\}", germplasm_db_id, call_url)
  
  results <- brapi_get_call(call_url)$data

  return(results)
}
