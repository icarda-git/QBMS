# Name:     qbms.R
# Purpose:  Set of functions to query BMS by a wrapper using BrAPI calls
# Author:   Khaled Al-Shamaa <k.el-shamaa@cgiar.org>
# Version:  0.3
#
# Revision: 
#           v0.1 - 24 Jul 2019 
#                * Initial version.
#
#           v0.2 - 20 Aug 2019 
#                * Adopt tidyverse style guide https://style.tidyverse.org/
#                * Add functions documentation using roxygen2 format.
#                * Add basic error handling to the functions.
#                * Add a function to retrieve the traits ontology of a trial.
#
#           v0.3 - 2 Jun 2020
#                * Call BrAPI directly (i.e. not required "CIP-RIU/brapi" from GitHub anymore).
#                * Add a function to get all data of the current active trial (combined all studies).
#                * Add a function to get a list of studies where given germplasm has been used.
#                * Add a function to get a specific germplasm data from all program trials.
#                * Handle BrAPI pagination in a proper way.
#
#           v0.3.1 - 9 Jun 2020
#                * Fix the "get_trial_data" function bug when you have more than one study in the same location. 
#                * Function "list_studies" returns studyName also, and function "set_study" input is studyName now.
#                * Simplify the "get_germplasm_list" function output by getting rid of nested lists.
#                * Deprecate the "list_all_studies" function in favor of "get_program_studies" function.
#
#           v0.4 - 3 Jul 2020
#                * Convert it into an R package.
#                * Add set_qbms_config function to setup connection configuration variables.
#                * Use the double colon approach for functions from external packages.
#                * Fix the deprecated API call in the get_trial_obs_ontology function.
#
#           v0.4.1 - 16 Oct 2020
#                * Simplify configuration by required only the URL of the BMS login page.
#                * Improve the performance of the internal get_program_trials function by passing the programDbId in the /trials GET call.
#                * Add debug_qbms function to get the internal config/state object.
#
# License:  GPLv3

# Load/install required packages
# if (!require(httr)) install.packages("httr")
# if (!require(tcltk)) install.packages("tcltk")
# if (!require(jsonlite)) install.packages("jsonlite")
# if (!require(plyr)) install.packages("plyr")
# if (!require(dplyr)) install.packages("dplyr")
# if (!require(data.table)) install.packages("data.table")

# Internal state variables/lists
qbms_globals <- new.env()
qbms_globals$config <- list(crop = NULL)
qbms_globals$state  <- list(token = NULL)


#' Debug internal QBMS status object
#' 
#' @description
#' Return the internal QBMS status object for debuging 
#' 
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @examples
#' View(debug_qbms())
#' @export

debug_qbms <- function(){
  return(qbms_globals)
}

#' Configure BMS server settings
#' 
#' @description
#' Set the connection configuration of the BMS server 
#' 
#' @param url       URL of the BMS login page (default is "http://localhost/ibpworkbench/")
#' @param path      BMS API path (default is "bmsapi")
#' @param page_size Page size (default is 1000)
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @examples
#' set_qbms_config("https://bms.example.com/ibpworkbench/controller/auth/login")
#' @export

set_qbms_config <- function(url = "http://localhost/ibpworkbench/controller/auth/login",
                            path = "bmsapi", page_size = 1000){

  qbms_globals$config$server    <- strsplit(url, "ibpworkbench")[[1]][1]
  qbms_globals$config$path      <- path
  qbms_globals$config$page_size <- page_size
  qbms_globals$config$base_url  <- paste0(qbms_globals$config$server, qbms_globals$config$path)
}


#' Internal function used for core BrAPI GET calls
#' 
#' @description
#' This function created for *internal use only* to cal BrAPI in GET method and 
#' retrieve the rough response data and send back the results. This function take
#' care of pagination, authintication, encoding, compress, decode JSON response, etc.
#' 
#' @param call_url BrAPI URL to call in GET method
#' @param page     Page number to retrieve in case of multi paged results (default is 0)
#' @param nested   If FLASE, then retrived JSON data will be flatten (default is TRUE)
#' @return result object returned by JSON API response
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}

brapi_get_call <- function(call_url, page = 0, nested = TRUE){
  separator <- if (grepl("\\?", call_url)) "&" else "?"
  full_url  <- paste0(call_url, separator, "page=", page, "&pageSize=", qbms_globals$config$page_size)

  auth_code <- paste0("Bearer ", qbms_globals$state$token)
  headers   <- c("Authorization" = auth_code, "Accept-Encoding" = "gzip, deflate")

  response  <- httr::GET(utils::URLencode(full_url), httr::add_headers(headers))

  result_object <- jsonlite::fromJSON(httr::content(response, as = "text"), flatten = !nested)
  result_info   <- result_object$result
  
  qbms_globals$state$total_pages <- result_object$metadata$pagination$totalPages
  qbms_globals$state$errors      <- result_object$errors

  return(result_info)
}


#' Login pop-up window
#' 
#' Build a GUI pop-up window using Tcl/Tk to insert BMS username and password
#' 
#' @return a vector of inserted username and password
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}

get_login_details <- function() {
  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, "Login BMS Server")
  
  ss <- "Please enter your BMS login details"
  tcltk::tkgrid(tcltk::tklabel(tt, text = ss), columnspan = 2, padx = 50, pady = 10)
  
  usr <- tcltk::tclVar("")
  pwd <- tcltk::tclVar("")
  
  user_label <- tcltk::tklabel(tt, text = "Username:")
  pass_label <- tcltk::tklabel(tt, text = "Password:")
  
  user_input <- tcltk::tkentry(tt, width = "30", textvariable = usr)
  pass_input <- tcltk::tkentry(tt, width = "30", textvariable = pwd, show = "*")
  
  tcltk::tkgrid(user_label, user_input, sticky = "ew", padx = 5)
  tcltk::tkgrid(pass_label, pass_input, sticky = "ew", padx = 5)
  
  on_okay <- function() {
    tcltk::tkdestroy(tt)
  }
  
  ok_button <- tcltk::tkbutton(tt, text = " OK ", command = on_okay)
  tcltk::tkbind(pass_input, "<Return>", on_okay)
  tcltk::tkgrid(ok_button, columnspan = 2, pady = 5)
  
  tcltk::tkfocus(tt)
  tcltk::tkwait.window(tt)
  
  invisible(c(usr = tcltk::tclvalue(usr), pwd = tcltk::tclvalue(pwd)))
}


#' Login to the BMS server
#' 
#' @description
#' Connect to the BMS server. If username or password parameters are missing, 
#' then a login window will pop-up to insert username and password. 
#' 
#' All other connection parameters (i.e. server IP or domain, connection port, 
#' API path, and connection protocol e.g. http://) will retrieve from the 
#' qbms_config list.
#' 
#' This function will update both of the qbms_config list (brapi connection 
#' object in the con key) and qbms_state list (token value in the token key).
#' 
#' @param username the BMS username (optional, default is NULL)
#' @param password the BMS password (optional, default is NULL)
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @examples
#' # config your BMS connection
#' set_qbms_config("https://bms.example.com/ibpworkbench/controller/auth/login")
#' 
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#' @export

login_bms <- function(username = NULL, password = NULL) {
  qbms_globals$config$base_url <- paste0(qbms_globals$config$server, qbms_globals$config$path)
  
  if (is.null(username) || is.null(password)) {
    credentials <- get_login_details()
  } else {
    credentials <- c(usr = username, pwd = password)
  }

  call_url  <- paste0(qbms_globals$config$base_url, "/brapi/v1/token")
  call_body <- list(username = credentials["usr"], password = credentials["pwd"])
  
  response <- httr::POST(call_url, body=call_body, encode="json")

  if (!is.null(httr::content(response)$errors)) {
    stop(httr::content(response)$errors[[1]]$message)
  }
  
  qbms_globals$state$token <- httr::content(response)$access_token
  qbms_globals$state$user  <- httr::content(response)$userDisplayName
  qbms_globals$state$expires_in <- httr::content(response)$expires_in
}


#' Get the list of supported crops
#' 
#' @return a list of supported crops
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}
#' @examples
#' # config your BMS connection
#' set_qbms_config("https://bms.example.com/ibpworkbench/controller/auth/login")
#' 
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#' 
#' # list supported crops in the bms server
#' list_crops()
#' @export

list_crops <- function() {
  if (is.null(qbms_globals$state$token)) {
    stop("No BMS server has been connected yet! You have to connect a BMS server first using the `bms_login()` function")
  }
  
  call_url <- paste0(qbms_globals$config$base_url, "/brapi/v1/crops")
  
  bms_crops <- brapi_get_call(call_url)
  
  return(bms_crops$data)
}


#' Set the current active crop
#' 
#' @description
#' This function will update the current active crop in the internal 
#' configuration object (including the brapi connection object).
#' 
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}, \code{\link{list_crops}}
#' @examples
#' # config your BMS connection
#' set_qbms_config("https://bms.example.com/ibpworkbench/controller/auth/login")
#' 
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#' 
#' set_crop("Tutorial1")
#' @export

set_crop <- function(crop_name) {
  valid_crops <- list_crops()
  
  if (!crop_name %in% valid_crops) {
    stop("Your crop name is not supported in this connected BMS server! You may use the `list_crops()` function to check the available crops")
  }
  
  qbms_globals$config$crop <- crop_name
}


#' Get the list of breeding programs names
#' 
#' @description
#' This function will retrieve the breeding programs list from the current active 
#' crop as configured in the internal configuration object using `set_crop()`
#' function.
#' 
#' @return a list of breeding programs names
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}, \code{\link{set_crop}}
#' @examples
#' # config your BMS connection
#' set_qbms_config("https://bms.example.com/ibpworkbench/controller/auth/login")
#' 
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#' 
#' set_crop("Tutorial1")
#' 
#' # list existing breeding programs
#' list_programs()
#' @export

list_programs <- function() {
  if (is.null(qbms_globals$state$token)) {
    stop("No BMS server has been connected yet! You have to connect a BMS server first using the `bms_login()` function")
  }
  
  if (is.null(qbms_globals$config$crop)) {
    stop("No crop has been selected yet! You have to set your crop first using the `set_crop()` function")
  }
  
  call_url <- paste0(qbms_globals$config$base_url, "/", qbms_globals$config$crop, "/brapi/v1/programs")
  
  bms_programs <- brapi_get_call(call_url)
  
  return(bms_programs$data[c("name")])
}


#' Set the current active breeding program
#' 
#' @description
#' This function will update the current active breeding program in the 
#' internal state object using the programDbId retrieved from BMS which is 
#' associated to the given program_name parameter.
#' 
#' @param program_name the name of the breeding program
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{list_programs}}
#' @examples
#' # config your BMS connection
#' set_qbms_config("https://bms.example.com/ibpworkbench/controller/auth/login")
#' 
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#' 
#' set_crop("Tutorial1")
#' 
#' # select a breeding program by name
#' set_program("Training Breeding Program")
#' @export

set_program <- function(program_name) {
  valid_programs <- list_programs()
  
  if (!program_name %in% valid_programs$name) {
    stop("Your breeding program name is not exists in this crop database! You may use the `list_programs()` function to check the available breeding programs")
  }

  call_url <- paste0(qbms_globals$config$base_url, "/", qbms_globals$config$crop, "/brapi/v1/programs")
  
  bms_programs <- brapi_get_call(call_url)

  program_row <- which(bms_programs$data$name == program_name)
  
  qbms_globals$state$program_db_id <- bms_programs$data[program_row, "programDbId"]
}


#' Internal function used to retrive the rough list of trials
#' 
#' @description
#' This function created for *internal use only* to retrieve the rough list of trials 
#' from the pre-selected (i.e. currently active) crop and breeding program combination
#' as already configured in the internal state object using `set_crop()` and `set_program()` 
#' functions respectivily.
#' 
#' @return a list of trials information
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{list_trials}}

get_program_trials <- function() {
  call_url <- paste0(qbms_globals$config$base_url, "/", qbms_globals$config$crop, "/brapi/v1/trials?programDbId=", qbms_globals$state$program_db_id)
  
  bms_crop_trials <- brapi_get_call(call_url, 0, FALSE)
  
  bms_program_trials <- bms_crop_trials$data
  
  if (qbms_globals$state$total_pages > 1 && is.null(qbms_globals$state$errors)) {
    last_page <- qbms_globals$state$total_pages - 1
    for (n in 1:last_page) {
      bms_crop_trials    <- brapi_get_call(call_url, n, FALSE)
      bms_program_trials <- plyr::rbind.fill(bms_program_trials, bms_crop_trials$data)
    }
  }
  
  return(bms_program_trials)  
}


#' Get the list of trials in the current active breeding program 
#' 
#' @description
#' This function will retrieve the trials list from the current active breeding 
#' program as configured in the internal state object using `set_program()` 
#' function.
#' 
#' @param year the starting year to filter the list of trials (optional, default is NULL)
#' @return a list of trials names
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}
#' @examples
#' # config your BMS connection
#' set_qbms_config("https://bms.example.com/ibpworkbench/controller/auth/login")
#' 
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#' 
#' set_crop("Tutorial1")
#' 
#' # select a breeding program by name
#' set_program("Training Breeding Program")
#' 
#' # list all studies/trials in the selected program
#' list_trials()
#' 
#' # filter listed studies/trials by year
#' list_trials(2017)
#' @export

list_trials <- function(year = NULL) {
  if (is.null(qbms_globals$state$program_db_id)) {
    stop("No breeding program has been selected yet! You have to set your breeding program first using the `set_program()` function")
  }
  
  bms_trials <- get_program_trials()

  # startDate format in bms_trials is yyyymmdd
  if (!is.null(year)) {
    if (!is.numeric(year)) {
      stop("Year parameter if exists should be numeric")
    }
    
    from_date <- year * 10000
    to_date   <- year * 10000 + 1231
    
    bms_trials <- subset(bms_trials, startDate >= from_date & startDate <= to_date)
  }
  
  trials <- unique(bms_trials[c("trialName")])
  
  if (length(trials$trialName) == 0) {
    warning("No single trial fit your query parameters!")
    trials <- NA
  }
  
  return(trials)
}


#' Set the current active trial
#' 
#' @description
#' This function will update the current active trial in the internal state 
#' object using the trialDbId retrieved from BMS which is associated to the 
#' given trial_name parameter.
#' 
#' @param trial_name the name of the trial
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{list_trials}}
#' @examples
#' # config your BMS connection
#' set_qbms_config("https://bms.example.com/ibpworkbench/controller/auth/login")
#' 
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#' 
#' set_crop("Tutorial1")
#' 
#' # select a breeding program by name
#' set_program("Training Breeding Program")
#' 
#' # select a specific study/trial by name
#' set_trial("CIDTN-2016")
#' @export

set_trial <- function(trial_name) {
  valid_trials <- list_trials()
  
  if (!trial_name %in% valid_trials$trialName) {
    stop("Your trial name is not exists in this breeding program!  You may use the `list_trials()` function to check the available trials")
  }
  
  bms_trials <- get_program_trials()
  
  trial_row <- which(bms_trials$trialName == trial_name)[1]
  
  qbms_globals$state$trial_db_id <- as.character(bms_trials[trial_row, c("trialDbId")])
}


#' Get the list of studies in the current active trial
#' 
#' @description
#' This function will retrieve the studies list from the current active trial 
#' as configured in the internal state object using `set_trial()` function.
#' 
#' @return a list of study and location names
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{set_trial}}
#' @examples
#' # config your BMS connection
#' set_qbms_config("https://bms.example.com/ibpworkbench/controller/auth/login")
#' 
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#' 
#' set_crop("Tutorial1")
#' 
#' # select a breeding program by name
#' set_program("Training Breeding Program")
#' 
#' # select a specific study/trial by name
#' set_trial("CIDTN-2016")
#' 
#' # list all environments/locations information in the selected study/trial
#' list_studies()
#' @export

list_studies <- function() {
  if (is.null(qbms_globals$state$trial_db_id)) {
    stop("No trial has been selected yet! You have to set your trial first using the `set_trial()` function")
  }
  
  bms_trials <- get_program_trials()
  
  trial_row <- which(bms_trials$trialDbId == qbms_globals$state$trial_db_id)

  studies <- bms_trials[trial_row, c("studies")][[1]][,c("studyName", "locationName")]

  return(studies)
}


#' Set the current active study by location name
#' 
#' @description
#' This function will update the current active study in the internal state 
#' object using the studyDbId retrieved from BMS which is associated to the 
#' given study_name parameter.
#' 
#' @param study_name the name of the study
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{set_trial}}, \code{\link{list_studies}}
#' @examples
#' # config your BMS connection
#' set_qbms_config("https://bms.example.com/ibpworkbench/controller/auth/login")
#' 
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#' 
#' set_crop("Tutorial1")
#' 
#' # select a breeding program by name
#' set_program("Training Breeding Program")
#' 
#' # select a specific study/trial by name
#' set_trial("CIDTN-2016")
#' 
#' # select a specific environment/location dataset
#' set_study("CIDTN-2016 Environment Number 1")
#' @export

set_study <- function(study_name) {
  valid_studies <- list_studies()
  
  if (!study_name %in% valid_studies$studyName) {
    stop("Your location name is not exists in this trial! You may use the `list_studies()` function to check the available study location names")
  }
  
  bms_trials <- get_program_trials()

  trial_row <- which(bms_trials$trialDbId == qbms_globals$state$trial_db_id)
  
  bms_studies <- bms_trials[trial_row, c("studies")][[1]]
  
  study_row <- which(bms_studies$studyName == study_name)
  
  qbms_globals$state$study_db_id <- as.character(bms_studies[study_row, "studyDbId"])
}


#' Get the details/metadata of the current active study
#' 
#' @description
#' This function will retrieve the details/metadata of the current active study
#' as configured in the internal state object using `set_study()` function.
#' 
#' @return a data frame of the study details/metadata
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{set_trial}}, \code{\link{set_study}}
#' @examples
#' # config your BMS connection
#' set_qbms_config("https://bms.example.com/ibpworkbench/controller/auth/login")
#' 
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#' 
#' set_crop("Tutorial1")
#' 
#' # select a breeding program by name
#' set_program("Training Breeding Program")
#' 
#' # select a specific study/trial by name
#' set_trial("CIDTN-2016")
#' 
#' # select a specific environment/location dataset
#' set_study("CIDTN-2016 Environment Number 1")
#' 
#' # retrieve the general information of the selected environment/location
#' info <- get_study_info()
#' @export

get_study_info <- function() {
  if (is.null(qbms_globals$state$study_db_id)) {
    stop("No study has been selected yet! You have to set your study first using the `set_study()` function")
  }

  crop_url <- paste0(qbms_globals$config$base_url, "/", qbms_globals$config$crop, "/brapi/v1")
  call_url <- paste0(crop_url, "/studies/", qbms_globals$state$study_db_id)
  
  study_info <- brapi_get_call(call_url)
  study_info <- as.data.frame(do.call(c, unlist(study_info, recursive=FALSE)))
  
  return(study_info)
}


#' Get the observations data of the current active study
#' 
#' @description
#' This function will retrieve the observations data of the current active study
#' as configured in the internal state object using `set_study()` function.
#' 
#' @return a data frame of the study observations data
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{set_trial}}, \code{\link{set_study}}
#' @examples
#' # config your BMS connection
#' set_qbms_config("https://bms.example.com/ibpworkbench/controller/auth/login")
#' 
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#' 
#' set_crop("Tutorial1")
#' 
#' # select a breeding program by name
#' set_program("Training Breeding Program")
#' 
#' # select a specific study/trial by name
#' set_trial("CIDTN-2016")
#' 
#' # select a specific environment/location dataset
#' set_study("CIDTN-2016 Environment Number 1")
#' 
#' # retrieve the data of the selected environment/location
#' data <- get_study_data()
#' @export

get_study_data <- function() {
  if (is.null(qbms_globals$state$study_db_id)) {
    stop("No study has been selected yet! You have to set your study first using the `set_study()` function")
  }

  crop_url <- paste0(qbms_globals$config$base_url, "/", qbms_globals$config$crop, "/brapi/v1")
  call_url <- paste0(crop_url, "/studies/", qbms_globals$state$study_db_id, "/table")
  
  study_result <- brapi_get_call(call_url)
  
  study_data <- as.data.frame(study_result$data)
  study_header <- c(study_result$headerRow, study_result$observationVariableNames)
  colnames(study_data) <- study_header
  
  return(study_data)
}


#' Get the germplasm list of the current active study
#' 
#' @description
#' This function will retrieve the germplasm list of the current active study
#' as configured in the internal state object using `set_study()` function.
#' 
#' @return a data frame of the study germplasm list
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{set_trial}}, \code{\link{set_study}}
#' @examples
#' # config your BMS connection
#' set_qbms_config("https://bms.example.com/ibpworkbench/controller/auth/login")
#' 
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#' 
#' set_crop("Tutorial1")
#' 
#' # select a breeding program by name
#' set_program("Training Breeding Program")
#' 
#' # select a specific study/trial by name
#' set_trial("CIDTN-2016")
#' 
#' # select a specific environment/location dataset
#' set_study("CIDTN-2016 Environment Number 1")
#' 
#' # retrieve the germplasm list of the selected environment/location
#' germplasm <- get_germplasm_list()
#' @export

get_germplasm_list <- function() {
  if (is.null(qbms_globals$state$trial_db_id)) {
    stop("No trial has been selected yet! You have to set your trial first using the `set_trial()` function")
  }
  
  crop_url <- paste0(qbms_globals$config$base_url, "/", qbms_globals$config$crop, "/brapi/v1")
  call_url <- paste0(crop_url, "/studies/", qbms_globals$state$study_db_id, "/germplasm")

  germplasms     <- brapi_get_call(call_url)
  germplasm_list <- as.data.frame(germplasms$data)
  
  if (qbms_globals$state$total_pages > 1 && is.null(qbms_globals$state$errors)) {
    last_page <- qbms_globals$state$total_pages - 1
    for (n in 1:last_page) {
      germplasms     <- brapi_get_call(call_url, n)
      germplasm_list <- plyr::rbind.fill(germplasm_list, as.data.frame(germplasms$data))
    }
  }
  
  germplasm_list[,c("synonyms","typeOfGermplasmStorageCode","taxonIds","donors")] <- list(NULL)
  
  return(germplasm_list)
}


#' Get the observations data of the current active trial
#' 
#' @description
#' This function will retrieve the observations data of the current active trial
#' (i.e. including all studies within) as configured in the internal state 
#' object using `set_trial()` function.
#' 
#' @return a data frame of the trial observations data
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{set_trial}}
#' @examples
#' # config your BMS connection
#' set_qbms_config("https://bms.example.com/ibpworkbench/controller/auth/login")
#' 
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#' 
#' set_crop("Tutorial1")
#' 
#' # select a breeding program by name
#' set_program("Training Breeding Program")
#' 
#' # select a specific study/trial by name
#' set_trial("CIDTN-2016")
#' 
#' # select a specific environment/location dataset
#' set_study("CIDTN-2016 Environment Number 1")
#' 
#' # retrive multi-environment trial data
#' MET <- get_trial_data()
#' @export

get_trial_data <- function() {
  trial_data <- data.frame()
  env <- list_studies()
  
  for (i in env$studyName) {
    set_study(i)
    study_data <- get_study_data()
    trial_data <- rbind(trial_data, study_data)
  }
  
  return(trial_data)
}


#' Get the traits ontology/metadata of the current active trial
#' 
#' @description
#' This function will retrive the traits ontology/metadata of the current active 
#' trial as configured in the internal state object using `set_trial()` function.
#' 
#' @return a data frame of the traits ontology/metadata
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{set_trial}}
#' @examples
#' # config your BMS connection
#' set_qbms_config("https://bms.example.com/ibpworkbench/controller/auth/login")
#' 
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#' 
#' set_crop("Tutorial1")
#' 
#' # select a breeding program by name
#' set_program("Training Breeding Program")
#' 
#' # select a specific study/trial by name
#' set_trial("CIDTN-2016")
#' 
#' # get observation variable ontology
#' ontology <- get_trial_obs_ontology()
#' @export

get_trial_obs_ontology <- function() {
  set_study(list_studies()[1, "studyName"])
  
  crop_url <- paste0(qbms_globals$config$base_url, "/", qbms_globals$config$crop, "/brapi/v1")
  call_url <- paste0(crop_url, "/studies/", qbms_globals$state$study_db_id, "/table")
  
  study_data <- brapi_get_call(call_url)
  study_obs  <- study_data$observationVariableDbIds
  
  my_url <- paste0(qbms_globals$config$base_url, "/crops/", qbms_globals$config$crop, 
                   "/variables/filter?programUUID=", qbms_globals$state$program_db_id,
                   "&variableIds=", paste(study_obs, collapse = ","))
  
  response <- httr::GET(my_url, httr::add_headers("X-Auth-Token" = qbms_globals$state$token))
  
  ontology <- jsonlite::fromJSON(httr::content(response, as = "text"), flatten = TRUE)

  return(ontology)
}


#' Get the list of locations information of the current selected crop
#' 
#' @description
#' This function will retrieve the locations information of the current active crop
#' as configured in the internal state object using `set_crop()` function.
#' 
#' @return a data frame of the locations information
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}, \code{\link{set_crop}}

get_crop_locations <- function() {
  if (is.null(qbms_globals$config$crop)) {
    stop("No crop has been selected yet! You have to set your crop first using the `set_crop()` function")
  }
  
  call_url  <- paste0(qbms_globals$config$base_url, "/", qbms_globals$config$crop, "/brapi/v1/locations")
  locations <- brapi_get_call(call_url, 0, FALSE)
  
  location_list <- as.data.frame(locations$data)

  if (qbms_globals$state$total_pages > 1 && is.null(qbms_globals$state$errors)) {
    last_page <- qbms_globals$state$total_pages - 1
    for (n in 1:last_page) {
      locations     <- brapi_get_call(call_url, n, FALSE)
      location_list <- plyr::rbind.fill(location_list, as.data.frame(locations$data))
    }
  }
  
  return(location_list)
}

#' Get the list of trials studies locations information of the current selected program
#' 
#' @description
#' This function will retrieve all environments/locations information of the trials studies in the
#' current active program as configured in the internal state object using `set_program()` function.
#' 
#' @return a data frame of locations information for each study in the program trials
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}
#' @examples
#' # config your BMS connection
#' set_qbms_config("https://bms.example.com/ibpworkbench/controller/auth/login")
#' 
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#' 
#' set_crop("Tutorial1")
#' 
#' # select a breeding program by name
#' set_program("Training Breeding Program")
#' 
#' # retrive all environments/locations information in the selected program studies/trials
#' program_studies <- get_program_studies()
#' @export

get_program_studies <- function() {
  if (is.null(qbms_globals$state$program_db_id)) {
    stop("No breeding program has been selected yet! You have to set your breeding program first using the `set_program()` function")
  }
  
  all_trials <- get_program_trials()
  program_trials <- subset(all_trials, programDbId == qbms_globals$state$program_db_id)
  
  for (row in 1:nrow(program_trials)) {
    trial <- program_trials[row, 1:5]
    trial_studies <- data.table::rbindlist(program_trials[row, "studies"])
    if (nrow(trial_studies) > 0) {
      if (row == 1) {
        studies <- cbind(trial, trial_studies, row.names = NULL)
      } else {
        studies <- rbind(studies, cbind(trial, trial_studies, row.names = NULL))
      } 
    }
  }
  
  crop_locations <- get_crop_locations()
  
  studies <- merge(studies, crop_locations, by = "locationDbId", all.x = TRUE, all.y = FALSE)
  
  return(studies)
}


#' Get the observations data of a given germplasm name
#' 
#' @description
#' This function will retrieve the observations data of the current active study
#' as configured in the internal state object using `set_study()` function.
#' 
#' @param germplasm_name the name of the germplasm
#' @return a data frame of the germplasm observations data aggregate from all trials
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}
#' @examples
#' # config your BMS connection
#' set_qbms_config("https://bms.example.com/ibpworkbench/controller/auth/login")
#' 
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#' 
#' set_crop("Tutorial1")
#' 
#' # select a breeding program by name
#' set_program("Training Breeding Program")
#' 
#' # retrive observations data of a given germplasm aggregated from all trials
#' germplasm_observations <- get_germplasm_data("FLIP10-3C")
#' @export

get_germplasm_data <- function(germplasm_name) {
  crop_url <- paste0(qbms_globals$config$base_url, "/", qbms_globals$config$crop, "/brapi/v1")
  
  # DEPRECATED in BMS API v16: https://app.swaggerhub.com/apis/ibp_bms/BMSAPI/16.0#/germplasm-resource-brapi/searchGermplasmsUsingGET
  # Use /search/germplasm: https://app.swaggerhub.com/apis/ibp_bms/BMSAPI/16.0#/germplasm-resource-brapi/postSearchGermplasmUsingPOST
  call_url <- paste0(crop_url, "/germplasm-search?germplasmName=", germplasm_name)
  
  germplasm_db_id <- brapi_get_call(call_url)$data$germplasmDbId
  
  # https://github.com/plantbreeding/API/blob/V1.2/Specification/Phenotypes/PhenotypesSearch_POST.md
  # Note 1: It does not work with germplasm name (BrAPI specifications): e.g. {"germplasmDbIds": ["ILC 3279"]}
  # Note 2: Return "Invalid request body" if we search for one germplasm_db_id!
  
  call_url  <- paste0(crop_url, "/phenotypes-search")
  call_body <- list(germplasmDbIds = c(germplasm_db_id,""), observationLevel = "PLOT")
  auth_code <- paste0("Bearer ", qbms_globals$state$token)
  
  response <- httr::POST(call_url, body=call_body, encode="json", httr::add_headers(c("Authorization" = auth_code, "Accept-Encoding" = "gzip, deflate")))
  
  results <- httr::content(response)$result$data
  
  flatten_results <- jsonlite::fromJSON(jsonlite::toJSON(results), flatten = TRUE)
  
  # unlist nested list with id
  unlisted_observations <- data.table::rbindlist(flatten_results$observations, fill = TRUE, idcol = "id")
  
  # create same id in remaining data frame
  flatten_results$id <- seq.int(nrow(flatten_results))
  
  # join data frame with unlisted list
  flatten_results <- dplyr::left_join(flatten_results, unlisted_observations, by = "id")
  
  # get rid of unnecessary columns
  flatten_results$observations <- NULL
  flatten_results$id <- NULL
  
  # we still need to filter out unnecessary columns
  results_df <- data.frame(matrix(nrow=dim(flatten_results)[1], ncol=dim(flatten_results)[2]))
  colnames(results_df) <- colnames(flatten_results)
  
  for(i in 1:ncol(flatten_results)){
    temp <- flatten_results[,i]
    temp[sapply(temp, is.null)] <- NA
    results_df[,i] <- unlist(temp)
  }

  crop_locations <- get_crop_locations()
  results_df <- merge(results_df, crop_locations, by.x = "studyLocationDbId", by.y = "locationDbId", all.x = TRUE)
  
  return(results_df)
}