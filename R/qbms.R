#' Name:     qbms.R
#' Purpose:  Set of functions to query BMS by a wrapper using BrAPI calls
#' Author:   Khaled Al-Shamaa <k.el-shamaa@cgiar.org>
#' Version:  0.7
#'
#' License:  GPLv3

#' Load/install required packages
#' if (!require(utils)) install.packages("utils")
#' if (!require(httr)) install.packages("httr")
#' if (!require(tcltk)) install.packages("tcltk")
#' if (!require(jsonlite)) install.packages("jsonlite")

#' Internal state variables/lists
qbms_globals <- new.env()
qbms_globals$config <- list(crop = NULL)
qbms_globals$state  <- list(token = NULL)

#' Combine data.frames by row, filling in missing columns
#'
#' @description
#' rbinds a list of data frames filling missing columns with NA
#'
#' @param ... the first argument data frame.
#' @param dfs input data frames to row bind together.
#' @return a single data frame

rbindx <- function(..., dfs = list(...)) {
  ns <- unique(unlist(sapply(dfs, names)))
  do.call(rbind, lapply(dfs, function(x) {
    for (n in ns[! ns %in% names(x)]) { x[[n]] <- NA }; x }))
}

#' Makes one data.table from a list of many
#'
#' @description
#' Same as do.call("rbind", x) on data.frames, but much faster.
#'
#' @param x A list containing data.table, data.frame or list objects.
#' @return an unkeyed data.table containing a concatenation of all the items passed in.

rbindlistx <- function(x) {
  u  <- unlist(x, recursive = FALSE)
  n  <- names(u)
  un <- unique(n)
  l  <- lapply(un, function(N) unlist(u[N == n], FALSE, FALSE))
  names(l) <- un
  d <- as.data.frame(l)
}

#' Debug internal QBMS status object
#'
#' @description
#' Return the internal QBMS status object for debugging 
#'
#' @return an environment object for the package config and status
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @examples
#' obj <- debug_qbms()
#' obj$config
#' obj$state
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
#' @param time_out  Number of seconds to wait for a response until giving up (default is 10)
#' @param no_auth   TRUE if the server doesn't require authentication/login (default is FALSE)
#' @param engine    Backend database (qbms default, breedbase)
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @return no return value
#' @examples
#' set_qbms_config("https://www.bms-uat-test.net/ibpworkbench")
#' @export

set_qbms_config <- function(url = "http://localhost/ibpworkbench/controller/auth/login",
                            path = "bmsapi", page_size = 1000, time_out = 120,
                            no_auth = FALSE, engine = "bms") {

  qbms_globals$config$server    <- regmatches(url, regexpr("^(?://|[^/]+)*", url))
  qbms_globals$config$path      <- path
  qbms_globals$config$page_size <- page_size
  qbms_globals$config$time_out  <- time_out
  qbms_globals$config$base_url  <- paste0(qbms_globals$config$server, "/", qbms_globals$config$path)
  qbms_globals$config$engine    <- engine

  if (no_auth == TRUE) {
    qbms_globals$state$token <- NA
  }
}


#' Internal function used for core BrAPI GET calls
#'
#' @description
#' This function created for *internal use only* to cal BrAPI in GET method and 
#' retrieve the rough response data and send back the results. This function take
#' care of pagination, authentication, encoding, compress, decode JSON response, etc.
#'
#' @param call_url BrAPI URL to call in GET method
#' @param page     Page number to retrieve in case of multi-paged results (default is 0)
#' @param nested   If FALSE, then retrieved JSON data will be flatten (default is TRUE)
#' @return result object returned by JSON API response
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}

brapi_get_call <- function(call_url, page = 0, nested = TRUE) {
  separator <- if (grepl("\\?", call_url)) "&" else "?"
  full_url  <- paste0(call_url, separator, "page=", page, "&pageSize=", qbms_globals$config$page_size)

  auth_code <- paste0("Bearer ", qbms_globals$state$token)
  headers   <- c("Authorization" = auth_code, "Accept-Encoding" = "gzip, deflate")

  response  <- httr::GET(url = utils::URLencode(full_url),
                         httr::add_headers(headers),
                         httr::timeout(qbms_globals$config$time_out))

  result_object <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), flatten = !nested)
  result_info   <- result_object$result

  qbms_globals$state$current_page <- result_object$metadata$pagination$currentPage
  qbms_globals$state$page_size    <- result_object$metadata$pagination$pageSize
  qbms_globals$state$total_count  <- result_object$metadata$pagination$totalCount
  qbms_globals$state$total_pages  <- result_object$metadata$pagination$totalPages
  qbms_globals$state$errors       <- result_object$errors

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
#' @return no return value
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @examples
#' if(interactive()) {
#' # config your BMS connection
#' set_qbms_config("https://www.bms-uat-test.net/ibpworkbench")
#'
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#' }
#' @export

login_bms <- function(username = NULL, password = NULL) {
  if (is.null(username) || is.null(password)) {
    credentials <- get_login_details()
  } else {
    credentials <- c(usr = username, pwd = password)
  }

  call_url  <- paste0(qbms_globals$config$base_url, "/brapi/v1/token")
  call_body <- list(username = credentials["usr"], password = credentials["pwd"])

  response <- httr::POST(url = utils::URLencode(call_url), body = call_body, encode = "json",
                         httr::timeout(qbms_globals$config$time_out))

  if (!is.null(httr::content(response)$errors)) {
    stop(httr::content(response)$errors[[1]]$message)
  }

  qbms_globals$state$token <- httr::content(response)$access_token
  qbms_globals$state$user  <- httr::content(response)$userDisplayName
  # as.POSIXct(qbms_globals$state$expires_in/1000, origin="1970-01-01")
  qbms_globals$state$expires_in <- httr::content(response)$expires_in
}


#' Get the list of supported crops
#'
#' @return a list of supported crops
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}
#' @examples
#' if(interactive()) {
#' # config your BMS connection
#' set_qbms_config("https://www.bms-uat-test.net/ibpworkbench")
#'
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#'
#' # list supported crops in the bms server
#' list_crops()
#' }
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
#' @param crop_name the name of the crop
#' @return no return value
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}, \code{\link{list_crops}}
#' @examples
#' if(interactive()) {
#' # config your BMS connection
#' set_qbms_config("https://www.bms-uat-test.net/ibpworkbench")
#'
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#'
#' set_crop("maize")
#' }
#' @export

set_crop <- function(crop_name) {
  valid_crops <- list_crops()

  if (!crop_name %in% valid_crops) {
    stop("Your crop name is not supported in this connected BMS server! You may use the `list_crops()` function to check the available crops")
  }

  if (qbms_globals$config$engine == "breedbase") {
    qbms_globals$config$crop <- ""
  } else {
    qbms_globals$config$crop <- crop_name
  }
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
#' if(interactive()) {
#' # config your BMS connection
#' set_qbms_config("https://www.bms-uat-test.net/ibpworkbench")
#'
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#'
#' set_crop("maize")
#'
#' # list existing breeding programs
#' list_programs()
#' }
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

  if (qbms_globals$config$engine == "breedbase") {
    bms_programs <- bms_programs$data[c("programName")]
  } else {
    bms_programs <- bms_programs$data[c("name")]
  }

  return(bms_programs)
}


#' Set the current active breeding program
#'
#' @description
#' This function will update the current active breeding program in the
#' internal state object using the programDbId retrieved from BMS which is
#' associated to the given program_name parameter.
#'
#' @param program_name the name of the breeding program
#' @return no return value
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{list_programs}}
#' @examples
#' if(interactive()) {
#' # config your BMS connection
#' set_qbms_config("https://www.bms-uat-test.net/ibpworkbench")
#'
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#'
#' set_crop("maize")
#'
#' # select a breeding program by name
#' set_program("MC Maize")
#' }
#' @export

set_program <- function(program_name) {
  valid_programs <- list_programs()

  if (!program_name %in% valid_programs[, 1]) {
    stop("Your breeding program name is not exists in this crop database! You may use the `list_programs()` function to check the available breeding programs")
  }

  call_url <- paste0(qbms_globals$config$base_url, "/", qbms_globals$config$crop, "/brapi/v1/programs")

  bms_programs <- brapi_get_call(call_url)

  if (qbms_globals$config$engine == "breedbase") {
    program_row <- which(bms_programs$data$programName == program_name)
  } else {
    program_row <- which(bms_programs$data$name == program_name)
  }

  qbms_globals$state$program_db_id <- bms_programs$data[program_row, "programDbId"]
}


#' Internal function used to retrieve the rough list of trials
#'
#' @description
#' This function created for *internal use only* to retrieve the rough list of trials
#' from the pre-selected (i.e. currently active) crop and breeding program combination
#' as already configured in the internal state object using `set_crop()` and `set_program()`
#' functions respectively.
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
      bms_program_trials <- rbindx(bms_program_trials, bms_crop_trials$data)
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
#' if(interactive()) {
#' # config your BMS connection
#' set_qbms_config("https://www.bms-uat-test.net/ibpworkbench")
#'
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#'
#' set_crop("maize")
#'
#' # select a breeding program by name
#' set_program("MC Maize")
#'
#' # list all studies/trials in the selected program
#' list_trials()
#'
#' # filter listed studies/trials by year
#' list_trials(2020)
#' }
#' @export

list_trials <- function(year = NULL) {
  if (is.null(qbms_globals$state$program_db_id)) {
    stop("No breeding program has been selected yet! You have to set your breeding program first using the `set_program()` function")
  }

  if (!is.null(year) && !is.numeric(year)) {
    stop("Year parameter should be numeric")
  }

  if (!is.null(year) && qbms_globals$config$engine == "breedbase") {
    stop("Year parameter is not supported in BreedBase!")
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


#' Set the current active trial
#'
#' @description
#' This function will update the current active trial in the internal state
#' object using the trialDbId retrieved from BMS which is associated to the
#' given trial_name parameter.
#'
#' @param trial_name the name of the trial
#' @return no return value
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{list_trials}}
#' @examples
#' if(interactive()) {
#' # config your BMS connection
#' set_qbms_config("https://www.bms-uat-test.net/ibpworkbench")
#'
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#'
#' set_crop("maize")
#'
#' # select a breeding program by name
#' set_program("MC Maize")
#'
#' # select a specific study/trial by name
#' set_trial("2018 PVT")
#' }
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
#' if(interactive()) {
#' # config your BMS connection
#' set_qbms_config("https://www.bms-uat-test.net/ibpworkbench")
#'
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#'
#' set_crop("maize")
#'
#' # select a breeding program by name
#' set_program("MC Maize")
#'
#' # select a specific study/trial by name
#' set_trial("2018 PVT")
#'
#' # list all environments/locations information in the selected study/trial
#' list_studies()
#' }
#' @export

list_studies <- function() {
  if (is.null(qbms_globals$state$trial_db_id)) {
    stop("No trial has been selected yet! You have to set your trial first using the `set_trial()` function")
  }

  bms_trials <- get_program_trials()

  trial_row <- which(bms_trials$trialDbId == qbms_globals$state$trial_db_id)

  studies <- bms_trials[trial_row, c("studies")][[1]][, c("studyName", "locationName")]

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
#' @return no return value
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{set_trial}}, \code{\link{list_studies}}
#' @examples
#' if(interactive()) {
#' # config your BMS connection
#' set_qbms_config("https://www.bms-uat-test.net/ibpworkbench")
#'
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#'
#' set_crop("maize")
#'
#' # select a breeding program by name
#' set_program("MC Maize")
#'
#' # select a specific study/trial by name
#' set_trial("2018 PVT")
#'
#' # select a specific environment/location dataset
#' set_study("2018 PVT Environment Number 1")
#' }
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
#' if(interactive()) {
#' # config your BMS connection
#' set_qbms_config("https://www.bms-uat-test.net/ibpworkbench")
#'
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#'
#' set_crop("maize")
#'
#' # select a breeding program by name
#' set_program("MC Maize")
#'
#' # select a specific study/trial by name
#' set_trial("2018 PVT")
#'
#' # select a specific environment/location dataset
#' set_study("2018 PVT Environment Number 1")
#'
#' # retrieve the general information of the selected environment/location
#' info <- get_study_info()
#' }
#' @export

get_study_info <- function() {
  if (is.null(qbms_globals$state$study_db_id)) {
    stop("No study has been selected yet! You have to set your study first using the `set_study()` function")
  }

  crop_url <- paste0(qbms_globals$config$base_url, "/", qbms_globals$config$crop, "/brapi/v1")
  call_url <- paste0(crop_url, "/studies/", qbms_globals$state$study_db_id)

  study_info <- brapi_get_call(call_url)
  study_info <- as.data.frame(t(as.matrix(do.call(c, unlist(study_info, recursive = FALSE)))))

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
#' @seealso \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}},
#'          \code{\link{set_trial}}, \code{\link{set_study}}
#' @examples
#' if(interactive()) {
#' # config your BMS connection
#' set_qbms_config("https://www.bms-uat-test.net/ibpworkbench")
#'
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#'
#' set_crop("maize")
#'
#' # select a breeding program by name
#' set_program("MC Maize")
#'
#' # select a specific study/trial by name
#' set_trial("2018 PVT")
#'
#' # select a specific environment/location dataset
#' set_study("2018 PVT Environment Number 1")
#'
#' # retrieve the data of the selected environment/location
#' data <- get_study_data()
#' }
#' @export

get_study_data <- function() {
  if (is.null(qbms_globals$state$study_db_id)) {
    stop("No study has been selected yet! You have to set your study first using the `set_study()` function")
  }

  crop_url <- paste0(qbms_globals$config$base_url, "/", qbms_globals$config$crop, "/brapi/v1")
  call_url <- paste0(crop_url, "/studies/", qbms_globals$state$study_db_id, "/table")

  study_result <- brapi_get_call(call_url)

  if (qbms_globals$config$engine == "breedbase") {
    study_data <- as.data.frame(study_result$data[-1, ])
    colnames(study_data) <- study_result$data[1, ]
  } else {
    study_data <- as.data.frame(study_result$data)
  }

  study_header <- c(study_result$headerRow, study_result$observationVariableNames)
  if (nrow(study_data) > 0) { 
    colnames(study_data) <- study_header
  }

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
#' if(interactive()) {
#' # config your BMS connection
#' set_qbms_config("https://www.bms-uat-test.net/ibpworkbench")
#'
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#'
#' set_crop("maize")
#'
#' # select a breeding program by name
#' set_program("MC Maize")
#'
#' # select a specific study/trial by name
#' set_trial("2018 PVT")
#'
#' # select a specific environment/location dataset
#' set_study("2018 PVT Environment Number 1")
#'
#' # retrieve the germplasm list of the selected environment/location
#' germplasm <- get_germplasm_list()
#' }
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
      germplasm_list <- rbindx(germplasm_list, as.data.frame(germplasms$data))
    }
  }

  if (qbms_globals$config$engine == "breedbase") {
    germplasm_list$check <- NA
    germplasm_list[, c("synonyms")] <- list(NULL)
  } else {
    # BMS POST /crops/{cropName}/programs/{programUUID}/studies/{studyId}/entries to extract entry type (test or check)
    call_url <- paste0(qbms_globals$config$base_url, "/crops/", qbms_globals$config$crop, 
                       "/programs/", qbms_globals$state$program_db_id,
                       "/studies/", qbms_globals$state$trial_db_id, "/entries")

    call_body <- paste0('{"filter":{"entryNumbers": ["', paste0(germplasm_list$entryNumber, collapse = '","'), '"]}}')

    response <- httr::POST(url = utils::URLencode(call_url), body = "", encode = "json",
                           httr::add_headers(c("X-Auth-Token" = qbms_globals$state$token), "Accept-Encoding" = "gzip, deflate"),
                           httr::timeout(qbms_globals$config$time_out))

    results <- jsonlite::fromJSON(httr::content(response, as = "text"), flatten = TRUE)

    germplasm_list <- merge(germplasm_list, results[, c("entryNumber", "properties.8255.value")], by = "entryNumber")

    germplasm_list$check <- ifelse(germplasm_list$properties.8255.value == 10180, 1, 0)

    germplasm_list[, c("synonyms", "typeOfGermplasmStorageCode", "taxonIds", "donors", "properties.8255.value")] <- list(NULL)
  }

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
#' if(interactive()) {
#' # config your BMS connection
#' set_qbms_config("https://www.bms-uat-test.net/ibpworkbench")
#'
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#'
#' set_crop("maize")
#'
#' # select a breeding program by name
#' set_program("MC Maize")
#'
#' # select a specific study/trial by name
#' set_trial("2018 PVT")
#'
#' # select a specific environment/location dataset
#' set_study("2018 PVT Environment Number 1")
#'
#' # retrive multi-environment trial data
#' MET <- get_trial_data()
#' }
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
#' This function will retrieve the traits ontology/metadata of the current active
#' trial as configured in the internal state object using `set_trial()` function.
#'
#' @return a data frame of the traits ontology/metadata
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso \code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{set_trial}}
#' @examples
#' if(interactive()) {
#' # config your BMS connection
#' set_qbms_config("https://www.bms-uat-test.net/ibpworkbench")
#'
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#'
#' set_crop("maize")
#'
#' # select a breeding program by name
#' set_program("MC Maize")
#'
#' # select a specific study/trial by name
#' set_trial("2018 PVT")
#'
#' # get observation variable ontology
#' ontology <- get_trial_obs_ontology()
#' }
#' @export

get_trial_obs_ontology <- function() {
  set_study(list_studies()[1, "studyName"])

  crop_url <- paste0(qbms_globals$config$base_url, "/", qbms_globals$config$crop, "/brapi/v1")
  call_url <- paste0(crop_url, "/studies/", qbms_globals$state$study_db_id, "/table")

  study_data <- brapi_get_call(call_url)

  if (qbms_globals$config$engine == "breedbase") {
    ontology <- as.data.frame(study_data$observationVariableNames)

    colnames(ontology) <- "observationVariableNames"
  } else {
    study_obs  <- study_data$observationVariableDbIds

    my_url <- paste0(qbms_globals$config$base_url, "/crops/", qbms_globals$config$crop,
                     "/variables/filter?programUUID=", qbms_globals$state$program_db_id,
                     "&variableIds=", paste(study_obs, collapse = ","))

    response <- httr::GET(url = utils::URLencode(my_url),
                          httr::add_headers("X-Auth-Token" = qbms_globals$state$token, "Accept-Encoding" = "gzip, deflate"),
                          httr::timeout(qbms_globals$config$time_out))

    ontology <- jsonlite::fromJSON(httr::content(response, as = "text"), flatten = TRUE)
  }

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
      location_list <- rbindx(location_list, as.data.frame(locations$data))
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
#' if(interactive()) {
#' # config your BMS connection
#' set_qbms_config("https://www.bms-uat-test.net/ibpworkbench")
#'
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#'
#' set_crop("maize")
#'
#' # select a breeding program by name
#' set_program("MC Maize")
#'
#' # retrive all environments/locations information in the selected program studies/trials
#' program_studies <- get_program_studies()
#' }
#' @export

get_program_studies <- function() {
  if (qbms_globals$config$engine == "breedbase") {
    stop("This function is not supported yet in BreedBase!")
  }

  if (is.null(qbms_globals$state$program_db_id)) {
    stop("No breeding program has been selected yet! You have to set your breeding program first using the `set_program()` function")
  }

  all_trials <- get_program_trials()
  program_trials <- all_trials[all_trials$programDbId == qbms_globals$state$program_db_id, ]

  colnames(program_trials) <- gsub("additionalInfo.", "", colnames(program_trials))

  for (row in 1:nrow(program_trials)) {
    trial <- program_trials[row, -7]
    trial_studies <- rbindlistx(program_trials[row, "studies"])
    if (nrow(trial_studies) > 0) {
      if (row == 1) {
        studies <- cbind(trial, trial_studies, row.names = NULL)
      } else {
        studies <- rbind(studies, cbind(trial, trial_studies, row.names = NULL))
      }
    }
  }

  studies <- studies[, unique(colnames(studies))]

  crop_locations <- get_crop_locations()

  studies <- merge(studies, crop_locations, by = "locationDbId", all.x = TRUE, all.y = FALSE)

  studies$testEntriesCount <- 0

  crop_url <- paste0(qbms_globals$config$base_url, "/crops/", qbms_globals$config$crop)

  for (i in unique(studies$trialDbId)){
    call_url <- paste0(crop_url, "/programs/", qbms_globals$state$program_db_id, "/studies/", i, "/entries/metadata")

    response <- httr::GET(url = utils::URLencode(call_url),
                          httr::add_headers("X-Auth-Token" = qbms_globals$state$token, "Accept-Encoding" = "gzip, deflate"),
                          httr::timeout(qbms_globals$config$time_out))
    metadata <- jsonlite::fromJSON(httr::content(response, as = "text"), flatten = TRUE)

    studies[studies$trialDbId == i, "testEntriesCount"] <- metadata$testEntriesCount
    studies[studies$trialDbId == i, "checkEntriesCount"] <- metadata$checkEntriesCount
  }

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
#' if(interactive()) {
#' # config your BMS connection
#' set_qbms_config("https://www.bms-uat-test.net/ibpworkbench")
#'
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#'
#' set_crop("maize")
#'
#' # select a breeding program by name
#' set_program("MC Maize")
#'
#' # retrive observations data of a given germplasm aggregated from all trials
#' germplasm_observations <- get_germplasm_data("BASFCORN-2-1")
#' }
#' @export

get_germplasm_data <- function(germplasm_name) {
  if (qbms_globals$config$engine == "breedbase") {
    stop("This function is not supported yet in BreedBase!")
  }

  crop_url <- paste0(qbms_globals$config$base_url, "/", qbms_globals$config$crop, "/brapi/v1")
  call_url <- paste0(crop_url, "/germplasm?germplasmName=", germplasm_name)

  results <- brapi_get_call(call_url)$data
  germplasm_db_id <- results[results$germplasmName == germplasm_name, "germplasmDbId"]

  # https://github.com/plantbreeding/API/blob/V1.2/Specification/Phenotypes/PhenotypesSearch_POST.md
  # Note 1: It does not work with germplasm name (BrAPI specifications): e.g. {"germplasmDbIds": ["ILC 3279"]}
  # Note 2: Return "Invalid request body" if we search for one germplasm_db_id!

  call_url  <- paste0(crop_url, "/phenotypes-search")
  call_body <- list(germplasmDbIds = c(germplasm_db_id, ""), observationLevel = "PLOT")
  auth_code <- paste0("Bearer ", qbms_globals$state$token)

  response <- httr::POST(url = utils::URLencode(call_url), body = call_body, encode = "json",
                         httr::add_headers(c("Authorization" = auth_code, "Accept-Encoding" = "gzip, deflate")),
                         httr::timeout(qbms_globals$config$time_out))

  results <- httr::content(response)$result$data

  flatten_results <- jsonlite::fromJSON(jsonlite::toJSON(results), flatten = TRUE)

  # unlist nested list with id
  #unlisted_observations <- data.table::rbindlist(flatten_results$observations, fill = TRUE, idcol = "id")

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

  crop_locations <- get_crop_locations()
  results_df <- merge(results_df, crop_locations, by.x = "studyLocationDbId", by.y = "locationDbId", all.x = TRUE)

  return(results_df)
}


#' Get Direct Parents
#'
#' @description
#' Internal helping function to split the given pedigree string that provides the parentage
#' through which a cultivar was obtained, and get the pedigrees of the direct parents.
#'
#' @param pedigree String provide the parentage through which a cultivar was obtained.
#' @return Vector of two items, the direct female and male parents.
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}

get_parents <- function(pedigree) {
  # make sure it is a string
  pedigree <- as.character(pedigree)

  # 1. we did not expect cross depth to be more than two digits (up to 99)
  cross <- regmatches(pedigree, gregexpr("/[0-9]{1,2}/", pedigree))

  if (length(cross[[1]]) != 0) {
    # find the highest cross order to cut at it and get parents sub-pedigree
    last_cross <- max(as.numeric(gsub("/", "", cross[[1]])))
    parents    <- strsplit(pedigree, paste0("/", last_cross, "/"))[[1]]
  } else {
    # 2. if it is not of type /#/, then try double backslash //
    cross <- regmatches(pedigree, gregexpr("//", pedigree))

    if (length(cross[[1]]) != 0) {
      # get parents sub-pedigree if it is crossed using //
      parents <- strsplit(pedigree, "//")[[1]]
    } else {
      # 3. if it is not // then try with single backslash /
      cross <- regmatches(pedigree, gregexpr("/", pedigree))

      if (length(cross[[1]]) != 0) {
        # get parents names
        parents <- strsplit(pedigree, "/")[[1]]
      } else {
        # 4. else, there is no more cross info in this pedigree, so parents are unknown
        parents <- c(NA, NA)
      }
    }
  }

  # remove leading/trailing white-space
  parents <- trimws(parents)

  # replace unknown parents by NA
  parents <- gsub("unknown", NA, parents)

  # send back a vector of two items, the direct female and male parents
  return(parents)
}


#' Building Pedigree Table Recursively
#'
#' @description
#' Internal helping function to build the pedigree table recursively.
#'
#' @param geno_list List of genotypes/germplasms names.
#' @param pedigree_list List of associated pedigree strings.
#' @param pedigree_df Pedigree data.frame as per previous call/iteration.
#' @return A data.frame that has three columns correspond to the identifiers for the individual,
#'         female parent and male parent, respectively.
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}

build_pedigree_table <- function(geno_list = NULL, pedigree_list = NULL, pedigree_df = NULL) {
  # check if geno list is not empty
  if (length(geno_list) == 0) warning("Empty genotype/germplasm list!")

  # check if the length of pedigree list is the same length of geno list
  if (length(pedigree_list) != length(geno_list)) warning("Pedigree list does not match the length of genotype/germplasm list!")

  # if no previous pedigree data.frame passed by current call
  if (is.null(pedigree_df)) {
    # create an empty pedigree data.frame
    pedigree_df   <- data.frame(Variety = factor(), Female = factor(), Male = factor())

    # make sure that all strings of genotype/germplasm and pedigree lists are in small letters (needs only first time)
    geno_list     <- tolower(geno_list)
    pedigree_list <- tolower(pedigree_list)
  }

  # create an empty dummy list of previous generation parents
  prev_generation <- c()

  # extract the parents of each genotype/germplasm in the given list
  for (i in 1:length(geno_list)) {
    geno    <- as.character(geno_list[i])
    cross   <- as.character(pedigree_list[i])
    parents <- get_parents(cross)

    # update the pedigree data.frame and dummy list of previous generation parents
    pedigree_df     <- rbind(c(geno, parents), pedigree_df)
    prev_generation <- c(prev_generation, parents)
  }

  # clean the previous generation parents list by remove NA and duplicates
  prev_generation <- prev_generation[which(!is.na(prev_generation))]
  prev_generation <- unique(prev_generation)

  # check if we still have any previous generation parents need to extract
  if (length(prev_generation) > 0) {
    # recall this function recursively to process the previous generation parents passing current pedigree data.frame
    build_pedigree_table(prev_generation, prev_generation, pedigree_df)
  } else {
    # rename the pedigree data.frame columns properly
    names(pedigree_df) <- c("Variety", "Female", "Male")

    # remove duplicated entries in the pedigree data.frame
    pedigree_df <- pedigree_df[!duplicated(pedigree_df$Variety), ]

    # return the pedigree data.frame
    return(pedigree_df)
  }
}


#' Get the Pedigree Table
#'
#' @description
#' Get the pedigree table starting from current germplasm list and associated
#' pedigree string that provides the parentage through which a cultivar was obtained.
#'
#' @param data germplasm dataset as a data.frame.
#' @param geno_column name of the column that identifies the genotype/germplasm names.
#' @param pedigree_column name of the column that identifies the pedigree strings.
#' @return A data.frame that has three columns correspond to the identifiers for the individual,
#'         female parent and male parent, respectively. The row giving the pedigree of an
#'         individual appears before any row where that individual appears as a parent.
#'         Founders use NA in the parental columns.
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @examples
#' if(interactive()) {
#' # config your BMS connection
#' set_qbms_config("https://www.bms-uat-test.net/ibpworkbench")
#'
#' # login using your BMS account (interactive mode)
#' # you can pass BMS username and password as parameters (batch mode)
#' login_bms()
#'
#' set_crop("maize")
#'
#' # select a breeding program by name
#' set_program("MC Maize")
#'
#' # select a specific study/trial by name
#' set_trial("2018 PVT")
#'
#' # select a specific environment/location dataset
#' set_study("2018 PVT Environment Number 1")
#'
#' # retrieve the germplasm list of the selected environment/location
#' germplasm <- get_germplasm_list()
#'
#' pedigree_table <- get_pedigree_table(germplasm, "germplasmName", "pedigree")
#'
#' #############################
#' # nadiv package way
#' # library(nadiv)
#'
#' # get additive relationship matrix in sparse matrix format
#' # A <- nadiv::makeA(pedigree_table)
#'
#' # get A inverse matrix using base R function
#' # AINV <- solve(as.matrix(A))
#'
#' #############################
#' # ASReml-R package way
#' # library(asreml)
#'
#' # represent A inverse matrix in efficient way using i,j index and Ainverse value
#' # actual genotype names of any given index are in the attr(ainv, "rowNames")
#' # ainv <- asreml::ainverse(pedigree_table)
#'
#' #############################
#' # dummy data set for testing
#' test <- data.frame(genotype = c("X", "Y"),
#'                    pedigree = c("A//B/D/2/C", "B/C/3/A//B/C/2/D"))
#'
#' pedigree_table <- get_pedigree_table(test, "genotype", "pedigree")
#' }
#' @export

get_pedigree_table <- function(data, geno_column = "germplasmName", pedigree_column = "pedigree") {
  # extract the list of genotypes/germplasms and associated pedigrees
  geno_list     <- data[, geno_column]
  pedigree_list <- data[, pedigree_column]

  # extract the first round of pedigree data.frame to check/audit it before the final call
  pedigree_df <- build_pedigree_table(geno_list, pedigree_list)

  # get only root genotypes (i.e., have no parents info)
  roots <- pedigree_df[is.na(pedigree_df$Female) & is.na(pedigree_df$Male), "Variety"]

  # compute the string edit distance
  diff <- utils::adist(roots)

  # keep the lower triangular part of the matrix
  diff[!lower.tri(diff)] <- NA

  # get the index of pairs with distance = 1 (i.e., one char difference)
  check <- which(diff == 1, arr.ind = TRUE)

  # replace index by the genotype name
  check <- cbind(roots[check[, 1]], roots[check[, 2]])

  # for each pair of similar genotype names
  for (i in 1:nrow(check)) {
    # go through all letters of the given pair
    for (j in 1:max(nchar(check[i, ]))) {
      # if the given letters in the j offset are same, then move to the next letter
      if (substr(check[i, 1], j, j) == substr(check[i, 2], j, j)) next

      # if they are not the same, then check
      # if the different letter is one of this group: <space>, -, _, .
      # then update the geno_list and pedigree_list to be the same
      if (substr(check[i, 1], j, j) %in% c(" ", "-", "_", ".")) {
        geno_list     <- gsub(check[i, 2], check[i, 1], geno_list,     ignore.case = TRUE)
        pedigree_list <- gsub(check[i, 2], check[i, 1], pedigree_list, ignore.case = TRUE)
      } else if (substr(check[i, 2], j, j) %in% c(" ", "-", "_", ".")) {
        geno_list     <- gsub(check[i, 1], check[i, 2], geno_list,     ignore.case = TRUE)
        pedigree_list <- gsub(check[i, 1], check[i, 2], pedigree_list, ignore.case = TRUE)
      }
    }
  }

  # get the final pedigree data.frame using updated/audited lists of geno_list and pedigree_list
  pedigree_df <- build_pedigree_table(geno_list, pedigree_list)

  return(pedigree_df)
}
