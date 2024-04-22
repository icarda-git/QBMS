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
brapi_map <- rbind(brapi_map, c("get_trial_obs_ontology", "v2", "variables"))

brapi_map <- rbind(brapi_map, c("get_germplasm_id", "v1", "germplasm?germplasmName={germplasmName}"))
brapi_map <- rbind(brapi_map, c("get_germplasm_id", "v2", "germplasm?germplasmName={germplasmName}"))

# POST: germplasmDbIds, observationLevel = "PLOT"
brapi_map <- rbind(brapi_map, c("get_germplasm_data", "v1", "phenotypes-search"))

brapi_map <- rbind(brapi_map, c("get_germplasm_attributes", "v1", "germplasm/{germplasmDbId}/attributes"))
brapi_map <- rbind(brapi_map, c("get_germplasm_attributes", "v2", "attributes?germplasmDbId={germplasmDbId}"))

############################### GIGWA calls ####################################

brapi_map <- rbind(brapi_map, c("gigwa_list_dbs", "v2", "programs"))
brapi_map <- rbind(brapi_map, c("gigwa_list_projects", "v2", "studies?programDbId={programDbId}"))

# POST: studyDbIds
brapi_map <- rbind(brapi_map, c("gigwa_list_runs", "v2", "search/variantsets"))

# POST: studyDbIds
brapi_map <- rbind(brapi_map, c("gigwa_get_samples", "v2", "search/germplasm"))

# POST: germplasmDbIds
brapi_map <- rbind(brapi_map, c("gigwa_get_metadata", "v2", "search/attributevalues"))

################################################################################

colnames(brapi_map) <- c("func_name", "brapi_ver", "brapi_call")


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


#' Combine Data Frames by Row, Filling in Missing Columns
#'
#' @description
#' Combines a list of data frames by row, filling in missing columns with NA.
#'
#' @param ... The first argument data frame.
#' @param dfs Input data frames to row bind together.
#' 
#' @return
#' A single data frame.

rbindx <- function(..., dfs = list(...)) {
  ns <- unique(unlist(sapply(dfs, names)))
  do.call(rbind, lapply(dfs, function(x) {
    for (n in ns[! ns %in% names(x)]) {
      x[[n]] <- NA
    }
    return(x)
  }))
}


#' Make One Data.Table from a List of Many
#'
#' @description
#' Performs the equivalent of do.call("rbind", x) on data.frames, but much faster.
#'
#' @param x A list containing data.table, data.frame, or list objects.
#' 
#' @return
#' An unkeyed data.table containing a concatenation of all the items passed in.

rbindlistx <- function(x) {
  u  <- unlist(x, recursive = FALSE)
  n  <- names(u)
  un <- unique(n)
  l  <- lapply(un, function(y) unlist(u[y == n], FALSE, FALSE))
  names(l) <- un
  d <- as.data.frame(l)
}


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
    if (engine == "gigwa") { path = "gigwa/rest"}
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


#' Common HTTP Headers to Send
#'
#' @description
#' Builds the list of common HTTP headers to send with each API call.
#'
#' @return
#' A list of common HTTP headers to send.

brapi_headers <- function() {
  auth_code <- paste0("Bearer ", qbms_globals$state$token)
  headers   <- c("Authorization" = auth_code, 
                 "Accept-Encoding" = "gzip, deflate",
                 "accept" = "application/json")
  headers
}


#' Async Version of HTTP GET Request
#'
#' @description
#' A small helper function to create an `async` version of the original HTTP GET request.
#'
#' @param full_url URL to retrieve.
#' @param nested Logical indicating whether to flatten nested data frames. Default is FALSE.
#' 
#' @return
#' Async version of the HTTP GET request.

get_async_page <- function(full_url, nested) {
}

if (requireNamespace("async", quietly = TRUE)) {
  get_async_page <- async::async(function(full_url, nested) {
    async::http_get(full_url, headers = brapi_headers())$
      then(async::http_stop_for_status)$
      then(function(resp) {
        jsonlite::fromJSON(rawToChar(resp$content), flatten = !nested)
      })
  })
}


#' Run All Supplied Pages
#'
#' @description
#' A small helper function to create a deferred value that is resolved when all 
#' listed pages are resolved.
#'
#' @param pages List of URLs to retrieve.
#' @param nested Logical indicating whether to flatten nested data frames. Default is FALSE.
#' 
#' @return
#' Async deferred object.

get_async_pages <- function(pages, nested) {
}

if (requireNamespace("async", quietly = TRUE)) {
  get_async_pages <- async::async(function(pages, nested) {
    reqs <- lapply(pages, get_async_page, nested)
    async::when_all(.list = reqs)$
      then(function(x) x)
  })
}


#' Internal Function Used for Core BrAPI GET Calls
#'
#' @description
#' This function is created for *internal use only* to call BrAPI in the GET method and
#' retrieve the raw response data and send back the results. This function takes
#' care of pagination, authentication, encoding, compression, decoding JSON response, etc.
#'
#' @param call_url BrAPI URL to call in GET method.
#' @param nested Logical indicating whether retrieved JSON data will be flattened. Default is TRUE.
#' 
#' @return
#' Result object returned by the JSON API response.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}

brapi_get_call <- function(call_url, nested = TRUE) {
  separator <- if (grepl("\\?", call_url)) "&" else "?"
  full_url  <- paste0(call_url, separator, "page=0&pageSize=", qbms_globals$config$page_size)

  headers  <- brapi_headers()
  response <- httr::GET(url = utils::URLencode(full_url),
                        httr::add_headers(headers),
                        httr::timeout(qbms_globals$config$time_out))
  
  result_object <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), flatten = !nested)
  result_data   <- as.data.frame(result_object$result$data)
  
  if (result_object$metadata$pagination$totalPages > 1 && is.null(result_object$errors)) {
    last_page <- result_object$metadata$pagination$totalPages - 1
    
    if (qbms_globals$config$verbose) {
      pb      <- utils::txtProgressBar(min = 0, max = last_page + 1, initial = 0, style = 3)
      pb_step <- 1
      utils::setTxtProgressBar(pb, 1)
    }
    
    for (n in 1:last_page) {
      full_url <- paste0(call_url, separator, "page=", n, "&pageSize=", qbms_globals$config$page_size)
      response <- httr::GET(url = utils::URLencode(full_url),
                            httr::add_headers(headers),
                            httr::timeout(qbms_globals$config$time_out))
      
      result_object <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), flatten = !nested)
      result_data   <- rbindx(result_data, as.data.frame(result_object$result$data))
      
      # update the progress bar
      if (qbms_globals$config$verbose) { utils::setTxtProgressBar(pb, n + 1) }
    }
    
    if (qbms_globals$config$verbose) {
      utils::setTxtProgressBar(pb, last_page + 1)
      close(pb)
    }
  }

  if (ncol(result_data) == 1) {
    result_object$result$data <- result_data[,1]
  } else {
    result_object$result$data <- result_data
  }
  
  result_data <- result_object$result
  
  qbms_globals$state$current_page <- result_object$metadata$pagination$currentPage
  qbms_globals$state$page_size    <- result_object$metadata$pagination$pageSize
  qbms_globals$state$total_count  <- result_object$metadata$pagination$totalCount
  qbms_globals$state$total_pages  <- result_object$metadata$pagination$totalPages
  qbms_globals$state$errors       <- result_object$errors

  return(result_data)
}

if (requireNamespace("async", quietly = TRUE)) {
  brapi_get_call <- function(call_url, nested = TRUE) {
    separator <- if (grepl("\\?", call_url)) "&" else "?"
    full_url  <- paste0(call_url, separator, "page=0&pageSize=", qbms_globals$config$page_size)
    
    result_object <- async::synchronise(get_async_page(full_url, nested))
    result_data   <- as.data.frame(result_object$result$data)
    total_pages   <- result_object$metadata$pagination$totalPages
    if (total_pages > 1) {
      pages <- c(seq(1, total_pages - 1))
      full_urls <- paste0(call_url, separator, "page=", pages, "&pageSize=", qbms_globals$config$page_size)
      all_pages <- async::synchronise(get_async_pages(full_urls, nested))

      for (n in 1:(total_pages - 1)) {
        result_data <- rbindx(result_data, as.data.frame(all_pages[[n]]$result$data))
      }
    }
  
    if (ncol(result_data) == 1) {
      result_object$result$data <- result_data[,1]
    } else {
      result_object$result$data <- result_data
    }
    
    result_data <- result_object$result
    
    qbms_globals$state$current_page <- result_object$metadata$pagination$currentPage
    qbms_globals$state$page_size    <- result_object$metadata$pagination$pageSize
    qbms_globals$state$total_count  <- result_object$metadata$pagination$totalCount
    qbms_globals$state$total_pages  <- result_object$metadata$pagination$totalPages
    qbms_globals$state$errors       <- result_object$errors
    
    return(result_data)
  }
}


brapi_post_search_call <- function(call_url, call_body, nested = TRUE) {
  headers  <- brapi_headers()
  call_url <- utils::URLencode(call_url)
  
  response <- httr::POST(url = call_url, body = call_body,
                         encode = "raw", httr::accept_json(), httr::content_type_json(),
                         httr::add_headers(headers), httr::timeout(qbms_globals$config$time_out))
  
  results <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), flatten = !nested)
  
  # https://plant-breeding-api.readthedocs.io/en/latest/docs/best_practices/Search_Services.html#post-search-entity
  if (response$status_code == 202) {
    repeat {
      Sys.sleep(qbms_globals$config$sleep)
      
      searchResultsDbId <- results$result$searchResultsDbId
      
      response <- httr::GET(url = paste(call_url, searchResultsDbId, sep = "/"), 
                            httr::add_headers(headers), httr::timeout(qbms_globals$config$time_out))
      
      results <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), flatten = !nested)
      
      if (response$status_code == 200) {
        break
      }
    }
  }
  
  return(results)
}


#' Login Pop-Up Window
#'
#' @description
#' Builds a GUI pop-up window using Tcl/Tk to insert the username and password.
#'
#' @return
#' A vector of inserted username and password.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}

get_login_details <- function() {
  if (is.null(qbms_globals$config$engine)) {
    stop("No server has been defined yet! You have to set your server configurations first using the `set_qbms_config()` function")
  }
  
  if (qbms_globals$config$engine == "bms") { server <- "BMS" }
  if (qbms_globals$config$engine == "breedbase") { server <- "BreedBase" }
  if (qbms_globals$config$engine == "gigwa") { server <- "GIGWA" }
  
  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, paste("Login", server, "Server"))

  ss <- paste("Please enter your", server, "login details")
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


#' Set Access Token Response
#'
#' @description
#' If the request for an access token is valid, the authorization server needs 
#' to generate an access token and return these to the client, typically along 
#' with some additional properties about the authorization.
#'
#' @param token The access token string as issued by the authorization server.
#' @param user The username (optional).
#' @param expires_in The lifetime in seconds of the access token (optional).
#' 
#' @return
#' No return value.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @export

set_token <- function(token, user = '', expires_in = NULL) {
  if (is.null(expires_in)) {
    expires_in <- as.numeric(Sys.time()) + 3600
  }
  
  qbms_globals$state$token <- token
  qbms_globals$state$user  <- user
  qbms_globals$state$expires_in <- expires_in
}


#' Login Using OAuth 2.0 Authentication 
#'
#' @description
#' If the request for an access token is valid, the authorization server needs 
#' to generate an access token and return these to the client, typically along 
#' with some additional properties about the authorization.
#'
#' @param authorize_url URL to send the client for authorization.
#' @param access_url URL used to exchange unauthenticated for authenticated token.
#' @param client_id Consumer key, also sometimes called the client ID.
#' @param client_secret Consumer secret, also sometimes called the client secret.
#' @param redirect_uri The URL that the user will be redirected to after authorization is complete (default is http://localhost:1410).
#' @param oauth2_cache A logical value or a string. TRUE means to cache using the default cache file .httr-oauth, FALSE means don't cache, 
#'                     and NA means to guess using some sensible heuristics. A string means use the specified path as the cache file.
#'                     Default is FALSE (i.e., don't cache).
#' 
#' @return
#' No return value.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @export

login_oauth2 <- function(authorize_url, access_url, client_id, client_secret = NULL, redirect_uri = "http://localhost:1410", oauth2_cache = FALSE) {
  app <- httr::oauth_app(appname = "QBMS", key = client_id, secret = client_secret, redirect_uri = redirect_uri)
  
  endpoint <- httr::oauth_endpoint(authorize = authorize_url, access = access_url)
  
  token <- httr::oauth2.0_token(endpoint, app, cache = oauth2_cache)
  
  set_token(token$credentials$id_token, '', token$credentials$expires_in)
}


#' Login to the Server
#'
#' @description
#' Connects to the server. If the username or password parameters are missing,
#' then a login window will pop up to insert the username and password.
#'
#' All other connection parameters (i.e., server IP or domain, connection port,
#' API path, and connection protocol e.g., http://) will be retrieved from the
#' qbms_config list.
#'
#' This function will update both the qbms_config list (brapi connection
#' object in the con key) and the qbms_state list (token value in the token key).
#'
#' @param username The username (optional, default is NULL).
#' @param password The password (optional, default is NULL).
#' @param encoding How should the named list body be encoded? Can be one of form 
#'                 (application/x-www-form-urlencoded), multipart (multipart/form-data), 
#'                 or json (application/json).
#' 
#' @return
#' No return value.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @examples
#' if(interactive()) {
#'   # Configure your BMS connection
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'
#'   # Login using your BMS account (interactive mode)
#'   # You can pass the BMS username and password as parameters (batch mode)
#'   login_bms()
#' }
#' 
#' @export

login_bms <- function(username = NULL, password = NULL, encoding = "json") {
  if (is.null(username) || is.null(password)) {
    credentials <- get_login_details()
  } else {
    credentials <- c(usr = username, pwd = password)
  }

  call_url  <- paste0(qbms_globals$config$base_url, "/brapi/v1/token")
  call_body <- list(username = credentials["usr"], password = credentials["pwd"])

  response <- httr::POST(url = utils::URLencode(call_url), body = call_body, encode = encoding,
                         httr::timeout(qbms_globals$config$time_out))

  if (!is.null(httr::content(response)$errors)) {
    stop(httr::content(response)$errors[[1]]$message)
  }

  set_token(httr::content(response)$access_token,
            httr::content(response)$userDisplayName,
            httr::content(response)$expires_in)
}


#' Login to the BreedBase Server
#'
#' @description
#' Logs in to the BreedBase server.
#'
#' @param username The username (optional, default is NULL).
#' @param password The password (optional, default is NULL).
#' 
#' @return
#' No return value.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @export

login_breedbase <- function(username = NULL, password = NULL) {
  login_bms(username, password, encoding = "form")
  
  if (is.null(qbms_globals$state$token)) {
    stop("Bad credentials") 
  }
}


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
    call_url  <- paste0(qbms_globals$config$base_url, "/brapi/", qbms_globals$config$brapi_ver, "/", 
                        brapi_map[brapi_map$func_name == "list_crops" & brapi_map$brapi_ver == qbms_globals$config$brapi_ver, "brapi_call"])
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
    call_url <- paste0(qbms_globals$config$base_url, 
                       ifelse(qbms_globals$config$crop == "", "", paste0("/", qbms_globals$config$crop)), 
                       "/brapi/", qbms_globals$config$brapi_ver, "/", 
                       brapi_map[brapi_map$func_name == "list_programs" & brapi_map$brapi_ver == qbms_globals$config$brapi_ver, "brapi_call"])

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
    call_url <- paste0(qbms_globals$config$base_url, 
                       ifelse(qbms_globals$config$crop == "", "", paste0("/", qbms_globals$config$crop)), 
                       "/brapi/", qbms_globals$config$brapi_ver, "/", 
                       brapi_map[brapi_map$func_name == "get_program_trials" & brapi_map$brapi_ver == qbms_globals$config$brapi_ver, "brapi_call"])
    
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
    call_url <- paste0(qbms_globals$config$base_url, 
                       ifelse(qbms_globals$config$crop == "", "", paste0("/", qbms_globals$config$crop)), 
                       "/brapi/", qbms_globals$config$brapi_ver, "/", 
                       brapi_map[brapi_map$func_name == "list_studies" & brapi_map$brapi_ver == qbms_globals$config$brapi_ver, "brapi_call"])
    
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
  
  call_url <- paste0(qbms_globals$config$base_url, 
                     ifelse(qbms_globals$config$crop == "", "", paste0("/", qbms_globals$config$crop)), 
                     "/brapi/", qbms_globals$config$brapi_ver, "/", 
                     brapi_map[brapi_map$func_name == "get_study_info" & brapi_map$brapi_ver == qbms_globals$config$brapi_ver, "brapi_call"])
  
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
  
  call_url <- paste0(qbms_globals$config$base_url, 
                     ifelse(qbms_globals$config$crop == "", "", paste0("/", qbms_globals$config$crop)), 
                     "/brapi/", qbms_globals$config$brapi_ver, "/", 
                     brapi_map[brapi_map$func_name == "get_study_data" & brapi_map$brapi_ver == qbms_globals$config$brapi_ver, "brapi_call"])
  
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

  call_url <- paste0(qbms_globals$config$base_url, 
                     ifelse(qbms_globals$config$crop == "", "", paste0("/", qbms_globals$config$crop)), 
                     "/brapi/", qbms_globals$config$brapi_ver, "/", 
                     brapi_map[brapi_map$func_name == "get_germplasm_list" & brapi_map$brapi_ver == qbms_globals$config$brapi_ver, "brapi_call"])
  
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
    call_url <- paste0(qbms_globals$config$base_url, 
                       ifelse(qbms_globals$config$crop == "", "", paste0("/", qbms_globals$config$crop)), 
                       "/brapi/", qbms_globals$config$brapi_ver, "/", 
                       brapi_map[brapi_map$func_name == "get_trial_obs_ontology" & brapi_map$brapi_ver == qbms_globals$config$brapi_ver, "brapi_call"])
    
    ontology <- brapi_get_call(call_url)$data
    
    qbms_globals$state$variables <- ontology
  }
  
  ontology <- ontology[ontology$observationVariableDbId %in% qbms_globals$state$observationVariableDbIds, ]

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
    call_url <- paste0(qbms_globals$config$base_url, 
                       ifelse(qbms_globals$config$crop == "", "", paste0("/", qbms_globals$config$crop)), 
                       "/brapi/", qbms_globals$config$brapi_ver, "/", 
                       brapi_map[brapi_map$func_name == "list_locations" & brapi_map$brapi_ver == qbms_globals$config$brapi_ver, "brapi_call"])

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
  
  call_url <- paste0(qbms_globals$config$base_url,
                     ifelse(qbms_globals$config$crop == "", "", paste0("/", qbms_globals$config$crop)),
                     "/brapi/", qbms_globals$config$brapi_ver, "/",
                     brapi_map[brapi_map$func_name == "get_germplasm_id" & brapi_map$brapi_ver == qbms_globals$config$brapi_ver, "brapi_call"])

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
  
  call_url <- paste0(qbms_globals$config$base_url, 
                     ifelse(qbms_globals$config$crop == "", "", paste0("/", qbms_globals$config$crop)), 
                     "/brapi/", qbms_globals$config$brapi_ver, "/", 
                     brapi_map[brapi_map$func_name == "get_germplasm_attributes" & brapi_map$brapi_ver == qbms_globals$config$brapi_ver, "brapi_call"])
  
  call_url <- sub("\\{germplasmDbId\\}", germplasm_db_id, call_url)
  
  results <- brapi_get_call(call_url)$data

  return(results)
}


## Pedigree ###############################################################################

#' Get Direct Parents
#'
#' @description
#' Internal helper function to split the given pedigree string that provides the parentage
#' through which a cultivar was obtained and retrieve the pedigrees of the direct parents.
#'
#' @param pedigree A string providing the parentage through which a cultivar was obtained.
#' 
#' @return 
#' A vector of two items, representing the direct female and male parents.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}

get_parents <- function(pedigree) {
  # make sure it is a string
  pedigree <- as.character(pedigree)

  # 1. we did not expect cross depth to be more than two digits (up to 99)
  cross <- regmatches(pedigree, gregexpr("/[0-9]{1,2}/", pedigree))

  if (length(cross[[1]]) != 0) {
    # find the highest cross order to cut at it and get parents sub-pedigree
    last_cross <- max(as.numeric(gsub("/", "", cross[[1]])))
    parents    <- regmatches(pedigree, regexpr(paste0("/", last_cross, "/"), pedigree), invert = TRUE)[[1]]
  } else {
    # 2. if it is not of type /#/, then try double backslash //
    cross <- regmatches(pedigree, gregexpr("//", pedigree))

    if (length(cross[[1]]) != 0) {
      # get parents sub-pedigree if it is crossed using //
      parents <- regmatches(pedigree, regexpr("//", pedigree), invert = TRUE)[[1]]
    } else {
      # 3. if it is not // then try with single backslash /
      cross <- regmatches(pedigree, gregexpr("/", pedigree))

      if (length(cross[[1]]) != 0) {
        # get parents names
        parents <- regmatches(pedigree, regexpr("/", pedigree), invert = TRUE)[[1]]
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
#' Internal helper function to build the pedigree table recursively.
#'
#' @param geno_list A list of genotypes/germplasm names.
#' @param pedigree_list A list of associated pedigree strings.
#' @param pedigree_df Pedigree data.frame as per the previous call/iteration.
#' 
#' @return 
#' A data.frame with three columns corresponding to the identifiers for the 
#' individual, female parent, and male parent, respectively.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}

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
    geno_list     <- tolower(iconv(geno_list, 'WINDOWS-1252', 'UTF-8'))
    pedigree_list <- tolower(iconv(pedigree_list, 'WINDOWS-1252', 'UTF-8'))
  }

  # create an empty dummy list of previous generation parents
  prev_generation <- c()

  # extract the parents of each genotype/germplasm in the given list
  for (i in 1:length(geno_list)) {
    geno    <- as.character(geno_list[i])
    cross   <- as.character(pedigree_list[i])
    parents <- get_parents(cross)

    # check for backcross cases and handle them properly
    female_bc <- regmatches(parents[1], regexec("(.+)\\*(\\d+)$", parents[1]))

    if (length(female_bc[[1]]) != 0) {
      n <- as.numeric(female_bc[[1]][3])
      if (n > 2) {
        parents <- c(female_bc[[1]][2], sub(parents[1], paste0(female_bc[[1]][2], "*", n - 1), cross, fixed = TRUE))
      } else {
        parents <- c(female_bc[[1]][2], sub(parents[1], female_bc[[1]][2], cross, fixed = TRUE))
      }
    } else {
      male_bc <- regmatches(parents[2], regexec("^(\\d+)\\*(.+)", parents[2]))
      
      if (length(male_bc[[1]]) != 0) {
        n <- as.numeric(male_bc[[1]][2])
        if (n > 2) {
          parents <- c(sub(parents[2], paste0(n - 1, "*", male_bc[[1]][3]), cross, fixed = TRUE), male_bc[[1]][3])
        } else {
          parents <- c(sub(parents[2], male_bc[[1]][3], cross, fixed = TRUE), male_bc[[1]][3])
        }
      }
    }
    
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
#' Retrieve the pedigree table starting from the current germplasm list and associated
#' pedigree string that provides the parentage through which a cultivar was obtained.
#'
#' @param data Germplasm dataset as a data.frame.
#' @param geno_column Name of the column that identifies the genotype/germplasm names.
#' @param pedigree_column Name of the column that identifies the pedigree strings.
#' 
#' @return 
#' A data.frame with three columns corresponding to the identifiers for the individual,
#' female parent, and male parent, respectively. The row giving the pedigree of an
#' individual appears before any row where that individual appears as a parent.
#' Founders use NA in the parental columns.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @examples
#' if (interactive()) {
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
#'
#'   pedigree_table <- get_pedigree_table(germplasm, "germplasmName", "pedigree")
#'
#'   #############################
#'   # nadiv package way
#'   # library(nadiv)
#'
#'   # Get additive relationship matrix in sparse matrix format
#'   # A <- nadiv::makeA(pedigree_table)
#'
#'   # Get A inverse matrix using base R function
#'   # AINV <- solve(as.matrix(A))
#'
#'   #############################
#'   # ASReml-R package way
#'   # library(asreml)
#'
#'   # Represent A inverse matrix in an efficient way using i, j index and Ainverse value
#'   # Actual genotype names of any given index are in the attr(ainv, "rowNames")
#'   # ainv <- asreml::ainverse(pedigree_table)
#'
#'   #############################
#'   # Dummy data set for testing
#'   test <- data.frame(genotype = c("X", "Y"),
#'                      pedigree = c("A//B/D/2/C", "B/C/3/A//B/C/2/D"))
#'
#'   pedigree_table <- get_pedigree_table(test, "genotype", "pedigree")
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

  # if there are cases of similar genotype names
  if (nrow(check) > 0) {
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
  }

  # get the final pedigree data.frame using updated/audited lists of geno_list and pedigree_list
  pedigree_df <- build_pedigree_table(geno_list, pedigree_list)

  return(pedigree_df)
}


## GIGWA #######################################################################

#' Login to the GIGWA Server
#'
#' @description
#' Connect to the GIGWA server. If the username or password parameters are missing,
#' a login window will pop up to insert the username and password.
#'
#' All other connection parameters (i.e., server IP or domain, connection port,
#' API path, and connection protocol e.g., http://) will be retrieved from the
#' `qbms_config` list.
#'
#' This function will update both the qbms_config list (brapi connection
#' object in the con key) and qbms_state list (token value in the token key).
#'
#' @param username The GIGWA username (optional, default is NULL).
#' @param password The GIGWA password (optional, default is NULL).
#' 
#' @return 
#' No return value.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @examples
#' if (interactive()) {
#'   # Configure your GIGWA connection
#'   set_qbms_config("http://localhost:59395/gigwa/index.jsp", time_out = 300, engine = "gigwa")
#'
#'   # Login using your GIGWA account (interactive mode)
#'   login_gigwa()
#'   
#'   # You can pass GIGWA username and password as parameters (batch mode)
#'   # login_gigwa("gigwadmin", "nimda")
#' }
#' @export

login_gigwa <- function(username = NULL, password = NULL) {
  if (is.null(username) || is.null(password)) {
    credentials <- get_login_details()
  } else {
    credentials <- c(usr = username, pwd = password)
  }
  
  call_url  <- paste0(qbms_globals$config$base_url, "/gigwa/generateToken")
  call_body <- list(username = credentials["usr"], password = credentials["pwd"])
  
  response <- httr::POST(url = utils::URLencode(call_url), body = call_body, encode = "json",
                         httr::timeout(qbms_globals$config$time_out))
  
  if (response$status_code == 403 || credentials["usr"] == "" || credentials["pwd"] == "") {
    stop("403 Forbidden")
  }
  
  set_token(httr::content(response)$token)
}


#' List GIGWA Databases
#' 
#' @description
#' Get the list of existing databases in the current GIGWA server.
#'
#' @return 
#' A list of existing databases.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso 
#' \code{\link{set_qbms_config}}
#' 
#' @examples
#' if (interactive()) {
#'   # Configure your GIGWA connection
#'   set_qbms_config("https://gigwa.southgreen.fr/gigwa/", 
#'                   time_out = 300, engine = "gigwa", no_auth = TRUE)
#'
#'   # List existing databases in the GIGWA server
#'   gigwa_list_dbs()
#' }
#' @export

gigwa_list_dbs <- function() {
  if (is.null(qbms_globals$state$token)) {
    stop("No server has been connected yet! You have to connect a server first using the `login_gigwa()` function")
  }
  
  call_url <- paste0(qbms_globals$config$base_url, "/brapi/v2/programs")
  
  gigwa_dbs <- brapi_get_call(call_url)$data
  
  return(gigwa_dbs)
}


#' Set the Current Active GIGWA Database by Name
#'
#' @description
#' This function updates the current active database in the internal
#' configuration object (including the brapi connection object).
#'
#' @param db_name The name of the database.
#' 
#' @return 
#' No return value.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso 
#' \code{\link{set_qbms_config}}, \code{\link{gigwa_list_dbs}}
#' 
#' @examples
#' if (interactive()) {
#'   # Configure your GIGWA connection
#'   set_qbms_config("https://gigwa.southgreen.fr/gigwa/", 
#'                   time_out = 300, engine = "gigwa", no_auth = TRUE)
#'
#'   # Select a database by name
#'   gigwa_set_db("Sorghum-JGI_v1")
#' }
#' @export

gigwa_set_db <- function(db_name) {
  valid_dbs <- gigwa_list_dbs()
  
  if (!db_name %in% valid_dbs) {
    stop("Your database name is not exists in this connected GIGWA server! You may use the `gigwa_list_dbs()` function to check the available databases")
  }
  
  qbms_globals$config$db <- db_name
  
  qbms_globals$state$gigwa_projects <- NULL
}


#' Get the List of All Projects in the Selected GIGWA Database
#'
#' @description
#' Retrieves the projects list from the currently active database as configured 
#' in the internal configuration object using the `gigwa_set_db()` function.
#'
#' @return 
#' A list of project names.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso 
#' \code{\link{set_qbms_config}}, \code{\link{gigwa_set_db}}
#' 
#' @examples
#' if (interactive()) {
#'   # Configure your GIGWA connection
#'   set_qbms_config("https://gigwa.southgreen.fr/gigwa/", 
#'                   time_out = 300, engine = "gigwa", no_auth = TRUE)
#'
#'   # Select a database by name
#'   gigwa_set_db("Sorghum-JGI_v1")
#'
#'   # List existing projects
#'   gigwa_list_projects()
#' }
#' @export

gigwa_list_projects <- function() {
  if (is.null(qbms_globals$state$token)) {
    stop("No server has been connected yet! You have to connect a GIGWA server first using the `login_gigwa()` function")
  }
  
  if (is.null(qbms_globals$config$db)) {
    stop("No database has been selected yet! You have to set your database first using the `gigwa_set_db()` function")
  }
  
  if (!is.null(qbms_globals$state$gigwa_projects)) {
    gigwa_projects <- qbms_globals$state$gigwa_projects
  } else {
    call_url <- paste0(qbms_globals$config$base_url, "/brapi/v2/studies?programDbId=", qbms_globals$config$db)
    
    gigwa_projects <- brapi_get_call(call_url)$data
    
    gigwa_projects <- gigwa_projects[, c("studyName", "studyDbId")]
    
    qbms_globals$state$gigwa_projects <- gigwa_projects
  }

  return(gigwa_projects[c("studyName")])
}


#' Set the Current Active GIGWA Project
#'
#' @description
#' This function updates the current active project in the internal state object 
#' using the programDbId retrieved from GIGWA which is associated with the given 
#' `project_name` parameter.
#'
#' @param project_name The name of the project.
#' 
#' @return 
#' No return value.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso 
#' \code{\link{set_qbms_config}}, \code{\link{gigwa_set_db}}, \code{\link{gigwa_list_projects}}
#' 
#' @examples
#' if (interactive()) {
#'   # Configure your GIGWA connection
#'   set_qbms_config("https://gigwa.southgreen.fr/gigwa/", 
#'                   time_out = 300, engine = "gigwa", no_auth = TRUE)
#'
#'   # Select a database by name
#'   gigwa_set_db("Sorghum-JGI_v1")
#'
#'   # Select a project by name
#'   gigwa_set_project("Nelson_et_al_2011")
#' }
#' @export

gigwa_set_project <- function(project_name) {
  valid_projects <- gigwa_list_projects()
  
  if (!project_name %in% valid_projects[,1]) {
    stop("Your project name is not exists in this database! You may use the `gigwa_list_projects()` function to check the available projects")
  }
  
  gigwa_projects <- qbms_globals$state$gigwa_projects

  project_row <- which(gigwa_projects$studyName == project_name)
  
  qbms_globals$state$study_db_id <- gigwa_projects[project_row, "studyDbId"]
  
  qbms_globals$state$genotyping_runs <- NULL
}


#' Get the List of the Run Names Available in the Selected GIGWA Project
#'
#' @description
#' This function retrieves the runs list from the currently active project as configured 
#' in the internal configuration object using the `gigwa_set_project()` function.
#'
#' @return 
#' A list of run names.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso 
#' \code{\link{set_qbms_config}}, \code{\link{gigwa_set_project}}
#' 
#' @examples
#' if (interactive()) {
#'   # Configure your GIGWA connection
#'   set_qbms_config("https://gigwa.southgreen.fr/gigwa/", 
#'                   time_out = 300, engine = "gigwa", no_auth = TRUE)
#'
#'   # Select a database by name
#'   gigwa_set_db("Sorghum-JGI_v1")
#'
#'   # Select a project by name
#'   gigwa_set_project("Nelson_et_al_2011")
#'   
#'   # List all runs in the selected project
#'   genotyping_list_runs()
#' }
#' @export

genotyping_list_runs <- function() {
  if (is.null(qbms_globals$state$study_db_id)) {
    stop("No project has been selected yet! You have to set your project first using the `gigwa_set_project()` function")
  }

  if (!is.null(qbms_globals$state$genotyping_runs)) {
    genotyping_runs <- qbms_globals$state$genotyping_runs
  } else {
    call_url  <- paste0(qbms_globals$config$base_url, "/brapi/v2/search/variantsets")
    call_body <- paste0('{"studyDbIds": ["', qbms_globals$state$study_db_id, '"]}')
    
    results <- brapi_post_search_call(call_url, call_body, FALSE)

    genotyping_runs <- as.data.frame(results$result$data[, c("variantSetName", "variantSetDbId")])
    
    qbms_globals$state$genotyping_runs <- genotyping_runs
  }
  
  return(genotyping_runs[c("variantSetName")])
}


#' Set the Current Active Genotyping Run (i.e., variant set)
#'
#' @description
#' This function updates the current active run in the internal state object using the 
#' `studyDbIds` retrieved from database, which are associated with the given `run_name` parameter.
#'
#' @param run_name The name of the run.
#' 
#' @return 
#' No return value.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso 
#' \code{\link{set_qbms_config}}, \code{\link{gigwa_set_project}}, \code{\link{genotyping_list_runs}}
#' 
#' @examples
#' if (interactive()) {
#'   # Configure your GIGWA connection
#'   set_qbms_config("https://gigwa.southgreen.fr/gigwa/", 
#'                   time_out = 300, engine = "gigwa", no_auth = TRUE)
#'
#'   # Select a database by name
#'   gigwa_set_db("Sorghum-JGI_v1")
#'
#'   # Select a project by name
#'   gigwa_set_project("Nelson_et_al_2011")
#'   
#'   # Select a specific run by name
#'   genotyping_set_run("run1")
#' }
#' @export

genotyping_set_run <- function(run_name) {
  valid_runs <- genotyping_list_runs()
  
  if (!run_name %in% unlist(valid_runs)) {
    stop("Your run name is not exists in this project! You may use the `genotyping_list_runs()` function to check the available runs")
  }

  genotyping_runs <- qbms_globals$state$genotyping_runs
  
  qbms_globals$state$variant_set_db_id <- genotyping_runs[genotyping_runs$variantSetName == run_name, "variantSetDbId"]
  
  qbms_globals$state$genotyping_samples <- NULL
}


#' Get the Samples List of the Current Active Run (i.e., variant set)
#'
#' @description
#' This function retrieves the samples list of the current active run
#' as configured in the internal state object using the `genotyping_set_run()` function.
#'
#' @return 
#' A vector of all samples in the selected run.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso 
#' \code{\link{set_qbms_config}}, \code{\link{genotyping_set_run}}
#' 
#' @examples
#' if (interactive()) {
#'   # Configure your GIGWA connection
#'   set_qbms_config("https://gigwa.southgreen.fr/gigwa/", 
#'                   time_out = 300, engine = "gigwa", no_auth = TRUE)
#'
#'   # Select a database by name
#'   gigwa_set_db("Sorghum-JGI_v1")
#'
#'   # Select a project by name
#'   gigwa_set_project("Nelson_et_al_2011")
#'   
#'   # Select a specific run by name
#'   genotyping_set_run("run1")
#'   
#'   # Get a list of all samples in the selected run
#'   samples <- genotyping_list_samples()
#' }
#' @export

genotyping_list_samples <- function() {
  if (is.null(qbms_globals$state$study_db_id)) {
    stop("No project has been selected yet! You have to set your project first using the `gigwa_set_project()` function")
  }
  
  if (!is.null(qbms_globals$state$genotyping_samples)) {
    genotyping_samples <- qbms_globals$state$genotyping_samples
  } else {
    call_url  <- paste0(qbms_globals$config$base_url, "/brapi/v2/search/germplasm")
    call_body <- paste0('{"studyDbIds": ["', qbms_globals$state$study_db_id, '"]}')
    
    results <- brapi_post_search_call(call_url, call_body, FALSE)
    
    genotyping_samples <- results$result$data[, c("germplasmName", "germplasmDbId")]
    
    qbms_globals$state$genotyping_samples <- genotyping_samples
  }

  return(genotyping_samples$germplasmName)
}


#' Get Available Variants in the Selected GIGWA Run
#'
#' @description
#' Query the variants (e.g., SNPs markers) in the selected GIGWA run that match a given criteria.
#' 
#' @param max_missing Maximum missing ratio (by sample) between 0 and 1 (default is 1 for 100\%).
#' @param min_maf Minimum Minor Allele Frequency (MAF) between 0 and 1 (default is 0 for 0\%).
#' @param samples A list of samples subset (default is NULL, which will retrieve for all samples).
#' @param start Start position of region (zero-based, inclusive) (e.g., 19750802).
#' @param end End position of region (zero-based, exclusive)	(e.g., 19850125).
#' @param referenceName Reference sequence name	(e.g., '6H' in the Barley LI-AM).
#' 
#' @return 
#' A data.frame that has the first 4 columns describing attributes of the SNP 
#' (rs#: variant name, alleles: reference allele / alternative allele, chrom: chromosome name, 
#' and pos: position in bp), while the following columns describe the SNP value for a 
#' single sample line using numerical coding 0, 1, and 2 for reference, heterozygous, and 
#' alternative/minor alleles.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @examples
#' if (interactive()) {
#'   # Configure your GIGWA connection
#'   set_qbms_config("https://gigwa.southgreen.fr/gigwa/", 
#'                   time_out = 300, engine = "gigwa", no_auth = TRUE)
#'
#'   # Select a database by name
#'   gigwa_set_db("Sorghum-JGI_v1")
#'
#'   # Select a project by name
#'   gigwa_set_project("Nelson_et_al_2011")
#'   
#'   # Select a specific run by name
#'   genotyping_set_run("run1")
#'   
#'   marker_matrix <- gigwa_get_variants(max_missing = 0.2, 
#'                                       min_maf = 0.35, 
#'                                       samples = c("ind1", "ind3", "ind7"))
#' }
#' @export

gigwa_get_variants <- function(max_missing = 1, min_maf = 0.5, samples = NULL, start = NULL, end = NULL, referenceName = NULL) {
  if (is.null(qbms_globals$state$study_db_id)) {
    stop("No project has been selected yet! You have to set your project first using the `gigwa_set_project()` function")
  }
  
  if (max_missing < 0 || max_missing > 1) {
    stop("The accepted `max_missing` value for the max missing data can range from 0 to 1")
  }
  
  if (min_maf < 0 || min_maf > 0.5) {
    stop("The accepted `maf` value for the minimum minor allele frequency can range from 0 to 0.5")
  }

  if (!is.null(samples)) {
    available_samples <- genotyping_list_samples()
    missing_samples <- setdiff(samples, available_samples)
    
    if (length(missing_samples) > 0) {
      stop("Some samples are not exists in this project! You may use the `genotyping_list_samples()` function to check the available samples")
    }
  } else {
    samples <- genotyping_list_samples()
  }
  
  if (!is.null(start) && !is.numeric(start)){
    stop("Start position should be numeric!")
  }
  
  if (!is.null(end) && !is.numeric(end)){
    stop("End position should be numeric!")
  }
  
  # https://gigwa-dev.southgreen.fr/gigwaV2/rest/swagger-ui/index.html?urls.primaryName=GA4GH%20API%20v0.6.0a5#/ga-4gh-rest-controller/searchVariantsUsingPOST
  # https://rest.ensembl.org/documentation/info/gavariants
  # https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.1#/Allele%20Matrix/post_search_allelematrix
  # https://brapigenotyping21.docs.apiary.io/#/reference/allele-matrix

  call_url <- paste0(qbms_globals$config$base_url, "/ga4gh/variants/search")
  
  auth_code <- paste0("Bearer ", qbms_globals$state$token)
  headers   <- c("Authorization" = auth_code, "Accept-Encoding" = "gzip, deflate")
  
  call_body <- list(alleleCount = "2",
                    searchMode = 0,
                    variantSetId = qbms_globals$state$study_db_id,
                    callSetIds = paste0(qbms_globals$state$study_db_id, "\u00A7", samples),
                    minmaf = min_maf * 100,
                    maxmaf = 50,
                    missingData = max_missing * 100)
  
  if (!is.null(referenceName)) call_body$referenceName <- referenceName
  if (!is.null(start)) call_body$start <- start
  if (!is.null(end)) call_body$end <- end
  
  response <- httr::POST(url = utils::URLencode(call_url), body = call_body, encode = "json", 
                         httr::add_headers(headers), httr::timeout(qbms_globals$config$time_out))
  
  results <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)
  
  total_variants <- results$count

  if (total_variants == 0) {
    stop("No variants match your filtering criteria!")
  }
  
  # setup the progress bar
  pb <- utils::txtProgressBar(min = 0, max = total_variants, initial = 0, style = 3) 
  pb_step <- round(total_variants/100)
  
  call_body <- list(alleleCount = "2",
                    searchMode = 3,
                    variantSetId = qbms_globals$state$study_db_id,
                    callSetIds = paste0(qbms_globals$state$study_db_id, "\u00A7", samples),
                    minmaf = min_maf * 100,
                    maxmaf = 50,
                    missingData = max_missing * 100,
                    getGT = TRUE,
                    pageSize = qbms_globals$config$page_size,
                    pageToken = "0")
  
  if (!is.null(referenceName)) call_body$referenceName <- referenceName
  if (!is.null(start)) call_body$start <- start
  if (!is.null(end)) call_body$end <- end
  
  g_matrix <- data.frame(matrix(ncol = length(samples) + 4, nrow = 0))

  repeat {
    repeat {
      # avoid MongoDB error because of a background operation is still running!
      # get the progress status of a process from its token. If no current process is associated with this token, returns null.
      # https://gigwa.southgreen.fr/gigwa/rest/swagger-ui/index.html?urls.primaryName=Gigwa%20API%20v2.5-RELEASE#/gigwa-rest-controller/getProcessProgressUsingGET
      response <- httr::GET(url = paste0(qbms_globals$config$base_url, "/gigwa/progress"), 
                            httr::add_headers(headers), httr::timeout(qbms_globals$config$time_out))

      if (httr::content(response, as = "text", encoding = "UTF-8") == "") {
        break
      }
    }

    response <- httr::POST(url = utils::URLencode(call_url), body = call_body, encode = "json", 
                           httr::add_headers(headers), httr::timeout(qbms_globals$config$time_out))

    results <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)
    
    n <- nrow(results$variants)

    for(i in 1:n){
      snp_name <- results$variants[i, "id"]
      alleles  <- paste0(results$variants[i, "referenceBases"], "/", results$variants[i, "alternateBases"])
      chrom    <- results$variants[i, "referenceName"]
      pos      <- results$variants[i, "start"]
      genotype <- unlist(lapply(results$variants[i, "calls"][[1]]$genotype, function(x){ ifelse(length(x) == 0, NA, sum(x)) }))
      g_matrix <- rbind(g_matrix, c(snp_name, alleles, chrom, pos, genotype))
    }

    # update the progress bar
    utils::setTxtProgressBar(pb, nrow(g_matrix))
    
    if (!exists("nextPageToken", results)) {
      break
    }

    call_body$pageToken <- results$nextPageToken
    call_body$searchMode <- 2
  }

  utils::setTxtProgressBar(pb, total_variants)
  close(pb)
  
  suppressWarnings(g_matrix[,-c(1:4)] <- as.data.frame(sapply(g_matrix[,-c(1:4)], as.numeric)))
  
  g_matrix[, 1] <- gsub(paste0(qbms_globals$state$study_db_id, "\u00A7"), "", g_matrix[, 1])
  
  colnames(g_matrix) <- c("rs#", "alleles", "chrom", "pos",
                          gsub(paste0(qbms_globals$state$study_db_id, "\u00A7"), "", results$variants[1, "calls"][[1]]$callSetId))

  return(g_matrix)
}


#' BrAPI Get Allele Matrix
#' 
#' @param samples samples
#' @param start start
#' @param end end
#' @param chrom chrom
#' @param snps snps 
#' @param snps_pageSize snps_pageSize
#' @param samples_pageSize samples_pageSize
#' @param simplify simplify
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @examples
#' if (interactive()) {
#'   # Configure your GIGWA connection
#'   set_qbms_config("https://gigwa.southgreen.fr/gigwa/", 
#'                   time_out = 300, engine = "gigwa", no_auth = TRUE)
#'
#'   # Select a database by name
#'   gigwa_set_db("Sorghum-JGI_v1")
#'
#'   # Select a project by name
#'   gigwa_set_project("Nelson_et_al_2011")
#'   
#'   # Select a specific run by name
#'   gigwa_set_run("run1")
#'   
#'   # Get a list of all samples in the selected run
#'   samples <- genotyping_list_samples()
#'   
#'   chk <- Sys.time()
#'   marker_matrix <- gigwa_get_variants(start = 0, 
#'                                       end = 012345678, 
#'                                       referenceName = "Sb01", 
#'                                       samples = samples)
#'   round(Sys.time()-chk, 2)
#'   
#'   chk <- Sys.time()
#'   geno_data <- brapi_get_allelematrix(samples = samples, 
#'                                       start = 0, 
#'                                       end = 012345678, 
#'                                       chrom = "Sb01", 
#'                                       snps = marker_matrix$`rs#`) 
#'   round(Sys.time()-chk, 2)
#'   
#'   all.equal(geno_data, marker_matrix[,5:12], check.attributes = FALSE)
#' }
#' @export

brapi_get_allelematrix <- function(samples = NULL, start = 0, end = "", chrom = NULL, snps = NULL, 
                                   snps_pageSize = 10000, samples_pageSize = 100, simplify = TRUE) {
  germplasmDbIds  <- ""
  variantDbIds    <- ""
  positionRanges  <- ""
  variantSetDbIds <- paste0('"', qbms_globals$state$variant_set_db_id, '"')
  
  if (is.null(samples)) {
    samples <- genotyping_list_samples()
  }
  
  germplasmNames <- samples
  germplasmDbIds <- paste0('"', paste0(paste0(qbms_globals$config$db, "\u00A7", germplasmNames), collapse = '","'), '"')

  if (!is.null(snps)) {
    variantNames <- snps
    variantDbIds <- paste0('"', paste0(paste0(qbms_globals$config$db, "\u00A7", variantNames), collapse = '","'), '"')
  }
  
  # if chrom is NULL, then we need to get the list from the following BrAPI endpoint (positionRanges can't be empty)
  # https://gigwa.southgreen.fr/gigwa/rest/brapi/v2/references?studyDbId=Sorghum-JGI_v1§1
  # the default start is 0, and the default end is empty string, and the simples positionRanges value is "chrom:0-"
  # variantDbIds masked by positionRanges (i.e., any snp exists outside defined range will be excluded from the results)
  
  referenceStart <- start
  referenceEnd   <- end
  referenceName  <- chrom
  positionRanges <- paste0(referenceName, ":", format(referenceStart, scientific = FALSE), "-", format(referenceEnd, scientific = FALSE))
  positionRanges <- paste0('"', paste0(positionRanges, collapse = '","'), '"')

  variants_pageSize <- snps_pageSize
  callsets_pageSize <- samples_pageSize
  
  variants_page <- 0
  callsets_page <- 0
  
  call_url <- paste0(qbms_globals$config$base_url, "/brapi/v2/search/allelematrix")
  
  post_schema <- paste0('{
                   "dataMatrixAbbreviations": ["GT"],
                   "variantSetDbIds": [', variantSetDbIds, '],
                   "positionRanges":  [', positionRanges, '],
                   "germplasmDbIds":  [', germplasmDbIds, '],
                   "variantDbIds":    [', variantDbIds, '],
                   "pagination": [
                      {"dimension": "variants", "page": {variants_page}, "pageSize": ', variants_pageSize, '}, 
                      {"dimension": "callsets", "page": {callsets_page}, "pageSize": ', callsets_pageSize, '}
                   ]
                }')
  
  call_body <- sub("\\{callsets_page\\}", callsets_page, sub("\\{variants_page\\}", variants_page, post_schema))
  
  results <- brapi_post_search_call(call_url, call_body, FALSE)
  
  pagination <- results$result$pagination
  
  geno_data <- as.data.frame(matrix(nrow = pagination$totalCount[1], ncol = pagination$totalCount[2]))
  
  range_start <- (pagination$page * pagination$pageSize) + 1
  range_end   <- ifelse(pagination$totalPages == (pagination$page + 1), 
                        pagination$totalCount, 
                        (pagination$page + 1) * pagination$pageSize)
  
  page_data <- as.data.frame(results$result$dataMatrices$dataMatrix)
  
  geno_data[range_start[1]:range_end[1], range_start[2]:range_end[2]] <- page_data
  
  resultVariantNames <- results$result$variantDbIds
  
  remaining_pages <- pagination$totalPages[1] * pagination$totalPages[2] - 1
  
  if (remaining_pages > 0) {
    pb <- utils::txtProgressBar(min = 0, max = remaining_pages, initial = 0, style = 3) 
    
    for (i in 0:(pagination$totalPages[1] - 1)) {
      for (j in 0:(pagination$totalPages[2] - 1)) {
        if (i == 0 & j == 0) next
        
        call_body <- sub("\\{callsets_page\\}", j, sub("\\{variants_page\\}", i, post_schema))
        
        results <- brapi_post_search_call(call_url, call_body, FALSE)
        
        pagination <- results$result$pagination
        
        range_start <- (pagination$page * pagination$pageSize) + 1
        range_end   <- ifelse(pagination$totalPages == (pagination$page + 1),
                              pagination$totalCount,
                              (pagination$page + 1) * pagination$pageSize)
        
        page_data <- as.data.frame(results$result$dataMatrices$dataMatrix)
        
        geno_data[range_start[1]:range_end[1], range_start[2]:range_end[2]] <- page_data
        
        if (j == 0) {
          resultVariantNames <- c(resultVariantNames, results$result$variantDbIds)
        }
        
        utils::setTxtProgressBar(pb, i * pagination$totalPages[2] + j)
      }
    }
    
    close(pb)
  }
  
  if (simplify) {
    geno_data <- as.matrix(geno_data)
    
    geno_data[geno_data == "."] <- NA
    geno_data[geno_data == "0"] <- -1
    geno_data[geno_data == "1"] <- 1
    
    geno_data[!geno_data %in% c(NA, -1, 1)] <- 0
    
    geno_data <- as.data.frame(matrix(as.numeric(unlist(geno_data)), 
                                      nrow = pagination$totalCount[1], 
                                      ncol = pagination$totalCount[2]))
    geno_data <- geno_data + 1
  }

  colnames(geno_data) <- germplasmNames
  rownames(geno_data) <- sub(paste0(qbms_globals$config$db, "\u00A7"), "", resultVariantNames)
  
  return(geno_data)
}


#' BrAPI Get Geno Map
#' 
#' @param start start
#' @param end end
#' @param chrom chrom
#' @param simplify simplify 
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @examples
#' if (interactive()) {
#'   # Configure your GIGWA connection
#'   set_qbms_config("https://gigwa.southgreen.fr/gigwa/", 
#'                   time_out = 300, engine = "gigwa", no_auth = TRUE)
#'
#'   # Select a database by name
#'   gigwa_set_db("Sorghum-JGI_v1")
#'
#'   # Select a project by name
#'   gigwa_set_project("Nelson_et_al_2011")
#'   
#'   # Select a specific run by name
#'   genotyping_set_run("run1")
#'   
#'   geno_map <- brapi_get_variants(start = 0, end = 12345678, chrom = c("Sb07"))
#' }
#' @export

brapi_get_variants <- function(start = NULL, end = NULL, chrom = NULL, simplify = TRUE) {
  startParam <- ifelse(is.null(start), "", paste('"start":', format(start, scientific = FALSE), ","))
  endParam   <- ifelse(is.null(end), "", paste('"end":', format(end, scientific = FALSE), ","))
  
  if (is.null(chrom)) {
    referenceDbIds <- ""
  } else {
    referenceNames <- chrom
    referenceDbIds <- paste0('"', paste0(paste0(qbms_globals$state$study_db_id, "\u00A7\u00A7", referenceNames), collapse = '","'), '"')
  }

  call_url <- paste0(qbms_globals$config$base_url, "/brapi/v2/search/variants")
  page     <- 0
  
  post_schema <- paste0('{', startParam, endParam,
                           '"referenceDbIds": [', referenceDbIds,'],
                            "page": {page}, 
                            "pageToken": {page},
                            "pageSize": ', qbms_globals$config$page_size, ',
                            "variantSetDbIds": ["', qbms_globals$state$variant_set_db_id, '"]
                         }')
  
  call_body <- gsub("\\{page\\}", page, post_schema)

  results <- brapi_post_search_call(call_url, call_body, FALSE)
  
  remaining_pages <- with(results$metadata$pagination, ceiling(totalCount/pageSize)) - 1

  geno_map <- as.data.frame(results$result$data)

  if (remaining_pages > 0) {
    pb <- utils::txtProgressBar(min = 0, max = remaining_pages, initial = 0, style = 3) 
    
    for (i in 1:remaining_pages) {
      call_body <- gsub("\\{page\\}", i, post_schema)
      
      results <- brapi_post_search_call(call_url, call_body, FALSE)
      
      page_data <- as.data.frame(results$result$data)
      
      geno_map <- rbind(geno_map, page_data)
      
      utils::setTxtProgressBar(pb, i)
    }
    
    close(pb)
  }

  if (simplify) {
    geno_map$alleles <- paste0(geno_map$referenceBases, "/", geno_map$alternateBases)
    
    geno_map <- geno_map[, c("variantNames", "alleles", "referenceName", "start")]
    geno_map <- geno_map[with(geno_map, order(referenceName, start)),]
    
    colnames(geno_map) <- c("rs#", "alleles", "chrom", "pos")
    rownames(geno_map) <- NULL
  }
  
  return(geno_map)
}


#' Get the Metadata of the Current Germplasm List
#'
#' @description
#' This function retrieves the metadata of the current active run
#' as configured in the internal state object using the `genotyping_set_run()` function.
#'
#' @return 
#' A data.frame of all metadata associated with the samples in the selected run.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso 
#' \code{\link{set_qbms_config}}, \code{\link{genotyping_set_run}}
#' 
#' @examples
#' if (interactive()) {
#'   # Configure your GIGWA connection
#'   set_qbms_config("https://gigwa.southgreen.fr/gigwa/", 
#'                   time_out = 300, engine = "gigwa", no_auth = TRUE)
#'
#'   # Select a database by name
#'   gigwa_set_db("3kG_10M")
#'
#'   # Select a project by name
#'   gigwa_set_project("3003_ind")
#'   
#'   # Select a specific run by name
#'   genotyping_set_run("1")
#'   
#'   # Get a list of all samples in the selected run
#'   metadata <- germplasm_get_metadata()
#' }
#' @export

germplasm_get_metadata <- function() {
  if (is.null(qbms_globals$state$study_db_id)) {
    stop("No project has been selected yet! You have to set your project first using the `gigwa_set_project()` function")
  }
  
  genotyping_list_samples()
  germplasmDbIds <- paste(qbms_globals$state$genotyping_samples$germplasmDbId, collapse = '","')

  call_url  <- paste0(qbms_globals$config$base_url, "/brapi/v2/search/attributevalues")
  call_body <- paste0('{"germplasmDbIds": ["', germplasmDbIds, '"]}')
  
  results <- brapi_post_search_call(call_url, call_body, FALSE)
  
  metadata <- stats::reshape(results$result$data[,-1], idvar = "germplasmName", timevar = "attributeValueDbId", direction = "wide")
  colnames(metadata) <- gsub("value\\.", "", colnames(metadata))
  
  return(metadata)
}


## TerraClimate ################################################################

#' Get TerraClimate Data for a Given Coordinate(s)
#'
#' @description
#' \href{https://www.climatologylab.org/terraclimate.html}{TerraClimate} is a monthly climate dataset 
#' for global terrestrial surfaces from 1958-2021. This function enables you to extract 
#' \href{https://www.climatologylab.org/terraclimate-variables.html}{climate variables} from the 
#' \href{http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html}{hosting server} 
#' provided by the \href{https://hpc.uidaho.edu/}{Idaho University} for a given coordinate(s) without 
#' a need to download the whole raster files in the netCDF format (~100MB per variable for each year) 
#' and provide them in a standard data frame format ready to use in your code. It also calculates the 
#' \href{https://www.worldclim.org/data/bioclim.html}{bioclimatic variables} using the \code{\link{calc_biovars}} 
#' function derivative from the \href{https://github.com/rspatial/dismo/blob/master/R/biovars.R}{dismo R package}.
#'
#' TerraClimate vs. \href{https://www.worldclim.org/data/worldclim21.html}{WorldClim}
#' \itemize{
#' \item 1958-2021 vs. 1970-2000
#' \item 14 vs. 7 climate variables
#' \item ~4 km vs. ~1 km spatial resolution
#' \item need to calculate vs. pre-calculated 19 bioclimatic variables
#' }
#'
#' @references Abatzoglou, J., Dobrowski, S., Parks, S. \emph{et al.} TerraClimate, a high-resolution 
#'             global dataset of monthly climate and climatic water balance from 1958-2015. 
#'             \emph{Sci Data} \strong{5}, 170191 (2018). 
#'             \doi{10.1038/sdata.2017.191}
#' 
#' @param lat  Vector of Latitude(s) in decimal degree format.
#' @param lon  Vector of Longitude(s) in decimal degree format.
#' @param from Start date as a string in the 'YYYY-MM-DD' format.
#' @param to   End date as a string in the 'YYYY-MM-DD' format.
#' @param clim_vars  List of all climate variables to be imported. Valid list includes: \emph{aet, def, pet,
#'                   ppt, q, soil, srad, swe, tmax, tmin, vap, ws, vpd, and PDSI}. Default is NULL for all.
#' @param month_mask A list of all months of interest (e.g., planting season: \code{c(10:12,1:5)}). 
#'                   Default is NULL for all.
#' @param offline    Extract TerraClimate data from downloaded netCDF files (default is FALSE)
#' @param data_path  String contains the directory path where downloaded netCDF files exists (default is './data/')
#' 
#' @return 
#' A list of two data.frame(s) for each pair of coordinates:
#' \itemize{
#' \item \strong{climate:} includes the climate variables (\href{https://www.climatologylab.org/terraclimate-variables.html}{reference}).
#' \item \strong{biovars:} includes the calculated bioclimatic variables (\href{https://www.worldclim.org/data/bioclim.html}{reference}).
#' }
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso 
#' \code{\link{ini_terraclimate}}
#' 
#' @examples
#' if (interactive()) {
#'   # data <- get_terraclimate(36.016, 36.943, 
#'   #                          '1979-09-01', '2012-06-30', 
#'   #                          c('ppt', 'tmin', 'tmax'), c(10:12,1:5))
#'   data <- get_terraclimate(36.016, 36.943, '1979-09-01', '2012-06-30')
#' 
#'   View(data$climate[[1]])
#' 
#'   View(data$biovars[[1]])
#' }
#' @export

get_terraclimate <- function(lat, lon, from = '1958-01-01', to = '2022-12-31', clim_vars = NULL, month_mask = NULL, offline = FALSE, data_path = './data/'){
  # check if the given lat coordinate values are valid
  lat <- as.numeric(lat)
  if (!all(lat >= -90 & lat <= 90)) {
    stop("The Latitude should be in decimal degree format and range from -90 to +90")
  }
  
  # check if the given lon coordinate values are valid
  lon <- as.numeric(lon)
  if (!all(lon >= -180 & lon <= 180)) {
    stop("The Longitude should be in decimal degree format and range from -180 to +180")
  }
  
  if (length(lat) == length(lon)) {
    loc_num <- length(lat)
  } else {
    stop("The number of Latitude/Longitude values should be the same!")
  }

  # check if the given from and to date values are valid
  start_date <- as.Date(from)
  final_date <- as.Date(to)   
  
  terraclimate_vars <- c('aet', 'def', 'pet', 'ppt', 'q', 'soil', 'srad', 'swe', 'tmax', 'tmin', 'vap', 'ws', 'vpd', 'PDSI')
  
  if (is.null(clim_vars)) {
    clim_vars <- terraclimate_vars
  } else {
    if (!all(clim_vars %in% terraclimate_vars)) stop("The given list of climatic variables is supported/existed in TerraClimate dataset!")
  }
  
  if (is.null(month_mask)) {
    if (is.null(month_mask)) month_mask <- 1:12
  } else {
    if (!all(month_mask %in% 1:12)) stop("The given month_mask values are valid!")
  }
  
  clim_data <- list()
  biovars   <- list()
  
  start_month <- as.numeric(format(start_date, '%m'))
  start_year  <- as.numeric(format(start_date, '%Y'))
  start_row   <- (start_year - 1958) * 12 + start_month
  
  final_month <- as.numeric(format(final_date, '%m'))
  final_year  <- as.numeric(format(final_date, '%Y'))
  final_row   <- (final_year - 1958) * 12 + final_month
  
  if (offline == TRUE) {
    years_num <- final_year - start_year + 1
    
    loc_matrix <- matrix(data = NA, nrow = 12 * years_num, ncol = length(clim_vars) + 2)
    
    loc_matrix[, 1] <- rep(start_year:final_year, each = 12)
    loc_matrix[, 2] <- rep(1:12, times = years_num)
    
    colnames(loc_matrix) <- c('year', 'month', clim_vars)
    
    for (i in 1:loc_num) clim_data[[i]] <- data.frame(loc_matrix)
    
    pb <- utils::txtProgressBar(min = 0, max = length(clim_vars) * years_num, initial = 0, style = 3) 
    
    for (var in clim_vars) {
      for (year in start_year:final_year) {
        file_path <- paste0(data_path, 'TerraClimate_', var, '_', year, '.nc')
        
        nc_metadata   <- RNetCDF::open.nc(file_path)
        
        lat_available <- RNetCDF::var.get.nc(nc_metadata, 'lat')
        lon_available <- RNetCDF::var.get.nc(nc_metadata, 'lon')
        
        obs_available <- length(RNetCDF::var.get.nc(nc_metadata, 'time'))
        
        for (i in 1:loc_num) {
          lat_index <- which.min(abs(lat_available - lat[i]))
          lon_index <- which.min(abs(lon_available - lon[i]))
          
          start <- c(lon_index, lat_index, 1)
          count <- c(1, 1, NA)
          
          values <- as.numeric(RNetCDF::var.get.nc(nc_metadata, variable = var, start = start, count = count, unpack = TRUE))
          
          col_index <- which(var == clim_vars) + 2
          row_index <- 12 * (year - start_year) + 1
          
          clim_data[[i]][row_index:(row_index + 11), col_index] <- values
        }
        
        utils::setTxtProgressBar(pb, years_num * (which(var == clim_vars) - 1) + (year - start_year + 1))
      }
    }
    
    close(pb)
    
    for (i in 1:loc_num) {
      if (all(c('ppt', 'tmin', 'tmax') %in% clim_vars)) {
        biovars[[i]] <- calc_biovars(clim_data[[i]])
      } else {
        biovars[[i]] <- NULL
      }
      
      clim_data[[i]] <- clim_data[[i]][start_month:(nrow(clim_data[[i]]) - final_month), ]
      clim_data[[i]] <- clim_data[[i]][clim_data[[i]]$month %in% month_mask, ]
      
      rownames(clim_data[[i]]) <- 1:nrow(clim_data[[i]])
    }
  } else {
    # setup the progress bar
    pb <- utils::txtProgressBar(min = 0, max = loc_num * length(clim_vars), initial = 0, style = 3) 

    for (var in clim_vars) {
      aggregate_url <- paste0('http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_', var, '_1958_CurrentYear_GLOBE.nc')
      nc_metadata   <- RNetCDF::open.nc(aggregate_url)
      
      lat_available <- RNetCDF::var.get.nc(nc_metadata, 'lat')
      lon_available <- RNetCDF::var.get.nc(nc_metadata, 'lon')
      
      obs_available <- length(RNetCDF::var.get.nc(nc_metadata, 'time'))
      
      for (i in 1:loc_num) {
        if (length(clim_data) < i) clim_data[[i]] <- data.frame(matrix(nrow = obs_available, ncol = 0))
        
        lat_index <- which.min(abs(lat_available - lat[i]))
        lon_index <- which.min(abs(lon_available - lon[i]))
        
        start <- c(lon_index, lat_index, 1)
        count <- c(1, 1, NA)
        
        # read in the full period of record using aggregated files
        values <- as.numeric(RNetCDF::var.get.nc(nc_metadata, variable = var, start = start, count = count, unpack = TRUE))
        
        clim_data[[i]] <- cbind(clim_data[[i]], values)
        
        # update the progress bar
        utils::setTxtProgressBar(pb, loc_num * which(clim_vars == var) - (loc_num - i))
      }
    }
    
    close(pb)
    
    for (i in 1:loc_num) {
      colnames(clim_data[[i]]) <- clim_vars
      
      years <- obs_available / 12
      last  <- 1958 + years - 1
      month <- rep(1:12, times = years)
      year  <- rep(1958:last, each = 12)
      
      clim_data[[i]] <- cbind(year, month, clim_data[[i]])
      
      if (all(c('ppt', 'tmin', 'tmax') %in% clim_vars)) {
        biovars[[i]] <- calc_biovars(clim_data[[i]][(start_row - (start_month - 1)):(final_row + (12 - final_month)), ])
      } else {
        biovars[[i]] <- NULL
      }
      
      clim_data[[i]] <- clim_data[[i]][start_row:final_row, ]
      clim_data[[i]] <- clim_data[[i]][clim_data[[i]]$month %in% month_mask, ]
      
      rownames(clim_data[[i]]) <- 1:nrow(clim_data[[i]])
    }
  }

  return(list(climate = clim_data, biovars = biovars))
}


#' Calculate the Bioclimatic Variables
#' 
#' @description
#' Bioclimatic variables are derived from the monthly temperature and rainfall 
#' values in order to generate more biologically meaningful variables. These are 
#' often used in species distribution modeling and related ecological modeling 
#' techniques. The bioclimatic variables represent annual trends (e.g., mean 
#' annual temperature, annual precipitation) seasonality (e.g., annual range in 
#' temperature and precipitation) and extreme or limiting environmental factors 
#' (e.g., temperature of the coldest and warmest month, and precipitation of the 
#' wet and dry quarters). A quarter is a period of three months (1/4 of the year).
#' 
#' They are coded as follows:
#' \itemize{
#' \item BIO1 = Annual Mean Temperature
#' \item BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
#' \item BIO3 = Isothermality (BIO2/BIO7) (* 100)
#' \item BIO4 = Temperature Seasonality (standard deviation *100)
#' \item BIO5 = Max Temperature of Warmest Month
#' \item BIO6 = Min Temperature of Coldest Month
#' \item BIO7 = Temperature Annual Range (BIO5-BIO6)
#' \item BIO8 = Mean Temperature of Wettest Quarter
#' \item BIO9 = Mean Temperature of Driest Quarter
#' \item BIO10 = Mean Temperature of Warmest Quarter
#' \item BIO11 = Mean Temperature of Coldest Quarter
#' \item BIO12 = Annual Precipitation
#' \item BIO13 = Precipitation of Wettest Month
#' \item BIO14 = Precipitation of Driest Month
#' \item BIO15 = Precipitation Seasonality (Coefficient of Variation)
#' \item BIO16 = Precipitation of Wettest Quarter
#' \item BIO17 = Precipitation of Driest Quarter
#' \item BIO18 = Precipitation of Warmest Quarter
#' \item BIO19 = Precipitation of Coldest Quarter
#' }
#'      
#' This work is derivative from the \href{https://github.com/rspatial/dismo/blob/master/R/biovars.R}{dismo R package}
#' 
#' @references Nix, 1986. A biogeographic analysis of Australian elapid snakes. 
#'             In: R. Longmore (ed.). Atlas of elapid snakes of Australia. 
#'             Australian Flora and Fauna Series 7. Australian Government Publishing 
#'             Service, Canberra.
#'
#' @author Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @author Robert Hijmans, Museum of Vertebrate Zoology, UC Berkeley
#' 
#' @param data Data.frame has 4 mandatory columns (year, ppt, tmin, and tmax), 
#'             and 12 rows (months) for each year sorted from Jan to Dec.
#' @return Data.frame has 19 columns for "bioclim" variables (bio1-bio19) and 
#'         last column for year (you will get one row per year).
#' 
#' @export

calc_biovars <- function(data) {
  year <- unique(data$year)
  
  if (length(which(table(data$year) != 12)) != 0) { 
    stop(paste('You have to have 12 rows (months) for each year in your dataset.',  
               'That is not the case for:', paste(attributes(which(table(data$year) != 12))$names, collapse = ', ')))
  }
  
  required_vars <- c('ppt', 'tmin', 'tmax')
  var_not_found <- required_vars[!(required_vars %in% colnames(data))]
  
  if(length(var_not_found) != 0) {
    stop(paste('Missing required column(s) in your dataset:', paste(var_not_found, collapse = ', ')))
  }
  
  prec <- matrix(data$ppt,  ncol = 12, byrow = TRUE)
  tmin <- matrix(data$tmin, ncol = 12, byrow = TRUE)
  tmax <- matrix(data$tmax, ncol = 12, byrow = TRUE)
  
  # can't have missing values in a row
  nas <- apply(prec, 1, function(x){ any(is.na(x)) } )
  nas <- nas | apply(tmin, 1, function(x){ any(is.na(x)) } )
  nas <- nas | apply(tmax, 1, function(x){ any(is.na(x)) } )
  
  p <- matrix(nrow = nrow(prec), ncol = 20)
  colnames(p) = c(paste0('bio', 1:19), 'year')
  
  if (all(nas)) { return(p) }
  
  prec[nas,] <- NA
  tmin[nas,] <- NA
  tmax[nas,] <- NA
  
  window <- function(x) { 
    lng <- length(x)
    x <- c(x,  x[1:3])
    m <- matrix(ncol = 3, nrow = lng)
    for (i in 1:3) { m[,i] <- x[i:(lng+i-1)] }
    apply(m, MARGIN = 1, FUN = sum)
  }
  
  cv <- function(x) {
    return(stats::sd(x) / mean(x) * 100)
  }
  
  tavg <- (tmin + tmax) / 2
  
  # P1. Annual Mean Temperature 
  p[, 1] <- apply(tavg, 1, mean)
  
  # P2. Mean Diurnal Range(Mean(period max-min)) 
  p[, 2] <- apply(tmax - tmin, 1, mean)
  
  # P4. Temperature Seasonality (standard deviation) 
  p[,4] <- 100 * apply(tavg, 1, stats::sd)
  
  # P5. Max Temperature of Warmest Period 
  p[, 5] <- apply(tmax, 1, max)
  
  # P6. Min Temperature of Coldest Period 
  p[, 6] <- apply(tmin, 1, min)
  
  # P7. Temperature Annual Range (P5-P6) 
  p[, 7] <- p[, 5] - p[, 6]
  
  # P3. Isothermality (P2 / P7) 
  p[, 3] <- 100 * p[, 2] / p[, 7]
  
  # P12. Annual Precipitation 
  p[, 12] <- apply(prec, 1, sum)
  
  # P13. Precipitation of Wettest Period 
  p[, 13] <-  apply(prec, 1, max)
  
  # P14. Precipitation of Driest Period 
  p[, 14] <-  apply(prec, 1, min)
  
  # P15. Precipitation Seasonality(Coefficient of Variation) 
  # the "1 +" is to avoid strange CVs for areas where mean rainfall is < 1)
  p[, 15] <- apply(prec+1, 1, cv)
  
  # precipitation by quarter (3 months)		
  wet <- t(apply(prec, 1, window))
  
  # P16. Precipitation of Wettest Quarter 
  p[, 16] <- apply(wet, 1, max)
  
  # P17. Precipitation of Driest Quarter 
  p[, 17] <- apply(wet, 1, min)
  
  tmp <- t(apply(tavg, 1, window)) / 3
  
  if (all(is.na(wet))) {
    p[, 8] <- NA		
    p[, 9] <- NA		
  } else {
    # P8. Mean Temperature of Wettest Quarter 
    wetqrt <- cbind(1:nrow(p), as.integer(apply(wet, 1, which.max)))
    p[, 8] <- tmp[wetqrt]
    
    # P9. Mean Temperature of Driest Quarter 
    dryqrt <- cbind(1:nrow(p), as.integer(apply(wet, 1, which.min)))
    p[, 9] <- tmp[dryqrt]
  }
  
  # P10. Mean Temperature of Warmest Quarter 
  p[, 10] <- apply(tmp, 1, max)
  
  # P11. Mean Temperature of Coldest Quarter
  p[, 11] <- apply(tmp, 1, min) 
  
  if (all(is.na(tmp))) {
    p[, 18] <- NA		
    p[, 19] <- NA
  } else {
    # P18. Precipitation of Warmest Quarter 
    hot <- cbind(1:nrow(p), as.integer(apply(tmp, 1, which.max)))
    p[, 18] <- wet[hot]
    
    # P19. Precipitation of Coldest Quarter 
    cold <- cbind(1:nrow(p), as.integer(apply(tmp, 1, which.min)))
    p[, 19] <- wet[cold]
  }
  
  p[, 20] <- year
  
  return(as.data.frame(p))
}


#' Download TerraClimate netCDF Data Files to Extract their Data Offline
#'
#' @param from Start date as a string in the 'YYYY-MM-DD' format.
#' @param to End date as a string in the 'YYYY-MM-DD' format.
#' @param clim_vars List of all climate variables to be imported. Valid list includes: \emph{aet, def, pet,
#'                 ppt, q, soil, srad, swe, tmax, tmin, vap, ws, vpd, and PDSI}. Default is NULL for all.
#' @param data_path String containing the directory path where downloaded netCDF files exist (default is './data/')
#' @param timeout Timeout in seconds to download each netCDF raster file (default is 300).
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso 
#' \code{\link{get_terraclimate}}
#' 
#' @examples
#' if (interactive()) {
#'   ini_terraclimate('2018-09-01', '2019-06-30', c('ppt', 'tmin', 'tmax'))
#'   
#'   x <- c(-6.716, 35.917, 76.884)
#'   y <- c(33.616, 33.833, 23.111)
#'   
#'   a <- get_terraclimate(y, x, '2018-09-01', '2019-06-30', c('ppt', 'tmin', 'tmax'))
#'   
#'   a$climate[[1]]
#'   a$biovars[[1]]
#'   
#'   b <- get_terraclimate(y, x, '2018-09-01', '2019-06-30', c('ppt', 'tmin', 'tmax'), offline = TRUE)
#'   
#'   b$climate[[1]]
#'   b$biovars[[1]]
#' }
#' @export

ini_terraclimate <- function(from = '2019-09-01', to = '2022-06-30', clim_vars = c('ppt', 'tmin', 'tmax'), data_path = './data/', timeout = 300) {
  options(timeout = timeout)
  
  download_url <- 'https://climate.northwestknowledge.net/TERRACLIMATE-DATA/'

  start_year <- as.numeric(format(as.Date(from), '%Y'))
  final_year <- as.numeric(format(as.Date(to), '%Y'))
  
  if (!dir.exists(data_path)) dir.create(data_path)
  
  for (var in clim_vars) {
    for (year in start_year:final_year) {
      file_path <- paste0(data_path, 'TerraClimate_', var, '_', year, '.nc')
      file_url  <- paste0(download_url, 'TerraClimate_', var, '_', year, '.nc')
      
      if (!file.exists(file_path)) {
        utils::download.file(file_url, file_path, mode = 'wb', method = 'libcurl')
      }
    }
  }
}


## HWSDv2 ######################################################################

#' Download and Setup HWSD v2.0 Data Files to Extract their Data Offline
#'
#' @param data_path String containing the directory path where downloaded HWSD v2.0 data files exist (default is './data/').
#' @param timeout Timeout in seconds to download each HWSD v2.0 data file (default is 300).
#' 
#' @return 
#' HWSDv2 object that has a list of two items: the HWSD2 raster and the HWSD2 SQLite connection.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso 
#' \code{\link{get_hwsd2}}
#' 
#' @export

ini_hwsd2 <- function(data_path = './data/', timeout = 300){
  options(timeout = timeout)
  
  con <- list()
  
  hwsd2_raster <- paste0(data_path, 'hwsd2.bil')
  hwsd2_sqlite <- paste0(data_path, 'HWSD2.sqlite')
  
  # create the destination directory if it is not exists yet
  if (!dir.exists(data_path)) {
    dir.create(data_path)
  }
  
  if (!file.exists(hwsd2_raster)) {
    # import the HWSD v2 raster soil unit map (download then unzip, 4 files, 1.74 GB)
    # https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/HWSD/HWSD2_RASTER.zip
    
    hwsd2_raster_url <- 'https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/HWSD/HWSD2_RASTER.zip'
    hwsd2_raster_zip <- paste0(data_path, 'HWSD2_RASTER.zip')
    
    # download the raster file
    utils::download.file(hwsd2_raster_url, hwsd2_raster_zip)
    
    # unzip the raster file in the given directory
    utils::unzip(hwsd2_raster_zip, exdir = data_path)
  }
  
  if (!file.exists(hwsd2_sqlite)) {
    # load the SQLite version of the soil attribute database
    # https://www.isric.org/sites/default/files/HWSD2.sqlite
    
    hwsd2_sqlite_url <- 'https://www.isric.org/sites/default/files/HWSD2.sqlite'
    
    # download the SQLite version of the HWSD v2 database
    utils::download.file(hwsd2_sqlite_url, hwsd2_sqlite, method = 'libcurl', mode = 'wb')
  }
  
  con$raster <- terra::rast(hwsd2_raster)
  con$sqlite <- DBI::dbConnect(RSQLite::SQLite(), dbname = hwsd2_sqlite)
  
  return(con)
}


#' Get HWSD v2 Soil Data for a Given Location(s)
#'
#' @description
#' The HWSD2_SMU table contains general information for each of the soil units 
#' occurring in any given SMU code (dominant soil unit and up to 11 associated soils).
#' 
#' The SEQUENCE column refers to the sequence in which soil units within the SMU 
#' are presented (in order of percentage share). The dominant soil has sequence 1. 
#' The sequence can range between 1 and 12.
#' 
#' The SHARE column refers to the share of the soil unit within the mapping unit in 
#' percentage. Shares of soil units within a mapping unit always sum up to 100 percent.
#' 
#' The HWSD2_LAYERS table provides soil attributes per depth layer for each of the 
#' seven depth layers (D1 to D7) separately (represented in the LAYER column in the 
#' HWSD2_LAYERS table). The depth of the top and bottom of each layer is defined in
#' the TOPDEP and BOTDEP columns, respectively. 
#'
#' @param df data.frame that list locations info including the coordinates in decimal degree format.
#' @param con the HWSDv2 object returns from the ini_hwsd2() function.
#' @param x longitude column name in the df data.frame (default is 'Longitude').
#' @param y latitude column name in the df data.frame (default is 'Latitude').
#' @param sequence the sequence in which soil units are presented (in order of percentage share). 
#'                 The dominant soil has sequence 1. The sequence can range between 1 and 12.
#' @param layer the depth layer range from 'D1' to 'D7'. The depth of the top and bottom of 
#'              each layer is defined in the TOPDEP and BOTDEP columns, respectively.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso 
#' \code{\link{ini_hwsd2}}
#' 
#' @examples
#' if (interactive()) {
#'   # create a simple data.frame for a list of locations and their coordinates
#'   Location  <- c('Tel-Hadya', 'Terbol', 'Marchouch')
#'   Latitude  <- c(36.016, 33.808, 33.616)
#'   Longitude <- c(36.943, 35.991, -6.716)
#'   
#'   sites <- data.frame(Location, Latitude, Longitude)
#'   
#'   # initiate, download, and setup the HWSD v2 in a given local directory
#'   hwsd2 <- ini_hwsd2(data_path = 'C:/Users/Kel-shamaa/Downloads/HWSD v2/')
#'   
#'   # query soil attributes for given sites using the HWSD v2 connection object
#'   #
#'   # sequence parameter, range between 1 and 12 (max), 1 is the dominant soil.
#'   # returned df has SHARE column refers to share%
#'   #
#'   # layer parameter refers to depth layer (D1 to D7).
#'   # returned df has TOPDEP/BOTDEP columns represent top/bottom layer depth in cm.
#'   sites <- get_hwsd2(df = sites, 
#'                      con = hwsd2, 
#'                      x = 'Longitude', 
#'                      y = 'Latitude', 
#'                      sequence = 1, 
#'                      layer = 'D1')
#' }
#' @export

get_hwsd2 <- function(df, con, x = 'longitude', y = 'latitude', sequence = 1, layer = 'D1') {
  total_loc <- nrow(df)
  
  df$seq_id <- 1:total_loc
  df$smu_id <- NULL
  
  # setup the progress bar
  pb <- utils::txtProgressBar(min = 0, max = total_loc, initial = 0, style = 3) 
  
  for(i in 1:total_loc) {
    longitude <- df[i, x]
    latitude  <- df[i, y]
    
    # crop the raster for the given coordinates using the raster resolution unit as the bounding box dimensions
    hwsd_loc <- terra::crop(con$raster, terra::ext(c(longitude, longitude + terra::res(con$raster)[1], latitude, latitude + terra::res(con$raster)[2]))) 
    
    # extract the raster pixel code (Soil Mapping Unit ID)
    df[i, 'smu_id'] <- hwsd_loc[1]
    
    # update progress bar every 5%
    if(i %% ceiling(total_loc/20) == 0) utils::setTxtProgressBar(pb,i)
  }
  
  # set the progress bar at 100% and close it
  utils::setTxtProgressBar(pb, i)
  close(pb)
  
  # get a list of unique smu ids 
  ids <- paste(unique(terra::na.omit(df$smu_id)), collapse = ',')
  
  # build up SQL statement conditions
  SEQ <- ifelse(sequence %in% 1:12, paste('SEQUENCE = ', sequence), 'SEQUENCE > 0')
  LYR <- ifelse(layer %in% paste0('D', 1:7), paste0('LAYER = "', layer, '"'), 'LAYER like "%"')
  
  # retrieve soil data for the selected soil component and depth layer
  hwds2_layers <- DBI::dbGetQuery(con$sqlite, paste('select * from HWSD2_LAYERS where', SEQ, 'and', LYR, 'and HWSD2_SMU_ID in (', ids, ')'))
  
  # associate soil attribution columns to the original sites data frame
  df <- merge(df, hwds2_layers, by.x = 'smu_id', by.y = 'HWSD2_SMU_ID', sort = FALSE)
  
  # sort records by soil component share and then layer depth
  df <- df[with(df, order(seq_id, SEQUENCE, LAYER)), ]
  
  return(df)
}


### Deprecated and will be removed in v2.0 #####################################


#' Get the List of the Run Names Available in the Selected GIGWA Project
#' 
#' This function is deprecated and will be removed in v2.0, use \code{\link{genotyping_list_runs}} instead.
#' 
#' @export

gigwa_list_runs <- function() {
  .Deprecated(msg = "gigwa_list_runs() is deprecated and will be removed in v2.0, use genotyping_list_runs() instead.")
  
  return(genotyping_list_runs())
}


#' Set the Current Active GIGWA Run
#' 
#' This function is deprecated and will be removed in v2.0, use \code{\link{genotyping_set_run}} instead.
#' 
#' @param run_name run name
#'
#' @export

gigwa_set_run <- function(run_name) {
  .Deprecated(msg = "gigwa_set_run() is deprecated and will be removed in v2.0, use genotyping_set_run() instead.")
  
  return(genotyping_set_run(run_name))
}


#' Get the Samples List of the Current Active GIGWA Run
#' 
#' This function is deprecated and will be removed in v2.0, use \code{\link{genotyping_list_samples}} instead.
#' 
#' @export

gigwa_get_samples <- function() {
  .Deprecated(msg = "gigwa_get_samples() is deprecated and will be removed in v2.0, use genotyping_list_samples() instead.")
  
  return(genotyping_list_samples())
}


#' Get the Metadata of the Current Active GIGWA Run
#' 
#' This function is deprecated and will be removed in v2.0, use \code{\link{germplasm_get_metadata}} instead.
#' 
#' @export

gigwa_get_metadata <- function() {
  .Deprecated(msg = "gigwa_get_metadata() is deprecated and will be removed in v2.0, use germplasm_get_metadata() instead.")
  
  return(germplasm_get_metadata())
}
