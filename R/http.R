
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

#' Generate Common HTTP Headers for API Requests
#'
#' @description
#' This function constructs a list of standard HTTP headers required for making API 
#' requests, ensuring proper authentication and encoding. These headers are typically 
#' included with each API call to provide necessary information such as authorization 
#' tokens and content acceptance types. The function is designed to work with 
#' authenticated APIs, including BrAPI.
#'
#' @return
#' A named list of common HTTP headers, including the authorization token (Bearer), 
#' content encoding, and accepted content types.
#'
#' @note
#' Ensure that the global state contains a valid authorization token before making API 
#' requests. This function retrieves the token from `qbms_globals$state$token`, which 
#' should be set after a successful login or authentication process.
#'
#' @return 
#' A named list containing key HTTP headers, including:
#' \itemize{
#'   \item \strong{Authorization:} Bearer token used for authenticated API access.
#'   \item \strong{Accept-Encoding:} Specifies supported compression types such as gzip and deflate.
#'   \item \strong{Accept:} Specifies that the client accepts responses in JSON format.
#' }
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}

brapi_headers <- function() {
  auth_code <- paste0("Bearer ", qbms_globals$state$token)
  headers   <- c("Authorization" = auth_code, 
                 "Accept-Encoding" = "gzip, deflate",
                 "accept" = "application/json")
  headers
}


#' Asynchronous HTTP GET Request
#'
#' @description
#' This helper function performs an HTTP GET request asynchronously, allowing the 
#' retrieval of data from a given URL without blocking the execution of other tasks. 
#' The function leverages the `async` package to make the GET request and parse the 
#' response. It supports handling JSON responses and optionally flattens nested 
#' data frames for easier manipulation.
#'
#' @param full_url A string containing the complete URL to retrieve data from. 
#'                 The URL should point to an API or resource that returns a JSON response.
#' @param nested Logical flag indicating whether nested data frames in the returned 
#'               JSON response should be flattened. Default is FALSE, meaning the data 
#'               will retain its nested structure; set to TRUE to simplify the response 
#'               by flattening nested structures.
#' 
#' @return
#' A deferred object representing the asynchronous HTTP GET request. When resolved, 
#' the object contains the parsed JSON response, either as a nested or flattened 
#' structure depending on the `nested` parameter.
#' 
#' @note
#' The function relies on the `async` package for asynchronous execution. Ensure that 
#' `async` is installed and loaded in your environment. Also, the response from the 
#' URL is expected to be in JSON format, as this function parses the content accordingly.
#' 
#' @return 
#' A deferred object that resolves to the parsed JSON response, with optional 
#' flattening of nested data structures.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}

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


#' Run All Supplied Pages Asynchronously
#'
#' @description
#' This helper function is designed to asynchronously retrieve data from multiple 
#' API pages (URLs) and consolidate the results once all pages have been fetched. 
#' The function creates a deferred object that is resolved when all the provided 
#' URLs are successfully retrieved. It leverages asynchronous programming to 
#' enhance performance, especially when dealing with multiple API requests.
#'
#' @param pages A list of URLs representing the individual API pages to be fetched.
#'              Each URL corresponds to a specific page of results in a paginated API.
#' @param nested Logical flag indicating whether nested data structures in the 
#'               returned JSON responses should be flattened. If set to FALSE 
#'               (default), the data will retain its nested structure; if set to TRUE, 
#'               nested data frames will be flattened into a simpler format.
#' 
#' @return
#' An asynchronous deferred object that, when resolved, provides the combined 
#' results of all pages as a list or data frame, depending on the structure of the response.
#' 
#' @note
#' The function requires the `async` package for handling the asynchronous 
#' retrieval of pages. Ensure that `async` is installed and loaded in your R environment.
#' 
#' @return 
#' A deferred object that resolves once all pages have been fetched and processed.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}

get_async_pages <- function(pages, nested) {
}

if (requireNamespace("async", quietly = TRUE)) {
  get_async_pages <- async::async(function(pages, nested) {
    reqs <- lapply(pages, get_async_page, nested)
    async::when_all(.list = reqs)$
      then(function(x) x)
  })
}


#' Internal Function for Core BrAPI GET Calls
#'
#' @description
#' This is an internal utility function designed to handle BrAPI GET requests. 
#' It efficiently manages the process of making API calls to BrAPI endpoints, handling 
#' aspects such as authentication, pagination, and JSON response parsing. Additionally, 
#' it supports optional data flattening for nested structures, handles encoding, and 
#' deals with compression for performance optimization.
#'
#' The function ensures the correct retrieval of paginated data by iteratively requesting 
#' additional pages if required, and combines the results into a single response object.
#' Progress is tracked and displayed if verbosity is enabled in the global configuration.
#'
#' @param call_url A string representing the full BrAPI URL for the GET request.
#' @param nested Logical flag indicating whether to flatten nested JSON data structures 
#'               into a more straightforward format. The default is TRUE, meaning nested 
#'               data will be flattened.
#' 
#' @return
#' A data frame or list containing the results from the BrAPI GET request. If the response 
#' includes multiple pages, the results from all pages are concatenated into a single object.
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


#' Internal Function Used for Core BrAPI POST Calls (Allele Matrix Search)
#'
#' @description
#' This function is used internally to execute POST calls for retrieving the allele matrix
#' via BrAPI. It handles the post request, waits for the results asynchronously if required,
#' and processes the results.
#'
#' @param call_url BrAPI URL for the POST request.
#' @param call_body The request body to send with the POST request.
#' @param nested Logical indicating whether to flatten the nested structure. Default is TRUE.
#' 
#' @return
#' A list of results obtained from the BrAPI POST call.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}

brapi_post_search_allelematrix <- function(call_url, call_body, nested = TRUE) {
  headers  <- brapi_headers()
  call_url <- utils::URLencode(call_url)

  response <- httr::POST(url = call_url, body = call_body,
                         encode = "raw", httr::accept_json(), httr::content_type_json(),
                         httr::add_headers(headers), httr::timeout(qbms_globals$config$time_out))
  
  results <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), flatten = !nested)
  
  # https://plant-breeding-api.readthedocs.io/en/latest/docs/best_practices/Search_Services.html#post-search-entity
  if (response$status_code == 202 || !is.null(results$result$searchResultsDbId)) {
    repeat {
      Sys.sleep(qbms_globals$config$sleep)
      
      searchResultsDbId <- results$result$searchResultsDbId
      
      response <- httr::GET(url = paste(call_url, searchResultsDbId, sep = "/"), 
                            httr::add_headers(headers), httr::timeout(qbms_globals$config$time_out))
      
      results <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), flatten = !nested)
      
      if (response$status_code == 200 && is.null(results$result$searchResultsDbId)) {
        break
      }
    }
  }
  
  return(results)
}


#' Internal Function Used for Core BrAPI POST Calls
#'
#' @description
#' This function is used internally to execute POST calls to BrAPI endpoints and
#' retrieve the results while handling pagination and long-running tasks.
#'
#' @param call_url BrAPI URL for the POST request.
#' @param call_body The request body to send with the POST request.
#' @param nested Logical indicating whether to flatten the nested structure. Default is TRUE.
#' 
#' @return
#' A list of results obtained from the BrAPI POST call.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}

brapi_post_search_call <- function(call_url, call_body, nested = TRUE) {
  headers  <- brapi_headers()
  call_url <- utils::URLencode(call_url)
  
  page_info <- paste0('{"page": {page}, "pageToken": {page}, "pageSize": ', qbms_globals$config$page_size)
  call_body <- paste0(page_info, ", ", substr(call_body, 2, nchar(call_body)))
  
  current_page <- 0
  
  repeat {
    page_body <- gsub("\\{page\\}", current_page, call_body)

    response <- httr::POST(url = call_url, body = page_body,
                           encode = "raw", httr::accept_json(), httr::content_type_json(),
                           httr::add_headers(headers), httr::timeout(qbms_globals$config$time_out))
    
    results <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), flatten = !nested)
    
    # https://plant-breeding-api.readthedocs.io/en/latest/docs/best_practices/Search_Services.html#post-search-entity
    if (response$status_code == 202 || !is.null(results$result$searchResultsDbId)) {
      repeat {
        Sys.sleep(qbms_globals$config$sleep)
        
        searchResultsDbId <- results$result$searchResultsDbId
        
        response <- httr::GET(url = paste(call_url, searchResultsDbId, sep = "/"), 
                              httr::add_headers(headers), httr::timeout(qbms_globals$config$time_out))
        
        results <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), flatten = !nested)
        
        if (response$status_code == 200 && is.null(results$result$searchResultsDbId)) {
          break
        }
      }
    }
    
    if (is.null(results$metadata$pagination$totalPages)) {
      # GIGWA /search/variants case!
      results$metadata$pagination$totalPages <- with(results$metadata$pagination, ceiling(totalCount/pageSize))
    }
    
    if (results$metadata$pagination$totalPages == 1) {
      break
    } else {
      if (results$metadata$pagination$currentPage == 0) {
        remaining_pages <- results$metadata$pagination$totalPages - 1
        pb <- utils::txtProgressBar(min = 0, max = remaining_pages, initial = 0, style = 3) 
        full_data <- results$result$data
      } else {
        full_data <- rbind(full_data, results$result$data)
      }
      
      if (current_page == results$metadata$pagination$totalPages - 1) {
        results$result$data <- full_data
        close(pb)
        break
      } else {
        current_page <- results$metadata$pagination$currentPage + 1
        utils::setTxtProgressBar(pb, current_page)
      }
    }
  }

  return(results)
}
