# Set the future plan to enable parallel execution
future::plan(future::multisession)

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
  headers   <- c(
    "Authorization" = auth_code, 
    "Accept-Encoding" = "gzip, deflate",
    "accept" = "application/json"
  )
  headers
}


#' Asynchronously Fetch a Single API Page
#'
#' @description
#' Sends an asynchronous HTTP GET request to fetch data from a single API page.
#'
#' @param full_url Character string specifying the full URL of the API endpoint to request.
#' @param nested Logical value indicating whether to flatten nested lists in the JSON response.
#'
#' @return A future representing the asynchronous operation, which will resolve to a list containing the parsed JSON response.
#'
#' @details
#' This function uses \code{future::future()} to perform the HTTP GET request asynchronously.
#' It retrieves the content from the specified URL, checks for HTTP errors, and parses the JSON response.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}

get_async_page <- function(full_url, nested) {
  future::future({
    resp <- httr::GET(full_url, httr::add_headers(.headers = brapi_headers()))
    httr::stop_for_status(resp)
    jsonlite::fromJSON(rawToChar(resp$content), flatten = !nested)
  })
}


#' Asynchronously Fetch Multiple API Pages
#'
#' @description
#' Sends asynchronous HTTP GET requests to fetch data from multiple API pages concurrently.
#'
#' @param pages Character vector of full URLs specifying the API endpoints to request.
#' @param nested Logical value indicating whether to flatten nested lists in the JSON responses.
#'
#' @return A list of parsed JSON responses from each page.
#'
#' @details
#' This function uses \code{future.apply::future_lapply()} to perform concurrent HTTP GET requests for multiple pages.
#' It retrieves and parses the JSON responses from each URL provided.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}

get_async_pages <- function(pages, nested) {
  future.apply::future_lapply(pages, function(full_url) {
    resp <- httr::GET(full_url, httr::add_headers(.headers = brapi_headers()))
    httr::stop_for_status(resp)
    jsonlite::fromJSON(rawToChar(resp$content), flatten = !nested)
  })
}


#' Internal Function for Core BrAPI GET Calls
#'
#' @description
#' Fetches data from an API endpoint, handles pagination by retrieving all pages, and consolidates the results into a single data frame.
#'
#' @param call_url Character string specifying the base URL of the API endpoint to request.
#' @param nested Logical value indicating whether to flatten nested lists in the JSON responses. Defaults to \code{TRUE}.
#'
#' @return A list containing the consolidated data and associated metadata from the API response.
#'
#' @details
#' This function performs the following steps:
#' \enumerate{
#'   \item Fetches the first page synchronously to determine the total number of pages.
#'   \item If multiple pages exist, it asynchronously fetches the remaining pages using \code{get_async_pages()}.
#'   \item Consolidates the data from all pages into a single data frame.
#'   \item Updates global state variables with pagination information.
#' }
#' It relies on global variables from \code{qbms_globals} to manage state and configuration.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}

brapi_get_call <- function(call_url, nested = TRUE) {
  separator <- if (grepl("\\?", call_url)) "&" else "?"
  full_url  <- paste0(call_url, separator, "page=0&pageSize=", qbms_globals$config$page_size)
  
  # Fetch the first page synchronously to get total number of pages
  result_future <- get_async_page(full_url, nested)
  result_object <- future::value(result_future)
  result_data   <- as.data.frame(result_object$result$data)
  total_pages   <- result_object$metadata$pagination$totalPages
  
  if (total_pages > 1) {
    pages <- seq(1, total_pages - 1)
    full_urls <- paste0(call_url, separator, "page=", pages, "&pageSize=", qbms_globals$config$page_size)
    
    # Fetch remaining pages asynchronously
    all_pages <- get_async_pages(full_urls, nested)
    
    # Combine data from all pages
    for (n in seq_along(all_pages)) {
      page_data <- as.data.frame(all_pages[[n]]$result$data)
      result_data <- rbindx(result_data, page_data)
    }
  }
  
  # Finalize the result data
  if (ncol(result_data) == 1) {
    result_object$result$data <- result_data[, 1]
  } else {
    result_object$result$data <- result_data
  }
  
  result_data <- result_object$result
  
  # Update global state with pagination info
  qbms_globals$state$current_page <- result_object$metadata$pagination$currentPage
  qbms_globals$state$page_size    <- result_object$metadata$pagination$pageSize
  qbms_globals$state$total_count  <- result_object$metadata$pagination$totalCount
  qbms_globals$state$total_pages  <- result_object$metadata$pagination$totalPages
  qbms_globals$state$errors       <- result_object$errors
  
  return(result_data)
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