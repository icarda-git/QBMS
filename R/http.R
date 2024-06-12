

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
