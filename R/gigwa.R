### GIGWA ######################################################################

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
  
  call_url <- get_brapi_url("gigwa_list_dbs")
  
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
    call_url <- get_brapi_url("gigwa_list_projects")
    call_url <- sub("\\{programDbId\\}", qbms_globals$config$db, call_url)
    
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
  
  qbms_globals$state$gigwa_runs      <- NULL
  qbms_globals$state$gigwa_samples   <- NULL
  qbms_globals$state$gigwa_sequences <- NULL
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
#'   gigwa_list_runs()
#' }
#' @export

gigwa_list_runs <- function() {
  if (is.null(qbms_globals$state$study_db_id)) {
    stop("No project has been selected yet! You have to set your project first using the `gigwa_set_project()` function")
  }
  
  if (!is.null(qbms_globals$state$gigwa_runs)) {
    gigwa_runs <- qbms_globals$state$gigwa_runs
  } else {
    call_url  <- get_brapi_url("gigwa_list_runs")
    call_body <- paste0('{"studyDbIds": ["', qbms_globals$state$study_db_id, '"]}')
    
    results <- brapi_post_search_call(call_url, call_body, FALSE)
    
    gigwa_runs <- as.data.frame(results$result$data[, c("variantSetName", "variantSetDbId")])
    
    qbms_globals$state$gigwa_runs <- gigwa_runs
  }
  
  return(gigwa_runs[c("variantSetName")])
}


#' Set the Current Active GIGWA Run
#'
#' @description
#' This function updates the current active run in the internal state object using the 
#' `studyDbIds` retrieved from GIGWA, which are associated with the given `run_name` parameter.
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
#' \code{\link{set_qbms_config}}, \code{\link{gigwa_set_project}}, \code{\link{gigwa_list_runs}}
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
#' }
#' @export

gigwa_set_run <- function(run_name) {
  valid_runs <- gigwa_list_runs()
  
  if (!run_name %in% unlist(valid_runs)) {
    stop("Your run name is not exists in this project! You may use the `gigwa_list_runs()` function to check the available runs")
  }
  
  gigwa_runs <- qbms_globals$state$gigwa_runs
  
  qbms_globals$state$variant_set_db_id <- gigwa_runs[gigwa_runs$variantSetName == run_name, "variantSetDbId"]
}


#' Get the Samples List of the Current Active GIGWA Project
#'
#' @description
#' This function retrieves the samples list of the current active project
#' as configured in the internal state object using the `gigwa_set_project()` function.
#'
#' @return 
#' A vector of all samples in the selected project.
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
#'   # Get a list of all samples in the selected project
#'   samples <- gigwa_get_samples()
#' }
#' @export

gigwa_get_samples <- function() {
  if (is.null(qbms_globals$state$study_db_id)) {
    stop("No project has been selected yet! You have to set your project first using the `gigwa_set_project()` function")
  }
  
  if (!is.null(qbms_globals$state$gigwa_samples)) {
    gigwa_samples <- qbms_globals$state$gigwa_samples
  } else {
    call_url  <- get_brapi_url("gigwa_get_samples")
    call_body <- paste0('{"studyDbIds": ["', qbms_globals$state$study_db_id, '"]}')
    
    results <- brapi_post_search_call(call_url, call_body, FALSE)
    
    gigwa_samples <- results$result$data[, c("sampleDbId", "germplasmDbId")]
    
    qbms_globals$state$gigwa_samples <- gigwa_samples
  }

  germplasmName <- sub(".*\u00A7", "", gigwa_samples$germplasmDbId)
  
  return(germplasmName)
}


#' Get the Sequences of the Current Active GIGWA Project
#'
#' @description
#' This function retrieves the list sequences of the current active project
#' as configured in the internal state object using the `gigwa_set_project()` function.
#'
#' @return 
#' A vector of all sequences in the selected project.
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
#'   # Get a list of all samples in the selected project
#'   chroms <- gigwa_get_sequences()
#' }
#' @export

gigwa_get_sequences <- function() {
  if (is.null(qbms_globals$state$study_db_id)) {
    stop("No project has been selected yet! You have to set your project first using the `gigwa_set_project()` function")
  }
  
  if (!is.null(qbms_globals$state$gigwa_sequences)) {
    gigwa_sequences <- qbms_globals$state$gigwa_sequences
  } else {
    call_url  <- get_brapi_url("gigwa_get_sequences")
    call_body <- paste0('{"studyDbIds": ["', qbms_globals$state$study_db_id, '"]}')
    
    results <- brapi_post_search_call(call_url, call_body, FALSE)
    
    gigwa_sequences <- results$result$data$referenceName
    
    qbms_globals$state$gigwa_sequences <- gigwa_sequences
  }
  
  return(gigwa_sequences)
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
#'   gigwa_set_run("run1")
#'   
#'   # Get the marker matrix 
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
    available_samples <- gigwa_get_samples()
    missing_samples <- setdiff(samples, available_samples)
    
    if (length(missing_samples) > 0) {
      stop("Some samples are not exists in this project! You may use the `gigwa_get_samples()` function to check the available samples")
    }
  } else {
    samples <- gigwa_get_samples()
  }
  
  if (!is.null(start) && !is.numeric(start)){
    stop("Start position should be numeric!")
  }
  
  if (!is.null(end) && !is.numeric(end)){
    stop("End position should be numeric!")
  }
  
  # https://gigwa-dev.southgreen.fr/gigwaV2/rest/swagger-ui/index.html?urls.primaryName=GA4GH%20API%20v0.6.0a5#/ga-4gh-rest-controller/searchVariantsUsingPOST
  # https://rest.ensembl.org/documentation/info/gavariants
  
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


#' Get GIGWA Marker Matrix
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
#'   # Get the list of all samples in the selected project
#'   samples <- gigwa_get_samples()
#'
#'   # Get the list of all sequences in the selected project
#'   chroms <- gigwa_get_sequences()
#'   
#'   # Get the marker matrix
#'   geno_data <- gigwa_get_allelematrix(samples = samples[1:5],
#'                                       start   = 0,
#'                                       end     = 1234567,
#'                                       chrom   = chroms[1:3])
#' }
#' @export

gigwa_get_allelematrix <- function(samples = NULL, start = 0, end = "", chrom = NULL, snps = NULL, 
                                   snps_pageSize = 10000, samples_pageSize = 100, simplify = TRUE) {
  germplasmDbIds  <- ""
  variantDbIds    <- ""
  positionRanges  <- ""
  variantSetDbIds <- paste0('"', qbms_globals$state$variant_set_db_id, '"')
  
  if (is.null(samples)) {
    samples <- gigwa_get_samples()
  }
  
  germplasmNames <- samples
  germplasmDbIds <- paste0('"', paste0(paste0(qbms_globals$config$db, "\u00A7", germplasmNames), collapse = '","'), '"')
  
  if (!is.null(snps)) {
    variantNames <- snps
    variantDbIds <- paste0('"', paste0(paste0(qbms_globals$config$db, "\u00A7", variantNames), collapse = '","'), '"')
  }
  
  if (is.null(chrom)) {
    chrom <- gigwa_get_sequences()
  }
  
  referenceStart <- start
  referenceEnd   <- end
  referenceName  <- chrom
  positionRanges <- paste0(referenceName, ":", format(referenceStart, scientific = FALSE), "-", format(referenceEnd, scientific = FALSE))
  positionRanges <- paste0('"', paste0(positionRanges, collapse = '","'), '"')
  
  variants_pageSize <- snps_pageSize
  callsets_pageSize <- samples_pageSize
  
  variants_page <- 0
  callsets_page <- 0
  
  call_url <- get_brapi_url("gigwa_get_allelematrix")
  
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
  
  results <- brapi_post_search_allelematrix(call_url, call_body, FALSE)
  
  pagination <- results$result$pagination
  
  geno_data <- as.data.frame(matrix(nrow = pagination$totalCount[1], ncol = pagination$totalCount[2]))
  
  range_start <- (pagination$page * pagination$pageSize) + 1
  range_end   <- ifelse(pagination$totalPages == (pagination$page + 1), 
                        pagination$totalCount, 
                        (pagination$page + 1) * pagination$pageSize)
  
  page_data <- as.data.frame(results$result$dataMatrices$dataMatrix)
  
  geno_data[range_start[1]:range_end[1], range_start[2]:range_end[2]] <- page_data
  
  resultVariantNames <- results$result$variantDbIds
  resultCallSetDbIds <- results$result$callSetDbIds
  
  remaining_pages <- pagination$totalPages[1] * pagination$totalPages[2] - 1
  
  if (remaining_pages > 0) {
    pb <- utils::txtProgressBar(min = 0, max = remaining_pages, initial = 0, style = 3) 
    
    for (i in 0:(pagination$totalPages[1] - 1)) {
      for (j in 0:(pagination$totalPages[2] - 1)) {
        if (i == 0 & j == 0) next
        
        call_body <- sub("\\{callsets_page\\}", j, sub("\\{variants_page\\}", i, post_schema))
        
        results <- brapi_post_search_allelematrix(call_url, call_body, FALSE)
        
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
        
        if (i == 0) {
          resultCallSetDbIds <- c(resultCallSetDbIds, results$result$callSetDbIds)
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
  
  temp <- merge(resultCallSetDbIds, qbms_globals$state$gigwa_samples, by.x = 1, by.y = "sampleDbId", sort = FALSE)

  colnames(geno_data) <- sub(".*\u00A7", "", temp$germplasmDbId)
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
#'   gigwa_set_run("run1")
#'   
#'   # Get the list of all sequences in the selected project
#'   chroms <- gigwa_get_sequences()
#'
#'   # Get the marker map
#'   geno_map <- gigwa_get_markers(start = 0, end = 12345678, chrom = chroms[7])
#' }
#' @export

gigwa_get_markers <- function(start = NULL, end = NULL, chrom = NULL, simplify = TRUE) {
  startParam <- ifelse(is.null(start), "", paste('"start":', format(start, scientific = FALSE), ","))
  endParam   <- ifelse(is.null(end), "", paste('"end":', format(end, scientific = FALSE), ","))
  
  if (is.null(chrom)) {
    referenceDbIds <- ""
  } else {
    referenceNames <- chrom
    referenceDbIds <- paste0('"', paste0(paste0(qbms_globals$state$study_db_id, "\u00A7\u00A7", referenceNames), collapse = '","'), '"')
  }
  
  call_url <- get_brapi_url("gigwa_get_markers")
  page     <- 0
  
  call_body <- paste0('{', startParam, endParam,
                        '"referenceDbIds": [', referenceDbIds,'],
                            "variantSetDbIds": ["', qbms_globals$state$variant_set_db_id, '"]
                         }')

  results <- brapi_post_search_call(call_url, call_body, FALSE)

  geno_map <- as.data.frame(results$result$data)

  if (simplify) {
    geno_map$alleles <- paste0(geno_map$referenceBases, "/", geno_map$alternateBases)
    
    geno_map <- geno_map[, c("variantNames", "alleles", "referenceName", "start")]
    geno_map <- geno_map[with(geno_map, order(referenceName, start)),]
    
    colnames(geno_map) <- c("rs#", "alleles", "chrom", "pos")
    rownames(geno_map) <- NULL
  }
  
  return(geno_map)
}


#' Get the Metadata of the Current Active GIGWA Run
#'
#' @description
#' This function retrieves the metadata of the current active run
#' as configured in the internal state object using the `gigwa_set_run()` function.
#'
#' @return 
#' A data.frame of all metadata associated with the samples in the selected run.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso 
#' \code{\link{set_qbms_config}}, \code{\link{gigwa_set_run}}
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
#'   gigwa_set_run("1")
#'   
#'   # Get a list of all samples in the selected run
#'   metadata <- gigwa_get_metadata()
#' }
#' @export

gigwa_get_metadata <- function() {
  if (is.null(qbms_globals$state$study_db_id)) {
    stop("No project has been selected yet! You have to set your project first using the `gigwa_set_project()` function")
  }
  
  gigwa_get_samples()
  germplasmDbIds <- paste(qbms_globals$state$gigwa_samples$germplasmDbId, collapse = '","')
  
  call_url  <- get_brapi_url("gigwa_get_metadata")
  call_body <- paste0('{"germplasmDbIds": ["', germplasmDbIds, '"]}')
  
  results <- brapi_post_search_call(call_url, call_body, FALSE)
  
  metadata <- stats::reshape(results$result$data[,-1], idvar = "germplasmName", timevar = "attributeValueDbId", direction = "wide")
  colnames(metadata) <- gsub("value\\.", "", colnames(metadata))
  
  return(metadata)
}
