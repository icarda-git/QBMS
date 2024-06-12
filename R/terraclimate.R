### TerraClimate ###############################################################

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
