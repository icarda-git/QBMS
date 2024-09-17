### HWSDv2 #####################################################################

#' Download and Setup HWSD v2.0 Data Files to Extract their Data Offline
#'
#' @description
#' Downloads and sets up the HWSD v2.0 data files required to extract soil data offline. The function
#' retrieves the HWSD raster soil unit map and the SQLite database containing soil attributes. If the files 
#' already exist in the specified directory, they are used directly. The function returns an object with 
#' the raster and SQLite connection for further queries.
#'
#' @param data_path String specifying the directory path where HWSD v2.0 data files are stored or should be downloaded (default is './data/').
#' @param timeout Timeout in seconds for downloading each HWSD v2.0 data file (default is 300).
#' 
#' @return 
#' A list object (`con`) containing two items: 
#'   - `raster`: HWSDv2 raster object for spatial queries.
#'   - `sqlite`: Connection to the HWSDv2 SQLite database.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso 
#' \code{\link{get_hwsd2}}
#' 
#' @examples
#' if (interactive()) {
#'   hwsd2 <- ini_hwsd2(data_path = 'C:/Users/Kel-shamaa/Downloads/HWSD v2/')
#' }
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
#' Queries the HWSD v2 database to retrieve soil information for specific locations based on their coordinates.
#' For each location, the function extracts the Soil Mapping Unit (SMU) code and retrieves soil attributes based 
#' on the specified sequence (soil dominance) and depth layer. The function returns the input data frame augmented 
#' with soil data from the HWSDv2 dataset.
#' 
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
#' @param df A data frame containing location information, including longitude and latitude in decimal degrees.
#' @param con The HWSDv2 object returned by the \code{ini_hwsd2()} function, containing the raster and SQLite connection.
#' @param x The column name in the data frame representing longitude (default is 'Longitude').
#' @param y The column name in the data frame representing latitude (default is 'Latitude').
#' @param sequence Integer indicating the soil unit's dominance order within the SMU (default is 1 for the dominant soil). 
#'                 Valid values range from 1 to 12.
#' @param layer String indicating the depth layer for which soil attributes should be retrieved (default is 'D1', with layers ranging from 'D1' to 'D7').
#' 
#' @return 
#' A data frame with the original location data augmented by soil attributes for the specified sequence and layer. 
#' The data frame includes additional columns such as `smu_id`, `SEQUENCE`, `LAYER`, and other soil attributes.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso 
#' \code{\link{ini_hwsd2}}
#' 
#' @examples
#' if (interactive()) {
#'   Location  <- c('Tel-Hadya', 'Terbol', 'Marchouch')
#'   Latitude  <- c(36.016, 33.808, 33.616)
#'   Longitude <- c(36.943, 35.991, -6.716)
#'   sites     <- data.frame(Location, Latitude, Longitude)
#'   
#'   hwsd2 <- ini_hwsd2(data_path = 'C:/Users/Kel-shamaa/Downloads/HWSD v2/')
#'   sites <- get_hwsd2(df = sites, con = hwsd2, x = 'Longitude', y = 'Latitude', 
#'                      sequence = 1, layer = 'D1')
#' }
#' 
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
