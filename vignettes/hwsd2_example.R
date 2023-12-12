#' Harmonized World Soils Database version 2.0 (HWSD v2.0)
#' https://gaez.fao.org/pages/hwsd
#' 
#' For more details, you can get the HWSD v2.0 technical report and instructions
#' https://doi.org/10.4060/cc3823en

install.packages("remotes")
remotes::install_github("icarda-git/QBMS")

library(QBMS)

#' create a simple data.frame for a list of locations and their coordinates
Location  <- c('Tel-Hadya', 'Terbol', 'Marchouch')
Latitude  <- c(36.016, 33.808, 33.616)
Longitude <- c(36.943, 35.991, -6.716)

sites <- data.frame(Location, Latitude, Longitude)

#' initiate, download, and setup the HWSD v2 in a given local directory
hwsd2 <- ini_hwsd2(data_path = 'C:/Users/Kel-shamaa/Downloads/HWSD v2/')

#' query soil attributes for given sites using the HWSD v2 connection object
#'  
#' sequence parameter, range between 1 and 12 (max), 1 is the dominant soil. 
#' returned df has SHARE column refers to share% 
#' 
#' layer parameter refers to depth layer (D1 to D7). 
#' returned df has TOPDEP/BOTDEP columns represent top/bottom layer depth in cm.
sites <- get_hwsd2(df = sites, con = hwsd2, x = 'Longitude', y = 'Latitude', sequence = 1, layer = 'D1')

## Advanced Users ##############################################################
#' # check the HWSD v2 raster 
#' print(hwsd2$raster)
#' 
#' # display the metadata for the layers table
#' DBI::dbGetQuery(hwsd2$sqlite, 'select * from HWSD2_LAYERS_METADATA')
#' 
#' # the lookup tables are shown for the coded fields
#' # for example, the USDA Texture Class codes (the column TEXTURE_USDA value)
#' # are linked to their names in table D_TEXTURE_USDA
#' DBI::dbGetQuery(hwsd2$sqlite, 'select * from D_TEXTURE_USDA')
#' 
#' # disconnect (close) the SQLite connection
#' DBI::dbDisconnect(hwsd2$sqlite)
