% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/terraclimate.R
\name{ini_terraclimate}
\alias{ini_terraclimate}
\title{Download TerraClimate netCDF Data Files to Extract their Data Offline}
\usage{
ini_terraclimate(
  from = "2019-09-01",
  to = "2022-06-30",
  clim_vars = c("ppt", "tmin", "tmax"),
  data_path = "./data/",
  timeout = 300
)
}
\arguments{
\item{from}{Start date as a string in the 'YYYY-MM-DD' format.}

\item{to}{End date as a string in the 'YYYY-MM-DD' format.}

\item{clim_vars}{List of all climate variables to be imported. Valid list includes: \emph{aet, def, pet,
ppt, q, soil, srad, swe, tmax, tmin, vap, ws, vpd, and PDSI}. Default is NULL for all.}

\item{data_path}{String containing the directory path where downloaded netCDF files exist (default is './data/')}

\item{timeout}{Timeout in seconds to download each netCDF raster file (default is 300).}
}
\description{
Download TerraClimate netCDF Data Files to Extract their Data Offline
}
\examples{
if (interactive()) {
  ini_terraclimate('2018-09-01', '2019-06-30', c('ppt', 'tmin', 'tmax'))
  
  x <- c(-6.716, 35.917, 76.884)
  y <- c(33.616, 33.833, 23.111)
  
  a <- get_terraclimate(y, x, '2018-09-01', '2019-06-30', c('ppt', 'tmin', 'tmax'))
  
  a$climate[[1]]
  a$biovars[[1]]
  
  b <- get_terraclimate(y, x, '2018-09-01', '2019-06-30', c('ppt', 'tmin', 'tmax'), offline = TRUE)
  
  b$climate[[1]]
  b$biovars[[1]]
}
}
\seealso{
\code{\link{get_terraclimate}}
}
\author{
Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
}
