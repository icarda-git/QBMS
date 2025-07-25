% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gigwa.R
\name{gigwa_list_dbs}
\alias{gigwa_list_dbs}
\title{List GIGWA Databases}
\usage{
gigwa_list_dbs()
}
\value{
A list of databases available on the connected GIGWA server.
}
\description{
Retrieve the list of available databases from the connected GIGWA server.
An active connection is required. If not connected, the function will throw an error.
}
\examples{
if (interactive()) {
  set_qbms_config("https://gigwa.southgreen.fr/gigwa/", 
                  time_out = 300, engine = "gigwa", no_auth = TRUE)
  gigwa_list_dbs()
}
}
\seealso{
\code{\link{set_qbms_config}}
}
\author{
Khaled Al-Shamaa (\email{k.el-shamaa@cgiar.org})
}
