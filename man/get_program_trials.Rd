% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qbms.R
\name{get_program_trials}
\alias{get_program_trials}
\title{Internal Function Used to Retrieve the Rough List of Trials}
\usage{
get_program_trials()
}
\value{
A list of trials information.
}
\description{
This function is created for *internal use only* to retrieve the raw list of trials
from the pre-selected (i.e., currently active) crop and breeding program combination
as already configured in the internal state object using `set_crop()` and `set_program()`
functions, respectively.
}
\seealso{
\code{\link{login_bms}}, \code{\link{set_crop}}, \code{\link{set_program}}, \code{\link{list_trials}}
}
\author{
Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
}
