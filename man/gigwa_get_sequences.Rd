% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gigwa.R
\name{gigwa_get_sequences}
\alias{gigwa_get_sequences}
\title{Get the Sequences of the Current Active GIGWA Project}
\usage{
gigwa_get_sequences()
}
\value{
A vector of all sequences in the selected project.
}
\description{
This function retrieves the list sequences of the current active project
as configured in the internal state object using the `gigwa_set_project()` function.
}
\examples{
if (interactive()) {
  # Configure your GIGWA connection
  set_qbms_config("https://gigwa.southgreen.fr/gigwa/", 
                  time_out = 300, engine = "gigwa", no_auth = TRUE)

  # Select a database by name
  gigwa_set_db("Sorghum-JGI_v1")

  # Select a project by name
  gigwa_set_project("Nelson_et_al_2011")
  
  # Get a list of all samples in the selected project
  chroms <- gigwa_get_sequences()
}
}
\seealso{
\code{\link{set_qbms_config}}, \code{\link{gigwa_set_project}}
}
\author{
Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
}
