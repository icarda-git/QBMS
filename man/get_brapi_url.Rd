% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brapi.R
\name{get_brapi_url}
\alias{get_brapi_url}
\title{Get the BrAPI Endpoint URL for a given QBMS function}
\usage{
get_brapi_url(func_name)
}
\arguments{
\item{func_name}{(string) The name of the QBMS function for which the BrAPI endpoint URL is required.}
}
\value{
A string representing the BrAPI endpoint URL.
}
\description{
Constructs the BrAPI endpoint URL for a given QBMS function based on the configured server,
crop, and BrAPI version. The function name is mapped to the corresponding BrAPI call using
internal mapping.
}
\author{
Khaled Al-Shamaa (\email{k.el-shamaa@cgiar.org})
}
