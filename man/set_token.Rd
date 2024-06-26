% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auth.R
\name{set_token}
\alias{set_token}
\title{Set Access Token Response}
\usage{
set_token(token, user = "", expires_in = NULL)
}
\arguments{
\item{token}{The access token string as issued by the authorization server.}

\item{user}{The username (optional).}

\item{expires_in}{The lifetime in seconds of the access token (optional).}
}
\value{
No return value.
}
\description{
If the request for an access token is valid, the authorization server needs 
to generate an access token and return these to the client, typically along 
with some additional properties about the authorization.
}
\author{
Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
}
