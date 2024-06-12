#' Login Pop-Up Window
#'
#' @description
#' Builds a GUI pop-up window using Tcl/Tk to insert the username and password.
#'
#' @return
#' A vector of inserted username and password.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}

get_login_details <- function() {
  if (is.null(qbms_globals$config$engine)) {
    stop("No server has been defined yet! You have to set your server configurations first using the `set_qbms_config()` function")
  }
  
  if (qbms_globals$config$engine == "bms") { server <- "BMS" }
  if (qbms_globals$config$engine == "breedbase") { server <- "BreedBase" }
  if (qbms_globals$config$engine == "gigwa") { server <- "GIGWA" }
  
  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, paste("Login", server, "Server"))
  
  ss <- paste("Please enter your", server, "login details")
  tcltk::tkgrid(tcltk::tklabel(tt, text = ss), columnspan = 2, padx = 50, pady = 10)
  
  usr <- tcltk::tclVar("")
  pwd <- tcltk::tclVar("")
  
  user_label <- tcltk::tklabel(tt, text = "Username:")
  pass_label <- tcltk::tklabel(tt, text = "Password:")
  
  user_input <- tcltk::tkentry(tt, width = "30", textvariable = usr)
  pass_input <- tcltk::tkentry(tt, width = "30", textvariable = pwd, show = "*")
  
  tcltk::tkgrid(user_label, user_input, sticky = "ew", padx = 5)
  tcltk::tkgrid(pass_label, pass_input, sticky = "ew", padx = 5)
  
  on_okay <- function() {
    tcltk::tkdestroy(tt)
  }
  
  ok_button <- tcltk::tkbutton(tt, text = " OK ", command = on_okay)
  tcltk::tkbind(pass_input, "<Return>", on_okay)
  tcltk::tkgrid(ok_button, columnspan = 2, pady = 5)
  
  tcltk::tkfocus(tt)
  tcltk::tkwait.window(tt)
  
  invisible(c(usr = tcltk::tclvalue(usr), pwd = tcltk::tclvalue(pwd)))
}


#' Set Access Token Response
#'
#' @description
#' If the request for an access token is valid, the authorization server needs 
#' to generate an access token and return these to the client, typically along 
#' with some additional properties about the authorization.
#'
#' @param token The access token string as issued by the authorization server.
#' @param user The username (optional).
#' @param expires_in The lifetime in seconds of the access token (optional).
#' 
#' @return
#' No return value.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @export

set_token <- function(token, user = '', expires_in = NULL) {
  if (is.null(expires_in)) {
    expires_in <- as.numeric(Sys.time()) + 3600
  }
  
  qbms_globals$state$token <- token
  qbms_globals$state$user  <- user
  qbms_globals$state$expires_in <- expires_in
}


#' Login Using OAuth 2.0 Authentication 
#'
#' @description
#' If the request for an access token is valid, the authorization server needs 
#' to generate an access token and return these to the client, typically along 
#' with some additional properties about the authorization.
#'
#' @param authorize_url URL to send the client for authorization.
#' @param access_url URL used to exchange unauthenticated for authenticated token.
#' @param client_id Consumer key, also sometimes called the client ID.
#' @param client_secret Consumer secret, also sometimes called the client secret.
#' @param redirect_uri The URL that the user will be redirected to after authorization is complete (default is http://localhost:1410).
#' @param oauth2_cache A logical value or a string. TRUE means to cache using the default cache file .httr-oauth, FALSE means don't cache, 
#'                     and NA means to guess using some sensible heuristics. A string means use the specified path as the cache file.
#'                     Default is FALSE (i.e., don't cache).
#' 
#' @return
#' No return value.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @export

login_oauth2 <- function(authorize_url, access_url, client_id, client_secret = NULL, redirect_uri = "http://localhost:1410", oauth2_cache = FALSE) {
  app <- httr::oauth_app(appname = "QBMS", key = client_id, secret = client_secret, redirect_uri = redirect_uri)
  
  endpoint <- httr::oauth_endpoint(authorize = authorize_url, access = access_url)
  
  token <- httr::oauth2.0_token(endpoint, app, cache = oauth2_cache)
  
  set_token(token$credentials$id_token, '', token$credentials$expires_in)
}


#' Login to the Server
#'
#' @description
#' Connects to the server. If the username or password parameters are missing,
#' then a login window will pop up to insert the username and password.
#'
#' All other connection parameters (i.e., server IP or domain, connection port,
#' API path, and connection protocol e.g., http://) will be retrieved from the
#' qbms_config list.
#'
#' This function will update both the qbms_config list (brapi connection
#' object in the con key) and the qbms_state list (token value in the token key).
#'
#' @param username The username (optional, default is NULL).
#' @param password The password (optional, default is NULL).
#' @param encoding How should the named list body be encoded? Can be one of form 
#'                 (application/x-www-form-urlencoded), multipart (multipart/form-data), 
#'                 or json (application/json).
#' 
#' @return
#' No return value.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @examples
#' if(interactive()) {
#'   # Configure your BMS connection
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'
#'   # Login using your BMS account (interactive mode)
#'   # You can pass the BMS username and password as parameters (batch mode)
#'   login_bms()
#' }
#' 
#' @export

login_bms <- function(username = NULL, password = NULL, encoding = "json") {
  if (is.null(username) || is.null(password)) {
    credentials <- get_login_details()
  } else {
    credentials <- c(usr = username, pwd = password)
  }
  
  call_url  <- paste0(qbms_globals$config$base_url, "/brapi/v1/token")
  call_body <- list(username = credentials["usr"], password = credentials["pwd"])
  
  response <- httr::POST(url = utils::URLencode(call_url), body = call_body, encode = encoding,
                         httr::timeout(qbms_globals$config$time_out))
  
  if (!is.null(httr::content(response)$errors)) {
    stop(httr::content(response)$errors[[1]]$message)
  }
  
  set_token(httr::content(response)$access_token,
            httr::content(response)$userDisplayName,
            httr::content(response)$expires_in)
}


#' Login to the BreedBase Server
#'
#' @description
#' Logs in to the BreedBase server.
#'
#' @param username The username (optional, default is NULL).
#' @param password The password (optional, default is NULL).
#' 
#' @return
#' No return value.
#' 
#' @author
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @export

login_breedbase <- function(username = NULL, password = NULL) {
  login_bms(username, password, encoding = "form")
  
  if (is.null(qbms_globals$state$token)) {
    stop("Bad credentials") 
  }
}
