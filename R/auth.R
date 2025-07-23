#' Login Pop-Up Window
#'
#' @description
#' Opens a GUI pop-up window using Tcl/Tk to prompt the user for their username and password.
#' The window title and prompt message adapt based on the type of server being used (e.g., BMS, GIGWA).
#'
#' @return
#' A vector containing the inserted username and password, with names \code{usr} and \code{pwd} respectively.
#' 
#' @author
#' Khaled Al-Shamaa (\email{k.el-shamaa@cgiar.org})

get_login_details <- function() {
  if (is.null(qbms_globals$config$engine)) {
    stop("No server has been defined yet! You have to set your server configurations first using the `set_qbms_config()` function")
  }
  
  if (qbms_globals$config$engine == "bms") { server <- "BMS" }
  if (qbms_globals$config$engine == "breedbase") { server <- "BreedBase" }
  if (qbms_globals$config$engine == "gigwa") { server <- "GIGWA" }
  if (qbms_globals$config$engine == "germinate") { server <- "Germinate" }
  
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
#' Stores the access token and associated details (such as username and expiration time) in the internal state.
#' The token is typically retrieved from the server during login and used for subsequent API requests.
#'
#' @param token The access token string issued by the authorization server.
#' @param user The username associated with the token (optional).
#' @param expires_in The lifetime of the access token in seconds (optional, default is 3600 seconds).
#' 
#' @return
#' No return value. Updates the internal state with the token info.
#' 
#' @author
#' Khaled Al-Shamaa (\email{k.el-shamaa@cgiar.org})
#' 
#' @export

set_token <- function(token, user = '', expires_in = 3600) {
  expires_in <- as.numeric(Sys.time()) + expires_in

  qbms_globals$state$token <- token
  qbms_globals$state$user  <- user
  qbms_globals$state$expires_in <- expires_in
}


#' Login using OAuth 2.0 Authentication 
#'
#' @description
#' Performs OAuth 2.0 authentication by sending the user to the authorization URL and exchanging the 
#' authorization code for an access token. This function supports caching of tokens for subsequent requests.
#'
#' @param authorize_url The URL where the client is redirected for user authorization.
#' @param access_url The URL used to exchange an authorization code for an access token.
#' @param client_id The client ID (consumer key) provided by the authorization server.
#' @param client_secret The client secret provided by the authorization server (optional).
#' @param redirect_uri The URL where the user will be redirected after authorization (default is http://localhost:1410).
#' @param scope Scopes to be requested from the resource owner.
#' 
#' @return
#' No return value. Updates the internal state with the access token and additional details.
#' 
#' @author
#' Khaled Al-Shamaa (\email{k.el-shamaa@cgiar.org})
#' @export

login_oauth2 <- function(authorize_url, access_url, client_id, client_secret = NULL, redirect_uri = "http://localhost:1410", scope = NULL) {
  client <- httr2::oauth_client(
    id = client_id,
    secret = client_secret,
    token_url = access_url,
    name = "QBMS"
  )
  
  token <- httr2::oauth_flow_auth_code(
    client = client,
    auth_url = authorize_url,
    redirect_uri = redirect_uri,
    scope = scope
  )
  
  set_token(token$id_token, '', token$expires_in)
}


#' Login to the Server
#'
#' @description
#' Connects to the BMS or related server using a username and password. If these are not provided,
#' a pop-up window will prompt the user to enter their credentials. The function handles authentication 
#' and stores the resulting access token internally for subsequent requests.
#'
#' @param username The username (optional, default is NULL). If not provided, the pop-up window is triggered.
#' @param password The password (optional, default is NULL). If not provided, the pop-up window is triggered.
#' @param encoding Specifies how the request body should be encoded: \code{form} (application/x-www-form-urlencoded), 
#'                 \code{multipart} (multipart/form-data), or \code{json} (application/json). Default is "json".
#' 
#' @return
#' No return value. The access token is stored internally for future use.
#' 
#' @author
#' Khaled Al-Shamaa (\email{k.el-shamaa@cgiar.org})
#' 
#' @examples
#' if(interactive()) {
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench", engine = "bms")
#'   
#'   # Login using your BMS account (interactive mode)
#'   login_bms()
#'   
#'   # You can pass BMS username and password as parameters (batch mode)
#'   # login_bms("username", "password")
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

  req <- httr2::request(utils::URLencode(call_url))
  req <- httr2::req_timeout(req, qbms_globals$config$time_out)
  
  if (encoding == "json") {
    req <- httr2::req_body_json(req, call_body)
  } else {
    req <- httr2::req_body_form(req, username = credentials["usr"], password = credentials["pwd"])
  }
  
  resp <- httr2::req_perform(req)
  
  content <- httr2::resp_body_json(resp)
  
  if (!is.null(content$errors)) {
    stop(content$errors[[1]]$message)
  }
  
  set_token(content$access_token,
            content$userDisplayName,
            content$expires_in)
}


#' Login to the Germinate Server
#'
#' @param username The username (optional, default is NULL).
#' @param password The password (optional, default is NULL).
#' 
#' @return
#' No return value. The access token is stored internally for future use.
#' 
#' @author
#' Khaled Al-Shamaa (\email{k.el-shamaa@cgiar.org})
#' 
#' @export

login_germinate <- function(username = NULL, password = NULL) {
  if (is.null(username) || is.null(password)) {
    credentials <- get_login_details()
  } else {
    credentials <- c(usr = username, pwd = password)
  }
  
  call_url  <- paste0(qbms_globals$config$base_url, "/token")
  call_body <- list(username = credentials["usr"], password = credentials["pwd"])
  
  req <- httr2::request(utils::URLencode(call_url))
  req <- httr2::req_timeout(req, qbms_globals$config$time_out)
  
  req <- httr2::req_body_json(req, call_body)

  resp <- httr2::req_perform(req)
  
  content <- httr2::resp_body_json(resp)
  
  if (!is.null(content$errors)) {
    stop(content$errors[[1]]$message)
  }
  
  content$expires_in <- content$createdOn + content$lifetime
  
  set_token(content$token,
            content$username,
            content$expires_in)
}

#' Login to the BreedBase Server
#'
#' @description
#' Logs in to the BreedBase server using a username and password. If credentials are not provided,
#' a pop-up window will prompt the user. The function is a wrapper around the \code{login_bms()} function,
#' with encoding set to \code{form}.
#'
#' @param username The username (optional, default is NULL).
#' @param password The password (optional, default is NULL).
#' 
#' @return
#' No return value. The access token is stored internally for future use.
#' 
#' @author
#' Khaled Al-Shamaa (\email{k.el-shamaa@cgiar.org})
#' 
#' @examples
#' if(interactive()) {
#'   set_qbms_config("https://cassavabase.org/", engine = "breedbase")
#'   
#'   # Login using your BreedBase account (interactive mode)
#'   login_breedbase()
#'   
#'    # You can pass BreedBase username and password as parameters (batch mode)
#'    # login_breedbase("username", "password")
#' }
#' 
#' @export

login_breedbase <- function(username = NULL, password = NULL) {
  login_bms(username, password, encoding = "form")
  
  if (is.null(qbms_globals$state$token)) {
    stop("Bad credentials") 
  }
}


#' Login to the GIGWA Server
#'
#' @description
#' Connect to the GIGWA server. If the \code{username} or \code{password} parameters are missing,
#' a login window will be triggered to capture these details.
#'
#' All connection settings (server URL, port, API path, and protocol) are read from the
#' \code{qbms_config()} list. The function will request an authentication token from the server
#' and update the \code{qbms_state()} list with the token.
#'
#' @param username The GIGWA username (optional, default is NULL).
#' @param password The GIGWA password (optional, default is NULL).
#' 
#' @return 
#' No return value. The authentication token will be stored internally.
#' 
#' @author 
#' Khaled Al-Shamaa (\email{k.el-shamaa@cgiar.org})
#' 
#' @examples
#' if (interactive()) {
#'   set_qbms_config("http://localhost:59395/gigwa/index.jsp", time_out = 300, engine = "gigwa")
#'
#'   # Login using your GIGWA account (interactive mode)
#'   login_gigwa()
#'   
#'   # You can pass GIGWA username and password as parameters (batch mode)
#'   # login_gigwa("gigwadmin", "nimda")
#' }
#' @export

login_gigwa <- function(username = NULL, password = NULL) {
  if (is.null(username) || is.null(password)) {
    credentials <- get_login_details()
  } else {
    credentials <- c(usr = username, pwd = password)
  }
  
  call_url  <- paste0(qbms_globals$config$base_url, "/gigwa/generateToken")
  call_body <- list(username = credentials["usr"], password = credentials["pwd"])
  
  req <- httr2::request(utils::URLencode(call_url))
  req <- httr2::req_body_json(req, call_body)
  req <- httr2::req_timeout(req, qbms_globals$config$time_out)
  
  resp <- httr2::req_perform(req)
  
  if (httr2::resp_status(resp) == 403 || credentials["usr"] == "" || credentials["pwd"] == "") {
    stop("403 Forbidden")
  }
  
  content <- httr2::resp_body_json(resp)
  
  set_token(content$token)
}

#' Login to a BrAPI Server
#'
#' @description
#' Authenticates with a BrAPI server using a username and password. If credentials 
#' are not provided, interactive prompt will request them from the user. This 
#' function serves as a wrapper for all \code{login_*} authentication methods and 
#' accepts additional parameters via \code{...}
#'
#' @param username The username (optional, default is NULL).
#' @param password The password (optional, default is NULL).
#' @param ... Additional arguments passed to \code{\link{login_oauth2}}.
#' 
#' @return
#' No return value. On success, the access token is stored internally for future use.
#' 
#' @author
#' Khaled Al-Shamaa (\email{k.el-shamaa@cgiar.org})
#' 
#' @seealso \code{\link{login_oauth2}}
#' 
#' @export

login <- function(username = NULL, password = NULL, ...) {
  
  if (qbms_globals$config$engine == "bms") {
    
    login_bms(username, password)
    
  } else if (qbms_globals$config$engine == "breedbase") {
    
    login_breedbase(username, password)
    
  } else if (qbms_globals$config$engine == "gigwa") {
    
    login_gigwa(username, password)
    
  } else if (qbms_globals$config$engine == "germinate") {
    
    login_germinate(username, password)
    
  } else if (qbms_globals$config$engine == "ebs") {
    
    # Get login_oauth2 function parameters
    oauth2_args <- as.list(formals(login_oauth2))
    
    # Get names of required parameters (those with no default)
    required_args <- names(oauth2_args)[sapply(oauth2_args, is.symbol)]
    
    # Capture what was actually passed via ...
    passed_args <- list(...)
    
    # Check if all required arguments are present
    missing_args <- setdiff(required_args, names(passed_args))
    
    if (length(missing_args) > 0) {
      stop(paste("Missing required arguments for login_oauth2:", paste(missing_args, collapse = ", ")))
    }
    
    # If all required args are present, call inner function
    do.call(login_oauth2, passed_args)
  }
}