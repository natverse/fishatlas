# hidden
fishatlas_connection <- function(server="https://fishatlas.neuro.mpg.de",
                                 token= "JHjK0tWM2w3cYDfzmCOx4VvlxgEIcPLx",
                                 conn=NULL,
                                 config=httr::config()) {
  if (!is.null(conn))
    return(conn)
  # collect any curl options defined as environment variables
  config=fishatlas_curl_options(config)
  conn=list(server = server, token = token, config=config)
  class(conn)='dv_conn'
  conn
}


# hidden
fishatlas_last_connection <- function(){
  conns = .fishatlasr_statevars$connections
  num_conns = length(conns)
  if (num_conns)
    conns[[num_conns]]
  else NULL
}

# hidden
fishatlas_cached_connection <- function(conn=NULL){
  if (is.null(conn))
    return(NULL)
  open_connections = names(.fishatlasr_statevars$connections)
  if (!length(open_connections))
    return(NULL)
  for (thisconn in open_connections) {
    thisconn = .fishatlasr_statevars$connections[[thisconn]]
    checkfields = c("server", "username", "authname", "authtype")
    checkfields = checkfields[!sapply(conn[checkfields],
                                      is.null)]
    if (isTRUE(all.equal(thisconn[checkfields], conn[checkfields])))
      return(thisconn)
  }
  return(NULL)
}

# hidden
fishatlas_cache_connection <- function(conn){
  .fishatlasr_statevars$connections[[fishatlas_connection_fingerprint(conn)]] = conn
}

# hidden
fishatlas_connection_fingerprint <- function(conn){
  paste(c(conn$server, httr::cookies(conn$authresponse)),
        collapse = "")
}

# hidden
fishatlas_login <- function(conn = NULL, Cache = TRUE, Force = FALSE, ...){
  if (is.character(conn) && grepl("^http", conn)) {
    stop("To connect to : ", conn, ", you must name the server argument i.e.\n",
         sprintf("  fishatlas_login(server=\"%s\")", conn))
  }
  if (is.null(conn)) {
    if (!length(pairlist(...))) {
      conn = fishatlas_last_connection()
    }
    if (is.null(conn))
      conn = fishatlas_connection(...)
  }
  if (!Force) {
    if (!is.null(conn$authresponse))
      return(invisible(conn))
    cached_conn = fishatlas_cached_connection(conn)
    if (!is.null(cached_conn))
      return(invisible(cached_conn))
  }
  if (isTRUE(conn$nologin)) {
    conn$authresponse = httr::GET(url = conn$server)
    httr::stop_for_status(conn$authresponse)
    res_cookies = httr::cookies(conn$authresponse)
    GAPS_row = grepl("GAPS", res_cookies$name)
    if (any(GAPS_row)) {
      token_value = res_cookies$value[GAPS_row][1]
      conn$config = httr::add_headers(`X-CSRFToken` = token_value,
                                      referer = "https://fishatlas.neuro.mpg.de/neurons/main_page")
    }
    else warning("I can't seem to find a GAPS token.", "You will not be able to POST to this site!")
  }else {
    if(is.null(conn$config)) conn$config=httr::config()
    conn$config = c(
      conn$config,
      httr::add_headers(
        Origin = "https://fishatlas.neuro.mpg.de",
        Host = "fishatlas.neuro.mpg.de",
        `X-CSRFToken` = conn$token,
        referer = "https://fishatlas.neuro.mpg.de/neurons/main_page",
        `Content-Type` = "application/json;charset=UTF-8",
        Cookie = "_pk_ref.1.1cf2=%5B%22%22%2C%22%22%2C1551187271%2C%22https%3A%2F%2Fwww.google.com%2F%22%5D; _pk_ses.1.1cf2=*; fe_typo_user=e5430f0716d8b4ea832b472d43c24446; _pk_id.1.1cf2=0e630dfe08158ed8.1551187271.1.1559211329.1551187271.; __unam=76e417d-16929f75e90-a871227-2; csrftoken=JHjK0tWM2w3cYDfzmCOx4VvlxgEIcPLx"
      )
    )
    conn$authresponse = httr::GET(url = conn$server,con=conn$config)
    httr::stop_for_status(conn$authresponse)
  }
  conn$cookies = unlist(httr::cookies(conn$authresponse))
  conn$config = c(conn$config, httr::set_cookies(conn$cookies))
  if (Cache)
    fishatlas_cache_connection(conn)
  invisible(conn)
}

# hidden
getenvoroption <- function(vars, prefix="fishatlas_"){
  fullvars=paste0(prefix, vars)
  res=Sys.getenv(fullvars, names = T, unset = NA)
  if(all(is.na(res))){
    # no env variables are set, let's try options
    res=do.call(options, as.list(fullvars))
  } else {
    # convert environment variables into options style list
    res=as.list(res)
    # replace missing values with NULL
    res=sapply(res, function(x) if(is.na(x)) NULL else x)
  }
  # give result the original variable names
  names(res)=vars
  res
}

# hidden
fishatlas_curl_options <- function(extra_opts=httr::config()) {
  envs=Sys.getenv()
  curlopts=envs[grepl("^fishatlas_curl_", names(envs))]
  if (length(curlopts)) {
    names(curlopts) = sub("fishatlas_curl_", "", names(curlopts))
  } else {
    curlopts = list()
  }
  keep=setdiff(names(curlopts), names(extra_opts$options))
  curlopts=as.list(curlopts[keep])
  # environment variables come in as strings, but sometimes we want numbers
  curlopts <- sapply(curlopts, function(x) switch(x, `0`=0L, `1`=1L, x), simplify = F)
  c(extra_opts, do.call(httr::config, curlopts))
}

## Login interactively
# selServ <- selenium(jvmargs = c("-Dwebdriver.chrome.verboseLogging=true"))
# remDr <- remoteDriver(port = 4567L, browserName = "chrome")
# remDr$open()
# remDr$navigate(url)
# df=remDr$getPageSource()
