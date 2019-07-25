#' Congress API
#'
#' Make request to ProPublic Congress API.
#'
#' @param path path to endpoint
#' @keywords internal
congress_api <- function(path, query = NULL) {
  stopifnot(length(path) == 1L)

  baseurl <- "https://api.propublica.org"
  path <- paste0("congress/v1/", path)
  url <- httr::modify_url(baseurl, path = path, query = query)

  # get api key
  api_key <- get_api_key()

  # build headers
  request <- httr::add_headers(
    "X-API-Key" = api_key,
    "Content-Type" = "application/json",
    "Accept" = "application/json")

  # get response
  response <- httr::GET(url, request)

  # check response type
  if (httr::http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  # parse content
  content <- httr::content(response, "text", encoding = "UTF-8")
  json <- jsonlite::fromJSON(content, simplifyVector = FALSE)

  # check errors
  if (httr::http_error(response) || !identical(json$status, "OK")) {
    stop(
      sprintf(
        "%s\nCongress API request failed [%s]\n%s\n",
        url,
        httr::status_code(response),
        json$errors[[1]]$error
      ),
      call. = FALSE
    )
  }

  # return object
  new_congress(json$results, path)
}

#' Create a new congress object
#' @param json list data from json api
#' @param path path to location of data
#' @keywords internal
new_congress <- function(json, path) {
  # set class
  class(json) <- c("ppcongress_api", class(json))
  attr(json, "path") <- path

  json
}

#' Get user's API key
#' @keywords internal
get_api_key <- function() {
  api_key <- Sys.getenv("PROPUBLICA_API_KEY")
  if (nchar(api_key) == 0) {
    msg <- paste("You must put your congress API key in your .Renviron file.",
                 "It should look like this.\n",
                 "PROPUBLICA_API_KEY=XXXXXXXX")
    stop(msg, call. = FALSE)
  }

  api_key
}

#' @export
print.ppcongress_api <- function(x, ...) {
  cat("<propublica ", attr(x, "path"), ">\n", sep = "")
  utils::str(x, 1, give.attr = FALSE)
  invisible(x)
}

#' Set class of pp object
#'
#' @param x a ppcongres_api object
#' @param name the name of the new class
set_ppclass <- function(x, name) {
  stopifnot(inherits(x, "ppcongress_api"))
  new_name <- sprintf("pp%s", name)
  class(x) <- c(new_name, class(x))
  x
}
