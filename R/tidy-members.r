#' Convert json object to a dataframe
#' @param x list object
#' @param ... other parameters passed to \code{\link[tibble]{as_tibble}}:
#' @export
as_tibble.ppmembers <- function(x, ...) {

  # convert list to data frame
  mems <- x$members
  mems <- purrr::transpose(mems)
  mems <- purrr::map(mems, replace_nulls)
  mems <- purrr::simplify_all(mems)
  df <- tibble::as_tibble(mems)

  # fix column types
  fix_column_types(df)
}

#' @rdname as_tibble.ppmembers
#' @export
as_tibble.ppmember <- function(x, ...) {
  # convert list to data frame
  mems <- replace_nulls(x)
  class(mems) <- "list"
  df <- tibble::as_tibble(mems)

  # fix column types
  fix_column_types(df)
}

#' @rdname as_tibble.ppmembers
#' @export
as_tibble.ppmembers_new <- function(x, ...) {
  as_tibble.ppmembers(x, ...)
}

#' @rdname as_tibble.ppmembers
#' @export
as_tibble.ppmembers_location <- function(x, ...) {
  df <- lapply(x, as_tibble.ppmember)
  do.call(rbind, df)
}

#' @rdname as_tibble.ppmembers
#' @export
as_tibble.ppmembers_leaving <- function(x, ...) {
  as_tibble.ppmembers(x, ...)
}

#' @rdname as_tibble.ppmembers
#' @export
as_tibble.ppmember_votes <- function(x, ...) {

  # convert list to data frame
  votes <- x$votes
  votes <- purrr::transpose(votes)
  votes <- purrr::map(votes, replace_nulls)
  votes <- purrr::simplify_all(votes)
  df <- tibble::as_tibble(votes)

  # fix column types
  fix_column_types(df)
}

#' @rdname as_tibble.ppmembers
#' @export
as_tibble.ppmember_bills <- function(x, ...) {

  # convert list to data frame
  bills <- x$bills
  bills <- purrr::transpose(bills)
  bills <- purrr::map(bills, replace_nulls)
  bills <- purrr::simplify_all(bills)
  df <- tibble::as_tibble(bills)

  # fix column types
  fix_column_types(df)
}
