#' Convert json object to a dataframe
#' @param x list object
#' @param ... other parameters passed to \code{\link[tibble]{as_tibble}}:
#' @export
as_tibble.ppbills_search <- function(x, ...) {
  # check there are bills
  if (!("bills" %in% names(x))) {
    stop("There is no bills to parse", call. = FALSE)
  }

  # convert list to data frame
  df <- transpose_to_df(x$bills)

  # fix column types
  fix_column_types(df)
}

#' @rdname as_tibble.ppbills_search
#' @export
as_tibble.ppbills_recent <- function(x, ...) {
  as_tibble.ppbills_search(x, ...)
}

#' @rdname as_tibble.ppbills_search
#' @export
as_tibble.ppbills_search_subjects <- function(x, ...) {
  df <- transpose_to_df(x$subjects)

  # fix column types
  fix_column_types(df)
}

#' @rdname as_tibble.ppbills_search
#' @export
as_tibble.ppbills_upcoming <- function(x, ...) {
  as_tibble.ppbills_search(x, ...)
}

#' @rdname as_tibble.ppbills_search
#' @export
as_tibble.ppbill <- function(x, ...) {
  # convert list to data frame
  bill <- replace_nulls(x)
  class(bill) <- "list"

  # lists need to be wrapped in a list to have length 1
  to_wrap <- vapply(bill, is.list, logical(1L))
  bill[to_wrap] <- lapply(bill[to_wrap], function(y) list(y))

  # convert to data frame
  df <- tibble::as_tibble(bill)

  # fix column types
  fix_column_types(df)
}

#' @rdname as_tibble.ppbills_search
#' @export
as_tibble.ppbill_amendments <- function(x, ...) {
  # convert list to data frame
  df <- transpose_to_df(x$amendments)

  # fix column types
  fix_column_types(df)
}

#' @rdname as_tibble.ppbills_search
#' @export
as_tibble.ppbill_subjects <- function(x, ...) {
  # convert list to data frame
  x$subjects <- purrr::map_dfr(x$subjects, tibble::as_tibble)
  x$subjects <- list(x$subjects)
  x <- replace_nulls(x)
  class(x) <- "list"
  df <- as_tibble(x)

  # fix column types
  fix_column_types(df)
}

#' @rdname as_tibble.ppbills_search
#' @export
as_tibble.ppbill_related <- function(x, ...) {
  # check there are bills
  if (!("related_bills" %in% names(x))) {
    stop("There is no related bills to parse", call. = FALSE)
  }

  # convert list to data frame
  df <- transpose_to_df(x$related_bills)

  # fix column types
  fix_column_types(df)
}

#' @rdname as_tibble.ppbills_search
#' @export
as_tibble.ppbill_cosponsors <- function(x, ...) {
  # check there are bills
  if (!("cosponsors" %in% names(x))) {
    stop("There is no cosponsors to parse", call. = FALSE)
  }

  # convert list to data frame
  df <- transpose_to_df(x$cosponsors)

  # fix column types
  fix_column_types(df)
}
