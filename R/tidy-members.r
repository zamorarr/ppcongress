#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble

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
  cnames <- colnames(df)
  to_date <- intersect(c("date_of_birth", "begin_date", "end_date"), cnames)
  to_dttm <- intersect(c("last_updated"), cnames)
  to_integer <- intersect(c("seniority", "next_election", "senate_class"), cnames)

  df[to_date] <- lapply(df[to_date], as.Date)
  df[to_dttm] <- lapply(df[to_dttm], as.POSIXct)
  df[to_integer] <- lapply(df[to_integer], as.integer)
  df
}

#' @rdname as_tibble.ppmembers
#' @export
as_tibble.ppmember <- function(x, ...) {

  # convert list to data frame
  mems <- replace_nulls(x)
  df <- tibble::as_tibble(mems)

  # fix column types
  cnames <- colnames(df)
  to_date <- intersect(c("date_of_birth", "most_recent_vote"), cnames)
  to_dttm <- intersect(c("last_updated"), cnames)
  to_integer <- intersect(c("seniority", "next_election"), cnames)

  df[to_date] <- lapply(df[to_date], as.Date)
  df[to_dttm] <- lapply(df[to_dttm], as.POSIXct)
  df[to_integer] <- lapply(df[to_integer], as.integer)
  df
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
  df$date <- paste(df$date, df$time)
  df$time <- NULL

  cnames <- colnames(df)
  to_dttm <- intersect(c("date"), cnames)
  to_integer <- intersect(c("congress", "session", "roll_call"), cnames)
  to_df <- intersect(c("bill", "total", "amendment"), cnames)

  df[to_dttm] <- lapply(df[to_dttm], as.POSIXct)
  df[to_integer] <- lapply(df[to_integer], as.integer)
  df[to_df] <- lapply(df[to_df], function(x) {
    lapply(x, tibble::as_tibble)
  })

  df
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
  cnames <- colnames(df)
  to_date <- intersect(c("cosponsored_date", "introduced_date", "last_major_action_date"), cnames)
  to_integer <- intersect(c("congress", "cosponsors"), cnames)
  to_df <- intersect(c("cosponsors_by_party"), cnames)

  df[to_date] <- lapply(df[to_date], as.Date)
  df[to_integer] <- lapply(df[to_integer], as.integer)
  df[to_df] <- lapply(df[to_df], function(x) {
    lapply(x, tibble::as_tibble)
  })

  df
}
