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
  cnames <- colnames(df)
  to_integer <- intersect(c("congress"), cnames)
  to_date <- intersect(c("introduced_date", "last_vote", "house_passage", "senate_passage",
                         "enacted", "vetoed", "latest_major_action_date", "legislative_day"), cnames)
  to_dttm <- intersect(c("scheduled_at"), cnames)
  to_character <- intersect(c("gpo_pdf_uri"), cnames)
  to_df <- intersect(c("cosponsors_by_party"), cnames)

  df[to_integer] <- lapply(df[to_integer], as.integer)
  df[to_date] <- lapply(df[to_date], as.Date)
  df[to_dttm] <- lapply(df[to_dttm], function(x) {
    dttm <- gsub(":([0-9]{2})$", "\\1", x)
    as.POSIXct(dttm, tz = "America/New_York", format = "%Y-%m-%dT%H:%M:%S%z")
  })
  df[to_character] <- lapply(df[to_character], as.character)
  df[to_df] <- lapply(df[to_df], function(x) {
    lapply(x, tibble::as_tibble)
  })
  df
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
  cnames <- colnames(df)
  to_logical <- intersect(c("has_bills", "has_statements"), cnames)
  df[to_logical] <- lapply(df[to_logical], as.logical)
  df
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
  cnames <- colnames(df)
  to_date <- intersect(c("introduced_date", "last_vote", "house_passage",
                         "latest_major_action_date", "house_passage_vote",
                         "senate_passage_vote"), cnames)
  to_df <- intersect(c("cosponsors_by_party"), cnames)
  to_unlist <- intersect(c("committee_codes", "subcommittee_codes"), cnames)
  to_nested_df <- intersect(c("versions", "actions", "votes"), cnames)

  df[to_date] <- lapply(df[to_date], as.Date)

  df[to_df] <- lapply(df[to_df], function(x) {
    lapply(x, tibble::as_tibble)
  })

  df[to_unlist] <- lapply(df[to_unlist], function(x) {
    lapply(x, unlist)
  })

  df[to_nested_df] <- lapply(df[to_nested_df], function(x) {
    lapply(x, function(y) {
      z <- lapply(y, function(yi) tibble::as_tibble(replace_nulls(yi)))
      do.call(rbind, z)
    })
  })

  df
}

#' @rdname as_tibble.ppbills_search
#' @export
as_tibble.ppbill_amendments <- function(x, ...) {
  # convert list to data frame
  df <- transpose_to_df(x$amendments)

  # fix column types
  cnames <- colnames(df)
  to_date <- intersect(c("introduced_date", "latest_major_action_date"), cnames)

  df[to_date] <- lapply(df[to_date], as.Date)

  df
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
  cnames <- colnames(df)
  to_integer <- intersect(c("congress"), cnames)
  to_date <- intersect(c("introduced_date", "latest_major_action_date",
                         "house_passage_vote", "senate_passage_vote"), cnames)

  df[to_integer] <- lapply(df[to_integer], as.integer)
  df[to_date] <- lapply(df[to_date], as.Date)
  df
}
