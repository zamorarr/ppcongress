#' Replace nulls in a list with NAs
#' @param x a list
replace_nulls <- function(x) {
  # get type of non-null values
  values <- purrr::keep(x, ~ !(is.null(.x) | is_empty(.x)))

  if (length(values) > 0) {
    type <- class(unlist(values))
  } else {
    type <- "logical"
  }

  if (type == "list") stop("cannot replace nulls when values are a list", call. = FALSE)
  na <- switch(
    type,
    "logical" = NA,
    "integer" = NA_integer_,
    "double" = NA_real_,
    "numeric" = NA_real_,
    "complex" = NA_complex_,
    "character" = NA_character_)

  # convert NULLs to the right NA
  purrr::modify_if(x, function(.x) is.null(.x) | is_empty(.x), ~ na)
}

#' Extract a single result from a json list
#'
#' This is useful when we know that the resulting json
#' will contain exactly one result
#'
#' @param x a list
extract_single_result <- function(x) {
  attrs <- attributes(x)
  new_x <- x[[1]]
  attributes(new_x) <- c(attributes(new_x), attrs)
  new_x
}

#' Checks if object is empty
#' @param x a vector or list
is_empty <- function(x) {
  is.vector(x) && length(x) == 0L
}

#' Converts offset to integer
#'
#' @param x a result json
normalize_offset <- function(x) {
  cnames <- names(x)
  to_integer <- intersect(c("num_results", "offset"), cnames)
  x[to_integer] <- lapply(x[to_integer], as.integer)

  x
}

offset_from_page <- function(page) {
  20L*(page - 1L)
}

#' Convert list of lists to data frame
#'
#' @param x a list of lists
transpose_to_df <- function(x) {
  y <- purrr::transpose(x)
  y <- purrr::map(y, replace_nulls)
  y <- purrr::simplify_all(y)
  tibble::as_tibble(y)
}

normalize_time <- function(x) {
  dttm <- gsub(":([0-9]{2})$", "\\1", x)
  dttm <- gsub("T", " ", dttm)
  dttm <- gsub(" ", "", dttm)

  has_tz <- any(grepl("[0-9]{4}$", dttm))
  if (has_tz) {
    as.POSIXct(dttm, tz = "America/New_York", format = "%Y-%m-%d%H:%M:%S%z")
  } else {
    as.POSIXct(dttm, tz = "America/New_York", format = "%Y-%m-%d%H:%M:%S")
  }
}

list_to_df <- function(x) {
  tibble::as_tibble(replace_nulls(x))
}

fix_column_types <- function(df) {
  # existing column names
  cnames <- colnames(df)

  # specify columns to convert
  to_logical <- intersect(c(
    "has_bills", "has_statements", "at_large"), cnames)
  to_integer <- intersect(c(
    "congress", "seniority", "next_election", "senate_class", "session",
    "roll_call", "cosponsors"), cnames)
  to_date <- intersect(c(
    "introduced_date", "last_vote", "house_passage", "senate_passage",
    "enacted", "vetoed", "latest_major_action_date", "legislative_day",
    "house_passage_vote", "senate_passage_vote", "date",
    "date_of_birth", "begin_date", "end_date", "most_recent_vote",
    "start_date", "cosponsored_date"), cnames)
  to_dttm <- intersect(c(
    "scheduled_at", "last_updated", "datetime"), cnames)
  to_hms <- intersect(c(
    "time"), cnames)
  to_character <- intersect(c(
    "gpo_pdf_uri", "context", "middle_name", "suffix"), cnames)
  to_df <- intersect(c(
    "cosponsors_by_party", "bill", "total", "amendment"), cnames)
  to_unlist <- intersect(c(
    "committee_codes", "subcommittee_codes"), cnames)
  to_nested_df <- intersect(c(
    "versions", "actions", "votes"), cnames)

  # to dttm last_updated is wrong from members()
  # "2019-07-31 11:00:23 -0400"
  # what is scheduled_at? bills_upcoming

  # convert columns
  df[to_logical] <- lapply(df[to_logical], as.logical)
  df[to_integer] <- lapply(df[to_integer], as.integer)
  df[to_date] <- lapply(df[to_date], as.Date)
  df[to_character] <- lapply(df[to_character], as.character)
  df[to_hms] <- lapply(df[to_hms], hms::as.hms)

  #df[to_dttm] <- lapply(df[to_dttm], normalize_time)
  df[to_dttm] <- lapply(df[to_dttm], lubridate::as_datetime, tz = "America/New_York")

  df[to_df] <- lapply(df[to_df], function(x) {
    lapply(x, list_to_df)
  })

  df[to_unlist] <- lapply(df[to_unlist], function(x) {
    lapply(x, unlist)
  })

  df[to_nested_df] <- lapply(df[to_nested_df], function(x) {
    lapply(x, function(y) {
      z <- lapply(y, list_to_df)
      do.call(rbind, z)
    })
  })

  df
}
