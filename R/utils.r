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
