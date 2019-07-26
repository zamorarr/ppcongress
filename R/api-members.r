#' Lists of Members
#'
#' Get a list of members of a particular chamber in a particular Congress.
#' The results include all members who have served in that congress and chamber,
#'  including members who are no longer in office. To filter the list to only
#'  active members (or to see members who have left), use the in_office boolean
#'  attribute.
#'
#' @param chamber house or senate
#' @param congress 102-116 for House, 80-116 for Senate
#' @export
#'
#' @examples
#' \dontrun{
#' members("house", 116)
#' }
#'
#' @references \url{https://projects.propublica.org/api-docs/congress-api/members/#lists-of-members}
members <- function(chamber = c("house", "senate"), congress = 116) {
  chamber <- match.arg(chamber)
  path <- sprintf("%s/%s/members.json", congress, chamber)
  results <- congress_api(path)

  results <- set_ppclass(results, "members")
  results <- extract_single_result(results)
  results
}

#' Get a Specific Member
#'
#' Get biographical and Congressional role information for a particular member
#' of Congress
#'
#' @param member_id The ID of the member to retrieve; it is assigned by the \href{http://bioguide.congress.gov/biosearch/biosearch.asp}{Biographical Directory of the United States Congress} or can be retrieved from a member list request.
#' @export
#'
#' @examples
#' \dontrun{
#' member("O000172")
#' }
#'
#' @references \url{https://projects.propublica.org/api-docs/congress-api/members/#get-a-specific-member}
member <- function(member_id) {
  stopifnot(length(member_id) == 1L)
  path <- sprintf("members/%s.json", member_id)
  results <- congress_api(path)

  results <- set_ppclass(results, "member")
  results <- extract_single_result(results)
  results
}

#' Get new members
#'
#' Get a list of the 20 most recent new members of the current Congress.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' members_new()
#' }
#'
#' @references \url{https://projects.propublica.org/api-docs/congress-api/members/#get-new-members}
members_new <- function() {
  path <- "members/new.json"
  results <- congress_api(path)

  results <- set_ppclass(results, "members_new")
  results <- extract_single_result(results)
  results <- normalize_offset(results)

  # return value
  results
}

#' Get Current Members by State/District
#'
#' Get biographical and Congressional role information for a particular member of Congress
#' @param state two-letter state abbreviation (ex. "TX")
#' @param district district number (ex. 1)
#' @param chamber house or senate
#' @export
#'
#' @examples
#' \dontrun{
#' members_location("senate", "TX")
#' }
#'
#' @references \url{https://projects.propublica.org/api-docs/congress-api/members/#get-current-members-by-statedistrict}
members_location <- function(chamber = c("house", "senate"), state = NULL, district = NULL) {
  chamber <- match.arg(chamber)

  # check arguments
  if (is.null(state)) {
    stop("state cannot be null", call. = FALSE)
  }

  if (identical(chamber, "house") && is.null(district)) {
    stop("district cannot be null when chamber is house", call. = FALSE)
  }

  if (identical(chamber, "senate") && !is.null(district)) {
    stop("district must be null when chamber is sentate", call. = FALSE)
  }

  # build path
  if (chamber == "house") {
    path <- sprintf("members/%s/%s/%s/current.json", chamber, state, district)
  } else {
    path <- sprintf("members/%s/%s/current.json", chamber, state)
  }

  # get results
  results <- congress_api(path)
  results <- set_ppclass(results, "members_location")
  results
}

#' Get Members Leaving Office
#'
#'  get a list of members who have left the Senate or House or have announced plans to do so.
#'
#' @param chamber house or senate
#' @param congress 102-116 for House, 80-116 for Senate
#' @param page page of results
#'
#' @export
#'
#' @examples
#' \dontrun{
#' members_leaving("senate", 116)
#' }
#'
#' @references \url{https://projects.propublica.org/api-docs/congress-api/members/#get-members-leaving-office}
members_leaving <- function(chamber = c("house", "senate"), congress = 116, page = 1L) {
  chamber <- match.arg(chamber)
  path <- sprintf("%s/%s/members/leaving.json", congress, chamber)
  query <- list(offset = offset_from_page(page))
  results <- congress_api(path, query)

  # get results
  results <- set_ppclass(results, "members_leaving")
  results <- extract_single_result(results)
  results
}

#' Get a Specific Member’s Vote Positions
#'
#' Get the most recent vote positions for a specific member of the House of Representatives or Senate
#'
#' @param member_id The ID of the member to retrieve; it is assigned by the \href{http://bioguide.congress.gov/biosearch/biosearch.asp}{Biographical Directory of the United States Congress} or can be retrieved from a member list request.
#' @param page page of results
#' @export
#'
#' @examples
#' \dontrun{
#' member_votes("O000172")
#' }
#'
#' @references \url{https://projects.propublica.org/api-docs/congress-api/members/#get-a-specific-members-vote-positions}
member_votes <- function(member_id, page = 1L) {
  stopifnot(length(member_id) == 1L)
  path <- sprintf("members/%s/votes.json", member_id)
  query <- list(offset = offset_from_page(page))
  results <- congress_api(path, query)

  # get results
  results <- set_ppclass(results, "member_votes")
  results <- extract_single_result(results)
  results <- normalize_offset(results)
  results
}

#' Get Bills Cosponsored by a Specific Member
#'
#' Get the 20 most recent bill cosponsorships for a particular member, either
#' bills cosponsored or bills where cosponsorship was withdrawn
#' @param member_id The ID of the member to retrieve; it is assigned by the \href{http://bioguide.congress.gov/biosearch/biosearch.asp}{Biographical Directory of the United States Congress} or can be retrieved from a member list request.
#' @param type cosponsored or withdrawn
#' @param page page of results
#' @export
#'
#' @examples
#' \dontrun{
#' member_bills("O000172")
#' }
#'
#' @references \url{https://projects.propublica.org/api-docs/congress-api/members/#get-bills-cosponsored-by-a-specific-member}
member_bills <- function(member_id, type = c("cosponsored", "withdrawn"), page = 1L) {
  stopifnot(length(member_id) == 1L)
  type <- match.arg(type)
  path <- sprintf("members/%s/bills/%s.json", member_id, type)
  query <- list(offset = offset_from_page(page))
  results <- congress_api(path, query)

  # get results
  results <- set_ppclass(results, "member_bills")
  results <- extract_single_result(results)
  results <- normalize_offset(results)

  # return results
  results
}
