#' Search Bills
#'
#' Use this request to search the title and full text of legislation by keyword
#' to get the 20 most recent bills. Searches cover House and Senate bills from
#' the 113th Congress through the current Congress (116th). If multiple words
#' are given (e.g. query=health care) the search is treated as multiple keywords
#' using the OR operator. Quoting the words (e.g. query="health care") makes
#' it a phrase search. Search results can be sorted by date (the default) or by
#' relevance, and in ascending or descending order.
#'
#' @param query search text
#' @param sort sort by lastest major action date or relevance score
#' @param dir sort in descending or ascending order
#' @param page page of results
#' @export
#'
#' @examples
#' \dontrun{
#' bills_search("health care")
#' }
#'
#' @references \url{https://projects.propublica.org/api-docs/congress-api/bills/#search-bills}
bills_search <- function(query, sort = c("date", "_score"),
                         dir = c("desc", "asc"), page = 1L) {
  sort <- match.arg(sort)
  dir <- match.arg(dir)
  path <- "bills/search.json"
  params <- list(
    query = query,
    sort = sort,
    dir = dir,
    offset = offset_from_page(page)
  )

  # get results
  results <- congress_api(path, params, class = "bills_search")
  results
}

#' Get Recent Bills
#'
#' Use this request type to get summaries of the 20 most recent bills by type.
#' For the current Congress, “recent bills” can be one of four types
#' (see the descriptions below). For previous Congresses, “recent bills” means
#' the last 20 bills of that Congress. In the responses, an \code{active} value of true
#' means that the bill has seen action beyond introduction and committee
#' referral. Requests include a \code{chamber} value; to get recent bills from the
#' House and Senate, use both as the value. You can paginate through bills using
#' the \code{offset} querystring parameter that accepts multiples of 20. Bills before
#' the 113th Congress (prior to 2013) have fewer attribute values than those
#' from the 113th Congress onward, because the more recent bill data comes from
#' the bulk data provided by the Government Publishing Office. Details for the
#' older bills came from scraping Thomas.gov, the former congressional site of
#' the Library of Congress.
#'
#' @param chamber house or senate
#' @param congress 105-116
#' @param type introduced, updated, active, passed, enacted, or vetoed
#' @param page page of results
#'
#' @examples
#' \dontrun{
#' bills_recent("house", 116, "enacted")
#' }
#'
#' @references \url{https://projects.propublica.org/api-docs/congress-api/bills/#search-bills}
bills_recent <- function(chamber = c("house", "senate"),
                         congress = 116,
                         type = c("introduced", "updated", "active", "passed", "enacted", "vetoed"),
                         page = 1L) {

  chamber <- match.arg(chamber)
  type <- match.arg(type)

  path <- sprintf("%s/%s/bills/%s.json", congress, chamber, type)
  params <- list(
    offset = offset_from_page(page)
  )

  # get results
  results <- congress_api(path, params, class = "bills_recent")
  results
}

#' Recent Bills by a Specific Subject
#'
#' Use this request type to get the 20 most recently updated bills for a specific
#' legislative subject. Results can include more than one Congress.
#'
#' @param subject A slug version of a legislative subject
#' @param page page of results
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' bills_recent_subject("meat")
#' }
#'
#' @references \url{https://projects.propublica.org/api-docs/congress-api/bills/#get-recent-bills-by-a-specific-subject}
bills_recent_subject <- function(subject, page = 1L) {
  path <- sprintf("bills/subjects/%s.json", subject)
  #params <- list(offset = offset_from_page(page))

  # get results
  results <- congress_api(path, class = "bills_recent_subject", extract = FALSE)
  results
}

#' Search a Specific Bill Subject
#'
#' Use this request type to search for bill subjects that contain a specified
#' term.
#'
#' @param query a word or phrase to search
#' @param page page of results
#' @export
#'
#' @examples
#' \dontrun{
#' bills_search_subjects("climate")
#' }
#'
#' @references \url{https://projects.propublica.org/api-docs/congress-api/bills/#get-a-specific-bill-subject}
bills_search_subjects <- function(query, page = 1L) {
  path <- "bills/subjects/search.json"
  params <- list(
    query = query,
    offset = offset_from_page(page)
  )

  # get results
  results <- congress_api(path, params, class = "bills_search_subjects")
  results
}

#' Get Upcoming Bills
#'
#' Use this request type to get details on bills that may be considered by the
#' House or Senate in the near future, based on scheduled published or announced
#' by congressional leadership. The bills and their potential consideration are
#' taken from the \href{http://docs.house.gov/floor/}{House Majority Leader} and
#' floor updates from
#' \href{http://www.republicans.senate.gov/public/index.cfm/floor-updates}{Senate Republicans}.
#' The responses include a \code{legislative_day} attribute which is the earliest the
#' bills could be considered, and a \code{range} attribute that indicates whether the
#' bill information comes from a weekly schedule or a daily one. Combine the two
#' for the best sense of when a bill might come up for consideration. For Senate
#' bills, the response includes a \code{context} attribute reproducing the sentence that
#' includes mention of the bill. These responses omit bills that have not yet been
#' assigned a bill number or introduced, and additional bills may be considered
#' at any time.
#'
#' @param chamber house or senate
#' @export
#'
#' @examples
#' \dontrun{
#' bills_upcoming("house")
#' }
#'
#' @references \url{https://projects.propublica.org/api-docs/congress-api/bills/#get-upcoming-bills}
bills_upcoming <- function(chamber = c("house", "senate")) {
  chamber <- match.arg(chamber)
  path <- sprintf("bills/upcoming/%s.json", chamber)

  # get results
  results <- congress_api(path, class = "bills_upcoming")
  results
}

#' Get a Specific Bill
#'
#' Use this request type to get details about a particular bill, including
#' actions taken and votes. The attributes \code{house_passage_vote} and
#' \code{senate_passage_vote} are populated (with the date of passage) only upon
#' successful passage of the bill. Bills before the 113th Congress (prior to 2013)
#' have fewer attribute values than those from the 113th Congress onward,
#' because the more recent bill data comes from the bulk data provided by the
#' Government Publishing Office. Details for the older bills came from scraping
#' Thomas.gov, the former congressional site of the Library of Congress.
#'
#' @param bill_id a bill slug, for example hr4881 - these can be found in the recent bill response.
#' @param congress 105-116
#' @export
#'
#' @examples
#' \dontrun{
#' bill("hr502", 116)
#' }
#'
#' @references \url{https://projects.propublica.org/api-docs/congress-api/bills/#get-a-specific-bill}
bill <- function(bill_id, congress) {
  path <- sprintf("%s/bills/%s.json", congress, bill_id)

  # get results
  results <- congress_api(path)
  results <- set_ppclass(results, "bill")
  results <- extract_single_result(results)
  results
}

