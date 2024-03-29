% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api-bills.r
\name{bills_search}
\alias{bills_search}
\title{Search Bills}
\usage{
bills_search(query, sort = c("date", "_score"), dir = c("desc", "asc"),
  page = 1L)
}
\arguments{
\item{query}{search text}

\item{sort}{sort by lastest major action date or relevance score}

\item{dir}{sort in descending or ascending order}

\item{page}{page of results}
}
\description{
Use this request to search the title and full text of legislation by keyword
to get the 20 most recent bills. Searches cover House and Senate bills from
the 113th Congress through the current Congress (116th). If multiple words
are given (e.g. query=health care) the search is treated as multiple keywords
using the OR operator. Quoting the words (e.g. query="health care") makes
it a phrase search. Search results can be sorted by date (the default) or by
relevance, and in ascending or descending order.
}
\examples{
\dontrun{
bills_search("health care")
}

}
\references{
\url{https://projects.propublica.org/api-docs/congress-api/bills/#search-bills}
}
