% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api-bills.r
\name{bill}
\alias{bill}
\title{Get a Specific Bill}
\usage{
bill(bill_id, congress = 116L)
}
\arguments{
\item{bill_id}{a bill slug, for example hr4881 - these can be found in the recent bill response.}

\item{congress}{105-116}
}
\description{
Use this request type to get details about a particular bill, including
actions taken and votes. The attributes \code{house_passage_vote} and
\code{senate_passage_vote} are populated (with the date of passage) only upon
successful passage of the bill. Bills before the 113th Congress (prior to 2013)
have fewer attribute values than those from the 113th Congress onward,
because the more recent bill data comes from the bulk data provided by the
Government Publishing Office. Details for the older bills came from scraping
Thomas.gov, the former congressional site of the Library of Congress.
}
\examples{
\dontrun{
bill("hr502", 116)
bill("s1436", 116)
}

}
\references{
\url{https://projects.propublica.org/api-docs/congress-api/bills/#get-a-specific-bill}
}
