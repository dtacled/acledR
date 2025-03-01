% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acled_deletions_api.R
\name{acled_deletions_api}
\alias{acled_deletions_api}
\title{Request data from the ACLED Deletions API}
\usage{
acled_deletions_api(
  email = NULL,
  key = NULL,
  date_deleted = NULL,
  acled_access = TRUE,
  log = FALSE
)
}
\arguments{
\item{email}{character string. Email associated with your ACLED account registered at \url{https://developer.acleddata.com}.}

\item{key}{character string. Access key associated with your ACLED account registered at \url{https://developer.acleddata.com}.}

\item{date_deleted}{character string. Format 'yyyy-mm-dd' or Unix timestamp. The query will return all deleted events including and after the requested date/timestamp.}

\item{acled_access}{logical. If TRUE it means that you have utilized the acled_access function and there is no need for the email and key arguments.}

\item{log}{Only for testing purposes: you can use this to check if all the variables in your call were handled properly.}
}
\value{
Returns a tibble of ACLED data with columns for event_id_cnty and deleted_timestamp.
}
\description{
This function allows users to pull deleted ACLED event IDs from the Deletions API.
}
\examples{
\dontrun{

# Request deleted ACLED events since January 1, 2022
acled_deletions_api(date_deleted = "2022-01-01", acled_acess = TRUE)
}
}
\seealso{
\itemize{
\item \href{https://acleddata.com/download/35306/}{ACLED API guide}
\item \href{https://acleddata.com/download/35179/}{Keeping ACLED data up to date guide}
}

Other API and Access: 
\code{\link{acled_access}()},
\code{\link{acled_api}()},
\code{\link{acled_update}()}
}
\concept{API and Access}
