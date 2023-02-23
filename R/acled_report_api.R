#' @title Automatic country report with API
#' @name acled_report_api
#' @description Generate automatic exploratory reports of ACLED data by utilizing data directly from ACLED's API
#' @param country_report character string. A country from which to base the report.
#' @param regional_data boolean. True or False option to include (when TRUE) regional comparision in the report. Requires larger API calls.
#' @param region character string. A region which from which to base the report. It still requires a country.
#' @param start_date_report character string. The initial date (YYYY-MM-DD) from which to generate the report.
#' @param end_date_report character string. Last date (YYYY-MM-DD) from which to finish the report.
#' @param output_style character string. Desired format for the markdown file, recommended HTML or PDF. Defaults to HTML.
#' @param email character string. Email associated with your ACLED account registered at <https://developer.acleddata.com>.
#' @param key character string. Access key associated with your ACLED account registered at <https://developer.acleddata.com>.
#' @param acled_access logical. If TRUE (default), you have used the acled_access function and the email and key arguments are not required.
#' @param prompt logical. If TRUE (default), users will receive an interactive prompt providing information about their call (countries requested, number of country-days, and number of API calls required) and asking if they want to proceed with the call. If FALSE, the call continues without warning, but the call is split and returns a message specifying how many calls are being made.
#' @return Returns a markdown file stored in the project's current working directory.
#' @family Data Analysis
#' @examples
#' \dontrun{
#'
#' ## Generate report regarding Argentina's events throughout August-December 2022
#' acled_country_report_api(country_report = "Argentina", start_date_report = "2022-07-31",
#'                          end_date_report = "2023-01-01",
#'                          output_style = "HTML", prompt=FALSE)
#' }
#' @md

acled_report_api <- function(country_report = NULL,regional_data = TRUE, region = NULL,
                             start_date_report = "1997-01-01", end_date_report = Sys.Date(),
                             output_style = "HTML", email = NULL, key = NULL, acled_access = TRUE, prompt=TRUE){

  if(acled_access == TRUE){
    email <- Sys.getenv("acled_email")
    key <- Sys.getenv("acled_key")
  }

  if(is.null(country_report)){
    stop("No country provided, please provide a country to base the report on.")
  }

  if(is.null(start_date_report)){
    warning("Because no start_date_report was provided, the report will used the minimum date in the dataset.")
  }

  if(output_style == "HTML" | output_style == "html"){
    output = "html_document"
    } else if(output_style=="pdf"|output_style=="PDF"){
      output = "pdf_document"
    }

  if(regional_data == TRUE){
    call <- acled_api(regions=region, start_date = start_date_report,
                           end_date = end_date_report,
                           email = email, key = key, acled_access=FALSE,
                           prompt=prompt)
    data_file <- call %>%
      filter(.data$country == country_report)


    }else{
    data_file <- acled_api(countries=country_report, start_date = start_date_report,
                         end_date = end_date_report,
                         email = email, key = key, acled_access=FALSE,
                         prompt=prompt)
    }


  if(regional_data != TRUE){
    rmarkdown::render(input = "inst/rmarkdown/templates/template1.Rmd",
                    params = list(
                      country = as.character(country_report),
                      start_date = lubridate::ymd(start_date_report),
                      end_date = lubridate::ymd(end_date_report),
                      acled_email = email,
                      acled_key = key,
                      data = data_file),
                    output_format = output,
                    envir = new.env())
  }else{
    rmarkdown::render(input = "inst/rmarkdown/templates/template2.Rmd",
                      params = list(
                        country = as.character(country_report),
                        start_date = lubridate::ymd(start_date_report),
                        end_date = lubridate::ymd(end_date_report),
                        acled_email = email,
                        acled_key = key,
                        data = data_file,
                        region = region,
                        regional_data = call),
                      output_format = output,
                      envir = new.env())
    }
}
