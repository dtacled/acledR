## Automatic RO script without API


acled_country_report <- function(data, country_report = NULL,
                                     start_date_report = "1997-01-01", end_date_report = Sys.Date(),
                                     output_style = "HTML", email = NULL, key = NULL, acled_access = TRUE, prompt=TRUE){

  if(acled_access == TRUE){
    email <- Sys.getenv("acled_email")
    key <- Sys.getenv("acled_key")
  }

  if(is.null(country_report)){
    stop("No country provided, please provide a country to base the report on. ")
  }

  if(is.null(start_date_report)){
    warning("Because no start_date_report was provided, the report will used the minimum date in the dataset")
  }

  if(output_style == "HTML" | output_style == "html"){
    output = "html_document"
  } else if(output_style=="pdf"|output_style=="PDF"){
    output = "pdf_document"
  }

  data_file = data


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
}
