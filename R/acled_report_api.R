## Automatic RO script



## Two options:
# 1: Load the RO through a new dataset. (template2)
# 2: Load the RO through an existing. (template1)

acled_country_report_api <- function(country_report = NULL,
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

    # data_file <- acled_api(countries=country_report, start_date = start_date_report,
    #                        end_date = end_date_report,
    #                        email = email, key = key, acled_access=FALSE,
    #                        prompt=prompt)




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

library(acledR)
acled_access(email = "acledexamples@gmail.com", key = "M3PWwg3DIdhHMuDiilp5") #  This is an example, you will need to input your
acled_country_report_api(country_report = "Argentina", start_date_report= "2019-01-01", acled_access = TRUE, prompt = FALSE)
