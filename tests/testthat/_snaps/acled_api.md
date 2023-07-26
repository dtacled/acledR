# Users can stop a call if they need to

    Code
      acled_api(email = "acledexamples@gmail.com", key = "M3PWwg3DIdhHMuDiilp5",
        start_date = "2022-01-01", end_date = "2022-12-31", countries = "Argentina",
        prompt = T, acled_access = F, log = F)
    Message <simpleMessage>
      Requesting data for 1 countries. Accounting for the requested time period and ACLED coverage dates, this request includes 364 country-days.
      This request requires 1 API calls. Do you want to proceed with this request?
      If you need to increase your API quota, please contact access@acleddata.com
    Error <simpleError>
      User responded "No" when prompted about the number of API calls required. 
      If you need to increase your API quota, please contact access@acleddata.com

# Error when non existent event types

    Code
      acled_api(email = "acledexamples@gmail.com", key = "M3PWwg3DIdhHMuDiilp5",
        countries = "Argentina", start_date = "2021-01-01", end_date = "2022-01-01",
        event_types = c("Protests", "Superhero fight"), prompt = F, acled_access = F,
        log = F)
    Message <simpleMessage>
      Requesting data for 1 countries. Accounting for the requested time period and ACLED coverage dates, this request includes 365 country-days.
    Output
      [1] "Protests"        "Superhero Fight"
    Error <simpleError>
      One or more requested event types are not in the ACLED data. Event types include: Battles, Violence against civilians, Protests, Riots, Strategic Developments, and Explosions/Remote violence. Leave 'event_type = NULL' to request all event types from the API. 

