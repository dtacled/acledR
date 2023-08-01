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

