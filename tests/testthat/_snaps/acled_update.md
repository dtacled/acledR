# acled_update without deletions actually returns events that should be deleted

    Code
      acled_update(acledR::acled_old_dummy, email = "acledexamples@gmail.com", key = "M3PWwg3DIdhHMuDiilp5",
      deleted = FALSE, acled_access = F, prompts = F)
    Message <simpleMessage>
      Requesting data for 1 countries. Accounting for the requested time period and ACLED coverage dates, this request includes 29 country-days.
      Processing API request
      Extracting content from API request
      Dataset updated. 
       Old number of events: 326. 
       New events: 1. 
       Total new & modified events: 46
    Output
      # A tibble: 327 x 31
         event_id_cnty event_date  year time_precision disorder_type event_type
         <chr>         <date>     <dbl>          <dbl> <chr>         <chr>     
       1 ARG10607      2022-06-30  2022              1 Demostrations Protests  
       2 ARG10626      2022-06-30  2022              1 Demostrations Protests  
       3 ARG10618      2022-06-30  2022              1 Demostrations Protests  
       4 ARG10615      2022-06-30  2022              1 Demostrations Protests  
       5 ARG10627      2022-06-30  2022              1 Demostrations Protests  
       6 ARG10625      2022-06-30  2022              1 Demostrations Protests  
       7 ARG10621      2022-06-30  2022              1 Demostrations Protests  
       8 ARG10613      2022-06-30  2022              1 Demostrations Protests  
       9 ARG10614      2022-06-30  2022              1 Demostrations Protests  
      10 ARG10617      2022-06-30  2022              1 Demostrations Protests  
      # i 317 more rows
      # i 25 more variables: sub_event_type <chr>, actor1 <chr>, assoc_actor_1 <chr>,
      #   inter1 <dbl>, actor2 <chr>, assoc_actor_2 <chr>, inter2 <dbl>,
      #   interaction <dbl>, civilian_targeting <chr>, iso <dbl>, region <chr>,
      #   country <chr>, admin1 <chr>, admin2 <chr>, admin3 <lgl>, location <chr>,
      #   latitude <dbl>, longitude <dbl>, geo_precision <dbl>, source <chr>,
      #   source_scale <chr>, notes <chr>, fatalities <dbl>, tags <chr>, ...

