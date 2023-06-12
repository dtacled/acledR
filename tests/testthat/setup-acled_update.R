

countries_in_data <- unique(unlist(acledR::acled_old_dummy$country, use.names = F))

dupes_checks <- acled_update(acledR::acled_old_dummy,
                              countries = countries_in_data,
                             email = "acledexamples@gmail.com",
                             key = "M3PWwg3DIdhHMuDiilp5",
                             acled_access = F, prompts = F)

