


dupes_checks <- acled_update(acledR::acled_old_dummy,
                             email = "acledexamples@gmail.com",
                             key = "M3PWwg3DIdhHMuDiilp5",
                             acled_access = F, prompts = F)

dupes_checks_plus_bramex <- acled_update(acledR::acled_old_dummy,
                                         countries = c("Brazil","Mexico"),
                             email = "acledexamples@gmail.com",
                             key = "M3PWwg3DIdhHMuDiilp5",
                             acled_access = F, prompts = F)


test_more_than_one <- acled_update(acledR::acled_old_deletion_dummy,
                                   email = "acledexamples@gmail.com",
                                   key = "M3PWwg3DIdhHMuDiilp5",
                                   acled_access = F, prompts = F)
