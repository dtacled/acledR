if(identical(Sys.getenv("NOT_CRAN"), "true")) {

test <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                  country = "Argentina", start_date="2022-01-01",end_date = "2022-12-31",
                  prompt = F, acled_access = F, log = F, inter_numeric = TRUE)

test_changes <- test %>%
                left_join(acledR::acled_interaction_codes, by = c("inter1" = "Numeric Code")) %>%
                select(-inter1) %>%
                rename(inter1 = "Inter1/Inter2") %>%
                relocate(inter1, .after = assoc_actor_1) %>%
                left_join(acledR::acled_interaction_codes, by = c("inter2" = "Numeric Code")) %>%
                select(-inter2) %>%
                rename(inter2 = "Inter1/Inter2") %>%
                relocate(inter2, .after = assoc_actor_2) %>%
                mutate(interaction = case_when(
                  str_detect(interaction, "10") ~ "Sole State Forces",
                  str_detect(interaction, "20") ~ "Sole Rebel Groups",
                  str_detect(interaction, "30") ~ "Sole Political Militias",
                  str_detect(interaction, "40") ~ "Sole Identity Militias",
                  str_detect(interaction, "50") ~ "Sole Rioters",
                  str_detect(interaction, "60") ~ "Sole Protesters",
                  str_detect(interaction, "70") ~ "Sole Civilians",
                  str_detect(interaction, "80") ~ "Sole Others",
                  TRUE ~ as.character(interaction))) %>%
                mutate(interaction = str_replace_all(interaction, "(\\d)(\\d)", "\\1-\\2"),
                       interaction = str_replace(as.character(interaction), "1", "State Forces"),
                       interaction = str_replace(as.character(interaction), "2", "Rebel Groups"),
                       interaction = str_replace(as.character(interaction), "3", "Political Militias"),
                       interaction = str_replace(as.character(interaction), "4", "Identity Militias"),
                       interaction = str_replace(as.character(interaction), "5", "Rioters"),
                       interaction = str_replace(as.character(interaction), "6", "Protesters"),
                       interaction = str_replace(as.character(interaction), "7", "Civilians"),
                       interaction = str_replace(as.character(interaction), "8", "External/Other Forces"))
}
