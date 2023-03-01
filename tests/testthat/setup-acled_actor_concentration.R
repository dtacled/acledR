# Setup for test-acled_actor_concentration

received_data <- acled_api(email = "acledexamples@gmail.com", key = "M3PWwg3DIdhHMuDiilp5",countries = "Argentina", start_date="2022-01-01",end_date = "2022-12-31",prompt = F, acled_access = F, log = F)

received_data_ac <- received_data %>%
  acled_transform() %>%
  group_by(actor) %>%
  summarise(event_number=n())
