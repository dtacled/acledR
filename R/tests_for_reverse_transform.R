# running tests

data <- schema

data1 <- schema %>%
  head(5)

reverse_data <- data1 %>%
  # mutate(actor = str_c(actor, collapse = ";")) %>%
  select(-inter_type) %>%
  pivot_wider(names_from = type_of_actor, values_from = c(actor, inter), values_fn = str_c, values_fill = "") %>%
  mutate(actor1 = str_trim(actor1),
         actor2 = str_trim(actor2),
         assoc_actor_1 = str_trim(assoc_actor_1),§
         assoc_actor_2 = str_trim(assoc_actor_2))
