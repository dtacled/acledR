# #test
#
# disorder_type <- c(
#   "Political violence",
#   "Demonstrations"
# )
#
#
# data_check <- df_br %>%
#   separate_rows(disorder_type, sep = "; ")
#
# if(sum(str_to_sentence(disorder_type) %in% unique(data_check[["disorder_type"]])) < length(disorder_type)){
#     stop("One or more requested disorder types not in data.")
# }
#
#
# test <- acled_generate_counts(
#   df_br,
#   unit_id = "country",
#   time_id = "event_date",
#   time_target = "month",
#   event_type = c("Protests", "Riots")
# )



# df <- data.frame(x=c("actor1", "actor2", "actor3"),
#                  y=c(9,0.5,0.5))
#
# acled_actor_concentration(df$y, method = "Concentration", acled_dataframe = F)
