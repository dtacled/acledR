#' @title Regenerating ACLED's deleted columns
#' @name acled_regen
#' @description This formula allows to regenerate columns (when applicable) that may have been deleted from the dataset.
#' @param df data frame. ACLED's data frame where the regenerated column will be created
#' @param column character string. Name of the column to regenerate. See details for info on available columns
#' @note Please note that as of today, the one columns available to be regenerated are
#' - event_id_no_cnty
#' @returns Tibble with the regenerated column addedd at the end.
#' @md
#' @import stringr
#' @import dplyr


acled_regen <- function(df,column) {

  if(is.null(df)){
    stop("Please indicate a dataframe where to add the column")
  }

  if(column == "event_id_no_cnty"){
    if("event_id_cnty" %in% colnames(df)){
    df$acled_regen <- dplyr::mutate(event_id_no_cnty = stringr::str_extract(event_id_cnty, '\\d+'))}
    else {"To regenerate the event_id_no_cnty column, the referenced data frame must include the event_id_cnty column."}
  } else if(is.null(column)){
    stop("Please indicate a column in the column argument, make sure it is as string and without any typos")}
}
