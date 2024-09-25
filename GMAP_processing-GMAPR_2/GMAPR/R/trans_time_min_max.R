#min and max Time from each Transect

#' Maximum and minimum time for each transect from a GMAP campaign
#'
#' Can be used to identify beginning and ending times fro each transect. Would be useful in conjunction with time
#' flagging if specific flags need to be applied to entire transects.
#'
#' @param input Dataframe output from either analyte flagging or time flagging
#'
#' @return Returns a dataframe with the name of each transect and the beginning and ending times
#' @export
#'
#' @examples
tran_time_min_max <- function(input) {
  output <- input %>%
    group_by(Transect) %>%
    summarise(min = min(Time),
              max = max(Time))
  return(output)

}
