#' csv_output_max
#'
#' @param input dataframe of flagged GMAP data
#' @param newname name of the campaign or specific user provided name to identify the final files
#' @param onoff are the data collected onsite or offsite (inside vs outside the fenceline)
#' @param trantype The transect type either "MA" mapping, or "ST" stationary
#'
#' @return
#' @export
#'
#' @examples
csv_output_max <- function(input, newname = " ", onoff = " ", trantype = " ") {
  suppressWarnings(dir.create(file.path(getwd(), paste0(newname, "_max_flag"))))
  if (onoff == "off") {
    if (trantype == "MA") {
      write.csv(as.data.frame(input),
                file = file.path(getwd(), paste0(newname, "_max_flag"),
                                 paste(newname, "_offsite_mapping_max.csv",
                                       sep = "")))
    }
    if (trantype == "ST") {
      write.csv(as.data.frame(input),
                file = file.path(getwd(), paste0(newname, "_max_flag"),
                                 paste(newname, "_offsite_stationary_max.csv",
                                       sep = "")))
    }
    if (trantype == "CA") {
      write.csv(as.data.frame(input),
                file = file.path(getwd(), paste0(newname, "_max_flag"),
                                 paste(newname, "_offsite_calibration_max.csv",
                                       sep = "")))
    }
  }
  if (onoff == "on") {
    if (trantype == "MA") {
      write.csv(as.data.frame(input),
                file = file.path(getwd(), paste0(newname, "_max_flag"),
                                 paste(newname, "_onsite_mapping_max.csv",
                                       sep = "")))
    }
    if (trantype == "ST") {
      write.csv(as.data.frame(input),
                file = file.path(getwd(), paste0(newname, "_max_flag"),
                                 paste(newname, "_onsite_stationary_max.csv",
                                       sep = "")))
    }
    if (trantype == "CA") {
      write.csv(as.data.frame(input),
                file = file.path(getwd(), paste0(newname, "_max_flag"),
                                 paste(newname, "_onsite_calibration_max.csv",
                                       sep = "")))
    }
  }
}
