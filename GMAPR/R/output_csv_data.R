#' output_csv_data
#'
#' @param x the final flagged GMAP data frames
#' @param newname name of the campaign or specific user provided name to identify the final files
#' @param onoff are the data collected onsite or offsite (inside vs outside the fenceline)
#' @param trantype The transect type either "MA" mapping, or "ST" stationary
#'
#' @return returns a csv in the active working directory of the final flagged results
#' @export
#'
#' @examples
csv_output_data <- function(x, newname = " ", onoff = " ", trantype = " ") {
  suppressWarnings(dir.create(file.path(getwd(), paste0(newname, "_flag"))))
  input <- x
  if (onoff == "off") {
    if (trantype == "MA") {
      write.csv(as.data.frame(input),
                  file = file.path(getwd(), paste0(newname, "_flag"),
                                   paste(newname, "_offsite_mapping.csv",
                                         sep = "")))
      }
      if (trantype == "ST") {
        write.csv(as.data.frame(input),
                  file = file.path(getwd(), paste0(newname, "_flag"),
                                   paste(newname, "_offsite_stationary.csv",
                                         sep = "")))
      }
      if (trantype == "CA") {
        write.csv(as.data.frame(input),
                  file = file.path(getwd(), paste0(newname, "_flag"),
                                   paste(newname, "_offsite_calibration.csv",
                                         sep = "")))
      }
    }
    if (onoff == "on") {
      if (trantype == "MA") {
        write.csv(as.data.frame(input),
                  file = file.path(getwd(), paste0(newname, "_flag"),
                                   paste(newname, "_onsite_mapping.csv",
                                         sep = "")))
      }
      if (trantype == "ST") {
        write.csv(as.data.frame(input),
                  file = file.path(getwd(), paste0(newname, "_flag"),
                                   paste(newname, "_onsite_stationary.csv",
                                         sep = "")))
      }
      if (trantype == "CA") {
        write.csv(as.data.frame(input),
                  file = file.path(getwd(), paste0(newname, "_flag"),
                                   paste(newname, "_onsite_calibration.csv",
                                         sep = "")))
    }
  }
}

