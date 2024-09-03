#' Time flagging:
#'
#' Flagging analytes over specific temporal subsets: May need to be run multiple times if there are multiple subsets of times a user wants to apply a selected flag to multiple temporal subsets.
#'
#' @param x The output of analyte_flagging.Mus be a dataframe
#' @param timestart The time at which to begin applying the desired flag must be in the form of "YYYY-mm-dd HH:MM:SS"
#' @param timestop The time at which to end applying the time flag must be in the form of "YYYY-mm-dd HH:MM:SS"
#' @param timeqt The specific flag that the user wants to apply across the subset of times
#' @param analyte Analytes which to apply the specific flag
#'
#' @return Returns a dataframe with flagged data
#' @export
#'
#' @examples
time_flagging <- function(x, timestart = " ", timestop = " ", timeqt = " ",
                          analyte = c("H2Sflag", "CH4flag", "BENflag",
                                     "TOLflag", "XYPflag")) {
  analyte_vec <- unique(analyte)
  print(analyte_vec)
  input <- x %>%
    filter(Time >= as.POSIXlt(timestart) & Time <= as.POSIXlt(timestop)) %>%
    mutate(timeqt = timeqt) %>%
    mutate(Transect = gsub(".*/", "", Transect)) %>%
    mutate(Transect = gsub("\\..*", "", Transect)) %>%
    group_by(Transect) %>%
    rowwise() %>%
    mutate(H2Sflag = ifelse(is.element("H2Sflag", analyte_vec),
                            paste(H2Sflag, timeqt, sep = " "), H2Sflag)) %>%
    mutate(CH4flag = ifelse(is.element("CH4flag", analyte_vec),
                            paste(CH4flag, timeqt, sep = " "), CH4flag)) %>%
    mutate(BENflag = ifelse(is.element("BENflag", analyte_vec),
                            paste(BENflag, timeqt, sep = " "), BENflag)) %>%
    mutate(TOLflag = ifelse(is.element("TOLflag", analyte_vec),
                            paste(TOLflag, timeqt, sep = " "), TOLflag)) %>%
    mutate(XYPflag = ifelse(is.element("XYPflag", analyte_vec),
                            paste(XYPflag, timeqt, sep = " "), XYPflag)) %>%
    mutate(wsflag = ifelse(is.element("wsflag", analyte_vec),
                           paste(wsflag, timeqt, sep = " "), wsflag)) %>%
    mutate(wdflag = ifelse(is.element("wdflag", analyte_vec),
                           paste(wdflag, timeqt, sep = " "), wdflag)) %>%
    mutate(`H2S(ppb)` =
             replace(`H2S(ppb)`,
                     str_detect(H2Sflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                     NA)) %>%
    mutate(`CH4(ppm)` =
             replace(`CH4(ppm)`,
                     str_detect(CH4flag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                     NA)) %>%
    mutate(`BEN(ppb)` =
             replace(`BEN(ppb)`,
                     str_detect(BENflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                     NA)) %>%
    mutate(`TOL(ppb)` =
             replace(`TOL(ppb)`,
                     str_detect(TOLflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                     NA)) %>%
    mutate(`XYP(ppb)` =
             replace(`XYP(ppb)`,
                     str_detect(XYPflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                     NA)) %>%
    dplyr::select(!timeqt)
  input_2 <- x %>%
    filter(!Time %in% input$Time) %>%
    mutate(timeqt = timeqt) %>%
    mutate(Transect = gsub(".*/", "", Transect)) %>%
    mutate(Transect = gsub("\\..*", "", Transect)) %>%
    group_by(Transect) %>%
    rowwise() %>%
    mutate(`H2S(ppb)` =
             replace(`H2S(ppb)`,
                     str_detect(H2Sflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                     NA)) %>%
    mutate(`CH4(ppm)` =
             replace(`CH4(ppm)`,
                     str_detect(CH4flag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                     NA)) %>%
    mutate(`BEN(ppb)` =
             replace(`BEN(ppb)`,
                     str_detect(BENflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                     NA)) %>%
    mutate(`TOL(ppb)` =
             replace(`TOL(ppb)`,
                     str_detect(TOLflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                     NA)) %>%
    mutate(`XYP(ppb)` =
             replace(`XYP(ppb)`,
                     str_detect(XYPflag, "MD|ND|AM|AN|AT|AZ|BA|BN|QX"),
                     NA)) %>%
    dplyr::select(!timeqt)
  input_3 <- bind_rows(input, input_2) %>%
    arrange(Time)
  print(head(input))
  print(head(input_2))
  return(input_3)
}
