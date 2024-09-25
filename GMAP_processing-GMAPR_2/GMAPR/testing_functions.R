#testing functions
devtools::install("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/gmap_package/GMAPR/R")
d<-analyte_flagging(path = "C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/Phillips66_REG_Avitec_2023/phillips_validation/WOOD RIVER/GMAP Data WOOD RIVER/Mapping",
                    onoff = "off", trantype = "MA",
                    h2smdl = 9.61,h2shs=10300, ch4mdl = 0.0138,ch4hs = 200,
                    benmdl = 5.08,benhs = 106,tolmdl = 5.44,tolhs = 98,
                    xypmdl = 4.86,xyphs = 101.9)
a<-trans_max(path = "C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/Phillips66_REG_Avitec_2023/phillips_validation/WOOD RIVER/GMAP Data WOOD RIVER/Mapping",
             onoff = "off", trantype = "MA",
             h2smdl = 9.61,h2shs=10300, ch4mdl = 0.0138,ch4hs = 200,
             benmdl = 5.08,benhs = 106,tolmdl = 5.44,tolhs = 98,
             xypmdl = 4.86,xyphs = 101.9)


f<-unique(d$Transect)
f
g<-d%>%
  filter(Transect== "230724_MA03")
?analyte_flagging

Sys.setenv(PATH = paste("C:/Program Files (x86)/rtools40/usr/bin", Sys.getenv("PATH") ,sep = ";"))
Sys.setenv(BINPREF = "C:/rtools/mingw$(WIN)/bin/")
find_rtools()


path<-"C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/Phillips66_REG_Avitec_2023/phillips_validation/WOOD RIVER/GMAP Data WOOD RIVER/Mapping"
files <- list.files(path = path, pattern = "MA", full.names =T)

input <- lapply(files, FUN = read.table,
                skip = 7, header = TRUE, sep = "\t", fill = TRUE,
                stringsAsFactors = FALSE, na.strings = "NaN",
                col.names = c("Time", "CO2", "CH4", "H2S", "C2H2",
                              "M3DSU", "M3DSV", "M3DSW",
                              "M3DS2DSpeed", "M3DS3DSpeed",
                              "M3DSAzimuth", "M3DSElevation",
                              "M3DSSoS", "M3DSSonicTemp",
                              "M3DSErrorCode", "GPSTrack",
                              "GPSGroundSpeed", "GPSTime",
                              "GPSLatitude", "GPSLongitude", "GPSDoP",
                              "BEN", "TOL", "ETB", "XYO", "XYM",
                              "XYP", "SO2", "STY", "FOR", "NH3", "NO",
                              "NO2", "O3", "CellPressure",
                              "CellTemperature", "SUMMAPressure",
                              "VOC", "AirMar.Wind.Speed..m.s.",
                              "AirMar.Wind.Direction..TRUE.",
                              "AirMarAirTemp[oC]",
                              "AirMarRelHumidity[%]",
                              "AirMarBarometer[mBar]",
                              "AirMarHeading",
                              "AirMarGroundSpeed[MPH]",
                              "AirMar.Latitude",
                              "AirMar.Longitude"))

#analyte flagging output####
ana_flag_results<-analyte_flagging(path = "C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/Phillips66_REG_Avitec_2023/phillips_validation/WOOD RIVER/GMAP Data WOOD RIVER/Mapping",
                                   onoff = "off", trantype = "MA",
                                   h2smdl = 9.61,h2shs=10300, ch4mdl = 0.0138,ch4hs = 200,
                                   benmdl = 5.08,benhs = 106,tolmdl = 5.44,tolhs = 98,
                                   xypmdl = 4.86,xyphs = 101.9)
#time flagging output####
time_flag<-time_flagging(ana_flag_results,timestart = "2023-07-24 18:25:00",timestop = "2023-07-24 18:45:00", timeqt = "TEST", analyte = c("H2Sflag", "CH4flag", "BENflag",
                                                                                                                                           "TOLflag", "XYPflag"))
#time_series_table output####
h2s_time<-ana_flag_results%>%
  select(Time,Transect,`H2S(ppb)`,H2Sflag,Latitude,Longitude,ws,wsflag,wd,wdflag) %>%
  filter(!is.na(`H2S(ppb)`))

#maxes_hist_virdis
maxes_hist_virdis(ana_flag_results,analyte = c("H2S(ppb)","CH4(ppm)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)"))

#max_hist
maxes_hist(ana_flag_results,analyte = c("H2S(ppb)", "BEN(ppb)","CH4(ppm)", "TOL(ppb)", "XYP(ppb)"))


#transect_time_series_

time_series_plot(ana_flag_results,analyte = c("H2S(ppb)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)"))


#trans_max_min
tran_time_min_max(ana_flag_results)

#field_tool_map
field_tool_mapping(path_data = "C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/dashboard/test/on_machine_tool/on_machine_tool/GMAP Data TRADEBE/Mapping",
                   path_imagery = "C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/dashboard/test/on_machine_tool/on_machine_tool/m_4108721_sw_16_060_20220616.tif",
                   analyte = "ALL",
                   pattern = "MA")

#NA_id
f<-NA_id(path =  "C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/dashboard/test/on_machine_tool/on_machine_tool/GMAP Data TRADEBE/Mapping",
      trantype = "MA",
      analyte = c( "TOL", "XYP"))
