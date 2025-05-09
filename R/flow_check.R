flow_check <- function( x, flow_out = c("less", "greater", "both"),
                        flow_low = 3, flow_high = 7){
  if (flow_out=="less"){
    output_less <- x %>%
      filter(header=="Ambient-Volumetric-Flow" & value < flow_low)

    ifelse(length(output_less > 0),
                print("Ambient Volumetric Flow less than user flow_low input (BAD)"),
                print("Ambient Volumetric Flow greater than user flow_low input (GOOD)"))

    return(output_less)}

  if(flow_out=="greater"){
    output_great <- x %>%
      filter(header=="Ambient-Volumetric-Flow" & value > flow_high)

    ifelse(length(output_great > 0),
           print("Ambient Volumetric Flow greater than user flow_high input (BAD)"),
           print("Ambient Volumetric Flow less than user flow_high input (GOOD)"))

    return(output_great)}

  if(flow_out=="both"){

    output_less <- x %>%
      filter(header=="Ambient-Volumetric-Flow" & value < flow_low)

    ifelse(length(output_less > 0),
           print("Ambient Volumetric Flow less than user flow_low input (BAD)"),
           print("Ambient Volumetric Flow greater than user flow_low input (GOOD)"))

    output_great <- x %>%
      filter(header=="Ambient-Volumetric-Flow" & value > flow_high)

    ifelse(length(output_great > 0),
           print("Ambient Volumetric Flow greater than user flow_high input (BAD)"),
           print("Ambient Volumetric Flow less than user flow_high input (GOOD)"))

    output_both <- output_less %>%
      rbind(output_great)
    return(output_both)}
}
