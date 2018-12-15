# Libraries
library(dplyr)
library(lubridate)


# Functions
# Estimate rh, vpd and temperature-dewpoint depression from dewpoint and temp
rh.vpd.tdd <- function(mydata) {
  ## air temperature
  Ta <- mydata$temp
  ## Dewpoint temperature
  Td <- mydata$dewpoint
  ## saturated vapor pressure (vp.s).  The units are in kPa
  vp.s <- 0.611 * 10^((7.5*Ta)/(237.3+Ta))
  
  ## the actual vapor pressure, which is calculated from the dewpoint. The unit are in kPa
  vp.a <- 0.611 * 10^((7.5*Td)/(237.3+Td))
  
  ## the VPD (kPa):
  mydata$vpd <- vp.s - vp.a
  
  # relative humidity (%)
  mydata$rh <- 100*(vp.a/vp.s)
  
  # dewpoint depression (Tdd)
  mydata$Tdd <- mydata$temp - mydata$dewpoint 
  
  return(mydata)
}