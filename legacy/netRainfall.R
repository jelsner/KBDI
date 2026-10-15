# The script estimates net rainfall from 24-hr (daily) totals in inches. 
# No missing values are allowed.
netRainfall <- function(Rainfall24) {
  PR <- lag(Rainfall24)
  PR[1] <- 0
  
  CumR <- 0
  NetR <- numeric()
  
  for(i in 1:length(Rainfall24)) {
    R24 <- Rainfall24[i]
    
    if ( R24 == 0) {
      NetR[i] <- 0
      CumR <- 0
    } 
    else if( R24 > 0 & R24 <= .2) {
      CumR <- CumR + R24
      if (PR[i] > .2 | CumR > .2) NetR[i] <- R24
      else if (CumR > .2) NetR[i] <- CumR - .2
      else NetR[i] <- 0
    }
    
    else if ( R24 > .2) {
      if (CumR <= .2) {
        NetR[i] <- CumR + R24 - .2
        CumR <- CumR + R24
      }
      else {
        NetR[i] <- R24
        CumR <- CumR + R24
      }
    }
  }
  return(NetR)
}