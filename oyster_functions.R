#function to replace values in vector x with y
#and remove white spaces
oyster_modify_txt <- function(x, y) {
  
  #replace
  x1 <- stringr::str_replace_all(x, y)
  #remove white spaces
  x2 <- qdapRegex::rm_white(x1)
  #return
  x2
  
}

oyster_hhmm <- function(x) {
  #minutes to seconds
  x1 <- x * 60
  #seconds to period
  x2 <- lubridate::seconds_to_period(x1)
  #hhmm
  x3 <- sprintf("%02i:%02i", lubridate::hour(x2), 
                lubridate::minute(x2))
  #return
  x3
  
}

oyster_datetime <- function(x, y, month = 11, year = 2009) {
  #prepare date text
  y1 <- paste0(y, "/", month, "/", year)
  #convert to date class
  y1_date <- as.Date(y1, format = "%d/%m/%Y")
  #convert to datetime
  xy_datetime <- strptime(paste0(y1_date, " ", x), format = "%Y-%m-%d %H:%M")
  #return
  xy_datetime
  
}

oyster_googlemode <- function(x) {
  
  y <- if(x %in% c("DLR","DLR/LRC","LRC","TRAM")) {
       "Tram"
    } else if(x %in% c("HEX","LUL/NR/DLR","LUL/NR/LRC")) {
      "Rail"
    } else if(x %in% c("LUL/DLR","LUL/LRC","LUL/NR","LUL/TRAM")) {
      "Subway|Tram"
    } else if(x == "NR") {
      "Train"
    } else if(x %in% c("NR/DLR","NR/LRC")) {
      "Train|Tram"  
    } else {
      "Subway"
    }
  y  
}

oyster_googlemode_all <- function(x) {
  
  y <- sapply(x, oyster_googlemode, simplify = TRUE)
  y
  
}


