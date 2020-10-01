# Functions
message("Adding local functions...")

message("-> addNZSeason")
addNZSeason <- function(dt, dateTime){
  dt <- dt[, tmpM := lubridate::month(get(dateTime))] # sets 1 (Jan) - 12 (Dec). May already exist but we can't rely on it
  dt <- dt[, month := lubridate::month(get(dateTime), label = TRUE)]
  dt <- dt[, season := "Summer"] # easiest to set the default to be the one that bridges years
  dt <- dt[tmpM >= 3 & tmpM <= 5, season := "Autumn"]
  dt <- dt[tmpM >= 6 & tmpM <= 8 , season := "Winter"]
  dt <- dt[tmpM >= 9 & tmpM <= 11, season := "Spring"]
  # re-order to make sense
  dt <- dt[, season := factor(season, levels = c("Spring", "Summer", "Autumn", "Winter"))]
  table(dt$season, dt$month)
  dt$tmpM <- NULL
  return(dt)
}

#Q7 When was your house built? ----
#Before 1978 (1)
#Between 1978 – 1999 (2)
#Between 2000 - 2007 (3)
#After 2007 (4)
#Don't know (5)
code_Q7 <- function(dt){
  library(dplyr)
  dt[, Q7lab := dplyr::recode(Q7, `1` = "1977 or earlier", 
                                `2` = "1978-1999", 
                                `3` = "2000-2007",
                                `4` = "2008 or later"
  )]
  table(dt$Q7, dt$Q7lab, useNA = "always")
  dt[, Q7labAgg := dplyr::recode(Q7, `1` = "< 1999", 
                              `2` = "< 1999", 
                              `3` = ">= 2000",
                              `4` = ">= 2000"
  )]
  return(dt)
}

 