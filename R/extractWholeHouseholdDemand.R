# Extract whole household demand

# dataset in /extracts is only 2 households...
# use the EECA half-hourly data for speed

# Packages ----
library(data.table) # for data munching
library(lubridate) # for dateTimes
library(hms) # hms
library(GREENGridData) # for useful functions

# Functions ----
source(here::here("R", "functions.R"))

# Data ----
dataPath <- "~/temp/"
# half-hourly total household consumption

all.files <- list.files(paste0(dataPath, "ggHalfHourly"), full.names = TRUE)

l <- lapply(all.files, fread, sep=",")
dt <- rbindlist( l )

extractDT <- dt[circuit == "imputedTotalDemand_circuitsToSum_v1.1"]
table(extractDT$circuit)

summary(extractDT)

setkey( extractDT , linkID) # sorts them
of <- paste0(dataPath, "halfHourImputedTotalDemand_Fixed.csv")
data.table::fwrite(extractDT, of)
GREENGridData::gzipIt(of)