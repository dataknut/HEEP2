# Statistical power calculations

# Packages ----
library(data.table) # for data munching
library(ggplot2) # for plots
library(lubridate) # for dateTimes
library(pwr) # pwr tests - can be easier to use than stats::
library(hms) # hms
library(GREENGridData) # for useful functions
library(kableExtra) # tables

# Functions ----
source(here::here("R", "functions.R"))

# Paramaters ----
HEEP2full <- 280
HEEP2fullAvail <- 700
HEEP2poolOb <- 2800

# Data ----
# Sourced from https://reshare.ukdataservice.ac.uk/853334/
dataPath <- "~/temp/"
# half-hourly total household consumption (pre-prepared)
totalF <- paste0(dataPath, "halfHourImputedTotalDemand_Fixed.csv.gz")

# half-hourly lighting consumption
lightingF <- paste0(dataPath, "halfHourLighting.csv.gz")

# half-hourly heat pump consumption
heatPumpF <- paste0(dataPath, "halfHourHeatPump.csv.gz")

# household attribute data
hhF <- paste0(dataPath, "ggHouseholdAttributesSafe_2019-04-09.csv.gz")

# Data prep ---
# > power ----
totalDT <- data.table::fread(totalF)
setkey(totalDT, linkID)
uniqueN(totalDT$linkID)

heatPumpDT <- data.table::fread(heatPumpF)
uniqueN(heatPumpDT$linkID)
setkey(heatPumpDT, linkID)

lightingDT <- data.table::fread(lightingF)
uniqueN(lightingDT$linkID)
setkey(lightingDT, linkID)

powerDataPrep <- function(dt){
  dt[, meanConsumptionkWh := (meanPowerW/2)/1000]
  dt[, r_as_dateTime := lubridate::as_datetime(r_dateTimeHalfHour)]
  dt[, r_dateTimeNZ := lubridate::with_tz(r_as_dateTime, "Pacific/Auckland")]
  dt[, r_date := lubridate::as_date(r_dateTimeNZ)]
  dt[, r_halfHour := hms::as_hms(r_dateTimeNZ)]
  dt <- addNZSeason(dt, dateTime = "r_dateTimeNZ")
  return(dt)
}

totalDT <- powerDataPrep(totalDT)
heatPumpDT <- powerDataPrep(heatPumpDT)
lightingDT <- powerDataPrep(lightingDT)

# check - beware which hemisphere we are in?
table(totalDT$month, totalDT$season)

testDT <- totalDT[, .(meankWh = mean(meanConsumptionkWh)), 
                  keyby = .(r_halfHour, season)]
ggplot2::ggplot(testDT, aes(x = r_halfHour, y = meankWh, colour = season)) + 
  geom_line() +
  labs(caption = "Whole household kWh")


testDT <- lightingDT[, .(meankWh = mean(meanConsumptionkWh)), 
                  keyby = .(r_halfHour, season)]
ggplot2::ggplot(testDT, aes(x = r_halfHour, y = meankWh, colour = season)) + 
  geom_line() +
  labs(caption = "Lighting kWh where known")
# looks OK
message("# half-hour: whole household kWh")
summary(totalDT)

# > household ----
hhDT <- data.table::fread(hhF)
hhDT <- code_Q7(hhDT)
hhDT[, hasPV := `PV Inverter`]
hhDT[, hasPV := ifelse(hasPV == "", "No", hasPV)]

# check
uniqueN(hhDT$linkID)
uniqueN(totalDT$linkID)

setkey(hhDT, linkID)
setkey(totalDT, linkID)

#> data checks ----

# any zeros & negative numbers?
hist(totalDT$meanConsumptionkWh)
# some

# check if aggregated to daily sum
dailyAllDT <- totalDT[, .(sumkWh = sum(meanConsumptionkWh)), keyby = .(r_date, linkID)]
hist(dailyAllDT$sumkWh)
# still some neg - probably due to PV?

# check if aggregated to daily mean
dt <- dailyAllDT[, .(meankWh = mean(sumkWh),
                     sdkWh = sd(sumkWh)), keyby = .(linkID)]
hist(dt$meankWh)
# still some

# need to check -ve = mid-day, if not is not PV must just be errors?
totalDT[, testValues := "> 0"]
totalDT[, testValues := ifelse(meanConsumptionkWh == 0, "0", testValues)]
totalDT[, testValues := ifelse(meanConsumptionkWh < 0, "< 0", testValues)]
testDT <- totalDT[hhDT[, .(linkID, hasPV)]]
dt <- totalDT[testValues == "< 0", .(nValues = .N),
                  keyby = .(r_halfHour, season, linkID, testValues,hasPV)]

p <- ggplot2::ggplot(dt[!is.na(testValues) & !is.na(season)], aes(x = r_halfHour, y = nValues, colour = linkID)) +
  geom_line() +
  facet_grid(season ~ testValues + hasPV) +
  labs(x = "Half hour",
       y = "N",
       caption = "Colours = individual dwellings, legend omitted for clarity"
  ) +
  theme(legend.position="none")
p

library(plotly)
plotly::ggplotly(p)
# so:
# a) neg values indicate PV
# b) at least 1 dwelling had PV but didn't say so in survey - rf_06
hhDT[, hasPVfixed := ifelse(linkID == "rf_06", "yes", hasPV)]
hhDT[, .(n = .N), keyby = .(hasPV, hasPVfixed)]

# If we only care about network load we could keep all values > 0 only
# If we want total energy input then we need grid draw + PV input which we might be able
# to calculate from the PV circuit W - grid export
# but it gets complicated.
# So for now we'll leave out the dwellinmgs which seem to have PV
setkey(dailyAllDT, linkID)
dailyDT <- dailyAllDT[hhDT[hasPVfixed == "No"]]

dailyMeanDT <- dailyDT[!is.na(sumkWh), .(meankWh = mean(sumkWh, na.rm = TRUE),
                           sdkWh = sd(sumkWh, na.rm = TRUE)), keyby = .(linkID)]
setkey(dailyMeanDT, linkID)
#> final data ----
dailyMeanLinkedDT <- dailyMeanDT[hhDT]
dailyMeanLinkedDT <- dailyMeanLinkedDT[!is.na(meankWh)]

# Analysis ----

make_p95Table <- function(dt,groupVar, kWh = "meankWh"){
  # aggregates kWh
  # remember that kWh could be defined in a range of ways (daily sum, mean etc)
  t95 <- dt[!is.na(Q7labAgg), .(meankWh = mean(get(kWh)),
                                           minkWh = min(get(kWh)),
                                           maxkWh = max(get(kWh)),
                                           sdkWh = sd(get(kWh)),
                                           nHouseholds = uniqueN(linkID)),
                       keyby = .(category = get(groupVar))]
  setnames(t95, c("category"), groupVar)
  t90 <- copy(t95) # has to be a copy
  
  t95[, Threshold := "p < 0.05 (observed n)"]
  t95[, se := sdkWh/sqrt(nHouseholds)]
  t95[, error := qnorm(0.975)*se]
  t95[, CI_lower := meankWh - error]
  t95[, CI_upper := meankWh + error]
  
  t90[, Threshold := "p < 0.1 (observed n)"]
  t90[, se := sdkWh/sqrt(nHouseholds)]
  t90[, error := qnorm(0.95)*se]
  t90[, CI_lower := meankWh - error]
  t90[, CI_upper := meankWh + error]
  
  return(rbind(t90, t95))
}

make_CIplot <- function(t, kwh = "meankWh", xVar, xLab){
  # plots whatever kWh is by xVar
  p <- ggplot2::ggplot(obsT, aes(x = get(xVar), y = kWh, fill = Threshold)) +
    geom_col() + 
    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper)) +
    facet_wrap(Threshold ~  .) +
    labs(x = xLab,
         y = "Mean daily kWh"
    ) +
    theme(legend.position="bottom")
  ggplot2::ggsave(paste0(xVar,"_CI.png"), p, 
                  width = 6, path = here::here("plots"))
  return(p)
}

# > Dwelling age ----
obsT <- make_p95Table(dailyMeanLinkedDT[!is.na(Q7labAgg) & !is.na(meankWh)], 
                      groupVar = "Q7labAgg")

obsT[, kWh := meankWh]
make_CIplot(obsT, xVar = "Q7labAgg", xLab = "Dwelling age")

kableExtra::kable(obsT, digits = 3) %>%
  kable_styling()

# >> Power: Observations ----
# What 'effect size' do we observe?
m1 <- obsT[Q7labAgg == "< 1999" & Threshold %like% "0.05", meankWh]
m2 <- obsT[Q7labAgg == ">= 2000" & Threshold %like% "0.05", meankWh]

# common sd??
r <- lm(meankWh ~ Q7labAgg, data = dailyMeanLinkedDT)
results <- summary(r) # we need the rse https://online.stat.psu.edu/stat462/node/94/
rse <- results$sigma # rse

diff <- abs(m1-m2)
# Difference
diff
d <- diff/rse

n1 <- obsT[Q7labAgg == "< 1999" & Threshold %like% "0.05", nHouseholds]
n2 <- obsT[Q7labAgg == ">= 2000" & Threshold %like% "0.05", nHouseholds]

p1 <- n1/(n1 + n2) # with

# what effect size would we need for the GG n? p = 0.05
pwr95 <- pwr::pwr.t2n.test(n1 = n1,
                  n2 = n2,
                  d = , sig.level = 0.05, power = 0.8)
pwr95$d
# which means a kWh difference of
pwr95$d * rse
  
# what effect size would we need for the GG n? p = 0.1
pwr90 <- pwr::pwr.t2n.test(n1 = n1,
                  n2 = n2,
                  d = , sig.level = 0.1, power = 0.8)
pwr90
# which means a kWh difference of
pwr90$d * rse

# what effect size could we get with HEEPfull? p = 0.05
n1 <- p1 * HEEP2full
n2 <- HEEP2full - n1
pwr95_HEEP2full <- pwr::pwr.t2n.test(n1 = n1,
                  n2 = n2,
                  d = , sig.level = 0.05, power = 0.8)
pwr95_HEEP2full
#HEEP2full
# which means a kWh difference of
pwr95_HEEP2full$d * rse

# what effect size could we get with HEEPfullAvail? p = 0.05
n1 <- p1 * HEEP2fullAvail
n2 <- HEEP2fullAvail - n1
pwr95_HEEP2fullAvail <- pwr::pwr.t2n.test(n1 = n1,
                                     n2 = n2,
                                     d = , sig.level = 0.05, power = 0.8)
pwr95_HEEP2fullAvail
#HEEP2full
# which means a kWh difference of
pwr95_HEEP2fullAvail$d * rse

# what effect size could we get with HEEP2pool? p = 0.05
# HEEP2pool obtained - see table
n1 <- p1 * HEEP2poolOb
n2 <- HEEP2poolOb - n1
pwr95_HEEP2poolOb <- pwr::pwr.t2n.test(n1 = n1,
                  n2 = n2,
                  d = , sig.level = 0.05, power = 0.8)
pwr95_HEEP2poolOb
# which means a kWh difference of
# HEEP2pool
pwr95_HEEP2poolOb$d * rse

# report table
kableExtra::kable(obsT, digits = 2) %>%
  kable_styling()


#> Heat pumps ----

#>> compare with/without ----
# GG
t <- table(hhDT$`Heat pump number`, useNA = "always")
t
prop.table(t)

# with elec data
dailyMeanLinkedDT[, heatPumps := `Heat pump number`]
dailyMeanLinkedDT[, heatPumps := ifelse(is.na(`Heat pump number`), 0, heatPumps)]
dailyMeanLinkedDT[, .(nHHs = uniqueN(linkID)), keyby = .(heatPumps)]

obsT <- make_p95Table(dailyMeanLinkedDT[!is.na(heatPumps)], "heatPumps")
obsT[, kWh := meankWh]
make_CIplot(obsT, kwh = "meankWh", xVar = "heatPumps", xLab = "N heat pumps")

kableExtra::kable(obsT, digits = 3) %>%
  kable_styling()

# switch to binary for HPs - yes or no
dailyMeanLinkedDT[, hasHeatPump := "No"]
dailyMeanLinkedDT[, hasHeatPump := ifelse(heatPumps > 0, "Yes", hasHeatPump)]
obsT <- make_p95Table(dailyMeanLinkedDT[!is.na(hasHeatPump)], "hasHeatPump")

# report table
kableExtra::kable(obsT, digits = 2) %>%
  kable_styling()

# >> Power: Observations ----
# What 'effect size' do we observe?
m1 <- obsT[hasHeatPump == "No" & Threshold %like% "0.05", meankWh]
m2 <- obsT[hasHeatPump == "Yes" & Threshold %like% "0.05", meankWh]

# common sd??
r <- lm(meankWh ~ hasHeatPump, data = dailyMeanLinkedDT)
results <- summary(r) # we need the rse https://online.stat.psu.edu/stat462/node/94/
rse <- results$sigma # rse

diff <- abs(m1-m2)
# Difference
diff
d <- diff/rse

n1 <- obsT[hasHeatPump == "No" & Threshold %like% "0.05", nHouseholds]
n2 <- obsT[hasHeatPump == "Yes" & Threshold %like% "0.05", nHouseholds]

# what effect size would we need for the GG n? p = 0.05
pwr95 <- pwr::pwr.t2n.test(n1 = n1,
                           n2 = n2,
                           d = , sig.level = 0.05, power = 0.8)
pwr95
pwr95$d
# which means a kWh difference of
pwr95$d * rse

# what effect size would we need for the GG n? p = 0.1
pwr90 <- pwr::pwr.t2n.test(n1 = n1,
                           n2 = n2,
                           d = , sig.level = 0.1, power = 0.8)
pwr90
# which means a kWh difference of
pwr90$d * rse

# what effect size could we get with HEEP2full? p = 0.05
# Use HCS proportions
# HCS: 45% owned, 27% renters, overall = 38% (Vicki White and Mark Jones 2017)
n1 <- HEEP2full - (HEEP2full*0.38)
n2 <- HEEP2full*0.38
pwr95_HEEP2full <- pwr::pwr.t2n.test(n1 = n1,
                                     n2 = n2,
                                     d = , sig.level = 0.05, power = 0.8)
pwr95_HEEP2full
#HEEP2full
# which means a kWh difference of
pwr95_HEEP2full$d * rse

# for drawing plot
getDrange <- function(nList,p){
  dt <- data.table::data.table()
  for(n in nList){
    n2 <- n * p # p = the proportion that have X
    n1 <- n - n2
    pres <- pwr::pwr.t2n.test(n1 = n1,
                             n2 = n2,
                             d = , sig.level = 0.05, power = 0.8)
    res <- as.data.table(c(n1,n2,n, pres$d))
    dt <- rbind(dt,transpose(res))
  }
  return(dt)
}

nList <- seq(100,3000,50)
p <- 0.38
resDT <- getDrange(nList, p)
resDT[, kWhDiff := V4 * rse]
pl <- ggplot2::ggplot(resDT, aes(x = V3, y = kWhDiff)) + 
  geom_line() +
  geom_point() +
  geom_vline(xintercept = HEEP2full, alpha = 0.3) +
  geom_vline(xintercept = HEEP2fullAvail, alpha = 0.3) +
  geom_vline(xintercept = HEEP2poolOb, alpha = 0.3) +
  labs(x = "Total sample size",
       y = "Mean kWh difference",
       caption = paste0("p = 0.05, power = 0.8\n",
                        "% with heat pump = ",100*p))
pl
ggplot2::ggsave("kWhDiff_rangeHeatPumps.png", pl, 
                width = 6, path = here::here("plots"))

# what effect size could we get with HEEP2poolAvail? p = 0.05
# HEEP2pool obtained - see table
n1 <- HEEP2fullAvail - (HEEP2fullAvail*0.38)
n2 <- HEEP2fullAvail*0.38
pwr95_HEEP2poolAvail <- pwr::pwr.t2n.test(n1 = n1,
                                       n2 = n2,
                                       d = , sig.level = 0.05, power = 0.8)
pwr95_HEEP2poolAvail
# which means a kWh difference of
# HEEP2pool
pwr95_HEEP2poolAvail$d * rse

# what effect size could we get with HEEP2poolObtained? p = 0.05
# HEEP2pool obtained - see table

n1 <- HEEP2poolOb - (HEEP2poolOb*0.38)
n2 <- HEEP2poolOb*0.38
pwr95_HEEP2poolOb <- pwr::pwr.t2n.test(n1 = n1,
                                       n2 = n2,
                                       d = , sig.level = 0.05, power = 0.8)
pwr95_HEEP2poolOb
# which means a kWh difference of
# HEEP2pool
pwr95_HEEP2poolOb$d * rse

# >> compare am/pm ----
# use HP only data
# remove -ve values
heatPumpDT <- heatPumpDT[meanConsumptionkWh >= 0]

# check heat pump patterns
plotDT <- heatPumpDT[, .(meankWh = mean(meanConsumptionkWh)), 
                     keyby = .(r_halfHour, season)]
ggplot2::ggplot(plotDT, aes(x = r_halfHour, y = meankWh, colour = season)) +
  geom_line()

# Based on the line plot...

heatPumpDT[, period := ifelse(lubridate::hour(r_dateTimeHalfHour) >= 4 &
                                lubridate::hour(r_dateTimeHalfHour) < 10,
                                                "04:00 - 10:00", NA)]
heatPumpDT[, period := ifelse(lubridate::hour(r_dateTimeHalfHour) >= 16 &
                                lubridate::hour(r_dateTimeHalfHour) < 22,
                              "16:00 - 22:00", period)]

# check
with(heatPumpDT, table(lubridate::hour(r_dateTimeHalfHour),period))
     
dailyHeatPumpDT <- heatPumpDT[!is.na(period), .(meankWh = mean(meanConsumptionkWh), # use mean as some have multiple circuits
                                                nObs = .N), # how many obs?
                              keyby = .(r_date, period, linkID, season)]
# check
dailyHeatPumpDT[, .(meanSum = mean(meankWh)), 
                keyby = .(season, period)]

ggplot2::ggplot(dailyHeatPumpDT, aes(y = meankWh, x = period)) + 
  geom_boxplot() +
  facet_wrap(season ~ .)

dailyMeanHeatPumpDT <- dailyHeatPumpDT[, .(meankWh = mean(meankWh),
                                           sdkWh = sd(meankWh)),
                                       keyby = .(linkID, period, season)]

dailyMeanHeatPumpDT[, .(mean = mean(meankWh)), 
                keyby = .(season, period)]

setkey(dailyMeanHeatPumpDT, linkID)
dailyMeanHeatPumpDTLinkedDT <- dailyMeanHeatPumpDT[hhDT]

obsT <- make_p95Table(dailyMeanHeatPumpDTLinkedDT[!is.na(period)], groupVar = "period")

obsT[, kWh := meankWh]
p <- make_CIplot(obsT[season == "winter"], # winter only
                 kwh = "meankWh", xVar = "period", xLab = "Period")
p <- p + coord_flip() + labs(y = "Mean kWh")
ggplot2::ggsave(paste0("heatPumpByPeriod_Winter_CI.png"), p, 
                width = 6, path = here::here("plots"))

kableExtra::kable(obsT, digits = 3) %>%
  kable_styling()

# >> Power calcs - paired ----

# What 'effect size' do we observe?
m1 <- obsT[period == "04:00 - 10:00" & Threshold %like% "0.05", meankWh]
m2 <- obsT[period != "04:00 - 10:00" & Threshold %like% "0.05", meankWh]

# common sd??
# strictly speaking we have paired observations so is this correct?
r <- lm(meankWh ~ period, data = dailyMeanHeatPumpDTLinkedDT)
results <- summary(r) # we need the rse https://online.stat.psu.edu/stat462/node/94/
rse <- results$sigma # rse

diff <- abs(m1-m2)
# Difference
diff
d <- diff/rse

getPairedD <- function(nList,rse){
  dt <- data.table::data.table()
  for(n in nList){
    pres <- pwr::pwr.t.test(n = n,
                              d = , sig.level = 0.05, power = 0.8,
                            type = c("paired"))
    res <- as.data.table(c(n, pres$d, pres$d * rse))
    dt <- rbind(dt,transpose(res))
  }
  return(dt)
}

nList <- c(uniqueN(dailyMeanHeatPumpDT$linkID), HEEP2full,HEEP2fullAvail,HEEP2poolOb)
getPairedD(nList, rse) # print out the d and kWh diff required for each n


nList <- seq(50,1000,50)
pairedDT <- getPairedD(nList, rse) # print out the d and kWh diff required for each n

pl <- ggplot2::ggplot(pairedDT, aes(x = V1, y = V3)) + 
  geom_line() +
  geom_point() +
  labs(x = "Sub-group sample size",
       y = "Mean kWh difference",
       caption = paste0("p = 0.05, power = 0.8"))
pl
ggplot2::ggsave("kWhDiff_rangeHeatPumpsSubgroups.png", pl, 
                width = 6, path = here::here("plots"))

# > Lighting ----
# GG
t <- table(hhDT$Q49_coded, useNA = "always")
t
prop.table(t)

# need to use lighting extract

dailyLightingDT <- lightingDT[, .(sumkWh = sum(meanConsumptionkWh)), 
                              keyby = .(r_date, linkID)]
dailyMeanLightingDT <- dailyLightingDT[, .(meankWh = mean(sumkWh),
                                           sdkWh = sd(sumkWh)),
                                       keyby = .(linkID)]
setkey(dailyMeanLightingDT, linkID)
dailyMeanLightingLinkedDT <- dailyMeanLightingDT[hhDT]

dailyMeanLightingLinkedDT[, .(nHHs = uniqueN(linkID)), keyby = .(Q49_coded)]
dailyMeanLightingLinkedDT <- dailyMeanLightingLinkedDT[Q49_coded != "" & 
                                                         !is.na(meankWh)]

obsT <- make_p95Table(dailyMeanLightingLinkedDT[!is.na(Q49_coded)], groupVar = "Q49_coded")

obsT[, kWh := meankWh]
p <- make_CIplot(obsT, kwh = "meankWh", xVar = "Q49_coded", xLab = "Main lumen type")
p <- p + coord_flip() 
ggplot2::ggsave(paste0("Q49_coded_CI.png"), p, 
                width = 6, path = here::here("plots"))
  
kableExtra::kable(obsT, digits = 3) %>%
  kable_styling()

# > Power?

# Proportions ----

# > Confidence intervals ----
z <- qnorm(0.975) # p = 0.05
p <- 0.4 #test a p
n <- 150
MoE <- z * sqrt(p*(1-p)/(n-1)) # calculate margin of error
MoE

# > Power ----
# pwr.2p.test(h = , n = , sig.level =, power = ) 
# calculate for single sample
pwr::pwr.2p.test(h = , n = 360, sig.level = 0.05, power = 0.8) 
# but this produces h which needs converting back to %

# single sample test
# n = n in the single sample
pwr.p.test(h = , n = 360, sig.level = 0.05, power = 0.8)

# power.prop.test is easier to use
# calculate n for each group - e.g. % heat pumps in renters vs owner-occupiers
stats::power.prop.test(n = NULL, p1 = 0.4, p2 = 0.25, 
                       power = 0.8, sig.level = 0.05)


calculateProportionsMoE <- function(props, sig, samples){
  # calculate margins of error given prop, a range of significance thresholds (sigs) and sample sizes
  nProps <- length(props)
  nSamps <- length(samples)
  #initialise results array
  resultsArray <- array(numeric(nSamps*nProps),
                        dim=c(nSamps,nProps)
  )
  # loop over samples
  for (s in 1:nSamps){
    # loop over significance values
  for (p in 1:nProps){
    me <- qnorm(1-(sig/2)) * sqrt(props[p]*(1 - props[p])/(samples[s]-1))
    resultsArray[s,p] <- me # report effect size against sample size
  }
  }
  dt <- data.table::as.data.table(resultsArray) # convert to dt for tidying
  dt <- cbind(dt, samples)
  longDT <- data.table::as.data.table(reshape2::melt(dt, id=c("samples")))
  longDT <- data.table::setnames(longDT, "value", "moe")
  longDT <- data.table::setnames(longDT, "variable", "proportion")
  return(longDT) # returned the tidied & long form dt
}

samples <- seq(100,3000,10)
props <- c(0.1, 0.2, 0.3, 0.4, 0.5)
dt <- calculateProportionsMoE(props = props, # one sample
                                    sig = 0.05,
                                    samples = samples
                                    )
# need to recode vars
# must be an easier way
dt[, prop := ifelse(proportion == "V1", "10%", NA)]
dt[, prop := ifelse(proportion == "V2", "20%", prop)]
dt[, prop := ifelse(proportion == "V3", "30%", prop)]
dt[, prop := ifelse(proportion == "V4", "40%", prop)]
dt[, prop := ifelse(proportion == "V5", "50%", prop)]

p <- ggplot2::ggplot(dt, aes(x = samples, y = 100*moe, colour = prop)) +
  geom_point(alpha = 0.5) +
  geom_line() +
  labs(y = "Margin of error (+/-%)",
       x = "Sample N (single sample)") +
  scale_color_discrete(name="Proportion:") +
  theme(legend.position="bottom") +
  geom_vline(xintercept = HEEP2full, alpha = 0.3) +
  geom_vline(xintercept = HEEP2fullAvail, alpha = 0.3) +
  geom_vline(xintercept = HEEP2poolOb, alpha = 0.3)

p + annotate(geom = "text", 
             x = HEEP2full, 
             y = 0.9*(max(p$data$moe)*100), 
             label = paste0("n = 350"), 
             hjust = "left") +
  annotate(geom = "text", 
           x = HEEP2fullAvail, 
           y = 0.8*(max(p$data$moe)*100), 
           label = paste0("n = 700"), 
           hjust = "left") +
  annotate(geom = "text", 
           x = HEEP2poolOb, 
           y = 0.9*(max(p$data$moe)*100), 
           label = paste0("n = 2500"), 
           hjust = "left") 

ggplot2::ggsave("proportionsMoE.png", p, 
                width = 6, path = here::here("plots"))

# details
dt[samples == HEEP2full]

dt[samples == HEEP2fullAvail]

dt[samples == HEEP2poolOb]
