#!/usr/bin/env Rscript 
##==============================================================================
## NOAA_StormDatabase_analysis_v1.R
##==============================================================================
## A Evans 29/11/2015   R version 3.2.2 (2015-08-14) -- "Fire Safety"
##------------------------------------------------------------------------------
## Script to perform exploratory analysis on data acquired between 1950 and 2011 
## by the US National Weather Service (NWS).to determine the types of severe 
## weather events are most harmful for: a) population health, b) economy.
##------------------------------------------------------------------------------
## Process flow
##------------------------------------------------------------------------------
## Set working directory if required
## Extraction of NWS Directive 10-1605 Event types from pd01016005curr.pdf
## Read in variables required for analysis
## Assign NWS collection method identifier
## Dollar multiplier for damages
## Map reported events to NWS Directive 10-1605 standardised event types
## Aggregate fatalities by severe weather event and NWS collection method.
## Aggregate injuries by severe weather event and NWS collection method.
## Aggregate crop damages by severe weather event and NWS collection method.
## Aggregate property damages by severe weather event and NWS collection method.
## Aggregate fatalities and injuries by weather event
## Aggregate fatalities and injuries by event, derive total affected individuals 
## and subset the top most harmful event types.
## Output the results of anlaysis - plots and text files file
##------------------------------------------------------------------------------
## Required Data files
##------------------------------------------------------------------------------
## NWS Storm Documentation with standardised reporting categories
## https://www.ncdc.noaa.gov/stormevents/pd01016005curr.pdf 
##
## (NOAA) National Weather Service (NWS) Storm Database 1950-2011
## https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2 "Storm Data"  
## [47Mb] comma delimited bzip2 compressed file 
## The file consists of 37 columns and 902,298 rows, which includes a single 
## header row. Missing values are coded as blank fields.
##
## Required data files can be downloaded and placed within directory AECode
## Else, if absent, they will be downloaded automatically from the web
##------------------------------------------------------------------------------
## Run requirements
##------------------------------------------------------------------------------
## NOAA_StormDatabase_analysis.R can be run from within R or RStudio
## Or from the command line: Rscript NOAA_StormDatabase_analysis.R
##------------------------------------------------------------------------------
## Script Output
##------------------------------------------------------------------------------
## 1950_1954_Top10_HarmfulEvents_Economically.txt
## 1950_1954_Top10_HarmfulEvents_PopulationHealth.txt
## 1950_2011_Top10_HarmfulEvents_Economically.txt
## 1950_2011_Top10_HarmfulEvents_PopulationHealth.txt
## 1955_1995_Top10_HarmfulEvents_Economically.txt
## 1955_1995_Top10_HarmfulEvents_PopulationHealth.txt
## 1996_2011_Top10_HarmfulEvents_Economically.txt
## 1996_2011_Top10_HarmfulEvents_PopulationHealth.txt
## 1950_2011_Top10_HarmfulEvents_Economically.png
## 1950_2011_Top10_HarmfulEvents_PersonalHealth.png
##==============================================================================

#-------------------------------------------------------------------------------
# Set working directory if required
#-------------------------------------------------------------------------------
setwd("../AECode")

# load libraries
library(NLP)
library(tm)
library(ggplot2)
library(reshape)


# Function to round and insert commas 
fmt_rounded_big_mark_fun <- function(x){
    format(round(x,0), big.mark=",", scientific=FALSE)
}


# Function to round, insert commas and add dollar symbol as prefix
fmt_rounded_big_mark_dollar_fun <- function(x){
    x <- format(round(x,0), big.mark=",", scientific=FALSE)
    paste0("$",x)
}


#-------------------------------------------------------------------------------
# Extraction of NWS Directive 10-1605 Event types from pd01016005curr.pdf
#-------------------------------------------------------------------------------
# Using method of Gregory D. Horne TA:
# https://class.coursera.org/repdata-033/forum/thread?thread_id=46


if (!file.exists("pd01016005curr.pdf")) { 
        URL <- "https://www.ncdc.noaa.gov/stormevents/pd01016005curr.pdf"
        download.file(URL, destfile = "../AECode/pd01016005curr.pdf")
}

pdf <- readPDF(control = list(c(text = "-layout")))
pdf <- pdf(elem=list(uri="pd01016005curr.pdf"),language="en")
events <- c(pdf$content[seq(397, 420)], pdf$content[seq(425, 448)])

# clear down 
rm(pdf)

# change event type descriptions from caps to lower case
events <- tolower(events)

# Set import date format 
# col 3 time zones vary, but only interested in year
setClass("csvDate")
setAs("character","csvDate", function(from) as.Date(from, 
        format="%m/%d/%Y %H:%M:%S") )

# number of rows to read in
nr = 902297


#-------------------------------------------------------------------------------
# Read in variables required for analysis
#-------------------------------------------------------------------------------
vars <- rep("NULL", 37)
vars[2]  <- "csvDate"   # "BGN_DATE"
vars[8]  <- "character" # "EVTYPE"
vars[23] <- "numeric"   # "FATALITIES"
vars[24] <- "numeric"   # "INJURIES"
vars[25] <- "numeric"   # "PROPDMG"
vars[26] <- "character" # "PROPDMGEXP"
vars[27] <- "numeric"   # "CROPDMG"
vars[28] <- "character" # "CROPDMGEXP"
cc = vars

if (!file.exists("repdata_data_StormData.csv")) {
    if (!file.exists("repdata_data_StormData.csv.bz2")) { 
        URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        download.file(URL, destfile = "../AECode/repdata_data_StormData.csv.bz2")
    }
    stormdata <- read.csv("repdata_data_StormData.csv.bz2", sep=",", na.strings = "", 
                         header=TRUE, nrows=nr, colClasses=cc, strip.white=TRUE)
} else { stormdata <- read.csv("repdata_data_StormData.csv", sep=",", na.strings = "", 
                              header=TRUE, nrows=nr, colClasses=cc, strip.white=TRUE) 
}


# convert to column names to lower case
names(stormdata) <- tolower(names(stormdata))

# drop rows where no losses are reported: returns 254633
stormdata <-stormdata[stormdata$fatalities!=0 | stormdata$injuries!=0 |
                          stormdata$propdmg!=0 | stormdata$cropdmg!=0,]

# convert event types to lower case
stormdata$evtype <- tolower(stormdata$evtype)

# convert cropdmgexp multiplier code to upper case
stormdata$cropdmgexp <- toupper(stormdata$cropdmgexp)

# convert propdmgexp multiplier code to upper case
stormdata$propdmgexp <- toupper(stormdata$propdmgexp)


# Year
stormdata$year <- strftime(stormdata$bgn_date, "%Y")


#-------------------------------------------------------------------------------
# Assign NWS collection method identifier
#-------------------------------------------------------------------------------
# 1) 1950 through 1954: Records for tornado events only.
# 2) 1955 through 1995: Records for tornado, thunderstorm wind and hail events.
# 3) 1996 to present: Records for 48 NWS Directive 10-1605 event types NWS 
#    Storm Documentation, Table 1.

nws_collection_method_fun <- function(x){ 
    if (x < 1955) { 
        colmeth <- 1
    } else if (x >= 1955 & x < 1996) {
        colmeth <- 2
    } else if (x >= 1996) {
        colmeth <- 3
    } else {
        colmeth <- 0
    }
    return(colmeth)
}

stormdata$colmeth <- sapply(stormdata$year, nws_collection_method_fun )


#-------------------------------------------------------------------------------
# Dollar multiplier for damages
#-------------------------------------------------------------------------------

# correction of Nappa valley outlier - recode multiplier
stormdata$propdmgexp[(stormdata$bgn_date==as.Date("2006-01-01") & 
                          (round(stormdata$propdmg,0)==115) & 
                                    stormdata$propdmgexp=="B")] <- "M"

# Multipliers function in dollar amounts
dmgexp_multiplier_fun <- function(x) {
    # where B = US Billions, 10^9
    classifiers <- c("?","-","+",NA,"0","1","2","3","4","5","6","7","8","9",
                        "H","K","M","B")
    multipliers <- c(NA,NA,NA,NA,NA,10^1,10^2,10^3,10^4,10^5,10^6,10^7,10^8,
                        10^9,10^2,10^3,10^6,10^9)
    m <- multipliers[match(x,classifiers)]
    return(m)
}

# apply dollar multiplier to damage expense
stormdata$propdmgmult <- dmgexp_multiplier_fun(stormdata$propdmgexp)
stormdata$cropdmgmult <- dmgexp_multiplier_fun(stormdata$cropdmgexp)


#-------------------------------------------------------------------------------
# Map reported events to NWS Directive 10-1605 standardised event types
#-------------------------------------------------------------------------------

# "Astronomical Low Tide"
stormdata$event[stormdata$evtype %in% c("astronomical low tide","blow-out tide", 
                "blow-out tides")] <- events[1]

# "Avalanche"
stormdata$event[stormdata$evtype %in% c("avalance", "avalanche", "landslide", 
"landslides", "landslide/urban flood", "landslump", "mudslide", "mud slide", 
"mudslides", "mud slides", "mud slides urban flooding", "rock slide", 
"urban flood landslide", "mud/rock slide", "mudslide/landslide")] <- events[2]

# "Blizzard"
stormdata$event[stormdata$evtype %in% c("blizzard", "blizzard/winter storm", 
"high wind/blizzard", "ground blizzard", "blizzard and extreme wind chil", 
"blizzard and heavy snow", "blizzard/freezing rain", "blizzard/heavy snow", 
"blizzard/high wind", "blizzard summary", "blizzard weather", 
"high wind/ blizzard", "high wind/blizzard/freezing ra", 
"high wind/wind chill/blizzard", "blowing snow", 
"blowing snow & extreme wind ch", "blowing snow- extreme wind chi", 
"blowing snow/extreme wind chil", "snowstorm", 
"high wind and heavy snow")] <- events[3]

# "Coastal Flood"
stormdata$event[stormdata$evtype %in% c("beach erosion", "beach erosin", 
"beach erosion/coastal flood", "beach flood", "coastal erosion", "coastal flood", 
"coastal flooding", "coastalflood", "erosion/cstl flood", 
"coastal flooding/erosion", "coastal flooding/erosion", "coastalstorm", 
"coastal storm", "coastal surge", "high winds/coastal flood", " coastal flood", 
"coastal/tidal flood", "cstl flooding/erosion")] <- events[4]

# "Cold/Wind Chill"
stormdata$event[stormdata$evtype %in% c("cold", "cold air tornado", 
"cold and snow", "cold and wet conditions", "cold temperature", "cold wave", 
"cold weather", "cold/wind chill", "cold/winds", "cool and wet", 
"unseasonable cold", "unseasonably cold", "low temperature","cold and frost", 
"cold temperatures", "cold wind chill temperatures", "cool spell", 
"snow- high wind- wind chill", "wind chill", "wind chill/high wind", 
"high wind/low wind chill", "high winds and wind chill", "high wind/wind chill", 
"low wind chill", "record snow/cold", "record snowfall", "record winter snow", 
"prolong cold", "prolong cold/snow", "unseasonably cool", 
"unseasonably cool & wet")] <- events[5]

# "Debris Flow" - classifier absent from current database data cut
# stormdata$event[stormdata$evtype %in% c("")] <- events[6]

# "Dense Fog"
stormdata$event[stormdata$evtype %in% c("fog", "fog and cold temperatures", 
"dense fog", "patchy dense fog")] <- events[7]

# "Dense Smoke"
stormdata$event[stormdata$evtype %in% c("dense smoke", "smoke")] <- events[8]

# "Drought"
stormdata$event[stormdata$evtype %in% c("drought", "drought/excessive heat", 
"heat wave drought", "abnormally dry", "below normal precipitation", 
"record dry month", "record dryness", "dry", "dry conditions", "dry hot weather", 
"dryness", "dry pattern", "dry spell", "dry weather", "excessive heat/drought", 
"excessively dry", "heat drought", "heat/drought", "very dry", 
"unseasonably dry", "hot and dry", "hot/dry pattern","driest month", 
"snow drought", "warm dry conditions")] <- events[9]

# "Dust Devil"
stormdata$event[stormdata$evtype %in% c("dust devel", "dust devil", 
"dust devil waterspout")] <- events[10] 

# "Dust Storm"
stormdata$event[stormdata$evtype %in% c("blowing dust", "dust storm", 
"duststorm", "dust storm/high winds", "saharan dust", 
"high winds dust storm")] <- events[11] 

# "Excessive Heat"
stormdata$event[stormdata$evtype %in% c("extreme heat", "excessive heat", 
"record/excessive heat", "very warm", "unseasonably hot", "unusually warm", 
"unusual/record warmth", "unusual warmth", "unseasonably warm & wet", 
"unseasonably warm year", "unseasonably warm/wet", "record warm", 
"record warm temps.", "record warmth", "record heat wave", 
"record high temperature", "record high temperatures", "hot pattern", 
"hot spell", "hot weather", "high temperature record", "abnormal warmth", 
"prolong warmth")] <- events[12]

# "Extreme Cold/Wind Chill"
stormdata$event[stormdata$evtype %in% c("extended cold", "extreme cold", 
"extreme cold/wind chill", "extreme windchill", "extreme wind chill", 
"record cold", "record snow", "agricultural freeze", "hard freeze", 
"excessive cold", "extreme/record cold", "extreme wind chill/blowing sno", 
"extreme wind chills", "extreme windchill temperatures", "severe cold", 
"hyperthermia/exposure", "hypothermia", "hypothermia/exposure", "recordcold", 
"record cold and high wind", "record cold/frost", "record cool", "record  cold", 
"unusually cold", "unseasonal low temp", "low temperature record")] <- events[13]

# "Flash Flood"
stormdata$event[stormdata$evtype %in% c("dam break", " flash flood", 
"flash flood", "flash flood from ice jams", "flash flood - heavy rain", 
"flash flooding/thunderstorm wi", "flash flood/landslide", 
"flash flood landslides", "flash flood winds", "flash flood/", 
"flash flood/ flood", "flash flood/ street", "flash flood/flood", 
"flash flooding", "flash flooding/flood", "flash floods", "flash floooding", 
"flood flash", "flood flood/flash", "flood/flash", "flood/flash flood", 
"flood/flash flooding", "flood/flash/flood", "flood/flashflood", 
"local flash flood","rapidly rising water", "flash flood/heavy rain", 
"thunderstorm winds/flash flood", "dam failure")] <- events[14]

# "Flood"
stormdata$event[stormdata$evtype %in% c("flooding", "floods", 
"river and stream flood", "river flood", "river flooding", "major flood", 
"flood & heavy rain", "flood/rain/winds", "rural flood", "small stream flood", 
"snowmelt flooding", "urban and small", "urban and small stream floodin", 
"urban flood", "urban flooding", "urban floods", "urban small", 
"urban/small stream", "urban/small stream flood", "urban/sml stream fld", 
"flood", "flooding/heavy rain", "flood/river flood", "ice floes", "ice jam", 
"ice jam flooding", "ice jam flood (minor", "minor flooding", "breakup flooding", 
"severe turbulence", "flood/rain/wind", "flood/strong wind", "flood watch/", 
"hail flooding", "urban and small stream flood", "urban/small flooding", 
"urban small stream flood", "urban/small streamflood", 
"urban/small stream flooding", "urban/small strm fldg", "urban/sml stream fldg", 
"urban/street flooding", "local flood", "minor flood", "high winds/flooding", 
"highway flooding", "heavy rain/flooding", "heavy rain/urban flood", 
"stream flooding", "street flood", "street flooding", 
"small stream and urban flood", "small stream and urban floodin", 
"small stream flooding", "small stream urban flood", "small stream/urban flood", 
"sml stream fld", "urban and small stream", "urban/small", "small stream", 
"small stream and", "urban/small stream  flood", 
"urban/small stream  flood")] <- events[15] 

# "Frost/Freeze"
stormdata$event[stormdata$evtype %in% c("freeze","damaging freeze", "frost", 
"early frost", "freezing spray", "frost/freeze", "frost\\freeze", "early freeze", 
"first frost")] <- events[16]

# "Funnel Cloud"
stormdata$event[stormdata$evtype %in% c("funnel", "funnel cloud.", 
"funnel clouds", "funnels", "funnel cloud", "cold air funnel", 
"cold air funnels", "funnel cloud/hail")] <- events[17]

# "Freezing Fog"
stormdata$event[stormdata$evtype %in% c("freezing fog", "glaze", "glaze ice", 
"glaze/ice storm", "ice fog")] <- events[18]

# "Hail"
stormdata$event[stormdata$evtype %in% c("hail", "hail damage", "hail/wind", 
"hail/winds", "hail 0.75", "hail 0.88", "hail 075", "hail 088", "hail 1.00", 
"hail 1.75", "hail 1.75)", "hail 100", "hail 125", "hail 150", "hail 175", 
"hail 200", "hail 225", "hail 275", "hail 450", "hail 75", "hail 80", "hail 88", 
"hail(0.75)", "hailstorm", "small hail", "non severe hail", "deep hail", 
"hail aloft", "hail/icy roads", "hail storm" , "hailstorms", 
"ice pellets")] <- events[19]

# "Heat"
stormdata$event[stormdata$evtype %in% c("heat", "record heat", 
"unseasonably warm", "unseasonably warm and dry", "warm weather", "heat wave", 
"heat waves")] <- events[20]

# "Heavy Rain"
stormdata$event[stormdata$evtype %in% c("excessive rainfall", 
"excessive wetness", "heavy precipitation", "heavy rain", "heavy rain and flood", 
"heavy rain/lightning", "heavy rains/flooding", "heavy rain/small stream urban", 
"heavy rain/snow", "heavy rainfall", "heavy rains", "hvy rain", 
"heavy rain/severe weather","torrential rainfall", "rain/wind", 
"record rainfall", "rain", "rain/snow", "rainstorm", "heavy shower", 
"unseasonal rain", "heavy mix", "abnormally wet", "prolonged rain", 
"rain and wind", "rain damage", "rain (heavy)", "record precipitation", 
"excessive precipitation", "excessive rain", "extremely wet", "torrential rain", 
"heavy precipatation", "heavy rain and wind", "heavy rain effects", 
"heavy rain/mudslides/flood", "heavy rain; urban flood winds;", 
"heavy rain/wind", "heavy showers", "unseasonably wet", "wet month", 
"wet weather", "wet year", "record/excessive rainfall", "record low rainfall", 
"locally heavy rain", "early rain")] <- events[21]

# "Heavy Snow"
stormdata$event[stormdata$evtype %in% c("excessive snow", "heavy snow", 
"heavy snow and high winds", "heavy snow and strong winds", 
"heavy snow/blizzard", "heavy snow/blizzard/avalanche", 
"heavy snow/freezing rain", "heavy snow/high winds & flood", "heavy snow/ice", 
"heavy snowpack", "heavy snow shower", "heavy snow squalls", "heavy snow-squalls", 
"heavy snow/squalls", "heavy snow/wind", "heavy snow/winter storm", 
"heavy snow and", "heavy snow andblowing snow", "heavy snow and ice", 
"heavy snow and ice storm", "heavy snow/blowing snow", "heavy snowfreezing rain", 
"heavy snow/high", "heavy snow/high wind", "heavy snow/high winds", 
"heavy snow/high winds/freezing", "heavy snow & ice", "heavy snow/ice storm", 
"heavy snow/sleet", "heavy wet snow", "snow advisory", "drifting snow", 
"near record snow", "record may snow", 
"heavy snow   freezing rain")] <- events[22]

# "High Surf"
stormdata$event[stormdata$evtype %in% c("high surf","   high surf advisory", 
"heavy surf", "heavy surf and wind", "heavy surf coastal flooding", 
"heavy surf/high surf", "tidal flooding", "tidal flood", "rogue wave", 
"rough seas", "rough surf", "hazardous surf", "heavy rain/high surf", 
"astronomical high tide", "high surf advisories", "high surf advisory", 
"highswells", "high wind and high tides", "high  swells")] <- events[23]

# "High Wind" 
stormdata$event[stormdata$evtype %in% c("high wind (g40)", "high wind 48", 
"high wind 63", "high wind 70", "high winds", "high winds 55", "high winds 57", 
"high winds 58", "high winds 63","high winds 66", "high winds 67", 
"high winds 73","high winds 76", "high winds 80", "high winds 82", 
"high winds/cold", "strong winds", "tropical storm", "high wind", 
"high wind damage", "high wind/heavy snow", "high  winds", "high winds/", 
"high winds/heavy rain", "high winds heavy rains", "high winds/snow", 
"storm force winds", "wake low wind", " wind", "wind advisory", "wind gusts" , 
"strong wind gust")] <- events[24]

# "Hurricane (Typhoon)"
stormdata$event[stormdata$evtype %in% c("hurricane", "hurricane edouard", 
"hurricane emily", "hurricane erin", "hurricane felix", "hurricane gordon", 
"hurricane opal", "hurricane opal/high winds", "hurricane/typhoon", "typhoon", 
"remnants of floyd")] <- events[25]

# "Ice Storm"
stormdata$event[stormdata$evtype %in% c("ice storm", "ice storm/flash flood", 
"snow and ice storm", "snow/ice storm", "ice storm and snow", 
"icestorm/blizzard")] <- events[26]

# "Lake-Effect Snow" 
stormdata$event[stormdata$evtype %in% c("lake effect snow", "lake-effect snow", 
"heavy lake snow")] <- events[27]

# "Lakeshore Flood" 
stormdata$event[stormdata$evtype %in% c("lake flood", 
"lakeshore flood")] <- events[28]

# "Lightning"
stormdata$event[stormdata$evtype %in% c("lighting", "lightning  wauseon", 
"lightning damage", "lightning fire", "lightning injury", 
"lightning thunderstorm winds", "lightning thunderstorm windss", "lightning.", 
"ligntning", "lightning", "lightning and heavy rain", 
"lightning and thunderstorm win", "lightning/heavy rain", " lightning", 
"lightning and winds")] <- events[29]

# "Marine Hail"
stormdata$event[stormdata$evtype %in% c("marine hail")] <- events[30]

# "Marine High Wind"
stormdata$event[stormdata$evtype %in% c("marine high wind")] <- events[31]

# "Marine Strong Wind"
stormdata$event[stormdata$evtype %in% c("marine strong wind")] <- events[32]

# "Marine Thunderstorm Wind"
stormdata$event[stormdata$evtype %in% c("marine thunderstorm wind",
"marine tstm wind")] <- events[33]

# "Rip Current"
stormdata$event[stormdata$evtype %in% c("rip current", "rip currents/heavy surf", 
"rip currents", "drowning", "rip currents heavy surf")] <- events[34]

# "Seiche"
stormdata$event[stormdata$evtype %in% c("seiche")] <- events[35]

# "Sleet"
stormdata$event[stormdata$evtype %in% c("sleet", "sleet/ice storm", 
"mixed precip", "mixed precipitation", "light freezing rain", 
"freezing drizzle and freezing", "freezing rain and sleet", 
"freezing rain and snow", "freezing rain sleet and", 
"freezing rain sleet and light", "freezing rain", "freezing drizzle", 
"freezing rain/sleet", "freezing rain/snow", "snow freezing rain", 
"snow/freezing rain", "sleet & freezing rain", "sleet/freezing rain", 
"sleet/rain/snow", "sleet/snow" , "sleet storm", "snow/rain", "snow/rain/sleet", 
"snow sleet", "snow/sleet/rain", "snow and sleet", "wet snow")] <- events[36]

# "Storm Surge/Tide"
stormdata$event[stormdata$evtype %in% c("storm surge", "storm surge/tide", 
"high seas", "high swells", "high tides", "high water", "high waves", 
"high wind and seas", "high wind/seas", "heavy seas", 
"hurricane-generated swells", "heavy swells", "marine accident", "marine mishap", 
"wind and wave")] <- events[37]

# "Strong Wind"
stormdata$event[stormdata$evtype %in% c("wind storm", "strong wind", "wind", 
"winds", "wnd", "non-severe wind damage", "non-tstm wind", "non tstm wind", 
"wind damage", "wind/hail", "gusty wind", "gusty wind/hail", 
"gusty wind/hvy rain", "gusty wind/rain", "gusty winds", "gusty lake wind", 
"gusty thunderstorm wind", "gusty thunderstorm winds", "gradient wind", 
"gradient winds", "heatburst")] <- events[38]

# "Thunderstorm Wind"
stormdata$event[stormdata$evtype %in% c("gustnado", "gustnado and", "microburst", 
"microburst winds", "wet microburst", "wet micoburst", "dry microburst", 
"dry microburst 50", "dry microburst 53", "dry microburst 58", 
"dry microburst 61", "dry microburst 84", "dry microburst winds", "downburst", 
"dry mircoburst winds", "thunderstorm hail", "thunderstorm wind/awning", 
"thunderstorm wind", "thuderstorm winds", "thundeerstorm winds", 
"thunderestorm winds", "thunderstorm", "thunderstorm  winds", 
"thunderstorm damage", "thunderstorm damage to", "thunderstorm w inds", 
"thunderstorm winds 2", "thunderstorm wind (g40)", "thunderstorm winds 13", 
"thunderstorm wind 50", "thunderstorm wind g50", "thunderstorm winds 50", 
"thunderstorm wind g51", "thunderstorm wind g52", "thunderstorm wind 52", 
"thunderstorm winds 52", "thunderstorm winds53", "thunderstorm winds 53", 
"thunderstorm wind g55", "thunderstorm wind 56", "thunderstorm wind 59", 
"thunderstorm wind 59 mph", "thunderstorm wind 59 mph.", 
"thunderstorm wind 60 mph", "thunderstorm winds      le cen", 
"thunderstorm winds 60", "thunderstorm winds g60", "thunderstorm winds 61", 
"thunderstorm winds 62", "thunderstorm winds 63 mph", "thunderstorm wind 65 mph", 
"thunderstorm wind 65mph", "thunderstorm wind 69", "thunderstorm wind 98 mph", 
"thunderstorm wind g60", "thunderstorm wind g61", "thunderstorm wind trees", 
"thunderstorm wind.", "thunderstorm winds", "thunderstorm winds and", 
"thunderstorm winds.", "thunderstorm windss", "thunderstorm wins", 
"thunderstorms", "thunderstorms wind", "thunderstorms winds", "thunderstormw", 
"thunderstormw 50", "thunderstormw winds", "thunderstormwinds", 
"thunderstrom wind", "thunderstrom winds", "thundertorm winds", 
"thundertsorm wind", "thundestorm winds", "thunerstorm winds", "tstm", 
" tstm wind", "tstm wind and lightning", "tstm wind damage", " tstm wind (g45)", 
"tstm wind", "tstm wind (g35)", "tstm wind (g40)", "tstm wind 40", 
"tstm wind (41)", "tstm wind  (g45)","tstm wind (g45)", "tstm wind 45", 
"tstm wind g45", "tstm wind 50", "tstm wind 51", "tstm wind 52", "tstm wind 55", 
"tstm wind g58", "tstm wind 65)", "tstm winds", "tstm wnd", "tstmw", 
"tunderstorm wind", "severe thunderstorm", "thunderstorm wind/hail", 
"tstm wind/hail", "severe thunderstorm winds", "severe thunderstorms", 
"thunderstorm wind/lightning", "thunderstorm winds/ flood", 
"thunderstorm winds/flooding", "thunderstorm winds/funnel clou", 
"thunderstorm windshail", "thunderstorm winds hail", "thunderstorm winds/hail", 
"thunderstorm winds lightning", "thunderstorm wind/ tree", 
"thunderstorm wind/ trees", "thundersnow", "whirlwind", 
"thunderstorm winds funnel clou", "thunderstorm winds g", 
"thunderstorm winds/ hail", "thunderstorm winds heavy rain", 
"thunderstorm winds/heavy rain", "thunderstorm winds small strea", 
"thunderstorm winds urban flood", "tstm heavy rain", "thundersnow shower", 
"metro storm, may 26", "downburst winds")] <- events[39]

# "Tornado"
stormdata$event[stormdata$evtype %in% c("landspout", "tornado f0", "tornado f1", 
"tornado f2", "tornado f3", "tornadoes", "tornadoes", "tornado", 
"tornadoes, tstm wind, hail", "torndao", "tornado debris", "tornados", 
"tornado/waterspout", "rotating wall cloud", "wall cloud", 
"wall cloud/funnel cloud", "large wall cloud")] <- events[40]

# "Tropical Depression"
stormdata$event[stormdata$evtype %in% c("tropical depression")] <- events[41]

# "Tropical Storm"
stormdata$event[stormdata$evtype %in% c("tropical storm alberto", 
"tropical storm dean", "tropical storm gordon", 
"tropical storm jerry")] <- events[42]

# "Tsunami"
stormdata$event[stormdata$evtype %in% c("tsunami")] <- events[43]

# "Volcanic Ash"
stormdata$event[stormdata$evtype %in% c("volcanic ash", "vog", 
"volcanic ashfall", "volcanic ash plume", "volcanic eruption")] <- events[44]

# "Waterspout"
stormdata$event[stormdata$evtype %in% c("water spout", "waterspout", 
"waterspout-", "waterspout/", "waterspouts", "wayterspout", "waterspout tornado", 
"waterspout-tornado", "waterspout/tornado", "waterspout/ tornado", " waterspout", 
"waterspout funnel cloud")] <- events[45]

# "Wildfire" 
stormdata$event[stormdata$evtype %in% c("brush fire", "brush fires", 
"forest fires", "grass fires", "wild/forest fire", "wild/forest fires", 
"wildfire", "wildfires", "wild fires")] <- events[46]

# "Winter Storm"
stormdata$event[stormdata$evtype %in% c("winter storms", "winter storm", 
"winter storm high winds", "winter storms", "winter storm/high wind", 
"winter storm/high winds")] <- events[47]

# "Winter Weather"
stormdata$event[stormdata$evtype %in% c("bitter wind chill", 
"bitter wind chill temperatures", "falling snow/ice", "ice", "black ice",
"ice on road", "ice roads", "icy roads", "ice and snow", "ice/strong winds", 
"late season snow", "late freeze", "late season hail", "late season snowfall", 
"late-season snowfall", "late snow", "light snow", "light snowfall", "snow", 
"snow accumulation", "snow and heavy snow", "snow and ice", "snow/ bitter cold", 
"snow/blowing snow", "snow/cold", "snow/heavy snow", "snow/high winds", 
"snow/ice", "snow/ ice", "snow/sleet", "snow/sleet/freezing rain", "snow squall", 
"snow squalls", "winter weather", "winter weather mix", "winter weather/mix", 
"wintry mix", "moderate snow", "moderate snowfall", "light snow and sleet", 
"light snow/flurries", "light snow/freezing precip", "mountain snows", 
"winter mix", "wintery mix", "seasonal snowfall", "snow showers", 
"snowfall record", "snow and cold", "snow and wind", "snow\\cold", "first snow", 
"early snow", "early snowfall", "accumulated snowfall", "ice/snow", "patchy ice", 
"unusually late snow")] <- events[48]

# Additional category to mop up miscellaneous/ unclassifiable events
stormdata$event[stormdata$evtype %in% c("apache county", "excessive", 
"other","?","high", "record high", "record temperature", "record temperatures", 
"temperature record", "monthly precipitation", "monthly rainfall", 
"monthly snowfall", "monthly temperature", "southeast", "record low", "none", 
"lack of snow", "normal precipitation", "northern lights", "no severe weather", 
"red flag criteria", "red flag fire wx", "mild and dry pattern", 
"mild/dry pattern", "mild pattern")] <- "other"

# summary
stormdata$event[stormdata$evtype %in% c("summary august 10", "summary august 11", 
"summary august 17", "summary august 21", "summary august 2-3", 
"summary august 28", "summary august 4", "summary august 7", "summary august 9", 
"summary jan 17", "summary july 23-24", "summary june 18-19", "summary june 5-6", 
"summary june 6", "summary: nov. 16", "summary: nov. 6-7", "summary: oct. 20-21", 
"summary: october 31", "summary of april 12", "summary of april 13", 
"summary of april 21", "summary of april 27", "summary of april 3rd", 
"summary of august 1", "summary of july 11", "summary of july 2", 
"summary of july 22", "summary of july 26", "summary of july 29", 
"summary of july 3", "summary of june 10", "summary of june 11", 
"summary of june 12", "summary of june 13", "summary of june 15", 
"summary of june 16", "summary of june 18", "summary of june 23", 
"summary of june 24", "summary of june 3", "summary of june 30", 
"summary of june 4", "summary of june 6", "summary of march 14", 
"summary of march 23", "summary of march 24", "summary of march 24-25", 
"summary of march 27", "summary of march 29", "summary of may 10", 
"summary of may 13", "summary of may 14", "summary of may 22", 
"summary of may 22 am", "summary of may 22 pm", "summary of may 26 am", 
"summary of may 26 pm", "summary of may 31 am", "summary of may 31 pm", 
"summary of may 9-10", "summary: sept. 18", "summary sept. 25-26", 
"summary september 20", "summary september 23", "summary september 3", 
"summary september 4")] <- "summary"


#-------------------------------------------------------------------------------
# Aggregate fatalities by severe weather event and NWS collection method.
#-------------------------------------------------------------------------------

eventTotFatal <- aggregate(fatalities ~ event + colmeth, 
                             data=stormdata[stormdata$fatalities > 0,],
                               FUN=sum, na.rm=TRUE, na.action = na.omit)

# sort fatalities-event in descending order
eventTotFatalS <- eventTotFatal[order(-eventTotFatal$fatalities, 
                                         eventTotFatal$event),]

# subset the top 10 events for fatalities for each NWS collection period
eventTotFatalSmeth1 <- head((eventTotFatalS[eventTotFatalS$colmeth==1,]),10)
eventTotFatalSmeth2 <- head((eventTotFatalS[eventTotFatalS$colmeth==2,]),10)
eventTotFatalSmeth3 <- head((eventTotFatalS[eventTotFatalS$colmeth==3,]),10)

# Format injury counts for report output
eventTotFatalSmeth1$fatalities_fmt <- sapply(eventTotFatalSmeth1$fatalities, 
                                                fmt_rounded_big_mark_fun)
eventTotFatalSmeth2$fatalities_fmt <- sapply(eventTotFatalSmeth2$fatalities, 
                                                fmt_rounded_big_mark_fun)
eventTotFatalSmeth3$fatalities_fmt <- sapply(eventTotFatalSmeth3$fatalities, 
                                                fmt_rounded_big_mark_fun)


#-------------------------------------------------------------------------------
# Aggregate injuries by severe weather event and NWS collection method.
#-------------------------------------------------------------------------------

eventTotInjuries <- aggregate(injuries ~ event + colmeth, 
                                data=stormdata[stormdata$injuries > 0,], 
                                  FUN=sum, na.rm=TRUE, na.action = na.omit)

# sort fatalities-event in descending order
eventTotInjuriesS <- eventTotInjuries[order(-eventTotInjuries$injuries, 
                                                eventTotInjuries$event),]

# Subset the top 10 events for injuries for each NWS collection period
eventTotInjuriesSmeth1 <- head((eventTotInjuriesS[eventTotInjuriesS$colmeth==1,]),10)
eventTotInjuriesSmeth2 <- head((eventTotInjuriesS[eventTotInjuriesS$colmeth==2,]),10)
eventTotInjuriesSmeth3 <- head((eventTotInjuriesS[eventTotInjuriesS$colmeth==3,]),10)

# Format injury counts for report output
eventTotInjuriesSmeth1$injuries_fmt <- sapply(eventTotInjuriesSmeth1$injuries, 
                                                fmt_rounded_big_mark_fun)
eventTotInjuriesSmeth2$injuries_fmt <- sapply(eventTotInjuriesSmeth2$injuries, 
                                                fmt_rounded_big_mark_fun)
eventTotInjuriesSmeth3$injuries_fmt <- sapply(eventTotInjuriesSmeth3$injuries, 
                                                fmt_rounded_big_mark_fun)

# Apply damages multiplier
stormdata$cropdmgusd <- stormdata$cropdmg * stormdata$cropdmgmult


#-------------------------------------------------------------------------------
# Aggregate crop damages by severe weather event and NWS collection method.
#-------------------------------------------------------------------------------
eventTotCropdmgusd <- aggregate(cropdmgusd ~ event + colmeth, 
                                    data=stormdata[stormdata$cropdmgusd > 0,], 
                                        FUN=sum, na.rm=TRUE, na.action = na.omit)

# sort fatalities-event in descending order
eventTotCropdmgusdS <- eventTotCropdmgusd[order(-eventTotCropdmgusd$cropdmgusd, 
                                                    eventTotCropdmgusd$event),]


# Subset the top 10 events for crop damage for each NWS collection period
eventTotCropdmgusdSmeth1 <- head((eventTotCropdmgusdS[eventTotCropdmgusdS$colmeth==1,]),10)
eventTotCropdmgusdSmeth2 <- head((eventTotCropdmgusdS[eventTotCropdmgusdS$colmeth==2,]),10)
eventTotCropdmgusdSmeth3 <- head((eventTotCropdmgusdS[eventTotCropdmgusdS$colmeth==3,]),10)

# Format crop damage in USD for report output
eventTotCropdmgusdSmeth1$cropdmgusd_fmt <- sapply(eventTotCropdmgusdSmeth1$cropdmgusd, 
                                                  fmt_rounded_big_mark_dollar_fun)
eventTotCropdmgusdSmeth2$cropdmgusd_fmt <- sapply(eventTotCropdmgusdSmeth2$cropdmgusd, 
                                                  fmt_rounded_big_mark_dollar_fun)
eventTotCropdmgusdSmeth3$cropdmgusd_fmt <- sapply(eventTotCropdmgusdSmeth3$cropdmgusd, 
                                                  fmt_rounded_big_mark_dollar_fun)


# Apply damages multiplier
stormdata$propdmgusd <- stormdata$propdmg * stormdata$propdmgmult


#-------------------------------------------------------------------------------
# Aggregate property damages by severe weather event and NWS collection method.
#-------------------------------------------------------------------------------

eventTotPropdmgusd <- aggregate(propdmgusd ~ event + colmeth, 
                                    data=stormdata[stormdata$propdmgusd > 0,], 
                                        FUN=sum, na.rm=TRUE, na.action = na.omit)

# sort fatalities-event in descending order
eventTotPropdmgusdS <- eventTotPropdmgusd[order(-eventTotPropdmgusd$propdmgusd, 
                                                    eventTotPropdmgusd$event),]

# Subset the top 10 events for property damage for each NWS collection period
eventTotPropdmgusdSmeth1 <- head((eventTotPropdmgusdS[eventTotPropdmgusdS$colmeth==1,]),10)
eventTotPropdmgusdSmeth2 <- head((eventTotPropdmgusdS[eventTotPropdmgusdS$colmeth==2,]),10)
eventTotPropdmgusdSmeth3 <- head((eventTotPropdmgusdS[eventTotPropdmgusdS$colmeth==3,]),10)

# Format property damages in USD for report output
eventTotPropdmgusdSmeth1$propdmgusd_fmt <- sapply(eventTotPropdmgusdSmeth1$propdmgusd, 
                                                  fmt_rounded_big_mark_dollar_fun)
eventTotPropdmgusdSmeth2$propdmgusd_fmt <- sapply(eventTotPropdmgusdSmeth2$propdmgusd, 
                                                  fmt_rounded_big_mark_dollar_fun)
eventTotPropdmgusdSmeth3$propdmgusd_fmt <- sapply(eventTotPropdmgusdSmeth3$propdmgusd, 
                                                  fmt_rounded_big_mark_dollar_fun)


#-------------------------------------------------------------------------------
# Aggregate fatalities and injuries by weather event
#-------------------------------------------------------------------------------

eventTothealth <- aggregate(cbind(fatalities, injuries ) ~ event, data=stormdata, 
                                FUN=sum, na.rm=TRUE, na.action = na.omit)

eventTothealth$totindv <- eventTothealth$fatalities + eventTothealth$injuries

eventTothealthss <- head(eventTothealth[order(-eventTothealth$totindv, 
                        -eventTothealth$fatalities, -eventTothealth$injuries),],10)

# Format injuries, fatalities and total individuals harmed for report output
eventTothealthss$injuries_fmt <- sapply(eventTothealthss$injuries, 
                                                  fmt_rounded_big_mark_fun)
eventTothealthss$fatalities_fmt <- sapply(eventTothealthss$fatalities, 
                                                  fmt_rounded_big_mark_fun)
eventTothealthss$totindv_fmt <- sapply(eventTothealthss$totindv, 
                                                  fmt_rounded_big_mark_fun)

# reshape the dataframe to three col: event, variable, value
eventTothealthssm <- melt(eventTothealthss[1:3], id="event")


# Apply damage multipliers for crops and property
stormdata$cropdmgusd <- stormdata$cropdmg * stormdata$cropdmgmult
stormdata$propdmgusd <- stormdata$propdmg * stormdata$propdmgmult


#-------------------------------------------------------------------------------
# Aggregate fatalities and injuries by event, derive total affected individuals 
# and subset the top most harmful event types.
#-------------------------------------------------------------------------------

eventTotdamage <- aggregate(cbind(cropdmgusd, propdmgusd) ~ event, data=stormdata, 
                                FUN=sum, na.rm=TRUE, na.action = na.omit)

eventTotdamage$totusd <- eventTotdamage$cropdmgusd + eventTotdamage$propdmgusd

eventTotdamagess <- head(eventTotdamage[order(-eventTotdamage$totusd, 
                            -eventTotdamage$cropdmgusd, -eventTotdamage$propdmgusd),],10)


# Format crop, property and total damage in USD for report output
eventTotdamagess$cropdmgusd_fmt <- sapply(eventTotdamagess$cropdmgusd, 
                                                  fmt_rounded_big_mark_dollar_fun)
eventTotdamagess$propdmgusd_fmt <- sapply(eventTotdamagess$propdmgusd, 
                                                  fmt_rounded_big_mark_dollar_fun)
eventTotdamagess$totusd_fmt <- sapply(eventTotdamagess$totusd, 
                                                  fmt_rounded_big_mark_dollar_fun)

# reshape the dataframe to three col: event, variable, value
eventTodamagessssm <- melt(eventTotdamagess[1:3], id="event")


# Most harmful to health
p1 <- ggplot(eventTothealthssm, aes(x=reorder(event, value), y=value, 
            fill=factor(variable, labels = c("Fatalities","Injuries")))) + 
    labs (title = "Top 10 Most Harmful Severe Weather Events", x = "Event Type", 
          y = "Total number of affected individuals") + 
    geom_bar(stat="identity", alpha=.5) + 
    guides (fill = guide_legend (title = "Harm")) + 
    theme (axis.text.x = element_text (colour = "darkblue", angle = 90, hjust = 1),
           axis.text.y = element_text (colour = "darkblue"),
           axis.title = element_text (size = 12, colour = "black"), 
           title = element_text (size = 14, colour = "black", face = "bold"), 
           legend.text = element_text (size = 10, colour = "black"))


# Most harmful to crops and property
p2 <- ggplot(eventTodamagessssm, aes(x=reorder(event, value), y=value, 
            fill=factor(variable, labels = c("Crop","Property")))) + 
    labs (title = "Top 10 Economically Coslty Severe Weather Events", x = "Event Type", 
          y = "Total damage in USD") + geom_bar(stat="identity", alpha=.5) + 
    guides (fill = guide_legend (title = "Damage")) +
    theme (axis.text.x = element_text (colour = "darkblue", angle = 90, hjust = 1),
           axis.text.y = element_text (colour = "darkblue"),
           axis.title = element_text (size = 12, colour = "black"), 
           title = element_text (size = 14, colour = "black", face = "bold"), 
           legend.text = element_text (size = 10, colour = "black"))


#-------------------------------------------------------------------------------
# Export Results
#-------------------------------------------------------------------------------
# Export Top 10 most harmful event types for population health from 1950 through 1954
tmp <- cbind(eventTotFatalSmeth1$event, eventTotFatalSmeth1$fatalities_fmt, 
                eventTotInjuriesSmeth1$event, eventTotInjuriesSmeth1$injuries_fmt)
colnames(tmp) <- c("Event","Total_No_Fatalities","Event","Total_No_Injured")

filename<-"1950_1954_Top10_HarmfulEvents_PopulationHealth.txt"
write.table(cbind(Rank = 1:10,tmp),file=filename,sep="\t",quote=FALSE,row.names=FALSE,col.names=TRUE)


# Export Top 10 most harmful event types for population health from 1955 through 1995
tmp <- cbind(eventTotFatalSmeth2$event, eventTotFatalSmeth2$fatalities_fmt, 
eventTotInjuriesSmeth2$event, eventTotInjuriesSmeth2$injuries_fmt)
colnames(tmp) <- c("Event","Total_No_Fatalities","Event","Total_No_Injured")

filename<-"1955_1995_Top10_HarmfulEvents_PopulationHealth.txt"
write.table(cbind(Rank = 1:10,tmp),file=filename,sep="\t",quote=FALSE,row.names=FALSE,col.names=TRUE)


# Export Top 10 most harmful event types for population health from 1996 to 2011
tmp <- cbind(eventTotFatalSmeth3$event, eventTotFatalSmeth3$fatalities_fmt, 
                eventTotInjuriesSmeth3$event, eventTotInjuriesSmeth3$injuries_fmt)
colnames(tmp) <- c("Event","Total_No_Fatalities","Event","Total_No_Injured")

filename<-"1996_2011_Top10_HarmfulEvents_PopulationHealth.txt"
write.table(cbind(Rank = 1:10,tmp),file=filename,sep="\t",quote=FALSE,row.names=FALSE,col.names=TRUE)



# Export Top 10 most harmful event types for economic loss from 1950 through 1954
tmp <- cbind(eventTotCropdmgusdSmeth1$event, eventTotCropdmgusdSmeth1$cropdmgusd_fmt, 
              eventTotPropdmgusdSmeth1$event, eventTotPropdmgusdSmeth1$propdmgusd_fmt)
colnames(tmp) <- c("Event","Total_Crop_Damages_USD")

filename<-"1950_1954_Top10_HarmfulEvents_Economically.txt"
write.table(cbind(Rank = 1:10,tmp),file=filename,sep="\t",quote=FALSE,row.names=FALSE,col.names=TRUE)


# Export Top 10 most harmful event types for economic loss from 1955 through 1995
tmp <- cbind(eventTotCropdmgusdSmeth2$event, eventTotCropdmgusdSmeth2$cropdmgusd_fmt, 
              eventTotPropdmgusdSmeth2$event, eventTotPropdmgusdSmeth2$propdmgusd_fmt)
colnames(tmp) <- c("Event","Total_Crop_Damages_USD","Event","Total_Property_Damages_USD")

filename<-"1955_1995_Top10_HarmfulEvents_Economically.txt"
write.table(cbind(Rank = 1:10,tmp),file=filename,sep="\t",quote=FALSE,row.names=FALSE,col.names=TRUE)


# Export Top 10 most harmful event types for economic loss from 1996 to 2011
tmp <- cbind(eventTotCropdmgusdSmeth3$event, eventTotCropdmgusdSmeth3$cropdmgusd_fmt, 
               eventTotPropdmgusdSmeth3$event, eventTotPropdmgusdSmeth3$propdmgusd_fmt)
colnames(tmp) <- c("Event","Total_Crop_Damages_USD","Event","Total_Property_Damages_USD")

filename<-"1996_2011_Top10_HarmfulEvents_Economically.txt"
write.table(cbind(Rank = 1:10,tmp),file=filename,sep="\t",quote=FALSE,row.names=FALSE,col.names=TRUE)



# All NWS collection periods combined

# Export Top 10 most harmful event types for population health from 1950 to 2011 
tmp <- cbind(eventTothealthss$event, eventTothealthss$fatalities_fmt, 
                eventTothealthss$injuries_fmt, eventTothealthss$totindv_fmt)
colnames(tmp) <- c("Event","Total_No_Fatalities","Event","Total_No_Injured")

filename<-"1950_2011_Top10_HarmfulEvents_PopulationHealth.txt"
write.table(cbind(Rank = 1:10,tmp),file=filename,sep="\t",quote=FALSE,row.names=FALSE,col.names=TRUE)


# Export Top 10 most harmful event types economically 1950 to 2011
tmp <- cbind(eventTotdamagess$event, eventTotdamagess$cropdmgusd_fmt, 
             eventTotdamagess$propdmgusd_fmt, eventTotdamagess$totusd_fmt)
colnames(tmp) <- c("Event","Total_Crop_Damages_USD","Event","Total_Property_Damages_USD")

filename<-"1950_2011_Top10_HarmfulEvents_Economically.txt"
write.table(cbind(Rank = 1:10,tmp),file=filename,sep="\t",quote=FALSE,row.names=FALSE,col.names=TRUE)


#-------------------------------------------------------------------------------
# Output plots to file
#-------------------------------------------------------------------------------
png(file = "1950_2011_Top10_HarmfulEvents_PersonalHealth.png", width = 650, height = 650)
print(p1)
dev.off()

png(file = "1950_2011_Top10_HarmfulEvents_Economically.png", width = 650, height = 650)
print(p2)
dev.off()


#-------------------------------------------------------------------------------
# Clear down workspace
#-------------------------------------------------------------------------------
rm(list=ls())
