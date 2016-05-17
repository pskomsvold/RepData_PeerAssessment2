##Download the data
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
              "repdata_Fdata_StormData.csv.bz2")
###date/time downloaded
date()

##Load the data
data <- read.csv(bzfile("repdata_Fdata_StormData.csv.bz2"))

###Load packages
library(ggplot2)
library(knitr)
library(reshape2)

##Create copy of original data
originaldata <- data
data$EVTYPE <- tolower(data$EVTYPE)
##Cleanup/normalize EVTYPE
###Avalanche
data[grep("avalance", data$EVTYPE), "EVTYPE"] <- "avalanche"
###Thunderstorm wind
data[grepl("wind", data$EVTYPE) & !grepl("marine", data$EVTYPE) & 
       grepl("thunderstorm|tunderstorm|tunderstrom|thunderstrom|thundertsorm", data$EVTYPE), 
     "EVTYPE"] <- "thunderstorm wind"
data[grepl("tstm|thunderstorm", data$EVTYPE) & !grepl("marine", data$EVTYPE), "EVTYPE"] <- "thunderstorm wind"
data[grepl("thunderstorm", data$EVTYPE) & grepl("wins|w inds|damage|severe", data$EVTYPE), "EVTYPE"] <- "thunderstorm wind"
data[grep("thunderstormw", data$EVTYPE), "EVTYPE"] <- "thunderstorm wind"
###Blow-out tides
data[grep("blow-out tide", data$EVTYPE), "EVTYPE"] <- "blow-out tides"
###High tides
data[grep("high tide", data$EVTYPE), "EVTYPE"] <- "high tides"
data[grepl("high", data$EVTYPE) & grepl("water|seas", data$EVTYPE), "EVTYPE"] <- "high tides"
###High surf
data[grep("surf|rogue wave", data$EVTYPE), "EVTYPE"] <- "high surf"
data[grepl("high|heavy|rough", data$EVTYPE) & grepl("seas|swells|waves", data$EVTYPE), "EVTYPE"] <- "high surf"
###Blizzard
data[grep("blizzard", data$EVTYPE), "EVTYPE"] <- "blizzard"
###Coastal flood
data[grepl("coastal|beach", data$EVTYPE) & grepl("flood|surge|erosion", data$EVTYPE), "EVTYPE"] <- "coastal flood"
data[grep("beach erosin", data$EVTYPE), "EVTYPE"] <- "coastal flood"
###Coastal storm
data[grep("coastalstorm", data$EVTYPE), "EVTYPE"] <- "coastal storm"
###Freezing fog
data[grepl("fog", data$EVTYPE) & grepl("cold|ice", data$EVTYPE), "EVTYPE"] <- "freezing fog"
###Dense fog
data[grep("patchy dense fog", data$EVTYPE), "EVTYPE"] <- "dense fog"
###Debris flow
data[grep("mud", data$EVTYPE), "EVTYPE"] <- "debris flow"
data[grepl("landslide|landslump|rock slide", data$EVTYPE), "EVTYPE"] <- "debris flow"
###Fog
data[grep("vog", data$EVTYPE), "EVTYPE"] <- "fog"
###Dense smoke
data[grep("smoke", data$EVTYPE), "EVTYPE"] <- "dense smoke"
###Drought
data[grep("drought", data$EVTYPE), "EVTYPE"] <- "drought"
data[grep("unseasonably dry", data$EVTYPE), "EVTYPE"] <- "drought"
data[grepl("dry", data$EVTYPE) & !grepl("microburst", data$EVTYPE), "EVTYPE"] <- "drought"
data[grep("driest month", data$EVTYPE), "EVTYPE"] <- "drought"
###Dust storm
data[grepl("dust", data$EVTYPE) & !grepl("devil|devel", data$EVTYPE), 
     "EVTYPE"] <- "dust storm"
###Dust devil
data[grep("dust devel", data$EVTYPE), "EVTYPE"] <- "dust devil"
###Excessive heat
data[grepl("heat", data$EVTYPE) & !grepl("^heat$", data$EVTYPE), "EVTYPE"] <- "excessive heat"
data[grepl("warm", data$EVTYPE) & !grepl("weather", data$EVTYPE), "EVTYPE"] <- "excessive heat"
data[grepl("record warmth|record high", data$EVTYPE), "EVTYPE"] <- "excessive heat"
data[grep("hot", data$EVTYPE), "EVTYPE"] <- "excessive heat"
data[grep("high temperature record", data$EVTYPE), "EVTYPE"] <- "excessive heat"
###Flash flood
data[grep("flash", data$EVTYPE), "EVTYPE"] <- "flash flood"
data[grep("rapidly rising water", data$EVTYPE), "EVTYPE"] <- "flash flood"
###Frost/freeze
data[grep("frost|freeze", data$EVTYPE), 
     "EVTYPE"] <- "frost/freeze"
data[grepl("ice|icy", data$EVTYPE) & grepl("black|road|roads|patchy", data$EVTYPE), "EVTYPE"] <- "frost/freeze"
data[grep("^ice$", data$EVTYPE), "EVTYPE"] <- "frost/freeze"
data[grep("freezing drizzle and freezing", data$EVTYPE), "EVTYPE"] <- "frost/freeze"
data[grep("hypothermia|hyperthermia", data$EVTYPE), "EVTYPE"] <- "frost/freeze"
data[grep("record low", data$EVTYPE), "EVTYPE"] <- "frost/freeze"
###Waterspout
data[grep("waterspout|water spout", data$EVTYPE), "EVTYPE"] <- "waterspout"
###Ice storm
data[grep("ice pellets", data$EVTYPE), "EVTYPE"] <- "ice storm"
data[grep("glaze", data$EVTYPE), "EVTYPE"] <- "ice storm"
###Funnel cloud
data[grep("funnel", data$EVTYPE), "EVTYPE"] <- "funnel cloud"
data[grep("wall cloud", data$EVTYPE), "EVTYPE"] <- "funnel cloud"
###Cold/wind chill
data[grep("^cold$", data$EVTYPE), "EVTYPE"] <- "cold/wind chill"
data[grep("^wind chill$", data$EVTYPE), "EVTYPE"] <- "cold/wind chill"
data[grep("^low wind chill$", data$EVTYPE), "EVTYPE"] <- "cold/wind chill"
data[grepl("cold", data$EVTYPE) & 
       grepl("air|weather|temperature|temperatures|wet|winds", data$EVTYPE), "EVTYPE"] <- "cold/wind chill"
data[grep("low temperature", data$EVTYPE), "EVTYPE"] <- "cold/wind chill"
###Extreme cold/wind chill
data[grepl("cold", data$EVTYPE) & 
       grepl("excessive|extreme|severe|bitter|unusually|extended|prolong|unseasonably|unseasonable|record", data$EVTYPE), "EVTYPE"] <- "extreme cold/wind chill"
data[grepl("cool", data$EVTYPE) & 
       grepl("record|low", data$EVTYPE), "EVTYPE"] <- "extreme cold/wind chill"
data[grepl("temp", data$EVTYPE) & 
       grepl("record|low|unseasonal", data$EVTYPE), "EVTYPE"] <- "extreme cold/wind chill"
###Heavy snow
data[grepl("snow", data$EVTYPE) & 
       grepl("heavy|squall|accumulation|accumulated|squalls|advisory", data$EVTYPE), "EVTYPE"] <- "heavy snow" 
 data[grep("snowfall record", data$EVTYPE), "EVTYPE"] <- "heavy snow"
data[grep("snowstorm", data$EVTYPE), "EVTYPE"] <- "heavy snow"
###Sleet
data[grepl("snow", data$EVTYPE) & grepl("rain|sleet|showers|precip", data$EVTYPE), "EVTYPE"] <- "sleet" 
data[grep("freezing rain|sleet|freezing drizzle|freezing spray", data$EVTYPE), "EVTYPE"] <- "sleet"
###Winter weather
data[grepl("snow", data$EVTYPE) & !grepl("heavy", data$EVTYPE),
     "EVTYPE"] <- "winter weather"
data[grepl("winter", data$EVTYPE) & !grepl("storm", data$EVTYPE),
     "EVTYPE"] <- "winter weather"
data[grepl("mixed|monthly|normal", data$EVTYPE) & grepl("precip", data$EVTYPE),
     "EVTYPE"] <- "winter weather"
data[grepl("wet", data$EVTYPE) & grepl("month|weather|year|unseasonably|abnormally|cool", data$EVTYPE),
     "EVTYPE"] <- "winter weather"
data[grep("wintry mix", data$EVTYPE), "EVTYPE"] <- "winter weather"
data[grep("cold wave", data$EVTYPE), "EVTYPE"] <- "winter weather"
data[grep("cool spell", data$EVTYPE), "EVTYPE"] <- "winter weather"
data[grep("unseasonably cool", data$EVTYPE), "EVTYPE"] <- "winter weather"
data[grepl("ice", data$EVTYPE) & grepl("floes|jam", data$EVTYPE),
     "EVTYPE"] <- "winter weather"
###Winter storm
data[grep("winter storm", data$EVTYPE), "EVTYPE"] <- "winter storm"
###Lakeshore flood
data[grep("lake flood", data$EVTYPE), "EVTYPE"] <- "lakeshore flood"
###Flood
data[grepl("flood", data$EVTYPE) & !grepl("^lakeshore flood$", data$EVTYPE)
   & !grepl("^flash flood$", data$EVTYPE), "EVTYPE"] <- "flood"
data[grep("fld|dam failure|dam break", data$EVTYPE), "EVTYPE"] <- "flood"
###Hail
data[grepl("hail", data$EVTYPE) & !grepl("tornadoes|tstm", data$EVTYPE), "EVTYPE"] <- "hail"
###Heavy rain
data[grepl("rain", data$EVTYPE) & !grepl("freezing|tstm", data$EVTYPE), "EVTYPE"] <- "heavy rain"
data[grepl("heavy|excessive|extremely|record", data$EVTYPE) & 
       grepl("wet|precipitation|precipatation|shower|wetness", data$EVTYPE), "EVTYPE"] <- "heavy rain"
###High wind
data[grepl("high", data$EVTYPE) & grepl("wind", data$EVTYPE)
     & !grepl("marine", data$EVTYPE), "EVTYPE"] <- "high wind"
###Strong wind
data[grepl("wind", data$EVTYPE) & !grepl("marine|^thunderstorm wind|^cold/wind chill|
                                         ^extreme cold/wind chill", data$EVTYPE), 
                                          "EVTYPE"] <- "strong wind"
data[grepl("microburst|micoburst|downburst", data$EVTYPE), "EVTYPE"] <- "strong wind"
###Hurricane/typhoon
data[grepl("hurricane", data$EVTYPE) | grepl("typhoon", data$EVTYPE), "EVTYPE"] <- "hurricane/typhoon"
###Tornado
data[grepl("tornado|torndao", data$EVTYPE), "EVTYPE"] <- "tornado"
###Lightning
data[grepl("lighting|lightning|ligntning", data$EVTYPE), "EVTYPE"] <- "lightning"
###Waterspout
data[grep("wayterspout", data$EVTYPE), "EVTYPE"] <- "waterspout"
###Tropical storm
data[grepl("tropical", data$EVTYPE) & grepl("storm", data$EVTYPE), "EVTYPE"] <- "tropical storm"
###Volcanic ash
data[grep("volcanic", data$EVTYPE), "EVTYPE"] <- "volcanic ash"
###Rip current
data[grep("rip currents", data$EVTYPE), "EVTYPE"] <- "rip current"
###Wildfire
data[grep("fire", data$EVTYPE), "EVTYPE"] <- "wildfire"
###Storm surge/tide
data[grep("^surge$", data$EVTYPE), "EVTYPE"] <- "storm sturge/tide"
data[grep("^storm surge$", data$EVTYPE), "EVTYPE"] <- "storm sturge/tide"
###Unknown/other
data[!grepl("^astronomical low tide$|^avalanche$|^blizzard$|^blow-out tides$|
^coastal storm$|^cold/wind chill$|^debris flow$|^dense fog$|^dense smoke$|^drought$|
^dust devil$|^dust storm$|^excessive heat$|^extreme cold/wind chill$|^flash flood$|
^flood|^fog$|^freezing fog$|^frost/freeze$|^funnel cloud$|^hail$|^heat$|^heavy rain|
^heavy snow$|^high surf$|^high tides$|^hurricane/typhoon$|^ice storm$|^lakeshore flood|
^lightning$|^marine high wind|^marine strong wind$|^marine thunderstorm wind$|
^rip current$|^seiche$|^sleet$|^strong wind$|^storm surge/tide$|^thunderstorm wind$|
^tropical depression$|^tropical storm$|^tsunami$|^volcanic ash$|^waterspout$|
^wildfire$|^winter storm$|^winter weather$|^tornado$", data$EVTYPE), "EVTYPE"] <- "unknown/other"

data <- subset(data, EVTYPE != "unknown/other")


##Across the United States, which types of events are most harmful with respect 
##to population health?

###Calculate total number of injuries and fatalities
injuries <- with(data, aggregate(INJURIES ~ EVTYPE, FUN = sum))
fatalities <- with(data, aggregate(FATALITIES ~ EVTYPE, FUN = sum))

###Sort data by number of injuries/fatalities
injsort <- injuries[order(-injuries$INJURIES),]
fatalsort <- fatalities[order(-fatalities$FATALITIES),]

###Subset top 10
topinj <- injsort[1:10,]
topfatal <- fatalsort[1:10,]

###Plot top 10 events with injuries 
ggplot(data = topinj, aes(x = reorder(EVTYPE, INJURIES), y = INJURIES)) + geom_bar(stat = "identity") +
  coord_flip() + xlab("Event Type") + ylab("Number of injuries") + ggtitle("Number of injuries by weather events in the US: 1950 - 2011")

###Plot top 10 events with fatalities
ggplot(data = topfatal, aes(x = reorder(EVTYPE, FATALITIES), y = FATALITIES)) + geom_bar(stat = "identity") +
  coord_flip() + xlab("Event Type") + ylab("Number of fatalities") + ggtitle("Number of fatalities by weather events in the US: 1950 - 2011")

##Across the United States, which types of events have the greatest economic consequences?


###Refactor property damage estimates
###- ? + will be excluded
data$PROPEXP[data$PROPDMGEXP == "B"] <- 1000000000
data$PROPEXP[data$PROPDMGEXP == "8"] <- 1000000000
data$PROPEXP[data$PROPDMGEXP == "7"] <- 100000000
data$PROPEXP[data$PROPDMGEXP == "M"] <- 1000000
data$PROPEXP[data$PROPDMGEXP == "m"] <- 1000000
data$PROPEXP[data$PROPDMGEXP == "6"] <- 10000000
data$PROPEXP[data$PROPDMGEXP == "5"] <- 1000000
data$PROPEXP[data$PROPDMGEXP == "4"] <- 100000
data$PROPEXP[data$PROPDMGEXP == "K"] <- 1000
data$PROPEXP[data$PROPDMGEXP == "k"] <- 1000
data$PROPEXP[data$PROPDMGEXP == "3"] <- 10000
data$PROPEXP[data$PROPDMGEXP == "H"] <- 100
data$PROPEXP[data$PROPDMGEXP == "h"] <- 100
data$PROPEXP[data$PROPDMGEXP == "2"] <- 1000
data$PROPEXP[data$PROPDMGEXP == "1"] <- 100
data$PROPEXP[data$PROPDMGEXP == "0"] <- 10
data$PROPEXP[data$PROPDMGEXP == ""] <- 1

data$PROPDMGAMT <- data$PROPEXP * data$PROPDMG

###Refactor crop damage estimates
###? will be excluded
data$CROPEXP[data$CROPDMGEXP == "B"] <- 1000000000
data$CROPEXP[data$CROPDMGEXP == "M"] <- 1000000
data$CROPEXP[data$CROPDMGEXP == "m"] <- 1000000
data$CROPEXP[data$CROPDMGEXP == "K"] <- 1000
data$CROPEXP[data$CROPDMGEXP == "k"] <- 1000
data$CROPEXP[data$CROPDMGEXP == "2"] <- 1000
data$CROPEXP[data$CROPDMGEXP == "0"] <- 10
data$CROPEXP[data$CROPDMGEXP == ""] <- 1

data$CROPDMGAMT <- data$CROPEXP * data$CROPDMG

###Create total damage amount

data$TOTDMG <- data$PROPDMGAMT + data$CROPDMGAMT


###Calculate damage amounts by event type

propdmg <- with(data, aggregate(PROPDMGAMT ~ EVTYPE, FUN = sum))
cropdmg <- with(data, aggregate(CROPDMGAMT ~ EVTYPE, FUN = sum))
totdmg <- with(data, aggregate(TOTDMG ~ EVTYPE, FUN = sum))

totdmgsort <- totdmg[order(-totdmg$TOTDMG),]
totdmg5 <- totdmgsort[1:5,]

alldmg <- merge(x=totdmg5, y=cropdmg, by = "EVTYPE")
alldmg <- merge(x=alldmg, y=propdmg, by = "EVTYPE")

alldmg <- melt(alldmg, id="EVTYPE")
alldmg$variable <- factor(alldmg$variable, levels = c("TOTDMG", "PROPDMGAMT", "CROPDMGAMT"))

ggplot(data = alldmg, aes(x = reorder(EVTYPE, -value), y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge") + xlab("Event type") + 
  ylab("Amount of damage") + ggtitle("Economic consequences of weather events in the US: 1950 - 2011") +
  scale_fill_discrete(name="Damage type", labels = c("Total", "Property", "Crop"))
