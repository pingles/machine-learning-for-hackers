# Chap1
# 
# first up, we need to install the packages
#

install.packages("tm", dependencies=TRUE)
install.packages("arm", dependencies=TRUE)
install.packages("ggplot2", dependencies=TRUE)
install.packages("glmnet", dependencies=TRUE)
install.packages("igraph", dependencies=TRUE)
install.packages("lme4", dependencies=TRUE)
install.packages("lubridate", dependencies=TRUE)
install.packages("RCurl", dependencies=TRUE)
install.packages("reshape", dependencies=TRUE)
install.packages("RJSONIO", dependencies=TRUE)
install.packages("XML", dependencies=TRUE)

# load ggplot2
library(ggplot2)

# need help with a function? do this:
?read.delim

# download the data from here:
# http://www.infochimps.com/datasets/60000-documented-ufo-sightings-with-text-descriptions-and-metada
# and store in ./data/

setwd("/Users/paul/Work/machine-learning-for-hackers")

# we use stringsAsFactors so that R doesn't interpret
# strings as categorical data
ufo <- read.delim("./data/ch1/ufo_awesome.tsv", sep="\t", stringsAsFactors=FALSE, header=FALSE, na.strings="")

# lets set some headers
names(ufo) <- c("DateOccurred", "DateReported", "Location", "ShortDesc", "Duration", "LongDesc")

# looks like the file has some erroneous data in it
head(ufo[which(nchar(ufo$DateOccurred) != 8 | nchar(ufo$DateReported) != 8), 1])

goodrows <- ifelse(nchar(ufo$DateOccurred) != 8 | nchar(ufo$DateReported) != 8, FALSE, TRUE)
length(goodrows)
length(which(!goodrows))

# only keep the fixed data
ufo <- ufo[goodrows,]

# convert the dates to dates
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format="%Y%m%d")
ufo$DateReported <- as.Date(ufo$DateReported, format="%Y%m%d")

# lets look at the data, much better!
head(ufo)


location<-function(l) {
  split.location<-tryCatch(strsplit(l,",")[[1]], error= function(e) return(c(NA, NA)))
  clean.location<-gsub("^ ","",split.location)
  if (length(clean.location)>2) {
    return(c(NA,NA))
  }
  else {
    return(clean.location)
  }
}

city.state <- lapply(ufo$Location, location)
head(city.state)

# lists in R are key value structures- the keys are indexed
# by double bracket, values by single bracket

# need to convert to a matrix to merge with the current data
# do.call executes a function over a list. rbind = row bind.
location.matrix <- do.call(rbind, city.state)
ufo <- transform(ufo, USCity=location.matrix[,1], USState=tolower(location.matrix[,2]), stringsAsFactors=FALSE)

head(ufo)

# filter for only US states...

us.states <- c("ak", "al", "ar", "az", "ca", "co", "ct", "de", "fl", "ga", "gi", "ia", "id", "il", "in", "ks", "ky", "la", "ma", "md", "me", "mi", "mn", "mo", "ms", "mt", "nc", "nd", "ne", "nh", "nj", "nm", "nv", "ny", "oh", "ok", "or", "pa", "ri", "sc", "sd", "tn", "tx", "ut", "va", "vt", "wa", "wi", "wv", "wy")

ufo$USState <- us.states[match(ufo$USState, us.states)]
ufo$USCity[is.na(ufo$USState)] <- NA

ufo.us <- subset(ufo, !is.na(USState))



## 
## Analysing
##

## first up, lets summarise the date range

summary(ufo.us$DateOccurred)

## 1400 seems very early, lets plot the data
## as a histogram

date.hist <- ggplot(ufo.us, aes(x=DateOccurred)) + geom_histogram()
print(date.hist)
ggsave(plot=date.hist, filename="data/ch1/date_hist.png", height=6, width=8)

ufo.us <- subset(ufo.us, DateOccurred>=as.Date("1990-01-01"))
date.hist <- ggplot(ufo.us, aes(x=DateOccurred)) + geom_histogram()
print(date.hist)
nrow(ufo.us)

## lets reduce the data to have a year/month value
## and see whether there's a seasonal effect:

ufo.us$YearMonth <- strftime(ufo.us$DateOccurred, format="%Y-%m")

require(plyr)

date.range <- seq.Date(from=as.Date(min(ufo.us$DateOccurred)), to=as.Date(max(ufo.us$DateOccurred)), by="month")
date.strings <- strftime(date.range, "%Y-%m")

states.dates <- lapply(us.states,function(s) cbind(s,date.strings))
states.dates <- data.frame(do.call(rbind, states.dates), stringsAsFactors=FALSE)
head(states.dates)

## 1 ak      1990-01
## 2 ak      1990-02
## 3 ak      1990-03
## 4 ak      1990-04
## 5 ak      1990-05
## 6 ak      1990-06

all.sightings <- merge(states.dates,sightings.counts,by.x=c("s","date.strings"), by.y=c("USState","YearMonth"),all=TRUE)
names(all.sightings) <- c("State","YearMonth","Sightings")
all.sightings$Sightings[is.na(all.sightings$Sightings)] <- 0
all.sightings$YearMonth <- as.Date(rep(date.range,length(us.states)))
all.sightings$State <- as.factor(toupper(all.sightings$State))
head(all.sightings)


state.plot <- ggplot(all.sightings, aes(x=YearMonth,y=Sightings))+
  geom_line(aes(color="darkblue")) +
  facet_wrap(~State,nrow=10,ncol=5) +
  scale_color_manual(values=c("darkblue"="darkblue"),legend=FALSE) +
  xlab("Time")+ylab("Number of Sightings") +
  opts(title="Number of UFO sightings by Month-Year and U.S. State (1990-2010)")

print(state.plot)
