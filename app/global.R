library(V8)
library(stringr)
library(feather)
library(dplyr)
# library(plyr)
library(data.table)
library(dtplyr)
library(shiny)
library(dygraphs)
library(RMariaDB)
library(DBI)
# library(ggthemes)
# library(ggplot2)

# library(colorspace)
# library(jsonlite)
# library(plotly)
library(lubridate)
# library(RColorBrewer)
# library(reshape2)
library(tidyr)
library(xts)

library(leaflet)

site_data = read.csv('site_data.csv', stringsAsFactors=FALSE)

pass = readLines('~/git/hbef/RMySQL.config')    # for MV's local computer

linecolors = c("#000000", "#307975", "#691476", "#735E1F", "#6F0D2F",
    "#7F8D36", "#37096D", "#074670", "#0C2282", "#750D47")

# setwd('~/git/macrosheds/app')
grab = read_feather('../data/hbef/grab.feather')
grab = grab %>% select(which(sapply(., class) == 'numeric'), datetime, -waterYr)
grabcols = colnames(grab)
grabcols = grabcols[grabcols != 'datetime']
# sensor = read_feather('../data/hbef/sensor.feather')
# sensor = select_if(sensor, is.numeric)

# {4}(\".+)? = (.+)?,
#    \2 = list(\1, ("")),
variables = read.csv('../data/general/variables.csv', stringsAsFactors=FALSE)
# variables = read.csv('~/git/macrosheds/data/general/variables.csv', stringsAsFactors=FALSE)

grabvars = filter(variables, category == 'grab')
grabvars_display = mutate(grabvars,
        displayname=paste0(fullname, ' (', unit, ')')) %>%
    select(displayname, shortname, subcategory) %>%
    plyr::dlply(plyr::.(subcategory), function(x){
        plyr::daply(x, plyr::.(displayname), function(y){
            y['shortname']
        })
    })

codes999.9 <- c("timeEST", "temp", "ANC960", "ANCMet",
    "ionError", "ionBalance")
codes123 <- c("pH", "pHmetrohm", "spCond", "au254", "au275",
    "au295", "au350", "au400", "Ca", "Mg",
    "K", "Na", "TMAl", "OMAl", "Al_ICP", "NH4",
    "SO4", "NO3", "Cl", "PO4", "DOC", "TDN", "DIC",
    "DON", "SiO2", "Mn", "Fe", "F")

# Lists of Sites
#***************
sites_streams <- list("Watershed 1" = "W1",
    "Watershed 2" = "W2",
    "Watershed 3" = "W3",
    "Watershed 4" = "W4",
    "Watershed 5" = "W5",
    "Watershed 6" = "W6",
    "Watershed 7" = "W7",
    "Watershed 8" = "W8",
    "Watershed 9" = "W9",
    "HBK",
    "ML70",
    "SW")

#Precipitation sites
# If you update this list, also update conditional panel below
sites_precip <- list("RG1", "RG11", "RG23", "RG22", "N", "S", "SP")

# wateryears ----> see list after data import

# list of solutes that have units other than mg/L for data items
other_units <- c("pH",
    "DIC",
    "ANC960",
    "ANCMet",
    "cationCharge",
    "anionCharge",
    "spCond",
    "theoryCond",
    "temp",
    "ionBalance")


## Function to re-classify data class (e.g. numeric, character, etc.) of each variable (column) in a data
## frame. Uses defClasses & defClassesSample data to match data column with its intended
## data class.
standardizeClasses <- function(d) {
    # d:              data.frame to be checked for columns with only NA's
    # ColClasses :    vector of desired class types for the data.frame
    message(paste("In standardizeClasses for", deparse(substitute(d))))
    r <- nrow(d)
    c <- ncol(d)
    for (i in 1:c) {
        # 1. Insert an additional row with a sample value for each column
        ## Find index in defClassesSample that corresponds to column in d, save that index
        current_col_ofData <- colnames(d[i])
        ind_col <- which(current_col_ofData == colnames(defClassesSample), arr.ind = TRUE)
        ## Add corresponding sample value to last row of d
        d[r+1,i] <- defClassesSample[1,ind_col]
        # 2. Define class of each column according to what you specified in defClasses
        ind_row <- which(current_col_ofData == defClasses$VariableName, arr.ind = TRUE)
        switch(defClasses$Class[ind_row],
            integer=as.integer(d[[i]]),
            character=as.character(d[[i]]),
            #numeric=as.numeric(as.character(d[[i]])),
            numeric=as.numeric(d[[i]]),
            Date=as.Date(d[[i]]), #, origin='1970-01-01'
            ### !!! Class below not being used, causing problems
            #POSIXct=as.POSIXct(d[[i]], "%Y-%m-%d %H:%M", tz="EST", usetz=FALSE, na.rm=TRUE),
            factor=as.factor(d[[i]])
        )
    }
    ## 3. Delete last row of sample values
    d <- d[-(r+1),]
    d
}

# import MDL/LOQ data
dataLimits <- read.csv("data/Limits_MDL_LOQ.csv")

# data needed for standardizeClasses() function to work
defClasses <- read.csv("data/Rclasses.csv", header = TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
defClassesSample <- read.csv("data/RclassesSample.csv", header=TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
defClassesSample$date <- as.Date(defClassesSample$date, "%m/%d/%y")

# Grabbing Data from MySQL database ----
# USE WHEN LIVE ON REMOTE SITE
#**********************************************
y = RMariaDB::MariaDB()

con = dbConnect(y,
    user = 'root',
    password = pass,
    host = 'localhost',
    dbname = 'hbef')
tables = dbListTables(con)

# # Code for one-time use: to load data into mysql
# dataCurrent <- read.csv("data/current_clean20181202.csv", stringsAsFactors = FALSE, na.strings=c(""," ", "NA"))
#  dataCurrent$date <- as.Date(dataCurrent$date, "%m/%d/%y")
#  dataCurrent <- standardizeClasses(dataCurrent)
#  dbWriteTable(con, "current", dataCurrent, append = TRUE, row.names = FALSE)
#
# dataHistorical<- read.csv("data/historical.csv", stringsAsFactors = FALSE, na.strings=c(""," ", "NA"))
#  dataHistorical$date <- as.POSIXct(dataHistorical$date, "%Y-%m-%d")
#  dataHistorical <- standardizeClasses(dataHistorical)
#  dbWriteTable(con, "historical", dataHistorical, append = TRUE, row.names = FALSE)

# Get data from mysql
dataCurrent <- dbReadTable(con, "current")
dataHistorical <- dbReadTable(con, "historical")
dataSensor <- dbReadTable(con, "sensor2")
sensorvars = dbListFields(con, "sensor3")
sensorvars = sub('S3__', '', sensorvars)
sensorvars = sensorvars[-which(sensorvars %in% c('datetime', 'id', 'watershedID'))]
dataSensor$watershedID = paste0('W', as.character(dataSensor$watershedID))
dbDisconnect(con)

dataCurrent <- standardizeClasses(dataCurrent)
# necessary to prevent problems when downloading csv files
dataCurrent$notes <- gsub(",", ";", dataCurrent$notes)
dataHistorical <- standardizeClasses(dataHistorical)

dataAll = bind_rows(dataCurrent, select(dataHistorical, -canonical))

# ****  END OF DATA IMPORT & PREP ****


# Create water years *list* ----

# Water years list for dataAll
# used in ui.R and server.R for Panels 1-3 (QA/QC graphs)
wy <- levels(as.factor(dataAll$waterYr))
wy1 <- c()
for (i in 1:length(wy)) {
    wy1 <- c(wy1, wy[i])
}
#wy1 <- as.character(sort(as.numeric(wy1), decreasing=TRUE)) # sort so that recent years are first
wateryears <- as.list(wy1)

# Water years list for dataCurrent
# used for Panels 5 (DataEdits)
wy_current <- levels(as.factor(dataCurrent$waterYr))
wy1_current <- c()
for (i in 1:length(wy_current)) {
    wy1_current <- c(wy1_current, wy_current[i])
}
#wy1 <- as.character(sort(as.numeric(wy1), decreasing=TRUE)) # sort so that recent years are first
wateryears_current <- as.list(wy1_current)


# Find maximum date ----
# used in ui.R for Panel 4 (QA/QC "Free-for-all" graph)

maxDate_current <- max(dataCurrent$date, na.rm=TRUE)
maxDate_historical <- max(dataHistorical$date, na.rm=TRUE)
maxDate_sensor <- max(as.Date(dataSensor$date), na.rm=TRUE)

maxDate <- maxDate_historical # default value if dataCurrent or dataSensor are empty
if (maxDate_sensor > maxDate_current) maxDate <- maxDate_sensor
if (maxDate_sensor < maxDate_current) maxDate <- maxDate_current
