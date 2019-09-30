library(V8)
library(feather)
# library(plyr)
library(data.table)
# library(dtplyr)
library(shiny)
library(shinydashboard)
library(dygraphs)
library(Cairo)
# library(RMariaDB)
# library(DBI)
# library(ggthemes)
# library(ggplot2)

# library(colorspace)
# library(jsonlite)
# library(plotly)
library(lubridate)
# library(RColorBrewer)
# library(reshape2)
library(xts)
library(leaflet)
library(tidyverse)

# rsconnect::deployApp('~/git/macrosheds/app', appName='MacroSheds_demo')
# setwd('~/git/macrosheds/portal/')

site_data = read.csv('site_data.csv', stringsAsFactors=FALSE)

source('helpers.R')

conf = readLines('config.txt')
postgres_pw = extract_from_config('POSTGRESQL_PW')

con = DBI::dbConnect(RPostgres::Postgres(), host='localhost',
    dbname='macrosheds', user='mike', password=postgres_pw)

grab = DBI::dbGetQuery(con, paste0('select data_grab.datetime, site.site_name, ',
        'variable.variable_code, data_grab.value from data_grab, site, variable where ',
        'data_grab.site=site.id and data_grab.variable=variable.id;')) %>%
    dplyr::filter(datetime <= Sys.Date()) %>%
    dplyr::group_by(site_name, variable_code, datetime) %>%
    dplyr::summarize(value=mean(value,na.rm=TRUE)) %>%
    tidyr::spread(variable_code, value) %>%
    dplyr::ungroup()

# grab = read_feather('data/grab.feather')
# grab = grab %>% select(which(sapply(., class) == 'numeric'), datetime, -waterYr)
grabcols = colnames(grab)
grabcols = grabcols[grabcols != 'datetime']
# sensor = read_feather('../data/hbef/sensor.feather')
# sensor = select_if(sensor, is.numeric)

# {4}(\".+)? = (.+)?,
#    \2 = list(\1, ("")),
variables = read.csv('data/variables.csv', stringsAsFactors=FALSE)
# variables = read.csv('../data/general/variables.csv', stringsAsFactors=FALSE)
# variables = read.csv('~/git/macrosheds/data/general/variables.csv', stringsAsFactors=FALSE)

grabvars = filter(variables, variable_type == 'grab')
grabvars_display = mutate(grabvars,
        displayname=paste0(variable_name, ' (', unit, ')')) %>%
    select(displayname, variable_code, variable_subtype) %>%
    plyr::dlply(plyr::.(variable_subtype), function(x){
        plyr::daply(x, plyr::.(displayname), function(y){
            y['variable_code']
        })
    })

linecolors = c("#000000", "#307975", "#691476", "#735E1F", "#6F0D2F",
    "#7F8D36", "#37096D", "#074670", "#0C2282", "#750D47")

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

# import MDL/LOQ data
dataLimits <- read.csv("data/Limits_MDL_LOQ.csv")

# data needed for standardizeClasses() function to work
defClasses <- read.csv("data/Rclasses.csv", header = TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
defClassesSample <- read.csv("data/RclassesSample.csv", header=TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
defClassesSample$date <- as.Date(defClassesSample$date, "%m/%d/%y")

# Grabbing Data from MySQL database ----
# USE WHEN LIVE ON REMOTE SITE
#**********************************************
# y = RMariaDB::MariaDB()
#
# con = dbConnect(y,
#     user = 'root',
#     password = pass,
#     host = 'localhost',
#     dbname = 'hbef')
# tables = dbListTables(con)

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
# dataCurrentO <- dbReadTable(con, "current")
# dataHistoricalO <- dbReadTable(con, "historical")
# dataSensorO <- dbReadTable(con, "sensor2")
# sensorvarsO = dbListFields(con, "sensor3")
# write_feather(dataCurrent, '~/git/macrosheds/app/temp_shinyappsio/dataCurrent.feather')
# write_feather(dataHistorical, '~/git/macrosheds/app/temp_shinyappsio/dataHistorical.feather')
# write_feather(dataSensor, '~/git/macrosheds/app/temp_shinyappsio/dataSensor.feather')
# saveRDS(sensorvars, '~/git/macrosheds/app/temp_shinyappsio/sensorvars.rds')
# dataCurrent = as.data.frame(read_feather('temp_shinyappsio/dataCurrent.feather'))
# dataHistorical = as.data.frame(read_feather('temp_shinyappsio/dataHistorical.feather'))
dataSensor = as.data.frame(read_feather('temp_shinyappsio/dataSensor.feather'))
sensorvars = readRDS('temp_shinyappsio/sensorvars.rds')
sensorvars = sub('S3__', '', sensorvars)
sensorvars = sensorvars[-which(sensorvars %in% c('datetime', 'id', 'watershedID'))]
dataSensor$watershedID = paste0('W', as.character(dataSensor$watershedID))
# dbDisconnect(con)

# dataCurrent <- standardizeClasses(dataCurrent)
# necessary to prevent problems when downloading csv files
# dataCurrent$notes <- gsub(",", ";", dataCurrent$notes)
# dataHistorical <- standardizeClasses(dataHistorical)

# dataAll = bind_rows(dataCurrent, select(dataHistorical, -canonical))
# dataAllR = dataAll

# ****  END OF DATA IMPORT & PREP ****


# Create water years *list* ----

# # Water years list for dataAll
# # used in ui.R and server.R for Panels 1-3 (QA/QC graphs)
# wy <- levels(as.factor(dataAll$waterYr))
# wy1 <- c()
# for (i in 1:length(wy)) {
#     wy1 <- c(wy1, wy[i])
# }
# #wy1 <- as.character(sort(as.numeric(wy1), decreasing=TRUE)) # sort so that recent years are first
# wateryears <- as.list(wy1)
#
# # Water years list for dataCurrent
# # used for Panels 5 (DataEdits)
# wy_current <- levels(as.factor(dataCurrent$waterYr))
# wy1_current <- c()
# for (i in 1:length(wy_current)) {
#     wy1_current <- c(wy1_current, wy_current[i])
# }
# #wy1 <- as.character(sort(as.numeric(wy1), decreasing=TRUE)) # sort so that recent years are first
# wateryears_current <- as.list(wy1_current)


# Find maximum date ----
# used in ui.R for Panel 4 (QA/QC "Free-for-all" graph)

# maxDate_current <- max(dataCurrent$date, na.rm=TRUE)
# maxDate_historical <- max(dataHistorical$date, na.rm=TRUE)
# maxDate_sensor <- max(as.Date(dataSensor$date), na.rm=TRUE)

# maxDate <- maxDate_historical # default value if dataCurrent or dataSensor are empty
maxDate = max(grab$datetime, na.rm=TRUE)
# if (maxDate_sensor > maxDate_current) maxDate <- maxDate_sensor
# if (maxDate_sensor < maxDate_current) maxDate <- maxDate_current
