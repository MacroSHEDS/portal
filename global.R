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

grabcols = colnames(grab)
grabcols = grabcols[grabcols != 'datetime']
# sensor = read_feather('../data/hbef/sensor.feather')
# sensor = select_if(sensor, is.numeric)

variables = read.csv('data/variables.csv', stringsAsFactors=FALSE)

grabvars = filter(variables, variable_type == 'grab',
    ! variable_code %in% c('flowGageHt', 'precipCatch'))
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

sites = DBI::dbGetQuery(con, paste('select site_name as site from site',
        'order by site_name asc;')) %>%
    unlist() %>%
    unname()

sites_precip <- list("RG1", "RG11", "RG23", "RG22", "N", "S", "SP")

# list of solutes that have units other than mg/L for data items [NOT IN USE]
other_units <- c("pH", "DIC", "ANC960", "ANCMet", "cationCharge", "anionCharge",
    "spCond", "theoryCond", "temp", "ionBalance")

# import MDL/LOQ data  [NOT IN USE]
dataLimits <- read.csv("data/Limits_MDL_LOQ.csv")

# data needed for standardizeClasses() function to work [NOT IN USE]
defClasses <- read.csv("data/Rclasses.csv", header = TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
defClassesSample <- read.csv("data/RclassesSample.csv", header=TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
defClassesSample$date <- as.Date(defClassesSample$date, "%m/%d/%y")

# write_feather(dataSensor, '~/git/macrosheds/app/temp_shinyappsio/dataSensor.feather')
# saveRDS(sensorvars, '~/git/macrosheds/app/temp_shinyappsio/sensorvars.rds')
dataSensor = as.data.frame(read_feather('temp_shinyappsio/dataSensor.feather'))
sensorvars = readRDS('temp_shinyappsio/sensorvars.rds')
sensorvars = sub('S3__', '', sensorvars)
sensorvars = sensorvars[-which(sensorvars %in% c('datetime', 'id', 'watershedID'))]
dataSensor$watershedID = paste0('W', as.character(dataSensor$watershedID))

#populate default selections
initial_dtrng = as.Date(range(grab$datetime[grab$site_name == sites[1]],
    na.rm=TRUE))
dtrng = as.Date(range(grab$datetime, na.rm=TRUE))

default_site = 'W1'
grabvars_display_subset = populate_vars(grab[-(1:2)])
# default_var = get_default_var(grab[-(1:2)])

DBI::dbDisconnect(con)
