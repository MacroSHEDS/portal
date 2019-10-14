library(V8)
library(feather)
library(plyr)
# library(data.table)
# library(dtplyr)
library(shiny)
library(shinydashboard)
library(dygraphs)
# library(DBI)
# library(ggthemes)
# library(ggplot2)
# library(colorspace)
library(jsonlite)
library(lubridate)
library(xts)
library(leaflet)
library(tidyverse)
library(glue)

# rsconnect::deployApp('/home/mike/git/macrosheds/portal',
#     appName='MacroSheds_demo')
# setwd('~/git/macrosheds/portal/')

site_data = read.csv('data/site_data.csv', stringsAsFactors=FALSE)
site_data = filter(site_data, domain == 'HBEF')

source('helpers.R')

# sensor = read_feather('temp_shinyappsio/dataSensor.feather')
# write_feather(sensor, 'data/sensor.feather')
sensor = read_feather('data/sensor.feather')
sensor$watershedID = paste0('W', as.character(sensor$watershedID))
sensor = filter(sensor, datetime < as.Date('2013-01-01')) #super temporary***
colnames(sensor)[4] = 'site_name'
sites_with_Q = unique(sensor$site_name) #temporary fix**

flux = read_feather('data/flux.feather') %>%
    rename(datetime=date) %>%
    select(-Q_Ld) %>%
    filter(datetime < as.Date('2013-01-01')) #***see above
fluxnames = colnames(flux)[-(1:2)]

# conf = readLines('config.txt')
# postgres_pw = extract_from_config('POSTGRESQL_PW')
#
# con = DBI::dbConnect(RPostgres::Postgres(), host='localhost',
#     dbname='macrosheds', user='mike', password=postgres_pw)
#
# grab = DBI::dbGetQuery(con, paste0('select data_grab.datetime, site.site_name, ',
#         'variable.variable_code, data_grab.value from data_grab, site, variable where ',
#         'data_grab.site=site.id and data_grab.variable=variable.id;')) %>%
#     dplyr::filter(datetime <= Sys.Date()) %>%
#     dplyr::group_by(site_name, variable_code, datetime) %>%
#     dplyr::summarize(value=mean(value,na.rm=TRUE)) %>%
#     tidyr::spread(variable_code, value) %>%
#     dplyr::ungroup() %>%
#     filter(datetime < as.Date('2013-01-01')) #***see above

grab = read_feather('data/grab_munged.feather')

P = dplyr::select(grab, site_name, datetime, precipCatch) %>%
    dplyr::filter(! is.na(precipCatch))

grab = dplyr::filter(grab, site_name %in% sites_with_Q) #**see above

# Q = select(grab, site_name, datetime, flowGageHt)

grabcols = colnames(grab)
grabcols = grabcols[grabcols != 'datetime']

variables = read.csv('data/variables.csv', stringsAsFactors=FALSE)

grabvars = filter(variables, variable_type == 'grab',
    ! variable_code %in% c('flowGageHt', 'precipCatch')) %>%
    filter(variable_code %in% fluxnames) #temporary line probably
grabvars_display = mutate(grabvars,
        displayname=paste0(variable_name, ' (', unit, ')')) %>%
    select(displayname, variable_code, variable_subtype) %>%
    plyr::dlply(plyr::.(variable_subtype), function(x){
        plyr::daply(x, plyr::.(displayname), function(y){
            y['variable_code']
        })
    })

conc_vars = filter(variables, unit == 'mg/L') %>%
    pull(variable_code)

conc_units = c('ng/L', 'ug/L', 'mg/L', 'g/L', 'nM', 'uM', 'mM', 'M',
    'neq/L', 'ueq/L', 'meq/L', 'eq/L')
flux_units = c('Mg/ha/d', 'kg/ha/d', 'g/ha/d', 'mg/ha/d')

# linecolors = c("#000000", "#307975", "#691476", "#735E1F", "#6F0D2F",
#     "#7F8D36", "#37096D", "#074670", "#0C2282", "#750D47")
linecolors = c('black', 'gray', 'red')

# sites = DBI::dbGetQuery(con, paste('select site_name as site from site',
#         'order by site_name asc;')) %>%
#     unlist() %>%
#     unname()
# sites = sites[sites %in% sites_with_Q] #** see above
sites = paste0('W', 1:9) #super temporary

sites_precip <- list("RG1", "RG11", "RG23", "RG22", "N", "S", "SP") #temp

#populate default selections
initial_dtrng = as.Date(range(grab$datetime[grab$site_name == sites[1]],
    na.rm=TRUE))
dtrng = as.Date(range(grab$datetime, na.rm=TRUE))

default_site = 'W1'
grabvars_display_subset = populate_vars(grab[-(1:2)])
# default_var = get_default_var(grab[-(1:2)])

# DBI::dbDisconnect(con)
