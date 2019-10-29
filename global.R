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

source('helpers.R')

default_site = 'W1'
default_domain = 'hbef'

site_data = read.csv('data/site_data.csv', stringsAsFactors=FALSE)
site_data = filter(site_data, domain != 'neon')

default_sites = filter(site_data, domain == default_domain) %>%
    pull(site_name)

domains_df = unique(site_data[,c('domain', 'pretty_domain')])
domains = domains_df$domain
names(domains) = domains_df$pretty_domain

variables = read.csv('data/variables.csv', stringsAsFactors=FALSE)
fluxvars = variables$variable_code[as.logical(variables$flux_convertible)]

grabvars = filter(variables, variable_type == 'grab',
    ! variable_code %in% c('flowGageHt', 'P')) %>%
    filter(variable_code %in% fluxvars) #temporary line probably
grabvars_display = mutate(grabvars,
    displayname=paste0(variable_name, ' (', unit, ')')) %>%
    select(displayname, variable_code, variable_subtype) %>%
    plyr::dlply(plyr::.(variable_subtype), function(x){
        plyr::daply(x, plyr::.(displayname), function(y){
            y['variable_code']
        })
    })

#vars not in this list cant be unit converted
conc_vars = filter(variables, unit == 'mg/L',
        ! variable_code %in% c('alk', 'suspSed')) %>%
    pull(variable_code)

conc_units = c('ng/L', 'ug/L', 'mg/L', 'g/L', 'nM', 'uM', 'mM', 'M',
    'neq/L', 'ueq/L', 'meq/L', 'eq/L')
flux_units = c('Mg/ha/d', 'kg/ha/d', 'g/ha/d', 'mg/ha/d')
conc_flux3_names = c('Concentration'='Concentration','x'='Flux', 'y'='VWC')
names(conc_flux3_names)[2] = paste('Flux (interpolated)', enc2native('\U2753'))
names(conc_flux3_names)[3] = paste('Flux (VWC)', enc2native('\U2753'))

# linecolors = c("#000000", "#307975", "#691476", "#735E1F", "#6F0D2F",
#     "#7F8D36", "#37096D", "#074670", "#0C2282", "#750D47")
linecolors = c('black', 'gray', 'red')

sites_precip <- list("RG1", "RG11", "RG23", "RG22", "N", "S", "SP") #temporary

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
#     filter(datetime < as.Date('2013-01-01')) #temporary

Q = read_feather('data/hbef/discharge.feather') %>%
    filter(datetime < as.Date('2013-01-01')) #temporary
sites_with_Q = unique(Q$site_name) #temporary (hard-code with andrews sites)

grab = read_feather('data/hbef/grab.feather') %>%
    filter(datetime < as.Date('2013-01-01')) %>% #temporary
    filter(site_name %in% sites_with_Q) #temporary
grabvars_display_subset = populate_vars(grab[-(1:2)]) #hard-code this

P = read_feather('data/hbef/precip.feather') %>%
    filter(datetime < as.Date('2013-01-01')) #temporary

flux = read_feather('data/hbef/flux.feather') %>%
    rename(datetime=date) %>% #temporary
    select(-Q_Ld) %>% #temporary
    filter(datetime < as.Date('2013-01-01')) #temporary

initial_dtrng = as.Date(range(grab$datetime[grab$site_name == default_sites[1]],
    na.rm=TRUE))
dtrng = as.Date(range(grab$datetime, na.rm=TRUE))

# DBI::dbDisconnect(con)
