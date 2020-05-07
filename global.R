library(V8)
library(feather)
library(plyr)
library(data.table)
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
library(shinyjs)

# rsconnect::deployApp('/home/mike/git/macrosheds/portal',
#     appName='MacroSheds_demo')
# setwd('~/git/macrosheds/portal/')

source('helpers.R')

default_domain = 'hbef'
default_site = list('hbef'='W1', 'hjandrews'='GSLOOK', 'neon'='ARIK')

site_data = read_csv('data/site_data.csv') %>%
    filter(as.logical(in_workflow))

default_sitelist = site_data %>%
    filter(domain == default_domain, site_type == 'stream_gauge') %>%
    pull(site_name)

domains_df = unique(site_data[, c('domain', 'pretty_domain')])
domains = domains_df$domain
names(domains) = domains_df$pretty_domain

# variables = read.csv('data/variables.csv', stringsAsFactors=FALSE)
variables = read_csv('data/variables.csv')
fluxvars = variables$variable_code[as.logical(variables$flux_convertible)]

grabvars = filter(variables, variable_type == 'grab',
    ! variable_code %in% c('flowGageHt', 'P')) %>%
    filter(variable_code %in% fluxvars) #temporary line probably
grabvars_display = grabvars %>%
    mutate(displayname=paste0(variable_name, ' (', unit, ')')) %>%
    select(displayname, variable_code, variable_subtype) %>%
    plyr::dlply(plyr::.(variable_subtype), function(x){
        plyr::daply(x, plyr::.(displayname), function(y){
            y['variable_code']
        })
    })

pchemvars = list(
    hbef=c('pH', 'spCond', 'Ca', 'Mg', 'K', 'Na', 'TMAl', 'OMAl', 'Al_ICP',
        'NH4', 'SO4', 'NO3', 'Cl', 'PO4', 'DOC', 'TDN', 'DON', 'SiO2', 'Mn', 'Fe',
        'F', 'cationCharge', 'anionCharge', 'theoryCond', 'ionError', 'ionBalance'),
    hjandrews=c('alk', 'Ca', 'Cl', 'spCond', 'DOC', 'K', 'Mg', 'Na', 'NH3_N',
        'NO3_N', 'pH', 'PO4_P', 'SiO2', 'SO4_S', 'suspSed', 'TDN', 'TDP', 'TKN',
        'UTKN', 'UTN', 'UTP')) #temporary: update these lists as daily schedge tasks
pchemvars_display = grabvars %>% #might eventually encounter a pchemvar that's not in grabvars
    filter(variable_code %in% Reduce(union, pchemvars)) %>%
    mutate(displayname=paste0(variable_name, ' (', unit, ')')) %>%
    select(displayname, variable_code, variable_subtype) %>%
    plyr::dlply(plyr::.(variable_subtype), function(x){
        plyr::daply(x, plyr::.(displayname), function(y){
            y['variable_code']
        })
    })

# x1 = feather::read_feather('~/git/macrosheds/portal/data/hbef/pchem.feather')
# x2 = feather::read_feather('~/git/macrosheds/portal/data/hjandrews/pchem.feather')
# x1 = apply(x1[,-(1:2)], 2, function(x) sum(is.na(x))/length(x))
# x2 = apply(x2[,-(1:2)], 2, function(x) sum(is.na(x))/length(x))
# paste(names(x1[x1 != 1]), collapse="', '")
# paste(names(x2[x2 != 1]), collapse="', '")

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

raincolor = '#8ab5de'
# raincolorbold = '#4e8fcd'
# raincolorpale = '#c6dbef'
linecolors = c('#323232', '#008040', '#800080')#, raincolor) #'#08519C''#54278F'
pchemcolors = c('#585858', '#1bff8c', '#ff1bff')

sites_with_P = list(hbef=c('RG1', 'RG11', 'RG23', 'RG22', 'N', 'S', 'SP'),
    hjandrews=c('RD1507', 'L523RG', 'BLUERD', 'CARPMT', 'CENMET',
        'EARTHF', 'FORKS_', 'FRISEL', 'WS3GRD', 'GSWS10', 'GSWS01',
        'GSWS09', 'GSMACK', 'WS3JRD', 'MCRAEB', 'MIDWAY', 'WS3MRD',
        'MIRKWD', 'MACKWE', 'RS13RG', 'H15PND', 'RS03RG', 'RS05RG',
        'RS18RG', 'H15RCK', 'H15RDG', 'RDSEND', 'ROSSRG', 'WS1SDL',
        'SPOTFI', 'SLTRWD', 'TRAILS', 'UNIT3B', 'UNIT3H', 'UPLMET',
        'VANMET', 'VARMET', 'WS10RG', 'WS09RG')) #temporary (add flex)

sites_with_Q = c('W1', 'W2', 'W3', 'W4', 'W5', 'W6', 'W7', 'W8', 'W9',
    'GSLOOK', 'GSWS01', 'GSWS02', 'GSWS03', 'GSWS06', 'GSWS07', 'GSWS08',
    'GSWS09', 'GSWS10', 'GSWSMA', 'GSWSMC', 'GSWSMF') #temporary (add flex)

#KEEP the below comment block for convenience
# z = feather::read_feather('~/git/macrosheds/portal/data/hbef/pchem.feather')
# paste(unique(z$site_name), collapse="', '")
# z = feather::read_feather('~/git/macrosheds/portal/data/hjandrews/pchem.feather')
# paste(unique(z$site_name), collapse="', '")
sites_with_pchem = list(hbef=c('N', 'RG11', 'RG22', 'RG23', 'S'),
    hjandrews=c('RCADMN', 'RCHI15', 'RCHIF7', 'RCHIR7')) #temporary; derived above

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
#     dplyr::ungroup()

P = read_feather('data/hbef/precip.feather')
Q = read_feather('data/hbef/discharge.feather')
pchem = read_feather('data/hbef/pchem.feather')
grab = read_feather('data/hbef/grab.feather') %>%
    filter(site_name %in% sites_with_Q)

# vars_by_site = pchem %>%
#     select(site_name, one_of(pchemvars$hbef)) %>%
#     gather('var', 'val', pH:ionBalance) %>%
#     filter_at(vars(val), any_vars(! is.na(.))) %>%
#     group_by(site_name) %>%
#     distinct(var)
# plyr::dlply(vars_by_site, 'site_name',
#     function(x) x$var)

grabvars_display_subset = populate_vars(grab[-(1:2)]) #temporary (add flex for multi dmn, also see server.R)
pchemvars_display_subset = populate_vars(pchem[-(1:2)]) #temporary (add flex for multi dmn, also see server.R)
# grabvars_display_subset = function(){
#     v = populate_vars(grab[-(1:2)]) #temporary (add flex for multi dmn, also see server.R)
#     return(v)
# }

flux = read_feather('data/hbef/flux.feather')

initial_dtrng = as.Date(range(grab$datetime[grab$site_name == default_sitelist[1]],
    na.rm=TRUE))
dtrng = as.Date(range(grab$datetime, na.rm=TRUE))

# DBI::dbDisconnect(con)

# Q = feather::read_feather('~/git/macrosheds/portal/data/hjandrews/discharge.feather')
