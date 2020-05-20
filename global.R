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

#todo:
#most of the variables created in this script can be automatically generated.
    #those that can't should be read from a config file or spreadsheet eventually.
#get rid of legacy junk from hbwater portal, like tab numbers
#attend to trailing comments within this script
#change "grab" to "chem" everywhere

#uncomment and run to deploy demo app
# rsconnect::deployApp('/home/mike/git/macrosheds/portal',
#     appName='MacroSheds_demo')

#for local testing
# setwd('~/git/macrosheds/portal')
# setwd('/spencer/path/macrosheds/portal')

#complete this list with any commonly used dplyr funcs, maybe plyr::`.`
#refer to everything else explicity with ::
glue = glue::glue
filter = dplyr::filter
select = dplyr::select
mutate = dplyr::mutate
group_by = dplyr::group_by
summarize = dplyr::summarize
pull = dplyr::pull
distinct = dplyr::distinct
`%>%` = magrittr::`%>%`

source('helpers.R') #maybe package these or put them in a namespace called "ms"

#load global datasets
site_data = sm(readr::read_csv('data/site_data.csv')) %>%
    filter(as.logical(in_workflow))
variables = sm(readr::read_csv('data/variables.csv'))

#set defaults, which determine what data are shown when user lands
default_domain = 'hbef'
default_sites_by_domain = list(
    'hbef'='W1',
    'hjandrews'='GSLOOK',
    'neon'='ARIK') #this can be generated automatically and overridden here
default_sitelist = sitelist_from_domain(default_domain, site_type='stream_gauge')
default_site = default_sites_by_domain[[default_domain]]

#load landing datasets
P = read_feather(glue('data/{d}/precip.feather',
    d=default_domain)) #update once rain gages are aggregated more finely
Q = read_feather(glue('data/{d}/discharge/{s}.feather',
    d=default_domain, s=default_site))
pchem = read_feather(glue('data/{d}/pchem.feather',
    d=default_domain)) #update once rain gages are aggregated more finely
grab = read_feather(glue('data/{d}/chemistry/{s}.feather',
    d=default_domain, s=default_site))
    #filter(site_name %in% sites_with_Q) #still desirable?
flux = read_feather(glue('data/{d}/flux/{s}.feather',
    d=default_domain, s=default_site))

#make vector of domain IDs and their pretty names
domains_df = unique(site_data[, c('domain', 'pretty_domain')])
domains = domains_df$domain
names(domains) = domains_df$pretty_domain

#create collections of variable types, some of them as menu-ready lists
fluxvars = variables %>%
    filter(as.logical(flux_convertible)) %>%
    pull(variable_code)

grabvars = variables %>%
    filter(
        variable_type == 'grab',
        ! variable_code %in% c('flowGageHt', 'P'))
    # filter(variable_code %in% fluxvars) #might need this back temporarily

grabvars_display = grabvars %>%
    mutate(displayname=paste0(variable_name, ' (', unit, ')')) %>%
    select(displayname, variable_code, variable_subtype) %>%
    plyr::dlply(plyr::.(variable_subtype), function(x){
        plyr::daply(x, plyr::.(displayname), function(y){
            y['variable_code']
        })
    })

pchemvars = list( #temporary: update this list as part of a daily scheduled task
    hbef=c('pH', 'spCond', 'Ca', 'Mg', 'K', 'Na', 'TMAl', 'OMAl', 'Al_ICP',
        'NH4', 'SO4', 'NO3', 'Cl', 'PO4', 'DOC', 'TDN', 'DON', 'SiO2', 'Mn', 'Fe',
        'F', 'cationCharge', 'anionCharge', 'theoryCond', 'ionError', 'ionBalance'),
    hjandrews=c('alk', 'Ca', 'Cl', 'spCond', 'DOC', 'K', 'Mg', 'Na', 'NH3_N',
        'NO3_N', 'pH', 'PO4_P', 'SiO2', 'SO4_S', 'suspSed', 'TDN', 'TDP', 'TKN',
        'UTKN', 'UTN', 'UTP'))

pchemvars_display = grabvars %>% #might eventually encounter a pchemvar that's not in grabvars.
    filter(variable_code %in% Reduce(union, pchemvars)) %>%
    mutate(displayname=paste0(variable_name, ' (', unit, ')')) %>%
    select(displayname, variable_code, variable_subtype) %>%
    plyr::dlply(plyr::.(variable_subtype), function(x){
        plyr::daply(x, plyr::.(displayname), function(y){
            y['variable_code']
        })
    })

conc_vars = variables %>%
    filter(
        unit == 'mg/L', #all conc vars should be in this unit to begin with?
        ! variable_code %in% c('alk', 'suspSed')) %>%
    pull(variable_code)

conc_units = c('ng/L', 'ug/L', 'mg/L', 'g/L', 'nM', 'uM', 'mM', 'M',
    'neq/L', 'ueq/L', 'meq/L', 'eq/L')
flux_units = c('Mg/ha/d', 'kg/ha/d', 'g/ha/d', 'mg/ha/d')

#map pretty names (including mouseover ?s) to internal IDs for conc/flux metrics
conc_flux_names = c('Concentration'='Concentration','x'='Flux', 'y'='VWC')
names(conc_flux_names)[2] = paste('Flux (interpolated)', enc2native('\U2753'))
names(conc_flux_names)[3] = paste('Flux (VWC)', enc2native('\U2753'))

raincolor = '#8ab5de'
# raincolorbold = '#4e8fcd'
# raincolorpale = '#c6dbef'
linecolors = c('#323232', '#008040', '#800080')#, raincolor) #'#08519C''#54278F'
pchemcolors = c('#585858', '#1bff8c', '#ff1bff')

sites_with_P = sites_by_domain('precip')
sites_with_Q = sites_by_domain('discharge')
sites_with_pchem = sites_by_domain('precipchem')

#might need modification now that files are read site-by-site
grabvars_display_subset = populate_display_vars(grab[-(1:2)])
pchemvars_display_subset = populate_display_vars(pchem[-(1:2)])

dtrng = as.Date(range(grab$datetime, na.rm=TRUE))


