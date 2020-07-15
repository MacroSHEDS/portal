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
#attend to trailing comments within this script

#uncomment and run to deploy demo app
# rsconnect::deployApp('/home/mike/git/macrosheds/portal',
#     appName='MacroSheds_demo')

#for local testing
# setwd('~/git/macrosheds/portal')
# setwd('~/desktop/macrosheds/portal')

source('helpers.R') #maybe package these or put them in a namespace called "ms"
source('function_aliases.R')

#load global datasets
site_data = sm(readr::read_csv('data/site_data.csv')) %>%
    filter(as.logical(in_workflow))
if(any(duplicated(site_data$site_name))) stop('site_names must be unique, even across domains')
variables = sm(readr::read_csv('data/variables.csv'))

#set defaults, which determine what data are shown when user lands
default_domain = 'hbef'
default_sites_by_domain = list(
    'hbef'='W1',
    'hjandrews'='GSLOOK',
    'neon'='ARIK') #this can be generated automatically and overridden here
default_sitelist = sitelist_from_domain(default_domain, type='stream_gauge')
default_site = default_sites_by_domain[[default_domain]]

#load base data for when user lands in app (could use convenience functions here)
basedata = list(
    P = read_feather(glue('data/{d}/precip.feather',
        d=default_domain)), #update once rain gage interpolation is done
    Q = read_feather(glue('data/{d}/discharge/{s}.feather',
        d=default_domain, s=default_site)),
    pchem = read_feather(glue('data/{d}/pchem.feather',
        d=default_domain)), #update once rain gage interpolation is done
    chem = read_feather(glue('data/{d}/chemistry/{s}.feather',
        d=default_domain, s=default_site)),
    flux = read_feather(glue('data/{d}/flux/{s}.feather',
        d=default_domain, s=default_site))
)

#make vector of domain IDs and their pretty names
domains_df = unique(site_data[, c('domain', 'pretty_domain')])
domains_pretty = domains_df$domain
names(domains_pretty) = domains_df$pretty_domain

#create collections of variable types, some of them as menu-ready lists
fluxvars = variables %>%
    filter(as.logical(flux_convertible)) %>%
    pull(variable_code)

chemvars = variables %>%
    filter(
        #variable_type == 'grab',
        ! variable_code %in% c('flowGageHt', 'P'))
    # filter(variable_code %in% fluxvars) #might need this back temporarily
chemvars_display = generate_dropdown_varlist(chemvars)

pchemvars = list( #temporary: update this list as part of a daily scheduled task
    hbef=c('pH', 'spCond', 'Ca', 'Mg', 'K', 'Na', 'TMAl', 'OMAl', 'Al_ICP',
        'NH4', 'SO4', 'NO3', 'Cl', 'PO4', 'DOC', 'TDN', 'DON', 'SiO2', 'Mn', 'Fe',
        'F', 'cationCharge', 'anionCharge', 'theoryCond', 'ionError', 'ionBalance'),
    hjandrews=c('alk', 'Ca', 'Cl', 'spCond', 'DOC', 'K', 'Mg', 'Na', 'NH3_N',
        'NO3_N', 'pH', 'PO4_P', 'SiO2', 'SO4_S', 'suspSed', 'TDN', 'TDP', 'TKN',
        'UTKN', 'UTN', 'UTP'))
pchemvars_display = generate_dropdown_varlist(chemvars,
        filter_set=Reduce(union, pchemvars))

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

raincolors = c('#8ab5de', '#36486b', '#618685') #blues
linecolors = c('#323232', '#008040', '#800080') #black, green, purple
pchemcolors = c('#585858', '#1bff8c', '#ff1bff') #lighter shades of the above

sites_with_P = sites_by_var('precip')
sites_with_Q = sites_by_var('discharge')
sites_with_pchem = sites_by_var('precipchem')

#might need modification now that files are read site-by-site
chemvars_display_subset = filter_dropdown_varlist(basedata$chem)
pchemvars_display_subset = filter_dropdown_varlist(basedata$pchem)

dtrng = as.Date(range(basedata$chem$datetime, na.rm=TRUE))

