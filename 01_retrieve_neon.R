library(httr)
library(jsonlite)
# library(stringr)
library(tidyr)
library(data.table)
library(dtplyr)
# library(plyr)
library(tidyverse)
# library(dplyr, warn.conflicts = FALSE)
# library(lubridate)
library(feather)

setwd('/home/mike/git/macrosheds/')

#DP1.20093.001 #Chemical properties of surface water
#DP1.20267.001 #gauge height
#DP4.00133.001 #Z-Q rating curve (only HOPB and GUIL)
#DP4.00130.001 #continuous Q (only HOPB)

#download list of available datasets for the current data product
req = httr::GET(paste0("http://data.neonscience.org/api/v0/products/",
    'DP1.20093.001'))
txt = httr::content(req, as="text")
neondata = jsonlite::fromJSON(txt, simplifyDataFrame=TRUE, flatten=TRUE)

#get available urls, sites, and dates
urls = unlist(neondata$data$siteCodes$availableDataUrls)
avail_sets = stringr::str_match(urls, '(?:.*)/([A-Z]{4})/([0-9]{4}-[0-9]{2})')

avail_sets = avail_sets[avail_sets[, 2] == 'WALK', ]

grab = tibble()
for(i in 1:nrow(avail_sets)){
    print(paste0('i=',i))

    url = avail_sets[i,1]
    site = avail_sets[i,2]
    date = avail_sets[i,3]

    # write(paste('Processing:', site, date),
    #     '../../logs_etc/NEON/NEON_ingest.log', append=TRUE)

    #download a dataset for one site and month
    d = httr::GET(url)
    d = jsonlite::fromJSON(httr::content(d, as="text"))

    # data1_ind = intersect(grep("expanded", d$data$files$name),
    #     grep("fieldData", d$data$files$name))
    data2_ind = intersect(grep("expanded", d$data$files$name),
        grep("fieldSuperParent", d$data$files$name))
    data3_ind = intersect(grep("expanded", d$data$files$name),
        grep("externalLabData", d$data$files$name))
    data4_ind = intersect(grep("expanded", d$data$files$name),
        grep("domainLabData", d$data$files$name))

    # data1 = read.delim(d$data$files$url[data1_ind], sep=",",
    #     stringsAsFactors=FALSE)
    data2 = tryCatch({
        read.delim(d$data$files$url[data2_ind], sep=",",
            stringsAsFactors=FALSE)
        }, error=function(e){
            data.frame()
        })
    data3 = tryCatch({
        read.delim(d$data$files$url[data3_ind], sep=",",
            stringsAsFactors=FALSE)
        }, error=function(e){
            data.frame()
        })
    data4 = tryCatch({
        read.delim(d$data$files$url[data4_ind], sep=",",
            stringsAsFactors=FALSE)
        }, error=function(e){
            data.frame()
        })

    if(nrow(data2)){
        data2 = select(data2, siteID, collectDate, dissolvedOxygen,
            dissolvedOxygenSaturation, specificConductance, waterTemp,
            maxDepth)
    }
    if(nrow(data3)){
        data3 = select(data3, siteID, collectDate, pH, externalConductance,
            externalANC, starts_with('water'), starts_with('total'),
            starts_with('dissolved'), uvAbsorbance250, uvAbsorbance284,
            shipmentWarmQF, externalLabDataQF)
    }
    if(nrow(data4)){
        data4 = select(data4, siteID, collectDate, starts_with('alk'),
            starts_with('anc'))
    }

    grab_sub = plyr::join_all(list(data2, data3, data4), type='full') %>%
        group_by(collectDate) %>%
        summarise_each(list(~ if(is.numeric(.)){
                mean(., na.rm = TRUE)
            } else {
                first(.)
            })) %>%
        ungroup() %>%
        mutate(collectDate=as.POSIXct(collectDate, tz='UTC',
            format='%Y-%m-%dT%H:%MZ'))

    grab = bind_rows(grab, grab_sub)
}

grab_cur = DBI::dbReadTable(con, 'chemistry') %>%
# grab_cur = dtplyr::lazy_dt(DBI::dbReadTable(con, 'chemistry')) %>%
    mutate(uniqueID=gsub('_Dup', '', uniqueID)) %>%
    data.table(.) %>%
    .[!is.na(uniqueID) & !is.na(datetime),
        lapply(.SD, function(x) {
            if(is.numeric(x)) mean(x, na.rm=TRUE) else x[! is.na(x)][1L]
        }),
        by=list(uniqueID, datetime)] %>%
    as_tibble(.) %>%
    mutate(site=str_match(uniqueID, '^(.+?)_.+$')[,2],
        datetime=lubridate::with_tz(as.POSIXct(datetime, tz='US/Eastern',
            format='%m/%e/%y %H:%M'), tzone='UTC')) %>%
    select(-refNo, -uniqueID, -duplicate) %>%
    replace(is.na(.), NA) %>% #for NaNs
    as_tibble()

sensor_etc = dtplyr::lazy_dt(DBI::dbReadTable(con, 'sensor3')) %>%
    rename_all(function(x) gsub('S3__', '', x)) %>%
    mutate(site=paste0('W', watershedID)) %>%
    select(-id, -watershedID) %>%
    replace(is.na(.), NA) %>%
    as_tibble()

sensor = full_join(sensor_Q, sensor_etc) %>%
    arrange(datetime)

# tidyr::gather(grab_cur,'variable', 'value',
#     data = gather(data, 'Variable', 'Value', Light_PAR:Discharge_m3s)

write_feather(grab, 'data/hbef/grab.feather')
write_feather(sensor, 'data/hbef/sensor.feather')

dbDisconnect(con)
