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

#store these elsewhere
get_DP1.20093.001 = function(sets){

    grab = tibble()

    for(i in 1:nrow(sets)){
        print(paste0('i=',i))

        url = sets[i,1]
        site = sets[i,2]
        date = sets[i,3]

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
            mutate(datetime=as.POSIXct(collectDate, tz='UTC',
                format='%Y-%m-%dT%H:%MZ')) %>%
            select(-collectDate)

        grab = bind_rows(grab, grab_sub)
    }

    return(grab)
}
get_DP1.20288.001 = function(sets){

    sensor = tibble()

    for(i in 1:nrow(sets)){
        print(paste0('i=',i, '/', nrow(sets)))

        url = sets[i,1]
        site = sets[i,2]
        date = sets[i,3]

        # write(paste('Processing:', site, date),
        #     '../../logs_etc/NEON/NEON_ingest.log', append=TRUE)

        #download a dataset for one site and month
        d = httr::GET(url)
        d = jsonlite::fromJSON(httr::content(d, as="text"))

        data_inds = intersect(grep("expanded", d$data$files$name),
            grep("instantaneous", d$data$files$name))

        #determine which dataset is upstream/downstream if necessary
        updown_suffixes = c('-up', '-down')
        if(length(data_inds) == 2){
            position = str_split(d$data$files$name[data_inds[1]], '\\.')[[1]][7]
            updown_order = if(position == '101') 1:2 else 2:1
        } else if(length(data_inds) == 1){
            updown_order = 1:2
        } else {
            # write(paste('Problem with data file number for site', site,
            #     '(', prods_abb[p], date, ')'),
            #     '../../logs_etc/NEON/NEON_ingest.log', append=TRUE)
            next
        }

        for(j in 1:length(data_inds)){
            # print(paste('j=',j))

            #add appropriate suffix for upstream/downstream sites
            site_suffix = updown_suffixes[updown_order[j]]
            site_with_suffix = paste0(site, site_suffix)

            #download data
            sensor_sub = read.delim(d$data$files$url[data_inds[j]], sep=",",
                stringsAsFactors=FALSE)

            #get list of variables included
            varind = grep('SciRvw', colnames(sensor_sub))
            rgx = str_match(colnames(sensor_sub)[varind],
                '^(\\w*)(?:FinalQFSciRvw|SciRvwQF)$')
            varlist = flagprefixlist = rgx[,2]
            if('specificCond' %in% varlist){
                varlist[which(varlist == 'specificCond')] =
                    'specificConductance'
            }
            if('dissolvedOxygenSat' %in% varlist){
                varlist[which(varlist == 'dissolvedOxygenSat')] =
                    'dissolvedOxygenSaturation'
            }

            flagcols = grepl('.+?FinalQF(?!SciRvw)', colnames(sensor_sub),
                perl=TRUE)
            datacols = ! grepl('QF', colnames(sensor_sub), perl=TRUE)
            cols = flagcols | datacols
            sensor_sub = sensor_sub[, cols]

            #harmonize colnames
            if('startDateTime' %in% colnames(sensor_sub)){
                NULL #do nothing
            } else if('startDate' %in% colnames(sensor_sub)){
                colnames(sensor_sub)[which(colnames(sensor_sub) == 'startDate')] =
                    'startDateTime'
            } else if('endDate' %in% colnames(sensor_sub)){
                colnames(sensor_sub)[which(colnames(sensor_sub) == 'endDate')] =
                    'startDateTime'
            } else {
                # write(paste('Datetime column not found for:', current_var,
                #     site_with_suffix, date),
                #     '../../logs_etc/NEON/NEON_ingest.log', append=TRUE)
                next
            }

            sensor_sub = mutate(sensor_sub,
                datetime=as.POSIXct(startDateTime,
                    tz='UTC', format='%Y-%m-%dT%H:%M:%SZ'),
                site=site_with_suffix) %>%
                select(-startDateTime)

            sensor = bind_rows(sensor, sensor_sub)
        }
    }

    return(sensor)
}

#DP1.20093.001 #Chemical properties of surface water
#DP1.20267.001 #gauge height
#DP4.00133.001 #Z-Q rating curve (only HOPB and GUIL)
#DP4.00130.001 #continuous Q (only HOPB)

grab_data_products = c('DP1.20093.001')
sensor_data_products = c('DP1.20288.001')

for(g in 1:length(grab_data_products)){

    #get available datasets for this data product
    product = grab_data_products[g]
    req = httr::GET(paste0("http://data.neonscience.org/api/v0/products/",
        product))
    txt = httr::content(req, as="text")
    neondata = jsonlite::fromJSON(txt, simplifyDataFrame=TRUE, flatten=TRUE)

    #get available urls, sites, and dates
    urls = unlist(neondata$data$siteCodes$availableDataUrls)
    avail_sets = stringr::str_match(urls, '(?:.*)/([A-Z]{4})/([0-9]{4}-[0-9]{2})')

    avail_sets = avail_sets[avail_sets[, 2] == 'WALK', ] ####remove this line

    retrieval_func = get(paste0('get_', product))
    grab = do.call(retrieval_func, list(avail_sets)) #append to this?
}

for(s in 1:length(sensor_data_products)){

    #get available datasets for this data product
    product = sensor_data_products[s]
    req = httr::GET(paste0("http://data.neonscience.org/api/v0/products/",
        product))
    txt = httr::content(req, as="text")
    neondata = jsonlite::fromJSON(txt, simplifyDataFrame=TRUE, flatten=TRUE)

    #get available urls, sites, and dates
    urls = unlist(neondata$data$siteCodes$availableDataUrls)
    avail_sets = stringr::str_match(urls, '(?:.*)/([A-Z]{4})/([0-9]{4}-[0-9]{2})')

    avail_sets = avail_sets[avail_sets[, 2] == 'WALK', ] ####remove this line

    retrieval_func = get(paste0('get_', product))
    sensor = do.call(retrieval_func, list(avail_sets)) #append to this?
}

# tidyr::gather(grab_cur,'variable', 'value',
#     data = gather(data, 'Variable', 'Value', Light_PAR:Discharge_m3s)

write_feather(grab, 'data/neon/grab.feather')
write_feather(sensor, 'data/neon/sensor.feather')

zip('data/neon.zip', list.files('data/neon', recursive=TRUE, full.names=TRUE))
out = drive_upload("data/neon.zip",
    as_id('https://drive.google.com/drive/folders/0ABfF-JkuRvL5Uk9PVA'))
