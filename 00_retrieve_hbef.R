library(RMariaDB)
library(DBI)
library(stringr)
library(tidyr)
library(plyr)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(lubridate)
library(feather)
library(rethinker)

setwd('/home/mike/git/macrosheds/')

conf = readLines('config.txt')
extract_from_config = function(key){
    ind = which(lapply(conf, function(x) grepl(key, x)) == TRUE)
    val = stringr::str_match(conf[ind], '.*\\"(.*)\\"')[2]
    return(val)
}
pw = extract_from_config('MYSQL_PW')

con = DBI::dbConnect(RMariaDB::MariaDB(), dbname='hbef',
    username='root', password=pw)

#can't use lazy_dt with summarize_if yet
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
    # filter(!is.na(uniqueID) & !is.na(datetime)) %>%
    # group_by(uniqueID, datetime) %>%
    # summarize_if(is.numeric, mean, na.rm=TRUE) %>%
    # ungroup() %>%
    mutate(site=str_match(uniqueID, '^(.+?)_.+$')[,2],
        datetime=lubridate::with_tz(as.POSIXct(datetime, tz='US/Eastern',
            format='%m/%e/%y %H:%M'), tzone='UTC')) %>%
    select(-refNo, -uniqueID, -duplicate) %>%
    replace(is.na(.), NA) %>% #for NaNs
    as_tibble()

grab_hist = DBI::dbReadTable(con, 'historical') %>%
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
    select(-refNo, -uniqueID, -date, -timeEST, -duplicate) %>%
    replace(is.na(.), NA) %>%
    as_tibble()

grab = full_join(grab_cur, grab_hist) %>%
    arrange(datetime)

sensor_Q = dtplyr::lazy_dt(DBI::dbReadTable(con, 'sensor2')) %>%
    mutate(site=paste0('W', watershedID)) %>%
    select(-id, -watershedID) %>%
    replace(is.na(.), NA) %>%
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

dbDisconnect(con)

#save locally and zip
write_feather(grab, 'data/hbef/grab.feather')
write_feather(sensor, 'data/hbef/sensor.feather')

zip('data/hbef.zip', list.files('data/hbef', recursive=TRUE, full.names=TRUE))
out = drive_upload("data/hbef.zip",
    as_id('https://drive.google.com/drive/folders/0ABfF-JkuRvL5Uk9PVA'))

#write data to rethinkdb
recon = openConnection(host="localhost", port=28015, authKey=NULL, v="V0_4")

dbs = cursorToList(r()$dbList()$run(recon))
if(! 'macrosheds' %in% dbs){
    r()$dbCreate('macrosheds')$run(recon)
}

tables = cursorToList(r('macrosheds')$tableList()$run(recon))
if(! 'sourcedata' %in% dbs){
    r('macrosheds')$tableCreate('sourcedata')$run(recon)
}

grablong = gather(select(grab, -waterYr), 'variable', 'value', Ca:canonical)
grablong$id = 1:nrow(grablong)
grablist = plyr::daply(grablong, .(id), list)
grablist = unname(grablist)

chunker_ingester = function(l, chunksize=100000){

    #determine chunks based on number of records
    n_recs = length(l)
    n_full_chunks = floor(n_recs / chunksize)
    partial_chunk_len = n_recs %% chunksize

    #convert directly to dict if small enough, otherwise do it chunkwise
    if(n_full_chunks == 0){
        r('macrosheds', 'sourcedata')$insert(l)$run(recon)
    } else {
        for(i in 1:n_full_chunks){
            chunk = l[1:chunksize]
            l[1:chunksize] = NULL
            r('macrosheds', 'sourcedata')$insert(chunk)$run(recon)
        }
    }

    if(partial_chunk_len){
        r('macrosheds', 'sourcedata')$insert(l)$run(recon)
    }
}

chunker_ingester(grablist)

r('macrosheds', 'sourcedata')$insert(grablist[1:5])$run(recon)
r('macrosheds', 'sourcedata')$get(1)$run(recon)
r('macrosheds', 'sourcedata')$delete()$run(recon)
r('macrosheds', 'sourcedata')$count()$run(recon)
close(recon)
