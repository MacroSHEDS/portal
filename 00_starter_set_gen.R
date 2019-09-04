library(RMariaDB)
library(DBI)
library(stringr)
library(tidyr)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)

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

# grab_cur = DBI::dbReadTable(con, 'chemistry') %>%
#     mutate(uniqueID=gsub('_Dup', '', uniqueID)) %>%
#     group_by(uniqueID, datetime) %>%
#     summarize_if(is.numeric, mean, na.rm=TRUE) %>%
#     as_tibble()
# grab_cur = DBI::dbReadTable(con, 'chemistry') %>%

grab_cur = dtplyr::lazy_dt(DBI::dbReadTable(con, 'chemistry')) %>%
    mutate(uniqueID=gsub('_Dup', '', uniqueID)) %>%
    as_tibble()
grab_cur = group_by(grab_cur, uniqueID, datetime) %>%
    # group_by(uniqueID, datetime) %>%
    summarize_if(is.numeric, mean, na.rm=TRUE) %>%
    ungroup() %>%
    # as.data.table() %>%
    # lazy_dt() %>%
    mutate(uniqueID=str_match(uniqueID, '^(.+?)_.+$')[,2],
        datetime=as.POSIXct(datetime, tz='UTC', format='%m/%e/%y %H:%M')) %>%
    # as_tibble()
    # as.data.table()
# grab_cur = lazy_dt(grab_cur, immutable=FALSE) %>%
# grab_cur = mutate(grab_cur, uniqueID=str_match(uniqueID, '^(.+?)_.+$')[2]) %>%
    select(-waterYr, -refNo) %>%
    # select(-uniqueID) %>%
    # rename(uniqueID=temp) %>%
    as_tibble()


    select(-refNo,
grab_hist = dtplyr::lazy_dt(DBI::dbReadTable(con, 'historical'))
sens_Q = dtplyr::lazy_dt(DBI::dbReadTable(con, 'sensor2'))
sens_etc = dtplyr::lazy_dt(DBI::dbReadTable(con, 'sensor3'))

tidyr::gather(grab_cur,'variable', 'value',
    data = gather(data, 'Variable', 'Value', Light_PAR:Discharge_m3s)

setdiff(colnames(grab_hist), colnames(grab_cur))
colnames(sens_etc)

dbDisconnect(con)
