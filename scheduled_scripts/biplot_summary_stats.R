df <- network_domain_default_sites

# Currently viewing monthly summaries is disabled on biplot, primarilay becuase
# general products are only saved as calendar year summaries. This should be changed
# in the future
compute_monthly_summary <- function(df) {

    all_domain <- tibble()
    for(i in 1:nrow(df)) {

        dom <- df$domain[i]

        net <- df$network[i]

        dom_path <- glue('data/{d}/stream_chemistry/', d = dom)

        site_files <- list.files(dom_path)

        sites <- str_split_fixed(site_files, pattern = '[.]', n = 2)[,1]

        stream_sites <- site_data %>%
            filter(domain == dom,
                   site_type %in% c('stream_gauge')) %>%
            filter(site_name %in% sites) %>%
            pull(site_name)

        all_sites <- tibble()
        for(p in 1:length(stream_sites)) {

            path_chem <- glue("data/{d}/stream_chemistry/{s}.feather",
                              d = dom,
                              s = stream_sites[p])

            path_q <- glue("data/{d}/discharge/{s}.feather",
                           d = dom,
                           s = stream_sites[p])

            path_flux <- glue("data/{d}/stream_flux_inst/{s}.feather",
                              d = dom,
                              s = stream_sites[p])

            if(!file.exists(path_chem)) {
                site_chem <- tibble()
            } else {

                site_chem <- sm(read_feather(path_chem) %>%
                                    mutate(Year = year(datetime),
                                           Month = month(datetime)) %>%
                                    select(-datetime))

                #site_chem <- cleve_var_prefix(site_chem)

                site_chem <- site_chem %>%
                    group_by(site_name, Year, Month, var) %>%
                    summarise(val = mean(val, na.rm = TRUE)) %>%
                    ungroup() %>%
                    mutate(Date = ymd(paste(Year, Month, 1, sep = '-'))) %>%
                    mutate(var = glue('{v}_conc', v = var)) %>%
                    mutate(domain = dom)

            }

                if(!file.exists(path_flux)) {
                    site_flux <- tibble()
                } else {

                    site_flux <- sm(read_feather(path_flux) %>%
                                        mutate(Year = year(datetime),
                                               Month = month(datetime)) %>%
                                        select(-datetime))

                    #site_flux <- cleve_var_prefix(site_flux)

                    site_flux <- site_flux %>%
                        group_by(site_name, Year, Month, var) %>%
                        summarise(val = mean(val, na.rm = TRUE)) %>%
                        mutate(m_factor = case_when(Month %in% c(1,3,5,7,8,10,12) ~ 31,
                                                    Month %in% c(4,6,9,11) ~ 30,
                                                    Month == 2 ~ 28)) %>%
                        mutate(val = val * m_factor) %>%
                        mutate(Date = ymd(paste(Year, Month, 1, sep = '-'))) %>%
                        ungroup() %>%
                        select(-m_factor) %>%
                        mutate(var = glue('{v}_flux', v = var)) %>%
                        mutate(domain = dom)
                    site_flux[is.numeric(site_flux) & site_flux <= 0.00000001] <- NA
                }

                if(file.exists(path_chem) && file.exists(path_flux)){
                    joined <- rbind(site_chem, site_flux)
                }

                if(file.exists(path_chem) &&  !file.exists(path_flux)){
                    joined <- site_chem
                }

                if(!file.exists(path_chem) &&  file.exists(path_flux)){
                    joined <- site_flux
                }

            if(!file.exists(path_q)) {
                site_q <- tibble()
            } else {

                site_q <- sm(read_feather(path_q)) %>%
                    mutate(Year = year(datetime),
                           Month = month(datetime),
                           day = day(datetime))

                #site_q <- cleve_var_prefix(site_q)

                site_q <- site_q %>%
                    group_by(site_name, Year, Month, day) %>%
                    summarise(val = mean(val, na.rm = TRUE)) %>%
                    mutate(NAs = ifelse(is.na(val), 1, 0)) %>%
                    ungroup() %>%
                    group_by(site_name, Year, Month) %>%
                    summarise(val = sum(val*86400/1000, na.rm = TRUE),
                              NAs = sum(NAs, na.rm = TRUE)) %>%
                    mutate(Date = paste(Year, Month, 1, sep = '-')) %>%
                    mutate(Date = ymd(Date)) %>%
                    ungroup() %>%
                    select(-NAs) %>%
                    mutate(domain = dom,
                           var = 'IS_discharge')

                joined <- rbind(joined, site_q)
            }

            all_sites <- rbind.fill(all_sites, joined)
        }

            all_domain <-  rbind.fill(all_domain, all_sites)
    }

    write_feather(all_domain, 'data/general/biplot/month.feather')
}
compute_monthly_summary(network_domain_default_sites)

# These functions should probably be combinded at some point, but for now,
# compute_yearly_summary_ws() appends compute_yearly_summary with ws_traits
compute_yearly_summary <- function(df,
                                   filter_ms_interp = FALSE,
                                   filter_ms_status = FALSE) {

    all_domain <- tibble()
    for(i in 1:nrow(df)) {

        dom <- df$domain[i]

        net <- df$network[i]

        dom_path <- glue('data/{d}/stream_chemistry/', d = dom)

        site_files <- list.files(dom_path)

        sites <- str_split_fixed(site_files, pattern = '[.]', n = 2)[,1]

        stream_sites <- site_data %>%
            filter(domain == dom,
                   site_type == 'stream_gauge') %>%
            filter(site_name %in% sites) %>%
            pull(site_name)

        all_sites <- tibble()

        if(length(stream_sites) == 0) {

        } else{

            for(p in 1:length(stream_sites)) {

                path_chem <- glue("data/{d}/stream_chemistry/{s}.feather",
                                  d = dom,
                                  s = stream_sites[p])

                path_q <- glue("data/{d}/discharge/{s}.feather",
                               d = dom,
                               s = stream_sites[p])

                path_flux <- glue("data/{d}/stream_flux_inst_scaled/{s}.feather",
                                  d = dom,
                                  s = stream_sites[p])

                path_precip <- glue("data/{d}/precipitation/{s}.feather",
                                    d = dom,
                                    s = stream_sites[p])

                path_precip_chem <- glue("data/{d}/precip_chemistry/{s}.feather",
                                         d = dom,
                                         s = stream_sites[p])

                path_precip_flux <- glue("data/{d}/precip_flux_inst_scaled/{s}.feather",
                                         d = dom,
                                         s = stream_sites[p])

                #Stream discharge ####
                if(!file.exists(path_q)) {
                    site_q <- tibble()
                    q_record_length <- 365
                } else {

                    site_q <- sm(read_feather(path_q)) %>%
                        mutate(Year = year(datetime),
                               Month = month(datetime),
                               Day = day(datetime)) %>%
                        filter(Year != year(Sys.Date()))

                    q_record_length <- detrmin_mean_record_length(site_q)

                    if(filter_ms_interp){
                        site_q <- site_q %>%
                            filter(ms_interp == 0)
                    }
                    if(filter_ms_status){
                        site_q <- site_q %>%
                            filter(ms_status == 0)
                    }

                    if(nrow(site_q) == 0){
                        site_q <- tibble()
                        q_record_length <- 365
                    } else{

                        site_q <- site_q %>%
                            group_by(site_name, Year, Month, Day) %>%
                            summarise(val = mean(val, na.rm = T)) %>%
                            ungroup() %>%
                            mutate(Date = ymd(paste(Year, 1, 1, sep = '-'))) %>%
                            mutate(val = val*86400) %>%
                            group_by(site_name, Date, Year) %>%
                            summarise(val = sum(val, na.rm = TRUE),
                                      count = n()) %>%
                            mutate(val = val/1000) %>%
                            mutate(missing = (q_record_length-count)/q_record_length) %>%
                            mutate(missing = ifelse(missing < 0, 0, missing)) %>%
                            ungroup() %>%
                            select(-count) %>%
                            mutate(var = 'discharge') %>%
                            mutate(domain = dom)
                    }

                }

                all_sites <- rbind(all_sites, site_q)

                #Stream chemistry concentration ####
                if(!file.exists(path_chem)) {
                    site_chem <- tibble()
                } else {

                    #first taking a monthly mean and then taking a yearly mean from monthly
                    #mean. This is to try to limit sampling bias, where 100 samples are taken
                    #in the summer but only a few in the winter

                    site_chem <- sm(read_feather(path_chem) %>%
                                        mutate(Year = year(datetime),
                                               Month = month(datetime),
                                               Day = day(datetime)) %>%
                                        select(-datetime)) %>%
                        mutate(var = drop_var_prefix(var)) %>%
                        filter(Year != year(Sys.Date()))

                    if(filter_ms_interp){
                        site_chem <- site_chem %>%
                            filter(ms_interp == 0)
                    }
                    if(filter_ms_status){
                        site_chem <- site_chem %>%
                            filter(ms_status == 0)
                    }

                    site_chem <- site_chem %>%
                        group_by(site_name, Year, Month, Day, var) %>%
                        summarise(val = mean(val, na.rm = TRUE)) %>%
                        ungroup() %>%
                        mutate(Date = ymd(paste(Year, 1, 1, sep = '-'))) %>%
                        select(-Month) %>%
                        group_by(site_name, Date, Year, var) %>%
                        summarise(val = mean(val, na.rm = TRUE),
                                  count = n()) %>%
                        ungroup() %>%
                        mutate(missing = (q_record_length-count)/q_record_length) %>%
                        mutate(missing = ifelse(missing < 0, 0, missing)) %>%
                        select(-count) %>%
                        mutate(var = glue('{v}_conc', v = var)) %>%
                        mutate(domain = dom)
                }

                all_sites <- rbind(all_sites, site_chem)

                #Stream chemistry flux ####
                if(!file.exists(path_flux)) {
                    site_flux <- tibble()
                } else {

                    site_flux <- read_feather(path_flux)  %>%
                        mutate(Year = year(datetime),
                               Month = month(datetime),
                               Day = day(datetime)) %>%
                        select(-datetime) %>%
                        mutate(var = drop_var_prefix(var)) %>%
                        filter(Year != year(Sys.Date()))

                    if(filter_ms_interp){
                        site_flux <- site_flux %>%
                            filter(ms_interp == 0)
                    }
                    if(filter_ms_status){
                        site_flux <- site_flux %>%
                            filter(ms_status == 0)
                    }

                    site_flux <- site_flux %>%
                        group_by(site_name, Year, Month, Day, var) %>%
                        summarise(val = mean(val, na.rm = T)) %>%
                        ungroup() %>%
                        group_by(site_name, Year, var) %>%
                        mutate(Date = ymd(paste(Year, 1, 1, sep = '-'))) %>%
                        ungroup() %>%
                        group_by(site_name, Date, Year, var) %>%
                        summarise(val = sum(val, na.rm = TRUE),
                                  count = n()) %>%
                        ungroup() %>%
                        mutate(missing = (q_record_length-count)/q_record_length) %>%
                        mutate(missing = ifelse(missing < 0, 0, missing)) %>%
                        select(-count) %>%
                        mutate(var = glue('{v}_flux', v = var)) %>%
                        mutate(domain = dom)

                    site_flux[is.numeric(site_flux) & site_flux <= 0.00000001] <- NA
                }

                all_sites <- rbind(all_sites, site_flux)

                #Precipitation ####
                if(!file.exists(path_precip)) {
                    site_precip <- tibble()
                } else {

                    site_precip <- sm(read_feather(path_precip)) %>%
                        mutate(Year = year(datetime),
                               Month = month(datetime),
                               Day = day(datetime)) %>%
                        filter(Year != year(Sys.Date()))

                    if(filter_ms_interp){
                        site_precip <- site_precip %>%
                            filter(ms_interp == 0)
                    }
                    if(filter_ms_status){
                        site_precip <- site_precip %>%
                            filter(ms_status == 0)
                    }

                    site_precip <- site_precip %>%
                        group_by(site_name, Year, Month, Day) %>%
                        summarise(val = sum(val, na.rm = T)) %>%
                        ungroup() %>%
                        mutate(Date = ymd(paste(Year, 1, 1, sep = '-'))) %>%
                        group_by(site_name, Date, Year) %>%
                        summarise(val = sum(val, na.rm = TRUE),
                                  count = n()) %>%
                        ungroup() %>%
                        mutate(missing = (365-count)/365) %>%
                        mutate(missing = ifelse(missing < 0, 0, missing)) %>%
                        select(-count) %>%
                        mutate(var = 'precip') %>%
                        mutate(domain = dom)

                }

                all_sites <- rbind(all_sites, site_precip)

                #Precipitation chemistry concentration ####
                if(!file.exists(path_precip_chem)) {
                    site_precip_chem <- tibble()
                } else {

                    site_precip_chem <- sm(read_feather(path_precip_chem) %>%
                                               mutate(Year = year(datetime),
                                                      Month = month(datetime),
                                                      Day = day(datetime)) %>%
                                               select(-datetime)) %>%
                        mutate(var = drop_var_prefix(var)) %>%
                        filter(Year != year(Sys.Date()))

                    if(filter_ms_interp){
                        site_precip_chem <- site_precip_chem %>%
                            filter(ms_interp == 0)
                    }
                    if(filter_ms_status){
                        site_precip_chem <- site_precip_chem %>%
                            filter(ms_status == 0)
                    }

                    site_precip_chem <- site_precip_chem %>%
                        group_by(site_name, Year, Month, Day, var) %>%
                        summarise(val = mean(val, na.rm = TRUE)) %>%
                        ungroup() %>%
                        mutate(Date = ymd(paste(Year, 1, 1, sep = '-'))) %>%
                        select(-Month) %>%
                        group_by(site_name, Date, Year, var) %>%
                        summarise(val = mean(val, na.rm = TRUE),
                                  count = n()) %>%
                        ungroup() %>%
                        mutate(missing = (356-count)/365) %>%
                        mutate(missing = ifelse(missing < 0, 0, missing)) %>%
                        select(-count) %>%
                        mutate(var = glue('{v}_precip_conc', v = var)) %>%
                        mutate(domain = dom)

                }

                all_sites <- rbind(all_sites, site_precip_chem)

                #Precipitation chemistry flux ####
                if(!file.exists(path_precip_flux)) {
                    site_precip_flux <- tibble()
                } else {

                    site_precip_flux <- read_feather(path_precip_flux)  %>%
                        mutate(Year = year(datetime),
                               Month = month(datetime),
                               Day = day(datetime)) %>%
                        select(-datetime) %>%
                        mutate(var = drop_var_prefix(var)) %>%
                        filter(Year != year(Sys.Date()))

                    if(filter_ms_interp){
                        site_precip_flux <- site_precip_flux %>%
                            filter(ms_interp == 0)
                    }
                    if(filter_ms_status){
                        site_precip_flux <- site_precip_flux %>%
                            filter(ms_status == 0)
                    }

                    site_precip_flux <- site_precip_flux %>%
                        group_by(site_name, Year, Month, Day, var) %>%
                        summarise(val = mean(val, na.rm = T)) %>%
                        ungroup() %>%
                        group_by(site_name, Year, var) %>%
                        mutate(Date = ymd(paste(Year, 1, 1, sep = '-'))) %>%
                        ungroup() %>%
                        group_by(site_name, Date, Year, var) %>%
                        summarise(val = sum(val, na.rm = TRUE),
                                  count = n()) %>%
                        ungroup() %>%
                        mutate(missing = (356-count)/365) %>%
                        mutate(missing = ifelse(missing < 0, 0, missing)) %>%
                        mutate(var = glue('{v}_precip_flux', v = var)) %>%
                        mutate(domain = dom) %>%
                        select(-count)

                    site_precip_flux[is.numeric(site_precip_flux) & site_precip_flux <= 0.00000001] <- NA
                }

                all_sites <- rbind(all_sites, site_precip_flux)
            }
        }

        all_domain <-  rbind.fill(all_domain, all_sites)

    }

    all_domain <- all_domain %>%
        mutate(missing = missing*100) %>%
        mutate(missing = as.numeric(substr(missing, 1, 2))) %>%
        mutate(val = round(val, 4))

    dir.create('data/general/biplot')

    if(filter_ms_interp && filter_ms_status){
        write_feather(all_domain, 'data/general/biplot/year_interp0_status0.feather')
    }

    if(filter_ms_interp && !filter_ms_status){
        write_feather(all_domain, 'data/general/biplot/year_interp0.feather')
    }

    if(!filter_ms_interp && filter_ms_status){
        write_feather(all_domain, 'data/general/biplot/year_status0.feather')
    }

    if(!filter_ms_interp && ! filter_ms_status){
        write_feather(all_domain, 'data/general/biplot/year.feather')
    }
}

compute_yearly_summary_ws <- function(df) {

    all_domain <- tibble()
    for(i in 1:nrow(df)) {

        dom <- df$domain[i]

        net <- df$network[i]

        dom_path <- glue('data/{d}/ws_traits', d = dom)

        prod_files <- list.files(dom_path, full.names = T, recursive = T)

        prod_files <- prod_files[! grepl('raw_', prod_files)]

        all_prods <- tibble()

        if(!length(prod_files) == 0){

            for(p in 1:length(prod_files)) {

                prod_tib <- read_feather(prod_files[p])

                prod_names <- names(prod_tib)

                if(! 'pctCellErr' %in% prod_names){
                    prod_tib <- prod_tib %>%
                        mutate(pctCellErr = NA)
                }

                all_prods <- rbind(all_prods, prod_tib)

            }

            all_prods <- all_prods %>%
                mutate(Date = ymd(paste0(year, '-01', '-01'))) %>%
                mutate(domain = dom) %>%
                rename(Year = year)

            all_domain <- rbind(all_domain, all_prods)
        }
    }

    # Remove standard deviation
    all_ws_vars <- unique(all_domain$var)
    ws_vars_keep <- all_ws_vars[! grepl('sd', all_ws_vars)]
    ws_vars_keep <- ws_vars_keep[! grepl('1992', ws_vars_keep)]

    all_domain <- all_domain %>%
        filter(var %in% !!ws_vars_keep)

    summary_file_paths <- grep('year', list.files('data/general/biplot',
                                                  full.names = TRUE), value = T)

    for(s in 1:length(summary_file_paths)){

        conc_sum <- read_feather(summary_file_paths[s]) %>%
            mutate(pctCellErr = NA)

        all_domain <- all_domain %>%
            mutate(missing = 0)

        final <- rbind(conc_sum, all_domain)

        areas <- site_data %>%
            filter(site_type == 'stream_gauge') %>%
            select(site_name, domain, val = ws_area_ha) %>%
            mutate(Date = NA,
                   Year = NA,
                   var = 'area') %>%
            filter(!is.na(val)) %>%
            mutate(missing = 0) %>%
            mutate(pctCellErr = NA)

        #calc area normalized q
        area_q <- areas %>%
            select(site_name, domain, area = val) %>%
            full_join(., conc_sum, by = c('site_name', 'domain')) %>%
            mutate(discharge_a = ifelse(var == 'discharge', val/(area*10000), NA)) %>%
            mutate(discharge_a = discharge_a*1000) %>%
            filter(!is.na(discharge_a)) %>%
            mutate(var = 'discharge_a') %>%
            select(-val, -area) %>%
            rename(val = discharge_a)

        final <- rbind(final, areas, area_q) %>%
            filter(Year < year(Sys.Date()) | is.na(Year))

        write_feather(final, summary_file_paths[s])
    }
}

compute_yearly_summary(network_domain_default_sites,
                       filter_ms_interp = FALSE,
                       filter_ms_status = FALSE)

compute_yearly_summary_ws(network_domain_default_sites)



#### Switch to simple means ####

# On pause, it is difficulte to consiter removing an unknown number of

# These functions should probably be combinded at some point, but for now,
# compute_yearly_summary_ws() appends compute_yearly_summary with ws_traits
# compute_yearly_summary <- function(df,
#                                    filter_ms_interp = FALSE,
#                                    filter_ms_status = FALSE) {
#
#     all_domain <- tibble()
#     for(i in 1:nrow(df)) {
#
#         dom <- df$domain[i]
#
#         net <- df$network[i]
#
#         dom_path <- glue('data/{d}/stream_chemistry/', d = dom)
#
#         site_files <- list.files(dom_path)
#
#         sites <- str_split_fixed(site_files, pattern = '[.]', n = 2)[,1]
#
#         stream_sites <- site_data %>%
#             filter(domain == dom,
#                    site_type == 'stream_gauge') %>%
#             filter(site_name %in% sites) %>%
#             pull(site_name)
#
#         all_sites <- tibble()
#
#         if(length(stream_sites) == 0) {
#
#         } else{
#
#             for(p in 1:length(stream_sites)) {
#
#                 path_chem <- glue("data/{d}/stream_chemistry/{s}.feather",
#                                   d = dom,
#                                   s = stream_sites[p])
#
#                 path_q <- glue("data/{d}/discharge/{s}.feather",
#                                d = dom,
#                                s = stream_sites[p])
#
#                 path_flux <- glue("data/{d}/stream_flux_inst_scaled/{s}.feather",
#                                   d = dom,
#                                   s = stream_sites[p])
#
#                 path_precip <- glue("data/{d}/precipitation/{s}.feather",
#                                     d = dom,
#                                     s = stream_sites[p])
#
#                 path_precip_chem <- glue("data/{d}/precip_chemistry/{s}.feather",
#                                          d = dom,
#                                          s = stream_sites[p])
#
#                 path_precip_flux <- glue("data/{d}/precip_flux_inst_scaled/{s}.feather",
#                                          d = dom,
#                                          s = stream_sites[p])
#
#                 #Stream discharge ####
#                 if(!file.exists(path_q)) {
#                     site_q <- tibble()
#                     q_record_length <- 365
#                 } else {
#
#                     site_q <- sm(read_feather(path_q)) %>%
#                         mutate(Year = year(datetime),
#                                Month = month(datetime),
#                                Day = day(datetime)) %>%
#                         filter(Year != year(Sys.Date()))
#
#                     if(filter_ms_interp){
#                         site_q <- site_q %>%
#                             filter(ms_interp == 0)
#                     }
#                     if(filter_ms_status){
#                         site_q <- site_q %>%
#                             filter(ms_status == 0)
#                     }
#                     if(nrow(site_q) == 0){
#                         site_q <- tibble()
#                         q_record_length <- 365
#
#                     } else{
#
#                         q_record_length <- detrmin_mean_record_length(site_q)
#
#                         site_q <- site_q %>%
#                             group_by(site_name, Year, Month, Day) %>%
#                             summarise(val = mean(val, na.rm = T)) %>%
#                             ungroup() %>%
#                             mutate(Date = ymd(paste(Year, 1, 1, sep = '-'))) %>%
#                             group_by(site_name, Date, Year) %>%
#                             summarise(val = mean(val, na.rm = TRUE),
#                                       count = n()) %>%
#                             mutate(missing = (q_record_length-count)/q_record_length) %>%
#                             mutate(missing = ifelse(missing < 0, 0, missing)) %>%
#                             ungroup() %>%
#                             select(-count) %>%
#                             mutate(var = 'discharge') %>%
#                             mutate(domain = dom)
#                     }
#
#                 }
#
#                 all_sites <- rbind(all_sites, site_q)
#
#                 #Stream chemistry concentration ####
#                 if(!file.exists(path_chem)) {
#                     site_chem <- tibble()
#                 } else {
#
#                     #first taking a monthly mean and then taking a yearly mean from monthly
#                     #mean. This is to try to limit sampling bias, where 100 samples are taken
#                     #in the summer but only a few in the winter
#
#                     site_chem <- sm(read_feather(path_chem) %>%
#                                         mutate(Year = year(datetime),
#                                                Month = month(datetime),
#                                                Day = day(datetime)) %>%
#                                         select(-datetime)) %>%
#                         mutate(var = drop_var_prefix(var)) %>%
#                         filter(Year != year(Sys.Date()))
#
#                     if(filter_ms_interp){
#                         site_chem <- site_chem %>%
#                             filter(ms_interp == 0)
#                     }
#                     if(filter_ms_status){
#                         site_chem <- site_chem %>%
#                             filter(ms_status == 0)
#                     }
#
#                     site_chem <- site_chem %>%
#                         group_by(site_name, Year, Month, Day, var) %>%
#                         summarise(val = mean(val, na.rm = TRUE)) %>%
#                         ungroup() %>%
#                         mutate(Date = ymd(paste(Year, 1, 1, sep = '-'))) %>%
#                         select(-Month) %>%
#                         group_by(site_name, Date, Year, var) %>%
#                         summarise(val = mean(val, na.rm = TRUE),
#                                   count = n()) %>%
#                         ungroup() %>%
#                         mutate(missing = (q_record_length-count)/q_record_length) %>%
#                         mutate(missing = ifelse(missing < 0, 0, missing)) %>%
#                         select(-count) %>%
#                         mutate(var = glue('{v}_conc', v = var)) %>%
#                         mutate(domain = dom)
#
#                 }
#
#                 all_sites <- rbind(all_sites, site_chem)
#
#                 #Stream chemistry flux ####
#                 if(!file.exists(path_flux)) {
#                     site_flux <- tibble()
#                 } else {
#
#                     site_flux <- read_feather(path_flux)  %>%
#                         mutate(Year = year(datetime),
#                                Month = month(datetime),
#                                Day = day(datetime)) %>%
#                         select(-datetime) %>%
#                         mutate(var = drop_var_prefix(var)) %>%
#                         filter(Year != year(Sys.Date()))
#
#                     if(filter_ms_interp){
#                         site_flux <- site_flux %>%
#                             filter(ms_interp == 0)
#                     }
#                     if(filter_ms_status){
#                         site_flux <- site_flux %>%
#                             filter(ms_status == 0)
#                     }
#
#                     site_flux <- site_flux %>%
#                         group_by(site_name, Year, Month, Day, var) %>%
#                         summarise(val = mean(val, na.rm = T)) %>%
#                         ungroup() %>%
#                         group_by(site_name, Year, var) %>%
#                         mutate(Date = ymd(paste(Year, 1, 1, sep = '-'))) %>%
#                         ungroup() %>%
#                         group_by(site_name, Date, Year, var) %>%
#                         summarise(val = mean(val, na.rm = TRUE),
#                                   count = n()) %>%
#                         ungroup() %>%
#                         mutate(missing = (q_record_length-count)/q_record_length) %>%
#                         mutate(missing = ifelse(missing < 0, 0, missing)) %>%
#                         select(-count) %>%
#                         mutate(var = glue('{v}_flux', v = var)) %>%
#                         mutate(domain = dom)
#
#                     # site_flux[is.numeric(site_flux) & site_flux <= 0.00000001] <- NA
#                 }
#
#                 all_sites <- rbind(all_sites, site_flux)
#
#                 #Precipitation ####
#                 if(!file.exists(path_precip)) {
#                     site_precip <- tibble()
#                 } else {
#
#                     site_precip <- sm(read_feather(path_precip)) %>%
#                         mutate(Year = year(datetime),
#                                Month = month(datetime),
#                                Day = day(datetime)) %>%
#                         filter(Year != year(Sys.Date()))
#
#                     if(filter_ms_interp){
#                         site_precip <- site_precip %>%
#                             filter(ms_interp == 0)
#                     }
#                     if(filter_ms_status){
#                         site_precip <- site_precip %>%
#                             filter(ms_status == 0)
#                     }
#
#                     site_precip <- site_precip %>%
#                         group_by(site_name, Year, Month, Day) %>%
#                         summarise(val = mean(val, na.rm = T)) %>%
#                         ungroup() %>%
#                         mutate(Date = ymd(paste(Year, 1, 1, sep = '-'))) %>%
#                         group_by(site_name, Date, Year) %>%
#                         summarise(val = sum(val, na.rm = TRUE),
#                                   count = n()) %>%
#                         ungroup() %>%
#                         mutate(var = 'precip') %>%
#                         mutate(domain = dom) %>%
#                         mutate(missing = (365-count)/365) %>%
#                         mutate(missing = ifelse(missing < 0, 0, missing)) %>%
#                         select(-count)
#                 }
#
#                 all_sites <- rbind(all_sites, site_precip)
#
#                 #Precipitation chemistry concentration ####
#                 if(!file.exists(path_precip_chem)) {
#                     site_precip_chem <- tibble()
#                 } else {
#
#                     site_precip_chem <- sm(read_feather(path_precip_chem) %>%
#                                                mutate(Year = year(datetime),
#                                                       Month = month(datetime),
#                                                       Day = day(datetime)) %>%
#                                                select(-datetime)) %>%
#                         mutate(var = drop_var_prefix(var)) %>%
#                         filter(Year != year(Sys.Date()))
#
#                     if(filter_ms_interp){
#                         site_precip_chem <- site_precip_chem %>%
#                             filter(ms_interp == 0)
#                     }
#                     if(filter_ms_status){
#                         site_precip_chem <- site_precip_chem %>%
#                             filter(ms_status == 0)
#                     }
#
#                     site_precip_chem <- site_precip_chem %>%
#                         group_by(site_name, Year, Month, Day, var) %>%
#                         summarise(val = mean(val, na.rm = TRUE)) %>%
#                         ungroup() %>%
#                         mutate(Date = ymd(paste(Year, 1, 1, sep = '-'))) %>%
#                         select(-Month) %>%
#                         group_by(site_name, Date, Year, var) %>%
#                         summarise(val = mean(val, na.rm = TRUE),
#                                   n = n()) %>%
#                         ungroup() %>%
#                         mutate(var = glue('{v}_precip_conc', v = var)) %>%
#                         mutate(domain = dom) %>%
#                         mutate(missing = (365-n)/365) %>%
#                         mutate(missing = ifelse(missing < 0, 0, missing)) %>%
#                         select(-n)
#
#                 }
#
#                 all_sites <- rbind(all_sites, site_precip_chem)
#
#                 #Precipitation chemistry flux ####
#                 if(!file.exists(path_precip_flux)) {
#                     site_precip_flux <- tibble()
#                 } else {
#
#                     site_precip_flux <- read_feather(path_precip_flux)  %>%
#                         mutate(Year = year(datetime),
#                                Month = month(datetime),
#                                Day = day(datetime)) %>%
#                         select(-datetime) %>%
#                         mutate(var = drop_var_prefix(var)) %>%
#                         filter(Year != year(Sys.Date()))
#
#                     if(filter_ms_interp){
#                         site_precip_flux <- site_precip_flux %>%
#                             filter(ms_interp == 0)
#                     }
#                     if(filter_ms_status){
#                         site_precip_flux <- site_precip_flux %>%
#                             filter(ms_status == 0)
#                     }
#
#                     site_precip_flux <- site_precip_flux %>%
#                         group_by(site_name, Year, Month, Day, var) %>%
#                         summarise(val = mean(val, na.rm = T)) %>%
#                         ungroup() %>%
#                         group_by(site_name, Year, var) %>%
#                         mutate(Date = ymd(paste(Year, 1, 1, sep = '-'))) %>%
#                         ungroup() %>%
#                         group_by(site_name, Date, Year, var) %>%
#                         summarise(val = mean(val, na.rm = TRUE),
#                                   n = n()) %>%
#                         ungroup() %>%
#                         mutate(var = glue('{v}_precip_flux', v = var)) %>%
#                         mutate(domain = dom) %>%
#                         mutate(missing = (365-n)/365) %>%
#                         mutate(missing = ifelse(missing < 0, 0, missing)) %>%
#                         select(-n)
#
#                     site_precip_flux[is.numeric(site_precip_flux) & site_precip_flux <= 0.00000001] <- NA
#                 }
#
#                 all_sites <- rbind(all_sites, site_precip_flux)
#             }
#         }
#
#         all_domain <-  rbind.fill(all_domain, all_sites)
#
#     }
#
#     all_domain <- all_domain %>%
#         mutate(missing = missing*100) %>%
#         mutate(missing = as.numeric(substr(missing, 1, 2))) %>%
#         mutate(val = round(val, 4))
#
#     dir.create('data/general/biplot')
#
#     if(filter_ms_interp && filter_ms_status){
#         write_feather(all_domain, 'data/general/biplot/year_interp0_status0.feather')
#     }
#
#     if(filter_ms_interp && !filter_ms_status){
#         write_feather(all_domain, 'data/general/biplot/year_interp0.feather')
#     }
#
#     if(!filter_ms_interp && filter_ms_status){
#         write_feather(all_domain, 'data/general/biplot/year_status0.feather')
#     }
#
#     if(!filter_ms_interp && ! filter_ms_status){
#         write_feather(all_domain, 'data/general/biplot/year.feather')
#     }
# }
#
# compute_yearly_summary_ws <- function(df) {
#
#     all_domain <- tibble()
#     for(i in 1:nrow(df)) {
#
#         dom <- df$domain[i]
#
#         net <- df$network[i]
#
#         dom_path <- glue('data/{d}/ws_traits', d = dom)
#
#         prod_files <- list.files(dom_path, full.names = T, recursive = T)
#
#         prod_files <- prod_files[! grepl('raw_', prod_files)]
#
#         all_prods <- tibble()
#
#         if(!length(prod_files) == 0){
#
#             for(p in 1:length(prod_files)) {
#
#                 prod_tib <- read_feather(prod_files[p])
#
#                 prod_names <- names(prod_tib)
#
#                 if(! 'pctCellErr' %in% prod_names){
#                     prod_tib <- prod_tib %>%
#                         mutate(pctCellErr = NA)
#                 }
#
#                 all_prods <- rbind(all_prods, prod_tib)
#
#             }
#
#             all_prods <- all_prods %>%
#                 mutate(Date = ymd(paste0(year, '-01', '-01'))) %>%
#                 mutate(domain = dom) %>%
#                 rename(Year = year)
#
#             all_domain <- rbind(all_domain, all_prods)
#         }
#     }
#
#     summary_file_paths <- grep('year', list.files('data/general/biplot',
#                                                   full.names = TRUE), value = T)
#
#     for(s in 1:length(summary_file_paths)){
#
#         conc_sum <- read_feather(summary_file_paths[s]) %>%
#             mutate(pctCellErr = NA)
#
#         all_domain <- all_domain %>%
#             mutate(missing = 0)
#
#         final <- rbind(conc_sum, all_domain)
#
#         areas <- site_data %>%
#             filter(site_type == 'stream_gauge') %>%
#             select(site_name, domain, val = ws_area_ha) %>%
#             mutate(Date = NA,
#                    Year = NA,
#                    var = 'area') %>%
#             filter(!is.na(val)) %>%
#             mutate(missing = 0) %>%
#             mutate(pctCellErr = NA)
#
#         #calc area normalized q
#         area_q <- areas %>%
#             select(site_name, domain, area = val) %>%
#             full_join(., conc_sum, by = c('site_name', 'domain')) %>%
#             mutate(discharge_a = ifelse(var == 'discharge', ((val*31556952)/1000)/(area*10000), NA)) %>%
#             mutate(discharge_a = discharge_a*1000) %>%
#             filter(!is.na(discharge_a)) %>%
#             mutate(var = 'discharge_a') %>%
#             select(-val, -area) %>%
#             rename(val = discharge_a)
#
#         final <- rbind(final, areas, area_q) %>%
#             filter(Year < year(Sys.Date()) | is.na(Year))
#
#         write_feather(final, summary_file_paths[s])
#     }
# }
#
# compute_yearly_summary(network_domain_default_sites,
#                        filter_ms_interp = FALSE,
#                        filter_ms_status = FALSE)
#
# compute_yearly_summary_ws(network_domain_default_sites)
#
#

