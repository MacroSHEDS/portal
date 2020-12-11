 df <- network_domain_default_sites
 agg <- 'month'
 i <- 2
 p <-2

cleve_var_prefix <- function(df){

     df <- df %>%
         mutate(var_ = str_split(var, '_', n = Inf))
     for(f in 1:nrow(df)){

         df[f, 'var'] <- paste(pull(df[f, 'var_'])[[1]][-1], collapse = '_')
     }

     df <- df %>%
         select(-var_)

     return(df)
 }

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

    write_feather(all_domain, 'data/biplot/month.feather')
}


compute_yearly_summary_ws <- function(df) {

    all_domain <- tibble()
    for(i in 1:nrow(df)) {
        print(paste0('i = ', i))
        dom <- df$domain[i]

        net <- df$network[i]

        dom_path <- glue('data/{d}/ws_traits', d = dom)

        prod_files <- list.files(dom_path, full.names = T, recursive = T)

        all_prods <- tibble()

        if(!length(prod_files) == 0){


            for(p in 1:length(prod_files)) {
                print(p)

                if(str_split_fixed(prod_files[p], '/', n = Inf)[1,4] %in% c('prism_precip', 'prism_temp_mean')) {
                    prod_tib <- tibble()
                } else {
                    prod_tib <- read_feather(prod_files[p])
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

    conc_sum <- read_feather('data/biplot/year.feather')

    final <- rbind(conc_sum, all_domain)
    write_feather(final, 'data/biplot/year.feather')
}

compute_yearly_summary_ws(network_domain_default_sites)

compute_monthly_summary(network_domain_default_sites)

compute_yearly_summary <- function(df) {

    all_domain <- tibble()
    for(i in 1:nrow(df)) {
        print(paste0('i = ', i))

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
        for(p in 1:length(stream_sites)) {
            print(paste('p = ',p))

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

                #first taking a monthly mean and then taking a yearly mean from monthly
                #mean. This is to try to limit sampling bias, where 100 samples are taken
                #in the summer but only a few in the winter

                site_chem <- sm(read_feather(path_chem) %>%
                                    mutate(Year = year(datetime),
                                           Month = month(datetime)) %>%
                                    select(-datetime))

                #site_chem <- cleve_var_prefix(site_chem)

                site_chem <- site_chem %>%
                    group_by(site_name, Year, Month, var) %>%
                    summarise(val = mean(val, na.rm = TRUE)) %>%
                    ungroup() %>%
                    mutate(Date = ymd(paste(Year, 1, 1, sep = '-'))) %>%
                    select(-Month) %>%
                    group_by(site_name, Date, Year, var) %>%
                    summarise(val = mean(val, na.rm = TRUE)) %>%
                    ungroup() %>%
                    mutate(var = glue('{v}_conc', v = var)) %>%
                    mutate(domain = dom)
            }

                if(!file.exists(path_flux)) {
                    site_flux <- tibble()
                } else {

                    site_flux <- read_feather(path_flux)  %>%
                        mutate(Year = year(datetime),
                               Month = month(datetime)) %>%
                        select(-datetime)

                    #site_flux <- cleve_var_prefix(site_flux)

                    site_flux <- site_flux %>%
                        group_by(site_name, Year, Month, var) %>%
                        summarise(val = mean(val, na.rm = TRUE)) %>%
                        mutate(Date = ymd(paste(Year, 1, 1, sep = '-'))) %>%
                        ungroup() %>%
                        select(-Month) %>%
                        group_by(site_name, Date, Year, var) %>%
                        summarise(val = mean(val, na.rm = TRUE)*365) %>%
                        ungroup() %>%
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

                    site_q <- sm(read_feather(path_q) %>%
                                     mutate(Year = year(datetime),
                                            Month = month(datetime),
                                            day = day(datetime)))

                    #site_q <- cleve_var_prefix(site_q)

                    site_q <- site_q %>%
                                     group_by(site_name, Year, Month, day) %>%
                                     summarise(val = mean(val, na.rm = TRUE)) %>%
                                     mutate(NAs = ifelse(is.na(val), 1, 0)) %>%
                        ungroup() %>%
                        mutate(Date = ymd(paste(Year, 1, 1, sep = '-'))) %>%
                        select(-Month) %>%
                        group_by(site_name, Date, Year) %>%
                        summarise(val = sum(val, na.rm = TRUE),
                                  NAs = sum(NAs, na.rm = TRUE)) %>%
                        mutate(val = val*86400/1000) %>%
                        ungroup() %>%
                        select(-NAs) %>%
                        mutate(var = 'IS_discharge') %>%
                        mutate(domain = dom)

                    joined <- rbind(joined, site_q)
                }

            all_sites <- rbind(all_sites, joined)
        }

        all_domain <-  rbind.fill(all_domain, all_sites)

    }

    write_feather(all_domain, 'data/biplot/year.feather')
}

compute_yearly_summary(network_domain_default_sites)

look <- read_feather('data/biplot/year.feather')

#
# look_new <- cleve_var_prefix(look)
#
# write_feather(look_new, 'data/biplot/month.feather')
