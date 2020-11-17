 df <- domains_df
 agg <- 'month'
 i <- 2
 p <-2

 compute_monthly_summary <- function(df) {
    for(i in 1:nrow(df)) {
        
        dom <- df$domain[i]
        
        net <- df$network[i]
        
        dom_path <- glue('data/{d}/chemistry/', d = dom)
        
        site_files <- list.files(dom_path)
        
        sites <- str_split_fixed(site_files, pattern = '[.]', n = 2)[,1]
        
        stream_sites <- site_data %>%
            filter(domain == dom, 
                   site_type == 'stream_gauge') %>%
            filter(site_name %in% sites) %>%
            pull(site_name)
        
        for(p in 1:length(stream_sites)) {
            
            # path <- glue("data/{n}/{d}/chemistry/{s}.feather",
            #              d = site_data$domain[i],
            #              n = site_data$network[i],
            #              s = site_data$site_name[i])
            
            path_chem <- glue("data/{d}/chemistry/{s}.feather",
                              d = dom,
                              s = stream_sites[p])
            
            path_q <- glue("data/{d}/discharge/{s}.feather",
                           d = dom,
                           s = stream_sites[p])
            
            path_flux <- glue("data/{d}/flux/{s}.feather",
                              d = dom,
                              s = stream_sites[p])
            
            site_chem <- sm(read_feather(path_chem) %>%
                                mutate(Year = year(datetime),
                                       Month = month(datetime)) %>%
                                select(-datetime))
            
            site_chem <- site_chem %>%
                group_by(site_name, Year, Month) %>%
                summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
                ungroup() %>%
                mutate(Date = ymd(paste(Year, Month, 1, sep = '-'))) %>%
                mutate(domain = dom)
            
            if(dom == 'neon') {
                joined <- site_chem
            } else {
            
            site_flux <- sm(read_feather(path_flux) %>%
                                mutate(Year = year(datetime),
                                       Month = month(datetime)) %>%
                                select(-datetime)) %>%
                group_by(site_name, Year, Month) %>%
                summarise(across(where(is.numeric), ~ (mean(.x, na.rm = TRUE)), .names = '{col}_flux')) %>%
                mutate(m_factor = case_when(Month %in% c(1,3,5,7,8,10,12) ~ 31,
                                            Month %in% c(4,6,9,11) ~ 30,
                                            Month == 2 ~ 28)) %>%
                mutate(across(contains('flux'), ~.x * m_factor)) %>%
                mutate(Date = ymd(paste(Year, Month, 1, sep = '-'))) %>%
                ungroup() %>%
                select(-m_factor)
            site_flux[is.numeric(site_flux) & site_flux <= 0.00000001] <- NA 
            
            joined <- full_join(site_chem, site_flux)
            
            site_q <- sm(read_feather(path_q))
                         
            if(dom == 'hbef') {
                site_q <- site_q %>%
                    rename(Q = discharge) 
            }
            
            site_q <- sm(site_q %>%
                             mutate(Year = year(datetime),
                                    Month = month(datetime),
                                    day = day(datetime)) %>%
                             group_by(site_name, Year, Month, day) %>%
                             summarise(Q = mean(Q, na.rm = TRUE)) %>%
                             mutate(NAs = ifelse(is.na(Q), 1, 0))) %>%
                ungroup() %>%
                group_by(site_name, Year, Month) %>%
                summarise(Q = sum(Q*86400/1000, na.rm = TRUE),
                          NAs = sum(NAs, na.rm = TRUE)) %>%
                mutate(Date = paste(Year, Month, 1, sep = '-')) %>%
                mutate(Date = ymd(Date)) %>%
                ungroup()
            
            joined <- full_join(joined, site_q) %>%
                mutate(domain = dom)
            }
            
            if(p == 1) {
                all_sites <- joined %>%
                    filter(site_name == 'fake')
            }
            
            all_sites <- rbind(all_sites, joined)
        }
        
        if(i == 1) {
            all_domain <- all_sites %>%
                filter(site_name == 'fake')
        }
            
            all_domain <-  rbind.fill(all_domain, all_sites)
    }
    
    write_feather(all_domain, 'data/biplot/month.feather')
}

#compute_monthly_summary(domains_df)

compute_yearly_summary <- function(df) {
    for(i in 1:nrow(df)) {
        
        dom <- df$domain[i]
        
        net <- df$network[i]
        
        dom_path <- glue('data/{d}/chemistry/', d = dom)
        
        site_files <- list.files(dom_path)
        
        sites <- str_split_fixed(site_files, pattern = '[.]', n = 2)[,1]
        
        stream_sites <- site_data %>%
            filter(domain == dom, 
                   site_type == 'stream_gauge') %>%
            filter(site_name %in% sites) %>%
            pull(site_name)
        
        traits_path <- glue('data/{d}/ws_traits',
                            d = dom)
        trait_files <- list.files(traits_path, full.names = TRUE)
        
        if(length(trait_files) != 0) {
        
        traits_final <- read_feather(trait_files[1]) %>%
            rename(Year = year)
        
        for(t in 2:length(trait_files)) {
            trat <- read_feather(trait_files[t]) %>%
                rename(Year = year)
            
            traits_final <- full_join(traits_final, trat, by = c('Year', 'site_name')) 
        }
        }
        
        for(p in 1:length(stream_sites)) {
            
            # path <- glue("data/{n}/{d}/chemistry/{s}.feather",
            #              d = site_data$domain[i],
            #              n = site_data$network[i],
            #              s = site_data$site_name[i])
            
            path_chem <- glue("data/{d}/chemistry/{s}.feather",
                              d = dom,
                              s = stream_sites[p])
            
            path_q <- glue("data/{d}/discharge/{s}.feather",
                           d = dom,
                           s = stream_sites[p])
            
            path_flux <- glue("data/{d}/flux/{s}.feather",
                              d = dom,
                              s = stream_sites[p])
            
            site_chem <- sm(read_feather(path_chem) %>%
                                mutate(Year = year(datetime),
                                       Month = month(datetime)) %>%
                                select(-datetime))
            
            site_chem <- site_chem %>%
                group_by(site_name, Year, Month) %>%
                summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
                ungroup() %>%
                mutate(Date = ymd(paste(Year, 1, 1, sep = '-'))) %>%
                select(-Month) %>%
                group_by(site_name, Date) %>%
                summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
                ungroup() %>%
                mutate(domain = dom)
            
            if(dom == 'neon') {
                joined <- site_chem
            } else {
            
                site_flux <- read_feather(path_flux) %>%
                                    mutate(Year = year(datetime),
                                           Month = month(datetime)) %>%
                                    select(-datetime) %>%
                    group_by(site_name, Year, Month) %>%
                    summarise(across(where(is.numeric), ~ (mean(.x, na.rm = TRUE)), .names = '{col}_flux')) %>%
                    mutate(Date = ymd(paste(Year, 1, 1, sep = '-'))) %>%
                    ungroup() %>%
                    select(-Month) %>%
                    group_by(site_name, Date, Year) %>%
                    summarise(across(contains('flux'), ~ (mean(.x, na.rm = TRUE))*365)) %>%
                    ungroup()
                site_flux[is.numeric(site_flux) & site_flux <= 0.00000001] <- NA 
                
                joined <- full_join(site_chem, site_flux)
                
                site_q <- sm(read_feather(path_q))
                
                if(dom == 'hbef') {
                    site_q <- site_q %>%
                        rename(Q = discharge) 
                }
                site_q <- sm(site_q %>%
                                 mutate(Year = year(datetime),
                                        Month = month(datetime),
                                        day = day(datetime)) %>%
                                 group_by(site_name, Year, Month, day) %>%
                                 summarise(Q = mean(Q, na.rm = TRUE)) %>%
                                 mutate(NAs = ifelse(is.na(Q), 1, 0))) %>%
                    ungroup() %>%
                    mutate(Date = ymd(paste(Year, 1, 1, sep = '-'))) %>%
                    select(-Year, -Month) %>%
                    group_by(site_name, Date) %>%
                    summarise(Q = sum(Q*86400/1000, na.rm = TRUE),
                              NAs = sum(NAs, na.rm = TRUE)) %>%
                    ungroup()
                
                joined <- full_join(joined, site_q)
            }
            
            if(p == 1) {
                all_sites <- joined %>%
                    filter(site_name == 'fake')
            }
            
            all_sites <- rbind(all_sites, joined)
        }
        
        all_sites <- all_sites %>%
            full_join(traits_final, by = c('Year', 'site_name'))
        
        if(i == 1) {
            all_domain <- all_sites %>%
                filter(site_name == 'fake')
        }
        
        all_domain <-  rbind.fill(all_domain, all_sites) 
        
    }
    
    write_feather(all_domain, 'data/biplot/year.feather')
}

compute_yearly_summary(domains_df)

look <- read_feather('data/biplot/year.feather')
