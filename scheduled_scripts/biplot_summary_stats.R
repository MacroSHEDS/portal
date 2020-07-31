#  df <- domains_df
#  agg <- 'month'
#  i <- 2
#  p <-1

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
                                mutate(year = year(datetime),
                                       month = month(datetime)) %>%
                                select(-datetime))
            
            site_chem <- site_chem %>%
                group_by(site_name, year, month) %>%
                summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
                ungroup() %>%
                mutate(date = paste(year, month, 1, sep = '-')) %>%
                mutate(month_an = as.numeric(ifelse(month %in% c(1,2,3,4,5,6,7,8,9), paste0(year, '.', 0, month), paste0(year, '.', month)))) %>%
                mutate(date = ymd(date)) %>%
                mutate(domain = dom)
            
            if(dom == 'neon') {
                joined <- site_chem
            } else {
            
            site_flux <- sm(read_feather(path_flux) %>%
                                mutate(year = year(datetime),
                                       month = month(datetime)) %>%
                                mutate(m_factor = case_when(month %in% c(1,3,5,7,8,10,12) ~ 31,
                                                            month %in% c(4,6,9,11) ~ 30,
                                                            month == 2 ~ 28)) %>%
                                select(-datetime)) %>%
                group_by(site_name, year, month) %>%
                summarise(across(where(is.numeric), ~ (mean(.x, na.rm = TRUE))*m_factor, .names = '{col}_flux')) %>%
                mutate(date = paste(year, month, 1, sep = '-')) %>%
                mutate(date = ymd(date)) %>%
                mutate(month_an = as.numeric(ifelse(month %in% c(1,2,3,4,5,6,7,8,9), paste0(year, '.', 0, month), paste0(year, '.', month))))%>%
                ungroup() %>%
                select(-m_factor_flux)
            site_flux[is.numeric(site_flux) & site_flux <= 0.00000001] <- NA 
            
            joined <- full_join(site_chem, site_flux)
            
            site_q <- sm(read_feather(path_q) %>%
                             mutate(year = year(datetime),
                                    month = month(datetime),
                                    day = day(datetime)) %>%
                             group_by(site_name, year, month, day) %>%
                             summarise(Q = mean(Q, na.rm = TRUE)) %>%
                             mutate(NAs = ifelse(is.na(Q), 1, 0))) %>%
                ungroup() %>%
                group_by(site_name, year, month) %>%
                summarise(Q = sum(Q*86400/1000, na.rm = TRUE),
                          NAs = sum(NAs, na.rm = TRUE)) %>%
                mutate(date = paste(year, month, 1, sep = '-')) %>%
                mutate(date = ymd(date)) %>%
                mutate(month_an = as.numeric(ifelse(month %in% c(1,2,3,4,5,6,7,8,9), paste0(year, '.', 0, month), paste0(year, '.', month)))) %>%
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

# compute_monthly_summary(domains_df)

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
                                mutate(year = year(datetime),
                                       month = month(datetime)) %>%
                                select(-datetime))
            
            site_chem <- site_chem %>%
                group_by(site_name, year, month) %>%
                summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
                ungroup() %>%
                mutate(date = ymd(paste(year, 1, 1, sep = '-'))) %>%
                group_by(site_name, date) %>%
                summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
                ungroup() %>%
                mutate(domain = dom)
            
            if(dom == 'neon') {
                joined <- site_chem
            } else {
                
                site_flux <- sm(read_feather(path_flux) %>%
                                    mutate(year = year(datetime),
                                           month = month(datetime)) %>%
                                    mutate(m_factor = case_when(month %in% c(1,3,5,7,8,10,12) ~ 31,
                                                                month %in% c(4,6,9,11) ~ 30,
                                                                month == 2 ~ 28)) %>%
                                    select(-datetime)) %>%
                    group_by(site_name, year, month) %>%
                    summarise(across(where(is.numeric), ~ (mean(.x, na.rm = TRUE))*m_factor, .names = '{col}_flux')) %>%
                    mutate(date = ymd(paste(year, 1, 1, sep = '-'))) %>%
                    select(-year, -month) %>%
                    group_by(site_name, date) %>%
                    summarise(across(where(is.numeric), ~ (mean(.x, na.rm = TRUE))*365)) %>%
                    ungroup()
                site_flux[is.numeric(site_flux) & site_flux <= 0.00000001] <- NA 
                
                joined <- full_join(site_chem, site_flux)
                
                site_q <- sm(read_feather(path_q) %>%
                                 mutate(year = year(datetime),
                                        month = month(datetime),
                                        day = day(datetime)) %>%
                                 group_by(site_name, year, month, day) %>%
                                 summarise(Q = mean(Q, na.rm = TRUE)) %>%
                                 mutate(NAs = ifelse(is.na(Q), 1, 0))) %>%
                    ungroup() %>%
                    mutate(date = ymd(paste(year, 1, 1, sep = '-'))) %>%
                    select(-year, -month) %>%
                    group_by(site_name, date) %>%
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
        
        if(i == 1) {
            all_domain <- all_sites %>%
                filter(site_name == 'fake')
        }
        
        all_domain <-  rbind.fill(all_domain, all_sites)
    }
    
    write_feather(all_domain, 'data/biplot/year.feather')
}

# compute_yearly_summary(domains_df)


