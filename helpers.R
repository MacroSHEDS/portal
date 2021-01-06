sm = suppressMessages
sw = suppressWarnings

extract_from_config = function(key){
    ind = which(lapply(conf, function(x) grepl(key, x)) == TRUE)
    val = stringr::str_match(conf[ind], '.*\\"(.*)\\"')[2]
    return(val)
}

get_ylab <- function(v,
                    conc_flux,
                    conc_unit,
                    flux_unit){

    yunit <- ifelse(conc_flux == 'Flux',
                    flux_unit,
                    conc_unit)

    if(conc_flux == 'Flux'){
        unit <- yunit
    } else {
        unit <- ifelse(v %in% conc_vars,
                       yunit,
                       chemvars$unit[chemvars$variable_code == v])
    }

    ylab <- glue('{var} ({u})',
                 var = v,
                 u = unit)

    return(ylab)
}

# datelims=dates; mainlab=colnames(alldata)[-1]; plotgroup='nSiteNVar'; ylab=ylab; px_per_lab=20
plot_empty_dygraph = function(datelims, mainlab='', maindiv=NULL, plotgroup,
    ylab, px_per_lab){

    datelims = as.POSIXct(datelims)
    dateseq = seq(datelims[1], datelims[2], by='day')

    nrows = length(dateseq)
    ncols = length(mainlab)
    emptydat = matrix(rep(rep(0, length.out=nrows), times=ncols), ncol=ncols)
    emptydat = xts(emptydat, order.by=dateseq, tzone='UTC')
    dimnames(emptydat) = list(NULL, mainlab)

    dg = dygraph(emptydat, group=plotgroup) %>%
        dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
            colors='transparent', retainDateWindow=TRUE) %>%
        dyAxis('y', label=ylab, labelWidth=16, labelHeight=10,
            pixelsPerLabel=px_per_lab, rangePad=10)

    if(! is.null(maindiv)){
        dg = dg %>%
            dyLegend(show='always', labelsSeparateLines=FALSE,
                labelsDiv=maindiv)
    }

    return(dg)
}

get_timeslider_extent = function(basedata, selected_daterange){

    if(nrow(basedata$chem)){
        dset = basedata$chem
    } else if(nrow(basedata$Q)){
        dset = basedata$Q
    } else if(nrow(basedata$P)){
        dset = basedata$P
    } else {
        dset = tibble(datetime=selected_daterange)
    }

    dtrng = dset %>%
        mutate(datetime = as.Date(datetime)) %>%
        pull(datetime) %>%
        range(., na.rm=TRUE)

    return(dtrng)
}

parse_molecular_formulae = function(formulae){

    #`formulae` is a vector

    # formulae = c('C', 'C4', 'Cl', 'Cl2', 'CCl', 'C2Cl', 'C2Cl2', 'C2Cl2B2')
    # formulae = 'BCH10He10PLi2'
    # formulae='Mn'

    conc_vars = str_match(formulae, '^(?:OM|TM|DO|TD|UT|UTK|TK)?([A-Za-z0-9]+)_?')[,2]
    two_let_symb_num = str_extract_all(conc_vars, '([A-Z][a-z][0-9]+)')
    conc_vars = str_remove_all(conc_vars, '([A-Z][a-z][0-9]+)')
    one_let_symb_num = str_extract_all(conc_vars, '([A-Z][0-9]+)')
    conc_vars = str_remove_all(conc_vars, '([A-Z][0-9]+)')
    two_let_symb = str_extract_all(conc_vars, '([A-Z][a-z])')
    conc_vars = str_remove_all(conc_vars, '([A-Z][a-z])')
    one_let_symb = str_extract_all(conc_vars, '([A-Z])')

    constituents = mapply(c, SIMPLIFY=FALSE,
        two_let_symb_num, one_let_symb_num, two_let_symb, one_let_symb)

    return(constituents) # a list of vectors
}

combine_atomic_masses = function(molecular_constituents){

    #`molecular_constituents` is a vector

    xmat = str_match(molecular_constituents,
        '([A-Z][a-z]?)([0-9]+)?')[, -1, drop=FALSE]
    elems = xmat[,1]
    mults = as.numeric(xmat[,2])
    mults[is.na(mults)] = 1
    molecular_mass = sum(PeriodicTable::mass(elems) * mults)

    return(molecular_mass) #a scalar
}

convert_conc_units <- function(d,
                               input_unit = 'mg/L',
                               desired_unit){

    #input_unit is the unit of concs (must be 'mg/L')
    #desired unit is one of the keys in the call to `switch` below

    cnms <- colnames(d)

    varnames <- str_match(string = cnms[grepl('val_', cnms)],
                          pattern = 'val_(.+)')[, 2]

    varnames <- varnames[varnames %in% conc_vars]

    if(! length(varnames)) return(d)

    require(PeriodicTable)

    if(grepl('g', desired_unit)){

        conv_factor <- switch(desired_unit,
                              'ng/L' = 1000000,
                              'ug/L' = 1000,
                              'mg/L' = 1,
                              'g/L' = 0.001)

    } else {

        constituents <- parse_molecular_formulae(varnames)
        molar_masses <- sapply(constituents, combine_atomic_masses)

        conv_factors <- switch(substr(desired_unit, 0, 1),
            'n' = molar_masses * 1000000, #desired unit could be nM or neq/L
            'u' = molar_masses * 1000, #and so on...
            'm' = molar_masses,
            'M' = molar_masses / 1000, #desired unit must be 'M'
            'e' = molar_masses / 1000) #desired unit must be 'eq/L'

        if(grepl('q', desired_unit)){
            valences <- variables$valence[variables$variable_code %in% varnames]
            conv_factors <- conv_factors * valences
        }

        # converted = data.frame(mapply(`*`, conc_df, mm_scaled, SIMPLIFY=FALSE))
    }

    for(vn in varnames){

        vn <- paste0('val_', vn)
        d <- mutate(d,
                    !!vn := !!sym(vn) * conv_factor)
                    # across(! starts_with('ms_') & ! matches('datetime'),
                    #       ~(. * conv_factor)))
    }

    return(d)
}

convert_flux_units = function(d,
                              input_unit = 'kg/ha/d',
                              desired_unit){

    #d is a data frame or tibble of numeric flux data
    #input_unit is the unit of flux (must be 'kg/ha/d')
    #desired unit is one of the keys in the call to `switch` below

    cnms <- colnames(d)

    varname <- str_match(string = cnms[grepl('ms_status_', cnms)],
                         pattern = 'ms_status_(.+)')[, 2]

    if(! varname %in% conc_vars) return(d)

    conv_factor <- switch(desired_unit,
        'Mg/ha/d' = 0.001,
        'kg/ha/d' = 1,
        'g/ha/d' = 1000,
        'mg/ha/d' = 1000000)

    d <- mutate(d,
                across(! starts_with('ms_') & ! matches('datetime'),
                       ~(. * conv_factor)))

    return(d)
}

# log10_ceiling = function(x) {
#     10^(ceiling(log10(x)))
# }

pad_ts <- function(d,
                   vars,
                   datebounds){

    dtcol <- as.POSIXct(datebounds,
                        tz = 'UTC') %>%
        lubridate::with_tz(lubridate::tz(d$datetime[1]))

    pad_rows <- d[c(1, nrow(d)), ]

    ms_cols <- grepl(pattern = '(?:P_)?ms_(status|interp)_',
                     x = colnames(pad_rows))

    pad_rows[, ms_cols] <- 0
    pad_rows[, ! ms_cols] <- NA
    pad_rows$datetime <- dtcol

    d_padded <- bind_rows(d, pad_rows)

    # if('site_name' %in% colnames(d)){
    #     spatial_unit = 'site_name'
    #     sites_or_dmns = unique(d$site_name)
    # } else if('domain' %in% colnames(d)){
    #     spatial_unit = 'domain'
    #     sites_or_dmns = unique(d$domain)
    # } else{ #probably never run, but here for unforseen back-compatibility needs
    #     spatial_unit = 'site_name'
    #     sites_or_dmns = 'placeholder'
    # }

    # cnms <- colnames(d)
    # sites <- cnms[! grepl(pattern = '(?:ms_|datetime)',
    #                       x = cnms,
    #                       perl = TRUE)]
    # nsites <- length(sites)
    # nvars <- length(vars)
    #
    # dt_pad_rows <- tibble(a = rep(x = as.POSIXct(datebounds),
    #                               times = nsites),
    #                       b = rep(sites, each=2))
    #
    # dt_pad_rows <- dplyr::bind_cols(dt_pad_rows,
    #                                 as.data.frame(matrix(NA_real_,
    #                                                      ncol = nvars,
    #                                                      nrow = nsites * 2)))
    #
    # colnames(dt_pad_rows) = c('datetime', spatial_unit, vars)
    # dt_pad_rows$datetime = lubridate::force_tz(dt_pad_rows$datetime, 'UTC')
    #
    #
    # df_padded = d %>%
    #     bind_rows(dt_pad_rows)

    # if(sites_or_dmns[1] == 'placeholder'){
    #     df_padded = select(df_padded, -site_name)
    # }

    return(d_padded)
}

# r=raindata; l=streamdata#
left_forward_rolljoin = function(l, r){

    r = as.data.table(r)
    l = as.data.table(l)
    setkey(r, 'datetime')
    setkey(l, 'datetime')

    alldf = r[l, roll=TRUE]
    alldf = as_tibble(alldf)

    #prevent excessive forward extrapolating of rain vars
    if(nrow(r)){

        gratuitous_end_roll_r = l$datetime > r$datetime[nrow(r) - 1]

        if(sum(gratuitous_end_roll_r, na.rm=TRUE) == 1){
            gratuitous_end_roll_r[length(gratuitous_end_roll_r)] = FALSE
        }

        if(nrow(alldf)){
            alldf[gratuitous_end_roll_r, ! colnames(alldf) == 'datetime'] = NA
        }
    }

    return(alldf)
}

sites_from_feathers = function(directory){
    sitenames = unlist(strsplit(list.files(directory), '.feather'))
    return(sitenames)
}

sites_by_var = function(var){

    sitelist = list()
    for(i in 1:length(domains_pretty)){
        psitevec = sites_from_feathers(glue('data/{d}/{v}',
            d=domains_pretty[i], v=var))
        sitelist = append(sitelist, list(psitevec))
        names(sitelist)[i] = domains_pretty[i]
    }

    return(sitelist)
}

most_recent_year = function(date_range){

    # if(is.null(date_range)) return(as.POSIXct(c(NA, NA)))

    mry = c(
        max(date_range[2] - lubridate::days(365),
            date_range[1],
            na.rm=TRUE),
        date_range[2]
    )

    return(mry)
}

get_sitelist <- function(domain, type){

    #type is one or more of the types listed in site_data.csv, e.g. 'stream_gauge'

    sitelist <- site_data %>%
        filter(domain == !!domain,
               # network == !!network, #we should eventually observe hierarchy all the way up to the network
               site_type %in% !!type) %>%
        pull(site_name)

    return(sitelist)
}

try_read_feather <- function(path){

    out <- try(feather::read_feather(path),
               silent = TRUE)

    if('try-error' %in% class(out)){
        out <- tibble()
    } else {
        out <- out %>%
            mutate(val = errors::set_errors(val,
                                            val_err)) %>%
            select(-val_err)
    }

    return(out)
}

# var='precip'; dmns=c('hbef', 'neon'); sites=c('CUPE', 'W1', 'BIGC')
read_combine_feathers <- function(var, dmns, sites = NULL){

    #in order to allow duplicate sitenames across domains, must invoke js here
    #see ms_todo

    #in case duplicate sitenames do appear, this will make it a bit less
    #likely that there's a collision. this can be simplified if js solution
    #is implemented
    dmn_sites <- site_data %>%
        filter(domain %in% dmns, site_type %in% c('stream_gauge', 'stream_sampling_point')) %>%
        filter(site_name %in% sites) %>%
        select(domain, site_name)

    combined_data <- tibble()
    for(i in 1:nrow(dmn_sites)){

        filestr <- glue('data/{d}/{v}/{s}.feather',
                        d = dmn_sites$domain[i],
                        v = var,
                        s = dmn_sites$site_name[i])

        data_part <- try_read_feather(filestr)

        # if(var %in% c('precip', 'pchem')) data_part$domain = dmn_sites$domain[i]

        combined_data <- bind_rows(combined_data,
                                   data_part)
    }

    return(combined_data)
}

generate_dropdown_varlist = function(chemvars, filter_set=NULL){

    if(! is.null(filter_set)){
        chemvars = filter(chemvars, variable_code %in% filter_set)
    }

    chemvars = chemvars %>%
        mutate(displayname=paste0(variable_name, ' (', unit, ')')) %>%
        select(displayname, variable_code, variable_subtype) %>%
        plyr::dlply(plyr::.(variable_subtype), function(x){
            plyr::daply(x, plyr::.(displayname), function(y){
                y['variable_code']
            })
        })

    return(chemvars)
}

filter_dropdown_varlist = function(filter_set, vartype = 'stream'){

    # filter_set = sw(select(filter_set, -one_of('site_name', 'datetime')))

    if(nrow(filter_set) == 0){
        return(list(Anions = c(),
                    Cations = c(),
                    Other = c()))
    }

    # populated_vars_bool = sapply(filter_set, function(x) ! all(is.na(x)))
    # populated_vars = names(populated_vars_bool[populated_vars_bool])
    avail_vars <- drop_var_prefix(unique(filter_set$var))

    if(vartype == 'stream'){
        vars_display_subset <- chemvars_display
    } else if(vartype == 'precip'){
        vars_display_subset <- pchemvars_display
    }

    for(i in 1:length(vars_display_subset)){
        l <- vars_display_subset[[i]]
        l[! l %in% avail_vars] <- NULL
        vars_display_subset[[i]] <- l
    }

    return(vars_display_subset)
}

numeric_any <- function(num_vec){
    return(as.numeric(any(as.logical(num_vec))))
}

# df=data3; agg_selection=agg; which_dataset='pchem'; conc_flux_selection=conc_flux
# df=pchem3; agg_selection=agg; which_dataset='pchem'; conc_flux_selection=conc_flux
ms_aggregate <- function(d, agg_selection, conc_flux_selection = NULL){

    #agg_selection is a user input object, e.g. input$AGG3
    #which_dataset is one of 'chem', 'q', 'p', 'pchem'
    #conc_flux_selection must be supplied as e.g. input$CONC_FLUX3 if
        #which_dataset is 'chem' or 'pchem'

    # if(! which_dataset %in% c('chem', 'q', 'p', 'pchem')){
    #     stop("which_dataset must be one of 'chem', 'q', 'p', 'pchem'")
    # }
    #
    # if(which_dataset %in% c('chem', 'pchem') && is.null(conc_flux_selection)){
    #     stop(paste0("conc_flux_selection must be supplied when which_dataset",
    #         "is 'chem' or'pchem'"))
    # }

    if(nrow(d) == 0) return(d)
    if(agg_selection == 'Instantaneous') return(d)
    # if(agg_selection == 'Daily' && which_dataset == 'pchem') return(d)

    agg_period <- switch(agg_selection,
                         'Daily' = 'day',
                         'Monthly' = 'month',
                         'Yearly' = 'year')

    # var_is_volumetric <- d$var[1] %in% c('precipitation', 'discharge')
    var_is_p <- d$var[1] == 'precipitation'
    var_is_q <- d$var[1] == 'discharge'

    #round to desired_interval and summarize
    d <- sw(d %>%
        mutate(datetime = lubridate::round_date(datetime,
                                                agg_period)) %>%
        group_by(site_name, var, datetime) %>%
        summarize(
            val = if(n() > 1){
                if(var_is_q){
                    max_ind <- which.max(val)
                    if(max_ind) val[max_ind] else NA #max removes error
                    # max(val, na.rm = TRUE)
                } else if(conc_flux_selection == 'VWC' || var_is_p){
                    sum(val, na.rm = TRUE)
                } else {
                    mean(val, na.rm = TRUE)
                }
            } else {
                first(val) #needed for uncertainty propagation to work
            },
            across(any_of(c('ms_status', 'ms_interp')), numeric_any)) %>%
            # ms_status = numeric_any(ms_status)) %>%
        ungroup() %>%
        select(datetime, site_name, var, val, one_of('ms_status', 'ms_interp')))

    # d <- mutate(d,
    #              datetime = lubridate::floor_date(datetime,
    #                                               agg_period))
    #
    # # if('site_name' %in% colnames(d)){
    # d <- group_by(d,
    #                datetime, site_name, var)
    # # } else if('domain' %in% colnames(d)){
    # #     d = group_by(d, datetime, domain)
    # # } else {
    # #     d = group_by(d, datetime)
    # # }
    #
    # # if(which_dataset %in% c('chem', 'pchem')){
    # if(drop_var_prefix(d$var[1]) %in% c('precipitation', 'discharge')){
    #     d <- summarize_all(d,
    #                         list(~max(., na.rm=TRUE)))
    # } else if(conc_flux_selection == 'VWC'){
    #     d <- summarize_all(d,
    #                         list(~sum(., na.rm=TRUE)))
    # } else {
    #     d <- summarize_all(d,
    #                         list(~mean(., na.rm=TRUE)))
    # }

    # d <- inject_timeseries_NAs(d = ungroup(d),
    #                             fill_by = agg_period)

    return(d)
}

# df=ungroup(df); fill_by=agg_period
inject_timeseries_NAs = function(df, fill_by){

    dt_fill = seq.POSIXt(df$datetime[1], df$datetime[nrow(df)], by=fill_by)
    ndates = length(dt_fill)

    # if('site_name' %in% colnames(df)){

    sites = unique(df$site_name)
    nsites = length(sites)

    dt_fill_tb = tibble(datetime=rep(dt_fill, times=nsites),
        site_name=rep(sites, each=ndates))
    df = right_join(df, dt_fill_tb, by=c('site_name', 'datetime'))

    # } else if('domain' %in% colnames(df)){
    #
    #     domains = unique(df$domain)
    #     ndomains = length(domains)
    #
    #     dt_fill_tb = tibble(datetime=rep(dt_fill, times=ndomains),
    #         domain=rep(domains, each=ndates))
    #     df = right_join(df, dt_fill_tb, by=c('domain', 'datetime'))
    #
    # } else {
    #
    #     dt_fill_tb = tibble(datetime=dt_fill)
    #     df = right_join(df, dt_fill_tb, by='datetime')
    # }

    return(df)
}

# v=varA; conc_flux_selection=conc_flux; show_input_concentration=show_pchem
pad_widen_join <- function(v,
                           sites,
                           dates,
                           streamdata,
                           raindata,
                           show_input_concentration = FALSE){

    #subset dataset by variable, create columns for each site, generate empty
    #tibbles for missing data, prefix precip data with "P_"

    #raindata is optional.

    if(is.null(sites) || length(sites) == 0) sites = ' '

    if(length(v) == 0){
        return(manufacture_empty_plotdata(sites = sites))
    }

    streamdata_exist <- as.logical(nrow(streamdata))
    raindata_exist <- ! missing(raindata) && as.logical(nrow(raindata))

    if(streamdata_exist){

        streamdata <- streamdata %>%
            select(- ! ends_with(paste0('_', v)),
                   datetime,
                   site_name) %>%
            tidyr::pivot_wider(names_from = site_name,
                               values_from = paste0('val_', v))

    } else {
        streamdata <- manufacture_empty_plotdata(sites = sites)
    }

    if(show_input_concentration){

        if(raindata_exist){

            raindata <- raindata %>%
                select(- ! ends_with(paste0('_', v)),
                       datetime,
                       site_name) %>%
                tidyr::pivot_wider(names_from = site_name,
                                   values_from = paste0('val_', v)) %>%
                rename_with(~paste0('P_', .),
                            .cols = -datetime)
                            # .cols = any_of(!!sites))

        } else {
            raindata <- manufacture_empty_plotdata(sites = paste0('P_', sites))
        }
    }

    if(streamdata_exist && show_input_concentration){

        alldata <- dplyr::full_join(streamdata,
                                    raindata,
                                    by = 'datetime')
        # alldata <- left_forward_rolljoin(streamdata, raindata)

    } else if(raindata_exist && show_input_concentration){
        alldata <- raindata
    # } else if(streamdata_exist){
    #     alldata <- streamdata
    } else {
        alldata <- streamdata
    }

    alldata <- pad_ts(d = alldata,
                      vars = v,
                      datebounds = dates)

    return(alldata)
}

convert_portal_units <- function(d,
                                 conversion_enabled,
                                 conc_flux_selection,
                                 conc_unit,
                                 flux_unit){

    if(nrow(d) == 0) return(d)

    if(conversion_enabled){

        if(conc_flux_selection %in% c('Concentration', 'VWC')){

            d <- convert_conc_units(d = d,
                                    desired_unit = conc_unit)

        } else if(conc_flux_selection == 'Flux'){

            d <- convert_flux_units(d = d,
                                    desired_unit = flux_unit)
        }
    }

    return(d)
}

# set=streamdata; sites=' '
manufacture_empty_plotdata = function(sites){

    outdata = matrix(NA, ncol=length(sites) + 1, nrow=0,
        dimnames=list(NULL, c('datetime', sites)))
    outdata = as_tibble(outdata) %>%
        mutate_all(as.numeric) %>%
        mutate(datetime = as.POSIXct(datetime, origin='1970-01-01'))

    return(outdata)
}

get_rainsites <- function(alldata,
                          streamsites,
                          show_input_concentration){

    #streamsites needed in order to correctly order rainsites

    if(show_input_concentration && nrow(alldata)){

        # if(conc_flux_selection == 'VWC'){
        cnms <- colnames(alldata)
        rainsites <- cnms[grep('^P_(?!ms_)',
                               cnms,
                               perl = TRUE)]
        siteorder <- order(streamsites)
        rainsites <- sort(rainsites)[siteorder]

    } else {
        rainsites <- vector(length = 0,
                            mode = 'character')
    }

    return(rainsites)
}

generate_dropdown_sitelist = function(domain_vec){

    sitelist = list()
    for(i in 1:length(domain_vec)){
        domain_sites <- get_sitelist(domain = domain_vec[i],
                                     type = c('stream_gauge', 'stream_sampling_point'))
        sitelist[[i]] = domain_sites
    }
    names(sitelist) = names(domains_pretty[match(domain_vec, domains_pretty)])

    return(sitelist)
}

# sites_selected=sites; sites_all=displabs
# colorvec=linecolors; pad_length=length(displabs)
selection_color_match = function(sites_selected, sites_all, colorvec){

    as.character(factor(sites_all, levels=sites_selected))
    matched_colors = colorvec[match(sites_all, sites_selected)]

    return(matched_colors)
}

update_site_obvs <- function() {

    files <- list.files("data/")

    domain <- files[- grep("[.]", files)]

    observations <- tibble(site_name = as.character(), number = as.numeric())

    for(i in 1:length(domain)) {

        site_files <- list.files(paste0("data/", domain[i]), full.names = TRUE, recursive = TRUE)

        if(!purrr::is_empty(grep("flux", site_files))) {
            site_files <- site_files[- grep("flux", site_files)]
        }

        for(e in 1:length(site_files)) {

            file <- sm(read_feather(site_files[e])) %>%
                select(-datetime)

            name <- unique(file$site_name)

            if(length(name) > 1) {
                name <- file[[1]]

                file <- file[-1]
                file[!is.na(file)] <- 1

                new <- tibble("site_name" = name,
                    "observations" = file)

                new <- new %>%
                    group_by(site_name) %>%
                    summarise_all(sum, na.rm = TRUE)
            } else {

                file <- select(file, -site_name)

                file[!is.na(file)] <- 1

                num <- sum(colSums(file, na.rm = TRUE), na.rm = TRUE)

                new <- tibble("site_name" = name,
                    "observations" = num)
            }

            observations <- rbind(new, observations)
        }
    }

    sites <- sm(read_csv("data/site_data.csv"))

    full <- left_join(sites, observations, by = "site_name")

    write_csv(full, "data/site_data.csv")
}

get_local_solar_time <- function(df, time_scheme) {

    df <- site_data %>%
        select(longitude, local_time_zone, site_name) %>%
        right_join(df,
                   by = 'site_name')

    # df <- left_join(df,
    #                 site_info,
    #                 by = 'site_name')

    sites <- unique(df$site_name)

    final <- tibble()

    for(i in 1:length(sites)){

        times <- df %>%
            filter(site_name == !!sites[i]) %>%
            mutate(Local = force_tz(with_tz(datetime,
                                            tzone = local_time_zone),
                                    tzone = 'UTC')) %>%
            mutate(local_dif = Local - datetime,
                   doy = yday(datetime)) %>%
            mutate(solar_dif = solartime::computeSolarToLocalTimeDifference(
                longitude,
                local_dif,
                doy)) %>%
            mutate(Solar = Local + seconds(solar_dif * 60 * 60)) %>%
            mutate(datetime = .data[[time_scheme]]) %>%
            select(-solar_dif, -doy, -local_dif, -Local, -Solar, -longitude,
                   -local_time_zone)

        final <- bind_rows(final, times)
    }

    return(final)
}

# Biplot stuff

convertible <- function(var) {

    test <- pull(variables %>%
                          filter(variable_code == var) %>%
                          select(unit))

    if(length(test) == 0){
        return(FALSE)
    } else{
        if(test == 'mg/L'){
            return(TRUE)
        } else{
            return(FALSE)
        }
    }
}

convert_conc_units_bi = function(df, col, input_unit='mg/L', desired_unit){

    #df is a data frame or tibble of numeric concentration data
    #input_unit is the unit of concs (must be 'mg/L')
    #desired unit is one of the keys in the call to `switch` below

    require(PeriodicTable)
    if(!input_unit == 'mg/L' || length(input_unit) == 0) {
        return(df)
    }

    conc_df = df[col]

    conc_cols=col

    if(grepl('g', desired_unit)){

        converted = switch(desired_unit,
                           'ng/L' = conc_df * 1000000,
                           'ug/L' = conc_df * 1000,
                           'mg/L' = conc_df,
                           'g/L' = conc_df / 1000)

    } else {

        solutes = colnames(conc_df)
        constituents = parse_molecular_formulae(solutes)
        molar_mass = sapply(constituents, combine_atomic_masses)

        mm_scaled = switch(substr(desired_unit, 0, 1),
                           'n' = molar_mass * 1000000, #desired unit could be nM or neq/L
                           'u' = molar_mass * 1000, #and so on...
                           'm' = molar_mass,
                           'M' = molar_mass / 1000, #desired unit must be 'M'
                           'e' = molar_mass / 1000) #desired unit must be 'eq/L'

        if(grepl('q', desired_unit)){
            valence = variables$valence[variables$variable_code %in% solutes]
            mm_scaled = mm_scaled * valence
        }

        converted = data.frame(mapply(`*`, conc_df, mm_scaled, SIMPLIFY=FALSE))
    }

    df[conc_cols] = converted

    return(df)
}

convert_flux_units_bi = function(df, col, input_unit='kg', desired_unit, summary_file){

    #df is a data frame or tibble of numeric flux data
    #input_unit is the unit of flux (must be 'kg/ha/d')
    #desired unit is one of the keys in the call to `switch` below

    col_name <- col
    flux_cols = col_name

    if(grepl('/ha', desired_unit)) {

        if('area' %in% colnames(df)){

            df <- df %>%
                mutate(!!flux_cols := .data[[flux_cols]]/area)

        } else{

           # summary_file <- read_feather('data/biplot/year.feather')

            sites <- df %>%
                pull(site_name)

            sites <- unique(sites)

            areas <- summary_file %>%
                filter(site_name %in% sites,
                       var == 'area') %>%
                select(site_name, area=val)

            df <- df %>%
                left_join(., areas, by = 'site_name') %>%
                mutate(!!flux_cols := .data[[flux_cols]]/area) %>%
                select(-area)
        }


        flux_df = df[col_name]

        converted = switch(desired_unit,
                           'Mg/ha' = flux_df / 1000,
                           'kg/ha' = flux_df,
                           'g/ha' = flux_df * 1000,
                           'mg/ha' = flux_df * 1000000)

    } else{

        flux_df = df[col_name]

        converted = switch(desired_unit,
                           'Mg' = flux_df / 1000,
                           'kg' = flux_df,
                           'g' = flux_df * 1000,
                           'mg' = flux_df * 1000000)
    }

    df[flux_cols] = converted

    return(df)
}

convert_area_nor_q_bi = function(df, summary_file){

    #converts q in M^3

    if('area' %in% colnames(df)){

        df <- df %>%
            mutate(discharge = discharge/(area*10000)) %>%
            filter(!is.na(discharge))

    } else{

       # summary_file <- read_feather('data/biplot/year.feather')

        sites <- df %>%
            pull(site_name)

        sites <- unique(sites)

        areas <- summary_file %>%
            filter(site_name %in% sites,
                   var == 'area') %>%
            select(site_name, area=val)

        df <- df %>%
            left_join(., areas, by = 'site_name') %>%
            mutate(discharge = discharge/(area*10000)) %>%
            select(-area) %>%
            filter(!is.na(discharge))
    }

    return(df)
}

load_portal_config <- function(from_where){

    #this loads our "configuration" datasets into the global environment.
    #as of 11/17/20 those datasets include only site_data and variables.
    #depending on the type of instance (remote/local),
    #those datasets are either read as local CSVs or as google sheets. for ms
    #developers, this will always be "remote". for future users, it may be a
    #configurable option.

    if(from_where == 'remote'){

        variables <- sm(googlesheets4::read_sheet(conf$variables_gsheet,
                                                na = c('', 'NA')))

        site_data <- sm(googlesheets4::read_sheet(conf$site_data_gsheet,
                                                  na = c('', 'NA')))

    } else if(from_where == 'local'){

        variables <- sm(read_csv('data/general/variables.csv'))
        site_data <- sm(read_csv('data/general/site_data.csv'))

    } else {
        stop('from_where must be either "local" or "remote"')
    }

    assign('variables',
           variables,
           pos = .GlobalEnv)

    assign('site_data',
           site_data,
           pos = .GlobalEnv)
}

generate_dropdown_varlist_ws = function(variables){

    ws_vars <- variables %>%
        filter(variable_type == 'ws_char') %>%
        mutate(displayname=ifelse(!is.na(unit), paste0(variable_name, ' (', unit, ')'), variable_name)) %>%
        select(displayname, variable_code, variable_subtype) %>%
        plyr::dlply(plyr::.(variable_subtype), function(x){
            plyr::daply(x, plyr::.(displayname), function(y){
                y['variable_code']
            })
        })

    return(ws_vars)
}

filter_dropdown_varlist_bi = function(filter_set, vartype = 'conc'){

    if(nrow(filter_set) == 0){
        return(list(Anions = c(),
                    Cations = c(),
                    Other = c()))
    }

    avail_vars <- grep(vartype, unique(filter_set$var) , value = TRUE)

    avail_vars <- str_remove_all(avail_vars, paste0('_', vartype))


    vars_display_subset <- chemvars_display

    for(i in 1:length(vars_display_subset)){

        l <- vars_display_subset[[i]]
        l[! l %in% avail_vars] <- NULL
        vars_display_subset[[i]] <- l
    }

    return(vars_display_subset)
}

drop_var_prefix <- function(x){

    unprefixed <- substr(x, 4, nchar(x))

    return(unprefixed)
}

extract_var_prefix <- function(x){

    prefix <- substr(x, 1, 2)

    return(prefix)
}

get_default_site <- function(domain){

    site <- network_domain_default_sites %>%
        filter(domain == !!domain) %>%
               # network == !!network) %>% #TODO: observe network level in portal
        pull(default_site)

    return(site)
}

ms_read_portalsite <- function(domain,
                               site_name,
                               prodname){

    #read data from network/domain/site, arrange by variable then datetime.
    #insert val_err column
    #into the val column as errors attribute and then remove val_err column
    #(error/uncertainty is handled by the errors package as an attribute,
    #so it must be written/read as a separate column).

    d <- read_feather(glue('data/{dmn}/{p}/{s}.feather',
                           dmn = domain,
                           p = prodname,
                           s = site_name))

    d <- d %>%
        mutate(val = errors::set_errors(val, val_err)) %>%
        select(-val_err) %>%
        arrange(var, datetime)

    return(d)
}

filter_agg_widen_unprefix <- function(d,
                                      selected_vars,
                                      selected_datebounds,
                                      selected_agg,
                                      selected_prefixes,
                                      show_uncert,
                                      show_flagged,
                                      show_imputed,
                                      conc_or_flux){

    if(nrow(d) == 0) return(d)

    prefix_in_selected <- stringr::str_split(string = extract_var_prefix(d$var),
                                             pattern = '') %>%
                              purrr::map(~all(.x %in% selected_prefixes)) %>%
                              unlist()

    d <- filter(d,
                prefix_in_selected,
                drop_var_prefix(var) %in% selected_vars,
                datetime >= selected_datebounds[1], datetime <= selected_datebounds[2]) %>%
        mutate(var = drop_var_prefix(var))

    if(! show_uncert){
        d <- mutate(d,
                    val = errors::drop_errors(val))
    }

    if(! show_flagged){
        d <- filter(d,
                    ms_status == 0)
    }

    if(! show_imputed){
        d <- filter(d,
                    ms_interp == 0)
    }

    if(nrow(d) == 0) return(d)

    # d = pad_ts(d, vars=selected_vars, datebounds=selected_datebounds)
    d <- ms_aggregate(d = d,
                      agg_selection = selected_agg,
                      conc_flux_selection = conc_or_flux)

    d <-  pivot_wider(d,
                      names_from = var,
                      values_from = c('val', 'ms_status', 'ms_interp'))

    # if(init_vals$enable_unitconvert){
    #
    #     if(conc_flux %in% c('Concentration', 'VWC')){
    #
    #         datachem <- convert_conc_units(datachem,
    #                                        desired_unit = conc_unit)
    #     } else if(conc_flux == 'Flux'){
    #
    #         datachem <- convert_flux_units(datachem,
    #                                        desired_unit = flux_unit)
    #     }
    # }

    return(d)
}
