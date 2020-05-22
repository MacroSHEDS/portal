sm = suppressMessages
sw = suppressWarnings

extract_from_config = function(key){
    ind = which(lapply(conf, function(x) grepl(key, x)) == TRUE)
    val = stringr::str_match(conf[ind], '.*\\"(.*)\\"')[2]
    return(val)
}

# df = grab_subset[, -(1:2)]

plot_empty_dygraph = function(datelims, plotgroup, ylab, px_per_lab){

    datelims = as.POSIXct(datelims)
    dateseq = seq(datelims[1], datelims[2], by='day')
    emptydat = xts(rep(0, length.out=length(dateseq)),
        order.by=dateseq, tzone='UTC')
    dg = dygraph(emptydat, group=plotgroup) %>%
        dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
            colors='transparent', retainDateWindow=TRUE) %>%
        dyAxis('y', label=ylab, labelWidth=16, labelHeight=10,
            pixelsPerLabel=px_per_lab, rangePad=10)

    return(dg)
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

convert_conc_units = function(df, input_unit='mg/L', desired_unit){

    #df is a data frame or tibble of numeric concentration data
    #input_unit is the unit of concs (must be 'mg/L')
    #desired unit is one of the keys in the call to `switch` below

    require(PeriodicTable)

    conc_cols = colnames(df) %in% conc_vars
    conc_df = df[conc_cols]

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

convert_flux_units = function(df, input_unit='kg/ha/d', desired_unit){

    #df is a data frame or tibble of numeric flux data
    #input_unit is the unit of flux (must be 'kg/ha/d')
    #desired unit is one of the keys in the call to `switch` below

    flux_cols = colnames(df) %in% conc_vars
    flux_df = df[flux_cols]

    converted = switch(desired_unit,
        'Mg/ha/d' = flux_df / 1000,
        'kg/ha/d' = flux_df,
        'g/ha/d' = flux_df * 1000,
        'mg/ha/d' = flux_df * 1000000)

    df[flux_cols] = converted

    return(df)
}

# log10_ceiling = function(x) {
#     10^(ceiling(log10(x)))
# }

# tsdf=dataPrecip3; sites=unique(dataPrecip3$site_name)
# vars='precip'; datebounds=dd
pad_ts3 = function(tsdf, sites, vars, datebounds){

    # if(is.null(sites)) sites = 'placeholder'
    if(is.null(sites) || length(sites) == 0) sites = 'placeholder'

    nsites = length(sites)
    nvars = length(vars)

    dt_ext_rows = tibble(rep(as.POSIXct(datebounds), nsites),
        rep(sites, each=2))
    dt_ext_rows = bind_cols(dt_ext_rows, as.data.frame(matrix(NA_real_,
        ncol=nvars, nrow=nsites * 2)))
    colnames(dt_ext_rows) = c('datetime', 'site_name', vars)

    # if(class(tsdf$datetime) == 'Date'){
    #     dt_ext_rows$datetime = as.Date(dt_ext_rows$datetime)
    # } else {
    #     dt_ext_rows$datetime = as.POSIXct(dt_ext_rows$datetime)
    # }

    df_padded = bind_rows(dt_ext_rows, tsdf) #CONVERTS TZ. VERIFY LEGITNESS

    if(sites[1] == 'placeholder'){
        df_padded = select(df_padded, -site_name)
    }

    return(df_padded)
}

rolljoin = function(raindf, streamdf, rainsitevec, streamsitevec){
    #raindf and streamdf must be datqa.tables

    #forward rolling join by datetime
    setkey(raindf, 'datetime')
    setkey(streamdf, 'datetime')
    alldf = raindf[streamdf, roll=TRUE]
    alldf = as_tibble(alldf) %>%
        select(datetime, one_of(streamsitevec), one_of(rainsitevec))

    #prevent excessive forward extrapolating of rain vars
    gratuitous_end_roll_r = streamdf$datetime >
        raindf$datetime[nrow(raindf) - 1]

    if(sum(gratuitous_end_roll_r, na.rm=TRUE) == 1){
        gratuitous_end_roll_r[length(gratuitous_end_roll_r)] = FALSE
    }

    if(nrow(alldf)) alldf[gratuitous_end_roll_r, rainsitevec] = NA

    #prevent excessive forward extrapolating of stream vars
    gratuitous_end_roll_s = raindf$datetime >
        streamdf$datetime[nrow(streamdf) - 1]

    if(sum(gratuitous_end_roll_s, na.rm=TRUE) == 1){
        gratuitous_end_roll_s[length(gratuitous_end_roll_s)] = FALSE
    }

    if(nrow(alldf)) alldf[gratuitous_end_roll_s, streamsitevec] = NA

    return(alldf)
}

sites_from_feathers = function(directory){
    sitenames = unlist(strsplit(list.files(directory), '.feather'))
    return(sitenames)
}

sites_by_domain = function(var){

    sitelist = list()
    for(i in 1:length(domains)){
        psitevec = sites_from_feathers(glue('data/{d}/{v}',
            d=domains[i], v=var))
        sitelist = append(sitelist, list(psitevec))
        names(sitelist)[i] = domains[i]
    }

    return(sitelist)
}

most_recent_year = function(date_range){
    mry = c(
        max(date_range[2] - lubridate::days(365),
            date_range[1],
            na.rm=TRUE),
        date_range[2]
    )

    return(mry)
}

sitelist_from_domain = function(dmn, site_type){

    sitelist = site_data %>%
        filter(domain == dmn, site_type == site_type) %>%
        pull(site_name)

    return(sitelist)
}

try_read_feather = function(path){

    out = try(feather::read_feather(path), silent=TRUE)
    if('try-error' %in% class(out)) out = tibble()

    return(out)
}

generate_dropdown_varlist = function(grabvars, filter_set=NULL){

    if(! is.null(filter_set)){
        grabvars = filter(grabvars, variable_code %in% filter_set)
    }

    grabvars = grabvars %>%
        mutate(displayname=paste0(variable_name, ' (', unit, ')')) %>%
        select(displayname, variable_code, variable_subtype) %>%
        plyr::dlply(plyr::.(variable_subtype), function(x){
            plyr::daply(x, plyr::.(displayname), function(y){
                y['variable_code']
            })
        })

    return(grabvars)
}

filter_dropdown_varlist = function(filter_set, vartype='stream'){

    filter_set = sw(select(filter_set, -one_of('site_name', 'datetime')))

    populated_vars_bool = sapply(filter_set, function(x) ! all(is.na(x)))
    populated_vars = names(populated_vars_bool[populated_vars_bool])

    if(vartype == 'stream'){
        vars_display_subset = grabvars_display
    } else if(vartype == 'precip'){
        vars_display_subset = pchemvars_display
    }

    for(i in 1:length(vars_display_subset)){
        l = vars_display_subset[[i]]
        l[! l %in% populated_vars] = NULL
        vars_display_subset[[i]] = l
    }

    return(vars_display_subset)
}

ms_aggregate = function(df, agg_selection, which_dataset,
    conc_flux_selection=NULL){

    #agg_selection is a user input object, e.g. input$AGG3
    #which_dataset is one of 'grab', 'q', 'p', 'pchem'
    #conc_flux_selection must be supplied as e.g. input$CONC_FLUX3 if
    #which_dataset is 'grab' or 'pchem'

    if(! which_dataset %in% c('grab', 'q', 'p', 'pchem')){
        stop("which_dataset must be one of 'grab', 'q', 'p', 'pchem'")
    }

    if(which_dataset %in% c('grab', 'pchem') && is.null(conc_flux_selection)){
        stop(paste0("conc_flux_selection must be supplied when which_dataset",
            "is 'grab' or'pchem'"))
    }

    if(nrow(df) == 0) return(df)
    if(agg_selection == 'Instantaneous') return(df)
    if(agg_selection == 'Daily' && which_dataset == 'pchem') return(df)

    agg_period = switch(agg_selection, 'Daily'='day', 'Monthly'='month',
        'Yearly'='year')
    df = group_by(df, datetime=floor_date(datetime, agg_period), site_name)

    if(which_dataset %in% c('grab', 'pchem')){

        if(conc_flux_selection == 'VWC'){
            df = summarize_all(df, list(~sum(., na.rm=TRUE)))
        } else {
            df = summarize_all(df, list(~mean(., na.rm=TRUE)))
        }

    } else if(which_dataset == 'p'){
        df = summarize_all(df, list(~mean(., na.rm=TRUE)))
    } else if(which_dataset == 'q'){
        df = summarize_all(df, list(~max(., na.rm=TRUE)))
    }

    df = ungroup(df)

    return(df)
}

prep_mainfacets3 = function(v, dmn, sites, streamdata, raindata,
    conc_flux_selection, show_input_concentration){

    if(is.na(v)) return()

    streamdata = streamdata %>%
        filter(site_name %in% sites) %>%
        select(datetime, site_name, one_of(v)) %>%
        group_by(datetime, site_name) %>%
        summarize_all(mean, na.rm=TRUE) %>%
        spread(site_name, !!v) %>%
        data.table()

    if(show_input_concentration & ! is.null(raindata)){

        raindata = raindata %>%
            select(datetime, site_name, one_of(v)) %>%
            spread(site_name, !!v) %>%
            rename_at(vars(-one_of('datetime', 'hbef pchem')), #shouldnt be hardcoded
                ~paste0('P_', .)) %>%
            data.table()

        if(conc_flux_selection == 'VWC'){
            rainsites = colnames(raindata)[-1]
        } else {
            rainsites = paste(dmn, 'pchem')
        }

        if(! nrow(raindata)){
            raindata = tibble(datetime=streamdata$datetime)
            raindata[rainsites] = NA
            raindata = as.data.table(raindata)
        }

        alldata = rolljoin(raindata, streamdata, rainsites, sites)

    } else {
        alldata = as_tibble(streamdata)
    }

    return(alldata)
}
