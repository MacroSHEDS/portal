sm = suppressMessages
sw = suppressWarnings

extract_from_config = function(key){
    ind = which(lapply(conf, function(x) grepl(key, x)) == TRUE)
    val = stringr::str_match(conf[ind], '.*\\"(.*)\\"')[2]
    return(val)
}

get_ylab = function(v, conc_or_flux, yunit){

    if(conc_or_flux == 'Flux'){
        unit = yunit
    } else {
        unit = ifelse(v %in% conc_vars, yunit,
            chemvars$unit[chemvars$variable_code == v])
    }

    ylab = glue('{var} ({u})', var=v, u=unit)

    return(ylab)
}

# datelims=date3; mainlab=colnames(alldata)[-1]; plotgroup='nSiteNVar'; ylab=ylab; px_per_lab=20
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

pad_ts = function(tsdf, vars, datebounds){

    #if tsdf is precip or pchem, data are aggregated by domain, so a domain
    #column is passed. otherwise a site_name column is passed

    if('site_name' %in% colnames(tsdf)){
        spatial_unit = 'site_name'
        sites_or_dmns = unique(tsdf$site_name)
    } else if('domain' %in% colnames(tsdf)){
        spatial_unit = 'domain'
        sites_or_dmns = unique(tsdf$domain)
    } else{ #probably never run, but here for unforseen back-compatibility needs
        spatial_unit = 'site_name'
        sites_or_dmns = 'placeholder'
    }

    n_sites_or_dmns = length(sites_or_dmns)
    nvars = length(vars)

    dt_ext_rows = tibble(rep(as.POSIXct(datebounds), n_sites_or_dmns),
        rep(sites_or_dmns, each=2))
    dt_ext_rows = bind_cols(dt_ext_rows, as.data.frame(matrix(NA_real_,
        ncol=nvars, nrow=n_sites_or_dmns * 2)))
    colnames(dt_ext_rows) = c('datetime', spatial_unit, vars)
    dt_ext_rows$datetime = lubridate::force_tz(dt_ext_rows$datetime, 'UTC')


    df_padded = tsdf %>%
        bind_rows(dt_ext_rows)

    if(sites_or_dmns[1] == 'placeholder'){
        df_padded = select(df_padded, -site_name)
    }

    return(df_padded)
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

sitelist_from_domain = function(dmn, type){

    #type is one of the types listed in site_data.csv, e.g. 'stream_gauge'

    sitelist = site_data %>%
        filter(domain == dmn, site_type == type) %>%
        pull(site_name)

    return(sitelist)
}

try_read_feather = function(path){

    out = try(feather::read_feather(path), silent=TRUE)
    if('try-error' %in% class(out)) out = tibble()

    return(out)
}

# var='precip'; dmns=c('hbef', 'neon'); sites=c('CUPE', 'W1', 'BIGC')
read_combine_feathers = function(var, dmns, sites=NULL){

    #in order to allow duplicate sitenames across domains, must invoke js here
    #see ms_todo

    #in case duplicate sitenames do appear, this will make it a bit less
    #likely that there's a collision. this can be simplified if js solution
    #is implemented
    if(var %in% c('precip', 'pchem')){
        dmn_sites = tibble(domain=dmns, site_name=NA)
    } else {
        dmn_sites = site_data %>%
            filter(domain %in% dmns, site_type == 'stream_gauge') %>%
            filter(site_name %in% sites) %>%
            select(domain, site_name)
    }

    combined_data = tibble()
    for(i in 1:nrow(dmn_sites)){

        if(var %in% c('precip', 'pchem')){
            filestr = glue('data/{d}/{v}.feather', d=dmn_sites$domain[i], v=var)
        } else {
            filestr = glue('data/{d}/{v}/{s}.feather',
                d=dmn_sites$domain[i], v=var, s=dmn_sites$site_name[i])
        }

        data_part = try_read_feather(filestr)
        if(var %in% c('precip', 'pchem')) data_part$domain = dmn_sites$domain[i]
        combined_data = bind_rows(combined_data, data_part)
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

filter_dropdown_varlist = function(filter_set, vartype='stream'){

    filter_set = sw(select(filter_set, -one_of('site_name', 'datetime')))

    if(nrow(filter_set) == 0){
        return(list(Anions=c(), Cations=c(), Other=c()))
    }

    populated_vars_bool = sapply(filter_set, function(x) ! all(is.na(x)))
    populated_vars = names(populated_vars_bool[populated_vars_bool])

    if(vartype == 'stream'){
        vars_display_subset = chemvars_display
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

# df=data3; agg_selection=agg; which_dataset='pchem'; conc_flux_selection=conc_flux
# df=pchem3; agg_selection=agg; which_dataset='pchem'; conc_flux_selection=conc_flux
ms_aggregate = function(df, agg_selection, which_dataset,
    conc_flux_selection=NULL){

    #agg_selection is a user input object, e.g. input$AGG3
    #which_dataset is one of 'chem', 'q', 'p', 'pchem'
    #conc_flux_selection must be supplied as e.g. input$CONC_FLUX3 if
        #which_dataset is 'chem' or 'pchem'

    if(! which_dataset %in% c('chem', 'q', 'p', 'pchem')){
        stop("which_dataset must be one of 'chem', 'q', 'p', 'pchem'")
    }

    if(which_dataset %in% c('chem', 'pchem') && is.null(conc_flux_selection)){
        stop(paste0("conc_flux_selection must be supplied when which_dataset",
            "is 'chem' or'pchem'"))
    }

    if(nrow(df) == 0) return(df)
    if(agg_selection == 'Instantaneous') return(df)
    if(agg_selection == 'Daily' && which_dataset == 'pchem') return(df)

    agg_period = switch(agg_selection,
        'Daily'='day', 'Monthly'='month', 'Yearly'='year')

    df = mutate(df, datetime = lubridate::floor_date(datetime, agg_period))

    if('site_name' %in% colnames(df)){
        df = group_by(df, datetime, site_name)
    } else if('domain' %in% colnames(df)){
        df = group_by(df, datetime, domain)
    } else {
        df = group_by(df, datetime)
    }

    if(which_dataset %in% c('chem', 'pchem')){

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

    df = inject_timeseries_NAs(ungroup(df), fill_by=agg_period)

    return(df)
}

# df=ungroup(df); fill_by=agg_period
inject_timeseries_NAs = function(df, fill_by){

    dt_fill = seq.POSIXt(df$datetime[1], df$datetime[nrow(df)], by=fill_by)
    ndates = length(dt_fill)

    if('site_name' %in% colnames(df)){

        sites = unique(df$site_name)
        nsites = length(sites)

        dt_fill_tb = tibble(datetime=rep(dt_fill, times=nsites),
            site_name=rep(sites, each=ndates))
        df = right_join(df, dt_fill_tb, by=c('site_name', 'datetime'))

    } else if('domain' %in% colnames(df)){

        domains = unique(df$domain)
        ndomains = length(domains)

        dt_fill_tb = tibble(datetime=rep(dt_fill, times=ndomains),
            domain=rep(domains, each=ndates))
        df = right_join(df, dt_fill_tb, by=c('domain', 'datetime'))

    } else {

        dt_fill_tb = tibble(datetime=dt_fill)
        df = right_join(df, dt_fill_tb, by='datetime')
    }

    return(df)
}

# v=varA; conc_flux_selection=conc_flux; show_input_concentration=show_pchem
prep_mainfacets3 = function(v, dmns, sites, streamdata, raindata,
    conc_flux_selection, show_input_concentration){

    # raindata=rr; streamdata=ll

    if(is.null(sites) || length(sites) == 0) sites = ' '
    if(length(v) == 0){
        return(manufacture_empty_plotdata(set='streamdata', sites=sites))
    }

    streamdata_exist = nrow(streamdata)
    raindata_exist = nrow(raindata)
    # streamdata_exist = ! is.null(streamdata) && nrow(streamdata)
    # raindata_exist = ! is.null(raindata) && nrow(raindata)

    if(streamdata_exist){

        streamdata = streamdata %>%
            filter(site_name %in% sites) %>%
            select(datetime, site_name, one_of(v)) %>%
            group_by(datetime, site_name) %>%
            summarize_all(mean, na.rm=TRUE) %>%
            spread(site_name, !!v)

    } else {
        streamdata = manufacture_empty_plotdata(set='streamdata', sites=sites)
        # streamdata = tibble(datetime=as.POSIXct(NA))
    }

    if(show_input_concentration){

        if(raindata_exist){

            raindata = raindata %>%
                select(datetime, site_name, one_of(v)) %>%
                spread(site_name, !!v) %>%
                rename_at(vars(-one_of('datetime'), -ends_with(' pchem')),
                    ~paste0('P_', .))

        } else {
            raindata = manufacture_empty_plotdata(set='raindata', dmns=dmns,
                sites=sites, conc_flux_selection=conc_flux_selection)
        }

        # rainsites = colnames(raindata)[-1]
    }

    if(streamdata_exist && show_input_concentration){
        alldata = left_forward_rolljoin(streamdata, raindata)
    } else if(streamdata_exist){
        alldata = as_tibble(streamdata)
    } else if(raindata_exist){
        alldata = as_tibble(raindata)
    } else {
        alldata = as_tibble(streamdata)
    }

    return(alldata)
}

# set=streamdata; sites=' '
manufacture_empty_plotdata = function(set, dmns=NULL, sites=NULL,
    conc_flux_selection=NULL){

    #if set=='raindata', conc_flux_selection must be supplied,
    #and if conc_flux_selection != 'VWC', dmns must additionally be supplied.
    #write error catchers for these conditions

    if(set == 'raindata'){

        if(conc_flux_selection == 'VWC'){
            sites_or_domains = paste0('P_', sites)
        } else {
            sites_or_domains = paste(dmns, 'pchem')
        }

    } else if(set == 'streamdata'){
        sites_or_domains = sites
    }

        outdata = matrix(NA, ncol=length(sites_or_domains) + 1, nrow=0,
            dimnames=list(NULL, c('datetime', sites_or_domains)))
        outdata = as_tibble(outdata) %>%
            mutate_all(as.numeric) %>%
            mutate(datetime = as.POSIXct(datetime, origin='1970-01-01'))

    return(outdata)
}

get_rainsites = function(raindata, alldata, streamsites,
    conc_flux_selection, show_input_concentration){

    #streamsites needed to correctly order rainsites

    if(show_input_concentration && nrow(alldata)){

        if(conc_flux_selection == 'VWC'){
            cnms = colnames(alldata)
            rainsites = cnms[grep('^P_', cnms)]
            siteorder = order(streamsites)
            rainsites = sort(rainsites)[siteorder]
        } else {
            rainsites = unique(raindata$site_name) #e.g. "hbef pchem"
        }

    } else {
        rainsites = vector(length=0, mode='character')
    }

    return(rainsites)
}

generate_dropdown_sitelist = function(domain_vec){

    sitelist = list()
    for(i in 1:length(domain_vec)){
        domain_sites = sitelist_from_domain(domain_vec[i], 'stream_gauge')
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
