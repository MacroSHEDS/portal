extract_from_config = function(key){
    ind = which(lapply(conf, function(x) grepl(key, x)) == TRUE)
    val = stringr::str_match(conf[ind], '.*\\"(.*)\\"')[2]
    return(val)
}

populate_vars = function(df){

    populated_vars_bool = sapply(df, function(x) ! all(is.na(x)))
    populated_vars = names(populated_vars_bool[populated_vars_bool])

    grabvars_display_subset = grabvars_display

    for(i in 1:length(grabvars_display_subset)){
        l = grabvars_display_subset[[i]]
        l[! l %in% populated_vars] = NULL
        grabvars_display_subset[[i]] = l
    }

    return(grabvars_display_subset)

    # grabvars_display_vec = unlist(grabvars_display)
    # vars_present_bool = grabvars_display_vec %in% populated_vars
    # default_var = unname(grabvars_display_vec[vars_present_bool][1])
    #
    # return(default_var)
}

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

    conc_vars = str_match(formulae, '^(?:OM|TM|DO|TD)?([A-Za-z0-9]+)_?')[,2]
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

    # df = data.frame(datetime=as.POSIXct(1:4, origin='1970-01-01'),
    #     sitename=rep('a', 4), Cl=1:4)
    # dd <<- df
    # df = dd
    # desired_unit='meq/L'

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
