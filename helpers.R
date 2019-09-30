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

main_biplot = function(x, y){
    plot(grab[[x]], grab[[y]])
}

summary_biplot = function(x, y){
    plot(x, y)
}

log10_ceiling = function(x) {
    10^(ceiling(log10(x)))
}
