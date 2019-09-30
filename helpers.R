extract_from_config = function(key){
    ind = which(lapply(conf, function(x) grepl(key, x)) == TRUE)
    val = stringr::str_match(conf[ind], '.*\\"(.*)\\"')[2]
    return(val)
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
