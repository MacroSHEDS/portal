setwd('~/git/macrosheds/portal/data/hbef/')
library(feather)
hbef_wsheds_tolower = function(d){

    ff = dir(d, pattern='w[0-9].feather')
    for(f in ff){
        setwd(d)
        x = read_feather(f)
        x$site_code = tolower(x$site_code)
        write_feather(x, f)
        setwd('..')
    }
}

hbef_wsheds_tolower('chemistry')
hbef_wsheds_tolower('discharge')
hbef_wsheds_tolower('flux')

# hbef_wsheds_toupper = function(d){
#
#     ff = dir(d, pattern='w[0-9].feather')
#     for(f in ff){
#         setwd(d)
#         x = read_feather(f)
#         x$site_code = toupper(x$site_code)
#         write_feather(x, f)
#         setwd('..')
#     }
# }
#
# hbef_wsheds_toupper('chemistry')
# hbef_wsheds_toupper('discharge')
# hbef_wsheds_toupper('flux')
