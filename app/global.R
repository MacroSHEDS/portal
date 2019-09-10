library(stringr)
library(feather)

# setwd('~/git/macrosheds/app')
grab = read_feather('../data/hbef/grab.feather')
sensor = read_feather('../data/hbef/sensor.feather')
