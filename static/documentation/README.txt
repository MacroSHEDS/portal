Readme for MacroSheds datasets

---

TIME-SERIES DATASETS

File Names:
"Network: czo, Domain: boulder"
"Network: czo, Domain: calhoun"
"Network: lter, Domain: hbef"
"Network: usfs, Domain: fernow"
etc.

Description:
This MacroSheds product includes time series of discharge, stream chemistry, stream chemistry flux,
precipitation, precipitation chemistry, and precipitation chemistry flux. It is divided up by network
(top-level managing institution of the original data) and domain (collecting/reporting institution).
For column descriptions, see the resource entitled "timeseries column descriptions".

---

WATERSHED TRAIT TIME-SERIES DATASETS

File Names:
"spatial_timeseries_climate"
"spatial_timeseries_hydrology"
"spatial_timeseries_landcover"
"spatial_timeseries_parentmaterial"
"spatial_timeseries_terrain"
"spatial_timeseries_vegetation"

Description:
This MacroSheds product represents spatial watershed averages of gridded data products available
primarily through Google Earth Engine. For most of these products, grids are generated on a recurring
basis. We here report watershed averages for each gridded product at whatever temporal
interval is available, up to daily (as in the case of e.g. PRISM precip). For some products,
only a few temporal "snapshots" are available. Still, in every case, we report these spatial
averages as time-series data.

Each watershed summary variable is prefixed with a two-letter code. The first letter
designates the variable's category (variable_category_code), and the second indicates
its source (data_source_code). We use these codes internally as a compact way to store
variable metadata that is used to filter this dataset. You may do the same, using the
resources entitled "variable_category_codes" and "data_source_codes". For column descriptions,
see "watershed_trait_timeseries column descriptions". For more information
about each variable, download the Variable table from the Data tab at macrosheds.org.

---

WATERSHED TRAIT SUMMARIES:

File Name:
"watershed_summaries"

Description:
This MacroSheds product represents spatial *and* temporal watershed summary statistics of
gridded data products available primarily through Google Earth Engine. It includes a subset
of the variables included in the spatial time-series datasets, summarized across time,
yielding a single value per variable per watershed.

Each watershed summary variable is prefixed with a two-letter code. The first letter
designates the variable's category (variable_category_code), and the second indicates
its source (data_source_code). We use these codes internally as a compact way to store
variable metadata that is used to filter this dataset. You may do the same, using the
resources entitled "variable_category_codes" and "data_source_codes". For column descriptions,
see "watershed_summary column descriptions".

