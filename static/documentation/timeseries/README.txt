Note: missing values in long/indexed data tables can either be explicit (i.e. each NA value
gets a row with a timestamp) or implicit (i.e. NA rows are omitted, but you can still tell
a sample is missing because there's a gap in the timestamps). Currently, missing values in
MacroSheds time-series data are semi-explicit, meaning we've filled in the first and last row
of every data gap. This was done to limit unnecessary data transfer and processing, and thus
improve performance on our data portal. Soon we will have a dedicated server for data downloads,
and we'll make our missing values fully explicit. For now, if you cast downloaded MacroSheds
time-series data from long/indexed to wide/Cartesian format, most NAs will be populated. You
can use tidyr::complete to fill in the rest.
