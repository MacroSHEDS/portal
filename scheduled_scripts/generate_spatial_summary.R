
#### Generate watershed summary file ####

generate_watershed_summaries <- function(){

    fils <- list.files('data', recursive = T, full.names = T)
    fils <- fils[grepl('ws_traits', fils)]


    wide_spat_data <- site_data %>%
        filter(in_workflow == 1,
               site_type == 'stream_gauge') %>%
        select(network, domain, site_name, ws_area_ha)

    # Prism precip
    precip_files <- fils[grepl('cc_precip', fils)]
    precip_files <- precip_files[grepl('sum', precip_files)]

    precip <- map_dfr(precip_files, read_feather) %>%
        filter(year != substr(Sys.Date(), 0, 4),
               var == 'cc_cumulative_precip') %>%
        group_by(site_name) %>%
        summarise(cc_mean_annual_precip = mean(val, na.arm = TRUE)) %>%
        filter(!is.na(cc_mean_annual_precip))

    # Prism temp
    temp_files <- fils[grepl('cc_temp', fils)]
    temp_files <- temp_files[grepl('sum', temp_files)]

    temp <- map_dfr(temp_files, read_feather) %>%
        filter(year != substr(Sys.Date(), 0, 4),
               var == 'cc_temp_mean') %>%
        group_by(site_name) %>%
        summarise(cc_mean_annual_temp = mean(val, na.arm = TRUE)) %>%
        filter(!is.na(cc_mean_annual_temp))

    # start of season
    sos_files <- fils[grepl('start_season', fils)]

    sos <- map_dfr(sos_files, read_feather) %>%
        filter(year != substr(Sys.Date(), 0, 4),
               var == 'vd_sos_mean') %>%
        group_by(site_name) %>%
        summarise(vd_mean_sos = mean(val, na.arm = TRUE)) %>%
        filter(!is.na(vd_mean_sos))

    # end of season
    eos_files <- fils[grepl('end_season', fils)]

    eos <- map_dfr(eos_files, read_feather) %>%
        filter(year != substr(Sys.Date(), 0, 4),
               var == 'vd_eos_mean') %>%
        group_by(site_name) %>%
        summarise(vd_mean_eos = mean(val, na.arm = TRUE)) %>%
        filter(!is.na(vd_mean_eos))

    # end of season
    los_files <- fils[grepl('length_season', fils)]

    los <- map_dfr(los_files, read_feather) %>%
        filter(year != substr(Sys.Date(), 0, 4),
               var == 'vd_los_mean') %>%
        group_by(site_name) %>%
        summarise(vd_mean_los = mean(val, na.arm = TRUE)) %>%
        filter(!is.na(vd_mean_los))

    # gpp
    gpp_files <- fils[grepl('gpp', fils)]
    gpp_files <- gpp_files[grepl('sum', gpp_files)]

    gpp <- map_dfr(gpp_files, read_feather) %>%
        filter(year != substr(Sys.Date(), 0, 4),
               var == 'va_gpp_sum') %>%
        group_by(site_name) %>%
        summarise(va_mean_annual_gpp = mean(val, na.arm = TRUE)) %>%
        filter(!is.na(va_mean_annual_gpp))

    # npp
    npp_files <- fils[grepl('npp', fils)]

    npp <- map_dfr(npp_files, read_feather) %>%
        filter(year != substr(Sys.Date(), 0, 4),
               var == 'va_npp_median') %>%
        group_by(site_name) %>%
        summarise(va_mean_annual_npp = mean(val, na.arm = TRUE)) %>%
        filter(!is.na(va_mean_annual_npp))

    # terrain
    terrain_fils <- fils[grepl('terrain', fils)]

    terrain <- map_dfr(terrain_fils, read_feather) %>%
        filter(var %in% c('te_elev_mean',
                          'te_elev_min',
                          'te_elev_max',
                          'te_aspect_mean',
                          'te_slope_mean')) %>%
        select(-year) %>%
        pivot_wider(names_from = 'var', values_from = 'val')

    # bfi
    bfi_fils <- fils[grepl('bfi', fils)]

    bfi <- map_dfr(bfi_fils, read_feather) %>%
        filter(var %in% c('hd_bfi_mean')) %>%
        filter(pctCellErr <= 15) %>%
        select(-year, -pctCellErr) %>%
        pivot_wider(names_from = 'var', values_from = 'val')

    # nlcd
    nlcd_fils <- fils[grepl('nlcd', fils)]

    nlcd <- map_dfr(nlcd_fils, read_feather) %>%
        filter(var %in% c('vg_nlcd_barren',
                          'vg_nlcd_crop',
                          'vg_nlcd_dev_hi',
                          'vg_nlcd_dev_low',
                          'vg_nlcd_dev_med',
                          'vg_nlcd_dev_open',
                          'vg_nlcd_forest_dec',
                          'vg_nlcd_forest_evr',
                          'vg_nlcd_forest_mix',
                          'vg_nlcd_grass',
                          'vg_nlcd_ice_snow',
                          'vg_nlcd_pasture',
                          'vg_nlcd_shrub',
                          'vg_nlcd_water',
                          'vg_nlcd_wetland_herb',
                          'vg_nlcd_wetland_wood',
                          'vg_nlcd_shrub_dwr',
                          'vg_nlcd_sedge',
                          'vg_lncd_lichens',
                          'vg_nlcd_moss')) %>%
        group_by(site_name) %>%
        mutate(max_year = max(year)) %>%
        filter(year == max_year) %>%
        select(-year, -max_year) %>%
        pivot_wider(names_from = 'var', values_from = 'val')

    # soil
    soil_fils <- fils[grepl('soil', fils)]

    soil <- map_dfr(soil_fils, read_feather) %>%
        filter(var %in% c('pf_soil_org',
                          'pf_soil_sand',
                          'pf_soil_silt',
                          'pf_soil_clay',
                          'pf_soil_ph')) %>%
        filter(pctCellErr <= 15) %>%
        select(-year, -pctCellErr) %>%
        pivot_wider(names_from = 'var', values_from = 'val')

    # soil thickness
    soil_thickness_fils <- fils[grepl('pelletier_soil_thickness', fils)]

    soil_thickness <- map_dfr(soil_thickness_fils, read_feather) %>%
        filter(var %in% c('pi_soil_thickness')) %>%
        filter(pctCellErr <= 15) %>%
        select(-year, -pctCellErr) %>%
        pivot_wider(names_from = 'var', values_from = 'val') %>%
        mutate(pi_soil_thickness = round(pi_soil_thickness, 2))

    # et_ref
    et_ref_fils <- fils[grepl('et_ref', fils)]
    et_ref_fils <- et_ref_fils[grepl('sum', et_ref_fils)]

    et_ref_thickness <- map_dfr(et_ref_fils, read_feather) %>%
        filter(var %in% c('ci_et_grass_ref_mean'),
               !is.na(val)) %>%
        select(-year) %>%
        group_by(site_name) %>%
        summarise(ci_mean_annual_et = mean(val))

    # geological chem
    geochem_fils <- fils[grepl('geochemical', fils)]

    geochem <- map_dfr(geochem_fils, read_feather) %>%
        filter(var %in% c('pd_geo_Al2O3_mean',
                          'pd_geo_CaO_mean',
                          'pd_geo_CompressStrength_mean',
                          'pd_geo_Fe2O3_mean',
                          'pd_geo_HydaulicCond_mean',
                          'pd_geo_K2O_mean',
                          'pd_geo_MgO_mean',
                          'pd_geo_N_mean',
                          'pd_geo_Na2O_mean',
                          'pd_geo_P2O5_mean',
                          'pd_geo_S_mean',
                          'pd_geo_SiO2_mean'),
               !is.na(val)) %>%
        select(-year) %>%
        group_by(site_name, var) %>%
        summarise(mean_val = mean(val)) %>%
        pivot_wider(names_from = 'var', values_from = 'mean_val')


    watershed_summaries <- full_join(wide_spat_data, precip, by = 'site_name') %>%
        full_join(temp, by = 'site_name') %>%
        full_join(sos, by = 'site_name') %>%
        full_join(eos, by = 'site_name') %>%
        full_join(los, by = 'site_name') %>%
        full_join(gpp, by = 'site_name') %>%
        full_join(npp, by = 'site_name') %>%
        full_join(terrain, by = 'site_name') %>%
        full_join(bfi, by = 'site_name') %>%
        full_join(nlcd, by = 'site_name') %>%
        full_join(soil, by = 'site_name') %>%
        full_join(soil_thickness, by = 'site_name') %>%
        full_join(et_ref_thickness, by = 'site_name') %>%
        full_join(geochem, by = 'site_name')

    write_csv(watershed_summaries, 'data/general/watershed_summaries.csv')

}

generate_watershed_raw_spatial_dataset <- function(){

    domains <- list.files('data/')
    domains <- domains[!grepl('general', domains)]

    ws_trait_folders <- unique(list.files(glue('data/{d}/ws_traits',
                                               d = domains)))

    all_files <- list.files('data', recursive = TRUE, full.names = TRUE)

    raw_spatial_dat <- tibble()
    for(i in 1:length(ws_trait_folders)){

        trait_files <- all_files[grepl(ws_trait_folders[i], all_files)]

        extention <- str_split_fixed(trait_files, '/', n = Inf)[,5]
        check_sum_raw <- str_split_fixed(extention, '_', n = Inf)[,1]
        sum_raw_prez <- unique(check_sum_raw)

        if(sum_raw_prez == 'sum' || ! all(c('raw', 'sum') %in% sum_raw_prez)){
            all_trait <- map_dfr(trait_files, read_feather)%>%
                mutate(datetime = ymd(paste(year, 1, 1, sep = '-'))) %>%
                select(-year)

            if(is.na(all(all_trait$year))){
                all_trait <- all_trait %>%
                    rename(datetime = year)
            }
        }

        if(all(c('raw', 'sum') %in% sum_raw_prez)){
            trait_files <- trait_files[grepl('raw', trait_files)]

            all_trait <- map_dfr(trait_files, read_feather)
        }

        raw_spatial_dat <- rbind.fill(raw_spatial_dat, all_trait)
    }

    site_doms <- site_data %>%
        select(network, domain, site_name)

    raw_spatial_dat <- raw_spatial_dat %>%
        filter(!is.na(val)) %>%
        left_join(site_doms, by = 'site_name') %>%
        select(network, domain, site_name, var, datetime, val, pctCellErr)

    spat_variable <- unique(raw_spatial_dat$var)

    universal_products_meta <- universal_products %>%
        select(data_class, data_source, data_class_code, data_source_code)

    meta_data <- variables %>%
        filter(variable_code %in% !!spat_variable) %>%
        select(variable_code, variable_name, unit, variable_type, variable_subtype) %>%
        mutate(data_class_code = substr(variable_code, 0, 1),
               data_source_code = substr(variable_code, 2, 2)) %>%
        left_join(universal_products_meta, by = c('data_class_code', 'data_source_code')) %>%
        select(-data_class_code, -data_source_code)

    write_csv(raw_spatial_dat, 'data/general/watershed_raw_spatial_timeseries.csv')
    write_csv(meta_data, 'data/general/watershed_raw_spatial_timeseries_meta.csv')
}

generate_watershed_summaries()

generate_watershed_raw_spatial_dataset()

