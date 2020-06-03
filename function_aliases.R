#complete this list with any common, globally (across macrosheds) used funcs.
#refer to everything else explicity with ::
#this will make packaging easier and more professional

read_csv = readr::read_csv
read_feather = feather::read_feather
glue = glue::glue
filter = dplyr::filter
select = dplyr::select
mutate = dplyr::mutate
mutate_all = dplyr::mutate_all
mutate_at = dplyr::mutate_at
mutate_if = dplyr::mutate_if
group_by = dplyr::group_by
ungroup = dplyr::ungroup
summarize = dplyr::summarize
summarize_all = dplyr::summarize_all
summarize_at = dplyr::summarize_at
summarize_if = dplyr::summarize_if
left_join = dplyr::left_join
right_join = dplyr::right_join
full_join = dplyr::full_join
vars = dplyr::vars
pull = dplyr::pull
distinct = dplyr::distinct
`%>%` = magrittr::`%>%`
`.` = plyr::`.`
sm = suppressMessages
sw = suppressWarnings
