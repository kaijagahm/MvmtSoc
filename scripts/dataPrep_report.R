inpa <- read_feather(here("data/dataPrep/inpa.feather"))
inpa_nrow <- nrow(inpa)
inpa_vultures <- unique(inpa$local_identifier)
inpa_nvultures <- length(inpa_vultures)
rm(inpa)

ornitela <- read_feather(here("data/dataPrep/ornitela.feather"))
ornitela_nrow <- nrow(ornitela)
ornitela_vultures <- unique(ornitela$local_identifier)
ornitela_nvultures <- length(ornitela_vultures)
rm(ornitela)

joined <- read_feather(here("data/dataPrep/joined.feather"))
joined_nrow <- nrow(joined)
joined_vultures <- unique(joined$Nili_id)
joined_nvultures <- length(joined_vultures)
rm(joined)

load(here("data/dataPrep/removed_periods.Rda"))
rp_nrow <- nrow(removed_periods)
rp_vultures <- unique(removed_periods$Nili_id)
rp_nvultures <- length(rp_vultures)
rm(removed_periods)

load(here("data/dataPrep/cleaned.Rda"))
c_nrow <- nrow(cleaned)
c_vultures <- unique(cleaned$Nili_id)
c_nvultures <- length(c_vultures)
rm(cleaned)

load(here("data/dataPrep/removed_captures.Rda"))
rc_nrow <- nrow(removed_captures)
rc_vultures <- unique(removed_captures$Nili_id)
rc_nvultures <- length(rc_vultures)
rm(removed_captures)

load(here("data/dataPrep/data_masked.Rda"))
masked_nrow <- nrow(data_masked)
masked_vultures <- unique(data_masked$Nili_id)
masked_nvultures <- length(masked_vultures)
rm(data_masked)

# hereafter, these are per-season lists
load(here("data/dataPrep/season_names.Rda"))
load(here("data/dataPrep/seasons_list.Rda"))
sl_nrow <- map_dbl(seasons_list, nrow)
sl_vultures <- map(seasons_list, ~unique(.x$Nili_id))
sl_nvultures <- map_dbl(sl_vultures, length)
rm(seasons_list)

load(here("data/dataPrep/removed_northern.Rda"))
rno_nrow <- map_dbl(removed_northern, nrow)
rno_vultures <- map(removed_northern, ~unique(.x$Nili_id))
rno_nvultures <- map_dbl(rno_vultures, length)
rm(removed_northern)

load(here("data/dataPrep/removed_lfr.Rda"))
rlf_nrow <- map_dbl(removed_lfr, nrow)
rlf_vultures <- map(removed_lfr, ~unique(.x$Nili_id))
rlf_nvultures <- map_dbl(rlf_vultures, length)
rm(removed_lfr)

load(here("data/dataPrep/downsampled_10min_forSocial.Rda"))
ds_nrow <- map_dbl(downsampled_10min_forSocial, nrow)
ds_vultures <- map(downsampled_10min_forSocial, ~unique(.x$Nili_id))
ds_nvultures <- map_dbl(ds_vultures, length)
rm(downsampled_10min_forSocial)

load(here("data/dataPrep/roosts.Rda"))
r_nrow <- map_dbl(roosts, nrow)
r_vultures <- map(roosts, ~unique(.x$Nili_id))
r_nvultures <- map_dbl(r_vultures, length)
rm(roosts)

load(here("data/dataPrep/removed_nighttime.Rda"))
rni_nrow <- map_dbl(removed_nighttime, nrow)
rni_vultures <- map(removed_nighttime, ~unique(.x$Nili_id))
rni_nvultures <- map_dbl(rni_vultures, length)
rm(removed_nighttime)

load(here("data/dataPrep/removed_lowppd.Rda"))
rlp_nrow <- map_dbl(removed_lowppd, nrow)
rlp_vultures <- map(removed_lowppd, ~unique(.x$Nili_id))
rlp_nvultures <- map_dbl(rlp_vultures, length)
rm(removed_lowppd)

load(here("data/dataPrep/removed_too_few_days.Rda"))
rt_nrow <- map_dbl(removed_too_few_days, nrow)
rt_vultures <- map(removed_too_few_days, ~unique(.x$Nili_id))
rt_nvultures <- map_dbl(rt_vultures, length)
rm(removed_too_few_days)

load(here("data/dataPrep/downsampled_10min.Rda"))
d_nrow <- map_dbl(downsampled_10min, nrow)
d_vultures <- map(downsampled_10min, ~unique(.x$Nili_id))
d_nvultures <- map_dbl(d_vultures, length)
rm(downsampled_10min)

dataPrep_report_stats <- list("dataset" = c("inpa", "ornitela", "joined", "removed_periods", "data_cleaned", "removed_captures", "data_masked_2", "seasons_list", "removed_northern", "removed_lfr", "downsampled_10min_forSocial", "roosts", "removed_nighttime", "removed_lowppd", "removed_too_few_days", "downsampled_10min"),
                              "nrow" = list(inpa_nrow, ornitela_nrow, joined_nrow, rp_nrow, dc_nrow, rc_nrow, masked_nrow, sl_nrow, rno_nrow, rlf_nrow, ds_nrow, r_nrow, rni_nrow, rlp_nrow, rt_nrow, d_nrow),
                              "vultures" = list(inpa_vultures, ornitela_vultures, joined_vultures, rp_vultures, dc_vultures, rc_vultures, masked_vultures, sl_vultures, rno_vultures, rlf_vultures, ds_vultures, r_vultures, rni_vultures, rlp_vultures, rt_vultures, d_vultures),
                              "nvultures" = list(inpa_nvultures, ornitela_nvultures, joined_nvultures, rp_nvultures, dc_nvultures, rc_nvultures, masked_nvultures, sl_nvultures, rno_nvultures, rlf_nvultures, ds_nvultures, r_nvultures, rni_nvultures, rlp_nvultures, rt_nvultures, d_nvultures))

save(dataPrep_report_stats, file = "data/dataPrep/dataPrep_report_stats.Rda")
