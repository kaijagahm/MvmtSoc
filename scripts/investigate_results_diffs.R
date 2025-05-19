# Script to investigate any changes to the results after implementing the filter_vaid_data function
# 2025-05-19
# Kaija Gahm
library(tidyverse)
library(targets)
tar_load(fixed_names)
tar_load(removed_beforeafter_deploy)

before <- fixed_names %>% group_by(Nili_id) %>% summarize(ndays = length(unique(dateOnly)), minday = min(dateOnly), maxday = max(dateOnly), nrows = n())

after <- removed_beforeafter_deploy %>% group_by(Nili_id) %>% summarize(ndays = length(unique(dateOnly)), minday = min(dateOnly), maxday = max(dateOnly), nrows = n())

changed <- left_join(before, after, by = "Nili_id")

tar_load(space_use)
# Now I can go and manually compare the current version of space_use to the version that I sent to Orr on 5/15/25.
# First, I checked through all 16 individuals that appear in `changed` to see if they appear in the original space_use file and in the new space_use file. The results were consistent: cork, hadav, oded, phili, thronos, and xena appeared in both; the other 10 individuals appeared in neither. That means that the day differences didn't add or remove any individuals from the analysis.
changed %>% filter(ndays.x != ndays.y) %>% mutate(diff_days = ndays.x-ndays.y) %>% filter(Nili_id %in% space_use$Nili_id)
# Next, I checked each of the remaining 6 (above) in both space_use files, paying particular attention to the pc1 column. 
# cork: no changes greater than the 3rd decimal place
# hadav: tiny increase in pc1 for S22: 1.199 --> 1.218. For the other seasons, no appreciable change.
# All other individuals: no changes greater than the 3rd decimal place in any seasons.

# Also looked at the model outputs and compared them to the results in the draft--no large changes. A couple of the coefficients changed by tiiiiny amounts, but not enough to affect any of the conclusions. No changes to statistical significance, direction of relationship, or anything like that.