trouble <- c("chegovar", "juno", "levy", "sabath", "tammy", "tyra", "uri") # these are the ones that were present in my fall 2020 season but not in elvira's... but now she can't find them.

targets::tar_load(fixed_names)
targets::tar_load(seasons_list)

all(trouble %in% fixed_names$Nili_id) # true--so they are in there. Are they present in the first season, though?
all(trouble %in% seasons_list[[1]]$Nili_id) # true--they are in there too.

## How many vultures do I have listed in the table? See figuresTables.R
## How many vultures do I have in the social data?
tar_load(downsampled_10min_forSocial)