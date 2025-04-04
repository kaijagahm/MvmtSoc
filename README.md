# Code for "Social interactions in an avian scavenger are driven by social preference and movement, depending on the social situation"
## Authors
Kaija Gahm, Marta Acácio, Nili Anglister, Gideon Vaadia, Orr Spiegel, Noa Pinter-Wollman
Corresponding author: Kaija Gahm (kgahm@ucla.edu)

# Overview
The data cleaning and preparation steps are all contained in the targets pipeline. Outputs are in the _targets/ folder and can be loaded with tar_load(). 

After the targets pipeline is finished, the modeling happens in the script called "scripts/mixedModels.R" and the figures and tables are created in "scripts/figuresTables.R". Figures are saved to the "fig/" directory. 

Model objects, along with other data files, are saved to the data/ folder, which is not included in this repository for size reasons. Data files needed in the pipeline are either raw (provided on Dryad) or derived (created from the code and/or provided on Dryad, depending on the file and how long it takes to create).

# File structure
### _targets.R
The R script that outlines the targets pipeline, with informative headings. The functions that are used in the targets pipeline are defined in "R/functions.R".
### .Rprofile
### MvmtSoc.Rproj
### .gitignore
As shown in the gitignore, data files are not included here; they can be obtained from Dryad or derived from the pipeline itself.
### renv.lock
### README.md
This file
### _targets.yaml
File pointing to where the targets pipeline is stored; important for tar_make() to know where to find the code and do its job.
### _targets/
Folder containing the output objects from running the targets pipeline
### credentials/
This folder is not included in the repo. To fully reproduce the project, must have movebank and stadia credentials. Instead of using these credentials, someone looking to reproduce this code can obtain the raw data from the Dryad repo.
### data/
Not included here
### fig/
Figures and tables
### R/
Contains only one script, "functions.R", which is where the functions used in the targets pipeline are defined. Note that this is distinct from the modeling code and the code to generate the figures and tables, which can be found in "scripts/"
### renv/
Info for renv--package management
### scripts/
Folder containing the modeling script and the script for creating figures and tables.

## Dryad link for data:
