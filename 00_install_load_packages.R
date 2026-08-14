# Install packages -------------------------------------------------------

# uncomment these lines as needed for package installation
# install.packages('broom')
# install.packages('broom.mixed')
# install.packages('chron')
# install.packages('dplyr')
# install.packages('ggplot2')
# install.packages('ggpmisc')
# install.packages('ggpubr')
# install.packages('ISLR')
# install.packages('leaflet')
# install.packages('leaflet.extras2')
# install.packages('lme4')
# install.packages('lmerTest')
# install.packages('lubridate')
# install.packages('maptiles')
# install.packages('merTools')
# install.packages('mgcv')
# install.packages('nls.multstart')
# install.packages('patchwork')
# install.packages('pavo')
# install.packages('performance')
# install.packages('purrr)
# install.packages('readxl')
# install.packages('rTPC')
# install.packages('remotes')
# install.packages('scales')
# install.packages('stringr')
# install.packages('terra')
# install.packages('tibble')
# install.packages('tidyr')
# install.packages('tidyverse')
# install.packages('viridis')

# for AOI and climateR installation: 
remotes::install_github("mikejohnson51/AOI")
remotes::install_github("mikejohnson51/climateR")

# Load libraries ----------------------------------------------------------

library(AOI)
library(broom)
library(broom.mixed)
library(chron)
library(climateR)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(ISLR)
library(leaflet)
library(leaflet.extras2)
library(lme4)
library(lmerTest)
library(lubridate)
library(maptiles)
library(merTools)
library(mgcv)
library(nls.multstart)
library(patchwork)
library(pavo)
library(performance)
library(purrr)
library(readxl)
library(rTPC)
library(remotes)
library(scales)
library(stringr)
library(terra)
library(tibble)
library(tidyr)
library(tidyverse)
library(viridis)


# ***** Set working directory (this will need to be edited to run on a local computer!!) ***** -----

setwd('/Users/lklinek/Desktop/Redwood/Klinek_et_al_2026')# edit this line with local filepath of Klinek_et_al_2026 folder

# check to ensure wd is set properly
list.files(getwd()) # should output the list of scripts and folders within Klinek et al. 2026 folder

