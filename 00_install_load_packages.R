# Install packages -------------------------------------------------------

# uncomment these lines as needed for package installation
# install.packages('ggplot2')
# install.packages('ggpmisc')
# install.packages('ggpubr')
# install.packages('leaflet')
# install.packages('lubridate')
# install.packages('maptiles')
# install.packages('mgcv')
# install.packages('pavo')
# install.packages('readxl')
# install.packages('stringr')
# install.packages('terra')
# install.packages('tidyverse')
# install.packages('viridis')


# Load libraries ----------------------------------------------------------

library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(leaflet)
library(lubridate)
library(maptiles)
library(mgcv)
library(pavo)
library(readxl)
library(stringr)
library(terra)
library(tidyverse)
library(viridis)


# do i use these ones?
# library(chron)
# library(scales)


# ***** Set working directory (this will need to be edited to run on a local computer!!) ***** -----

setwd('/Users/lklinek/Desktop/Redwood/Klinek_et_al_2025') # edit this line with local filepath of Klinek_et_al_2025 folder

# check to ensure wd is set properly
list.files(getwd()) # should output the list of scripts and folders within Klinek et al. 2025 folder

