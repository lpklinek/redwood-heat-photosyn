
# Late Summer Heat and High Vapor Pressure Deficit Lead to Photosynthetic Downregulation in Coast Redwoods (data and code)

L. Klinek, J. Au, M. Rodriguez-Caton, M. Palat Rao, L. Fety, G. Koch, T. Dawson, and T.S.
Magney

This repository contains all code and data associated with the paper:

Klinek et al. (2025), Late Summer Heat and High Vapor Pressure Deficit Lead to Photosynthetic Downregulation in Coast Redwoods.

To reference the paper, data, or methods, please cite Klinek et al. 2025.
Contact Lily Klinek with questions at lpklinek\@ucdavis.edu.
This repository's DOI is: [![DOI](https://zenodo.org/badge/962820915.svg)](https://doi.org/10.5281/zenodo.15226480)

## 1. Overview

This repository contains code to:

-   wrangle and clean coast redwood physiological and fluorescence data, collected at Garcia River Forest in Mendocino County

-   analyze and visualize relationships between stomatal parameters, fluorescence parameters, and environmental conditions, particularly relating to heat stress

-   analyze and visualize temperature response curves collected using the LI-COR LI6800 portable gas exchange instrument, and light response curves collected using the WALZ JUNIOR-PAM fluorometer

Users can download a zip file of the entire repository by clicking on the green `code` tab at the top of the page, and then clicking `Download ZIP`. Users with GitHub accounts can also `fork` the repository.

## 2. Organization and Workflow

The repository is organized as follows:

-   R scripts for data cleaning, processing, and visualization are contained in the main repository folder, and are numbered in the order they should be run

-   `data` contains all data inputs and outputs.

    -   `raw` contains raw data and associated metadata, with folders for each data type

    -   `processed` contains cleaned, processed data outputted by scripts 01-04

-   `R_functions` contains functions called in wrangling scripts

## 3. Data and Metadata

All raw, unprocessed data can be found in `data/raw/` .

#### [Metadata]{.underline}

Folder name: `00_metadata`

The metadata for all sampling done at Garcia River Forest can be found in the `klinek_metadata_complete.csv` file within the folder `00_metadata` in the raw data directory.
Below is a basic description of each of the columns in the metadata csv:

| Variable Name | Description |
|----------------------|-------------------------------------------------|
| Date | Date of data collection, MM/DD/YY |
| Site | Site name (data was originally collected as part of a multi-site study, but only data from Garcia River Forest was used in the analysis for this paper) |
| TreeID | individual tree ID, one of E, G, or H |
| BranchID | branch ID (1-3), since measurements were taken on three branches at each tree |
| Li600_1, Li600_2, Li600_3 | Li600 scan \# for replicate measurements 1, 2, and 3, respectively |
| Dark_Li600 | Li600 scan \# for the dark-adapted measurement on each branch |
| jPAM_start | start time for light-response curve collected with the WALZ JUNIOR-PAM |
| jPAM_end | end time for light-response curve collected with the WALZ JUNIOR-PAM |
| Hour | hour of measurement (rounded to nearest whole hour) for days where measurements were taken multiple times throughout the day |
| jPAM_directory | filename containing the JUNIOR-PAM data associated with that branch |

#### [LI-COR LI600 Data]{.underline}

Folder name: `01_li600_data_raw`

Data is organized in folders named with measurement date (YYYY-MM-DD).
Within each folder, key measurements for all scans are contained in the .csv file with name beginning with "Manual_gsw+F_LI_COR_Default".
Columns 1-4 (LightDark, Site, TreeID, and Hour) have been manually inputted with associated metadata for each scan.
LightDark describes the light condition for each leaf (L: light-adapted, measurement taken in ambient light; D: dark-adapted).
The other .csv files in each folder are the detailed, high-resolution measurements taken during each individual scan.

Information on all LI600 data columns can be found in the LI-COR LI600 instrument manual, linked [here](https://www.licor.com/support/LI-600/topics/data-file-descriptions.html#Data).

#### [Meteorological Data]{.underline}

Folder name: `02_meteo_data_raw`

All meteorological data for Garcia River Forest is contained in the "Garcia_meteo_all_101724.csv" file.
Data was collected using a METER ATMOS 41 weather station and a METER ZL6 data logger.
Units and variable name for each column can be found in row 3 of the .csv file.

Meteorological data was gap-filled using free, open-access weather station data from the nearby Boonville weather station (ID: USR0000CBOO) in the Global Historical Climatology Network (GHCN).
All Boonville weather data used for this analysis is contained in the "boonville_meteo_raw.csv" file.
Additional information about the GHCN can be found in [Menne et al. 2012](https://doi.org/10.1175/JTECH-D-11-00103.1), and further documentation and metadata can be accessed [here](https://www.ncei.noaa.gov/pub/data/cdo/documentation/GHCND_documentation.pdf).
Data was accessed and downloaded using NOAA's [Climate Data Online](https://www.ncdc.noaa.gov/cdo-web/) tool.

#### [JUNIOR-PAM Data]{.underline}

Folder name: `03_jpam_data_raw`

JUNIOR-PAM data files are labeled with collection date and treeID in the format `MMDDYY_treeID.` Data from 2022 are simply labeled `MMDD_treeID`.
This is cleaned and corrected by the processing script for consistency.
Filenames with a number after the treeID are indicative of multiple rounds of sampling done in the same day, wherein the number refers to the sampling round of that file (ex. `040824_GarciaH_2.csv` corresponds to the the second round of sampling for tree GarciaH on 04/08/24).
Each data collection has a `.csv` file and a `.pam` file – the `.pam` file can only be properly opened and accessed using the WALZ WinControl-3 software.
Detailed descriptions of each variable can be found in the [instrument manual](https://www.walz.com/files/downloads/manuals/junior-pam/JUNIOR_PAM_02.pdf).

#### [LI-COR LI6800 Data]{.underline}

Folder name: `04_li6800_data_raw`

This folder contains temperature response curve data collected using the LI6800.
Each filename is named with the collection date, instrument sample ID, and treeID in the format `YYYY-MM-DD-####_logdata_site_tree`.
Each sample is saved in a `.csv` file, a `.xlsx` file, and a Unix executable file.
Information about each variable can be found in the instrument manual's [Summary of symbols](https://www.licor.com/support/LI-6800/topics/symbols.html#Summaryofsymbols).

## 4. Instructions for Use

First, open the `00_install_load_packages.R` script and edit line 43 to set your working directory to wherever the `Klinek_et_al_2025` folder has been saved on your local computer.

Once the working directory has been set, data cleaning and processing scripts should be run in the following numeric order:

`00_install_load_packages.R`

`01_read_wrangle_li600.R`

`02_read_wrangle_meteorology_data.R`

`03_read_wrangle_jpam_data.R`

`04_read_wrangle_li6800_data.R`

Scripts 05-12 can be used to replicate the figures in Klinek et al. 2025.

## 5. Issues or Questions

All inquiries should be directed to Lily Klinek at lpklinek\@ucdavis.edu – please don't hesitate to reach out!
