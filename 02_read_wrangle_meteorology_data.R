# Reading and wrangling site meteorological data
# Also gap-fills with data from closest weather station in Boonville
# Written by Lily Klinek (lpklinek@ucdavis.edu)





## Preparation and reading data -----
# source required functions
source('./R_functions/calc_avp.R')
source('./R_functions/calc_svp.R')

# read in met data 
Garcia_meteo <- read_csv('./data/raw/02_meteo_data_raw/Garcia_meteo_all_101724.csv', skip=2)



## Cleaning met data ------
# reformatting timestamp, changing variables to numeric, selecting relevant columns
all_meteo <- Garcia_meteo %>%
  mutate(Timestamp = mdy_hm(Timestamp)) %>%
  mutate(across(c(2:19), as.numeric)) %>%
  select(-c(4, 5, 6, 8, 12, 13, 15:19))

# Setting correct column names
colnames(all_meteo) = c("Timestamp", "radiation_watts_per_m2", "precip_mm", "windspeed_m_per_s", "Tair", "RH", "atm_pressure_kPa", "max_precip_rate", "Site")

## Calculating vapor pressure parameters -----
# calculating SVP
all_meteo$SVP_kPa <- sapply(all_meteo$Tair, calc_svp)

# calculating AVP
all_meteo$AVP_kPa <- mapply(calc_avp, all_meteo$SVP_kPa, all_meteo$RH, all_meteo$atm_pressure_kPa)

# calculating VPD
all_meteo$VPD_kPa <- all_meteo$SVP_kPa - all_meteo$AVP_kPa


## aggregating data to get daily values -----
all_meteo_aggregated <- all_meteo %>%
  mutate(date = format(Timestamp, "%Y-%m-%d")) %>%
  group_by(date) %>%
  filter(!is.na(Tair)) %>%
  summarize(Tmax = max(Tair, na.rm=T),
            Tmin = min(Tair, na.rm=T),
            Tavg = mean(Tair, na.rm=T),
            daily_precip = sum(precip_mm),
            RHmax = max(RH, na.rm=T),
            RHmin = min(RH, na.rm=T),
            RHavg = mean(RH, na.rm=T),
            VPDmin = min(VPD_kPa, na.rm=T),
            VPDmax = max(VPD_kPa, na.rm=T),
            VPDavg = mean(VPD_kPa, na.rm=T)) %>%
  mutate(date = as.Date(date))


## gap-filling site met data with Boonville data -----

# read boonville data
boonville_met <- read.csv('./data/raw/02_meteo_data_raw/boonville_meteo_raw.csv') %>%
  select(DATE, TMAX, TMIN, TAVG) %>%
  mutate(TMAX = (TMAX-32)*(5/9),
         TMIN = (TMIN-32)*(5/9),
         TAVG = (TAVG-32)*(5/9)) %>%
  mutate(DATE = ymd(DATE))

# define the full date range
full_dates <- seq.Date(min(all_meteo_aggregated$date), ymd('2024-09-28'), by = "day")

# join with met data to get NA rows for dates with missing data
all_meteo_complete <- data.frame(date = full_dates) %>%
  left_join(all_meteo_aggregated, by = "date")


# merge the complete dataset with Boonville weather station data
patched_data <- all_meteo_complete %>%
  # left join to keep all rows from the complete sequence of dates
  left_join(boonville_met, by = c("date" = "DATE"), suffix = c("_met", "_local")) %>%
  
  # patching temperature data with measurements from Boonville
  mutate(
    Tmax = ifelse(is.na(Tmax), TMAX, Tmax),
    Tmin = ifelse(is.na(Tmin), TMIN, Tmin),
    Tavg = ifelse(is.na(Tavg), TAVG, Tavg)
  ) %>%
  
  # select relevant columns
  select(date, Site, Tmax, Tmin, Tavg, VPDmin, VPDavg, VPDmax, everything())

# Saving processed output ----

write_csv(all_meteo_complete, './data/processed/meteo_data_processed.csv',
          overwrite=TRUE)