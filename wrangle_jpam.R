##
## Function to wrangle junior-PAM data 
## 



wrangle_jPAM <- function(file) {
  # extract treeID from data filename
  treeID <- str_extract(file, allTreeIDs)
  treeID <- treeID[!is.na(treeID)]
  
  # shortening filename
  directory <- (str_sub(basename(file), start=1, end=-5))
  
  # pulling out metadata for this particular file from metadata df using the filename
  file_meta <- metadata %>%
    dplyr::filter(jpam_directory == directory) %>%
    filter(!is.na(jPAM_start)) %>%
    # fixing datetime format
    mutate(jPAM_start = ymd_hms(paste(as.character(Date), as.character(jPAM_start))),
           jPAM_end = ymd_hms(paste(as.character(Date), as.character(jPAM_end))))
  
  # reading in actual jpam data
  df <- read.csv2(file, header=T, skip=1, na.strings ="-") %>%
    dplyr::filter(!is.na(No.)) %>%
    # cleaning column names
    rename_with(~sub("^...", "", .), starts_with("X")) %>%
    rename_with(~sub("^..", "", .), starts_with("1")) %>%
    rename_with(~sub("^..", "", .), starts_with("2")) %>%
    mutate(Datetime = ymd_hms(paste(Date, Time))) %>%
    mutate(treeID = treeID) %>%
    # creating BranchID columns and Hour columns in jPAM data using metadata
    mutate(BranchID = NA) %>%
    mutate(Hour = NA) %>%
    mutate_at(vars(BranchID),
              # the start and end times for each branch were recorded in the metadata
              # creating BranchID column based on these start and end times
              ~ case_when(Datetime >= file_meta$jPAM_start[1] & Datetime <= (file_meta$jPAM_end[1]+1) ~ 1,
                          Datetime >= file_meta$jPAM_start[2] & Datetime <= (file_meta$jPAM_end[2]+1) ~ 2,
                          Datetime >= file_meta$jPAM_start[3] & Datetime <= (file_meta$jPAM_end[3]+1) ~ 3)) %>%
    # Hour column is for diurnal data
    mutate_at(vars(Hour),
              ~ case_when(Datetime >= file_meta$jPAM_start[1] & Datetime <= file_meta$jPAM_end[1] ~ file_meta$Hour[1],
                          Datetime >= file_meta$jPAM_start[2] & Datetime <= file_meta$jPAM_end[2] ~ file_meta$Hour[2],
                          Datetime >= file_meta$jPAM_start[3] & Datetime <= file_meta$jPAM_end[3] ~ file_meta$Hour[3])) %>%
    group_by(BranchID) %>%
    # calculating parameter maxima
    mutate(F_max = max(F),
           Fm_max = max(Fm.),
           ETR_max = max(as.numeric(ETR), na.rm=T)) %>%
    rowwise() %>%
    # reformatting and cleaning columns
    mutate(F_rel = as.numeric(F) / as.numeric(F_max),
           Fm_rel = Fm. / as.numeric(Fm_max),
           ETR_rel = as.numeric(ETR) / as.numeric(ETR_max),
           PAR = PAR,
           YPSII = Y..II.,
           NPQ = as.numeric(NPQ),
           ETR = as.numeric(ETR)) %>%
    dplyr::select(-(c(Y..II.)))
  
  return(df)
}