# Package ID: edi.233.5 Cataloging System:https://pasta.edirepository.org.
# Data set title: Interagency Ecological Program: Fish catch and water quality data from the Sacramento River floodplain and tidal slough, collected by the Yolo Bypass Fish Monitoring Program, 1998-2024..
# Data set creator:   Interagency Ecological Program -
# Data set creator:  Lisa Vance - California Department of Water Resources
# Data set creator:  Nicole Kwan - California Department of Water Resources
# Contact:  Lisa Vance -  California Department of Water Resources  - lisa.vance@water.ca.gov
# Contact:  Nicole Kwan -  California Department of Water Resources  - Nicole.Kwan@water.ca.gov
# Contact:  Naoaki Ikemiyagi -  California Department of Fish and Wildlife  -
# Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())

# setwd("C:/users/my_name/my_dir")

options(HTTPUserAgent = "EDI_CodeGen")

inUrl2 <- "https://pasta.lternet.edu/package/data/eml/edi/233/5/4488201fee45953b001f70acf30f7734"
infile2 <- tempfile()
try(download.file(
  inUrl2,
  infile2,
  method = "curl",
  extra = paste0(' -A "', getOption("HTTPUserAgent"), '"')
))
if (is.na(file.size(infile2))) {
  download.file(inUrl2, infile2, method = "auto")
}


dt2 <- read.csv(
  infile2,
  header = F,
  skip = 1,
  sep = ",",
  quot = '"',
  col.names = c(
    "EventID",
    "StationCode",
    "Datetime",
    "SampleDate",
    "WaterTemp",
    "SpecificConductance",
    "Conductivity",
    "Turbidity",
    "DO",
    "pH",
    "Secchi",
    "Tide",
    "WeatherCode",
    "VegetationRank",
    "SubstrateCode",
    "HabitatType",
    "MicrocystisRank",
    "MethodCode",
    "GearCode",
    "GearConditionCode",
    "SampleAltered",
    "FieldComments",
    "Flag_WQ",
    "Comment_WQ",
    "Duplicated"
  ),
  check.names = TRUE
)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$EventID) != "factor") {
  dt2$EventID <- as.factor(dt2$EventID)
}
if (class(dt2$StationCode) != "factor") {
  dt2$StationCode <- as.factor(dt2$StationCode)
}
# attempting to convert dt2$Datetime dateTime string to R date structure (date or POSIXct)
tmpDateFormat <- "%m/%d/%Y %H:%M"
tmp2Datetime <- as.POSIXct(dt2$Datetime, format = tmpDateFormat)
# Keep the new dates only if they all converted correctly
if (
  nrow(dt2[dt2$Datetime != "", ]) == length(tmp2Datetime[!is.na(tmp2Datetime)])
) {
  dt2$Datetime <- tmp2Datetime
} else {
  print(
    "Date conversion failed for dt2$Datetime. Please inspect the data and do the date conversion yourself."
  )
}

# attempting to convert dt2$SampleDate dateTime string to R date structure (date or POSIXct)
tmpDateFormat <- "%m/%d/%Y"
tmp2SampleDate <- as.Date(dt2$SampleDate, format = tmpDateFormat)
# Keep the new dates only if they all converted correctly
if (
  nrow(dt2[dt2$SampleDate != "", ]) ==
    length(tmp2SampleDate[!is.na(tmp2SampleDate)])
) {
  dt2$SampleDate <- tmp2SampleDate
} else {
  print(
    "Date conversion failed for dt2$SampleDate. Please inspect the data and do the date conversion yourself."
  )
}

if (class(dt2$WaterTemp) == "factor") {
  dt2$WaterTemp <- as.numeric(levels(dt2$WaterTemp))[as.integer(dt2$WaterTemp)]
}
if (class(dt2$WaterTemp) == "character") {
  dt2$WaterTemp <- as.numeric(dt2$WaterTemp)
}
if (class(dt2$SpecificConductance) == "factor") {
  dt2$SpecificConductance <- as.numeric(levels(
    dt2$SpecificConductance
  ))[as.integer(dt2$SpecificConductance)]
}
if (class(dt2$SpecificConductance) == "character") {
  dt2$SpecificConductance <- as.numeric(dt2$SpecificConductance)
}
if (class(dt2$Conductivity) == "factor") {
  dt2$Conductivity <- as.numeric(levels(dt2$Conductivity))[as.integer(
    dt2$Conductivity
  )]
}
if (class(dt2$Conductivity) == "character") {
  dt2$Conductivity <- as.numeric(dt2$Conductivity)
}
if (class(dt2$Turbidity) == "factor") {
  dt2$Turbidity <- as.numeric(levels(dt2$Turbidity))[as.integer(dt2$Turbidity)]
}
if (class(dt2$Turbidity) == "character") {
  dt2$Turbidity <- as.numeric(dt2$Turbidity)
}
if (class(dt2$DO) == "factor") {
  dt2$DO <- as.numeric(levels(dt2$DO))[as.integer(dt2$DO)]
}
if (class(dt2$DO) == "character") {
  dt2$DO <- as.numeric(dt2$DO)
}
if (class(dt2$pH) == "factor") {
  dt2$pH <- as.numeric(levels(dt2$pH))[as.integer(dt2$pH)]
}
if (class(dt2$pH) == "character") {
  dt2$pH <- as.numeric(dt2$pH)
}
if (class(dt2$Secchi) == "factor") {
  dt2$Secchi <- as.numeric(levels(dt2$Secchi))[as.integer(dt2$Secchi)]
}
if (class(dt2$Secchi) == "character") {
  dt2$Secchi <- as.numeric(dt2$Secchi)
}
if (class(dt2$Tide) != "factor") {
  dt2$Tide <- as.factor(dt2$Tide)
}
if (class(dt2$WeatherCode) != "factor") {
  dt2$WeatherCode <- as.factor(dt2$WeatherCode)
}
if (class(dt2$VegetationRank) != "factor") {
  dt2$VegetationRank <- as.factor(dt2$VegetationRank)
}
if (class(dt2$SubstrateCode) != "factor") {
  dt2$SubstrateCode <- as.factor(dt2$SubstrateCode)
}
if (class(dt2$HabitatType) != "factor") {
  dt2$HabitatType <- as.factor(dt2$HabitatType)
}
if (class(dt2$MicrocystisRank) != "factor") {
  dt2$MicrocystisRank <- as.factor(dt2$MicrocystisRank)
}
if (class(dt2$MethodCode) != "factor") {
  dt2$MethodCode <- as.factor(dt2$MethodCode)
}
if (class(dt2$GearCode) != "factor") {
  dt2$GearCode <- as.factor(dt2$GearCode)
}
if (class(dt2$GearConditionCode) != "factor") {
  dt2$GearConditionCode <- as.factor(dt2$GearConditionCode)
}
if (class(dt2$SampleAltered) != "factor") {
  dt2$SampleAltered <- as.factor(dt2$SampleAltered)
}
if (class(dt2$FieldComments) != "factor") {
  dt2$FieldComments <- as.factor(dt2$FieldComments)
}
if (class(dt2$Flag_WQ) != "factor") {
  dt2$Flag_WQ <- as.factor(dt2$Flag_WQ)
}
if (class(dt2$Comment_WQ) != "factor") {
  dt2$Comment_WQ <- as.factor(dt2$Comment_WQ)
}
if (class(dt2$Duplicated) != "factor") {
  dt2$Duplicated <- as.factor(dt2$Duplicated)
}

# Convert Missing Values to NA for non-dates

dt2$WaterTemp <- ifelse(
  (trimws(as.character(dt2$WaterTemp)) == trimws("NA")),
  NA,
  dt2$WaterTemp
)
suppressWarnings(
  dt2$WaterTemp <- ifelse(
    !is.na(as.numeric("NA")) &
      (trimws(as.character(dt2$WaterTemp)) == as.character(as.numeric("NA"))),
    NA,
    dt2$WaterTemp
  )
)
dt2$SpecificConductance <- ifelse(
  (trimws(as.character(dt2$SpecificConductance)) == trimws("NA")),
  NA,
  dt2$SpecificConductance
)
suppressWarnings(
  dt2$SpecificConductance <- ifelse(
    !is.na(as.numeric("NA")) &
      (trimws(as.character(dt2$SpecificConductance)) ==
        as.character(as.numeric("NA"))),
    NA,
    dt2$SpecificConductance
  )
)
dt2$Conductivity <- ifelse(
  (trimws(as.character(dt2$Conductivity)) == trimws("NA")),
  NA,
  dt2$Conductivity
)
suppressWarnings(
  dt2$Conductivity <- ifelse(
    !is.na(as.numeric("NA")) &
      (trimws(as.character(dt2$Conductivity)) ==
        as.character(as.numeric("NA"))),
    NA,
    dt2$Conductivity
  )
)
dt2$Turbidity <- ifelse(
  (trimws(as.character(dt2$Turbidity)) == trimws("NA")),
  NA,
  dt2$Turbidity
)
suppressWarnings(
  dt2$Turbidity <- ifelse(
    !is.na(as.numeric("NA")) &
      (trimws(as.character(dt2$Turbidity)) == as.character(as.numeric("NA"))),
    NA,
    dt2$Turbidity
  )
)
dt2$DO <- ifelse((trimws(as.character(dt2$DO)) == trimws("NA")), NA, dt2$DO)
suppressWarnings(
  dt2$DO <- ifelse(
    !is.na(as.numeric("NA")) &
      (trimws(as.character(dt2$DO)) == as.character(as.numeric("NA"))),
    NA,
    dt2$DO
  )
)
dt2$pH <- ifelse((trimws(as.character(dt2$pH)) == trimws("NA")), NA, dt2$pH)
suppressWarnings(
  dt2$pH <- ifelse(
    !is.na(as.numeric("NA")) &
      (trimws(as.character(dt2$pH)) == as.character(as.numeric("NA"))),
    NA,
    dt2$pH
  )
)
dt2$Secchi <- ifelse(
  (trimws(as.character(dt2$Secchi)) == trimws("NA")),
  NA,
  dt2$Secchi
)
suppressWarnings(
  dt2$Secchi <- ifelse(
    !is.na(as.numeric("NA")) &
      (trimws(as.character(dt2$Secchi)) == as.character(as.numeric("NA"))),
    NA,
    dt2$Secchi
  )
)
dt2$Tide <- as.factor(ifelse(
  (trimws(as.character(dt2$Tide)) == trimws("NA")),
  NA,
  as.character(dt2$Tide)
))
dt2$WeatherCode <- as.factor(ifelse(
  (trimws(as.character(dt2$WeatherCode)) == trimws("NA")),
  NA,
  as.character(dt2$WeatherCode)
))
dt2$VegetationRank <- as.factor(ifelse(
  (trimws(as.character(dt2$VegetationRank)) == trimws("NA")),
  NA,
  as.character(dt2$VegetationRank)
))
dt2$SubstrateCode <- as.factor(ifelse(
  (trimws(as.character(dt2$SubstrateCode)) == trimws("NA")),
  NA,
  as.character(dt2$SubstrateCode)
))
dt2$HabitatType <- as.factor(ifelse(
  (trimws(as.character(dt2$HabitatType)) == trimws("NA")),
  NA,
  as.character(dt2$HabitatType)
))
dt2$MicrocystisRank <- as.factor(ifelse(
  (trimws(as.character(dt2$MicrocystisRank)) == trimws("NA")),
  NA,
  as.character(dt2$MicrocystisRank)
))
dt2$MethodCode <- as.factor(ifelse(
  (trimws(as.character(dt2$MethodCode)) == trimws("NA")),
  NA,
  as.character(dt2$MethodCode)
))
dt2$GearCode <- as.factor(ifelse(
  (trimws(as.character(dt2$GearCode)) == trimws("NA")),
  NA,
  as.character(dt2$GearCode)
))
dt2$GearConditionCode <- as.factor(ifelse(
  (trimws(as.character(dt2$GearConditionCode)) == trimws("NA")),
  NA,
  as.character(dt2$GearConditionCode)
))
dt2$SampleAltered <- as.factor(ifelse(
  (trimws(as.character(dt2$SampleAltered)) == trimws("NA")),
  NA,
  as.character(dt2$SampleAltered)
))
dt2$FieldComments <- as.factor(ifelse(
  (trimws(as.character(dt2$FieldComments)) == trimws("NA")),
  NA,
  as.character(dt2$FieldComments)
))


inUrl7 <- "https://pasta.lternet.edu/package/data/eml/edi/233/5/89146f1382d7dfa3bbf3e4b1554eb5cc"
infile7 <- tempfile()
try(download.file(
  inUrl7,
  infile7,
  method = "curl",
  extra = paste0(' -A "', getOption("HTTPUserAgent"), '"')
))
if (is.na(file.size(infile7))) {
  download.file(inUrl7, infile7, method = "auto")
}


dt7 <- read.csv(
  infile7,
  header = F,
  skip = 1,
  sep = ",",
  quot = '"',
  col.names = c(
    "StationCode",
    "StationName",
    "StationNumber",
    "Latitude",
    "Longitude",
    "PeriodOfRecordFrom",
    "PeriodOfRecordTo",
    "MonitoringType",
    "MethodCode"
  ),
  check.names = TRUE
)

unlink(infile7)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt7$StationCode) != "factor") {
  dt7$StationCode <- as.factor(dt7$StationCode)
}
if (class(dt7$StationName) != "factor") {
  dt7$StationName <- as.factor(dt7$StationName)
}
if (class(dt7$StationNumber) != "factor") {
  dt7$StationNumber <- as.factor(dt7$StationNumber)
}
if (class(dt7$Latitude) == "factor") {
  dt7$Latitude <- as.numeric(levels(dt7$Latitude))[as.integer(dt7$Latitude)]
}
if (class(dt7$Latitude) == "character") {
  dt7$Latitude <- as.numeric(dt7$Latitude)
}
if (class(dt7$Longitude) == "factor") {
  dt7$Longitude <- as.numeric(levels(dt7$Longitude))[as.integer(dt7$Longitude)]
}
if (class(dt7$Longitude) == "character") {
  dt7$Longitude <- as.numeric(dt7$Longitude)
}
if (class(dt7$PeriodOfRecordFrom) != "factor") {
  dt7$PeriodOfRecordFrom <- as.factor(dt7$PeriodOfRecordFrom)
}
if (class(dt7$PeriodOfRecordTo) != "factor") {
  dt7$PeriodOfRecordTo <- as.factor(dt7$PeriodOfRecordTo)
}
if (class(dt7$MonitoringType) != "factor") {
  dt7$MonitoringType <- as.factor(dt7$MonitoringType)
}
if (class(dt7$MethodCode) != "factor") {
  dt7$MethodCode <- as.factor(dt7$MethodCode)
}


inUrl8 <- "https://pasta.lternet.edu/package/data/eml/edi/233/5/405122cb55c6996661c0dee20ab77a6c"
infile8 <- tempfile()
try(download.file(
  inUrl8,
  infile8,
  method = "curl",
  extra = paste0(' -A "', getOption("HTTPUserAgent"), '"')
))
if (is.na(file.size(infile8))) {
  download.file(inUrl8, infile8, method = "auto")
}


dt8 <- read.csv(
  infile8,
  header = F,
  skip = 1,
  sep = ",",
  quot = '"',
  col.names = c(
    "OrganismCode",
    "IEPFishCode",
    "CommonName",
    "Native",
    "Phylum",
    "Class",
    "Order",
    "Family",
    "Genus",
    "Species",
    "Taxa"
  ),
  check.names = TRUE
)

unlink(infile8)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt8$OrganismCode) != "factor") {
  dt8$OrganismCode <- as.factor(dt8$OrganismCode)
}
if (class(dt8$IEPFishCode) != "factor") {
  dt8$IEPFishCode <- as.factor(dt8$IEPFishCode)
}
if (class(dt8$CommonName) != "factor") {
  dt8$CommonName <- as.factor(dt8$CommonName)
}
if (class(dt8$Native) != "factor") {
  dt8$Native <- as.factor(dt8$Native)
}
if (class(dt8$Phylum) != "factor") {
  dt8$Phylum <- as.factor(dt8$Phylum)
}
if (class(dt8$Class) != "factor") {
  dt8$Class <- as.factor(dt8$Class)
}
if (class(dt8$Order) != "factor") {
  dt8$Order <- as.factor(dt8$Order)
}
if (class(dt8$Family) != "factor") {
  dt8$Family <- as.factor(dt8$Family)
}
if (class(dt8$Genus) != "factor") {
  dt8$Genus <- as.factor(dt8$Genus)
}
if (class(dt8$Species) != "factor") {
  dt8$Species <- as.factor(dt8$Species)
}
# if (class(dt8$Taxa)!="factor") dt8$Taxa<- as.factor(dt8$Taxa)

# Convert Missing Values to NA for non-dates

dt8$Native <- as.factor(ifelse(
  (trimws(as.character(dt8$Native)) == trimws("NA")),
  NA,
  as.character(dt8$Native)
))
dt8$Phylum <- as.factor(ifelse(
  (trimws(as.character(dt8$Phylum)) == trimws("NA")),
  NA,
  as.character(dt8$Phylum)
))
dt8$Class <- as.factor(ifelse(
  (trimws(as.character(dt8$Class)) == trimws("NA")),
  NA,
  as.character(dt8$Class)
))
dt8$Order <- as.factor(ifelse(
  (trimws(as.character(dt8$Order)) == trimws("NA")),
  NA,
  as.character(dt8$Order)
))
dt8$Family <- as.factor(ifelse(
  (trimws(as.character(dt8$Family)) == trimws("NA")),
  NA,
  as.character(dt8$Family)
))
dt8$Genus <- as.factor(ifelse(
  (trimws(as.character(dt8$Genus)) == trimws("NA")),
  NA,
  as.character(dt8$Genus)
))
dt8$Species <- as.factor(ifelse(
  (trimws(as.character(dt8$Species)) == trimws("NA")),
  NA,
  as.character(dt8$Species)
))


inUrl9 <- "https://pasta.lternet.edu/package/data/eml/edi/233/5/b2b92d9dbfb78cfb1a5716174dfceab1"
infile9 <- tempfile()
try(download.file(
  inUrl9,
  infile9,
  method = "curl",
  extra = paste0(' -A "', getOption("HTTPUserAgent"), '"')
))
if (is.na(file.size(infile9))) {
  download.file(inUrl9, infile9, method = "auto")
}


dt9 <- read.csv(
  infile9,
  header = F,
  skip = 1,
  sep = ",",
  quot = '"',
  col.names = c(
    "SampleID",
    "EventID",
    "OrganismCode",
    "Count"
  ),
  check.names = TRUE
)

unlink(infile9)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt9$SampleID) != "factor") {
  dt9$SampleID <- as.factor(dt9$SampleID)
}
if (class(dt9$EventID) != "factor") {
  dt9$EventID <- as.factor(dt9$EventID)
}
if (class(dt9$OrganismCode) != "factor") {
  dt9$OrganismCode <- as.factor(dt9$OrganismCode)
}
if (class(dt9$Count) == "factor") {
  dt9$Count <- as.numeric(levels(dt9$Count))[as.integer(dt9$Count)]
}
if (class(dt9$Count) == "character") {
  dt9$Count <- as.numeric(dt9$Count)
}


if (!dir.exists("data")) {
  dir.create("data")
}

dt2 |>
  dplyr::select(SampleID = EventID, Station = StationCode, Date = SampleDate) |>
  dplyr::left_join(dplyr::select(
    dt7,
    Station = StationCode,
    Latitude,
    Longitude
  )) |>
  dplyr::mutate(
    Source = "YBFMP",
    Year = lubridate::year(Date),
    # https://github.com/EnvironmentalScienceAssociates/esaRmisc
    WaterYear = esaRmisc::water_year(Date),
    Month = lubridate::month(Date),
    DOY = lubridate::yday(Date),
    DOWY = esaRmisc::wy_yday(Date),
    SourceStation = paste(Source, Station),
    LatRound = round(Latitude, 1),
    LonRound = round(Longitude, 1)
  ) |>
  saveRDS(file.path("data", "Samples-YBFMP.rds"))

dt9 |>
  dplyr::left_join(dplyr::select(dt8, OrganismCode, Taxa)) |>
  dplyr::mutate(
    Taxa = ifelse(Taxa == "Lampetra ayresi", "Lampetra ayresii", Taxa),
    Taxa = sub(" spp.", "", Taxa)
  ) |>
  dplyr::select(SampleID = EventID, Taxa, Count) |>
  # for now, the app is focused on counts of present species
  # it reduces the size of the dataset to drop the zero counts
  dplyr::filter(Count > 0) |>
  saveRDS(file.path("data", "Counts-YBFMP.rds"))

if (Sys.getenv("EgnyteKey") != "") {
  remote_path = file.path(
    "Shared",
    "Admin",
    "Practices",
    "Fish and Aquatic Science",
    "Data Science",
    "EDI-SFE-Data"
  )

  for (i in list.files("data", "-YBFMP")) {
    Sys.sleep(0.4)
    # install egnyter with remotes::install_github("thinkelman-esa/egnyter")
    egnyter::upload_file(
      file.path("data", i),
      file.path(remote_path, i),
      domain = "https://oneesa.egnyte.com",
      token = Sys.getenv("EgnyteKey")
    )
  }
}
