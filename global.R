options(dplyr.summarise.inform = FALSE)
library(shiny)
library(bslib)
library(shinyWidgets)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(lubridate)
library(reactable)

taxa_df = read.csv(file.path("data", "all_taxa.csv")) |>
  mutate(
    fed_status = ifelse(fed_status == "", NA, fed_status),
    state_status = ifelse(state_status %in% c("", " "), NA, state_status)
  ) |>
  rename(
    CommonName = common_name,
    NativeSpecies = native,
    FederalStatus = fed_status,
    StateStatus = state_status
  )

taxa_list = list(
  "native" = taxa_df$Taxa[
    !is.na(taxa_df$NativeSpecies) & taxa_df$NativeSpecies
  ],
  "fed" = taxa_df$Taxa[!is.na(taxa_df$FederalStatus)],
  "state" = taxa_df$Taxa[!is.na(taxa_df$StateStatus)]
)

taxa_others = taxa_df$Taxa[
  !(taxa_df$Taxa %in% unique(unlist(taxa_list, use.names = FALSE)))
]

taxa_filters = c(
  "Native Species" = "native",
  "Federal Special Status" = "fed",
  "State Special Status" = "state",
  "All Others" = "others"
)

sources = c(
  "20mm",
  "Bay Study",
  "DJFMP",
  "EDSM",
  "FMWT",
  "Salvage",
  "SKT",
  "SLS",
  "STN",
  "Suisun",
  "YBFMP"
)
source_colors = c(
  "#8dd3c7",
  "#ffffb3",
  "#bebada",
  "#fb8072",
  "#80b1d3",
  "#fdb462",
  "#b3de69",
  "#fccde5",
  "#d9d9d9",
  "#bc80bd",
  "#ccebc5"
)
pal = colorFactor(source_colors, sources)

sample_files = c("Samples-SFE.rds", "Samples-YBFMP.rds")
count_files = c(
  "Counts-YBFMP.rds",
  paste0("Counts-SFE-", gsub(" ", "", sources[sources != "YBFMP"]), ".rds")
)
data_files = c(sample_files, count_files)

if (!all(file.exists(file.path("data", data_files)))) {
  if (Sys.getenv("EgnyteKey") == "") {
    stop("Need to run prep-data.R to get data used in the Shiny app.")
  } else {
    remote_path = file.path(
      "Shared",
      "Admin",
      "Practices",
      "Fish and Aquatic Science",
      "Data Science",
      "EDI-SFE-Data"
    )
    for (i in data_files) {
      if (!file.exists(file.path("data", i))) {
        Sys.sleep(0.4)
        # install egnyter with remotes::install_github("thinkelman-esa/egnyter")
        egnyter::download_file(
          file.path(remote_path, i),
          file.path("data", i),
          domain = "https://oneesa.egnyte.com",
          token = Sys.getenv("EgnyteKey")
        )
      }
    }
  }
}

samples = lapply(sample_files, function(x) readRDS(file.path("data", x))) |>
  bind_rows() |>
  mutate(FillColor = pal(Source))

yrs_range = list(
  "Water" = c(
    "Min" = min(samples$WaterYear, na.rm = TRUE),
    "Max" = max(samples$WaterYear, na.rm = TRUE)
  ),
  "Calendar" = c(
    "Min" = min(samples$Year, na.rm = TRUE),
    "Max" = max(samples$Year, na.rm = TRUE)
  )
)

lat_min = min(samples$Latitude, na.rm = TRUE)
lat_max = max(samples$Latitude, na.rm = TRUE)
lon_min = min(samples$Longitude, na.rm = TRUE)
lon_max = max(samples$Longitude, na.rm = TRUE)

boundary_mat = matrix(
  c(
    lon_min,
    lon_min,
    lon_max,
    lon_max,
    lon_min,
    lat_min,
    lat_max,
    lat_max,
    lat_min,
    lat_min
  ),
  ncol = 2
)
colnames(boundary_mat) = c("lon", "lat")

boundary = st_sfc(st_polygon(list(boundary_mat)), crs = 4326)
