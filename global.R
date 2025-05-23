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

if (!dir.exists("data")) dir.create("data")

sources = c("20mm", "Bay Study", "DJFMP", "EDSM", "FMWT", "Salvage", 
            "SKT", "SLS", "STN", "Suisun", "YBFMP")
source_colors = c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462",
                  "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd", "#ccebc5")
pal = colorFactor(source_colors, sources)
# sources_sel = c("20mm", "Bay Study")

sample_files = c("Samples-SFE.rds", "Samples-YBFMP.rds")
count_files = c("Counts-YBFMP.rds", 
                paste0("Counts-SFE-", gsub(" ", "", sources[sources != "YBFMP"]), ".rds"))
data_files = c(sample_files, count_files)

if (!all(file.exists(file.path("data", data_files)))){
  if (Sys.getenv("EgnyteKey") == ""){
    stop("Need to run prep-data.R to get data used in the Shiny app.")
  } else {
    remote_path = file.path("Shared", "Admin", "Practices", "Fish and Aquatic Science",
                            "Data Science", "EDI-SFE-Data")
    for (i in data_files){
      if (!file.exists(file.path("data", i))){
        Sys.sleep(0.4)
        # install egnyter with remotes::install_github("thinkelman-esa/egnyter")
        egnyter::download_file(file.path(remote_path, i),
                               file.path("data", i),
                               domain = "https://oneesa.egnyte.com",
                               token = Sys.getenv("EgnyteKey"))
      }
    }
  }
}

samples = lapply(sample_files, function(x) readRDS(file.path("data", x))) |> 
  bind_rows()

yr_min = min(samples$Year, na.rm = TRUE)
yr_max = max(samples$Year, na.rm = TRUE)

lat_min = min(samples$Latitude, na.rm = TRUE)
lat_max = max(samples$Latitude, na.rm = TRUE)
lon_min = min(samples$Longitude, na.rm = TRUE)
lon_max = max(samples$Longitude, na.rm = TRUE)

boundary_mat = matrix(c(lon_min, lon_min, lon_max, lon_max, lon_min,
                        lat_min, lat_max, lat_max, lat_min, lat_min), 
                      ncol = 2)
colnames(boundary_mat) = c("lon", "lat")

boundary = st_sfc(st_polygon(list(boundary_mat)), crs = 4326)

