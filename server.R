
function(input, output, session) {
  
  rv <- reactiveValues(last_sources = sources_sel, shape = NULL, summ = NULL,
                       # initialize counts with empty list
                       counts = setNames(vector("list", length(sources)), sources))
  
  observe({
    lbl = if (input$year_type == "Water") "Water Years" else "Years"
    updateSliderInput(session, "years", label = lbl, 
                      min = yrs_range[[input$year_type]][["Min"]], 
                      max = yrs_range[[input$year_type]][["Max"]], 
                      value = c(yrs_range[[input$year_type]][["Min"]], 
                                yrs_range[[input$year_type]][["Max"]]))
  })
  
  samplesSubYear <- reactive({
    samples[samples[["Year"]] >= input$years[1] & samples[["Year"]] <= input$years[2],]
  })
  
  observe({
    req(nrow(samplesSubYear()) > 0)
    
    samples_sub = samplesSubYear()
    opts = sort(unique(samples_sub$Source))
    
    if (!setequal(input$sources, rv$last_sources)) rv$last_sources = input$sources
    overlap = rv$last_sources[rv$last_sources %in% opts]
    sel = if (is.null(input$sources)) NULL else if (length(overlap) > 0) overlap else opts
    
    updatePickerInput(session, "sources", choices = opts, selected = sel)
  })
  
  samplesSubSource <- reactive({
    samples_sub = samplesSubYear()
    samples_sub[samples_sub[["Source"]] %in% input$sources,]
  })
  
  stations <- reactive({
    # not exactly stations b/c same station label can have many points
    req(nrow(samplesSubSource()) > 0)
    samplesSubSource() |> 
      select(Source, Station, SourceStation, LatRound, LonRound, Latitude, Longitude) |> 
      distinct() |> 
      filter(!(is.na(Latitude) | is.na(Longitude)))
  })
  
  stationPoints <- reactive({
    st_as_sf(stations(), coords = c("Longitude", "Latitude"), crs = 4326)
  })
  
  sourcePoints <- reactive({
    stations() |>
      group_by(Source, LatRound, LonRound) |>
      summarise(N = n(),
                Latitude = mean(Latitude, na.rm = TRUE),
                Longitude = mean(Longitude, na.rm = TRUE)) |>
      filter(!(is.na(Latitude) | is.na(Longitude))) |>
      st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  })
  
  output$map = renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE)) |>
      setView(lng = -121.75, lat = 38.36, zoom = 8) |>
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") |> 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |> 
      addLayersControl(baseGroups = c("Topo", "Satellite"),
                       options = layersControlOptions(collapsed = FALSE)) |> 
      addDrawToolbar(
        targetGroup = "draw",
        singleFeature = TRUE,
        polylineOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) |> 
      addPolygons(data = boundary, 
                  weight = 3,
                  fillOpacity = 0)
  })
  
  proxy <- leafletProxy("map") 
  
  observe({
    proxy |> 
      clearControls() |> 
      addLegend("bottomright", pal = pal, values = input$sources, 
                title = "Data Source", opacity = 1)
  })
  
  observe({
    proxy |> 
      clearGroup("sources") |> 
      clearGroup("stations") |> 
      clearControls()
    
    if (nrow(sourcePoints()) > 0){
      proxy |> 
        addCircleMarkers(data = stationPoints(), 
                         label = ~Station, 
                         radius = 4,
                         color = "black",
                         weight = 1,
                         opacity = 1,
                         fillColor = ~pal(Source),
                         fillOpacity = 0.8,
                         group = "stations") |> 
        groupOptions("stations", zoomLevels = 11:20) |> 
        addCircleMarkers(data = sourcePoints(), 
                         label = ~ paste("N =", N),
                         radius = 6,
                         color = "black",
                         weight = 1,
                         opacity = 1,
                         fillColor = ~pal(Source),
                         fillOpacity = 0.8,
                         group = "sources") |> 
        groupOptions("sources", zoomLevels = 7:10)  |> 
        addLegend("bottomright", pal = pal, values = unique(samplesSubSource()$Source), 
                  title = "Data Source", opacity = 1)
    }
  })
  
  observeEvent(input$map_draw_new_feature, {
    rv$shape = geojsonsf::geojson_sf(jsonify::to_json(input$map_draw_new_feature, unbox = TRUE))
  })
  
  observeEvent(input$map_draw_edited_features, {
    rv$shape = geojsonsf::geojson_sf(jsonify::to_json(input$map_draw_edited_features, unbox = TRUE))
  })
  
  observeEvent(input$map_draw_deleted_features, {
    rv$shape = NULL
  })
  
  samplesSubSpatial <- reactive({
    req(rv$shape)
    samples_sub = samplesSubSource()
    stations_selected = st_join(stationPoints(), rv$shape, join = st_within) |> 
      filter(!is.na(feature_type))
    samples_sub[samples_sub[["SourceStation"]] %in% stations_selected[["SourceStation"]],]
  })
  
  sourcesSpatial <- reactive({
    unique(samplesSubSpatial()$Source)
  })
  
  output$groupby <- renderUI({
    req(rv$shape)
    opts = c("Taxa", "Source", "Water Year" = "WaterYear", "Month", "Day of Water Year" = "DOWY")
    sel = c("Taxa", "Source", "Water Year" = "WaterYear")
    
    if (input$year_type == "Calendar"){
      opts = c("Taxa", "Source", "Year", "Month", "Day of Year" = "DOY")
      sel = c("Taxa", "Source", "Year")
    }
    
    pickerInput(inputId = "group_by", label = "Group By", multiple = TRUE, 
                choices = opts, selected = sel)
  })
  
  groupby <- reactive({
    req(rv$shape)
    gb = input$group_by
    # carry date forward if both year and day of year are selected
    if (all(c("Year", "DOY") %in% gb) | all(c("WaterYear", "DOWY") %in% gb)) gb = c(gb, "Date")
    gb
  })
  
  output$messageButton <- renderUI({
    if (is.null(rv$shape)){
      helpText("Use map drawing tools to select samples to include in abundance tally.")
    } else {
      validate(need(nrow(samplesSubSpatial()) > 0, "No data in selected area"))
      input_task_button("tally_fish", "Tally Fish Abundance")
    }
  })
  
  observeEvent(rv$shape, {
    withProgress(message = "Gathering data...", value = 0, {
      for (x in sourcesSpatial()){
        if (is.null(rv$counts[[x]])){
          incProgress(1/length(sourcesSpatial()), detail = x)
          fl = if (x == "YBFMP") "Counts-YBFMP.rds" else paste0("Counts-SFE-", gsub(" ", "", x), ".rds")
          rv$counts[[x]] = readRDS(file.path("data", fl))
        }
      }
    })
  })
  
  output$sourceMessage <- renderUI({
    req(rv$shape)
    p(paste("Sources in selected area:",
            paste(sourcesSpatial(), collapse = ", ")))
  })
  
  observeEvent(input$tally_fish,{
    req(rv$shape, nrow(samplesSubSpatial()) > 0)
    samples_sub = samplesSubSpatial()
    counts_sub = lapply(rv$counts, function(dfx){
      if (!is.null(dfx)){
        dfx |> 
          filter(SampleID %in% samples_sub$SampleID) |> 
          group_by(SampleID, Taxa) |> 
          summarise(Count = sum(Count, na.rm = TRUE))
      }
    }) |> 
      bind_rows()
    
    rv$summ = left_join(counts_sub, select(samples_sub, SampleID, Source, Year, WaterYear, 
                                        Month, DOY, DOWY, Date),
                        by = join_by(SampleID)) |> 
      group_by(across(all_of(groupby()))) |> 
      summarise(Count = sum(Count, na.rm = TRUE))
    
    updateTabsetPanel(session, "nav", selected = "Table")
  })
  
  output$taxa <- renderUI({
    req("Taxa" %in% input$group_by, rv$summ)
    taxa = sort(unique(rv$summ$Taxa))
    pickerInput(inputId = "taxa", label = "Taxa", multiple = TRUE,
                choices = taxa, selected = taxa,
                options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 10,
                               `selected-text-format` = "count > 1"))
  })
  
  output$months <- renderUI({
    req("Month" %in% input$group_by, rv$summ)
    pickerInput(inputId = "months", label = "Month", multiple = TRUE, 
                choices = 1:12, selected = 1:12,
                options = list(`actions-box` = TRUE, `live-search` = TRUE,
                               `selected-text-format` = "count > 7"))
  })
  
  output$doy <- renderUI({
    req(any(c("DOY", "DOWY") %in% input$group_by), rv$summ)
    if (input$year_type == "Water"){
      lbl = "Day of Water Year"
      mn = min(rv$summ$DOWY, na.rm = TRUE)
      mx = max(rv$summ$DOWY, na.rm = TRUE)
    } else {
      lbl = "Day of Year"
      mn = min(rv$summ$DOY, na.rm = TRUE)
      mx = max(rv$summ$DOY, na.rm = TRUE)
    }
    sliderInput("doy", label = lbl, min = mn, max = mx, value = c(mn, mx), step = 1)
  })
  
  table <- reactive({
    req(rv$summ)
    out = rv$summ
    if ("Taxa" %in% input$group_by){
      req(input$taxa)
      out = out[out[["Taxa"]] %in% input$taxa, ]
    }
    if ("Month" %in% input$group_by){
      req(input$months)
      out = out[out[["Month"]] %in% input$months, ]
    }
    if ("DOY" %in% input$group_by){
      req(input$doy)
      out = out[out[["DOY"]] >= input$doy[1] &
                  out[["DOY"]] <= input$doy[2], ]
    }
    if ("DOWY" %in% input$group_by){
      req(input$doy)
      out = out[out[["DOWY"]] >= input$doy[1] &
                  out[["DOWY"]] <= input$doy[2], ]
    }
    out
  })
  
  output$table <- renderReactable({
    req(rv$summ)
    dfx = mutate(table(), Count = round(Count))
    chars = sapply(colnames(dfx), function(x){
      max(nchar(c(x, as.character(unique(dfx[[x]])))), na.rm = TRUE)
    })
    
    col_widths <- function(chars){
      colDef(minWidth = chars*10 + 30)
    }
    
    reactable(dfx,
              highlight = TRUE,
              fullWidth = FALSE,
              defaultColDef = colDef(
                defaultSortOrder = "desc",
                headerStyle = list(background = "#f7f7f8")),
              columns = lapply(chars, col_widths),
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(15, 25, 50, 100),
              defaultPageSize = 15)
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("EDI-SFE-Fish-Abundance-", round(as.numeric(Sys.time())), ".csv")
    },
    content = function(file) {
      write.csv(table(), file, row.names = FALSE)
    }
  )
  
}
