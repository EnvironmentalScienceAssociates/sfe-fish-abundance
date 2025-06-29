
function(input, output, session) {
  
  rv <- reactiveValues(last_sources = sources, shape = NULL, summ = NULL,
                       # initialize counts with empty list
                       counts = setNames(vector("list", length(sources)), sources))
  
  
  yrMin <- reactive({
    yrs_range[[input$year_type]][["Min"]]
  })
  
  yrMax <- reactive({
    yrs_range[[input$year_type]][["Max"]]
  })
  
  observe({
    lbl = if (input$year_type == "Water") "Water Years" else "Years"
    updateSliderInput(session, "years", label = lbl, 
                      min = yrMin(), max = yrMax(), 
                      value = c(yrMin(), yrMax()))
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
      select(Source, Station, SourceStation, LatRound, LonRound, 
             Latitude, Longitude, FillColor) |> 
      distinct() |> 
      filter(!(is.na(Latitude) | is.na(Longitude)))
  })
  
  stationPoints <- reactive({
    st_as_sf(stations(), coords = c("Longitude", "Latitude"), crs = 4326)
  })
  
  sourcePoints <- reactive({
    stations() |>
      group_by(Source, LatRound, LonRound, FillColor) |>
      summarise(N = n(),
                Latitude = mean(Latitude, na.rm = TRUE),
                Longitude = mean(Longitude, na.rm = TRUE))  |> 
      mutate(Label = paste("N =", N)) |>
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
        leafgl::addGlPoints(data = stationPoints(), 
                            label = stationPoints()$Station,
                            radius = 10,
                            fillColor = stationPoints()$FillColor,
                            fillOpacity = 1,
                            group = "stations") |> 
        groupOptions("stations", zoomLevels = 11:20) |> 
        leafgl::addGlPoints(data = sourcePoints(), 
                            label = sourcePoints()$Label,
                            radius = 10,
                            fillColor = sourcePoints()$FillColor,
                            fillOpacity = 1,
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
    req(rv$shape, input$year_type)
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
  
  output$taxaFilters <- renderUI({
    req("Taxa" %in% colnames(rv$summ), rv$summ)
    tagList(
      checkboxGroupInput("taxa_filters", "Filter Taxa List",
                         choices = taxa_filters, selected = taxa_filters),
      input_switch("use_common", "Use Common Name")
    )
  })
  
  taxaSub <- reactive({
    req("Taxa" %in% colnames(rv$summ), rv$summ, input$taxa_filters)
    
    summ_taxa = distinct(select(ungroup(rv$summ), Taxa))
    
    taxa_special = NULL
    tf = input$taxa_filters[input$taxa_filters != "others"]
    if (length(tf) > 0) taxa_special = unique(unlist(taxa_list[tf], use.names = FALSE))
    
    to = NULL
    if ("others" %in% input$taxa_filters) to = taxa_others
    
    tmp = summ_taxa |> 
      filter(Taxa %in% c(taxa_special, to)) |> 
      left_join(taxa_df, by = "Taxa") |> 
      arrange(Taxa)
  })
  
  output$taxa <- renderUI({
    req("Taxa" %in% colnames(rv$summ), rv$summ, taxaSub())
    
    dfx = taxaSub()
    
    if (input$use_common) dfx = arrange(dfx, CommonName)
    taxa = dfx$Taxa
    if (input$use_common) taxa = setNames(taxa, dfx$CommonName)
    
    pickerInput(inputId = "taxa", label = "Taxa", multiple = TRUE,
                choices = taxa, selected = taxa,
                options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 8,
                               `selected-text-format` = "count > 1"))
  })
  
  output$months <- renderUI({
    req("Month" %in% colnames(rv$summ), rv$summ)
    mnths = sort(unique(rv$summ$Month))
    pickerInput(inputId = "months", label = "Month", multiple = TRUE, 
                choices = mnths, selected = mnths,
                options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 6,
                               `selected-text-format` = "count > 7"))
  })
  
  output$doy <- renderUI({
    req(any(c("DOY", "DOWY") %in% colnames(rv$summ)), rv$summ)
    if (input$year_type == "Water"){
      lbl = "Day of Water Year"
      mn = min(rv$summ$DOWY, na.rm = TRUE)
      mx = max(rv$summ$DOWY, na.rm = TRUE)
    } else {
      lbl = "Day of Year"
      mn = min(rv$summ$DOY, na.rm = TRUE)
      mx = max(rv$summ$DOY, na.rm = TRUE)
    }
    sliderInput("doy", label = lbl, min = mn, max = mx, value = c(mn, mx), 
                step = 1, ticks = FALSE)
  })
  
  table <- reactive({
    req(rv$summ)
    out = rv$summ
    if ("Taxa" %in% colnames(rv$summ)){
      req(input$taxa, length(input$taxa_filters) > 0)
      out = out[out[["Taxa"]] %in% input$taxa, ]
      out = out |> 
        left_join(taxa_df, by = "Taxa") |> 
        relocate(CommonName, .after = Taxa)
    }
    if ("Month" %in% colnames(rv$summ)){
      req(input$months)
      out = out[out[["Month"]] %in% input$months, ]
    }
    if ("DOY" %in% colnames(rv$summ)){
      req(input$doy)
      out = out[out[["DOY"]] >= input$doy[1] &
                  out[["DOY"]] <= input$doy[2], ]
    }
    if ("DOWY" %in% colnames(rv$summ)){
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
      colDef(minWidth = chars*10 + 40)
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
  
  output$helpText <- renderUI({
    msg = ""
    
    if (input$nav == "Map"){
      msg = paste0("The primary use-case for this app is as a first step for determining species 
      presence at specific locations based on ongoing Bay-Delta monitoring surveys (", yrMin(), " - ", yrMax(), "). <br><br>
      
      The Surveys dropdown menu includes only the surveys with data for the selected year range. 
      At the default zoom level, the map shows aggregated survey station locations. Zoom in to 
      see actual station locations for the selected surveys. <br><br>
      
      Use the map drawing tools on the left side of the map to select the area of interest. 
      Only data from the stations inside the drawn polygon will be included in the summary table.<br><br>
      
      After drawing a polygon, select the parameters to include in the summary table from the 
      'Group By' dropdown menu and click on 'Tally Fish Abundance'.")
    }
    
    if (input$nav == "Table" & is.null(rv$summ)){
      msg = "First select data on the Map tab with the drawing tools and then click on the 
      'Tally Fish Abundance' button to see the summary table."
    }
    
    if (input$nav == "Table" & !is.null(rv$summ)){
      msg = "Click on a column heading to sort the table by that column. Filter the table 
      with the dropdown menu(s) in the sidebar. Click the 'Download Table' button to download a 
      CSV file with the filtered data."
    }
    
    HTML(msg)
  })
  
}
