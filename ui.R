
page_sidebar(
  title = "SFE Fish Abundance",
  sidebar = sidebar(
    width = 320,
    conditionalPanel(
      condition = 'input.nav == "Map"',
      radioButtons("year_type", "Year Type", choices = c("Water", "Calendar"), 
                   selected = "Water", inline = TRUE),
      sliderInput(inputId = "years", label = "Years", sep = "", step = 1, 
                  min = yrs_range[["Water"]][["Min"]], max = yrs_range[["Water"]][["Max"]], 
                  value = c(yrs_range[["Water"]][["Min"]], yrs_range[["Water"]][["Max"]])),
      pickerInput(inputId = "sources", label = "Sources", multiple = TRUE, 
                  choices = sources, selected = sources_sel,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 5,
                                 `selected-text-format` = "count > 3")),
      uiOutput("groupby"),
      uiOutput("messageButton")
    ),
    conditionalPanel(
      condition = 'input.nav == "Table"',
      uiOutput("taxa"),
      uiOutput("months"),
      uiOutput("doy"),
      downloadButton("download", "Download Table", icon = icon("download"))
    ),
    br(),
    br(),
    a(img(src="ESA-small.png", alt="ESA logo", width = "200"), 
      href = "https://esassoc.com/",
      target = "_blank"),
    helpText("For issues with this app, contact Travis Hinkelman (thinkelman@esassoc.com).")
  ),
  navset_card_underline(
    id = "nav",
    nav_panel(
      title = "Map",
      leafletOutput("map"),
      absolutePanel(bottom = 20, left = 20, uiOutput("sourceMessage"))
    ),
    nav_panel(
      title = "Table",
      reactableOutput("table")
    ),
    nav_menu(
      title = "Links",
      nav_item(HTML('<a href="https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1075&revision=2" target="_blank">SFE Data</a>')),
      nav_item(HTML('<a href="https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=233&revision=5" target="_blank">YBFMP Data</a>')),
      nav_item(HTML('<a href="https://github.com/EnvironmentalScienceAssociates/sfe-fish-abundance" target="_blank">Code</a>'))
    )
  )
)

