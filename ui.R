
page_sidebar(
  title = "EDI SFE Fish Abundance",
  sidebar = sidebar(
    width = 300,
    conditionalPanel(
      condition = 'input.nav == "Map"',
      sliderInput(inputId = "years", label = "Years", min = yr_min, max = yr_max, 
                  value = c(yr_min, yr_max), sep = "", step = 1),
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
      uiOutput("dateRange"),
      downloadButton("download", "Download Table", icon = icon("download"))
    )

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
      DT::DTOutput("table")
    )
  )
)

