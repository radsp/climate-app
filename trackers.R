
tab_trackers <-
  tabPanel(
    "Trackers",
    useShinyjs(),
    includeCSS("www/style.css"),
    tags$head(HTML("<script type='text/javascript' src='keep-alive.js'></script>")),

    # Tabs

    tags$head(tags$script('var dimension = [0, 0];$(document).on("shiny:connected", function(e) {
                                      dimension[0] = window.innerWidth; dimension[1] = window.innerHeight; Shiny.onInputChange("dimension", dimension);});
                                      $(window).resize(function(e) {dimension[0] = window.innerWidth; dimension[1] = window.innerHeight;
                                      Shiny.onInputChange("dimension", dimension); });')),
    fluidPage(
      fluidRow(
        wellPanel(
          style = "padding-bottom: 4px; padding-top:4px;font-size:12px;",
          fluidRow(
            column(width = 6, HTML("<h5>FILTER</h5>")),
            column(
              width = 6, align = "right",
              actionButton(
                inputId = "goplot_track", label = "Update plots",
                class = "btn-primary", style = "margin-top:10px;"
              )
            )
          ),
          fluidRow(
            style = "margin-top:4px; margin-bottom:0px;padding-bottom:0px; padding-top:0px;margin-bottom:0px;",
            column(
              width = 4, # style = "margin-top:-5px;",
              selectizeInput(
                inputId = "ev_track", label = HTML("Climate Variable"),
                choices = c(
                  "Rainfall" = "rf", "Rainfall (cumulative)" = "rfacc", "Air Temperature" = "tair",
                  "Land Surface Temperature" = "lstd",
                  "Vegetation Index" = "ndvi", "Specific Humidity" = "sh", "Soil Moisture" = "sm"
                ),
                selected = "rf"
              )
            ),
            # column(6, actionButton(inputId = "goplot_track", label = "Update plots", class = "btn-primary",
            #                        style = "margin-top:18px;"))

            column(
              width = 4,
              selectizeInput(
                inputId = "epi_track", label = HTML("Overlay Variable"),
                choices = c(
                  "None" = "none", "Suspected Cases" = "suspected_cases",
                  "Suspected Cases per 100K" = "suspected_cases_rate",
                  "Tested Cases" = "tested_cases", "Tested Cases per 100K" = "tested_cases_rate",
                  "Confirmed Cases" = "confirmed_cases", "Confirmed Cases per 100K" = "confirmed_cases_rate",
                  "Test Positivity Rate (TPR)" = "tpr",
                  "Severe Cases" = "severe_cases", "Severe Cases per 100K" = "severe_cases_rate",
                  "Malaria Deaths" = "malaria_deaths", "Malaria Deaths per 10K" = "malaria_deaths_rate"
                ),
                selected = "none"
              ),
              shinyBS::bsTooltip(
                id = "epi_track",
                title = "Access Restricted by Country",
                placement = "top"
              )
            ),
            column(
              width = 4,
              selectizeInput(
                inputId = "gres_track", label = HTML("Geographical Aggregation"),
                choices = c("National (Country)" = "admin0", "Admin. Level 1" = "admin1", "Admin. Level 2" = "admin2"),
                selected = "admin0"
              )
            )
          ),
          fluidRow(
            style = "margin-top:0px; margin-bottom:1px;padding-bottom:1px;padding-top:0px;",
            column(
              width = 4, style = "margin-top:-14px;",
              div(
                class = "smallbox",
                pickerInput(
                  inputId = "adm0_track", label = HTML("Country"),
                  choices = adm0_choice, selected = as.vector(adm0_choice),
                  options = list(`actions-box` = TRUE, `selectedTextFormat` = "count > 3"), multiple = TRUE
                )
              )
            ),
            column(
              width = 4, style = "margin-top:-10px;",
              div(
                class = "smallbox",
                pickerInput(
                  inputId = "adm1_track", label = HTML("Admin. Level 1"),
                  choices = "None", selected = "None", options = list(`actions-box` = TRUE), multiple = TRUE
                )
              )
            ),
            column(
              width = 4, style = "margin-top:-10px;",
              div(
                class = "smallbox",
                pickerInput(
                  inputId = "adm2_track", label = HTML("Admin. Level 2"),
                  choices = "None", selected = "None", options = list(`actions-box` = TRUE), multiple = TRUE
                )
              )
            )
          )
        )
      ),
      fluidRow(
        # fluidPage(

        tabsetPanel(
          tabPanel(
            "Seasonality",
            br(),
            HTML("<H4> Seasonality Tracker </H4>"),
            # HTML("text xtext txt"),
            # textOutput(outputId = "test_out"),
            # br(),
            br(),
            fluidRow(
              column(
                width = 3,
                div(
                  class = "smallbox",
                  pickerInput(
                    inputId = "year_ssn", label = HTML("<b>Year</b> (select up to 3)"),
                    choices = 2017:2022, selected = c(2022, 2021),
                    options = list(`max-options` = 3), multiple = T
                  )
                )
              ),
              column(
                width = 3,
                div(
                  class = "smallbox",
                  radioGroupButtons(
                    inputId = "yax_ssn", label = HTML("<b>Y-Axis Range</b>"), choices = c("Same", "Free"),
                    justified = F, checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                  )
                )
              ),
              column(
                width = 6,
                div(
                  class = "smallbox",
                  radioGroupButtons(
                    inputId = "sort_ssn", label = HTML("<b>Sort Plots</b>"), choices = c("Alphabetically", "By Latitude"),
                    justified = F, checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                  )
                )
              )
            ),
            fluidRow(
              style = "margin-top:10px; margin-bottom:15px;",
              column(
                width = 12,
                htmlOutput(outputId = "legend_ssn")
              )
            ),
            withSpinner(plotlyOutput("myplot1_track"))
          ),
          tabPanel(
            "Historical",
            HTML("<H4> Historical Tracker </H4>"),
            # HTML("text xtext txt"),
            # textOutput(outputId = "test_out2"),
            # br(),
            br(),
            fluidRow(
              column(
                width = 4,
                airDatepickerInput(
                  inputId = "date_histo", label = HTML("<b>Date Range</b>"), range = TRUE,
                  view = "months", minView = "months", clearButton = T, update_on = "close",
                  dateFormat = "MMM yyyy", monthsField = "monthsShort", separator = " to ",
                  minDate = as.Date("2017-01-01"), maxDate = date_now, value = c(as.Date("2017-01-01"), date_now)
                )
              ),
              column(
                width = 3,
                radioGroupButtons(
                  inputId = "yax_histo", label = HTML("<b>Y-Axis Range</b>"), choices = c("Same", "Free"),
                  justified = F, checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                )
              ),
              column(
                width = 5,
                radioGroupButtons(
                  inputId = "sort_histo", label = HTML("<b>Sort Plots</b>"), choices = c("Alphabetically", "By Latitude"),
                  justified = F, checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                )
              )
            ),
            fluidRow(
              style = "margin-top:10px; margin-bottom:15px;",
              column(
                width = 12,
                htmlOutput(outputId = "legend_histo")
              )
            ),
            withSpinner(plotlyOutput("myplot2_track", width = "100%"))
          )
        )



        # )
      )
    )
  )







# tab_trackers <- tabPanel ("Seasonal and Historical Trackers",
#
#   # Filter panel expanded ----------------------------------------------
#
#   fluidPage(
#
#     fluidRow(
#         absolutePanel(
#           id = "filter_track", height = "10%", width = "100%", left = "15px",
#           style = "z-index: 100; background-color:#EEEEEE; padding: 15px;",
#
#           fluidRow(
#             column (width = 2,
#                     selectizeInput(
#                       inputId = "ev_track", label = HTML("<b>Climate Variable</b>"),
#                       choices = c("Rainfall" = "rf", "Rainfall (cumulative)" = "rfacc", "Air Temperature" = "tair",
#                                   "Land Surface Temperature" = "lstd",
#                                   "Vegetation Index" = "ndvi", "Specific Humidity" = "sh", "Soil Moisture" = "sm"),
#                       selected = "rf"
#                     )
#             ),
#
#             column (width = 2,
#                     selectizeInput (
#                       inputId = "gres_track", label = HTML("<b>Geographical Aggregation</b>"),
#                       choices = c("National (Country)" = "admin0", "Admin. Level 1" = "admin1", "Admin. Level 2" = "admin2"),
#                       selected = "admin0"
#                     )
#             ),
#
#             column (width = 3,
#                     pickerInput(
#                       inputId = "adm0_track", label = HTML("<b>Country</b>"),
#                       choices = adm0_choice, selected = as.vector(adm0_choice),
#                       options = list(`actions-box` = TRUE, `selectedTextFormat` = 'count > 3'), multiple = TRUE
#                     )
#             ),
#
#             column(width = 4, uiOutput("adm1_track")),
#             column(width = 4, uiOutput("adm2_track"))
#
#           ),
#
#           fluidRow(
#             actionButton(inputId = "goplot_track", label = "Update plots", class = "btn-primary")
#           )
#
#         )
#
#     ),
#
#
#
#     # Main Panel ----------------------------------------------
#
#     fluidRow(
#
#       ## > Plot panel--------------------------------------------
#
#       div (
#         # fluidPage (
#           column (id = "plot_panel_track",
#                   width = 8, offset = 2,
#                   HTML("<H4> Seasonality Tracker </H4>"),
#                   HTML("<p> The following plots show how climate variables vary between season ..... [more explanation +
#                what questions they answer] </p>"),
#                   br(), br(),
#                   textOutput(outputId = "test_out"),
#                   br(),
#                   fluidRow(
#                     shinydashboard::box(
#                       style = 'width: 1200px; height: 500px; overflow-y: scroll;',
#                       plotlyOutput("myplot1_track", height = "500px")
#                     )
#                   ),
#                   br(), br(),
#                   HTML("<H4> Historical Tracker </H4>"),
#                   HTML("<p> The following plots show how climate variables vary historically ..... [more explanation +
#                what questions they answer] </p>"),
#                   br(), br(),
#                   fluidRow(
#                     div(
#                       style = 'width: 1000px; overflow-x: scroll; height: 200px; overflow-y: scroll',
#                       plotOutput("myplot2_track", height = "500px")
#                     )
#
#                   )
#           )
#         )
#       )
#       # end of plot panel & main panel
#     )
#   # )
#
# )
