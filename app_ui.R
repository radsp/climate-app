# ui <- fluidPage(
#   
#   title = "CLIMATE",
#   theme = shinytheme("paper"),
#   useShinyjs(),
#   
#   tags$head(tags$style(HTML('#Sidebar {width: 1400px;}'))),
#   
#   tags$head(HTML("
#                 <script type='text/javascript' src='keep-alive.js'></script>")),
#   
#   navbarPage(HTML("<title>CLIMATE</title>"),
#             
#     tabPanel("Tool",
#       div(id = "Sidebar", 
#           sidebarPanel(
#             fluidRow(style = "margin-right:-30px;",
#               column(width = 10, HTML("<h3>Model Inputs and Parameters</h3>")),
#               column(width = 2, style = "margin-top: 15px;", 
#                      actionBttn("hide_sidebar", label = NULL, style = "material-circle", 
#                                 icon = icon("arrow-left"), 
#                                 size = "sm", color = "default"),
#                      bsTooltip("hide_sidebar", title = "Hide panel", 
#                                 placement = "top", trigger = "hover"))
#             ),
#            
#             br(),
#             hr(style = "border-top: 1px solid #9aa5a6;"),
#             HTML("<h4>Input Data</h4>"),
#             br(),
#             fluidRow(
#               column(6, selectInput(inputId = "country", label = "Country", choices = c("C1", "C2", "C3"))),
#               column(6, selectInput(inputId = "spatial_aggr", label = "Select administrative level", 
#                                     choices = c("Level 1" = "admin1", "Level 2" = "admin2"),
#                                     selected = "admin2"))
#             ),
#             selectInput(inputId = "date_range", label = "Select time period", choices = c("TBD1", "TBD2")),
#             selectInput(inputId = "epi_indi", label = "Select indicator to be modeled", 
#                         choices = c("Malaria confirmed cases" = "confirmed_cases")),
#             pickerInput(inputId = "env_indi", label = "Select environmental covariates",
#                         choices = c("Rainfall" = "totprec", 
#                                     "Land surface temperature (day)" = "lst_day", 
#                                     "Land surface temperature (night)" = "lst_night",
#                                     "Normalized difference vegeration index (NDVI)" = "ndvi", 
#                                     "Normalized difference water index (NDWI)" = "ndwi6"),
#                         selected = c("totprec", "lst_day", "ndwi6"), 
#                         options = list('actions-box' = TRUE),
#                         multiple = TRUE),
#             checkboxInput(inputId = "env_anomalies", label = "Include environmental anomaly", value = TRUE),
#             numericInput(inputId = "env_lag_length", label = "Maximum environmental lag (in months)",
#                         value = "6", min = 0, max = 13, step = 1),
#             br(),
#             hr(style = "border-top: 1px solid #9aa5a6;"),
#             HTML("<h4>Model parameters</h4>"),
#             br(),
#             selectInput(inputId = "fc_model_family", label = "Error distribution family", 
#                         choices = c("Gaussian" = "gaussian()"), selected = "gaussian()"),
#             selectInput(inputId = "fc_splines", label = "Spline function used to model long-term trend and lagged environmental variable",
#                         choices = c("Thin plate" = "tp"), selected = "tp"),
#             checkboxInput(inputId = "fc_cyclicals", label = "Include seasonal cyclical in the model", value = TRUE),
#             # checkboxInput(inputId = "env_anomalies", label = "Include environmental anomaly", value = TRUE),
#             numericInput(inputId = "fc_future_period", label = "Forecast span (in months)", 
#                          value = 2, min = 2, max = 2),
#             br(),
#             hr(style = "border-top: 1px solid #9aa5a6;"),
#             br(),
#             actionButton(inputId = "run_model", label = "Run model")
#           )
#       ), 
#       mainPanel(
#         fluidRow(style = "margin-left:-40px;",
#           hidden(
#             div(id = "side_expand", 
#               column(width = 1,# style = "background-color:#4d3a7d;",
#                      # wellPanel(
#                        actionBttn("showSidebar", label = NULL, style = "material-circle",
#                                   icon = icon("arrow-right"), size = "sm", color = "default")) #)
#               
#           )),
#           
#           # add_busy_spinner(spin = "cube-grid"),
#           
#           column(width = 10, offset = 1, 
#                  
#                  fluidRow(uiOutput("result_header")),
#                  
#                  fluidRow(column(7, leafletOutput("out_map"))),
#                  
#                  hr(),
#                  
#                  # fluidRow( HTML("<h4>Time series</h4>") ),
#                  
#                  fluidRow(uiOutput("timeseries_txt")),
#                  
#                  fluidRow(uiOutput("woreda_select_input")),
#                  
#                  
#                  fluidRow(plotOutput("ts_output"))
#           
#           )
#           
#           
#           # column(width = 10, offset = 1, plotOutput("myplot"))
#         )
#           
#           
#         
#       ) # end "TOOL" main panel
#     ), # end "TOOL" tab panel
#     tabPanel("Documentation",
#       "Put methods, data etc here"
#     )
#   )
# )


# ui <- page_navbar(
#   nav("First tab"),
#   nav("Second tab"),
#   nav_item(a(href="http://stackoverflow.com", "stackoverflow")))

# ui <- page_navbar(
#   window_title = "CLIMATE",
#   theme = bs_theme(bootswatch = "litera"),
#   title = div(style = "margin-left:0; margin-right:0;padding-right: 0; padding-left:0; font-size: 0px;",HTML("")),
#   
#   # Landing/Welcome Page --------------------------------------------------------------------
#   
#   tabPanel("CLIMATE",
#            # tags$div(style = "margin-left: 10px;",
#                     fluidPage(HTML("Welcome to CLIMATE dashboard!<br>
#                               [Description of each plot (what questions they answer), how to use the dashboard, etc.]"),
#                               fluidRow(HTML("This is Row 1")),
#                               fluidRow(HTML("This is Row 2"))
#                               
#                     )
#            # )
#            
#   ),
#   
#   # Seasonality and Historical Tracker ----------------------------------------------------------
#   
#   
#   tabPanel(
#     title = "Seasonality & Historical Trackers",
#     tags$div(style = "margin-left:12px;",
#     fluidPage(
#       
#           fluidRow(HTML("<h5>Seasonality Tracker</h5>")),
#           fluidRow(
#             column(width = 6, "Col1"),
#             column(width = 6, "Col 2")
#           )
#         
#       )
#     )
#            
#     
#   ),
#   
#   
#   ######
#   tabPanel(title = "Tab X",
#            
#     tags$div( style = "margin-left: 10px; padding-left: 12px;",
#               
#       fluidPage( 
#         div(style = "margin-left:12px;",
#         
#         # fixedPanel(
#         #   left = 0, right = 0,
#         #   selectInput(
#         #     inputId = "geo_ssn", label = c("Select Geography"),
#         #     choices = c("National", "Admin. Level 1", "Admin. Level 2"),
#         #     selected = "National")
#         # 
#         # ),         
#                  
#         fluidRow(# style = "margin-left: 10px;",
#           HTML("<h5>Seasonality Tracker</h3>")
#         ),
#         fluidRow(
#           column(width = 6, "Hello"),
#           column(width = 6, "GoodBye")
#         )
#       )
#       )
#     )
#   ),
#   
#   # Comparison ---------------------------------------------------------------------------------
#   
#   tabPanel("Compare"),
#   
#   # Data Download ------------------------------------------------------------------------------
#   tabPanel("Data Download"),
#   
#   # HTML Tags ----------------------------------------------------------------------------------
#   tags$style(HTML(".navbar-brand { width: 0px; margin-right: 0; margin-left: 0; padding-right: 0px; padding-left: 0px;}"))
# )



# ui <- page_navbar(
#   window_title = "CLIMATE",
#   theme = bs_theme(bootswatch = "litera"),
#   title = div(style = "margin-left:0; margin-right:0;padding-right: 0; padding-left:0; font-size: 0px;",HTML("")),
#   
#   # Landing/Welcome Page --------------------------------------------------------------------
#   
#   tabPanel("CLIMATE",
#            # tags$div(style = "margin-left: 10px;",
#                     fluidPage(HTML("Welcome to CLIMATE dashboard!<br>
#                               [Description of each plot (what questions they answer), how to use the dashboard, etc.]"),
#                               fluidRow(HTML("This is Row 1")),
#                               fluidRow(HTML("This is Row 2"))


# ui <- page_navbar(
# 
#   window_title = "CLIMATE",
#   title = "",
#   theme = bs_theme(bootswatch = "litera"),
#   tabPanel("Panel1",
#            tags$div(style = "margin-left:25px;",
#            fluidPage(
#              fluidRow("This is Row 1"),
#              fluidRow("This is Row 2")
#            ),
#            
#            
#            )),
#   tabPanel("Panel2",
#            tags$div(style = "margin-left:25px;",
#            fluidPage(
#              fluidRow("This is Row 1"),
#              fluidRow("This is Row 2"),
#              fluidRow(column(width = 6, style = "margin: 0px; padding-left:0px;", "This is column 1"), column(width = 6, "This is column2"))
#            ))),
#   tabPanel("Panel3"),
#   tags$style(HTML(".navbar-brand { width: 0px; margin-right: 0; margin-left: 0; padding-right: 0px; padding-left: 0px;}"))
#   
# )



ui <- bslib::page_navbar(
  
  window_title = "CLIMATE", title = "",
  theme = bs_theme(bootswatch = "litera", version = 5, bg = "white", fg = "black", font_scale = 0.8),
  
  tabPanel(
    "CLIMATE",
    tags$div(style = "margin-left: 25px;",
      fluidPage(
        fluidRow(HTML("Welcome to CLIMATE dashboard!<br> [Description of each plot (what questions they answer), 
                      how to use the dashboard, etc.]"))
      )
    )
  ),
  
  tabPanel(
    "Seasonality and Historical Trends",
    tags$div(
      style = "margin-left:25px;",
      fluidPage(
        fluidRow(
          selectInput(
            inputId = "ev_ssn", label = "Climate Variable",
            choices = c("Rainfall (Monthly average)", "Rainfall (Cumulative)", 
                        "Air Temperature", "Land Surface Temperature", "Specific Humidity", "Soil Moisture",
                        "Vegetation Index")
          ),
          selectInput(
            inputId = "geo_ssn", label = "Geographical Level",
            choices = c("National", "Admin. Level 1", "Admin. Level 2"),
            selected = "National"
          ),
          selectInput(
            inputId = "ctry_ssn", label = "Region/Country",
            choices = c("All Countries", "Western Africa", "Eastern Africa", "Central/Southern Africa",
                        "Southeast Asia",
                        "----------------------", "Country 1", "Country 2", "Country 3"),
            selected = "All Countries"
          )
        ),
        br(),
        br(),
        fluidRow(
          column(width = 6, "This is column 1"),
          column(width = 6, "This is column 2")
        )
      )
             
      
    )
  ),
  
  tabPanel("Data Download"),
  tabPanel("Documentation")
  
)

