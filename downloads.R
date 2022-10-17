tab_download <-
  tabPanel(
    "Downloads",
    fluidPage(
      fluidRow(
        wellPanel(
          style = "padding-bottom: 4px; padding-top:4px;font-size:12px;",
          fluidRow(
            column(width = 6, HTML("<h5>FILTER</h5>")),
            column(
              width = 6, align = "right",
              actionButton(
                inputId = "godata", label = "Get Data",
                class = "btn-primary", style = "margin-top:10px;"
              )
            )
          ),
          fluidRow(
            style = "margin-top:4px; margin-bottom:0px;padding-bottom:0px; padding-top:0px;margin-bottom:0px;",
            column(
              width = 4, # style = "margin-top:-5px;",
              div(
                class = "smalbox",
                pickerInput(
                  inputId = "ev_down", label = HTML("Climate Variable"),
                  choices = c(
                    "Rainfall" = "rf", "Rainfall (cumulative)" = "rfacc", "Air Temperature" = "tair",
                    "Land Surface Temperature" = "lstd",
                    "Vegetation Index" = "ndvi", "Specific Humidity" = "sh", "Soil Moisture" = "sm"
                  ),
                  selected = "rf", options = list(`actions-box` = TRUE), multiple = TRUE
                )
              )
            ),
            div(
              class = "smallbox",
              column(
                width = 4,
                airDatepickerInput(
                  inputId = "date_down", label = HTML("<b>Date Range</b>"), range = TRUE,
                  view = "months", minView = "months", clearButton = T, update_on = "close",
                  dateFormat = "MMM yyyy", monthsField = "monthsShort", separator = " to ",
                  minDate = as.Date("2017-01-01"), maxDate = date_now, value = c(as.Date("2017-01-01"), date_now)
                )
              )
            ),
            column(
              width = 4,
              selectizeInput(
                inputId = "gres_down", label = HTML("Geographical Aggregation"),
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
                  inputId = "adm0_down", label = HTML("Country"),
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
                  inputId = "adm1_down", label = HTML("Admin. Level 1"),
                  choices = "None", selected = "None", options = list(`actions-box` = TRUE), multiple = TRUE
                )
              )
            ),
            column(
              width = 4, style = "margin-top:-10px;",
              div(
                class = "smallbox",
                pickerInput(
                  inputId = "adm2_down", label = HTML("Admin. Level 2"),
                  choices = "None", selected = "None", options = list(`actions-box` = TRUE), multiple = TRUE
                )
              )
            )
          )
        )
      ),
      br(),
      br(),
      fluidRow(
        column(
          width = 6, HTML("<H4> Table Preview </H4> (first 10 rows)")
        ),
        column(
          width = 6, align = "right",
          downloadButton("download_btn",
            label = "Download",
            class = "btn-primary", style = "margin-top:10px;"
          )
        )
      ),
      tableOutput("table")
    )
  )
