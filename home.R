tab_home <- tabPanel (
  "", icon = icon('house'), # icon = icon("home"),
  tags$div (style = 'margin-left:5px;', # "margin-left:25px;",
    fluidPage (
      fluidRow(
        HTML("Welcome to CLIMATE dashboard! <br><br> 
             [Description of each plot (what questions they answer),
             how to use the dashboard, etc.")
      )
    )
  )
  
)