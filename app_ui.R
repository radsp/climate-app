ui <- fluidPage(
  
  includeCSS("www/style.css"),
  
  navbarPage(
  # bslib::page_navbar(
    
  #   theme = bs_theme(bootswatch = "paper", version = 3, 
  #                    fg = "black", bg = "white"),
    
    useShinyjs(),
    
    tags$head(HTML("<script type='text/javascript' src='keep-alive.js'></script>")),
    
    # Tabs
    
    tags$head(tags$script('var dimension = [0, 0];$(document).on("shiny:connected", function(e) {
                                      dimension[0] = window.innerWidth; dimension[1] = window.innerHeight; Shiny.onInputChange("dimension", dimension);});
                                      $(window).resize(function(e) {dimension[0] = window.innerWidth; dimension[1] = window.innerHeight;
                                      Shiny.onInputChange("dimension", dimension); });')),
    
    # tab_home,
    tab_trackers,
    #tab_compare,
    #tab_download,
    #tab_docs,
    # tabPanel("Compare/Overlay"),
    tabPanel("Download Data"),
    
    title = "CLIMATE",
    # window_title = "CLIMATE",
    inverse = F
    
    
  )
  
)
