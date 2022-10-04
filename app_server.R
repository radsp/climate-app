server <-function(input, output, session) {
  
  # Prevent "greying out" when running in Civis Platform
  observe(input$alive_count)
  session$allowReconnect("force")
  
  screen_dim <- reactive({
    return(as.numeric(input$dimension))
  })
  
 
  
  # Trackers tab -----------------------------------------------------------------------------
  
  # output$myplot1_track <- renderPlot({plot(runif(100), runif(100))})
  # output$myplot2_track <- renderPlot({plot(runif(100), runif(100))})
  
  
  ## > Update admin level 1 & 2 selection ------------
  
  # observe({
  #   if ( (input$gres_track %in% c("admin1", "admin2")) ) {
  #     adm1_list <- get_choice_adm1(input$adm0_track)
  #     output$adm1_track <- renderUI({
  #       pickerInput(
  #         inputId = "adm1_track", label = HTML("<b>Admin. Level 1</b>"),
  #         choices = adm1_list$choice,
  #         selected = adm1_list$selected,
  #         options = list(`actions-box` = TRUE, `selectedTextFormat` = 'count > 3'), multiple = TRUE
  #       )
  #     })
  #   } else {
  #     output$adm1_track <- renderUI({NULL})
  #   }
  # })
  
  
  # observeEvent(c(input$gres_track, input$adm0_track, input$adm1_track), {
  #   
  #   if (input$gres_track == "admin0") {
  #     
  #     updatePickerInput(
  #       session = session, inputId = "adm0_track", options = list(`actions-box` = TRUE))
  #     updatePickerInput(
  #       session = session, inputId = "adm1_track", choices = "None", selected = "None")
  #     updatePickerInput(
  #       session = session, inputId = "adm2_track", choices = "None", selected = "None")
  #     shinyjs::disable("adm1_track")
  #     shinyjs::disable("adm2_track")
  #     
  #   } else if (input$gres_track == "admin1") {
  # 
  #     updatePickerInput(
  #       session = session, inputId = "adm2_track", choices = "None", selected = "None")
  #     updatePickerInput(
  #       session = session, inputId = "adm0_track", options = list(`actions-box` = TRUE), multiple = TRUE)
  #     
  #     adm1_list <- get_choice_adm1(input$adm0_track)
  #     
  #     if (any(input$adm1_track %in% adm1_list$selected)) {
  #       adm1_slctd <- input$adm1_track[input$adm1_track %in% adm1_list$selected]
  #     } else {
  #       adm1_slctd <- adm1_list$selected
  #     }
  #     
  #     updatePickerInput (
  #       session = session, inputId = "adm1_track",
  #       choices = adm1_list$choice, selected = adm1_slctd,
  #       options = list(`actions-box` = TRUE, `selectedTextFormat` = 'count > 3'), multiple = TRUE)
  #     
  #     shinyjs::enable("adm1_track")
  #     shinyjs::disable("adm2_track")
  #     
  #   } else {
  #     
  #     shinyjs::enable("adm1_track")
  #     shinyjs::enable("adm2_track")
  #     
  #     if (length(input$adm0_track) > 3) {
  #       adm0_slctd <- input$adm0_track[1:3]
  #     } else {
  #       adm0_slctd <- input$adm0_track
  #     }
  #     updatePickerInput(
  #       session = session, inputId = "adm0_track", selected = adm0_slctd,
  #       options = list(`max-options` = 3, `actions-box` = TRUE), multiple = TRUE
  #     )
  #     
  #     adm1_list <- get_choice_adm1(adm0_slctd)
  #     adm1_slctd <- adm1_list$selected
  #     
  #     if (length(input$adm0_track > 1)) {
  #       if (any(input$adm1_track %in% adm1_list$selected)) {
  #         adm1_tmp <- input$adm1_track[input$adm1_track %in% adm1_list$selected]
  #         adm1_slctd <- adm1_tmp[1:(min(adm1_tmp, 5))]
  #       } else {
  #         adm1_slctd <- adm1_list$selected[1:5]
  #       }
  #       updatePickerInput(
  #         session = session, inputId = "adm1_track", choices = adm1_list$choice, selected = adm1_slctd,
  #         options = list(`max-options` = 5, `actions-box` = TRUE, `selectedTextFormat` = 'count > 3'), multiple = TRUE
  #       )
  #     } else {
  #       updatePickerInput(
  #         session = session, inputId = "adm1_track", choices = adm1_list$choice, selected = adm1_slctd,
  #         options = list(`actions-box` = TRUE, `selectedTextFormat` = 'count > 3'), multiple = TRUE
  #       )
  #     }
  #     
  #     
  #     
  #     
  #     
  #   }
  #   
  #   
  #   
  # })
  # 
  
  
  observeEvent(input$gres_track, {
    
    if (input$gres_track == "admin0") {
      
      updatePickerInput(
        session = session, inputId = "adm1_track",
        choices = "None", selected = "None"
      )
      updatePickerInput(
        session = session, inputId = "adm2_track",
        choices = "None", selected = "None"
      )
      updatePickerInput(
        session = session, inputId = "adm0_track",
        options = list(`actions-box` = TRUE)
      )
      shinyjs::disable("adm1_track")
      shinyjs::disable("adm2_track")
      
    } else if (input$gres_track == "admin1") {
      
      adm1_list <- get_choice_adm1(input$adm0_track)
      
      if (any(input$adm1_track %in% adm1_list$selected)) {
        adm1_slctd <- input$adm1_track[input$adm1_track %in% adm1_list$selected]
      } else {
        adm1_slctd <- adm1_list$selected
      }
      
      updatePickerInput (
        session = session, inputId = "adm1_track", choices = adm1_list$choice, selected = adm1_list$selected, 
        options = list(`actions-box` = TRUE, `selectedTextFormat` = 'count > 3'))
      
      updatePickerInput(
        session = session, inputId = "adm2_track",
        choices = "None", selected = "None"
      )
      
      shinyjs::enable("adm1_track")
      shinyjs::disable("adm2_track")
      
    } else {
      
      if( length(input$adm0_track) > 3) {
        adm0_slctd <- input$adm0_track[1:3]
      } else {
        adm0_slctd <- input$adm0_track
      }
      
      updatePickerInput(
        session = session, inputId = "adm0_track", selected = adm0_slctd,
        options = list(`max-options` = 3, `actions-box` = TRUE, `selectedTextFormat` = 'count > 3'))
      
      adm1_list <- get_choice_adm1(input$adm0_track)
      
      if (any(input$adm1_track %in% adm1_list$selected)) {
        adm1_tmp <- input$adm1_track[input$adm1_track %in% adm1_list$selected]
        adm1_slctd <- adm1_tmp[1:(min(length(adm1_tmp), 5))]
      } else {
        adm1_slctd <- adm1_list$selected[1:5]
      }
      
      updatePickerInput(
        session, inputId = "adm1_track", choices = adm1_list$choice, selected = adm1_slctd, 
        options = list(`max-options` = 5, `actions-box` = TRUE, `selectedTextFormat` = 'count > 3'))
      
      adm2_list <- get_choice_adm2(input$adm0_track, adm1_slctd)
      
      updatePickerInput(
        session, inputId = "adm2_track", choices = adm2_list$choice, selected = adm2_list$selected, 
        options = list(`actions-box` = TRUE, `selectedTextFormat` = 'count > 3')
      )
      
      shinyjs::enable("adm1_track")
      shinyjs::enable("adm2_track")
      
    }
    
    
  })
  
  
  
  # observeEvent(input$gres_track,{
  #   
  #   if (input$gres_track == "admin2") {
  #     
  #     if( length(input$adm0_track) > 3) {
  #       adm0_slctd <- input$adm0_track[1:3]
  #     } else {
  #       adm0_slctd <- input$adm0_track
  #     }
  #     
  #     updatePickerInput(session, inputId = "adm0_track",
  #       options = list(`max-options` = 3, `actions-box` = TRUE, `selectedTextFormat` = 'count > 3'),
  #                        selected = adm0_slctd)
  #     
  #     adm1_list <- get_choice_adm1(input$adm0_track)
  #     
  #     if ( (all(input$adm1_track == "None")) & (any(input$adm1_track %in% adm1_list$selected)) ) {
  #       adm1_slctd <- input$adm1_track[input$adm1_track %in% adm1_list$selected]
  #     } else {
  #       adm1_slctd <- adm1_list$selected
  #     }
  # 
  #     if (all(input$adm1_track == "None")) {
  #       updatePickerInput (
  #         session = session, inputId = "adm1_track",
  #         choices = adm1_list$choice, selected = adm1_list$selected,
  #         options = list(`actions-box` = TRUE, `selectedTextFormat` = 'count > 3'))
  #     } else {
  #       if(length(input$adm0_track) > 1){
  #         updatePickerInput(
  #           session = session, inputId  = "adm1_track",
  #           choices = adm1_list$choice, selected = adm1_slctd[1:5],
  #           options = list(`action-box` = TRUE, `max-options` = 5, `selectedTextFormat` = 'count > 3'))
  #       } else {
  #         updatePickerInput(
  #           session = session, inputId  = "adm1_track",
  #           choices = adm1_list$choice, selected = adm1_slctd[1:5],
  #           options = list(`action-box` = TRUE, `max-options` = 5, `selectedTextFormat` = 'count > 3')
  #         )
  #       }
  # 
  #     }
  #     shinyjs::enable("adm1_track")
  #     shinyjs::enable("adm2_track")
  #   } else if (input$gres_track == "admin1") {
  #     if (all(input$adm1_track == "None")) {
  #       adm1_list <- get_choice_adm1(input$adm0_track)
  #       updatePickerInput (
  #         session = session, inputId = "adm1_track",
  #         choices = adm1_list$choice, selected = adm1_list$selected,
  #         options = list(`actions-box` = TRUE, `selectedTextFormat` = 'count > 3'))
  #     } else {
  #       updatePickerInput (
  #         session = session, inputId = "adm1_track", 
  #         options = list(`actions-box` = TRUE, `selectedTextFormat` = 'count > 3'))
  #     }
  # 
  #     updatePickerInput(
  #       session = session, inputId = "adm2_track",
  #       choices = "None", selected = "None"
  #     )
  # 
  #     shinyjs::enable("adm1_track")
  #     shinyjs::disable("adm2_track")
  #   } else {
  #     updatePickerInput(
  #       session = session, inputId = "adm1_track",
  #       choices = "None", selected = "None"
  #     )
  #     updatePickerInput(
  #       session = session, inputId = "adm2_track",
  #       choices = "None", selected = "None"
  #     )
  #     updatePickerInput(
  #       session = session, inputId = "adm0_track",
  #       options = list(`actions-box` = TRUE, `selectedTextFormat` = 'count > 3')
  #     )
  #     shinyjs::disable("adm1_track")
  #     shinyjs::disable("adm2_track")
  #   }
  # }, ignoreNULL = F)



  observeEvent(input$adm0_track,{

    adm1_list <- get_choice_adm1(input$adm0_track)

    if ( (any(input$adm1_track %in% "None")) & (any(input$adm1_track %in% adm1_list$selected)) ) {
      adm1_slctd <- input$adm1_track[input$adm1_track %in% adm1_list$selected]
    } else {
      adm1_slctd <- adm1_list$selected
    }

    if (input$gres_track == "admin1") {
      updatePickerInput (
        session = session, inputId = "adm1_track",
        choices = adm1_list$choice, selected = adm1_slctd,
        options = list(`actions-box` = TRUE, `selectedTextFormat` = 'count > 3'))
    } else if (input$gres_track == "admin2") {
      if(length(input$adm0_track) > 1){
        updatePickerInput(
          session = session, inputId  = "adm1_track",
          choices = adm1_list$choice, selected = adm1_slctd[1:5],
          options = list(`action-box` = TRUE, `max-options` = 5, `selectedTextFormat` = 'count > 3'))
      } else {
        updatePickerInput(
          session = session, inputId  = "adm1_track",
          choices = adm1_list$choice, selected = adm1_slctd[1:5],
          options = list(`action-box` = TRUE, `max-options` = 5, `selectedTextFormat` = 'count > 3')
        )
      }
    }
  })


  observeEvent(input$adm1_track, {
    if (input$gres_track == "admin2")  {
      adm2_list <- get_choice_adm2(input$adm0_track, input$adm1_track)
      updatePickerInput(
        session = session, inputId  = "adm2_track",
        choices = adm2_list$choice, selected = adm2_list$selected,
        options = list(`action-box` = TRUE, `selectedTextFormat` = 'count > 3')
      )
    }
  })


  
  
  ## > Hide & Show Filter panel -----------------------
  
  # Close filter
  observeEvent(
    input$bt2mini_track, {
      shinyjs::hide("filter_track")
      shinyjs::show("filter_hidden_track")
      shinyjs::runjs("$('#plot_panel_track').
                     removeClass('col-sm-8 offset-md-4 col-sm-offset-4').
                     addClass('col-sm-11 offset-md-1 col-sm-offset-1')")
      shinyjs::runjs("$(window).trigger('resize')")
    }
  )
  
  # Open filter
  # observeEvent(
  #   input$bt2maxi_track, {
  #     shinyjs::show("filter_track")
  #     shinyjs::hide("filter_hidden_track")
  #     shinyjs::runjs("$('#plot_panel_track').
  #                    removeClass('col-sm-11 offset-md-1 col-sm-offset-1').
  #                    addClass('col-sm-8 offset-md-4 col-sm-offset-4')")
  #     shinyjs::runjs("$(window).trigger('resize')")
  #   }
  # )
  
  
  ## > Plot seasonal trackers -------------------------
  
  output$legend_ssn <- renderUI({
    
    yrs <- sort(input$year_ssn)
    
    ev <- get_ev_par(input$ev_track) 
    
    line_seq <- rev(rev(lgnd_seq)[1:length(yrs)])
    
    ltxt <- paste0(ev$label, ": &nbsp;", "<span style='background-color:#cccccc'>&nbsp;&nbsp;&nbsp; &nbsp; </span> &nbsp;
      Historical Range &nbsp; &nbsp; 
      <font color = #999999> -&nbsp;-&nbsp;-&nbsp;-&nbsp;</font> &nbsp; Historical Average ")
    
    for(i in 1:length(yrs)) {
      ltxt <- paste0(ltxt, "&nbsp;&nbsp;&nbsp;&nbsp;", 
             "<span class = ", line_seq[i], "></span> &nbsp; ", yrs[i])
    }
    
    
    # HTML(
    #   " <span style='background-color:#cccccc'>&nbsp;&nbsp;&nbsp;&nbsp; &nbsp; </span> &nbsp;
    #   Historical Range &nbsp; &nbsp; &nbsp;
    #   <font color = #999999> -&nbsp;-&nbsp;-&nbsp;-&nbsp;</font> &nbsp; Historical Average &nbsp;&nbsp;&nbsp;&nbsp; 
    #   <span class = 'linered'></span> &nbsp; 20xx &nbsp;&nbsp;&nbsp;&nbsp; 
    #   <span class = 'lineblue'></span> &nbsp; 20xx")
    
    HTML(ltxt)
  })
  

  
  xmain <- eventReactive (input$goplot_track, {
        get_data_main(adm_res = input$gres_track, gid0 = input$adm0_track, 
                      gid1 = input$adm1_track, gid2 = input$adm2_track, ev = input$ev_track)
  }, ignoreNULL = F)
  

  
  # output$test_out <- renderText({
  # #   dtest <- xmain()
  # #   paste("agg = ", dtest$agg, "\n ev = ", dtest$ev, "\n yrs = ", paste0(dtest$yrs, collapse = ", "))
  # #   paste0(head(dtest$df))
  #   paste(input$adm1_track, collapse = ", ")
  # })
  

  output$myplot1_track <- renderPlotly({
    
    ssize <- screen_dim()

    # tt0 <- proc.time()

    x2pl <- xmain()
    x2pl$df <- x2pl$df %>%
      filter(year %in% as.numeric(input$year_ssn))
    x2pl$yrs <- as.numeric(input$year_ssn)
    
    yax_std <- ifelse(input$yax_ssn == "Same", TRUE, FALSE)

    tt1 <- proc.time()

    uu <- get_plot_ssn(u = x2pl$df, ev = x2pl$ev, epi = NULL, agg = x2pl$agg, yrs = x2pl$yrs, ssn = "cy", yax_std = yax_std, ssize = ssize)

    # tt2 <- proc.time()

    # ttd1 <- tt1 - tt0
    # ttd2 <- tt2 - tt1
    # output$test_out <- renderText({
    #   paste0("Getting data = ", ttd1[["elapsed"]], " sec;", " Plotting data = ", ttd2[["elapsed"]]/60, " sec.\n
    #           Screen dim = ", ssize[1], ", ", ssize[2])
    # })

    return(uu)

  })
  
  
  output$test_out2 <- renderText({
      paste0(paste0(input$date_histo, collapse = ", "), " class = ", class(input$date_histo))
    })
  
  
  
  
  output$myplot2_track <- renderPlotly({
    
    ssize <- screen_dim()
    
    x2pl <- xmain()
    x2pl$df <- x2pl$df %>%
      filter((date >= input$date_histo[1]) & (date <= input$date_histo[2]))
    x2pl$yrs <- as.numeric(input$year_ssn)
    
    yax_std <- ifelse(input$yax_histo == "Same", TRUE, FALSE)
    
    uu <- get_plot_histo(u = x2pl$df, ev = x2pl$ev, epi = NULL, agg = x2pl$agg, date_range = input$date_histo, ssn = "cy", yax_std = yax_std, ssize = ssize)
    
    return(uu)
    
  })
  
}

