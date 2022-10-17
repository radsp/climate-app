server <- function(input, output, session) {
  # User Level Filtering -----------
  # check if the app is running locally or on MDIVE
  query <- shiny::parseQueryString(session$request$QUERY_STRING)
  userid <- query$civisuserid
  CIVIS_SERVICE_ID <- Sys.getenv("CIVIS_SERVICE_ID")
  
  if (is.null(userid) && CIVIS_SERVICE_ID == "") {
    message("Running locally. Getting user from API")
    userid <- civis::users_list_me()$id
  }
  
  # Change this to test the  user permissions for a given userid
  # userid <- 10780 # PMI Quality Control user (all countries, except Myanmar)
  # userid <-14237 # Busisani Dube (Zimbabwe only)
  
  countries_granted <- get_countries(user = userid, permissionset = mdiver::MDIVE_PERMISSION_SET_ID)
  
  epi_cols <- c(
    "suspected_cases",
    "suspected_cases_rate",
    "tested_cases", 
    "tested_cases_rate",
    "confirmed_cases",
    "confirmed_cases_rate",
    "tpr",
    "severe_cases",
    "severe_cases_rate",
    "malaria_deaths",
    "malaria_deaths_rate"
    )
  
  # Remove surveillance data for countries the user does not have access to
  df_all_ulf <- df_all
  df_all_ulf[!(df_all_ulf$country %in% countries_granted), epi_cols] <- NA
  
  # Prevent "greying out" when running in Civis Platform
  observe(input$alive_count)
  session$allowReconnect("force")

  screen_dim <- reactive({
    return(as.numeric(input$dimension))
  })

  # observeEvent(input$dimension, {
  #
  #   ssize <- screen_dim()
  #
  #   ncols <- ifelse(ssize[1] < 980, 3, 5)
  #
  #   nrows <- as.integer(ceiling((length(ctry))/ncols))
  #
  #   print(paste0("ncol = ", ncols, ", nrows = ", nrows))
  # }, ignoreNULL = F)





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
    nctry <- length(input$adm0_track)

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
      if (nctry > 1) {
        if (nctry > 3) {
          adm0_slctd <- input$adm0_track[1:3]
        } else {
          adm0_slctd <- input$adm0_track
        }

        updatePickerInput(
          session = session, inputId = "adm0_track", selected = adm0_slctd,
          options = list(`max-options` = 3, `actions-box` = TRUE, `selectedTextFormat` = "count > 3")
        )
      }

      adm1_list <- get_choice_adm1(input$adm0_track)


      if (any(input$adm1_track %in% adm1_list$selected)) {
        adm1_slctd <- input$adm1_track[input$adm1_track %in% adm1_list$selected]
      } else {
        adm1_slctd <- adm1_list$selected
      }

      if (length(adm1_slctd) > 30) {
        adm1_slctd <- adm1_slctd[1:30]
      }

      if (nctry > 1) {
        nmax1 <- 30
      } else {
        nmax1 <- NULL
      }

      updatePickerInput(
        session = session, inputId = "adm1_track", choices = adm1_list$choice, selected = adm1_list$selected,
        options = list(`actions-box` = TRUE, `selectedTextFormat` = "count > 3", `maxOptions` = nmax1)
      )

      updatePickerInput(
        session = session, inputId = "adm2_track",
        choices = "None", selected = "None"
      )

      shinyjs::enable("adm1_track")
      shinyjs::disable("adm2_track")
    } else {
      if (length(input$adm0_track) > 3) {
        adm0_slctd <- input$adm0_track[1:3]
      } else {
        adm0_slctd <- input$adm0_track
      }

      updatePickerInput(
        session = session, inputId = "adm0_track", selected = adm0_slctd,
        options = list(`max-options` = 3, `actions-box` = TRUE, `selectedTextFormat` = "count > 3")
      )

      adm1_list <- get_choice_adm1(input$adm0_track)

      if (any(input$adm1_track %in% adm1_list$selected)) {
        adm1_tmp <- input$adm1_track[input$adm1_track %in% adm1_list$selected]
        adm1_slctd <- adm1_tmp[1:(min(length(adm1_tmp), 5))]
      } else {
        adm1_slctd <- adm1_list$selected[1:5]
      }

      if (nctry > 1) {
        nmax1 <- 30
      } else {
        nmax1 <- NULL
      }


      updatePickerInput(
        session,
        inputId = "adm1_track", choices = adm1_list$choice, selected = adm1_slctd,
        options = list(`max-options` = nmax1, `actions-box` = TRUE, `selectedTextFormat` = "count > 3")
      )

      adm2_list <- get_choice_adm2(input$adm0_track, adm1_slctd)

      updatePickerInput(
        session,
        inputId = "adm2_track", choices = adm2_list$choice, selected = adm2_list$selected,
        options = list(`actions-box` = TRUE, `selectedTextFormat` = "count > 3")
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



  observeEvent(input$adm0_track, {
    adm1_list <- get_choice_adm1(input$adm0_track)

    if ((any(input$adm1_track %in% "None")) & (any(input$adm1_track %in% adm1_list$selected))) {
      adm1_slctd <- input$adm1_track[input$adm1_track %in% adm1_list$selected]
    } else {
      adm1_slctd <- adm1_list$selected
    }

    if (input$gres_track == "admin1") {
      updatePickerInput(
        session = session, inputId = "adm1_track",
        choices = adm1_list$choice, selected = adm1_slctd,
        options = list(`actions-box` = TRUE, `selectedTextFormat` = "count > 3")
      )
    } else if (input$gres_track == "admin2") {
      if (length(input$adm0_track) > 1) {
        updatePickerInput(
          session = session, inputId = "adm1_track",
          choices = adm1_list$choice, selected = adm1_slctd[1:5],
          options = list(`action-box` = TRUE, `max-options` = 5, `selectedTextFormat` = "count > 3")
        )
      } else {
        updatePickerInput(
          session = session, inputId = "adm1_track",
          choices = adm1_list$choice, selected = adm1_slctd[1:5],
          options = list(`action-box` = TRUE, `max-options` = 5, `selectedTextFormat` = "count > 3")
        )
      }
    }
  })


  observeEvent(input$adm1_track, {
    if (input$gres_track == "admin2") {
      adm2_list <- get_choice_adm2(input$adm0_track, input$adm1_track)
      updatePickerInput(
        session = session, inputId = "adm2_track",
        choices = adm2_list$choice, selected = adm2_list$selected,
        options = list(`action-box` = TRUE, `selectedTextFormat` = "count > 3")
      )
    }
  })




  ## > Hide & Show Filter panel -----------------------

  # Close filter
  observeEvent(
    input$bt2mini_track,
    {
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


  ## > Plot seasonal trackers --------------------------

  lgnd_par <- eventReactive(input$goplot_track,
    {
      ev <- get_ev_par(input$ev_track)
      epi <- get_epi_par(input$epi_track)

      return(list(ev = ev, epi = epi))
    },
    ignoreNULL = F
  )


  output$legend_ssn <- renderUI({
    par <- lgnd_par()

    yrs <- sort(input$year_ssn)

    # ev <- get_ev_par(input$ev_track)

    line_seq <- rev(rev(lgnd_seq)[1:length(yrs)])

    ltxt <- paste0("<br><b>", par$ev$label, "</b>: &nbsp;&nbsp;", "<span style='background-color:#cccccc'>&nbsp;&nbsp;&nbsp; &nbsp; </span> &nbsp;
      Historical Range &nbsp; &nbsp;
      <font color = #999999> -&nbsp;-&nbsp;-&nbsp;-&nbsp;</font> &nbsp; Historical Average ")

    for (i in 1:length(yrs)) {
      ltxt <- paste0(
        ltxt, "&nbsp;&nbsp;&nbsp;&nbsp;",
        "<span class = ", line_seq[i], "></span> &nbsp; ", yrs[i]
      )
    }

    if (par$epi$unit != "unknown variable") {
      line_seq_epi <- rev(rev(lgnd_epi_seq)[1:length(yrs)])
      ltxt <- paste0(ltxt, "<br><b>", par$epi$label, "</b>:")
      for (i in 1:length(yrs)) {
        ltxt <- paste0(
          ltxt, "&nbsp;&nbsp;&nbsp;&nbsp;",
          # "<div class = ", line_seq_epi[i], "> <div class = 'circle'> </div></div> &nbsp; ", yrs[i])
          "<span class = ", line_seq_epi[i], "></span> &nbsp; ", yrs[i]
        )
      }
    }

    # qq <- xmain()
    # ssize <- screen_dim()
    #
    # # tt0 <- proc.time()
    #
    # qq$df <- qq$df %>%
    #   filter(year %in% as.numeric(input$year_ssn))
    # qq$yrs <- as.numeric(input$year_ssn)
    #
    # yax_std <- ifelse(qq$yax_ssn == "Same", TRUE, FALSE)
    #
    #
    # uu <- get_dummy(u = qq$df, ev = qq$ev, epi = qq$epi, agg = qq$agg, yrs = qq$yrs, ssn = "cy", yax_std = yax_std, ssize = ssize)
    # ltxt <- paste0(ltxt, " <br>Epi data = ", paste0(names(uu), collapse = ", "))


    # HTML(
    #   " <span style='background-color:#cccccc'>&nbsp;&nbsp;&nbsp;&nbsp; &nbsp; </span> &nbsp;
    #   Historical Range &nbsp; &nbsp; &nbsp;
    #   <font color = #999999> -&nbsp;-&nbsp;-&nbsp;-&nbsp;</font> &nbsp; Historical Average &nbsp;&nbsp;&nbsp;&nbsp;
    #   <span class = 'linered'></span> &nbsp; 20xx &nbsp;&nbsp;&nbsp;&nbsp;
    #   <span class = 'lineblue'></span> &nbsp; 20xx")

    HTML(ltxt)
  })



  xmain <- eventReactive(input$goplot_track,
    {
      if (input$epi_track == "none") {
        epi <- NULL
      } else {
        epi <- input$epi_track
      }
      get_data_main(
        df = df_all_ulf,
        adm_res = input$gres_track, 
        gid0 = input$adm0_track,
        gid1 = input$adm1_track, 
        gid2 = input$adm2_track, 
        ev = input$ev_track, 
        epi = epi,
        userid
      )
    },
    ignoreNULL = F
  )



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

    if (input$sort_ssn == "Alphabetically") {
      order <- "alpha"
    } else {
      order <- "lat"
    }


    uu <- get_plot_ssn(u = x2pl$df, ev = x2pl$ev, epi = x2pl$epi, agg = x2pl$agg, yrs = x2pl$yrs, ssn = "cy", yax_std = yax_std, ssize = ssize, order = order)

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

    uu <- get_plot_histo(u = x2pl$df, ev = x2pl$ev, epi = x2pl$epi, agg = x2pl$agg, date_range = input$date_histo, ssn = "cy", yax_std = yax_std, ssize = ssize)

    return(uu)
  })



  # Download Tab -------------------------------------------------------------------------------------

  observeEvent(input$gres_down, {
    nctry <- length(input$adm0_down)

    if (input$gres_down == "admin0") {
      updatePickerInput(
        session = session, inputId = "adm1_down",
        choices = "None", selected = "None"
      )
      updatePickerInput(
        session = session, inputId = "adm2_down",
        choices = "None", selected = "None"
      )
      updatePickerInput(
        session = session, inputId = "adm0_down",
        options = list(`actions-box` = TRUE)
      )
      shinyjs::disable("adm1_down")
      shinyjs::disable("adm2_down")
    } else if (input$gres_down == "admin1") {
      if (nctry > 1) {
        if (nctry > 3) {
          adm0_slctd <- input$adm0_down[1:3]
        } else {
          adm0_slctd <- input$adm0_down
        }

        updatePickerInput(
          session = session, inputId = "adm0_down", selected = adm0_slctd,
          options = list(`max-options` = 3, `actions-box` = TRUE, `selectedTextFormat` = "count > 3")
        )
      }

      adm1_list <- get_choice_adm1(input$adm0_down)


      if (any(input$adm1_down %in% adm1_list$selected)) {
        adm1_slctd <- input$adm1_down[input$adm1_down %in% adm1_list$selected]
      } else {
        adm1_slctd <- adm1_list$selected
      }

      if (length(adm1_slctd) > 30) {
        adm1_slctd <- adm1_slctd[1:30]
      }

      if (nctry > 1) {
        nmax1 <- 30
      } else {
        nmax1 <- NULL
      }

      updatePickerInput(
        session = session, inputId = "adm1_down", choices = adm1_list$choice, selected = adm1_list$selected,
        options = list(`actions-box` = TRUE, `selectedTextFormat` = "count > 3", `maxOptions` = nmax1)
      )

      updatePickerInput(
        session = session, inputId = "adm2_down",
        choices = "None", selected = "None"
      )

      shinyjs::enable("adm1_down")
      shinyjs::disable("adm2_down")
    } else {
      if (length(input$adm0_down) > 3) {
        adm0_slctd <- input$adm0_down[1:3]
      } else {
        adm0_slctd <- input$adm0_down
      }

      updatePickerInput(
        session = session, inputId = "adm0_down", selected = adm0_slctd,
        options = list(`max-options` = 3, `actions-box` = TRUE, `selectedTextFormat` = "count > 3")
      )

      adm1_list <- get_choice_adm1(input$adm0_down)

      if (any(input$adm1_down %in% adm1_list$selected)) {
        adm1_tmp <- input$adm1_down[input$adm1_down %in% adm1_list$selected]
        adm1_slctd <- adm1_tmp[1:(min(length(adm1_tmp), 5))]
      } else {
        adm1_slctd <- adm1_list$selected[1:5]
      }

      if (nctry > 1) {
        nmax1 <- 30
      } else {
        nmax1 <- NULL
      }


      updatePickerInput(
        session,
        inputId = "adm1_down", choices = adm1_list$choice, selected = adm1_slctd,
        options = list(`max-options` = nmax1, `actions-box` = TRUE, `selectedTextFormat` = "count > 3")
      )

      adm2_list <- get_choice_adm2(input$adm0_down, adm1_slctd)

      updatePickerInput(
        session,
        inputId = "adm2_down", choices = adm2_list$choice, selected = adm2_list$selected,
        options = list(`actions-box` = TRUE, `selectedTextFormat` = "count > 3")
      )

      shinyjs::enable("adm1_down")
      shinyjs::enable("adm2_down")
    }
  })


  observeEvent(input$adm0_down, {
    adm1_list <- get_choice_adm1(input$adm0_down)

    if ((any(input$adm1_down %in% "None")) & (any(input$adm1_down %in% adm1_list$selected))) {
      adm1_slctd <- input$adm1_down[input$adm1_down %in% adm1_list$selected]
    } else {
      adm1_slctd <- adm1_list$selected
    }

    if (input$gres_down == "admin1") {
      updatePickerInput(
        session = session, inputId = "adm1_down",
        choices = adm1_list$choice, selected = adm1_slctd,
        options = list(`actions-box` = TRUE, `selectedTextFormat` = "count > 3")
      )
    } else if (input$gres_down == "admin2") {
      if (length(input$adm0_down) > 1) {
        updatePickerInput(
          session = session, inputId = "adm1_down",
          choices = adm1_list$choice, selected = adm1_slctd[1:5],
          options = list(`action-box` = TRUE, `max-options` = 5, `selectedTextFormat` = "count > 3")
        )
      } else {
        updatePickerInput(
          session = session, inputId = "adm1_down",
          choices = adm1_list$choice, selected = adm1_slctd[1:5],
          options = list(`action-box` = TRUE, `max-options` = 5, `selectedTextFormat` = "count > 3")
        )
      }
    }
  })


  observeEvent(input$adm1_down, {
    if (input$gres_down == "admin2") {
      adm2_list <- get_choice_adm2(input$adm0_down, input$adm1_down)
      updatePickerInput(
        session = session, inputId = "adm2_down",
        choices = adm2_list$choice, selected = adm2_list$selected,
        options = list(`action-box` = TRUE, `selectedTextFormat` = "count > 3")
      )
    }
  })



  xtab <- eventReactive(input$godata,
    {
      if (input$gres_down == "admin0") {
        gid <- input$adm0_down
        agg <- 0
        adm_var <- "country"
      } else if (input$gres_down == "admin1") {
        gid <- input$adm1_down
        agg <- 1
        adm_var <- c("country", "admin_level_1")
      } else {
        gid <- input$adm2_down
        agg <- 2
        adm_var <- c("country", "admin_level_1", "admin_level_2")
      }

      # y <- x0 %>%
      #   filter((geo_id %in% gid) & (variable_name %in% input$ev_down) & (date >= input$date_down[1]) & (date <= input$date_down[2])) %>%
      #   select(c(all_of(adm_var), variable_name, date, value, monthly_ave, minval, maxval)) %>%
      #   rename(longterm_mean = monthly_ave, longterm_min = minval, longterm_max = maxval)

      # nev <- length(input$ev_down)
      # qev <- ifelse(nev == 1, paste0("= '", input$ev_down, "'"),
      #   paste0("IN (", paste0(paste0("'", input$ev_down, "'"), collapse = ", "), ")")
      # )
      #
      # query <- paste0(
      #   "SELECT geo_id,", adm_var,
      #   ", variable_name, date, value monthly_ave, minval, maxval
      #           FROM staging_pmihq.climate_app WHERE ",
      #   "geo_id IN (", paste0(gid, collapse = ", "), ") AND ",
      #   "variable_name ", qev, " AND (date BETWEEN '", input$date_down[1], "' AND '", input$date_down[2], "');"
      # )


      # y <- read_civis(sql(query),
      #   database = "PMI"
      # ) %>%
      #   mutate(date = as.Date(as.character(date)))
      
      y <- df_all_ulf %>%
        filter(geo_id %in% gid,
               variable_name %in% input$ev_down,
               date >= input$date_down[1],
               date <= input$date_down[2]) %>%
        select(c(all_of(adm_var), variable_name, date, value, monthly_ave, minval, maxval))

      return(y)
    },
    ignoreNULL = T
  )


  output$table <- renderTable({
    yy <- xtab()
    yy[1:10, ]
  })


  output$download_btn <- downloadHandler(
    filename <- function() {
      paste0("MDIVE_", paste0(input$ev_down, collapse = "_"), ".csv")
    },
    content = function(file) {
      write.csv(xtab(), file, row.names = F)
    }
  )
}
