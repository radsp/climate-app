server <-function(input, output, session) {
  
  # Prevent "greying out" when running in Civis Platform
  observe(input$alive_count)
  session$allowReconnect("force")
  
  observeEvent(input$showSidebar, {
    shinyjs::show(id = "Sidebar")
    shinyjs::hide(id = "side_expand")
  })
  
  observeEvent(input$hide_sidebar, {
    shinyjs::hide(id = "Sidebar")
    shinyjs::show(id = "side_expand")
    
  })
  
  # 
  # 
  # # output$myplot <- renderPlot({plot(runif(100), runif(100))})
  # 
  # 
  # # Run model and get report data output ------------------------------
  # 
  # # run_model <- eventReactive(input$run_model, {
  # 
  # out_report <- reactiveValues(data = NULL)
  # 
  # observeEvent(input$run_model, {
  #   
  #   # show_modal_spinner(text = "Calculating ....... this may take several minutes")
  #   
  #   pfm_env_var <- tibble(environ_var_code = input$env_indi)
  #   
  #   pfm_report_settings <- 
  #     epidemiar::create_named_list(
  #       report_period,
  #       report_value_type,
  #       report_inc_per,
  #       epi_date_type,
  #       epi_interpolate,
  #       epi_transform,
  #       model_run,
  #       env_var = pfm_env_var,
  #       env_lag_length = as.numeric(input$env_lag_length) * 30,
  #       env_anomalies =input$env_anomalies,
  #       fc_splines = input$fc_splines,
  #       fc_cyclicals = input$fc_cyclicals,
  #       fc_future_period = as.numeric(input$fc_future_period) * 4,
  #       fc_clusters = pfm_fc_clusters,
  #       fc_ncores,
  #       ed_summary_period,
  #       ed_method,
  #       ed_control = pfm_ed_control)
  #   
  #   out_report$data <- run_epidemia(
  #                     #data
  #                     epi_data = epi_data, 
  #                     env_data = env_data, 
  #                     env_ref_data = env_ref_data, 
  #                     env_info = env_info,
  #                     #fields
  #                     casefield = test_pf_tot, 
  #                     groupfield = woreda_name, 
  #                     populationfield = pop_at_risk,
  #                     obsfield = environ_var_code, 
  #                     valuefield = obs_value,
  #                     #required settings
  #                     fc_model_family = input$fc_model_family,
  #                     #other settings
  #                     report_settings = pfm_report_settings)
  #   
  #   # return(out_report)
  #   
  #   # remove_modal_spinner()
  #   
  # })
  # 
  # # Generate result header  ------------------------------------------
  # 
  # output$result_header <- renderText({
  #   if(!is.null(out_report$data)) {
  #     HTML("<h2><b>Malaria Early Warning Alert</b></h2><br>
  #          <p>Early Warning Alerts are alerts generated during the forecast period, and are based on the 
  #          relationship between historical incidence values and environmental conditions. Alerts occur when
  #          this relationship shows a forecasted value greater than alert threshold.</p><hr>
  #          <h4><b>Alert Map</b></h4><br>")
  #   } else {
  #     NULL
  #   }
  # })
  # 
  # 
  # # Generate time series header ---------------------------------------
  # 
  # output$timeseries_txt <- renderUI({
  #   if (!is.null(out_report$data)) {
  # 
  #       HTML("<h4><b>Malaria forecasts and alerts time series</b></h4><br>")
  # 
  #   } else {
  #     NULL
  #   }
  # })
  # 
  # # Map generation ----------------------------------------------------
  # 
  # output$out_map <- renderLeaflet({
  #   if(!is.null(out_report$data)) {
  #     y <- out_report$data
  #     
  #     #woredas used in report data
  #     report_woreda_names <- y$params_meta$groupings
  #     
  #     #woreda info on used woredas
  #     report_woredas <- tibble(report_woreda_names) %>% 
  #       left_join(woredas %>% select(woreda_name, WID, zone),
  #                 by = c("report_woreda_names" = "woreda_name")) %>% 
  #       rename(woreda_name = report_woreda_names) %>% 
  #       arrange(zone, woreda_name)
  #     
  #     xindi <- "pfm"
  #     
  #     early_exists <- !is.na(y$params_meta$report_dates$ed_sum$min)
  #     
  #     summary_map_data <- y$summary_data %>%
  #       #get falciparum data
  #       mutate(respon_var = xindi) %>%
  #       #Hook into existing short labels for full labels to display in legend
  #       mutate(ed_legend = if(early_exists){
  #         factor(ed_sum_level, levels = levels(ed_sum_level), 
  #                labels = ed_overview_labels, ordered = TRUE)
  #       } else {
  #         #else set label levels from what they would have been
  #         factor(ed_sum_level, levels = c("Low", "Medium", "High"),
  #                labels = ed_overview_labels, ordered = TRUE)
  #       },
  #       ew_legend = factor(ew_level, levels = levels(ew_level),
  #                          labels = ew_overview_labels, ordered = TRUE),
  #       #reverse the factor order for plotting
  #       ed_legend = factor(ed_legend, levels = rev(levels(ed_legend))),
  #       ew_legend = factor(ew_legend, levels = rev(levels(ew_legend)))) %>%
  #       #get all woredas, get WID
  #       right_join(woredas %>% select(woreda_name, WID),
  #                  by = "woreda_name") %>%
  #       #get "No Data" instead of NA for non included woredas
  #       mutate_at(c("ed_legend", "ew_legend"), fct_explicit_na, na_level = "No Data")
  #     
  #     
  #     sfp <- left_join(s2_amh, summary_map_data, by = "WID")
  #     
  #     mymap <- leaflet(sfp) %>%
  #       setView(lng =  39.02104, lat = 11.99837, zoom = 9)  %>%
  #       addProviderTiles("CartoDB.Positron") %>%
  #       addPolygons(color = "lightgray", weight = 0.85, # smoothFactor = 0.5,
  #                   fillOpacity = 0.8,
  #                   fillColor = ~pal(as.character(sfp$ew_legend))) %>%
  #       addLegend(pal = pal, values = as.character(sfp$ew_legend), title = "Early Warning", opacity = 1)
  #     
  #     (mymap)
  #     
  #   }
  #   
  #   
  # })
  # 
  # 
  # 
  # # Time series graph ----------------------------------------------
  # 
  # output$woreda_select_input <- renderUI({
  #   
  #   y <- out_report$data
  #   
  #   if(!is.null(out_report$data)) {
  #     
  #     
  #     #woredas used in report data
  #     report_woreda_names <- as.list(y$params_meta$groupings)
  #     # names(report_woreda_names) <- paste("District ", 1:length(report_woreda_names), sep = "")
  #     
  #     HTML("District: ")
  #     selectInput("this_woreda", label = NULL, choices = report_woreda_names, 
  #                 selected = report_woreda_names[[1]])
  #   } else {
  #     NULL
  #   }
  #   
  # })
  # 
  # output$ts_output <- renderPlot({
  #   
  #   
  #   
  #   if(!is.null(out_report$data)) {
  #     y <- out_report$data
  #     
  #     woreda_axis_dates <- y$params_meta$report_dates$full$seq
  #     woreda_axis_weeks <- y$modeling_results_data %>% 
  #       select(obs_date, week_epidemiar) %>% 
  #       arrange(obs_date) %>% 
  #       unique() %>% 
  #       pull(week_epidemiar)
  #     
  #     early_exists <- !is.na(y$params_meta$report_dates$ed_sum$min)
  #     
  #     y4gg <- y$modeling_results_data %>%
  #       #recoding Early Detection Alert for graphing (NA to not show up for when not alert, and 0.01 to graph at bottom of chart)
  #       mutate(value = case_when(series == "ed" & value == 0 ~ NA_real_,
  #                                series == "ed" & value == 1 ~ 0.01,
  #                                TRUE ~ value),
  #              value = case_when(series == "ew" & value == 0 ~ NA_real_,
  #                                series == "ew" & value == 1 ~ 0.01,
  #                                TRUE ~ value)) %>% 
  #       # inner_join(tibble(respon_var = c("pfm", "pv"),
  #       #                    facet = c("italic(P.~falciparum)~and~mixed", "italic(P.~vivax)")),
  #       #             by = "") %>% 
  #       #extra spacing added as a hack to increase distance between items in legend
  #       #known issue in ggplot that margin doesn't do what it should
  #       #https://github.com/tidyverse/ggplot2/issues/1502
  #       # now appears to be fixed, so removing extra spaces
  #       mutate(lab = factor(lab,
  #                           levels = c("Early Detection Alert", "Early Warning Alert", "Alert Threshold", 
  #                                      "Forecast Trend", "Observed"),
  #                           #labels = c("Early Detection Alert ", "Early Warning Alert ", "Alert Threshold ", 
  #                           #           "Forecast Trend ", "Observed "),
  #                           labels = c("Early Detection Alert", "Early Warning Alert", "Alert Threshold", 
  #                                      "Forecast Trend", "Observed"),
  #                           #put in correct order
  #                           ordered = TRUE)) 
  #     
  #     this_woreda <- if_else(is.null(input$this_woreda), "District 1", input$this_woreda)
  #     this_modeling_data <- y4gg %>% filter(woreda_name == this_woreda) %>%
  #       mutate(lab = factor(lab,
  #                           levels = c("Early Detection Alert", "Early Warning Alert", "Alert Threshold", 
  #                                      "Forecast Trend", "Observed"),
  #                           #labels = c("Early Detection Alert ", "Early Warning Alert ", "Alert Threshold ", 
  #                           #           "Forecast Trend ", "Observed "),
  #                           labels = c("Early Detection Alert", "Early Warning Alert", "Alert Threshold", 
  #                                      "Forecast Trend", "Observed"),
  #                           #put in correct order
  #                           ordered = TRUE)) 
  #     
  #     control_chart <- this_modeling_data %>%
  #       #and plot
  #       ggplot(aes(x = obs_date, y = value, color = lab)) +
  #       # facet_wrap(~ facet, scales = "free_y", ncol = 1, 
  #       #            strip.position = "right", labeller = label_parsed) +
  #       #box for early detection
  #       {if (early_exists) annotate("rect", xmin = y$params_meta$report_dates$ed_sum$min - 2, 
  #                                   xmax = y$params_meta$report_dates$ed_sum$max + 2,
  #                                   ymin = -Inf, ymax = Inf, fill = "skyblue1", alpha = 0.1)} +
  #       #box for early warning
  #       annotate("rect", xmin = y$params_meta$report_dates$forecast$min - 2, 
  #                xmax = y$params_meta$report_dates$forecast$max + 2,
  #                ymin = -Inf, ymax = Inf, fill = "orchid", alpha = 0.1) +
  #       #label for box for early detection
  #       {if (early_exists) annotate("text", label = "Early Detection",
  #                                   x = mean(y$params_meta$report_dates$ed_sum$seq), 
  #                                   #x = params_meta$report_dates$ed_sum$max + 2, hjust = 1, #end of box, right justified
  #                                   y = Inf, vjust = "top",
  #                                   color = "steelblue", lineheight = 1, size = 3.1)} +
  #       #label for box for early warning
  #       annotate("text", label = "Early Warning",
  #                x = mean(y$params_meta$report_dates$forecast$seq), 
  #                #x = params_meta$report_dates$forecast$min - 2, hjust = 0, #beginning of box, left justified
  #                y = Inf, vjust = "top",
  #                color = "darkorchid4", lineheight = 1, size = 3.1) +
  #       #dashed line to indicate start of requested forecast period (or default week after known data)
  #       geom_vline(aes(xintercept = as.numeric(y$params_meta$report_dates$prev$max + 3.5)),
  #                  linetype = "dashed", color = "gray50") +
  #       #all the series 
  #       geom_line(aes(linetype = lab, size = lab)) +
  #       #    geom_ribbon(aes(ymin = lower, ymax = upper, fill = lab, alpha = lab), color = NA) +
  #       geom_point(aes(shape = lab, size = lab)) +
  #       scale_x_date(breaks = woreda_axis_dates, labels = woreda_axis_weeks, minor_breaks = NULL) +
  #       #attempt to pad top of chart to avoid crowding
  #       # scale_y_continuous(expand = c(0.2, 0), limits = c(0, NA)) +
  #       labs(y = "Incidence (per 1000)", x = "Week") +
  #       scale_color_manual(values = c("tomato", "tomato", "tomato3", "mediumorchid4", "black")) +
  #       #    scale_fill_manual(values = c(NA, NA, "thistle", NA)) +
  #       scale_linetype_manual(values = c(0, 0, 5, 0, 1)) +
  #       scale_shape_manual(values = c(17, 2, NA, 124, NA)) +
  #       scale_size_manual(values = c(2.5, 2.5, 0.5, 4, 0.5)) +
  #       scale_alpha_manual(values = c(0, 0, 0, 0.5, 0)) +
  #       # plot themes
  #       theme_gray(base_size = 18, base_family = "") +
  #       woreda_legend + 
  #       woreda_x_axis +
  #       woreda_panel_theme
  #     
  #     print(control_chart)
  #     
  #     
  #   }
  #   
  #  
  #   
  # })
  # 
  
  
  
  
}