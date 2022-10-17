
get_choice_adm1 <- function(ctry_list) {
  ctry_str <- adm_master %>%
    filter(geo_id %in% ctry_list) %>%
    pull(country)

  y <- adm_master %>%
    filter((aggregation_level == 1) & (country %in% ctry_str)) %>%
    arrange(country, admin_level_1)
  n <- length(ctry_list)
  if (n == 1) {
    ychoice <- unlist(map2(
      y$admin_level_1, as.character(y$geo_id),
      ~ {
        names(.y) <- .x
        .y
      }
    ))
  } else if (n > 1) {
    anames <- split(y$admin_level_1, y$country)
    avals <- split(as.character(y$geo_id), y$country)
    ychoice <- map2(anames, avals, ~ {
      names(.y) <- .x
      .y
    })
  } else {
    ychoice <- NULL
  }
  return(list(choice = ychoice, selected = as.character(y$geo_id)))
}


get_choice_adm2 <- function(ctry_list, adm1_list) {
  if (is.null(adm1_list) | (length(adm1_list) == 0) | all(tolower(adm1_list) == "none")) {
    geo_tmp <- adm_master %>% filter(geo_id %in% ctry_list)
    adm1_list <- adm_master %>% filter((country %in% geo_tmp$country) & (aggregation_level == 1))
  }

  area_str <- adm_master %>%
    filter(geo_id %in% adm1_list) %>%
    select(country, admin_level_1)

  y <- adm_master %>%
    filter((aggregation_level == 2) & (country %in% unique(area_str$country)) & (admin_level_1 %in% area_str$admin_level_1)) %>%
    arrange(country, admin_level_1, admin_level_2)

  nctry <- length(ctry_list)

  if (nctry == 1) {
    anames <- split(y$admin_level_2, y$admin_level_1)
    avals <- split(as.character(y$geo_id), y$admin_level_1)
    ychoice <- map2(anames, avals, ~ {
      names(.y) <- .x
      .y
    })
  } else if (nctry > 1) {
    y <- y %>%
      mutate(adm2head = paste(country, admin_level_1, sep = " | "))
    anames <- split(y$admin_level_2, y$adm2head)
    avals <- split(as.character(y$geo_id), y$adm2head)
    ychoice <- map2(anames, avals, ~ {
      names(.y) <- .x
      .y
    })
  } else {
    ychoice <- NULL
  }
  return(list(choice = ychoice, selected = as.character(y$geo_id)))
}




# Testing function --------------------------
# cc <- x0 %>% filter(aggregation_level == 0) %>% select(geo_id, country) %>% distinct()
# gid <- unique(cc$geo_id)
# # gid <- adm_master %>% filter((country == "Angola") & (aggregation_level == 1)) %>% pull(geo_id)
# ev <- "rf"; # yrs = c(2021, 2022)
# agg <- 1; epi <- NULL
# xu <- get_data_main(adm_res = "admin0", gid, gid1 = NULL, gid2 = NULL, ev = ev)


get_data_main <- function(df, adm_res, gid0, gid1, gid2, ev, epi = NULL, userid) {
  if (adm_res == "admin0") {
    gid <- gid0
    agg <- 0
  } else if (adm_res == "admin1") {
    gid <- gid1
    agg <- 1
  } else {
    gid <- gid2
    agg <- 2
  }
  
  # query <- paste0(
  #   "SELECT * FROM staging_pmihq.climate_app WHERE ",
  #   "geo_id IN ('", paste0(gid, collapse = "', '"), "') AND ",
  #   "variable_name = '", ev, "' AND aggregation_level = ", agg
  # )
  #
  # y <- read_civis(sql(query),
  #   database = "PMI"
  # ) %>%
  #   mutate(date = as.Date(as.character(date)))
  
  y <- df %>%
    filter(
      geo_id %in% gid,
      variable_name == ev,
      aggregation_level == agg
    )

  n0 <- length(unique(y$country))
  n1 <- length(unique(y$admin_level_1))
  
  if ((n0 == 1) & (n1 <= 1)) {
    y$adm_label <- y$adm_label_tmp
  } else if ((n0 == 1) & (n1 > 1)) {
    if (agg == 1) {
      y$adm_label <- y$adm_label_tmp
    } else {
      y$adm_label <- y$adm_label12
    }
  } else if ((n0 > 1)) {
    if (agg == 0) {
      y$adm_label <- y$adm_label_tmp
    } else if (agg == 1) {
      y$adm_label <- y$adm_label01
    } else {
      y$adm_label <- y$adm_label012
    }
  }
  
  y <- y %>% 
    select(-c(adm_label_tmp, adm_label01, adm_label012, adm_label12))
  out <- list(df = y, agg = agg, ev = ev, epi = epi)

  return(out)
}



# Plot core function (1 plot)

plot_ev_ssn <- function(u, show_legend = T, ymax_val, ev_par, yrs, pcol, prow, epi_par = NULL, nth_plot = NULL) {
  ev_val <- paste0("%{y:.2f} ", ev_par$unit)
  pmain <- unique(u$adm_label)

  yrs <- sort(yrs)
  nyrs <- length(yrs)
  clr_yrs <- rev(rev(clr_seq)[1:nyrs])

  d0 <- min(u$date, na.rm = T) - 5
  dF <- max(u$date, na.rm = T) + 5

  w <- ifelse(pcol <= 3, 250, 300)

  p <- plot_ly(data = u, height = 250 * prow, width = w * pcol) %>%
    add_ribbons(
      ymin = ~minval, ymax = ~maxval, x = ~date, line = list(color = clr_bar_grey), fillcolor = clr_bar_grey,
      opacity = 0.2, name = "Historical data (2000 to 2021)",
      hovertemplate = "Historical Range (2000 to 2021)  <extra></extra>",
      showlegend = F, yaxis = "y"
    ) %>%
    add_trace(
      x = ~date, y = ~minval, type = "scatter", mode = "lines", line = list(color = clr_bar_grey, width = 1),
      name = "Historical minimum", hovertemplate = paste0("Hist. Min: ", ev_val, " <extra></extra>"), showlegend = show_legend, yaxis = "y"
    ) %>%
    add_trace(
      x = ~date, y = ~monthly_ave, type = "scatter", mode = "lines", line = list(color = clr_bar_grey, width = 2, dash = "dot"),
      name = "Historical Average", hovertemplate = paste0("Hist. Ave: ", ev_val, "  <extra></extra>"), showlegend = show_legend, yaxis = "y"
    ) %>%
    add_trace(
      x = ~date, y = ~maxval, type = "scatter", mode = "lines", line = list(color = clr_bar_grey, width = 1),
      name = "Historical maximum", hovertemplate = paste0("Hist. Max: ", ev_val, " <extra></extra>"), showlegend = show_legend, yaxis = "y"
    )

  for (i in 1:nyrs) {
    yname <- paste0("clim_t", i)
    clri <- clr_yrs[i]

    dfi <- u %>%
      select(date, all_of(yname)) %>%
      rename(var_y = yname)

    # if (i == 1) {
    #   hovtemp <- paste0("<b>", epi_par$label, "</b><br>", yrs[i], ": ", ev_val," <extra></extra>")
    # } else {
    #   hovtemp <- paste0(yrs[i], ": ", ev_val," <extra></extra>")
    # }

    p <- add_trace(p,
      x = ~date, y = ~var_y, data = dfi, type = "scatter", mode = "lines",
      line = list(color = clri, width = 2), name = as.character(yrs[i]),
      hovertemplate = paste0(yrs[i], ": ", ev_val, " <extra></extra>"), showlegend = show_legend, yaxis = "y"
    )
  }



  if (!is.null(epi_par)) {
    clr_epi_yrs <- rev(rev(clr_epi_seq)[1:nyrs])

    if (epi_par$label == "TPR") {
      epi_val <- paste0("%{y:.2f} ", epi_par$unit)
    } else {
      epi_val <- paste0("%{y:d} ", epi_par$unit)
    }


    for (i in 1:nyrs) {
      yname <- paste0("epi_t", i)
      clri <- clr_epi_yrs[i]

      dei <- u %>%
        select(date, all_of(yname)) %>%
        rename(var_y = yname)

      p <- add_trace(p,
        x = ~date, y = ~var_y, data = dei, type = "scatter", mode = "lines+markers",
        line = list(color = clri, width = 2), marker = list(color = clri, size = 10), name = as.character(yrs[i]),
        hovertemplate = paste0(yrs[i], ": ", epi_val, " <extra></extra>"), showlegend = show_legend, yaxis = "y2"
      )
    }

    tick2 <- pretty(epi_par$minaxs:epi_par$epi_max)

    tdf <- data.frame(xpos = max(u$date), ypos = tick2, text = label_number(scale_cut = cut_short_scale())(tick2))

    p <- add_trace(p,
      x = ~xpos, y = ~ypos, text = ~text, type = "scatter", mode = "text",
      textposition = "middle left", data = tdf, yaxis = "y2",
      hovertemplate = "<span style='color:white'> </span><extra></extra>",
      textfont = list(size = 8)
    )
  }


  p <- p %>%
    layout(
      autosize = F,
      annotations = list(
        text = paste0("<b>", pmain, "</b>"),
        xref = "paper", yref = "paper",
        yanchor = "bottom", xanchor = "left",
        align = "center",
        x = 0.05, y = 0.98,
        font = list(size = 11), showarrow = F
      ),
      xaxis = list(
        title = "",
        tickangle = 0,
        dtick = "M1", tickformat = "%b",
        ticks = "outside", ticklen = 4,
        tickfont = list(size = 8),
        ticklabelstep = 3,
        showgrid = F,
        range = c(d0, dF),
        # margin = list(pad = -20),
        hoverformat = "<b>%b<b>",
        hoverlabel = list(font = list(weight = 800))
      ),
      yaxis = list(
        title = paste0(ev_par$label, " (", ev_par$unit, ")"),
        # margin = list(pad = 10),
        range = list(ev_par$minaxs, 1.05 * ymax_val),
        # position = t0+20,
        tickfont = list(size = 8)
      ),
      hovermode = "x unified", hoverdistance = 1
    )


  if (!is.null(epi_par)) {
    kaxs <- paste0("y", ifelse(nth_plot == 1, "", as.numeric((2 * nth_plot) - 1)))

    p <- p %>%
      layout(yaxis2 = list(
        side = "right", overlaying = kaxs, range = list(0, 1.1 * epi_par$epi_max), showgrid = F,
        showticklabels = F
      ))
  }


  (p)
}



# plot_ev_ssn <- function(u, show_legend = T, ymax_val, ev_par, yrs, pcol, prow) {
#
#   ev_val <- paste0("%{y:.2f} ", ev_par$unit)
#   pmain <- unique(u$adm_label)
#
#   yrs <- sort(yrs)
#   nyrs <- length(yrs)
#   clr_yrs <- rev(rev(clr_seq)[1:nyrs])
#
#   w <- ifelse(pcol<=3, 250, 300)
#
#   p <- plot_ly(data = u, height = 250 * prow, width = w * pcol) %>%
#     add_ribbons(ymin = ~ minval, ymax = ~ maxval, x = ~ date, line = list(color = clr_bar_grey), fillcolor = clr_bar_grey,
#                 opacity = 0.2, name = "Historical data (2000 to 2021)", hoverinfo = "none", showlegend = F) %>%
#     add_trace(x = ~ date, y = ~ minval, type = "scatter", mode = "lines", line = list(color = clr_bar_grey, width = 1),
#               name = "Historical minimum", hovertemplate = paste0("Hist. Min: ", ev_val, " <extra></extra>"), showlegend = show_legend) %>%
#     add_trace(x = ~ date, y = ~ monthly_ave, type = 'scatter', mode = "lines", line = list(color = clr_bar_grey, width = 2, dash = "dot"),
#               name = "Historical Average", hovertemplate = paste0("Hist. Ave: ", ev_val,"  <extra></extra>"), showlegend = show_legend) %>%
#     add_trace(x = ~ date, y = ~ maxval, type = "scatter", mode = "lines", line = list(color = clr_bar_grey, width = 1),
#               name = "Historical maximum", hovertemplate = paste0("Hist. Max: ", ev_val, " <extra></extra>"), showlegend = show_legend)
#
#   for(i in 1:nyrs) {
#
#     yname <- paste0("clim_t",i)
#     clri <- clr_yrs[i]
#
#     dfi <- u %>% select(date, all_of(yname)) %>%
#       rename(var_y = yname)
#
#     p <- add_trace(p, x = ~ date, y = ~ var_y, data = dfi, type = "scatter", mode = "lines",
#                    line = list(color = clri, width = 2),  name = as.character(yrs[i]),
#                 hovertemplate = paste0(yrs[i], ": ", ev_val," <extra></extra>"), showlegend = show_legend)
#   }
#
#
#   p <- p %>%
#     layout(
#       autosize = F,
#       annotations = list(text = paste0("<b>", pmain, "</b>"),
#                          xref = "paper", yref = "paper",
#                          yanchor = "bottom", xanchor = "left",
#                          align = "center",
#                          x = 0.05, y = 0.98,
#                          font = list(size = 10), showarrow = F),
#       xaxis = list(
#         title = "",
#         tickangle = -45,
#         dtick = "M1", tickformat = "%b",
#         ticks = "outside", ticklen = 4,
#         tickfont = list(size = 8),
#         showgrid = F,
#         # margin = list(pad = -20),
#         hoverformat = "<b>%b<b>",
#         hoverlabel = list(font = list(weight = 800))
#       ),
#       yaxis = list(
#         title = paste0(ev_par$label, " (", ev_par$unit, ")"),
#         # margin = list(pad = 10),
#         range =  list(0, ymax_val),
#         tickfont = list(size = 8)
#       ),
#       hovermode = "x unified"
#     )
#
#
#   (p)
#
# }


get_ev_par <- function(ev) {
  if (grepl("^rf", ev)) {
    y <- list(unit = "mm", label = "Rainfall", maxround = 25, minaxs = 0)
  } else if (grepl("lstd", ev)) {
    y <- list(unit = "\u00B0C", label = "Land Surface Temp.", maxround = 5, minaxs = -12)
  } else if (grepl("tair", ev)) {
    y <- list(unit = "\u00B0C", label = "Air Temp.", maxround = 5, minaxs = 0)
  } else if (grepl("ndvi", ev)) {
    y <- list(unit = "unitless", label = "NDVI", maxround = 0.2, minaxs = -0.2)
  } else if (grepl("sh", ev)) {
    y <- list(unit = "g/kg", label = "Specific Humidity", maxround = 2, minaxs = 0)
  } else if (grepl("sm")) {
    y <- list(unit = "kg/m^2", label = "Soil Moisture", maxround = 2, minaxs = 0)
  } else {
    y <- list(unit = "unknown variable", label = "Unknown variable", maxround = 1, minaxs = 1)
  }

  return(y)
}



get_epi_par <- function(epi) {
  if (grepl("suspected", epi)) {
    y <- list(unit = ifelse(grepl("rate", epi), "cases/100K", "cases"), label = "Suspected Cases", maxround = 20, minaxs = 0)
  } else if (grepl("confirmed", epi)) {
    y <- list(unit = ifelse(grepl("rate", epi), "cases/100K", "cases"), label = "Confirmed Cases", maxround = 20, minaxs = 0)
  } else if (grepl("severe", epi)) {
    y <- list(unit = ifelse(grepl("rate", epi), "cases/100K", "cases"), label = "Severe Cases", maxround = 20, minaxs = 0)
  } else if (grepl("tested", epi)) {
    y <- list(unit = ifelse(grepl("rate", epi), "cases/100K", "cases"), label = "Tested Cases", maxround = 20, minaxs = 0)
  } else if (grepl("deaths", epi)) {
    y <- list(unit = ifelse(grepl("rate", epi), "deaths/1M", "deaths"), label = "Malaria Deaths", maxround = 5, minaxs = 0)
  } else if (grepl("tpr", epi)) {
    y <- list(unit = "", label = "TPR", maxround = 0.2, minaxs = 0)
  } else {
    y <- list(unit = "unknown variable", label = "Unknown variable", maxround = 1, minaxs = 0)
  }

  if (grepl("deaths", epi)) {
    y$label <- "Malaria Deaths per 1M"
  } else if (grepl("rate", epi)) {
    y$label <- paste0(y$label, " per 100K")
  }

  return(y)
}


get_dummy <- function(u, ev = NULL, epi = NULL, agg, yrs = c(2021, 2022), ssn = "cy", yax_std = F, ssize = c(1200, 950)) {

  # if(is.null(ev)) {ev = unique(xdf$variable_name)}

  ev_par <- get_ev_par(ev = ev)

  # Set the order of month for plotting + month labeling
  if (ssn == "cy") {
    u$yr2use <- u$year
    u$mo2use <- u$month
    mo_label <- month.abb
    u <- u %>%
      left_join(., mo_lookup %>% select(month, date_cy), by = "month") %>%
      rename(date4pl = date_cy)
  } else {
    u$yr2use <- u$yr_ssn
    mo2use <- u$mo_ssn
    mo_label <- month.abb[mo_lookup$month_ssn]
    u <- u %>%
      left_join(., mo_lookup %>% select(month, date_ssn), by = "month") %>%
      rename(date4pl = date_ssn)
  }

  if (is.null(yrs)) {
    yrs <- unique(u$yr2use)
  }

  adm_grp <- switch(as.character(agg),
    "1" = "admin_level_1",
    "2" = "admin_level_2",
    "0" = "country"
  )
  adm_grp <- u %>%
    select(geo_id, country, admin_level_1, admin_level_2) %>%
    distinct() %>%
    arrange(country, admin_level_1, admin_level_2) %>%
    pull(geo_id)

  tt <- data.frame(tid = 1:length(yrs), yr2use = sort(yrs))

  u1 <- u %>%
    filter(yr2use %in% tt$yr2use) %>%
    select(geo_id, country, admin_level_1, admin_level_2, adm_label, aggregation_level, date, date4pl, month, yr2use, value, monthly_ave, minval, maxval, all_of(epi)) %>%
    left_join(., tt, by = "yr2use") %>%
    mutate(name = paste0("clim_t", tid)) %>%
    pivot_wider(
      values_from = c(value, all_of(epi)), names_from = name,
      id_cols = c(geo_id, country, admin_level_1, admin_level_2, adm_label, date4pl, month, monthly_ave, minval, maxval)
    ) %>%
    rename(date = date4pl)

  if (!is.null(epi)) {
    u1 <- u1 %>%
      rename_with(., ~ gsub("value\\_", "", .x)) %>%
      rename_with(., ~ gsub(paste0(epi, "_clim"), "epi", .x))
  }
  return(u1)
}


get_plot_ssn <- function(u, ev = NULL, epi = NULL, agg, yrs = c(2021, 2022), ssn = "cy", yax_std = F, ssize = c(1200, 950), order = "alpha") {

  # if(is.null(ev)) {ev = unique(xdf$variable_name)}

  ev_par <- get_ev_par(ev = ev)

  # Set the order of month for plotting + month labeling
  if (ssn == "cy") {
    u$yr2use <- u$year
    u$mo2use <- u$month
    mo_label <- month.abb
    u <- u %>%
      left_join(., mo_lookup %>% select(month, date_cy), by = "month") %>%
      rename(date4pl = date_cy)
  } else {
    u$yr2use <- u$yr_ssn
    mo2use <- u$mo_ssn
    mo_label <- month.abb[mo_lookup$month_ssn]
    u <- u %>%
      left_join(., mo_lookup %>% select(month, date_ssn), by = "month") %>%
      rename(date4pl = date_ssn)
  }

  if (is.null(yrs)) {
    yrs <- unique(u$yr2use)
  }

  # adm_grp <- switch(as.character(agg), '1' = 'admin_level_1', '2' = 'admin_level_2', '0' = 'country')
  adm_grp <- u %>%
    select(geo_id, country, admin_level_1, admin_level_2) %>%
    distinct() %>%
    arrange(country, admin_level_1, admin_level_2) %>%
    pull(geo_id)

  tt <- data.frame(tid = 1:length(yrs), yr2use = sort(yrs))

  u1 <- u %>%
    filter(yr2use %in% tt$yr2use) %>%
    select(geo_id, country, admin_level_1, admin_level_2, adm_label, aggregation_level, date, date4pl, month, yr2use, value, monthly_ave, minval, maxval, all_of(epi)) %>%
    left_join(., tt, by = "yr2use") %>%
    mutate(name = paste0("clim_t", tid)) %>%
    pivot_wider(
      values_from = c(value, all_of(epi)), names_from = name,
      id_cols = c(geo_id, country, admin_level_1, admin_level_2, adm_label, date4pl, month, monthly_ave, minval, maxval)
    ) %>%
    rename(date = date4pl)

  if (!is.null(epi)) {
    u1 <- u1 %>%
      rename_with(., ~ gsub("value\\_", "", .x)) %>%
      rename_with(., ~ gsub(paste0(epi, "_clim"), "epi", .x))
  }


  ugrp <- split(u1, u1$geo_id)


  if (order == "alpha") {
    alpha_order <- u1 %>%
      select(geo_id, country, admin_level_1, admin_level_2) %>%
      distinct() %>%
      arrange(country, admin_level_1, admin_level_2)

    ugrp <- ugrp[c(as.character(alpha_order$geo_id))]
  } else {
    ll_order <- u1 %>%
      select(geo_id) %>%
      distinct() %>%
      left_join(., ctrll) %>%
      arrange(lat)
    ugrp <- ugrp[rev(c(as.character(ll_order$geo_id)))]
  }



  ncols <- ifelse(ssize[1] < 980, 3, 5)

  nrows <- as.integer(ceiling((length(adm_grp)) / ncols))


  pp <- list()

  for (i in 1:length(ugrp)) {
    uin <- ugrp[[i]] %>% arrange(date)
    show_legend <- ifelse(i == 1, TRUE, FALSE)

    if (!is.null(epi)) {
      epi_par <- get_epi_par(epi)
      nth_plot <- i
    } else {
      epi_par <- nth_plot <- NULL
    }


    if (yax_std) {
      ymax_val <- plyr::round_any(max(u1[, c("minval", "maxval", paste0("clim_t", 1:(nrow(tt))))], na.rm = TRUE), ev_par$maxround, f = ceiling)
      if (!is.null(epi)) {
        epi_max <- plyr::round_any(max(u1[, c(paste0("epi_t", 1:(nrow(tt))))], na.rm = TRUE), epi_par$maxround, f = ceiling)
        epi_max <- ifelse(is.infinite(epi_max), 1, epi_max)
        epi_par$epi_max <- epi_max
      }
    } else {
      ymax_val <- plyr::round_any(max(uin[, c("minval", "maxval", paste0("clim_t", 1:(nrow(tt))))], na.rm = TRUE), ev_par$maxround, f = ceiling)
      if (!is.null(epi)) {
        epi_max <- plyr::round_any(max(uin[, c(paste0("epi_t", 1:(nrow(tt))))], na.rm = TRUE), epi_par$maxround, f = ceiling)
        epi_max <- ifelse(is.infinite(epi_max), 1, epi_max)
        epi_par$epi_max <- epi_max
      }
    }




    pp[[i]] <- plot_ev_ssn(
      u = uin, show_legend = show_legend, ymax_val = ymax_val, ev_par = ev_par,
      yrs = yrs, pcol = ncols, prow = nrows, epi_par = epi_par, nth_plot = nth_plot
    )
  }

  ncols <- min(5, length(pp))
  subplot(pp,
    nrows = nrows, # , margin = c(0.03, 0.03, 0.03, 0.03)#,
    widths = rep(signif(0.98 / 5, digits = 3), times = ncols),
    heights = rep(signif(0.98 / nrows, digits = 5), times = nrows)
  ) %>%
    layout(showlegend = F)
}



# Historical Tab -----------------------------------------------------------------------------------


## > plot 1 historical -----------------------------------------------------------------------------



plot_ev_histo <- function(u, show_legend = T, ymax_val, ev_par, pcol, prow, epi_par = NULL, nth_plot = NULL) {
  ev_val <- paste0("%{y:.2f} ", ev_par$unit)
  pmain <- unique(u$adm_label)

  d0 <- min(u$date, na.rm = T) - 5
  dF <- max(u$date, na.rm = T) + 5

  w <- ifelse(pcol <= 2, 400, 400)

  p <- plot_ly(data = u, height = 300 * prow, width = w * pcol) %>%
    add_ribbons(
      ymin = ~minval, ymax = ~maxval, x = ~date, line = list(color = clr_bar_grey), fillcolor = clr_bar_grey,
      opacity = 0.2, name = "Historical data (2000 to 2021)", showlegend = F, yaxis = "y",
      hovertemplate = "Historical Range (2000 to 2021)  <extra></extra>"
    ) %>%
    add_trace(
      x = ~date, y = ~minval, type = "scatter", mode = "lines", line = list(color = clr_bar_grey, width = 1),
      name = "Historical minimum", hovertemplate = paste0("Hist. Min: ", ev_val, " <extra></extra>"), showlegend = show_legend, yaxis = "y"
    ) %>%
    add_trace(
      x = ~date, y = ~monthly_ave, type = "scatter", mode = "lines", line = list(color = clr_bar_grey, width = 2, dash = "dot"),
      name = "Historical Average", hovertemplate = paste0("Hist. Ave: ", ev_val, "  <extra></extra>"), showlegend = show_legend, yaxis = "y"
    ) %>%
    add_trace(
      x = ~date, y = ~maxval, type = "scatter", mode = "lines", line = list(color = clr_bar_grey, width = 1),
      name = "Historical maximum", hovertemplate = paste0("Hist. Max: ", ev_val, " <extra></extra>"), showlegend = show_legend, yaxis = "y"
    ) %>%
    add_trace(
      x = ~date, y = ~value, type = "scatter", mode = "lines", line = list(color = clr_red, width = 1),
      name = "Current Month", hovertemplate = paste0("Current Month: ", ev_val, " <extra></extra>"), showlegend = show_legend, yaxis = "y"
    )

  if (!is.null(epi_par)) {
    tick2 <- pretty(epi_par$minaxs:epi_par$epi_max)

    tdf <- data.frame(xpos = max(u$date), ypos = tick2, text = label_number(scale_cut = cut_short_scale())(tick2))

    if (epi_par$label == "TPR") {
      epi_val <- paste0("%{y:.2f} ", epi_par$unit)
    } else {
      epi_val <- paste0("%{y:d} ", epi_par$unit)
    }



    p <- add_trace(p,
      x = ~date, y = ~value_epi, type = "scatter", mode = "lines+markers",
      line = list(color = clr_epi1, width = 1), marker = list(color = clr_epi1, size = 7),
      name = epi_par$label, hovertemplate = paste0(epi_par$label, ": ", epi_val, "<extra></extra>"),
      showlegend = show_legend, yaxis = "y2"
    )
    p <- add_trace(p,
      x = ~xpos, y = ~ypos, text = ~text, type = "scatter", mode = "text",
      textposition = "middle left", data = tdf, yaxis = "y2",
      hovertemplate = "<span style='color:white'> </span><extra></extra>",
      textfont = list(size = 8)
    )
  }


  p <- p %>%
    layout(
      autosize = F,
      annotations = list(
        text = paste0("<b>", pmain, "</b>"),
        xref = "paper", yref = "paper",
        yanchor = "bottom", xanchor = "left",
        align = "center",
        x = 0.05, y = 0.98,
        font = list(size = 10), showarrow = F
      ),
      xaxis = list(
        title = "",
        tickangle = 0,
        dtick = "M6", tickformat = "%b<br>%Y",
        ticks = "outside", ticklen = 4,
        tickfont = list(size = 8),
        showgrid = F,
        range = c(d0, dF),
        # margin = list(pad = -20),
        hoverformat = "<b>%b-%Y<b>",
        hoverlabel = list(font = list(weight = 800))
      ),
      yaxis = list(
        title = paste0(ev_par$label, " (", ev_par$unit, ")"),
        # margin = list(pad = 10),
        range = list(ev_par$minaxs, 1.05 * ymax_val),
        # position = t0,
        tickfont = list(size = 8)
      ),
      hovermode = "x unified", hoverdistance = 1
    )

  if (!is.null(epi_par)) {
    kaxs <- paste0("y", ifelse(nth_plot == 1, "", as.numeric((2 * nth_plot) - 1)))
    p <- p %>%
      layout(yaxis2 = list(side = "right", overlaying = kaxs, range = list(0, 1.1 * epi_par$epi_max), showgrid = F, showticklabels = F))
  }


  (p)
}



## > wrapper to plot historical --------------------------------------------------------------------

get_plot_histo <- function(u, ev = NULL, epi = NULL, agg, date_range, ssn = "cy", yax_std = F, ssize = c(1200, 950)) {
  ev_par <- get_ev_par(ev = ev)

  adm_grp <- u %>%
    select(geo_id, country, admin_level_1, admin_level_2) %>%
    distinct() %>%
    arrange(country, admin_level_1, admin_level_2) %>%
    pull(geo_id)


  u1 <- u %>%
    select(geo_id, country, admin_level_1, admin_level_2, adm_label, aggregation_level, date, month, value, monthly_ave, minval, maxval, all_of(epi))


  if (!is.null(epi)) {
    u1 <- u1 %>% rename(value_epi = epi)
    epi_par <- get_epi_par(epi)
  } else {
    epi_par <- NULL
  }


  ugrp <- split(u1, u1$geo_id)

  alpha_order <- u1 %>%
    select(geo_id, country, admin_level_1, admin_level_2) %>%
    distinct() %>%
    arrange(country, admin_level_1, admin_level_2)

  ugrp <- ugrp[c(as.character(alpha_order$geo_id))]

  ncols <- ifelse(ssize[1] < 980, 2, 4)

  nrows <- ceiling(length(adm_grp) / ncols)


  pp <- list()

  nth_plot <- NULL

  for (i in 1:length(ugrp)) {
    uin <- ugrp[[i]] %>% arrange(date)
    show_legend <- ifelse(i == 1, TRUE, FALSE)

    if (yax_std) {
      ymax_val <- plyr::round_any(max(u1[, c("minval", "maxval", "value")], na.rm = TRUE), ev_par$maxround, f = ceiling)
      if (!is.null(epi)) {
        epi_max <- plyr::round_any(max(u1[, "value_epi"], na.rm = T), epi_par$maxround, f = ceiling)
        epi_max <- ifelse(is.infinite(epi_max), 1, epi_max)
        epi_par$epi_max <- epi_max
        nth_plot <- i
      }
    } else {
      ymax_val <- plyr::round_any(max(uin[, c("minval", "maxval", "value")], na.rm = TRUE), ev_par$maxround, f = ceiling)
      if (!is.null(epi)) {
        epi_max <- plyr::round_any(max(uin[, "value_epi"], na.rm = T), epi_par$maxround, f = ceiling)
        epi_max <- ifelse(is.infinite(epi_max), 1, epi_max)
        epi_par$epi_max <- epi_max
        nth_plot <- i
      }
    }

    pp[[i]] <- plot_ev_histo(
      u = uin, show_legend = show_legend, ymax_val = ymax_val, ev_par = ev_par, pcol = ncols, prow = nrows,
      epi_par = epi_par, nth_plot = nth_plot
    )
  }


  subplot(pp,
    nrows = nrows, # , margin = c(0.03, 0.03, 0.03, 0.03)#,
    widths = rep(signif(0.98 / ncols, digits = 3), ncols), heights = rep(signif(0.98 / nrows, digits = 5), nrows)
  ) %>%
    layout(showlegend = F)
}
