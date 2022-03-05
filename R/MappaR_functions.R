viz_render_map <- function(map,
                           session,
                           input,
                           d,
                           kpi_spec,
                           first_map_redraw,
                           train_test_filter,
                           user_filter,
                           plot_postcode_area){
  # get the inputs
  PostcodeArea <- NULL
  PostcodeUnit <- NULL
  unit_plot <- NULL
  response <- input$response
  weight <- input$weight
  line_thickness <- input$MappaR_line_thickness
  label_size <- ifelse(input$MappaR_label_size==0,0, input$MappaR_label_size + 5)
  opacity <- input$MappaR_opacity
  hotspots <- input$MappaR_hotspots
  colour1 <- input$MappaR_colour1
  colour2 <- input$MappaR_colour2
  colour3 <- input$MappaR_colour3
  show_sectors <- input$MappaR_sectors
  label_style <- list('box-shadow' = '3px 3px rgba(0,0,0,0.25)','font-size' = '16px','border-color' = 'rgba(0,0,0,0.5)')
  # only proceed if inputs are not null
  if(!is.null(d) &
     !is.null(response) &
     !is.null(weight) &
     response %in% names(d) &
     weight %in% c('N', 'no weights', names(d)) &
     response != 'select feature'){
    # extract the postcode summaries
    rows_to_summarise <- which(train_test_filter*user_filter==1)
    area_summary <- NULL
    sector_summary <- NULL
    unit_summary <- NULL
    if('PostcodeArea' %in% names(d)){
      area_summary <- postcode_summary(d, rows_to_summarise, response, weight, 'PostcodeArea')
    }
    if(show_sectors & 'PostcodeSector' %in% names(d)){
      sector_summary <- postcode_summary(d, rows_to_summarise, response, weight, 'PostcodeSector')
      if('PostcodeUnit' %in% names(d)){
        unit_summary <- postcode_summary(d, rows_to_summarise, response, weight, 'PostcodeUnit')
      }
    }
    # merge the area_summary onto the shapefile
    if(!is.null(area_summary)){
      setDF(area_summary)
      if(weight=='no weights'){
        area_summary$area_plot <- area_summary[,3]
      } else {
        area_summary$area_plot <- area_summary[,3]/area_summary[,2]
      }
      areas_sf <- merge(x=uk_areas, y=area_summary, by = 'PostcodeArea', all.x = TRUE)
    }
    # merge the sector_summary onto the shapefile
    if(!is.null(sector_summary)){
      setDF(sector_summary)
      if(weight=='no weights'){
        sector_summary$area_plot <- sector_summary[,3]
      } else {
        sector_summary$sector_plot <- sector_summary[,3]/sector_summary[,2]
      }
      sectors_sf <- merge(x=uk_sectors, y=sector_summary, by = 'PostcodeSector', all.x = TRUE)
    }
    # merge the unit_summary onto the unit data.table
    if(!is.null(unit_summary)){
      setkey(unit_summary, PostcodeUnit)
      if(nrow(unit_summary)>100000){
        units_sf <- unit_summary[uk_units[substr(PostcodeUnit,1,regexpr('[0-9]', PostcodeUnit)-1)==plot_postcode_area]]
      } else {
        units_sf <- unit_summary[uk_units]
      }
      setnames(units_sf, c('PostcodeUnit','weight','response','X','Y'))
      units_sf <- units_sf[!is.na(weight),]
      if(weight=='no weights'){
        units_sf[, unit_plot := response]
      } else {
        units_sf[, unit_plot := response/weight]
      }
    }
    # show labels
    if(label_size==0){
      show_area_labels <- FALSE
    } else {
      show_area_labels <- TRUE
    }
    # clear map
    m <- map %>%
      leaflet::leafletProxy() %>%
      leaflet::clearShapes() %>%
      leaflet::clearMarkers() %>%
      leaflet::clearControls()
    # area bins, labels and opacity
    if(!is.null(area_summary)){
      bins_area <- unique(stats::quantile(round(area_summary$area_plot,6), na.rm = TRUE, probs = 0:20/20))
      bins_area[1] <- bins_area[1] - 0.000001
      bins_area[length(bins_area)] <- bins_area[length(bins_area)] + 0.000001
      pal_area <- leaflet::colorBin(palette = grDevices::colorRamp(c(colour1,colour2,colour3), interpolate="linear"), domain = NULL, bins = bins_area)
      if(length(bins_area)>1){area_fillColor <- pal_area(areas_sf$area_plot)} else {area_fillColor <- 0}
      area_labels <- apply_kpi_format(areas_sf$area_plot, response, weight, kpi_spec)
      opacity_area_modifier <- hot_spotted_opacity(areas_sf$area_plot, hotspots)
    }
    # sector bins, labels and opacity
    if(!is.null(sector_summary)){
      bins_sector <- unique(stats::quantile(round(sector_summary$sector_plot,6), na.rm = TRUE, probs = 0:20/20))
      bins_sector[1] <- bins_sector[1] - 0.000001
      bins_sector[length(bins_sector)] <- bins_sector[length(bins_sector)] + 0.000001
      pal_sector <- leaflet::colorBin(palette = grDevices::colorRamp(c(colour1,colour2,colour3), interpolate="linear"), domain = NULL, bins = bins_sector)
      if(length(bins_sector)>1){sector_fillColor <- pal_sector(sectors_sf$sector_plot)} else {sector_fillColor <- 0}
      sector_labels <- apply_kpi_format(sectors_sf$sector_plot, response, weight, kpi_spec)
      opacity_sector_modifier <- hot_spotted_opacity(sectors_sf$sector_plot, hotspots)
    }
    # unit bins, labels and opacity
    if(!is.null(unit_summary)){
      bins_unit <- unique(stats::quantile(round(units_sf$unit_plot,6), na.rm = TRUE, probs = 0:20/20))
      bins_unit[1] <- bins_unit[1] - 0.000001
      bins_unit[length(bins_unit)] <- bins_unit[length(bins_unit)] + 0.000001
      pal_unit <- leaflet::colorBin(palette = grDevices::colorRamp(c(colour1,colour2,colour3), interpolate="linear"), domain = NULL, bins = bins_unit)
      if(length(bins_unit)>1){unit_fillColor <- pal_unit(units_sf$unit_plot)} else {unit_fillColor <- 0}
      unit_labels <- apply_kpi_format(units_sf$unit_plot, response, weight, kpi_spec)
    }
    # add on sectors if available - sectors before areas to get polygon order correct
    if(!is.null(sector_summary)){
      m %>%
        leaflet::addMapPane('sector_polygons', zIndex = 405) %>%
        leaflet::addPolygons(data = sectors_sf,
                             layerId = sectors_sf$PostcodeSector,
                             group = 'Sector',
                             weight = line_thickness * 0.1,
                             opacity = 1,
                             color = "black",
                             smoothFactor = 0,
                             fillColor = sector_fillColor,
                             fillOpacity = opacity * opacity_sector_modifier,
                             label = lapply(paste(sep = "", '<b>',sectors_sf$PostcodeSector,'</b><br/>',sector_labels), HTML),
                             labelOptions = labelOptions(textOnly = FALSE, style=label_style),
                             highlightOptions = highlightOptions(color='white', weight = 2*line_thickness, bringToFront = TRUE, sendToBack = TRUE),
                             options = pathOptions(pane = "sector_polygons")
        )
    }
    # add on units if available
    if(!is.null(unit_summary)){
      m %>%
        addMapPane('points', zIndex = 420) %>%
        addCircles(data = units_sf,
                   layerId = units_sf$PostcodeUnit,
                   lng=units_sf$X,
                   lat=units_sf$Y,
                   label = lapply(paste(sep = "", '<b>',units_sf$PostcodeUnit,'</b><br/>',unit_labels), HTML),
                   labelOptions = labelOptions(textOnly = FALSE,style=label_style),
                   radius = 50,
                   weight = 0,
                   stroke = FALSE,
                   fill = TRUE,
                   fillColor = ~pal_unit(units_sf$unit_plot),
                   fillOpacity = ifelse(is.na(units_sf$unit_plot),0.5,1.0),
                   highlightOptions = highlightOptions(color='white', opacity = 1, weight = 1, fillOpacity = 1, bringToFront = TRUE, sendToBack = TRUE),
                   group = "Unit",
                   options = pathOptions(pane = "points")
                   )
    }
    # add on areas if available
    if(!is.null(area_summary)){
      m %>%
        leaflet::addMapPane('area_polygons', zIndex = 405) %>%
        leaflet::addPolygons(data = areas_sf,
                             layerId = areas_sf$PostcodeArea,
                             group = 'Area',
                             weight = line_thickness,
                             opacity = 1,
                             color = "black",
                             smoothFactor = 0,
                             fillColor = area_fillColor,
                             fillOpacity = opacity * opacity_area_modifier,
                             label = lapply(paste(sep = "", '<b>',areas_sf$PostcodeArea,'</b><br/>',area_labels), HTML),
                             labelOptions = labelOptions(textOnly = FALSE, style=label_style),
                             highlightOptions = highlightOptions(color='white', weight = 2*line_thickness, bringToFront = TRUE, sendToBack = TRUE),
                             options = pathOptions(pane = "area_polygons")
        ) %>%
        leaflet::addLabelOnlyMarkers(lng = areas_sf$X,
                                     lat = areas_sf$Y,
                                     label = lapply(paste(sep = "", '<b>',areas_sf$PostcodeArea,'</b><br/>',area_labels), HTML),
                                     labelOptions = labelOptions(
                                       style = list('color' = "black", 'font-size' = paste0(label_size, 'px')),
                                       noHide = show_area_labels,
                                       direction = 'center',
                                       textOnly = TRUE)
        )
    }
  }
}
apply_kpi_format <- function(x, response, weight, kpi_spec){
  kpi_numerator <- NULL
  kpi_denominator <- NULL
  # function to format the number x according to whatever format has been defined in the kpi_spec
  if(is.numeric(x) & !is.null(response) & !is.null(weight)){
    format_row <- kpi_spec[kpi_numerator==response & kpi_denominator==weight,]
    if(nrow(format_row)>0){
      significant_digits <- format_row$kpi_signif
      divisor <- format_row$kpi_divisor
      decimal_places <- format_row$kpi_dp
      prefix <- format_row$kpi_prefix
      suffix <- format_row$kpi_suffix
      if(is.na(significant_digits)) significant_digits <- 6
      if(is.na(divisor)) divisor <- 1
      #if(is.na(decimal_places)) decimal_places <- 3
      if(is.na(prefix)) prefix <- ''
      if(is.na(suffix)) suffix <- ''
      # format number
      x_MappaR <- x / divisor
      if(!is.na(decimal_places) & is.numeric(decimal_places)){
        x_MappaR <- format(round(x_MappaR,decimal_places), nsmall = decimal_places, big.mark = ',')
      } else {
        x_MappaR <- format(x_MappaR, digits = significant_digits, big.mark = ',')
      }
      x_MappaR <- paste(sep = '', prefix, x_MappaR, suffix)
    } else {
      # simple format depending on magnitude of number
      m <- mean(x, na.rm = TRUE)
      if(!is.na(m)){
        if(log10(abs(m)+1)<0){
          x_MappaR <- format(round(x,3), nsmall = 3, big.mark = ',')
        } else if (log10(abs(m)+1)<2){
          x_MappaR <- format(round(x,3), nsmall = 2, big.mark = ',')
        } else {
          x_MappaR <- format(round(x,3), nsmall = 0, big.mark = ',')
        }
      } else {
        x_MappaR <- NA
      }
    }
  } else {
    x_MappaR <- NA
  }

}
maxN <- function(x, N=2){
  len <- length(x)
  # replace NAs with smallest value in x
  x[is.na(x)] <- min(x, na.rm = TRUE)
  if(N>len){
    warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  sort(x,partial=len-N+1)[len-N+1]
}
coords <- function(postcode){
  centroid <- NULL
  zoom <- NULL
  if(nchar(postcode)<3){
    # postcode area
    if(postcode %in% uk_areas$PostcodeArea){
      centroid <- list(uk_areas$X[uk_areas$PostcodeArea==postcode], uk_areas$Y[uk_areas$PostcodeArea==postcode])
      zoom <- 10
    }
  } else if (nchar(postcode)<=6){
    # most likely a postcode sector
    if(postcode %in% uk_sectors$PostcodeSector){
      centroid <- list(uk_sectors$X[uk_sectors$PostcodeSector==postcode], uk_sectors$Y[uk_sectors$PostcodeSector==postcode])
      zoom <- 13
    }
  } else {
    # postcode unit
    if(postcode %in% uk_units[['PostcodeUnit']]){
      centroid <- list(uk_units$X[uk_units$PostcodeUnit==postcode], uk_units$Y[uk_units$PostcodeUnit==postcode])
      zoom <- 15
    }
  }
  return(list(centroid,zoom))
}
postcode_summary <- function(d, rows_to_summarise, response, weight, resolution){
  if(length(rows_to_summarise)==nrow(d)){
    if(weight %in% c('N','no weights')){
      d_cols <- d[, .SD, .SDcols = c(resolution, response)]
      d_cols[, weight := 1]
    } else {
      d_cols <- d[, .SD, .SDcols = c(resolution, response, weight)]
    }
  } else {
    if(weight %in% c('N','no weights')){
      d_cols <- d[rows_to_summarise, .SD, .SDcols = c(resolution, response)]
      d_cols[, weight := 1]
    } else {
      d_cols <- d[rows_to_summarise, .SD, .SDcols = c(resolution, response, weight)]
    }
  }
  names(d_cols) <- c('resolution','response','weight')
  summary <- d_cols[, list(V1 = sum(weight, na.rm = TRUE), V2 = sum(response, na.rm = TRUE)), by = 'resolution']
  names(summary) <- c(resolution, response, weight)
  return(summary)
}
hot_spotted_opacity <- function(p, hotspots){
  if(hotspots==0){
    opacity_modifier <- 1
  } else if (hotspots>0){
    opacity_modifier <- ifelse(!is.na(p) & p > maxN(p-1e-06, hotspots),1,0)
  } else if (hotspots<0){
    opacity_modifier <- ifelse(!is.na(p) & p < -maxN(-p-1e-06, -hotspots),1,0)
  }
}
