#' @import sf
MappaR_server <- function(input, output, session, d, RVs){
  first_map_redraw <- reactiveVal(TRUE)
  plot_postcode_area <- reactiveVal('E')
  output$MappaR_uk_map <- leaflet::renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE, zoomControl = FALSE, attributionControl=TRUE)) %>%
      addTiles(group = "OSM") %>%
      addProviderTiles("Esri.WorldStreetMap", group = 'Esri') %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Stamen") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addLayersControl(
        baseGroups = c('Blank','Esri','OSM','Stamen','Satellite'),
        overlayGroups = c('Area','Sector','Unit'),
        options = layersControlOptions(position = "topleft",
                                       collapsed = FALSE,
                                       autoZIndex = TRUE)) %>%
      hideGroup(c('Sector','Unit')) %>%
      htmlwidgets::onRender("function(el, x) {L.control.zoom({ position: 'topleft' }).addTo(this)}")  %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Reset",
        onClick=JS("function(btn, map){map.setView([54.81,-1],6);}"))) %>%
      setView(lng=-1,lat=54.81,zoom=6)
  })
  observe({
    if(input$tabs=='MappaR'){
      withProgress(message = 'MappaR', detail = 'redrawing map', value = 0.5,{
        viz_render_map('MappaR_uk_map',
                       session,
                       input,
                       d(),
                       RVs$kpi_spec,
                       first_map_redraw(),
                       RVs$train_test_filter,
                       RVs$user_filter,
                       plot_postcode_area())
      })
    }
  })
  observeEvent(input$tabs,{
    if(input$tabs=='MappaR' & first_map_redraw()){
      if(input$response!='select feature'){
        withProgress(message = 'MappaR', detail = 'redrawing map', value = 0.5,{
          viz_render_map('MappaR_uk_map',
                         session,
                         input,
                         d(),
                         RVs$kpi_spec,
                         first_map_redraw(),
                         RVs$train_test_filter,
                         RVs$user_filter,
                         plot_postcode_area())
        })
        first_map_redraw(FALSE)
      }
    }
  })
  observeEvent(input$MappaR_dark_mode, {
    if(input$MappaR_dark_mode){
      session$sendCustomMessage("background-color", "#242d31")
    } else {
      session$sendCustomMessage("background-color", "#FFFFFF")
    }
  })
  observeEvent(input$MappaR_uk_map_shape_mouseover,{
    PostcodeArea <- NULL
    PostcodeArea_name <- NULL
    pointId <- input$MappaR_uk_map_shape_mouseover$id
    if(nchar(pointId)>2){
      pointId_area <- substr(pointId,1,regexpr('[0-9]', pointId)-1)
    } else {
      pointId_area <- pointId
    }
    name <- postcode_area_name_mapping[PostcodeArea==pointId_area, PostcodeArea_name]
    output$MappaR_panel_title <- renderText({pointId})
    output$MappaR_panel_location <- renderUI(({name}))
  })
  output$MappaR_filters <- renderText({
    if(input$ChartaR_filter_list[1]=='no filter'){
      filter_text <- ''
    } else {
      filter_text <- paste(input$ChartaR_filter_list, collapse = ', ')
    }
    if(input$ChartaR_training_test=='All'){
      train_test_filter_text <- ''
    } else {
      train_test_filter_text <- input$ChartaR_training_test
    }
    if(filter_text==''){
      train_test_filter_text
    } else {
      if(train_test_filter_text==''){
        filter_text
      } else {
        paste0(train_test_filter_text, ', ', filter_text)
      }
    }
  })
  observeEvent(input$MappaR_postcode, ignoreInit = TRUE,{
    if(input$MappaR_postcode!=''){
      # find and zoom
      coords_and_zoom <- coords(input$MappaR_postcode)
      postcode_centroid <- coords_and_zoom[[1]]
      zoom_level <- coords_and_zoom[[2]]
      if(!is.null(postcode_centroid)){
        leafletProxy("MappaR_uk_map", session) %>% setView(lng=postcode_centroid[[1]],lat=postcode_centroid[[2]],zoom=zoom_level)
        if(nchar(input$MappaR_postcode)>2){
          postcode_area <- substr(input$MappaR_postcode,1,regexpr('[0-9]', input$MappaR_postcode)-1)
          plot_postcode_area(postcode_area)
        } else {
          plot_postcode_area(input$MappaR_postcode)
        }
      } else {
        showNotification('Postcode not found', type = 'error')
      }
    }
  })
  observeEvent(input$MappaR_palettes, {
    p <- input$MappaR_palettes
    if(p=='Spectral'){
      updateSpectrumInput(session, 'MappaR_colour1', selected = 'blue')
      updateSpectrumInput(session, 'MappaR_colour2', selected = 'yellow')
      updateSpectrumInput(session, 'MappaR_colour3', selected = 'red')
    } else if(p=='Christmas') {
      updateSpectrumInput(session, 'MappaR_colour1', selected = 'green')
      updateSpectrumInput(session, 'MappaR_colour2', selected = 'white')
      updateSpectrumInput(session, 'MappaR_colour3', selected = 'red')
    } else if(p=='Greys'){
      updateSpectrumInput(session, 'MappaR_colour1', selected = 'white')
      updateSpectrumInput(session, 'MappaR_colour2', selected = 'grey')
      updateSpectrumInput(session, 'MappaR_colour3', selected = 'black')
    } else if(p=='Other1'){

    } else if(p=='Other2'){

    }
  })
  observeEvent(c(input$MappaR_colour1, input$MappaR_colour2, input$MappaR_colour3), ignoreInit = TRUE, {
    updateRadioGroupButtons(session, inputId = 'MappaR_palettes', selected = character(0))
  })
}

