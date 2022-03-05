MappaR_ui <- function(){
  button_style_move_map <- 'padding:3px; font-size:80%; margin-left:0px; margin-right:0px;color: #000; border-color: #3e6e37'
  button_style_update <- 'padding:3px; font-size:80%; margin-left:0px; margin-right:0px;color: #fff; background-color: #4bb03c; border-color: #3e6e37'
  tabItem("MappaR",
          tags$head(tags$script('
          // Define function to set height of "MappaR_uk_map" relative to brower window
          set_map_height = function() {
            var window_height = $(window).height();
            var header_height = $(".main-header").height();
            var boxHeight = (window_height - header_height) - 30;
            $("#MappaR_uk_map").height(boxHeight);
          };
          // Set input$box_height when the connection is established
          $(document).on("shiny:connected", function(event) {
            set_map_height();
          });
          // Refresh the box height on every window resize event
          $(window).on("resize", function(){
            set_map_height();
          });')),
          tags$head(tags$style(HTML('#MappaR_uk_map {background-color: rgb(36,45,49)}'))),
          tags$head(tags$style(HTML('#MappaR_controls {background-color: rgba(255,255,255,0.9)}'))),
          tags$head(tags$style(HTML('#MappaR_controls {border-width: 2px; border-color: rgb(255,255,255)}'))),
          tags$head(tags$style(HTML("#MappaR_panel_title {font-size: 48px; font-weight: 300; text-align:center}"))),
          tags$head(tags$style(HTML("#MappaR_panel_location {font-size: 20px; text-align:center}"))),
          tags$head(tags$style(HTML("#MappaR_filters {margin-top:10px; font-size: 12px; text-align:center; font-style: italic}"))),
          fluidRow(
            column(
              width = 12,
              leaflet::leafletOutput("MappaR_uk_map"),
              tags$script("
                          Shiny.addCustomMessageHandler('background-color', function(color) {
                          var map = document.getElementById('MappaR_uk_map');
                            map.style.backgroundColor = color;
                          });"
                          ),
              )
            ),
          absolutePanel(id = 'MappaR_controls',
                        class = 'panel panel-default',
                        top = '25%',
                        right = '2%',
                        width = 260,
                        fixed=TRUE,
                        draggable = TRUE,
                        height = "auto",
                        fluidRow(
                          column(width = 12,
                                 align = 'center',
                                 textOutput('MappaR_panel_title'),
                                 htmlOutput('MappaR_panel_location'),
                                 textOutput('MappaR_filters')
                                 )
                        ),
                        br(),
                        fluidRow(
                          column(width = 12,
                                 searchInput(
                                   inputId = "MappaR_postcode",
                                   label = NULL,
                                   placeholder = "enter postcode area",
                                   btnSearch = icon("search")
                                 )
                          )
                        ),
                        fluidRow(
                          column(width = 6,
                                 sliderInput(
                                   inputId = 'MappaR_line_thickness',
                                   label = 'Line thickness',
                                   min = 0,
                                   max = 5,
                                   value = 1,
                                   step = 0.1,
                                   ticks = FALSE,
                                   width = '100%'
                                 ),
                                 sliderInput(
                                   inputId = 'MappaR_opacity',
                                   label = 'Opacity',
                                   min = 0,
                                   max = 1,
                                   value = 1.00,
                                   step = 0.05,
                                   ticks = FALSE,
                                   width = '100%'
                                 ),
                          ),
                          column(width = 6,
                                 sliderInput(
                                   inputId = 'MappaR_hotspots',
                                   label = 'Hot/not-spots',
                                   min = -10,
                                   max = 10,
                                   value = 0,
                                   step = 1,
                                   ticks = FALSE,
                                   width = '100%'
                                 ),
                                 sliderInput(
                                   inputId = 'MappaR_label_size',
                                   label = 'Label size',
                                   min = 0,
                                   max = 20,
                                   value = 0,
                                   step = 1,
                                   ticks = FALSE,
                                   width = '100%'
                                 )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 12,
                            align = 'left',
                            fluidRow(
                              column(width = 4,
                                     spectrumInput(
                                       inputId = 'MappaR_colour1',
                                       label = 'Low',
                                       choices = NULL,
                                       selected = 'green',
                                       flat = TRUE,
                                       options = list(flat = 'false'),
                                       update_on = c('dragstop'),
                                       width = '100%'
                                       )
                                     ),
                              column(width = 4,
                                     spectrumInput(
                                       inputId = 'MappaR_colour2',
                                       label = 'Middle',
                                       choices = NULL,
                                       selected = 'white',
                                       flat = TRUE,
                                       options = list(flat = 'false'),
                                       update_on = c('dragstop'),
                                       width = '100%'
                                       )
                                     ),
                              column(width = 4,
                                     spectrumInput(
                                       inputId = 'MappaR_colour3',
                                       label = 'High',
                                       choices = NULL,
                                       selected = 'red',
                                       flat = TRUE,
                                       options = list(flat = 'false'),
                                       update_on = c('dragstop'),
                                       width = '100%'
                                       )
                                     )
                            ),
                            fluidRow(
                              style = 'margin-top: -10px; padding-top: -10px; margin-bottom: -10px; padding-bottom: -10px',
                              column(
                                width = 12,
                                radioGroupButtons(
                                  inputId = 'MappaR_palettes',
                                  label = NULL,
                                  justified = TRUE,
                                  size = 'xs',
                                  choices = c('Christmas','Spectral','Greys'),
                                  selected = 'Christmas'
                                  )
                              )
                            ),
                            fluidRow(
                              style = 'margin-top: -10px; padding-top: -10px; margin-bottom: -10px; padding-bottom: -10px',
                              column(
                                width = 4,
                                align = 'right',
                                div(
                                  checkboxInput(inputId = "MappaR_dark_mode",label = "Dark", value = TRUE),
                                  style = 'margin-top: -15px; padding-top: -10px;'
                                  )
                                ),
                              column(
                                width = 8,
                                align = 'center',
                                div(
                                  checkboxInput(inputId = "MappaR_sectors",label = "Sectors & units", value = FALSE),
                                  style = 'margin-top: -15px; padding-top: -10px;'
                                  )
                                )
                              )
                            )
                          )
                        )
          )
}

