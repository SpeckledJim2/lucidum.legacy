#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @import shinyFiles
#' @import shinycssloaders
action_button_style <- "color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left"

#### HEADER ####
header <- dashboardHeader(
  title = 'Lucidum',
  titleWidth = '280',
  # tags$li(
  #   class = "dropdown",
  #   htmlOutput('GlimmaR_model_index')
  # ),
  tags$li(class = "dropdown",
          actionButton(
            inputId = "GlimmaR_prev_model",
            label = '<',
            style="text-align: left; margin-top: 8px; margin-right: 3px")
  ),
  tags$li(class = "dropdown",
          actionButton(
            inputId = "GlimmaR_navigate",
            icon = icon('star'),
            label = 'GLMs (0/0)',
            style="color: #BAC6CE; background-color: #222D32; border-color: #BAC6CE; text-align: left; margin-top: 8px; margin-right: 3px")
  ),
  tags$li(class = "dropdown",
          div(style='padding-right:20px;',
              actionButton(
                inputId = "GlimmaR_next_model",
                label = '>',
                style="text-align: left; margin-top: 8px; margin-right: 3px")
          )
  ),
  tags$li(class = "dropdown",
          actionButton(
            inputId = "BoostaR_prev_model",
            label = '<',
            style="text-align: left; margin-top: 8px; margin-right: 3px")
  ),
  tags$li(class = "dropdown",
          actionButton(
            inputId = "BoostaR_navigate",
            icon = icon('rocket'),
            label = 'GBMs (0/0)',
            style="color: #BAC6CE; background-color: #222D32; border-color: #BAC6CE; text-align: left; margin-top: 8px; margin-right: 3px")
  ),
  tags$li(class = "dropdown",
          div(style='padding-right:20px;',
              actionButton(
                inputId = "BoostaR_next_model",
                label = '>',
                style="text-align: left; margin-top: 8px; margin-right: 3px")
              )
          ),
  tags$li(
    class = "dropdown",
    div('Dataset',
        style = 'color: white; font-size: 14px; margin-top: 16px; margin-bottom:-20px;'
        )
  ),
  tags$li(
    class = "dropdown",
    div(
      style="margin-left: 6px; margin-right: 14px; margin-top:8px; margin-bottom:-20px;",
      div(
        selectInput(inputId = "dataset",
                    width = 400,
                    label = NULL,
                    choices = 'select dataset'
                    ),
        style = 'font-weight: 600'
        )
      )
    )
)

#### SIDEBAR ####
sidebar <- dashboardSidebar(
  width = '280',
  sidebarMenu(
    br(),
    tags$head(tags$style(".sidebar-menu li a {padding-top: 6px; padding-bottom: 6px}")),
    tags$head(tags$style(".sidebar-menu li a {font-size: 16px}")),
    id = "tabs",     # Setting id makes input$tabs give the tabName of currently-selected tab
    menuItem("DataR", tabName = "DataR", icon = icon("bars")),
    menuItem("ChartaR", tabName = "ChartaR", icon = icon("chart-line")),
    menuItem("MappaR", tabName = "MappaR", icon = icon("map")),
    menuItem('GlimmaR', tabName = 'GlimmaR', icon = icon("star")),
    menuItem('BoostaR', tabName = 'BoostaR', icon = icon("rocket"))
  ),

  # select input for the response and weight
  div(
    selectInput(inputId = "response", label = 'Response', choices = c('select feature')),
    style="margin-bottom:-10px;"
    ),
  div(
    selectInput(inputId = "weight", label = "Weight", choices=c('')),
    style='margin-bottom:-10px;'
    ),
  tags$head(tags$script('
                          // Define function to set height of "sidebar_kpi"
                          setHeight_sidebar = function() {
                            var window_height = $(window).height();
                            var header_height = $(".main-header").height();
                            var num_rows = (window_height - header_height)/20 - 32 ;
                            var preview = document.getElementById("sidebar_kpi")
                            preview.setAttribute("size", num_rows);
                          };
                          // Set input$box_height when the connection is established
                          $(document).on("shiny:connected", function(event) {
                            setHeight_sidebar();
                          });
                          // Refresh the box height on every window resize event
                          $(window).on("resize", function(){
                            setHeight_sidebar();
                          });
                        ')),
  div(
    selectInput(inputId = "sidebar_kpi", label = 'Select KPI', choices = c(''), size = 15, selectize = FALSE),
    style='padding-bottom: 5px;'
  ),
  fluidRow(
    style = 'margin-left: 0px; margin-right: 0px;',
    column(width = 12,
           tags$style("#ChartaR_pbar-title {font-weight: 400;}"),
           tags$style(HTML(".progress-xs {margin-bottom: 0px;}")),
           div(
             progressBar(
               id = "ChartaR_pbar",
               value = 100,
               total = 100,
               display_pct = FALSE,
               size = 'xs',
               title = 'Filter',
               status = 'primary',
               striped = FALSE
             ),
             style = 'margin-bottom: 0px;',
           )
    ),
    div(
      radioGroupButtons(
        inputId = "ChartaR_training_test",
        label = NULL,
        choices = c('All','Train','Test'),
        individual = FALSE,
        size = 'xs',
        justified = TRUE
      ),
      style = 'margin-bottom:-30px; padding-top:0px ; padding-bottom:0px'
    ),
    div(
      selectInput(
        inputId = "ChartaR_filter_list",
        label = NULL,
        choices=c('no filter'),
        size = 8,
        width = '100%',
        selectize = FALSE,
        multiple = TRUE
      ),
      style = 'margin-bottom:-30px; padding-top:0px ; padding-bottom:0px'
    ),
    div(
      radioGroupButtons(
        inputId = "filter_operation",
        label = NULL,
        choices = c('Logical AND','Logical OR'),
        individual = FALSE,
        size = 'xs',
        justified = TRUE
      ),
      style = 'padding-top:0px ; padding-bottom:0px'
      )
    )
  )
#### BODY ####
body <- dashboardBody(

  # style tags
  tags$head(tags$style("body, label, input, button, radio, select {font-family: 'Helvetica Neue', Helvetica; font-weight: 400;}")),
  tags$head(tags$style("h1, h2, h3, h4 {font-family: 'Helvetica Neue', Helvetica; font-weight: 400;}")),
  tags$head(tags$style("body {overflow-y: hidden;}")), # prevents vertical scrolling in browser window
  tags$head(tags$style('input {autocorrect: "off"; spellcheck: "false";}')), # as they are annoying when typing
  # JS to return window dimensions - input$dimension[0] and input$dimension[1] contain width and height
  tags$head(tags$script('var dimension = [0, 0];
                         $(document).on("shiny:connected", function(e) {
                                dimension[0] = window.innerWidth;
                                dimension[1] = window.innerHeight;
                                Shiny.onInputChange("dimension", dimension);
                            });
                          $(window).resize(function(e) {
                                dimension[0] = window.innerWidth;
                                dimension[1] = window.innerHeight;
                                Shiny.onInputChange("dimension", dimension);
                            });'
                        )
            ),
  tabItems(
    DataR_ui(),
    ChartaR_ui(),
    MappaR_ui(),
    BoostaR_ui(),
    GlimmaR_ui()
    )
)

#### UI ####
ui <- dashboardPage(header, sidebar, body, title = 'Lucidum')

