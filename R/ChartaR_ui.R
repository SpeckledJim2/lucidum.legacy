ChartaR_ui <- function(){
  tabItem(
    tabName = "ChartaR",
    tabsetPanel(
      id = 'ChartaR_tabsetPanel',
      tabPanel('One way summary',
                     br(),
                     fluidRow(
                       column(
                         width = 3,
                         selectInput_ui(id = 'ChartaR_x_axis_feature', label = 'x-axis feature', height_divisor = 50, height_adj = 2, multiple = FALSE),
                         selectInput_ui(id = 'ChartaR_1W_add_columns', label = 'Additional y-axis columns', height_divisor = 55, height_adj = 3, multiple = TRUE)
                       ),
                       column(
                         width = 9,
                         fluidRow(
                           column(3,
                                  radioGroupButtons(
                                    inputId = "ChartaR_1W_sort_order",
                                    label = "x-axis sort order",
                                    choices = c('A-Z', 'Wt', 'Act','Add','PD'),
                                    individual = FALSE,
                                    size = 'xs',
                                    selected = 'A-Z')
                           ),
                           column(3,
                                  radioGroupButtons(
                                    inputId = "ChartaR_1W_group_low_exposure",
                                    label = "Group low weights",
                                    choices = c(0,5,10,20,50,'1%'),
                                    individual = FALSE,
                                    size = 'xs',
                                    selected = 0)
                           ),
                           column(2,
                                  align = 'left',
                                  radioGroupButtons(
                                    inputId = "ChartaR_A_vs_E_show_labels",
                                    label = "Labels",
                                    choices = c('-', 'Weight','All'),
                                    individual = FALSE,
                                    size = 'xs',
                                    selected = '-'
                                  )
                           ),
                           column(4,
                                  align = 'right',
                                  radioGroupButtons(
                                    inputId = "ChartaR_1W_banding",
                                    label = "Banding",
                                    choices = c('<','0.01','0.1','1','5','10','100','>',`<i class='fa fa-lock'></i>` = 'lock'),
                                    individual = FALSE,
                                    size = 'xs',
                                    selected = -1)
                           )
                         ),
                         fluidRow(
                           column(
                             width = 3,
                             radioGroupButtons(
                               inputId = "ChartaR_1W_error_bars",
                               label = "Error bars",
                               choices = c('-'='-', '90%'=0.9, '95%'=0.95, '99%'=0.99),
                               individual = FALSE,
                               size = 'xs',
                               selected = '-'
                               )
                             ),
                           column(
                             width = 3,
                             radioGroupButtons(
                               inputId = "ChartaR_A_vs_E_show_partial_dependencies",
                               label = "Partial dependencies",
                               choices = c('-','GLM','GBM','Both'),
                               individual = FALSE,
                               size = 'xs',
                               selected = '-'
                             )
                           ),
                           column(
                             width = 3,
                             radioGroupButtons(
                               inputId = "ChartaR_show_response",
                               label = "Response",
                               choices = c('Hide','Show'),
                               individual = FALSE,
                               size = 'xs',
                               selected = 'Show')
                             ),
                           column(
                             width = 3,
                             align = 'right',
                             radioGroupButtons(
                               inputId = "ChartaR_1W_y_transform",
                               label = "Response transform",
                               choices = c('-', 'Log','Exp','Logit','Base'),
                               individual = FALSE,
                               size = 'xs',
                               selected = '-'
                               )
                             )
                           ),
                         tabsetPanel(
                           id = 'ChartaR_one_way_tabs',
                           type = 'tabs',
                           tabPanel('Chart',
                                    one_way_chart_ui("ChartaR_one_way_chart")
                                    ),
                           tabPanel('Table',
                                    br(),
                                    one_way_table_ui("ChartaR_one_way_table")
                                    )
                           )
                         )
                       )
                     ),
            tabPanel('Histogram',
                     br(),
                     fluidRow(
                       column(
                         width = 3,
                         textInput(
                           'ChartaR_histogram_num_bins',
                           label = 'Histogram bins (max 10,000)',
                           width = '100%',
                           placeholder = 'auto'
                         ),
                         #DT::DTOutput('ChartaR_histogram_summary_table')
                         shinycssloaders::withSpinner(DT::DTOutput('ChartaR_histogram_summary_table')),
                       ),
                       column(
                         width = 9,
                         fluidRow(
                           column(
                             width = 3,
                             radioGroupButtons(
                               label = 'Distribution',
                               inputId = "hist_inc_cum",
                               choices = c('Incremental'=0,'Cumulative'=1),
                               individual = FALSE,
                               size = 'xs',
                               selected = 0
                               )
                              ),
                              column(
                                width = 3,
                                radioGroupButtons(
                                label = 'y axis type',
                                inputId = "hist_normalise",
                                choices = c('Sum'='','Probability'='probability'),
                                individual = FALSE,
                                size = 'xs',
                                selected = ''
                                )
                              ),
                              column(
                                width = 3,
                                radioGroupButtons(
                                label = 'log scale',
                                inputId = "hist_log_scale",
                                choices = c('-','X axis','Y axis','Both'),
                                individual = FALSE,
                                size = 'xs',
                                selected = '-'
                                )
                              ),
                              column(
                                width = 3,
                                align = 'right',
                                radioGroupButtons(
                                  label = 'Sample data for chart',
                                  inputId = "ChartaR_histogram_use_sample",
                                  choices = c('Use 100k','Use all'),
                                  individual = FALSE,
                                  size = 'xs',
                                  selected = 'Use 100k'
                                  )
                                )
                           ),
                         br(),
                         #plotly::plotlyOutput('histogram'),
                         shinycssloaders::withSpinner(plotly::plotlyOutput('histogram')),
                              tags$head(tags$script('
                                            // Define function to set height of "histogram"
                                            setHeightChartaR_histogram = function() {
                                              var window_height = $(window).height();
                                              var header_height = $(".main-header").height();

                                              var boxHeight = (window_height - header_height) - 220;
                                              $("#histogram").height(boxHeight);
                                            };
                                            // Set input$box_height when the connection is established
                                            $(document).on("shiny:connected", function(event) {
                                              setHeightChartaR_histogram();
                                            });
                                            // Refresh the box height on every window resize event
                                            $(window).on("resize", function(){
                                              setHeightChartaR_histogram();
                                            });
                                          '))
                         )
                       )
                     ),
      tabPanel('A vs E analysis',
               br(),
               fluidRow(
                 column(
                   width = 3,
                   selectInput(inputId = 'ChartaR_A_vs_E_feature_spec', label = 'Choose feature specification', width = '100%', choices = NULL),
                   materialSwitch(inputId = 'ChartaR_A_vs_E_apply_filter', label = 'Apply current filter to calculation', status = 'success', right = TRUE),
                   radioGroupButtons(inputId = 'ChartaR_A_vs_E_method', label = 'Metric type', choices = c('Absolute','Percentage'), selected = 'Absolute'),
                   selectInput_ui(id = 'ChartaR_A_vs_E_feature', label = 'Test feature', height_divisor = 20, height_adj = 24, multiple = FALSE),
                   actionButton(
                     inputId = "ChartaR_run_A_vs_E_analysis",
                     label = "Run analysis",
                     icon = icon("chevron-right"),
                     style="color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left"
                     )
                   ),
                 column(
                   width = 9,
                   fluidRow(
                     column(
                       width = 4,
                       radioGroupButtons(
                         inputId = "ChartaR_residual_error_min_exposure",
                         label = "Residual error min weight",
                         choices = c(0,5,10,20,50,100),
                         individual = FALSE,
                         size = 'xs',
                         selected = 10),
                       ),
                     column(
                       width = 4
                       # radioGroupButtons(
                       #   inputId = "ChartaR_random_consistent_min_exposure",
                       #   label = "Consistency calc min weight",
                       #   choices = c(0,10,20,50,100,200,500,1000),
                       #   individual = FALSE,
                       #   size = 'xs',
                       #   selected = 100),
                       ),
                     column(
                       width = 4,
                       align = 'right',
                       actionButton(
                         inputId = "ChartaR_A_vs_E_select_feature",
                         label = NULL,
                         icon = icon("chart-line")
                         )
                       )
                     ),
                   DT::DTOutput('ChartaR_residual_error_table')
                   # tabsetPanel(
                   #   id = 'ChartaR_A_vs_E_tabs',
                   #   type = 'tabs',
                   #   tabPanel('Residual error',
                   #            br(),
                   #            DT::DTOutput('ChartaR_residual_error_table')
                   #            ),
                   #   tabPanel('Random feature consistency',
                   #            )
                   #   )
                   )
                 )
               )
      )
    )
}

