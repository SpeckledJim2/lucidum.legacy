DataR_ui <- function(){
  action_button_style <- "color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left"
  tabItem("DataR",
          tabsetPanel(id = 'DataR_tabsetPanel',
                      tabPanel('Dataset viewer',
                               fluidRow(
                                 column(
                                   width = 12,
                                   align = 'right',
                                   checkboxInput(inputId='DataR_dataset_transpose',
                                                 label='Transpose',
                                                 value = FALSE
                                                 )
                                 )
                               ),
                               fluidRow(
                                 column(
                                   width = 12,
                                   shinycssloaders::withSpinner(DT::DTOutput('DataR_dataset_viewer')),
                                 )
                               )
                               ),
                      tabPanel('Dataset summary',
                               fluidRow(
                                 column(
                                   width = 8,
                                   fluidRow(
                                     column(
                                       width = 6,
                                       p('Dataset column summary', style = 'font-size: 20px; margin-top: 18px; margin-bottom: 0px')
                                     ),
                                     column(
                                       width = 6,
                                       align = 'right',
                                       style='margin-top:16px; padding-left:0px; margin-bottom:0px',
                                       actionButton(inputId = 'DataR_take_feature_to_ChartaR',
                                                    icon = icon('chart-line'),
                                                    label = NULL
                                                    ),
                                       actionButton(inputId = "DataR_refresh_column_summary",
                                                        icon = icon("chevron-right"),
                                                        label = 'Refresh',
                                                        style=action_button_style
                                                        )
                                     )
                                   ),
                                   br(),
                                   shinycssloaders::withSpinner(DT::DTOutput('DataR_column_summary')),
                                 ),
                                 column(
                                   width = 4,
                                   # fluidRow(
                                   #   column(
                                   #     width = 12,
                                   #     div(h3('Dataset summary'), style = 'margin-bottom:19px')
                                   #   )
                                   # ),
                                   #DT::DTOutput('DataR_high_level_summary'),
                                   fluidRow(
                                     column(
                                       width = 12,
                                       div(htmlOutput('DataR_feature'), style = 'margin-bottom:39px')
                                       #div(h3('Feature statistics'), style = 'margin-bottom:24px')
                                     )
                                   ),
                                   shinycssloaders::withSpinner(DT::DTOutput('DataR_feature_summary')),
                                 ),
                               ),
                      ),
                      tabPanel('KPI specification',
                               br(),
                               fluidRow(
                                 column(
                                   width = 4,
                                   div(
                                     p("Use the KPI specification to apply formatting to KPI's
                                       in summary charts and tables")
                                   ),
                                   style = 'font-size: 20px; font-weight: 400'
                                 ),
                                 column(
                                   width = 8,
                                   div(
                                     p('kpi_name: the name of the KPI', style = 'margin: 0 0 0 0'),
                                     p('kpi_numerator: numerical column name', style = 'margin: 0 0 0 0'),
                                     p('kpi_denominator: numerical column name, N (equal weights per row) or "no weights" (view totals instead of averages) ', style = 'margin: 0 0 0 0'),
                                     p('kpi_dp: number of decimal places to display (over-rides kpi_signif)', style = 'margin: 0 0 0 0'),
                                     p('kpi_signif: number of significant digits to display', style = 'margin: 0 0 0 0'),
                                     p('kpi_divisor: e.g. 0.01 for percentages, 1000 for thousands', style = 'margin: 0 0 0 0'),
                                     p('kpi_prefix: character text to appear in front of KPI, e.g. $', style = 'margin: 0 0 0 0'),
                                     p('kpi_suffix: character text to apepar after KPI, e.g. %', style = 'margin: 0 0 0 0')
                                   ),
                                   style = 'font-size: 12px; font-weight: 400;'
                                 )
                               ),
                               tags$hr(style="border-color: black; margin-bottom: 6px"),
                               edit_specification_ui('DataR_kpi_specification'),
                               # the code below should go into the module itself!
                               tags$head(tags$script('
                                            // Define function to set height of "DataR_kpi_specification-specification"
                                            setHeight_DataR_kpi_specification = function() {
                                              var window_height = $(window).height();
                                              var header_height = $(".main-header").height();
                                              var boxHeight = (window_height - header_height) - 360;
                                              $("#DataR_kpi_specification-specification").height(boxHeight);
                                            };
                                            // Set input$box_height when the connection is established
                                            $(document).on("shiny:connected", function(event) {
                                              setHeight_DataR_kpi_specification();
                                            });
                                            // Refresh the box height on every window resize event
                                            $(window).on("resize", function(){
                                              setHeight_DataR_kpi_specification();
                                            });
                                          '))
                               ),
                      tabPanel('Filter specification',
                               br(),
                               fluidRow(
                                 column(
                                   width = 4,
                                   div(
                                     p("Use the filter specification to define filters to apply to charts and tables")
                                   ),
                                   style = 'font-size: 20px; font-weight: 400'
                                 ),
                                 column(
                                   width = 8,
                                   div(
                                     p('the filter expression is a normal R type expression', style = 'margin: 0 0 0 0'),
                                     p('using the dataset column names', style = 'margin: 0 0 0 0'),
                                     p('e.g. my_column>5 or my_column=="my_text"', style = 'margin: 0 0 0 0'),
                                     p('use == for equality', style = 'margin: 0 0 0 0'),
                                     p('use & for logical AND', style = 'margin: 0 0 0 0'),
                                     p('use | for logical OR', style = 'margin: 0 0 0 0'),
                                     p(' ', style = 'margin: 0 0 0 0'),
                                     p(' ', style = 'margin: 0 0 0 0')
                                   ),
                                   style = 'font-size: 12px; font-weight: 400;'
                                 )
                               ),
                               tags$hr(style="border-color: black; margin-bottom: 6px"),
                               edit_specification_ui('DataR_filter_specification'),
                               # the code below should go into the module itself!
                               tags$head(tags$script('
                                            // Define function to set height of "DataR_filter_specification-specification"
                                            setHeight_DataR_filter_specification = function() {
                                              var window_height = $(window).height();
                                              var header_height = $(".main-header").height();
                                              var boxHeight = (window_height - header_height) - 360;
                                              $("#DataR_filter_specification-specification").height(boxHeight);
                                            };
                                            // Set input$box_height when the connection is established
                                            $(document).on("shiny:connected", function(event) {
                                              setHeight_DataR_filter_specification();
                                            });
                                            // Refresh the box height on every window resize event
                                            $(window).on("resize", function(){
                                              setHeight_DataR_filter_specification();
                                            });
                                          '))
                      ),
                      tabPanel('Feature specification',
                               br(),
                               fluidRow(
                                 column(
                                   width = 4,
                                   div(
                                     p("Use the feature specification to define feature groupings
                                       and set GlimmaR model export base levels and bandings")
                                   ),
                                   style = 'font-size: 20px; font-weight: 400'
                                 ),
                                 column(
                                   width = 8,
                                   div(
                                     p('feature: the name of the KPI', style = 'margin: 0 0 0 0'),
                                     p('base_level: level set to 1.000 in GlimmaR table export', style = 'margin: 0 0 0 0'),
                                     p('min: minimum value for continuous features', style = 'margin: 0 0 0 0'),
                                     p('max: maximum value for continuous features', style = 'margin: 0 0 0 0'),
                                     p('banding: banding for continuous features', style = 'margin: 0 0 0 0'),
                                     p('monotonicity: for BoostaR models', style = 'margin: 0 0 0 0'),
                                     p('interaction_grouping: for feature selection', style = 'margin: 0 0 0 0'),
                                     p('use subsequent columns to define feature scenarios for BoostaR models with the word "feature"', style = 'margin: 0 0 0 0')
                                   ),
                                   style = 'font-size: 12px; font-weight: 400;'
                                 )
                               ),
                               tags$hr(style="border-color: black; margin-bottom: 6px"),
                               edit_specification_ui('DataR_feature_specification'),
                               # the code below should go into the module itself!
                               tags$head(tags$script('
                                            // Define function to set height of "DataR_feature_specification-specification"
                                            setHeight_DataR_feature_specification = function() {
                                              var window_height = $(window).height();
                                              var header_height = $(".main-header").height();
                                              var boxHeight = (window_height - header_height) - 360;
                                              $("#DataR_feature_specification-specification").height(boxHeight);
                                            };
                                            // Set input$box_height when the connection is established
                                            $(document).on("shiny:connected", function(event) {
                                              setHeight_DataR_feature_specification();
                                            });
                                            // Refresh the box height on every window resize event
                                            $(window).on("resize", function(){
                                              setHeight_DataR_feature_specification();
                                            });
                                          '))
                               ),
                      tabPanel('shinyAce',
                               fluidRow(
                                 column(
                                   width = 12,
                                   fluidRow(
                                     column(
                                       width = 6,
                                       h3('shinyAce')
                                       ),
                                     column(
                                       width = 6,
                                       align = 'right',
                                       br(),
                                       actionButton(
                                         inputId = "shinyAce_textsize_minus",
                                         label = "A-"
                                         ),
                                      actionButton(
                                        inputId = "shinyAce_textsize_plus",
                                        label = "A+"
                                        ),
                                      actionButton(
                                        "shinyAce_evaluate",
                                        label = 'Evaluate',
                                        icon = icon("chevron-right"),
                                        style="color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left"
                                        )
                                      )
                                     ),
                                     tags$head(tags$script('
                                      // Define function to set height of "shinyAce_code"
                                      setHeight_shinyACE_script_box = function() {
                                        var window_height = $(window).height();
                                        var header_height = $(".main-header").height();
                                        var h = (window_height - header_height - 600)+"px" ;
                                        var box = document.getElementById("shinyAce_code");
                                        box.style.height = h;
                                      };
                                      // Set input$box_height when the connection is established
                                      $(document).on("shiny:connected", function(event) {
                                        setHeight_shinyACE_script_box();
                                      });
                                      // Refresh the box height on every window resize event
                                      $(window).on("resize", function(){
                                        setHeight_shinyACE_script_box();
                                      });'
                                                           )
                                               ),
                                   shinyAce::aceEditor(
                                     "shinyAce_code",
                                     mode = "r",
                                     wordWrap = FALSE,
                                     autoScrollEditorIntoView = TRUE,
                                     placeholder = ''
                                     )
                                   )
                                 ),
                               fluidRow(
                                 column(
                                   width = 12,
                                   tags$head(tags$style("#shinyAce_output{font-size:10px; overflow-y:scroll; max-height: 400px; background: ghostwhite;}")),
                                   verbatimTextOutput("shinyAce_output")
                                 )
                               ),
                               )
                      )
          )
}

