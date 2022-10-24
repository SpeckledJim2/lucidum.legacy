selectInput_ui <- function(id, label = 'your_label', height_divisor, height_adj, multiple = FALSE){
  ns <- NS(id)
  id_name <- ns("selectInput")
  # can't use - in js function name, replace with underscore
  function_name <- paste0(gsub('-','_',id_name), '_function')
  js_code <- sprintf('// Define function to set height of "xaxis-selectInput"
                %s = function() {
                  var window_height = $(window).height();
                  var header_height = $(".main-header").height();
                  window_height = window.innerHeight;
                  var num_rows = Math.min(64,Math.floor(window_height/%s-%s));
                  var preview = document.getElementById("%s");
                  preview.setAttribute("size", num_rows);
                };
                // Set input$height when the connection is established
                $(document).on("shiny:connected", function(event) {
                  %s();
                });

                // Refresh the box height on every window resize event
                $(window).on("resize", function(){
                  %s();
                });
              ', function_name, height_divisor, height_adj, id_name,function_name,function_name)
  tagList(
    div(
      radioGroupButtons(
        width = '100%',
        inputId = ns('selectChooser'),
        label = label,
        choices = c('Original', 'A-Z','lucidum'),
        individual = FALSE,
        size = 'xs',
        justified = TRUE,
        selected = 'Original'),
      style = 'margin-top:0px; margin-bottom:-15px; padding-top:0px ; padding-bottom:0px'
    ),
    tags$head(tags$script(js_code)),
    div(
      radioGroupButtons(
        width = '100%',
        inputId = ns('selectChooserGroup'),
        label = NULL,
        choices = c('No groups', 'Use groups'),
        individual = FALSE,
        size = 'xs',
        justified = TRUE,
        selected = 'No groups'),
      style = 'margin-top:0px; margin-bottom:-15px; padding-top:0px ; padding-bottom:0px'
    ),
    fluidRow(
      column(
        width = 10,
        style = 'margin-left:0px; margin-right:0px;padding-right:0px',
        div(
          textInput(
            inputId = ns('search'),
            width = '100%',
            label = NULL,
            placeholder = 'search (regex)'
          ),
          style = 'margin-top:0px; margin-bottom:-15px;'
        )
      ),
      column(
        width = 2,
        style = 'margin-left:0px; margin-right:0px;padding-left:0px',
        actionButton(
          width = '100%',
          style = 'padding: 6px 0px',
          inputId = ns('clear_selection'),
          label = NULL,
          icon = icon("circle-minus")
        )
      )
    ),
    selectInput(
      inputId = ns("selectInput"),
      width = '100%',
      label = NULL,
      choices=c('none'),
      size = 20,
      multiple = multiple,
      selectize = FALSE
      )
    )
}
selectInput_server <- function(id,
                               all_cols,
                               feature_spec,
                               initial_selected,
                               update) {
  moduleServer(id, function(input, output, session) {
    startup <- reactiveVal(initial_selected)
    observe({
      update()
      choices <- selectInput_choices(
        input$selectChooser,
        input$selectChooserGroup,
        all_cols(),
        feature_spec(),
        input$search,
        update()
      )
      x <- isolate(input$selectInput)
      s <- isolate(input$search)
      if(!is.null(choices)){
        if(startup()==TRUE){
          selected <- NULL
          startup(FALSE)
        } else if(is.null(x)){
          selected <- NULL
        } else if (isTruthy(s)) {
          selected <- unlist(choices)[1]
        } else if (x %in% unlist(choices)) {
          selected <- x
        } else {
          selected <- unlist(choices)[1]
        }
        updateSelectInput(session, inputId = 'selectInput', choices = choices, selected = selected)
      }
    })
    observeEvent(input$clear_selection, {
      updateTextInput(session, inputId = 'search', value = '')
      updateSelectInput(session, inputId = 'selectInput', selected = character(0))
    })
    return(reactive({input$selectInput}))
  })
}
selectInput_choices <- function(selectChooser,
                                selectChooserGroup,
                                all_cols,
                                fs,
                                search,
                                update){
  feature <- NULL
  interaction_grouping <- NULL
  idx <- NULL
  # first establish feature order
  if (selectChooser=='Original'){
    # whatever was supplied
    choices <- all_cols
  } else if(selectChooser=='A-Z'){
    # alphabetical
    choices <- sort(all_cols)
  } else if(selectChooser=='lucidum'){
    # lucidum cols first
    recent_models <- intersect(all_cols, c('glm_prediction','lgbm_prediction'))
    glm_cols <- all_cols[grep('glm', all_cols)]
    LP_cols <- all_cols[grep('glm_LP', all_cols)]
    glm_cols <- setdiff(glm_cols, c(LP_cols, 'glm_prediction'))
    lgbm_cols <- all_cols[grep('lgbm', all_cols)]
    SHAP_cols <- all_cols[grep('lgbm_SHAP', all_cols)]
    lgbm_cols <- setdiff(lgbm_cols, c(SHAP_cols, 'lgbm_prediction'))
    other_cols <- setdiff(all_cols, c(recent_models, glm_cols, lgbm_cols, LP_cols, SHAP_cols))
    choices <- c(recent_models, glm_cols, lgbm_cols, LP_cols, SHAP_cols, other_cols)
  }
  # put features satisfying search at top of choices
  search_choices <- NULL
  if(!is.null(search) & search!=''){
    search_choices <- tryCatch({all_cols[grepl(search, all_cols)]}, error = function(e){e})
    if(class(choices)=='simpleError'){
      search_choices <- NULL
      choices <- all_cols
    } else {
      if(selectChooserGroup=='No groups'){
        choices <- c(search_choices, '-----', setdiff(all_cols, search_choices))
      } else {
        choices <- c(search_choices, setdiff(all_cols, search_choices))
      }
    }
  }
  # apply grouping if selected and it's possible to do so
  if (selectChooserGroup=='Use groups' &
      !is.null(fs) &
      !is.null(choices)
      ){
    if(nrow(fs)>0){
      choices_dt <- data.table(idx = 1:length(choices), feature = choices)
      setkey(choices_dt, feature)
      setkey(fs, feature)
      merged <- fs[, c('feature','interaction_grouping')][choices_dt][order(idx)]
      merged[is.na(interaction_grouping), interaction_grouping := 'No grouping']
      merged[, idx := NULL]
      # overwrite the grouping for any search terms
      if(!is.null(search) & search!='' & !is.null(search_choices)){
        merged[feature %in% search_choices, interaction_grouping := 'Search']
      }
      # overwrite the grouping for special lucidum cols
      if(selectChooser=='lucidum'){
        merged[feature %in% recent_models, interaction_grouping := 'Recent models']
        merged[feature %in% glm_cols, interaction_grouping := 'GLM columns']
        merged[feature %in% lgbm_cols, interaction_grouping := 'LGBM columns']
        merged[feature %in% LP_cols, interaction_grouping := 'GLM LP columns']
        merged[feature %in% SHAP_cols, interaction_grouping := 'LGBM SHAP columns']
      }
      # create the choices list
      choices <- split(merged, by = 'interaction_grouping', sorted = TRUE, keep.by = FALSE)
      choices <- lapply(choices, function(d){d[[1]]}) # convert to character list
      if(selectChooser=='A-Z'){
        choices <- lapply(choices, sort)
      }
      choices <- lapply(choices, list_if_length_one) # so selectInput choices look right
      if(names(choices)[1]==''){
        # selectInput lists must be named
        names(choices)[1] <- 'No grouping'
      }
    }
    # specify list order
    n <- names(choices)
    if(selectChooser!='lucidum'){
      if('No grouping' %in% n){
        choices <- choices[c(setdiff(n, 'No grouping'), 'No grouping')]
      }
      if('Search' %in% n){
        choices <- choices[c('Search', setdiff(n, 'Search'))]
      }
    } else {
      lucidum_cols <- intersect(c('Recent models','GLM columns','LGBM columns','GLM LP columns','LGBM SHAP columns'), names(choices))
      if('No grouping' %in% n){
        choices <- choices[c(lucidum_cols, setdiff(n, c(lucidum_cols, 'No grouping')), 'No grouping')]
      } else {
        choices <- choices[c(lucidum_cols, setdiff(n, lucidum_cols))]
      }
      if('Search' %in% n){
        choices <- choices[c('Search', setdiff(n, 'Search'))]
      }
    }
    # if(selectChooser!='lucidum'){
    #   if(nrow(fs)>0){
    #     choices_dt <- data.table(idx = 1:length(choices), feature = choices)
    #     setkey(choices_dt, feature)
    #     setkey(fs, feature)
    #     merged <- fs[, c('feature','interaction_grouping')][choices_dt]
    #     merged[is.na(interaction_grouping), interaction_grouping := 'no grouping']
    #     merged[, idx := NULL]
    #     if(!is.null(search) & search!='' & !is.null(search_choices)){
    #       merged[feature %in% search_choices, interaction_grouping := 'Search']
    #     }
    #     choices <- split(merged, by = 'interaction_grouping', sorted = TRUE, keep.by = FALSE)
    #     choices <- lapply(choices, function(d){d[[1]]}) # convert to character list
    #     if(selectChooser=='A-Z'){
    #       choices <- lapply(choices, sort)
    #     }
    #     choices <- lapply(choices, list_if_length_one) # so selectInput choices look right
    #     if(names(choices)[1]==''){
    #       # selectInput lists must be named
    #       names(choices)[1] <- 'no grouping'
    #     }
    #   }
    # } else {
    #   if(!is.null(search_choices)){
    #     choices <- list('Search' = list_if_length_one(search_choices),
    #                     'GLM columns' = list_if_length_one(setdiff(glm_cols, LP_cols)),
    #                     'LGBM columns' = list_if_length_one(setdiff(lgbm_cols, SHAP_cols)),
    #                     'Other columns' = list_if_length_one(other_cols),
    #                     'SHAP columns' = list_if_length_one(SHAP_cols),
    #                     'LP columns' = list_if_length_one(LP_cols)
    #     )
    #   } else {
    #     choices <- list('GLM columns' = list_if_length_one(setdiff(glm_cols, LP_cols)),
    #                     'LGBM columns' = list_if_length_one(setdiff(lgbm_cols, SHAP_cols)),
    #                     'Other columns' = list_if_length_one(other_cols),
    #                     'SHAP columns' = list_if_length_one(SHAP_cols),
    #                     'LP columns' = list_if_length_one(LP_cols)
    #     )
    #   }
    # }
  }
  return(choices)
}
selectInput_demo <- function(){
  ui <- fluidPage(
    tags$head(tags$style(
      HTML('
        body, label, input, button, select {
            background-color: grey;
        }')
    )),
    br(),
    column(
      width = 2,
      textOutput('clicked'),
      selectInput(inputId = 'dataset', label = 'Choose dataset', choices = c('MM_home','iris','mtcars'), selected = 'MM_home'),
      selectInput_ui(id = 'response', label = 'Response', height_divisor = 50, height_adj = 2),
      actionButton(inputId = 'clear_feature_spec', label = 'Clear feature spec')
    )
  )
  server <- function(input, output, session) {
    d <- reactiveVal()
    RVs <- reactiveValues(feature_spec = NULL)
    observeEvent(input$dataset, {
      d(setDT(get(input$dataset)))
      RVs$feature_spec <- load_specification(d(), input$dataset, '~/Dropbox/MappaR/Datasets', 'feature')
    })
    clicked <- selectInput_server("response",
                       reactive(names(d())),
                       reactive(RVs$feature_spec),
                       NULL)
    observe({
      output$clicked <- renderText(clicked())
    })

    observeEvent(input$clear_feature_spec, {
      RVs$feature_spec <- NULL
    })
  }
  shinyApp(ui, server)
}
list_if_length_one <- function(x){if(length(x)==1){x<-list(x)}else{x}}

