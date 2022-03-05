one_way_table_ui <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns('myTable'))
}
one_way_table_server <- function(id,
                                 d,
                                 response,
                                 additional_cols,
                                 weight,
                                 group_by_col,
                                 banding,
                                 sort_order,
                                 hide_low_exposure,
                                 show_partial_dependencies,
                                 response_transform,
                                 train_test_filter,
                                 user_filter,
                                 kpi_spec,
                                 feature_spec,
                                 dt_update,
                                 lgbm_objective,
                                 glm_objective) {
  moduleServer(id, function(input, output, session) {
    RVs <- reactiveValues(summary = NULL)
    observe({
      dt_update()
      RVs$summary <- viz_one_way_table(isolate(d()),
                                       response(),
                                       additional_cols(),
                                       weight(),
                                       group_by_col(),
                                       banding(),
                                       sort_order(),
                                       hide_low_exposure(),
                                       show_partial_dependencies(),
                                       response_transform(),
                                       train_test_filter(),
                                       user_filter(),
                                       kpi_spec(),
                                       feature_spec(),
                                       dt_update(),
                                       isolate(lgbm_objective()),
                                       isolate(glm_objective())
                                       )
    })
    output$myTable <- DT::renderDataTable({
      DT_one_way_table(RVs$summary, response(), weight(), kpi_spec(), response_transform())
      })
    return(reactive({RVs$summary}))
  })
}
one_way_table_demo <- function() {
  ui <- fluidPage(
    tags$head(tags$style(
      HTML('
        body, label, input, button, select {
            background-color: grey;
        }')
    )),
    br(),
    column(
      width = 3,
      radioGroupButtons(inputId = 'sort_order', label = 'Sort order', choices = c('A-Z','Weight','Actual','Add'), selected = 'A-Z'),
      selectInput(inputId = 'dataset', label = 'Choose dataset', choices = c('iris','mtcars','MM_home'), selected = 'mtcars'),
      selectInput(inputId = 'response', label = 'Response', choices = NULL, selected = NULL, multiple = FALSE, size = 5, selectize = FALSE),
      selectInput(inputId = 'additional', label = 'Additional cols', choices = NULL, selected = NULL, multiple = TRUE, size = 5, selectize = FALSE),
      selectInput(inputId = 'weight', label = 'Weight', choices = NULL, selected = NULL, multiple = FALSE, size = 5, selectize = FALSE),
      selectInput(inputId = 'group_by_col', label = 'Group by', choices = NULL, selected = NULL, multiple = FALSE, size = 5, selectize = FALSE),
      textInput(inputId = 'banding', label = 'Banding', value = 1)
    ),
    column(
      width = 9,
      one_way_table_ui("demo_table")
      )
    )
  server <- function(input, output, session) {
    d <- reactiveVal()
    observeEvent(input$dataset,{
      d(setDT(get(input$dataset)))
    })
    observeEvent(d(),{
      updateSelectInput(session, inputId = 'response', choices = numerical_cols(d()))
      updateSelectInput(session, inputId = 'additional', choices = numerical_cols(d()))
      updateSelectInput(session, inputId = 'weight', choices = c('N', 'no weights', numerical_cols(d())))
      updateSelectInput(session, inputId = 'group_by_col', choices = names(d()))
    })
    one_way_table_server("demo_table",
                         isolate(d),
                         reactive(input$response),
                         reactive(input$additional),
                         reactive(input$weight),
                         reactive(input$group_by_col),
                         reactive(as.numeric(input$banding)),
                         reactive(input$sort_order)
                         )
  }
  shinyApp(ui, server)
}
DT_one_way_table <- function(d_summary, response, weight, kpi_spec, response_transform){
  if(!is.null(d_summary) &
     response %in% names(d_summary) &
     weight %in% c('N', 'no weights', names(d_summary))
     ){
    d_display <- copy(d_summary)
    num_cols <- ncol(d_display)
    # add a total row
    total_row_firstcol <- data.table(V1='Total')
    setnames(total_row_firstcol, 'V1', names(d_summary)[1])
    if(weight=='N'){
      cols1 <- names(d_summary)[2]
      cols2 <- names(d_summary)[3:num_cols]
      summary1 <- d_display[, lapply(.SD, sum, na.rm = TRUE), .SDcols = cols1]
      wts <- d_display[[2]]
      summary2 <- d_display[, lapply(.SD, function(x){sum(x*wts, na.rm = TRUE)/sum(wts, na.rm = TRUE)}), .SDcols = cols2]
      total_row <- cbind(total_row_firstcol, summary1, summary2)
    } else if (weight=='no weights'){
      cols1 <- names(d_summary)[2:num_cols]
      summary1 <- d_display[, lapply(.SD, sum, na.rm = TRUE), .SDcols = cols1]
      total_row <- cbind(total_row_firstcol, summary1)
    } else {
      cols1 <- names(d_summary)[2:3]
      cols2 <- names(d_summary)[4:num_cols]
      summary1 <- d_display[, lapply(.SD, sum, na.rm = TRUE), .SDcols = cols1]
      wts <- d_display[[3]]
      summary2 <- d_display[, lapply(.SD, function(x){sum(x*wts, na.rm = TRUE)/sum(wts, na.rm = TRUE)}), .SDcols = cols2]
      total_row <- cbind(total_row_firstcol, summary1, summary2)
    }
    d_display <- rbind(d_display, total_row)
    num_rows <- nrow(d_display)
    if(!is.null(kpi_spec)){
      if(weight=='N'){
        cols <- names(d_display)[2]
      } else {
        cols <- names(d_display)[2:3]
      }
      if(weight %in% c('N','no weights')){
        cols1 <- names(d_display)[2]
        cols2 <- names(d_display)[3:num_cols]
      } else {
        cols1 <- names(d_display)[2:3]
        cols2 <- names(d_display)[4:num_cols]
      }
      d_display[, (cols1) := lapply(.SD, format, nsmall = 0, digits=1, big.mark=',', scientific = FALSE), .SDcols = cols1] # prettier
      if(response_transform=='-'){
        d_display[, (cols2) := lapply(.SD, apply_kpi_format, response, weight, kpi_spec), .SDcols = cols2] # prettier
      } else {
        d_display[, (cols2) := lapply(.SD, format, nsmall = 0, digits=3, big.mark=',', scientific = FALSE), .SDcols = cols2] # prettier
      }
      d_display[, (cols1) := lapply(.SD, as.character), .SDcols = cols1]
      d_display[, (cols2) := lapply(.SD, as.character), .SDcols = cols2]
    }
    dt <- DT::datatable(
      data = d_display,
      rownames= FALSE,
      extensions = 'Buttons',
      options = list(
        pageLength = num_rows,
        dom = 'Bfrt',
        scrollX = T,
        scrollY = 'calc(100vh - 453px)',
        columnDefs = list(list(width = '300px', targets = 0),
                          list(width = '100px', targets = 1),
                          list(className = 'dt-right', targets = '_all')
                          ),
        buttons =
          list('copy', list(
            extend = 'collection',
            buttons = list(list(extend='csv',filename = ''),
                           list(extend='excel',filename = ''),
                           list(extend='pdf',filename= '')),
            text = 'Download')
          )
        )
    ) %>%
      DT::formatStyle(names(d_summary), fontSize = '90%', lineHeight='40%') %>%
      DT::formatStyle(names(d_summary), target = "row", fontWeight = DT::styleEqual('Total', 'bold'))
  } else {
    dt <- NULL
  }
  return(dt)
}
viz_one_way_table <- function(d,
                              response_col,
                              additional_cols,
                              weight_col,
                              group_by_col,
                              banding,
                              sort_order,
                              hide_low_exposure,
                              show_partial_dependencies,
                              response_transform,
                              train_test_filter,
                              user_filter,
                              kpi_spec,
                              feature_spec,
                              dt_update,
                              lgbm_objective,
                              glm_objective){
  summary_col <- NULL
  LP_mean <- NULL
  original_group_by_col <- group_by_col
  if(!is.null(d) &
     !is.null(response_col) &
     !is.null(weight_col) &
     !is.null(group_by_col) &
     !is.null(train_test_filter) &
     !is.null(user_filter)
     ){
    if(response_col %in% names(d) &
       weight_col %in% c('N', 'no weights', names(d)) &
       group_by_col %in% names(d) &
       all(additional_cols %in% names(d))
       ){
      g <- d[[group_by_col]]
      numeric_group_by_col <- FALSE
      rows_to_summarise <- which(train_test_filter*user_filter==1)
      if(is.numeric(g)){
        # band the numerical variable for plotting
        numeric_group_by_col <- TRUE
        new_colname <- paste0(group_by_col, '_banded')
        banded <- floor(g/banding) * banding
        group_by_col <- banded[rows_to_summarise]
      } else if (class(g)[1] %in% c('IDate','Date','POSIXct')){
        numeric_group_by_col <- TRUE
        if(banding<=1){
          # day
          banded <- g
          new_colname <- paste0(group_by_col, '_day')
        } else if (banding==2){
          # week
          banded <- 100*year(g) + week(g)
          new_colname <- paste0(group_by_col, '_week')
        } else if (banding==4){
          # month
          banded <- 100*year(g) + month(g)
          new_colname <- paste0(group_by_col, '_month')
        } else if (banding==5){
          # quarter
          banded <- 100*year(g) + floor((month(g)-1)/3)+1
          new_colname <- paste0(group_by_col, '_quarter')
        } else if (banding>=10){
          # year
          banded <- year(g)
          new_colname <- paste0(group_by_col, '_year')
        }
        group_by_col <- banded[rows_to_summarise]
      }
      if(weight_col %in% c('N','no weights')){
        cols_to_summarise <- c(response_col, additional_cols)
      } else {
        cols_to_summarise <- c(weight_col, response_col, additional_cols)
      }
      # main summary
      if(length(rows_to_summarise)==nrow(d)){
        d_summary <- d[, c(count = .N, lapply(.SD, sum, na.rm = TRUE)), group_by_col, .SDcols = cols_to_summarise]
      } else {
        d_summary <- d[rows_to_summarise, c(count = .N,lapply(.SD, sum, na.rm = TRUE)), group_by_col, .SDcols = cols_to_summarise]
      }
      # extract weighted mean as needed later on to calibrate SHAP values
      if(weight_col %in% c('N','no weights')){
        wtd_mean <- sum(d_summary[,3], na.rm = TRUE)/sum(d_summary[,2], na.rm = TRUE)
      } else {
        wtd_mean <- sum(d_summary[,4], na.rm = TRUE)/sum(d_summary[,3], na.rm = TRUE)
      }
      last_non_SHAP_col <- ncol(d_summary)
      # grab objectives used
      objective <- NULL
      link <- lgbm_objectives[objective==lgbm_objective][['link']]
      link_glm <- glm_objectives[objective==glm_objective][['link']]
      # SHAP summary
      SHAP_col <- NULL
      if (show_partial_dependencies %in% c('GBM','Both')){
        SHAP_col <- paste0('lgbm_SHAP_', original_group_by_col)
        if(!(SHAP_col %in% names(d))){
          SHAP_col <- NULL
        }
      }
      if(!is.null(SHAP_col)){
        if(length(rows_to_summarise)==nrow(d)){
          SHAP_summary <- d[,c(min = lapply(.SD, min, na.rm = TRUE),
                               perc_5 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.05),
                               mean = lapply(.SD, mean, na.rm = TRUE),
                               perc_95 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.95),
                               max = lapply(.SD, max, na.rm = TRUE)
                               ),
                               group_by_col,
                               .SDcols = SHAP_col]
        } else {
          SHAP_summary <- d[rows_to_summarise,
                            c(min = lapply(.SD, min, na.rm = TRUE),
                              perc_5 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.05),
                              mean = lapply(.SD, mean, na.rm = TRUE),
                              perc_95 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.95),
                              max = lapply(.SD, max, na.rm = TRUE)
                              ),
                            group_by_col,
                            .SDcols = SHAP_col]
          }
        names(SHAP_summary)[2:6] <- c('min','perc_5','mean','perc_95','max')
        # scale SHAP values to mean
        # how to do this depends on the choice of objective
        if(link=='identity'){
          SHAP_summary[, 2:6] <- wtd_mean + SHAP_summary[, 2:6]
        } else if (link=='log'){
          SHAP_summary[, 2:6] <- exp(SHAP_summary[, 2:6]) * wtd_mean
        } else if (link=='logit'){
          # following is rough for now - won't always tie up due to logit
          SHAP_summary[, 2:6] <- exp(SHAP_summary[, 2:6])/(1+exp(SHAP_summary[, 2:6])) * wtd_mean * 2
        }
        # multiply SHAP_summary by row weights
        # needed for later weighted average removal of rows
        if(weight_col=='N'){
          SHAP_summary[, 2:6] <- SHAP_summary[, 2:6] * d_summary[[2]]
        } else {
          SHAP_summary[, 2:6] <- SHAP_summary[, 2:6] * d_summary[[3]]
        }
        d_summary <- cbind(d_summary, SHAP_summary[, 2:6])
      }
      # LP summary
      LP_col <- NULL
      if (show_partial_dependencies %in% c('GLM','Both')){
        LP_col <- paste0('glm_LP_', original_group_by_col)
        if(!(LP_col %in% names(d))){
          LP_col <- NULL
        }
      }
      if(!is.null(LP_col)){
        if(length(rows_to_summarise)==nrow(d)){
          LP_summary <- d[, lapply(.SD, mean, na.rm = TRUE), group_by_col, .SDcols = LP_col]
        } else {
          LP_summary <- d[rows_to_summarise, lapply(.SD, mean, na.rm = TRUE), group_by_col, .SDcols = LP_col]
        }
        names(LP_summary)[2] <- c('LP_mean')
        # scale SHAP values to mean
        # how to do this depends on the choice of objective
        if(link_glm=='identity'){
          LP_summary[, 2] <- wtd_mean + LP_summary[, 2]
        } else if (link_glm=='log'){
          LP_summary[, 2] <- exp(LP_summary[, 2]) * wtd_mean
        } else if (link_glm=='logit'){
          # following is rough for now - won't always tie up due to logit
          LP_summary[, 2] <- exp(LP_summary[, 2])/(1+exp(LP_summary[, 2])) * wtd_mean * 2
        }
        # multiply LP_summary by row weights
        # needed for later weighted average removal of rows
        if(weight_col=='N'){
          LP_summary[, 2] <- LP_summary[, 2] * d_summary[[2]]
        } else {
          LP_summary[, 2] <- LP_summary[, 2] * d_summary[[3]]
        }
        d_summary <- cbind(d_summary, LP_summary[, 2])
      }
      # correct name if grouping by a numerical quantity
      if(numeric_group_by_col){
        names(d_summary)[1] <- new_colname
      }
      # group low exposure rows and remove from summary
      # will add on the low exposure row summary later
      min_exposure <- 0
      if(hide_low_exposure!='0' & weight_col != 'no weights'){
        if(hide_low_exposure=='1%'){
          if(weight_col=='N'){
            min_exposure <- 0.01 * sum(d_summary[,2])
          } else {
            min_exposure <- 0.01 * sum(d_summary[,3])
          }
        } else {
          min_exposure <- as.numeric(hide_low_exposure)
        }
        if(weight_col=='N'){
          low_exposure_rows <- which(d_summary[[2]]<min_exposure)
        } else {
          low_exposure_rows <- which(d_summary[[3]]<min_exposure)
        }
        low_exposure_summary <- d_summary[low_exposure_rows,
                                          lapply(.SD, sum, na.rm = TRUE),
                                          .SDcols = 2:ncol(d_summary)
        ]
        if(nrow(low_exposure_summary)>0){
          low_exposure_summary[, summary_col := 999999999] # I can't think of a better way right now
          setcolorder(low_exposure_summary, 'summary_col')
          setnames(low_exposure_summary, 'summary_col', names(d_summary)[1])
          d_summary <- d_summary[-low_exposure_rows,]
        }
      }
      # divide by weight if specified
      if(weight_col == 'N'){
        first_col <- 3
        # divide all summary columns (3rd onwards) by the weight column (2nd)
        d_summary[, first_col:ncol(d_summary)] <- d_summary[, first_col:ncol(d_summary)] / d_summary[[2]]
        if(hide_low_exposure!='0' & weight_col != 'no weights'){
          if(nrow(low_exposure_summary)>0){
            low_exposure_summary[, first_col:ncol(d_summary)] <- low_exposure_summary[, first_col:ncol(d_summary)] / low_exposure_summary[[2]]
          }
        }
      } else if (weight_col != 'no weights'){
        first_col <- 4
        # divide all summary columns (4rd onwards) by the weight column (3rd)
        d_summary[, first_col:ncol(d_summary)] <- d_summary[, first_col:ncol(d_summary)] / d_summary[[3]]
        if(hide_low_exposure!='0' & weight_col != 'no weights'){
          if(nrow(low_exposure_summary)>0){
            low_exposure_summary[, first_col:ncol(d_summary)] <- low_exposure_summary[, first_col:ncol(d_summary)] / low_exposure_summary[[3]]
          }
        }
      }
      # append the low exposure row if present
      if(hide_low_exposure!='0' & weight_col != 'no weights'){
        if(nrow(low_exposure_summary)>0){
          d_summary <- rbind(d_summary, low_exposure_summary)
        }
      }
      # apply response transformation
      if(response_transform=='Log'){
        d_summary[, first_col:ncol(d_summary)] <- log(d_summary[, first_col:ncol(d_summary)])
      } else if (response_transform=='Exp'){
        d_summary[, first_col:ncol(d_summary)] <- exp(d_summary[, first_col:ncol(d_summary)])
      } else if (response_transform=='Logit'){
        d_summary[, first_col:ncol(d_summary)] <- log(d_summary[, first_col:ncol(d_summary)]/(1-d_summary[, first_col:ncol(d_summary)]))
      } else if (response_transform=='Base'){
        base_level <- feature_spec$base_level[feature_spec$feature==original_group_by_col]
        if(!shiny::isTruthy(base_level)) base_level <- character(0)
        if(length(base_level)>0){
          if(is.numeric(d[[original_group_by_col]])){
            base_level <- as.numeric(base_level)
            base_level_banded <- floor(base_level/banding) * banding
          }
          idx <- which(d_summary[[1]]==base_level)
          if(length(idx)==1){
            cols <- names(d_summary)[first_col:ncol(d_summary)]
            denominator <- d_summary[idx, .SD, .SDcols = cols]
            if(!is.null(SHAP_col)){
              denominator[, (c('min','perc_5','perc_95','max')) := mean] # what to adjust to the mean, not the percentiles
            }
            if(link_glm %in% c('identity','logit')){
              rebased_values <- as.data.table(mapply('-',d_summary[, .SD, .SDcols=cols], denominator))
            } else {
              rebased_values <- as.data.table(mapply('/',d_summary[, .SD, .SDcols=cols], denominator))
            }
            d_summary[, (cols) := rebased_values]
          }
        }
      }
      # sort table
      if(sort_order=='A-Z'){
        setorderv(d_summary, names(d_summary)[1])
        # if first row is NA make last row
        if(is.na(d_summary[[1]][1])){
          d_summary <- rbind(d_summary[2:.N], d_summary[1,])
        }
      } else if (sort_order=='Wt'){
        col <- ifelse(weight_col=='N',2,3)
        setorderv(d_summary, names(d_summary)[col], -1)
      } else if (sort_order=='Act'){
        col <- ifelse(weight_col %in% c('N','no weights'),3,4)
        setorderv(d_summary, names(d_summary)[col], -1)
      } else if (sort_order=='Add' & ncol(d_summary)>=4){
        col <- ifelse(weight_col=='N',4,5)
        setorderv(d_summary, names(d_summary)[col], -1)
      } else if (sort_order=='PD'){
        if('LP_mean' %in% names(d_summary)){
          setorder(d_summary, -LP_mean)
        } else if ('mean' %in% names(d_summary)){
          setorder(d_summary, -mean)
        } else {
          setorderv(d_summary, names(d_summary)[1])
          # if first row is NA make last row
          if(is.na(d_summary[[1]][1])){
            d_summary <- rbind(d_summary[2:.N], d_summary[1,])
          }
        }
      }
      # make the first column character ensuring not scientifc
      if(class(d_summary[[1]])=='numeric'){
        d_summary[[1]] <- prettyNum(d_summary[[1]], scientific = FALSE)
      } else {
        d_summary[[1]] <- as.character(d_summary[[1]])
      }
      # change the low exposure row
      low_exposure_row <- which(d_summary[[1]]=='999999999')
      d_summary[[1]][low_exposure_row] <- 'X'
      # only use if fewer than 5,000 rows
      if(nrow(d_summary)>5000){
        d_summary <- data.table(V1='too many levels to display (>5,000)')
      }
    } else {
      d_summary <- data.table(V1='at least one input is NULL')
    }
  } else {
    d_summary <- data.table(V1='at least one input is NULL')
  }
  # replace NA with 'NA'
  d_summary[[1]][is.na(d_summary[[1]])] <- 'NA'
  # ensure names are unique
  names(d_summary) <- make.unique(names(d_summary))
  return(d_summary)
}
