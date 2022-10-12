#' @import broom
#' @import splines
#' @import statmod
#' @import readr
#' @import openxlsx
GlimmaR_server <- function(input, output, session, d, RVs){
  GlimmaR_model_index <- reactiveVal(0)
  GlimmaR_text_size <- reactiveVal(14)
  # observeEvent(c(input$response, input$weight), ignoreInit = TRUE, {
  #   if(!is.null(d())){
  #     guess <- objective_guesser(d(), input$response, input$weight, 'glm')
  #     updateSelectInput(session, inputId = 'GlimmaR_objective', selected = guess)
  #   }
  # })
  observeEvent(c(RVs$GlimmaR_models, GlimmaR_model_index()), ignoreInit = TRUE, {
    # update the GlimmaR summary table
    # update the GBM model summary table
    dt <- GlimmaR_model_summary(RVs$GlimmaR_models)
    model_index <- GlimmaR_model_index()
    output$GlimmaR_model_summary <- DT::renderDT({
      dt %>%
        DT::datatable(rownames= FALSE,
                      extensions = 'Buttons',
                      selection=list(mode="single", target="row", selected = c(model_index)),
                      options = list(pageLength = nrow(dt),
                                     initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '12px'});}"),
                                     dom = 'Bfrt',
                                     scrollX = T,
                                     scrollY = 'calc(50vh)',
                                     searchHighlight=TRUE,
                                     buttons =
                                       list('colvis', 'copy', list(
                                         extend = 'collection',
                                         buttons = list(list(extend='csv',filename = ''),
                                                        list(extend='excel',filename = ''),
                                                        list(extend='pdf',filename= '')),
                                         text = 'Download')
                                       )
                      )
        ) %>%
        DT::formatStyle(columns = colnames(dt), lineHeight='40%', fontSize = '80%')
    })

    # update tabulated models selection
    curr_selection <- input$GlimmaR_model_chooser
    choices <- GlimmaR_tabulated_model_list(RVs$GlimmaR_models)
    if(length(curr_selection)==0){
      selected <- choices[1]
    } else {
      if(curr_selection %in% choices){
        selected <- curr_selection
      } else {
        selected <- choices[1]
      }
    }
    updateSelectInput(session, inputId = 'GlimmaR_model_chooser', choices = choices, selected = choices[length(choices)])
  })
  observeEvent(input$GlimmaR_model_chooser, ignoreInit = TRUE, {
    curr_selection <- input$GlimmaR_table_chooser
    choices <- GlimmaR_model_table_list(RVs$GlimmaR_models[[as.numeric(input$GlimmaR_model_chooser)]])
    if(length(curr_selection)==0){
      selected <- choices[1]
    } else {
      if(curr_selection %in% choices){
        selected <- curr_selection
      } else {
        selected <- choices[1]
      }
    }
    updateSelectInput(session, inputId = 'GlimmaR_table_chooser', choices = choices, selected = selected)
  })
  observeEvent(input$GlimmaR_textsize_minus, {
    GlimmaR_text_size(pmax(8,GlimmaR_text_size()-1))
    session$sendCustomMessage("glm_formula_text_size", paste(sep = '', GlimmaR_text_size(), 'px'))
  })
  observeEvent(input$GlimmaR_textsize_plus, {
    GlimmaR_text_size(pmin(30,GlimmaR_text_size()+1))
    session$sendCustomMessage("glm_formula_text_size", paste(sep = '', GlimmaR_text_size(), 'px'))
  })
  observeEvent(input$GlimmaR_formula_save, {
    volumes <- c('Home' = fs::path_home(), shinyFiles::getVolumes()())
    shinyFileSave(input, "GlimmaR_formula_save", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$GlimmaR_formula_save)
    if (nrow(fileinfo) > 0) {
      fileConn<-file(fileinfo$datapath)
      writeLines(isolate(input$GlimmaR_glm_formula), fileConn)
      close(fileConn)
    }
  })
  observeEvent(input$GlimmaR_formula_load, {
    volumes <- c('Home' = fs::path_home(), shinyFiles::getVolumes()())
    shinyFileChoose(input, "GlimmaR_formula_load", roots=volumes, session=session)
    fileinfo <- parseFilePaths(volumes, input$GlimmaR_formula_load)
    if (nrow(fileinfo) > 0) {
      glm_formula <- readr::read_file(file = fileinfo$datapath)
      updateTextAreaInput(session = session, inputId = 'GlimmaR_glm_formula', label = NULL, value = glm_formula)
    }
  })
  observeEvent(input$GlimmaR_navigate, {
    updateTabItems(session, inputId = 'tabs', selected = 'GlimmaR')
    updateNavbarPage(session = session, inputId = "GlimmaR_tabsetPanel", selected = "Model history")
  })
  observeEvent(input$GlimmaR_prev_model, {
    if(length(RVs$GlimmaR_models)>0){
      GlimmaR_model_index(max(1,GlimmaR_model_index()-1))
    }
  })
  observeEvent(input$GlimmaR_next_model, {
    if(length(RVs$GlimmaR_models)>0){
      n <- length(RVs$GlimmaR_models)
      GlimmaR_model_index(min(n,GlimmaR_model_index()+1))
    }
  })
  observeEvent(GlimmaR_model_index(), ignoreInit = TRUE, {
    if(length(RVs$GlimmaR_models)>0){
      updateActionButton(session, inputId = 'GlimmaR_navigate', label = paste0('GLMs (', GlimmaR_model_index(),'/',length(RVs$GlimmaR_models),')'))
      g <- RVs$GlimmaR_models[[GlimmaR_model_index()]]
      # update the formula text
      formula_text <- g$formula
      updateTextAreaInput(session, inputId = 'GlimmaR_glm_formula', value = formula_text)
      # update the ChartaR secondary column
      glm_prediction <- NULL
      d()[, glm_prediction := .SD, .SDcols = g$predict_col_name]
      if(g$predict_col_name %in% names(d())){
        updateSelectInput(session, inputId = 'ChartaR_1W_add_columns-selectInput', selected = g$predict_col_name)
      }
      # update the response and weight
      use_kpi <- FALSE
      # if(!is.null(g$kpi)){
      #   if(g$kpi %in% RVs$kpi_spec[['kpi_name']]){
      #     use_kpi <- TRUE
      #   }
      # }
      if(use_kpi){
        updateSelectInput(session, inputId = 'sidebar_kpi', selected = g$kpi)
      } else {
        updateSelectInput(session, inputId = 'response', selected = g$response)
        updateSelectInput(session, inputId = 'weight', selected = g$weight)
        updateSelectInput(session, inputId = 'sidebar_kpi', selected = g$kpi)
      }
      # update objective
      updateSelectInput(session, inputId = 'GlimmaR_objective', selected = g$objective)
      # update the LP cols
      if(!is.null(g$non_zero_weight_rows) & !is.null(g$LP_contributions)){
        LP_cols <- names(d())[grep('glm_LP', names(d()))] # get rid of any existing LP columns
        if(length(LP_cols)>0){
          d()[, (LP_cols) := NULL]
        }
        # append on new LP cols
        LP_cols <- names(g$LP_contributions)
        d()[g$non_zero_weight_rows, (LP_cols) := g$LP_contributions]
      }
    }
  })
  observeEvent(input$GlimmaR_model_summary_cell_clicked$row, {
    model_index <- input$GlimmaR_model_summary_cell_clicked$row
    GlimmaR_model_index(model_index)
  })
  output$GlimmaR_glm_coefficients <- DT::renderDataTable({
    if(GlimmaR_model_index()>0){
      g <- RVs$GlimmaR_models[[GlimmaR_model_index()]]
      c <- g$coefficients
      GlimmaR_coefficient_DT(c)
    }
  })
  output$GlimmaR_model_dispersion <- renderUI({
    if(GlimmaR_model_index()>0){
      g <- RVs$GlimmaR_models[[GlimmaR_model_index()]]
      if(is.null(g)){
        text <- ''
      } else {
        text <- paste0('<b>Dispersion: </b>', signif(g$dispersion,4))
      }
      p(HTML(text), style = 'font-size: 12px; margin-top: 26px')
    }
  })
  output$GlimmaR_model_NAs <- renderUI({
    if(GlimmaR_model_index()>0){
      g <- RVs$GlimmaR_models[[GlimmaR_model_index()]]
      if(g$count_NAs>0){
        text <- paste0('<b><span style=\"color:red\"><b>NAs in fitted: ', g$count_NAs, '</span>')
      } else {
        text <- paste0('<b>NAs in fitted</b>: 0')
      }
      p(HTML(text), style = 'font-size: 12px; margin-top: 26px')
    }
  })
  observe({
    volumes <- c('Home' = fs::path_home(), shinyFiles::getVolumes()())
    shinyFileSave(input, 'GlimmaR_save_model', roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$GlimmaR_save_model)
    isolate({
      if (nrow(fileinfo) > 0) {
        # leave only part of glm_model object needed for prediction
        # otherwise file will be huge (it retains data used to fit model)
        stripped_model <- strip_glm(RVs$GlimmaR_models[[GlimmaR_model_index()]]$model)
        saveRDS(stripped_model, file = fileinfo$datapath, compress = TRUE)
          confirmSweetAlert(session = session,
                            type = 'success',
                            inputId = "temp",
                            title = 'GLM saved',
                            btn_labels = c('OK')
          )
        }
    })
  })
  observe({
    if(length(RVs$BoostaR_models)>0){
      b <- RVs$BoostaR_models[[length(RVs$BoostaR_models)]]
    } else {
      b <- NULL
    }
    updateSelectInput(session,
                      inputId = 'GlimmaR_helper_feature',
                      choices = make_GlimmaR_helper_features(d(),
                                                             b,
                                                             input$GlimmaR_helper_feature_choice,
                                                             input$GlimmaR_helper_search
                                                             )
                      )
  })
  observe({
    updateSelectInput(session,
                      inputId = 'GlimmaR_helper_levels',
                      choices = make_GlimmaR_helper_levels(d(),
                                                           input$GlimmaR_helper_feature
                                                           )
    )
  })
  observe({
    updateTextAreaInput(session,
                        inputId = 'GlimmaR_formula_suggestion',
                        value = make_GlimmaR_formula_suggestion(
                          d = d(),
                          feature = input$GlimmaR_helper_feature,
                          options = input$GlimmaR_helper_levels,
                          level_grouping = input$GlimmaR_helper_levels_choice,
                          inputs = input$GlimmaR_helper_level_text
                          )
                        )
  })
  observeEvent(input$GlimmaR_build_GLM, {
    # increment the GlimmaR_model_index
    # model_index <- GlimmaR_model_index() + 1
    model_index <- length(RVs$GlimmaR_models) + 1
    # build LGM
    GlimmaR_model <- GlimmaR_build_GLM(session,
                                       d(),
                                       input$response,
                                       input$weight,
                                       input$GlimmaR_data_to_use,
                                       input$GlimmaR_glm_formula,
                                       input$GlimmaR_objective,
                                       model_index
                                       )
    if(!is.null(GlimmaR_model)){
      # increment the index
      GlimmaR_model_index(model_index)
      # add model to list
      GlimmaR_model$kpi <- input$sidebar_kpi
      GlimmaR_model$training_test <- input$GlimmaR_data_to_use
      GlimmaR_model$dataset_name <- RVs$dataset_name
      RVs$GlimmaR_models <- append(RVs$GlimmaR_models, list(GlimmaR_model))
      names(RVs$GlimmaR_models)[model_index] <- GlimmaR_model$predict_col_name
      # d has been updated, need to flag this
      RVs$dt_update <- RVs$dt_update + 1
    }
  })
  observeEvent(input$GlimmaR_goto_ChartaR, {
    last_clicked <- input$GlimmaR_glm_coefficients_cell_clicked$value
    # extracts first feature in the supplied GLM term
    if(!is.null(last_clicked)){
      n <- length(RVs$GlimmaR_models)
      all_model_variables <- all.vars(RVs$GlimmaR_models[[n]]$model$terms)
      feature <- NULL
      for (i in 1:length(all_model_variables)){
        present <- grep(all_model_variables[i], last_clicked, fixed = TRUE)
        if(length(present)>0){
          feature <- all_model_variables[i]
          break
        }
      }
      if(!is.null(feature)){
        if(feature %in% names(d()) & 'glm_prediction' %in% names(d())){
          updateSelectInput(session, inputId = 'ChartaR_x_axis_feature-selectInput', selected = feature)
          updateSelectInput(session, inputId = 'ChartaR_1W_add_columns-selectInput', selected = 'glm_prediction')
          updateTabItems(session, inputId = 'tabs', selected = 'ChartaR')
          updateNavbarPage(session = session, inputId = "ChartaR_tabsetPanel", selected = "One way summary")
        }
      }
    }
  })
  observeEvent(input$GlimmaR_tabulate, {
    #model_index <- length(RVs$GlimmaR_models)
    model_index <- GlimmaR_model_index()
    if(length(RVs$GlimmaR_models)>0){
      g <- RVs$GlimmaR_models[[model_index]]
      if(!is.null(g)){
        base_risk <- get_base_risk(d(), RVs$feature_spec, g$weight)
        withProgress(message = 'GlimmaR', detail = 'tabulate', value = 0.5, {
          RVs$GlimmaR_models[[model_index]]$tabulated_model <- export_model(d()[g$rows_used],
                                                                            g$model,
                                                                            base_risk,
                                                                            FALSE,
                                                                            input$GlimmaR_tabulate_scale,
                                                                            RVs$feature_spec,
                                                                            input$GlimmaR_tabulate_format,
                                                                            input$weight,
                                                                            input$response,
                                                                            d()[['glm_prediction']],
                                                                            model_name = input$sidebar_kpi
          )
        })
        updateNavbarPage(session = session, inputId = 'GlimmaR_tabsetPanel', selected = 'Tabulated models')
        confirmSweetAlert(session = session,
                          type = 'success',
                          inputId = "GlimmaR_sweet_alert",
                          title = "GLM tabulated",
                          text = paste0(length(RVs$GlimmaR_models[[model_index]]$tabulated_model), ' tables created'),
                          btn_labels = c('OK'))
      }
    }
  })
  output$GlimmaR_tabulated_model <- DT::renderDT({
    m_idx <- as.numeric(input$GlimmaR_model_chooser)
    t_name <- input$GlimmaR_table_chooser
    GlimmaR_format_table_DT(RVs$GlimmaR_models, m_idx, t_name, input$GlimmaR_transpose_table)
  })
  observeEvent(input$GlimmaR_export_tables, {
    # get the filename
    volumes <- c('Home' = fs::path_home(), shinyFiles::getVolumes()())
    shinyFileSave(input, "GlimmaR_export_tables", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$GlimmaR_export_tables)
    if(!is.null(input$GlimmaR_model_chooser)){
      m_idx <- as.numeric(input$GlimmaR_model_chooser)
      g_tabulated <- RVs$GlimmaR_models[[m_idx]]$tabulated_model
      # create and save the file
      if(length(fileinfo$datapath)>0){
        if(is.null(g_tabulated)){
          confirmSweetAlert(session = session,
                            type = 'error',
                            inputId = "glm_error",
                            title = "Error",
                            text = 'No GLM has been built',
                            btn_labels = c('OK'))
        } else {
          if(g_tabulated$format=='solo'){
            write_tables_to_excel(g_tabulated, fileinfo$datapath)
            confirmSweetAlert(session = session,
                              type = 'success',
                              inputId = "glm_error",
                              title = "Workbook saved",
                              text = '',
                              btn_labels = c('OK'))
          } else if (g_tabulated$format %in% c('long','long norm')){
            # can take a while for really big exports so use a withProgress
            withProgress(message = 'GlimmaR', detail = 'saving ratebook', value = 0.5, {
              # concatenate all of the tables into a single large table
              # minus one needed as last element of glm_exported_tables is the format, not a table
              concatenated_table <- g_tabulated[[1]]$table
              # keep a record of the number of rows in each table to colour in the exported workbook rows
              table_rows <- c(1) # first row is always the base level
              for (i in 2:(length(g_tabulated)-1)){
                nrows <- nrow(g_tabulated[[i]]$table) - 1 # because we don't include the summary row
                table_rows <- c(table_rows, nrows)
                concatenated_table <- rbind(concatenated_table, g_tabulated[[i]]$table[-1,])
              }
              # bind on a column called implementable which is a copy of the last column
              # this is because this extra column will be manually edited in a spreadsheet
              concatenated_table <- cbind(concatenated_table, implementable = concatenated_table$model_relativity)
              # create workbook to hold tables
              wb <- createWorkbook()
              ws <- 'GLM_EXPORT_LONG'
              addWorksheet(wb, ws)
              writeData(wb, ws, concatenated_table)
              # format workbook
              setColWidths(wb, ws, cols = 1:5, widths = 20)
              setColWidths(wb, ws, cols = 5:10, widths = 15)
              # define a white text on blue background style to use for all header rows
              header_style <- createStyle(fgFill = "#000000", fontColour = "#FFFFFF")
              colour1_style <- createStyle(fgFill = "#DCDCDC", fontColour = "#000000")
              colour2_style <- createStyle(fgFill = "#B4DCFF", fontColour = "#000000")
              addStyle(wb, sheet = ws, style=header_style, cols=1:10, rows=1)
              # loop to colour rows in workbook
              start_row <- 2
              colour_ind <- 0
              for (r in table_rows){
                if(colour_ind==0){
                  s <- colour1_style
                  colour_ind <- 1
                } else {
                  s <- colour2_style
                  colour_ind <- 0
                }
                end_row <- start_row + r - 1
                addStyle(wb, sheet = ws, style=s, rows=start_row:end_row, cols=1:10, gridExpand = T)
                start_row <- end_row + 1
              }
              # alignment and decimal places
              n <- nrow(concatenated_table) + 1
              addStyle(wb, sheet = ws, rows = 1:n, cols = 1:5, style = createStyle(halign = 'left'), gridExpand = TRUE, stack = TRUE)
              addStyle(wb, sheet = ws, rows = 1:n, cols = 6:10, style = createStyle(halign = 'right'), gridExpand = TRUE, stack = TRUE)
              addStyle(wb, sheet = ws, rows = 1:n, cols = 8, style = createStyle(numFmt = '#,##0'), gridExpand = TRUE, stack = TRUE)
              addStyle(wb, sheet = ws, rows = 1:n, cols = 9:10, style = createStyle(numFmt = '0.0000'), gridExpand = TRUE, stack = TRUE)
              # save and confirm - can take a few seconds so use withProgress
              saveWorkbook(wb, fileinfo$datapath, overwrite = TRUE)
            })
            confirmSweetAlert(session = session,
                              type = 'success',
                              inputId = "glm_error",
                              title = "Workbook saved",
                              text = '',
                              btn_labels = c('OK')
            )
          }
        }
      }
    }
  }
)}
