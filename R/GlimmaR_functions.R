GlimmaR_build_GLM <- function(session, d, response, weight, data_to_use, glm_formula, glm_objective, idx){
  l <- NULL
  if(!(response %in% names(d))){
  } else if (!(weight %in% c('N',names(d)))){
  } else if (is.null(d)){
  } else if (data_to_use=='Training only' & !('train_test' %in% names(d))){
    # training data selected but no train test column
    confirmSweetAlert(session = session,
                      type = 'error',
                      inputId = "build_error",
                      title = 'No train_test column',
                      text = 'Training only selected but there is no train_test column in the dataset',
                      btn_labels = c('OK')
    )
  } else {
    # attempt to turn text input into a formula
    original_glm_formula <- glm_formula
    glm_formula <- paste(response, ' ~ ', glm_formula)
    glm_formula <- tryCatch({stats::as.formula(glm_formula)},error = function(e){NULL})
    if(is.null(glm_formula)){
      confirmSweetAlert(session = session,
                        type = 'error',
                        inputId = "build_error",
                        title = 'Check model formula',
                        text = '',
                        btn_labels = c('OK')
      )
    } else if (all.vars(glm_formula)[1] %in% labels(stats::terms(glm_formula))){
      # response (left hand side) is also in formula (right hand side)
      confirmSweetAlert(session = session,
                        type = 'error',
                        inputId = "build_error",
                        title = 'Response contained in formula',
                        text = '',
                        btn_labels = c('OK')
      )
    } else if (any(all.vars(glm_formula) %in% names(d)[sapply(d, is.character)])){
      # formula contains character columns
      # R glm uses factor columns, not character
      confirmSweetAlert(session = session,
                        type = 'error',
                        inputId = "build_error",
                        title = 'Character columns in formula',
                        text = 'GLM requires factors: convert character columns to factors',
                        btn_labels = c('OK')
      )
    } else {
      # set the GLM family
      if (glm_objective=='gaussian'){
        family <- stats::gaussian(link='identity')
      } else if (glm_objective=='binomial'){
        family <- stats::binomial(link = 'logit')
      } else if (glm_objective=='poisson'){
        family <- stats::poisson(link = 'log')
      } else if (glm_objective=='gamma'){
        family <- stats::Gamma(link = 'log')
      } else if (glm_objective=='tweedie'){
        # link.power = 0 means log link which is usually what you want
        family <- statmod::tweedie(var.power = 1.1, link.power = 0)
      } else if (glm_objective=='quasipoisson'){
        family <- stats::quasipoisson(link = 'log')
      }
      # use whole dataset or just training
      if(data_to_use=='All rows'){
        include <- 1:nrow(d)
      } else {
        include <- which(d[['train_test']]==0)
      }
      # get rid of rows with non-zero weights for the model fit
      if(weight!='N'){
        non_zero_weight_rows <- which(d[[weight]]>0)
        include <- intersect(include, non_zero_weight_rows)
      } else {
        non_zero_weight_rows <- 1:nrow(d)
      }
      # build model
      withProgress(message = 'GlimmaR', detail = 'building model', value = 0.5,{
        start_time <- Sys.time()
        glm_model <- tryCatch({stats::glm(formula = glm_formula,
                                          model = FALSE,
                                          data = d[include],
                                          family = family)},
                              error = function(e){e})
        time <- as.numeric(difftime(Sys.time(), start_time, units = 's'))
        # check if something went wrong
        if(!(class(glm_model)[[1]]=='glm')){
          # something went wrong
          confirmSweetAlert(session = session,
                            type = 'error',
                            inputId = "build_error",
                            title = 'GLM build error',
                            text = glm_model$message,
                            btn_labels = c('OK')
          )
        } else {
          # get coefficient summary table in nice format
          c <- broom::tidy(glm_model)
          c$statistic <- NULL
          c[, 2:4] <- signif(c[,2:4])
          # predict on dataset
          incProgress(0.1, detail = 'predicting')
          fitted_glm <- tryCatch({stats::predict(glm_model, d, type = 'response')}, error = function(e){e})
          if(class(fitted_glm)=='simpleError'){
            confirmSweetAlert(session = session,
                              type = 'error',
                              inputId = "build_error",
                              title = 'GLM prediction error',
                              text = fitted_glm$message,
                              btn_labels = c('OK'))
          } else {
            # append predictions to main dataset
            fitted_glm[-non_zero_weight_rows] <- 0 # set predictions to zero for rows not used to build model
            count_NAs <- sum(is.na(fitted_glm[non_zero_weight_rows])) # in case we supplied a factor with NAs to the model
            predict_col_name <- paste(sep = '_', response, 'glm', idx)
            glm_prediction <- NULL
            d[, glm_prediction := fitted_glm]
            d[, (predict_col_name) := fitted_glm]
            # calculate linear predictor split by feature
            incProgress(0.1, detail = 'LP terms')
            LP_contributions <- gather_glm_terms(d[non_zero_weight_rows], glm_model)
            # if LP columns already exist, delete them
            LP_cols <- names(d)[grep('glm_LP', names(d))] # get rid of any existing SHAP columns
            if(length(LP_cols)>0){
              d[, (LP_cols) := NULL]
            }
            # append on new LP cols
            if(!is.null(LP_contributions)){
              LP_cols <- names(LP_contributions)
              d[non_zero_weight_rows, (LP_cols) := LP_contributions]
            }
            # return GlimmaR_model
            l <- list(idx = idx,
                      time = time,
                      model = glm_model,
                      training_data = data_to_use,
                      formula = original_glm_formula,
                      coefficients = c,
                      num_terms = nrow(c),
                      objective = glm_objective,
                      response = response,
                      weight = weight,
                      rows_used = include,
                      non_zero_weight_rows = non_zero_weight_rows,
                      predict_col_name = predict_col_name,
                      deviance = glm_model$deviance,
                      AIC = glm_model$aic,
                      dispersion = dispersion_estimate(glm_model),
                      count_NAs = count_NAs,
                      LP_contributions = LP_contributions
            )
          }
        }
      })
    }
  }
  return(l)
}
GlimmaR_coefficient_DT <- function(coefficients_dt){
  if(!is.null(coefficients_dt)){
    num_rows <- nrow(coefficients_dt)
    coefficients_dt %>%
      DT::datatable(rownames= TRUE,
                    extensions = 'Buttons',
                    selection = 'single',#HERE
                    options = list(pageLength = num_rows,
                                   dom = 'Bfrt',
                                   buttons =
                                     list('copy', list(
                                       extend = 'collection',
                                       buttons = list(list(extend='csv',filename = ''),
                                                      list(extend='excel',filename = ''),
                                                      list(extend='pdf',filename= '')),
                                       text = 'Download')
                                     ),
                                   scrollX = T,
                                   scrollY = 'calc(100vh - 338px)',
                                   searchHighlight=TRUE,
                                   columnDefs = list(list(width = '500px', targets =c(1))
                                   )

                    )) %>%
      DT::formatStyle(columns = 0:4, fontSize = '85%', lineHeight='70%') %>%
      DT::formatPercentage(c("p.value"), 1) %>%
      DT::formatSignif(c("estimate","std.error"), 4) %>%
      DT::formatStyle('p.value',
                      target = 'row',
                      backgroundColor = DT::styleInterval(c(0.01,0.05,0.1),
                                                          c(grDevices::rgb(210/255,255/255,210/255)
                                                            ,grDevices::rgb(240/255,255/255,220/255)
                                                            ,grDevices::rgb(255/255,255/255,220/255)
                                                            ,grDevices::rgb(255/255,220/255,220/255)
                                                          )
                      )
      )

  }
}
get_base_risk <- function(d, feature_spec, weight){
  base_risk <- d[1,]
  if(weight!='N'){
    base_risk[[weight]] <- 1
  }
  for (col in names(base_risk)){
    base_level <- feature_spec$base_level[feature_spec$feature==col]
    if(length(base_level)>0){
      if(is.na(base_level)){
        # do nothing
      } else {
        if(base_level!=''){
          if(class(base_risk[[col]])=='factor'){
            base_risk[[col]] <- base_level
          } else {
            base_risk[[col]] <- as.numeric(base_level)
          }
        }
      }
    }
  }
  return(base_risk)
}
export_model <- function(dat, model, base_risk, collapse, type, feature_spec = NULL, format, weight, response, fitted = NULL, model_name = NULL){
  # function converts a glm model into rectangular tables
  # get a list of the factor in the base_risk and whether they are present in the model
  grouping_var <- NULL
  grouping_var_1 <- NULL
  grouping_var_2 <- NULL
  factors_in_base_risk <- as.data.frame(all.vars(model$formula))
  factors_in_base_risk[,1] <- as.character(factors_in_base_risk[,1])
  factors_in_base_risk$id <- 1:nrow(factors_in_base_risk)
  names(factors_in_base_risk)[1] <- 'var1'
  # get model terms and append on any offset terms (which can be found in variables)
  model_terms <- attr(stats::terms(model), 'term.labels')
  # get any terms with an offset, removing the weight term if present
  model_terms_offset <- as.character(attr(stats::terms(model), 'variables'))
  model_terms_offset <- model_terms_offset[grep('offset\\(', model_terms_offset)]
  model_terms <- c(model_terms, model_terms_offset)
  model_terms_formulae <- paste('y ~', model_terms)
  variable_list <- data.frame('var1'=character(0), 'var2'=character (0))
  if(length(model_terms)>0){
    for (i in 1:length(model_terms_formulae)){
      variables <- all.vars(stats::as.formula(model_terms_formulae[i]))
      if (length(variables)==2){
        # simple factor
        variable_list <- rbind(variable_list, data.frame('var1'=variables[2],'var2'=''))
      } else if (length(variables)==3) {
        # two way interaction
        variable_list <- rbind(variable_list, data.frame('var1'=variables[2],'var2'=variables[3]))
      }
    }
  }
  # make the list unique and remove the weight if present
  variable_list <- unique(variable_list)
  variable_list <- variable_list[variable_list$var1!=weight,]
  # create dataframes containing the factors and interactions present in the model
  factors <- variable_list[variable_list$var2=='',]
  interactions <- variable_list[variable_list$var2!='',]
  factors[,1] <- as.character(factors[,1])
  interactions[,1] <- as.character(interactions[,1])
  interactions[,2] <- as.character(interactions[,2])
  # shuffle order in factors to make closer to original formula
  factors  <- merge(factors, factors_in_base_risk, by = 'var1', all.x = TRUE)
  factors <- factors[order(factors$id),]
  factors$id <- NULL
  # create a list to contain the model tables, extra 1 for the base level
  number_of_tables <- 1 + nrow(factors) + nrow(interactions)
  table_list <- vector('list', number_of_tables)
  # get the base level for the model and put it into the first element of the list
  if(model$family$link == 'logit'){type = 'link'} # can't export response for binomial
  base_level <- stats::predict(model, newdata = base_risk, type = type)
  # if long format, need extra information for the base level row
  if(format %in% c('long')){
    if(weight=='N'){
      wts <- rep(1,nrow(model$data))
    } else {
      wts <- model$data[[weight]]
    }
    # dat_subset <- data.frame(weight = wts, observed = model$y, fitted = model$fitted.values)
    # summary_tot <- dat_subset %>%
    #   dplyr::summarise_at(c('observed','fitted','weight'), sum, na.rm = TRUE) %>%
    #   dplyr::mutate_at(c('observed','fitted'), ~./ weight)

    # DATA.TABLE replacement
    cols <- c('observed','fitted')
    summary_tot <- data.table(weight = wts, observed = model$y, fitted = model$fitted.values)
    summary_tot <- summary_tot[, lapply(.SD, sum), .SDcols = c(cols,'weight')]
    summary_tot[, observed :=  observed/weight]
    summary_tot[, fitted :=  fitted/weight]
    if(is.null(model_name)) model_name <- paste(sep='_', response, weight)
    # include an operation signifier in the factor1 column for the base level table
    # strip out the base level
    if(type == 'link' | model$family$link == 'identity'){
      operation <- 'sum'
    } else if (model$family$link == 'log'){
      operation <- 'product'
    } else {
      # any other link function won't have a sensible response so revert to link function
      # no matter what the user asked for
      operation <- 'sum'
      type <- 'link'
    }
    base_table <- cbind(model_name=model_name,
                        factor1='ratebook_operation',
                        factor1_levels=operation,
                        factor2='',
                        factor2_levels='',
                        summary_tot,
                        model_relativity = base_level)
  } else {
    base_table <- data.frame(model_relativity = base_level)
  }
  table_list[[1]] <- list(table = base_table, name = 'base level')
  # create one way tables
  num_1D_tables <- number_of_tables - nrow(interactions) - 1
  # cumulative base adjustment for long format tables
  # adjust relativities so they average to 1.000
  if(type == 'link' | model$family$link == 'identity'){
    long_format_adj <- 0.0
  } else if (model$family$link == 'log'){
    long_format_adj <- 1.0
  } else {
    long_format_adj <- 0.0
  }
  for (i in 1:num_1D_tables){
    if(num_1D_tables==0) next
    factor_name <- factors[i,1]
    expanded_factor <- return_banded_levels(factor_name, feature_spec, FALSE, dat)
    # could change next line to only involve features that are present in the model, rather than the whole row
    dummy_risks <- base_risk[rep(1,each=nrow(expanded_factor$expanded_factor)),]
    dummy_risks[,factor_name] <- expanded_factor$expanded_factor_for_prediction
    predictions <- stats::predict(model, newdata = dummy_risks, type = type)
    # strip out the base level
    if(type == 'link' | model$family$link == 'identity'){
      predictions <- predictions - base_level
    } else if (model$family$link == 'log'){
      predictions <- predictions / base_level
    } else {
      # any other link function won't have a sensible response so revert to link function
      predictions <- predictions - base_level
    }
    # rounding
    predictions <- signif(predictions, 6)
    predictions[abs(predictions)<0.0000001] <- 0
    export_table <- setDT(cbind(expanded_factor$expanded_factor, predictions))
    colnames(export_table)[1] <- factor_name
    colnames(export_table)[2] <- 'model_relativity'
    if(format %in% c('long')){
      # append on KPI columns for
      # number of observations, weight, fitted, observed, model relativity
      # first band the feature if it is numeric
      print(factor_name)
      if(is.numeric(dat[[factor_name]])){
        banding <- feature_spec$banding[feature_spec$feature==factor_name]
        banding_min <- feature_spec$min[feature_spec$feature==factor_name]
        banding_max <- feature_spec$max[feature_spec$feature==factor_name]
        if(!is.na(banding)){
          if(length(banding)>0){
            if(banding=='') banding <- numeric(0)
          }
        } else {
          banding <- numeric(0)
        }
        if(length(banding)==0){
          # guess the bandings
          banding <- banding_guesser(dat[[factor_name]])
          banding_min <- min(dat[[factor_name]], na.rm = TRUE)
          banding_max <- max(dat[[factor_name]], na.rm = TRUE)
          # round down or up as needed
          banding_min <- floor(banding_min/banding)*banding
          banding_max <- ceiling(banding_max/banding)*banding
        }
        # apply same rounding as used in table creation to ensure merge can proceed
        grouped <- floor(model$data[[factor_name]]/banding)*banding
        grouped <- round(pmax(banding_min, pmin(banding_max, grouped)),6)
      } else {
        # categorical feature - just use raw data column
        grouped <- model$data[[factor_name]]
      }
      # extract actual and fitted from glm_model
      #dat_subset <- data.frame(grouping_var = dat[['grouping_var']], weight = dat[[weight]], observed = glm_model()$y, fitted = glm_model()$fitted.values)
      if(weight=='N'){
        wts <- rep(1,nrow(model$data))
      } else {
        wts <- model$data[[weight]]
      }
      # dat_subset <- data.frame(grouping_var = grouped, weight = wts, observed = model$y, fitted = model$fitted.values)
      # f <- c('observed', 'fitted')
      # f2 <- c('observed', 'fitted', 'weight')
      # summary <- dat_subset %>%
      #   dplyr::group_by(grouping_var) %>%
      #   dplyr::summarise_at(f2, sum, na.rm = TRUE) %>%
      #   dplyr::mutate_at(f, ~./ weight)


      # data.table replacement
      dat_subset <- data.table(grouping_var = grouped, weight = wts, observed = model$y, fitted = model$fitted.values)
      summary <- dat_subset[, lapply(.SD, sum), by = grouping_var, .SDcols = c('observed', 'fitted', 'weight')]
      summary[, observed := observed/weight]
      summary[, fitted := fitted/weight]

      # merge summary onto the rating table
      export_table <- merge(export_table, summary, by.x = factor_name, by.y = 'grouping_var', all.x = TRUE)
      if(format=='long norm'){
        # adjust relativities so they average to 1.000
        if(type == 'link' | model$family$link == 'identity'){
          average_relativity <- sum(export_table$model_relativity*export_table$weight, na.rm =TRUE) / sum(export_table$weight, na.rm = TRUE)
          export_table$model_relativity <- export_table$model_relativity - average_relativity
          long_format_adj <- long_format_adj + average_relativity
        } else if (model$family$link == 'log'){
          average_relativity <- sum(export_table$model_relativity*export_table$weight, na.rm =TRUE) / sum(export_table$weight, na.rm = TRUE)
          export_table$model_relativity <- export_table$model_relativity / average_relativity
          long_format_adj <- long_format_adj * average_relativity
        } else {
          # any other link function won't have a sensible response so revert to link function
          average_relativity <- sum(export_table$model_relativity*export_table$weight, na.rm =TRUE) / sum(export_table$weight, na.rm = TRUE)
          export_table$model_relativity <- export_table$model_relativity - average_relativity
          long_format_adj <- long_format_adj + average_relativity
        }
      }
      # swap around the position of the value column (2nd column) and rename
      export_table <- export_table[, c(1,3,4,5,2)]
      names(export_table)[5] <- 'model_relativity'
      # create a summary row at the bottom
      # this is a bit long-winded, but a useful check that the relativities average to 1.000
      export_table$weighted_relativity <- export_table$model_relativity * export_table$weight
      export_table$weighted_observed <- export_table$observed * export_table$weight
      export_table$weighted_fitted <- export_table$fitted * export_table$weight
      # summary_tot <- export_table %>%
      #   dplyr::summarise_at(c('weighted_observed','weighted_fitted','weight','weighted_relativity'), sum, na.rm = TRUE) %>%
      #   dplyr::mutate_at(c('weighted_observed','weighted_fitted','weighted_relativity'), ~./ weight)

      # data.table replacement
      cols <- c('weighted_observed','weighted_fitted','weight','weighted_relativity')
      summary_tot <- export_table[, lapply(.SD, sum, na.rm = TRUE), .SDcols = cols]
      cols <- c('weighted_observed','weighted_fitted','weighted_relativity')
      summary_tot[, (cols) := lapply(.SD, '/', weight), .SDcols = cols]
      summary_tot <- cbind('', summary_tot)
      export_table$weighted_relativity <- NULL
      export_table$weighted_observed <- NULL
      export_table$weighted_fitted <- NULL
      # bind the summary row to the overall table
      names(summary_tot) <- names(export_table)
      export_table <- rbind(summary_tot, export_table)
      # rename columns and create a blank column for factor 2
      names(export_table)[1] <- 'factor1_levels'
      export_table$factor2_levels <- ''
      # bind the factor name
      export_table <- cbind('factor1'=rep(factor_name, nrow(export_table)),
                            'factor2'=rep('',nrow(export_table)),
                            export_table)
      export_table <- export_table[, c(1,3,2,8,4,5,6,7)]
      # bind a model name row if one has been supplied
      if(!is.null(model_name)){
        export_table <- cbind('model_name'=rep(model_name, nrow(export_table)), export_table)
      }
    }
    table_list[[i+1]] <- list(table = export_table, name = factor_name)
  }
  # create two way tables
  for (i in 1:nrow(interactions)){
    if (nrow(interactions)==0) next
    factor1_name <- interactions[i,1]
    factor2_name <- interactions[i,2]
    expanded_factor_1 <- return_banded_levels(factor1_name, feature_spec, FALSE, dat)
    expanded_factor_2 <- return_banded_levels(factor2_name, feature_spec, FALSE, dat)
    expanded_factor_1$expanded_factor <- as.vector(expanded_factor_1$expanded_factor[[1]]) # expand.grid didn't like data frames, so make vector
    expanded_factor_2$expanded_factor <- as.vector(expanded_factor_2$expanded_factor[[1]])
    expanded_factor_1$expanded_factor_for_prediction <- as.vector(expanded_factor_1$expanded_factor_for_prediction[[1]])
    expanded_factor_2$expanded_factor_for_prediction <- as.vector(expanded_factor_2$expanded_factor_for_prediction[[1]])
    expanded_factor <- expand.grid(expanded_factor_1$expanded_factor, expanded_factor_2$expanded_factor)
    expanded_factor_for_prediction <- expand.grid(expanded_factor_1$expanded_factor_for_prediction, expanded_factor_2$expanded_factor_for_prediction)
    dummy_risks <- base_risk[rep(1,each=nrow(expanded_factor_for_prediction)),]
    dummy_risks1 <- dummy_risks
    dummy_risks2 <- dummy_risks
    dummy_risks[,factor1_name] <- expanded_factor[,1]
    dummy_risks[,factor2_name] <- expanded_factor[,2]
    dummy_risks1[,factor1_name] <- expanded_factor[,1]
    dummy_risks2[,factor2_name] <- expanded_factor[,2]
    predictions <- stats::predict(model, newdata = dummy_risks, type = type)
    predictions1 <- stats::predict(model, newdata = dummy_risks1, type = type)
    predictions2 <- stats::predict(model, newdata = dummy_risks2, type = type)
    if (collapse){
      if(type == 'link' | model$family$link == 'identity'){
        predictions <- predictions - base_level
      } else if (model$family$link == 'log'){
        predictions <- predictions / base_level
      } else {
        # any other link function won't have a sensible response so revert to link function
        predictions <- predictions - base_level
      }
    } else {
      if(type == 'link'| model$family$link == 'identity'){
        predictions <- predictions + base_level - predictions1 - predictions2
      } else if (model$family$link == 'log'){
        predictions <- (base_level * predictions) / (predictions1 * predictions2)
      } else {
        predictions <- predictions + base_level - predictions1 - predictions2
      }
    }
    # rounding and set numbers v close to zero to zero
    predictions <- signif(predictions, 6)
    predictions[abs(predictions)<0.0000001] <- 0
    export_table <- as.data.table(cbind(expanded_factor, predictions))
    colnames(export_table)[1] <- factor1_name
    colnames(export_table)[2] <- factor2_name
    colnames(export_table)[3] <- 'model_relativity'
    if(format=='solo'){
      # 2 dimensional table
      export_table <- dcast(export_table, export_table[[factor1_name]] ~ export_table[[factor2_name]], value.var = c('model_relativity'), fun.aggreagte=sum)
      colnames(export_table)[1] <- paste(factor1_name, '__X__', factor2_name, sep = '')
    } else if (format %in% c('long')){
      # long table with two columns for features
      # export_table is in correct format, we need to append KPI columns for
      # number of observations, weight, fitted, observed, model relativity
      # band feature 1 if numeric
      if(is.numeric(dat[[factor1_name]])){
        banding_min_1 <- feature_spec$min[feature_spec$feature==factor1_name]
        banding_max_1 <- feature_spec$max[feature_spec$feature==factor1_name]
        banding_1 <- feature_spec$banding[feature_spec$feature==factor1_name]
        if(length(banding_1)==0 | banding_1=='') banding_1 <- NA
        if(is.na(banding_1)){
          # guess the bandings
          banding_1 <- banding_guesser(dat[[factor1_name]])
          banding_min_1 <- min(dat[[factor1_name]], na.rm = TRUE)
          banding_max_1 <- max(dat[[factor1_name]], na.rm = TRUE)
        }
        dat$grouping_var_1 <- floor(model$data[[factor1_name]]/banding_1)*banding_1
        dat$grouping_var_1 <- pmax(banding_min_1, pmin(banding_max_1, dat$grouping_var_1))
      } else {
        # categorical feature - just use raw data column
        dat$grouping_var_1 <- model$data[[factor1_name]]
      }
      # band feature 2 if numeric
      if(is.numeric(dat[[factor2_name]])){
        banding_min_2 <- feature_spec$min[feature_spec$feature==factor2_name]
        banding_max_2 <- feature_spec$max[feature_spec$feature==factor2_name]
        banding_2 <- feature_spec$banding[feature_spec$feature==factor2_name]
        if(length(banding_2)==0 | banding_2=='') banding_2 <- NA
        if(is.na(banding_2)){
          # guess the bandings
          banding_2 <- banding_guesser(dat[[factor2_name]])
          banding_min_2 <- min(dat[[factor2_name]], na.rm = TRUE)
          banding_max_2 <- max(dat[[factor2_name]], na.rm = TRUE)
        }
        dat$grouping_var_2 <- floor(model$data[[factor2_name]]/banding_2)*banding_2
        dat$grouping_var_2 <- pmax(banding_min_2, pmin(banding_max_2, dat$grouping_var_2))
      } else {
        # categorical feature - just use raw data column
        dat$grouping_var_2 <- model$data[[factor2_name]]
      }
      # extract actual and fitted from glm_model
      if(weight=='N'){
        wts <- rep(1, nrow(dat))
      } else {
        wts <- dat[[weight]]
      }
      # dat_subset <- data.frame(grouping_var_1 = dat[['grouping_var_1']], grouping_var_2 = dat[['grouping_var_2']], weight = wts, observed = model$y, fitted = model$fitted.values)
      # f <- c('observed', 'fitted')
      # f2 <- c('observed', 'fitted', 'weight')
      # summary <- dat_subset %>%
      #   dplyr::group_by(grouping_var_1,grouping_var_2) %>%
      #   dplyr::summarise_at(f2, sum, na.rm = TRUE) %>%
      #   dplyr::mutate_at(f, ~./ weight)

      # data.table version
      observed <- NULL
      dat_subset <- data.table(grouping_var_1 = dat[['grouping_var_1']], grouping_var_2 = dat[['grouping_var_2']], weight = wts, observed = model$y, fitted = model$fitted.values)
      summary <- dat_subset[, lapply(.SD, sum), by = c('grouping_var_1','grouping_var_2'), .SDcols = c('observed', 'fitted', 'weight')]
      cols <- c('observed', 'fitted')
      summary <- summary[, (cols) := lapply(.SD, '/', weight), .SDcols = cols]

      # apply some rounding
      # this is probably not ideal
      if(is.numeric(summary$grouping_var_1)){
        summary$grouping_var_1 <- signif(summary$grouping_var_1,6)
      }
      if(is.numeric(summary$grouping_var_2)){
        summary$grouping_var_2 <- signif(summary$grouping_var_2,6)
      }
      # merge summary onto the rating table
      export_table <- merge(export_table, summary, by.x = c(factor1_name, factor2_name), by.y = c('grouping_var_1', 'grouping_var_2'), all.x = TRUE)
      # adjust relativities so they average to 1.000
      if(format=='long norm'){
        if(type == 'link' | model$family$link == 'identity'){
          average_relativity <- sum(export_table$model_relativity*export_table$weight, na.rm =TRUE) / sum(export_table$weight, na.rm = TRUE)
          export_table$model_relativity <- export_table$model_relativity - average_relativity
          long_format_adj <- long_format_adj + average_relativity
        } else if (model$family$link == 'log'){
          average_relativity <- sum(export_table$model_relativity*export_table$weight, na.rm =TRUE) / sum(export_table$weight, na.rm = TRUE)
          export_table$model_relativity <- export_table$model_relativity / average_relativity
          long_format_adj <- long_format_adj * average_relativity
        } else {
          # any other link function won't have a sensible response so revert to link function
          average_relativity <- sum(export_table$model_relativity*export_table$weight, na.rm =TRUE) / sum(export_table$weight, na.rm = TRUE)
          export_table$model_relativity <- export_table$model_relativity - average_relativity
          long_format_adj <- long_format_adj + average_relativity
        }
      }
      # swap around the position of the value column (2nd column) and rename
      export_table <- export_table[, c(1,2,4,5,6,3)]
      names(export_table)[1] <- 'factor1_levels'
      names(export_table)[2] <- 'factor2_levels'
      names(export_table)[6] <- 'model_relativity'
      # create a summary row at the bottom
      # this is a bit long-winded, but a useful check that the relativities average to 1.000
      export_table$weighted_relativity <- export_table$model_relativity * export_table$weight
      export_table$weighted_observed <- export_table$observed * export_table$weight
      export_table$weighted_fitted <- export_table$fitted * export_table$weight
      # summary_tot <- export_table %>%
      #   dplyr::summarise_at(c('weighted_observed','weighted_fitted','weight','weighted_relativity'), sum, na.rm = TRUE) %>%
      #   dplyr::mutate_at(c('weighted_observed','weighted_fitted','weighted_relativity'), ~./ weight)

      # data.table version
      cols <- c('weighted_observed','weighted_fitted','weighted_relativity')
      summary_tot <- export_table[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c(cols, 'weight')]
      summary_tot[, (cols) := lapply(.SD, '/', weight), .SDcols = cols]
      setcolorder(summary_tot, c('weighted_observed','weighted_fitted','weight','weighted_relativity'))
      summary_tot <- cbind('','', summary_tot)
      export_table$weighted_relativity <- NULL
      export_table$weighted_observed <- NULL
      export_table$weighted_fitted <- NULL
      # bind the summary row to the overall table
      names(summary_tot) <- names(export_table)
      export_table <- rbind(summary_tot, export_table)
      # bind the factor name
      export_table <- cbind('factor1'=rep(factor1_name, nrow(export_table)),
                            'factor2'=rep(factor2_name, nrow(export_table)),
                            export_table)
      # reorder columns yet again
      export_table <- export_table[, c(1,3,2,4,5,6,7,8)]
      # bind a model name row if one has been supplied
      if(!is.null(model_name)){
        export_table <- cbind('model_name'=rep(model_name, nrow(export_table)), export_table)
      }
    }
    table_list[[1 + nrow(factors) + i]] <- list(table = export_table, name = paste(sep = '__X__', factor1_name, factor2_name))
  }
  # adjust the long format base level if neccesary
  # adjust relativities so they average to 1.000
  if(format %in% c('long')){
    if(type == 'link' | model$family$link == 'identity'){
      table_list[[1]]$table$model_relativity <- table_list[[1]]$table$model_relativity + long_format_adj
    } else if (model$family$link == 'log'){
      table_list[[1]]$table$model_relativity <- table_list[[1]]$table$model_relativity * long_format_adj
    } else {
      # any other link function won't have a sensible response so revert to link function
      table_list[[1]]$table$model_relativity <-table_list[[1]]$table$model_relativity + long_format_adj
    }
  }
  # return list from function, include slot for the format of the tables
  table_names <- sapply(table_list, '[[', 'name')
  names(table_list) <- table_names
  table_list$format <- format
  return(table_list)
}
return_banded_levels <- function(factor_name, feature_spec, use_mid_point_column, dat){

  # if the feature spec contains banding/min/max then use those
  # otherwise make a guess as to a good banding level
  banding_min <- NA
  banding_max <- NA
  banding <- NA
  use_mid_point <- FALSE
  if(!is.null(feature_spec) & is.numeric(dat[[factor_name]]) & ('banding' %in% names(feature_spec))){
    banding_min <- feature_spec$min[feature_spec$feature==factor_name]
    banding_max <- feature_spec$max[feature_spec$feature==factor_name]
    banding <- feature_spec$banding[feature_spec$feature==factor_name]
    use_mid_point <- feature_spec$use_mid_point[feature_spec$feature==factor_name]
    if(length(banding_min)==0) banding_min <- NA
    if(length(banding_max)==0) banding_max <- NA
    if(length(banding)==0) banding <- NA
    if(length(use_mid_point)==0){
      use_mid_point <- FALSE
    } else if(is.na(use_mid_point)){
      use_mid_point <- FALSE
    } else if(class(use_mid_point)=='character'){
      if(use_mid_point=='TRUE') use_mid_point <- TRUE
    }

    # override use_mid_point if
    if(use_mid_point_column=='No'){
      use_mid_point <- FALSE
    }

  }

  # set the banding level if not using feature spec or banding level not found
  if(is.null(feature_spec) | is.na(banding) | banding==''){
    # guess the banding level if numeric
    if(is.numeric(dat[[factor_name]])){
      banding <- banding_guesser(dat[[factor_name]])
    }
  }

  # set the min if not using feature spec or banding level not found
  if(is.null(feature_spec) | is.na(banding_min) | banding_min==''){
    if(is.numeric(dat[[factor_name]])){
      banding_min <- min(dat[[factor_name]], na.rm = TRUE)
    }
  }

  # set the max if not using feature spec or banding level not found
  if(is.null(feature_spec) | is.na(banding_max) | banding_max==''){
    if(is.numeric(dat[[factor_name]])){
      banding_max <- max(dat[[factor_name]], na.rm = TRUE)
    }
  }

  # create levels we want to predict on
  if(is.numeric(dat[[factor_name]])){
    # in case feature spec saved as characters
    banding <- as.numeric(banding)
    banding_min <- as.numeric(banding_min)
    banding_max <- as.numeric(banding_max)
    # simple sequence - code below makes sure end points are correct
    banding_min <- floor(banding_min/banding)*banding
    banding_max_temp <- floor(banding_max/banding)*banding
    if(banding_max_temp==banding_max){
      banding_max <- banding_max_temp
    } else {
      banding_max <- banding_max_temp + banding
    }

    # expand out the feature
    # apply some rounding to make merging on data easier later
    expanded_factor <- round(seq(banding_min, banding_max, banding),6)
    expanded_factor <- as.data.frame(expanded_factor)

    # if use_mid_point is TRUE then predict on mid-point not start of band
    if(use_mid_point){
      expanded_factor_for_prediction <- expanded_factor + banding/2
    } else {
      expanded_factor_for_prediction <- expanded_factor
    }

  } else {
    # every level in the dataset for non-numeric features
    expanded_factor <- as.data.frame(levels(dat[[factor_name]]))
    expanded_factor_for_prediction <- expanded_factor
  }

  list(expanded_factor = expanded_factor, expanded_factor_for_prediction = expanded_factor_for_prediction)

}
GlimmaR_format_table_DT <- function(GlimmaR_models, model_index, table_name, transpose){
  if(length(model_index)>0 & !is.null(table_name)){
    dt <- GlimmaR_models[[model_index]]$tabulated_model[[table_name]]$table
    if(transpose=='Transpose' & GlimmaR_models[[model_index]]$tabulated_model$format=='solo'){
      dt <- data.table::transpose(dt, keep.names = names(dt)[1], make.names = names(dt)[1])
    }
    if(!is.null(dt)){
      # if(input$glm_exported_table_orientation=='Top'){
      #   dt <- data.table::transpose(dt, keep.names = names(dt)[1], make.names = 1)
      # }
      t <- dt %>% DT::datatable(rownames= TRUE,
                                options = list(pageLength = nrow(dt),
                                               dom = 'Bfrti',
                                               scrollX = T,
                                               scrollY = 'calc(100vh - 338px)',
                                               columnDefs = list(list(visible = F, targets = 0)
                                               )
                                )
      ) %>%
        DT::formatStyle(1:ncol(dt), lineHeight='50%', fontSize = '85%')
      if(GlimmaR_models[[model_index]]$tabulated_model$format %in% c('long','long norm')){
        # make first row bold as total row
        t <- t %>% DT::formatStyle(0,
                                   target = "row",
                                   fontWeight = DT::styleEqual(1, "bold"))
        t <- t %>% DT::formatRound(c("observed", "fitted"), 4, mark = ',')
        t <- t %>% DT::formatRound(c('weight'), digits=0, mark = ',')
        t <- t %>% DT::formatRound(c('model_relativity'), digits=4, mark = ',')
      }
    } else {
      t <- data.table(V1 = 'no model tabulated') %>% DT::datatable()
    }
    return(t)
  }
}
is_tabulated_model <- function(g){'tabulated_model' %in% names(g)}
GlimmaR_tabulated_model_list <- function(GlimmaR_models){
  if(!is.null(GlimmaR_models)){
    model_list <- as.list(1:length(GlimmaR_models))
    tabulated <- sapply(GlimmaR_models, is_tabulated_model)
    names(model_list) <- sapply(GlimmaR_models, '[[', 'predict_col_name')
    model_list <- model_list[tabulated]
  }
  return(model_list)
}
GlimmaR_model_table_list <- function(GlimmaR_model){
  if(is.null(GlimmaR_model$tabulated_model)){
    table_list <- NULL
  } else {
    table_list <- names(GlimmaR_model$tabulated_model)[1:(length(GlimmaR_model$tabulated_model)-1)]
    table_list <- as.list(table_list)
    names(table_list) <- paste0(1:length(table_list),' - ',table_list)
  }
  return(table_list)
}
objective_guesser <- function(d, response, weight, type){
  # if any response are negative then can only use squared loss
  if(is.null(d[[response]])){
    guess <- 'gaussian'
  } else {
    if(min(d[[response]], na.rm = TRUE)<0){
      guess <- 'gaussian'
    } else if (max(d[[response]], na.rm = TRUE)==1){
      guess <- 'binomial'
    } else if ((sum(d[[response]], na.rm = TRUE)-sum(floor(d[[response]]), na.rm = TRUE))==0 & max(d[[response]], na.rm = TRUE)<100){
      guess <- 'poisson'
    } else {
      guess <- 'gamma'
    }
  }
  if(type=='gbm'){
    if(guess=='gaussian') guess <- 'mean_squared_error'
    if(guess=='binomial') guess <- 'binary'
  }
  return(guess)
}
write_tables_to_excel <- function(tables, filename){
  index <- NULL
  dimensions <- NULL
  min_value <- NULL
  max_value <- NULL
  base <- NULL
  value <- NULL
  # takes a set of exported tables and the model coefficients and writes them to Excel

  # define a white text on blue background style to use for all header rows
  headerStyle <- createStyle(bgFill = "#020202", fontColour = "#FFFFFF", halign = 'center')

  # write the coefficients table to Excel
  wb <- createWorkbook()

  # format the index table
  # index_table <- vector('list', length(tables))
  n_tables <- length(tables) - 1
  index_table <- data.table(index = integer(),
                            table = character(),
                            dimensions = integer(),
                            min_value = numeric(),
                            max_value = numeric(),
                            span =numeric())[1:n_tables]

  # fill in the base table
  index_table[1,index := 1]
  index_table[1,table := 'base']
  index_table[1,dimensions := 0]

  for (i in 2:n_tables){
    #index_table[[i]] <- colnames(tables[[i]])[1]
    print(i)
    index_table[i,index := i]
    index_table[i,table := names(tables[[i]]$table)[1]]
    index_table[i,dimensions := ifelse(dim(tables[[i]]$table)[2]>2,2,1)]
    index_table[i,min_value := min(tables[[i]]$table[,-1])]
    index_table[i,max_value := max(tables[[i]]$table[,-1])]
    index_table[i,span := max(tables[[i]]$table[,-1])/min(tables[[i]]$table[,-1])]
  }

  # write the index worksheet to Excel
  addWorksheet(wb, "index")
  writeData(wb, "index", index_table)
  setColWidths(wb, "index", cols = 1, widths = 10)
  setColWidths(wb, "index", cols = 2, widths = 30)
  setColWidths(wb, "index", cols = 3:6, widths = 10)
  addStyle(wb = wb, sheet = "index", cols = 1:2, rows = 1:200, style = createStyle(halign = 'left'), gridExpand = TRUE)
  addStyle(wb = wb, sheet = "index", cols = 3:6, rows = 1:200, style = createStyle(halign = 'center'), gridExpand = TRUE)
  #addStyle(wb, sheet = "index", style=createStyle(bgFill = "#020202", fontColour = "#FFFFFF", halign = 'left'), cols=1:6, rows=1)

  # write the tables to Excel
  for (i in 1:n_tables){

    if (index_table[i,dimensions]==0){ # 0 dimensions means the base level - a single number

      # add worksheet and format
      openxlsx::addWorksheet(wb,as.character(i))
      openxlsx::setColWidths(wb, as.character(i), cols = 1:3, widths = 30)
      openxlsx::addStyle(wb = wb, sheet = as.character(i), cols = 1L, rows = 1:100, style = openxlsx::createStyle(halign = 'left'))
      openxlsx::addStyle(wb, sheet = as.character(i), cols = 2, rows = 1:100, style = openxlsx::createStyle(halign = 'center'))
      #addStyle(wb, sheet = as.character(i), style=headerStyle, cols=1:2, rows=1)
      #addStyle(wb, sheet = as.character(i), style = createStyle(halign = 'left',bgFill = "#020202", fontColour = "#FFFFFF"), cols=1, rows=1)

      table_to_write <- data.table(base = character(), value = double())[1:2]
      table_to_write[1, base := 'technical']
      table_to_write[2, base := 'implementable']
      table_to_write[1, value := tables[[i]]$table[[1]]]
      table_to_write[2, value := tables[[i]]$table[[1]]]

      openxlsx::writeData(wb, as.character(i), table_to_write)

    } else if (index_table[i,dimensions]==1) {

      # add worksheet and format
      openxlsx::addWorksheet(wb,as.character(i))
      openxlsx::setColWidths(wb, as.character(i), cols = 1:3, widths = 30)
      openxlsx::addStyle(wb = wb, sheet = as.character(i), cols = 1L, rows = 1:100, style = openxlsx::createStyle(halign = 'left'))
      openxlsx::addStyle(wb, sheet = as.character(i), cols = 2:3, rows = 1:100, style = openxlsx::createStyle(halign = 'center'), gridExpand = TRUE)
      #addStyle(wb, sheet = as.character(i), style=headerStyle, cols=1:3, rows=1)
      #addStyle(wb, sheet = as.character(i), style = createStyle(halign = 'left',bgFill = "#020202", fontColour = "#FFFFFF"), cols=1, rows=1)

      # create table in correct format
      table_to_write <- tables[[i]]$table
      names(table_to_write)[2] <- 'technical'
      table_to_write$implementable <- table_to_write$technical

      # write table
      openxlsx::writeData(wb, as.character(i), table_to_write)

    } else if (index_table[i,dimensions]==2) {

      # get the length of the second dimension
      second_dim_length <- dim(tables[[i]]$table)[2] - 1

      # going to write the table twice, once called tech and once called imp
      table_to_write <- tables[[i]]$table

      # write technical table
      w_name <- paste(as.character(i), '_tech', sep = '')
      openxlsx::addWorksheet(wb,w_name)
      openxlsx::setColWidths(wb, w_name, cols = 1:(second_dim_length+1), widths = 30)
      openxlsx::addStyle(wb, sheet = w_name, cols = 2:(second_dim_length+1), rows = 1:100, style = openxlsx::createStyle(halign = 'center'), gridExpand = TRUE)
      openxlsx::addStyle(wb = wb, sheet = w_name, cols = 1L, rows = 1:100, style = openxlsx::createStyle(halign = 'left'))
      #addStyle(wb, sheet = w_name, style=headerStyle, cols=1:(second_dim_length+1), rows=1)
      #addStyle(wb, sheet = w_name, style = createStyle(halign = 'left',bgFill = "#020202", fontColour = "#FFFFFF"), cols=1, rows=1)

      openxlsx::writeData(wb, w_name, table_to_write)

      # write implementable table
      w_name <- paste(as.character(i), '_imp', sep = '')
      openxlsx::addWorksheet(wb,w_name)
      openxlsx::setColWidths(wb, w_name, cols = 1:30, widths = 30)
      openxlsx::addStyle(wb, sheet = w_name, cols = 2:(second_dim_length+1), rows = 1:100, style = openxlsx::createStyle(halign = 'center'), gridExpand = TRUE)
      openxlsx::addStyle(wb = wb, sheet = w_name, cols = 1L, rows = 1:100, style = openxlsx::createStyle(halign = 'left'))
      #addStyle(wb, sheet = w_name, style=headerStyle, cols=1:(second_dim_length+1), rows=1)
      #addStyle(wb, sheet = w_name, style = createStyle(halign = 'left',bgFill = "#020202", fontColour = "#FFFFFF"), cols=1, rows=1)
      openxlsx::writeData(wb, w_name, table_to_write)

    }
  }
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
}
gather_glm_terms <- function(d, model){
  # takes input glm model
  # outputs data table of each feature's contribution to the terms outputs
  # get list of all features used in the model
  # loop through each feature
  # identify terms containing feature
  # sum terms
  # get the model features and model terms
  features <- all.vars(model$formula)
  model_terms <- attr(stats::terms(model), 'term.labels')
  if(length(features)>1 & length(model_terms) >0){
    features <- features[2:length(features)] # remove first element as is response
    model_terms_formulae <- paste('y ~', model_terms)
    model_terms_formulae <- lapply(model_terms_formulae, stats::as.formula)
    model_terms_features <- lapply(model_terms_formulae, all.vars)
    # create matrix to hold results
    glm_feature_contributions <- matrix(0, nrow = nrow(d), ncol = length(features))
    # get the linear prediction broken down into individual terms
    terms_predictions <- stats::predict(model, d, type = 'terms')
    # loop through each feature and create feature contributions to linear predictor
    for (i in 1:length(features)){
      cols <- rep(FALSE, length(features))
      # identify GLM term columns containing the feature
      feature <- features[i]
      term_contains_feature <- lapply(model_terms_features, is.element, feature)
      term_contains_feature <- lapply(term_contains_feature, sum)
      term_contains_feature <- unlist(term_contains_feature)
      cols <- ifelse(term_contains_feature>0,TRUE,FALSE)
      glm_feature_contributions[,i] <- rowSums(terms_predictions[, cols, drop = FALSE])
    }
    glm_feature_contributions <- as.data.table(glm_feature_contributions)
    names(glm_feature_contributions) <- paste(sep = '_', 'glm_LP', features)
  } else {
    glm_feature_contributions <- NULL
  }
  glm_feature_contributions
}
dispersion_estimate <- function(model){
  if(model$family$family %in% c('poisson','binomial')){
    1
  } else {
    sum((model$weights * model$residuals^2)[model$weights > 0])/model$df.residual
  }
}
strip_glm <- function(cm) {

  # strips out stuff we don't need from GLM model to make it smaller when saving
  cm$y <- c()
  cm$model <- c()

  cm$residuals <- c()
  cm$fitted.values <- c()
  cm$effects <- c()
  cm$qr$qr <- c()
  cm$linear.predictors <- c()
  cm$weights <- c()
  cm$prior.weights <- c()
  cm$data <- c()
  cm$offset <- c()

  # following means glm unable to predict using type = 'terms'
  cm$family$variance <- c()
  cm$family$dev.resids <- c()
  cm$family$aic <- c()
  cm$family$validmu <- c()
  cm$family$simulate <- c()
  attr(cm$terms,".Environment") <- globalenv() # otherwise the object is huge
  attr(cm$formula,".Environment") <- c()

  cm
}
make_GlimmaR_helper_features <- function(d, BoostaR_model, choice, search){
  features <- NULL
  if(!is.null(d)){
    if(choice=='Original'){
      features <- names(d)
    } else if (choice=='A-Z'){
      features <- sort(names(d))
    } else if (choice=='GBM'){
      if(!is.null(BoostaR_model)){
        features <- BoostaR_model$importances$Feature
      } else {
        features <- '-- no GBMs built --'
      }
    }
  }
  # filter on search
  search_choices <- NULL
  if(!is.null(search) & search!=''){
    features <- tryCatch({features[grepl(search, features)]}, error = function(e){e})
    if(class(features)=='simpleError'){
      features <- '-- no result --'
    } else if(length(features)==0){
      features <- '-- no result --'
    }
  }
  return(features)
}
make_GlimmaR_helper_levels <- function(d, feature){
  result <- '-- no feature selected --'
  if(!is.null(d) & !is.null(feature)){
    if(feature %in% names(d)){
      if(class(d[[feature]])[1]=='factor'){
        result <- levels(d[[feature]])
        if(length(result)>1000){
          result <- '-- too many levels (>1,000) --'
        }
      } else if (class(d[[feature]])[1] %in% c('integer','numeric')){
        result <- c('Identity',
                    'pmin(x, feature)',
                    'pmax(x, feature)',
                    'pmax(x, pmin(y, feature))',
                    'Polynomial (order)',
                    'log(feature)',
                    'log(1+feature)',
                    'sqrt(feature)',
                    'Piecewise linear (breaks)',
                    'if(feature=x,1,0)',
                    'if(feature<x,1,0)',
                    'if(feature>x,1,0)',
                    'between(feature,x,y)',
                    'Spline (df)',
                    'Spline (knots)'
        )
      }
    }
  }
  return(result)
}
make_GlimmaR_formula_suggestion <- function(d, feature, options, level_grouping, inputs){
  if(!is.null(d) & !is.null(feature)){
    comment_line <- paste0('# ', feature)
    formula_lines <- NULL
    if(class(d[[feature]])[1]=='factor'){
      if(level_grouping=='Single'){
        if(!is.null(options)){
          if(length(options)>0){
            formula_lines <- paste0("ifelse(", feature, "=='", options, "',1,0)")
            formula_lines <- paste(formula_lines, collapse = ' + \n')
            formula_lines <- paste(formula_lines, sep = '\n')
          }
        }
      } else if (level_grouping=='Group'){
        formula_lines <- paste(options, collapse = "','")
        formula_lines <- paste0("c('", formula_lines, "')")
        formula_lines <- paste0("ifelse(", feature, " %in% ", formula_lines, ",1,0)")
      }
    } else if(class(d[[feature]])[1] %in% c('integer','numeric')){
      if(length(options)==1){
        formula_lines <- make_numerical_feature_formula(feature, options, inputs)
      }
    }
    result <- paste(comment_line, formula_lines, sep = '\n')
  }
}
make_numerical_feature_formula <- function(feature, formula_type, inputs){
  # inputs is a sequence of numbers separated by commas
  inputs <- as.numeric(unlist(strsplit(inputs, ',')))
  n <- length(inputs)
  if(length(inputs)==0){
    inputs <- c('x','y')
  } else if (length(inputs)==1){
    inputs <- c(inputs[1], 'y')
  }
  if(formula_type=='Identity'){
    feature
  } else if (formula_type=='pmin(x, feature)'){
    paste0('pmin(', inputs[1], ', ', feature, ')')
  } else if (formula_type=='pmax(x, feature)'){
    paste0('pmax(', inputs[1], ', ', feature, ')')
  } else if (formula_type=='pmax(x, pmin(y, feature))'){
    paste0('pmax(', inputs[1], ', pmin(', inputs[2], ', ',feature, '))')
  } else if (formula_type=='Polynomial (order)'){
    paste0('poly(', feature, ', ', inputs[1], ')')
  } else if (formula_type=='log(feature)'){
    paste0('log(', feature, ')')
  } else if (formula_type=='log(1+feature)'){
    paste0('log(1+', feature, ')')
  } else if (formula_type=='sqrt(feature)'){
    paste0('sqrt(', feature, ')')
  } else if (formula_type=='Piecewise linear (breaks)'){
    if(n<=1){
      paste0('pmin(', inputs[1], ', ', feature, ') +\n',
             'pmax(', inputs[1], ', ', feature, ')')
    } else if (n==2){
      paste0('pmin(', inputs[1], ', ', feature, ') +\n',
             'pmax(', inputs[1], ', pmin(', inputs[2], ', ',feature, ')) +\n',
             'pmax(', inputs[2], ', ', feature, ')')
    } else {
      tstart <- paste0('pmin(', inputs[1], ', ', feature, ') +\n')
      tmiddle <- ''
      for(i in 2:n-1){
        tmiddle <- paste0(tmiddle, 'pmax(', inputs[i], ', pmin(', inputs[i+1], ', ',feature, ')) +\n')
      }
      tend <- paste0('pmax(', inputs[n], ', ', feature, ')')
      paste0(tstart, tmiddle, tend)
    }

  } else if (formula_type=='if(feature=x,1,0)'){
    paste0('ifelse(', feature, "==", inputs[1], ",1,0)")
  } else if (formula_type=='if(feature<x,1,0)'){
    paste0('ifelse(', feature, "<", inputs[1], ",1,0)")
  } else if (formula_type=='if(feature>x,1,0)'){
    paste0('ifelse(', feature, ">", inputs[1], ",1,0)")
  } else if (formula_type=='between(feature,x,y)'){
    paste0('between(', feature, ',', inputs[1], ',', inputs[2],')')
  } else if (formula_type=='Spline (df)'){
    paste0('ns(', feature, ', ', inputs[1], ')')
  } else if (formula_type=='Spline (knots)'){
    inputs <- paste(inputs, collapse = ',')
    paste0('ns(', feature, ', knots = c(', inputs, '))')
  }




}
GlimmaR_model_summary <- function(GlimmaR_models){
  # takes the key info from the BoostaR_models
  # and makes a summary data table
  rows <- lapply(GlimmaR_models, GlimmaR_model_summary_row)
  rbindlist(rows)
}
GlimmaR_model_summary_row <- function(GlimmaR_model){
  x <- data.table(idx = GlimmaR_model$idx,
                  time = round(GlimmaR_model$time, 1),
                  data = GlimmaR_model$dataset_name,
                  train = GlimmaR_model$training_data,
                  response = GlimmaR_model$response,
                  weight = GlimmaR_model$weight,
                  obj = GlimmaR_model$objective,
                  terms = GlimmaR_model$num_terms,
                  dev = signif(GlimmaR_model$deviance, 6),
                  AIC = signif(GlimmaR_model$AIC, 6),
                  dispersion = signif(GlimmaR_model$dispersion, 6),
                  NAs = GlimmaR_model$count_NAs
  )
  return(x)
}
