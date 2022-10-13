#' @rawNamespace import(plotly, except = slice)
one_way_chart_ui <- function(id) {
  ns <- NS(id)
  dimensionId <- ns("dimension")
  # code below provides a namespaced variable to get the window width and height
  # so we can resize the chart depending on window size
  tagList(
    tags$head(tags$script(sprintf("
      var dimensionId = '%s';
      var dimension = [0, 0];
      $(document).on('shiny:connected', function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange(dimensionId, dimension);
      });

      $(window).resize(function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange(dimensionId, dimension);
      });
    ", dimensionId))),
    plotly::plotlyOutput(ns("myPlot"))
  )
}
one_way_chart_server <- function(id, d, response, weight, show_labels, show_response, error_bars, kpi_spec, GlimmaR_models, train_test_filter, user_filter, filter_operation) {
  moduleServer(id, function(input, output, session) {
    output$myPlot <- plotly::renderPlotly({
      viz_one_way_chart(d(), response(), weight(), show_labels(), show_response(), error_bars(), kpi_spec(), input$dimension, GlimmaR_models, train_test_filter(), user_filter(), filter_operation())
    })
  })
}
viz_one_way_chart <- function(d, response, weight, show_labels, show_response, error_bars, kpi_spec, dimensions, GlimmaR_models, train_test_filter, user_filter, filter_operation){
  height_offset <- 355
  if(d[[1]][1]=='at least one input is NULL'){
    p <- plotly_empty(height = as.numeric(dimensions[2]) - height_offset, type = "scatter", mode = "markers") %>%
      config(displayModeBar = FALSE) %>%
      layout(title = list(text = 'Select x-axis feature',yref = "paper", y = 0.5)
      )
  } else if (d[[1]][1]=='too many levels to display (>5,000)') {
    p <- plotly_empty(height = as.numeric(dimensions[2]) - height_offset, type = "scatter", mode = "markers") %>%
      config(displayModeBar = FALSE) %>%
      layout(title = list(text = 'Too many levels to display',yref = "paper", y = 0.5)
      )
  } else if (nrow(d)==1){
    p <- plotly_empty(height = as.numeric(dimensions[2]) - height_offset, type = "scatter", mode = "markers") %>%
      config(displayModeBar = FALSE) %>%
      layout(title = list(text = 'x-axis feature has only one level',yref = "paper", y = 0.5)
      )
  } else {
    if(weight=='N'){
      first_line_col <- 3
    } else if (weight=='no weights'){
      first_line_col <- 3
    } else {
      first_line_col <- 4
    }
    # check for SHAP cols
    if(all(c('min','perc_5','mean','perc_95','max') %in% names(d))) SHAP_cols <- TRUE else SHAP_cols <- FALSE
    # remove rows with zero weight for plot (they make SHAP values look funny)
    include <- d[[first_line_col-1]] > 0
    d <- d[include]
    # check for LP col
    if('LP_mean' %in% names(d)) LP_col <- TRUE else LP_col <- FALSE
    # last non SHAP or LP line
    last_line_col <- ncol(d) - ifelse(SHAP_cols,5,0) - ifelse(LP_col,1,0)
    # setup plot
    xform <- list()
    yform <- list()
    yform2 <- list()
    # xform
    xform$xaxis_type <- 'category'
    xform$categoryorder <- 'array'
    xform$categoryarray <- d[[1]]
    xform$autotick <- FALSE
    xform$title <- boldify(ifelse(nrow(d)>200, paste0(names(d)[1], ' (', nrow(d), ' levels)'), names(d)[1]))
    xform$range <- NULL
    xform$showgrid <- FALSE
    xform$tickfont <- list(size = min(12,max(6,500/nrow(d))))
    xform$showticklabels <- ifelse(nrow(d)>200,FALSE,TRUE)
    # yform
    yform$showgrid <- FALSE
    yform$side <- 'right'
    yform$title <- boldify(names(d)[first_line_col-1])
    yform$range <- return_bar_limits(d[[first_line_col-1]])
    # yform2
    yform2$xaxis_type <- 'category'
    yform2$overlaying <- 'y'
    yform2$side <- 'left'
    #yform2$range <- return_y_axis_limits(d[[first_line_col]])
    if(show_response=='Show'){
      yform2$range <- return_y_axis_limits(as.matrix(d[,first_line_col:ncol(d)]))
    } else if (show_response=='Hide'){
      if(ncol(d)==last_line_col){
        col <- first_line_col
      } else {
        col <- last_line_col+1
      }
      yform2$range <- return_y_axis_limits(as.matrix(d[,col:ncol(d)]))
    }
    yform2$showgrid <- TRUE
    yform2$title <- boldify(names(d)[first_line_col])
    # add the bars, with distinct colours for NA and X
    colours <- rep('rgba(200, 240, 250,1.0)', nrow(d))
    na_col <- which(d[[1]]=='NA')
    X_col <- which(d[[1]]=='X')
    colours[na_col] <- 'rgba(255,150,150,1.0)'
    colours[X_col] <- 'rgba(255,150,150,1.0)'
    # choose weight labels
    if(show_labels=='-' | nrow(d)>200){
      weight_text_template <- NULL
    } else if (show_labels %in% c('Weight','All')){
      weight_text_template <- '%{y:.3s}'
    }
    # set up error bars
    eb <- NULL
    err_lower <- NULL
    err_higher <- NULL
    objective <- NA
    if(error_bars!='-'){
      p_value <- as.numeric(error_bars)
      if(first_line_col+1<=ncol(d)){
        # there is at least one prediction column present
        pred_col <- names(d)[[first_line_col+1]]
        weight_col <- names(d)[[first_line_col-1]]
        if(pred_col %in% names(GlimmaR_models)){
          # we have a predicted column from a GLM model
          # use the dispersion to derive the error bars
          dispersion <- GlimmaR_models[[pred_col]]$dispersion
          objective <- GlimmaR_models[[pred_col]]$objective
        } else if(names(d)[[first_line_col+1]]=='glm_prediction'){
          dispersion <- GlimmaR_models[[length(GlimmaR_models)]]$dispersion
          objective <- GlimmaR_models[[length(GlimmaR_models)]]$objective
        }
        if(!is.na(objective)){
          if(objective=='gaussian'){
            d[, err_lower := d[[pred_col]] - stats::qnorm(1-p_value, d[[pred_col]], sqrt(dispersion/d[[weight_col]]))]
            d[, err_higher := stats::qnorm(p_value, d[[pred_col]], sqrt(dispersion/d[[weight_col]]))- d[[pred_col]]]
          } else if(objective=='gamma'){
            s <- d[[pred_col]] / d[[weight_col]] * dispersion
            a <- d[[weight_col]] / dispersion
            d[, err_lower := stats::qgamma(1-p_value, shape = a, scale = s) - d[[pred_col]]]
            d[, err_higher := d[[pred_col]] - stats::qgamma(p_value, shape = a, scale = s)]
          } else if (objective=='binomial'){
            d[, err_lower := stats::qbinom(1-p_value, d[[weight_col]], d[[pred_col]])/d[[weight_col]] - d[[pred_col]]]
            d[, err_higher := d[[pred_col]] - stats::qbinom(p_value, d[[weight_col]], d[[pred_col]])/d[[weight_col]]]
          } else if (objective=='poisson'){
            d[, err_lower := stats::qpois(1-p_value, d[[weight_col]]*d[[pred_col]])/d[[weight_col]] - d[[pred_col]]]
            d[, err_higher := d[[pred_col]] - stats::qpois(p_value, d[[weight_col]]*d[[pred_col]])/d[[weight_col]]]
          } else if (objective=='quasipoisson'){
            # normal approximation as there are no percentiles for the quasipoisson
            # I'm not sure about this, so might turn it off
            d[, err_lower := d[[pred_col]] - stats::qnorm(1-p_value, d[[pred_col]], sqrt(dispersion*d[[pred_col]]/d[[weight_col]]))]
            d[, err_higher := stats::qnorm(p_value, d[[pred_col]], sqrt(dispersion*d[[pred_col]]/d[[weight_col]]))- d[[pred_col]]]
          } else if (objective=='tweedie'){
            # to be completed
          }
          eb <- list(
            color = '#888888',
            thickness = 0.5,
            type = "data",
            symmetric = FALSE,
            array = d[['err_lower']],
            arrayminus = d[['err_higher']]
          )
        }
      }
    }
    p <- plot_ly(height = as.numeric(dimensions[2]) - height_offset)
    # add weights
    p <- add_trace(p,
                   x = d[[1]],
                   y = d[[first_line_col-1]],
                   name = names(d)[first_line_col-1],
                   type = 'bar',
                   marker = list(color = colours),
                   yaxis = 'y',
                   texttemplate = weight_text_template,
                   textposition = 'outside',
                   textfont = list(color = 'rgba(100, 120, 125,1.0)')
    )
    if(show_response=='Show'){
      # add the lines
      for(i in first_line_col:last_line_col){
        pc <- plot_colour(i-first_line_col+1)
        if(i==first_line_col+1 & !is.null(eb)){
          errors_plot <- eb
        } else {
          errors_plot <- NULL
        }
        p <- add_trace(p,
                       x = d[[1]],
                       y = d[[i]],
                       type = 'scatter',
                       mode = 'lines+markers',
                       yaxis = 'y2',
                       name = names(d)[i],
                       marker = list(color = pc, size = 5),
                       line = list(color = pc, width = 2),
                       error_y = errors_plot
        )
      }
    }
    if(user_filter[1]=='no filter'){
      filter_text <- ''
    } else {
      filter_text <- combine_filters(user_filter, filter_operation)
    }
    if(train_test_filter=='All'){
      train_test_filter_text <- ''
    } else {
      train_test_filter_text <- train_test_filter
    }
    p <- p %>% layout(xaxis = xform,
                      yaxis = yform,
                      yaxis2 = yform2,
                      margin = list(r = 100, l = 50, t = 50),
                      title = list(text = paste0(boldify(yform2$title), filter_text, ' ', train_test_filter_text), font = list(size = 16, face='bold'))
                      ) %>%
      layout(legend = list(traceorder = 'normal',
                           orientation = 'v',
                           title=list(text='<b> Click to show/hide</b>'),
                           x = 1.05,
                           y = 1.05,
                           font = list(size = 10)
                           )
      )
    if(SHAP_cols){
      # add SHAP ribbons
      # mean
      # remove rows from d with NAs for SHAP values
      p <- p %>%
        add_trace(x = d[[1]], y = d[['mean']], type = 'scatter', mode = 'lines', yaxis = "y2",
                  line = list(color = 'rgba(200, 50, 50, 1.0)', dash = 'dot'),
                  showlegend = TRUE, name = 'SHAP_mean')
      # 5th-95th percentiles
      p <- p %>%
        add_trace(x = d[[1]], y = d[['perc_5']], type = 'scatter', mode = 'lines', yaxis = "y2",
                  fillcolor='rgba(200, 50, 50, 0.3)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
                  showlegend = FALSE, name = 'SHAP_5') %>%
        add_trace(x = d[[1]], y = d[['perc_95']], type = 'scatter', mode = 'lines', yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(200, 50, 50, 0.3)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
                  showlegend = TRUE, name = 'SHAP_5_95')
      # min to max SHAP
      p <- p %>%
        add_trace(x = d[[1]], y = d[['min']], type = 'scatter', mode = 'lines', yaxis = "y2",
                  fillcolor='rgba(200, 50, 50, 0.1)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
                  showlegend = FALSE, name = 'SHAP_min') %>%
        add_trace(x = d[[1]], y = d[['max']], type = 'scatter', mode = 'lines', yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(200, 50, 50, 0.1)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
                  showlegend = TRUE, name = 'SHAP_min_max')
    }
    if(LP_col){
      p <- p %>%
        add_trace(x = d[[1]], y = d[['LP_mean']], type = 'scatter', mode = 'lines', yaxis = "y2",
                  line = list(color = 'rgba(50, 50, 200, 1.0)', dash = 'dot'),
                  showlegend = TRUE, name = 'LP')
    }
    if(show_labels=='All'){
      range <- max(d[[first_line_col]], na.rm = TRUE) - min(d[[first_line_col]], na.rm = TRUE)
      p <- p %>%
        add_text(x = d[[1]],
                 y = d[[first_line_col]] + range * 0.03,
                 text = apply_kpi_format(d[[first_line_col]], response, weight, kpi_spec),
                 textfont = list(size = 10, color = 'black'),
                 textposition = "top",
                 yaxis = 'y2',
                 bgcolor = 'white',
                 showlegend = FALSE)
    }
  }
  # p <- p %>% toWebGL()
  p
}
return_y_axis_limits <- function(y){
  # return nicer limits than plotly defaults
  ymin <- min(y[is.finite(y)], na.rm = TRUE)
  ymax <- max(y[is.finite(y)], na.rm = TRUE)
  range <- ymax - ymin
  ymin_plot <- ymin - range * 0.05
  ymax_plot <- ymax + range * 0.1
  # if ymin_plot is close to zero vs the range then make zero
  if(abs(ymin_plot)/range<0.3 & ymin>=0){
    ymin_plot <- 0
  } else {
    # adjustment to get line out of way of bars
    ymin_plot <- ymin_plot - range * 0.3
  }
  return(c(ymin_plot,ymax_plot))
}
return_bar_limits <- function(y){
  # return nicer limits than plotly defaults
  ymin <- min(y, na.rm = TRUE)
  ymax <- max(y, na.rm = TRUE)
  ymin_plot <- 0
  ymax_plot <- ymax * 5
  return(c(ymin_plot,ymax_plot))
}
boldify <- function(x){
  paste('<b>', x, '</b>')
}
plot_colour <- function(x){
  # set up custom colours for up to three plots
  # after that it's all grey
  if(x==1){
    # dark grey
    pc <- 'rgb(40, 40, 40)'
  } else if (x==2){
    # nice red
    pc <- 'rgb(240, 50, 50)'
  } else if (x==3){
    # nice blue
    pc <- 'rgb(50, 60, 240)'
  } else if (x>3){
    # grey
    pc <- 'rgb(100, 100, 100)'
  }
  pc
}
