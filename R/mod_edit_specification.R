#' @import rhandsontable
#' @import data.table
#' @import shinyFiles
edit_specification_ui <- function(id) {
  ns <- NS(id)
  dimensionId <- ns("dimension")
  tagList(
    fluidRow(
      column(width = 12,
             style = 'margin-top: 0px; margin-bottom: -10px',
             shinyFiles::shinyFilesButton(
               id = ns('load_specification'),
               label = 'Load',
               filetype=list(txt="csv"),
               icon = icon('download'),
               title = 'Load specification file',
               style="color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left; padding: 10px 12px",
               multiple = FALSE
             ),
             shinyFiles::shinySaveButton(
               id = ns('save_specification'),
               label = 'Save',
               title = 'Save specification file',
               filename = "",
               filetype=list(txt="csv"),
               icon = icon('upload'),
               style="color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left; padding: 10px 12px",
               viewtype = "detail"
             ),
             div(htmlOutput(outputId = ns('spec_path')),
                 style = 'display: inline-block; margin-left: 10px; font-size: 12px; line-height: 1.0'
                 )
      )
    ),
    fluidRow(
      column(
        width = 12,
        align = 'right',
        p('right click in table to add/remove rows', style = 'font-size: 12px; margin: 0 0 0 0')
      )
    ),
    div(rhandsontable::rHandsontableOutput(ns('specification')), style = 'font-size: 12px')
  )

}
edit_specification_server <- function(id, specification, type) {
  moduleServer(id, function(input, output, session) {
    output$spec_path <- renderUI({HTML(paste0(' Specification path: <b>',
                                              ifelse(is.null(SPECIFICATION_PATH),
                                                     'none set',
                                                     SPECIFICATION_PATH
                                                     ),
                                              '</b><br/>type options(lucidum=list(specification_path="your_path"))
                                              into the console before library(lucidum)
                                              or add the command to your .Rprofile startup file'
                                              )
                                       )
      })
    observe({
      if(!is.null(specification())){
                  output$specification <- rhandsontable::renderRHandsontable({
                    rhandsontable::rhandsontable(
                      specification(),
                      selectCallback = TRUE,
                      rowHeaders = FALSE,
                      columnSorting = TRUE,
                      stretchH = "all",
                      height = input$dimension[2] - 500
                    )
                  })
      }
    })
    observe({
      if(is.null(SPECIFICATION_PATH) | SPECIFICATION_PATH=='specification path has not been set'){
        volumes <- c('Home' = fs::path_home(), shinyFiles::getVolumes()())
      } else {
        volumes <- c('Specifications' = SPECIFICATION_PATH, 'Home' = fs::path_home(), shinyFiles::getVolumes()())
      }
      shinyFileChoose(input, "load_specification", roots=volumes, session=session)
      fileinfo <- parseFilePaths(volumes, input$load_specification)
      isolate({
        if (nrow(fileinfo) > 0) {
          dt <- fread(fileinfo$datapath, header = TRUE, sep = ',')
          # check file for appropriateness
          valid_spec <- check_specification(dt, type)
          if(valid_spec){
            # ensure correct column formats
            # valid specification
            # make logical columns character - otherwise rhandontable will render as logical
            logical_cols <- names(dt)[which(as.vector(dt[,lapply(.SD, class)]) == "logical")]
            if(length(logical_cols)>0){
              dt[, (logical_cols):= lapply(.SD, as.character), .SDcols = logical_cols]
            }
            output$specification <- rhandsontable::renderRHandsontable({
              rhandsontable::rhandsontable(
                dt,
                selectCallback = TRUE,
                rowHeaders = FALSE,
                columnSorting = TRUE,
                stretchH = "all",
                height = input$dimension[2] - 500
              )
            })
            confirmSweetAlert(session = session,
                              type = 'success',
                              inputId = "spec_load_OK",
                              title = paste0(type, ' specification loaded'),
                              btn_labels = c('OK')
            )
          } else {
            confirmSweetAlert(session = session,
                              type = 'error',
                              inputId = "spec_load_error",
                              title = paste0('Error loading ',type, ' specification'),
                              text = 'Check file headers',
                              btn_labels = c('OK')
                              )
          }
        }
      })
    })
    observe({
      if(is.null(SPECIFICATION_PATH) | SPECIFICATION_PATH=='specification path has not been set'){
        volumes <- c('Home' = fs::path_home(), shinyFiles::getVolumes()())
      } else {
        volumes <- c('Specifications' = SPECIFICATION_PATH, 'Home' = fs::path_home(), shinyFiles::getVolumes()())
      }
      shinyFileSave(input, "save_specification", roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$save_specification)
      if(nrow(fileinfo)>0){
        dt <- setDT(rhandsontable::hot_to_r(isolate({input$specification})))
        fwrite(dt, fileinfo$datapath)
      }
    })
    spec_dt <- reactive({
      dt <- setDT(rhandsontable::hot_to_r(input$specification))
      if(nrow(dt)==0){
        specification()
        } else {
          dt
          }
      })
    return(spec_dt)
  })
}
edit_specification_demo <- function() {
  ui <- fluidPage(
    br(),
    textOutput('first_cell'),
    br(),
    edit_specification_ui('demo'),
    )
  server <- function(input, output, session) {
    RVs <- reactiveValues(myval = NULL)
    RVs$myval <- edit_specification_server("demo", reactive(datasets::iris))
    output$first_cell <- renderText({
      as.character(RVs$myval()[1,1])
      })
  }
  shinyApp(ui, server)
}
