#' Shiny app server
#'
#' @param input provided by shiny
#' @param output provided by shiny
#'
#' @export
#' @importFrom ggplot2 ggplot aes element_blank theme geom_point geom_text position_nudge
#' @importFrom ggplot2 theme_classic ggtitle xlab ylab geom_tile scale_x_continuous
#' @importFrom ggplot2 scale_fill_gradient scale_y_reverse coord_flip ggsave
#' @importFrom dplyr full_join mutate filter select arrange desc count
#' @importFrom DT renderDataTable
#' @importFrom magrittr "%>%"
#' @importFrom grDevices hcl
#' @importFrom stats hclust rnorm
#' @importFrom Seurat Idents DimPlot FeaturePlot GetAssayData NormalizeData FindVariableFeatures WhichCells RunUMAP RunTSNE VariableFeatures ScaleData RunPCA FindNeighbors Tool FindClusters FindMarkers DefaultAssay FetchData HoverLocator AverageExpression
#' @importFrom ggdendro dendro_data segment label
#' @importFrom plotly ggplotly renderPlotly subplot plot_ly layout event_data toWebGL
#' @importFrom patchwork plot_layout
#' @importFrom shiny Progress validate reactive renderText renderUI req validate conditionalPanel selectInput renderImage actionButton selectizeInput
#' @importFrom shiny updateSelectizeInput downloadButton eventReactive checkboxGroupInput radioButtons sliderInput withProgress shinyServer icon plotOutput
#' @importFrom shiny strong
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom rlang %||% UQ
#' @importFrom shinyFiles shinyDirChoose parseDirPath getVolumes
#' @importFrom methods is new

shinyAppServer <- shinyServer(function(session, input, output) {

  
  observe({
    data_files <- list.files(path = "./data", full.names = TRUE)
    updateSelectInput(session, "dataSelect", choices = data_files)
  })
  
  selected_data <- reactive({
    if (!is.null(input$dataSelect)) {
      readRDS(file.path("./data", input$dataSelect))
    } else {
      NULL
    }
  })
  
  


  #######################
  ### Embedded images ###
  #######################
  output$CellPilot <- renderImage({
    return(list(
      src = "../www/CellPilot.png",
      filetype = "image/png",
      width = 346.8, #2312
      height = 401.0, #2673
      alt = "error: www directory not found"
    ))
  }, deleteFile = FALSE)




})
