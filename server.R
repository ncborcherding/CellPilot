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

  #############################
  #Loading and Downloading Data
  #############################
  loaded_data <- reactive({
    req(input$dataSelect)
    if (!is.null(input$dataSelect)) {
      readRDS(file = paste0("./data/", input$dataSelect, ".rds"))
    } else {
      NULL
    }
  })
 
  
  output$download_SO <- downloadHandler(
    
    filename = function(){
      paste0(input$dataSelect, ".rds")
    }, 
    content = function(fname){
      saveRDS(loaded_data(), fname)
    }
  )
  
  output$download_meta<- downloadHandler(
    filename = function(){
      paste0(input$dataSelect, "_meta.data.csv")
    }, 
    content = function(fname){
      write.csv(loaded_data()@meta.data, fname)
    }
  )
  
  
  all_meta <- reactive({
    req(input$dataSelect)
    loaded_plot_data <- loaded_data()
    meta.headers <- data.frame(colnames(loaded_plot_data[[]]))
    colnames(meta.headers) <- "variables"
    meta.headers
  })
  
  all_dr <- reactive({
    req(input$dataSelect)
    dr <- names(loaded_data()@reductions)
    dr
  })
  
  all_genes <- reactive({
    # Load in the csv file containing all gene names used in the analysis. Used for quick accessibility
    req(input$dataSelect)
    genes <- data.frame(rownames(loaded_data()@assays$RNA@data))
    colnames(genes) <- "genes"
    genes
  })
  
  output$umapPlot <- renderPlotly({
    req(input$plot_meta)
    loaded_plot_data <- loaded_data()
    # Cells are colored according to the selection in the UI tSNE_plot_color
    dim_plot <- Seurat::DimPlot(object = loaded_plot_data)
    main <- Seurat::HoverLocator(plot = dim_plot,
                         information = SeuratObject::FetchData(object = loaded_plot_data,
                                                 vars = c("seurat_clusters", "donor")))
    
    # Next we create a legend for the right side of the plot that depicts the color scheme
    legend_label <- levels(loaded_plot_data@active.ident)
    legend_x_cord <- rep(1, length(legend_label))
    legend_y_cord <- rev(seq(1:length(legend_label)))
    manual_legend_data <- data.frame(legend_x_cord, legend_y_cord, legend_label)
    
    dim_leg <- ggplot(data = manual_legend_data,
                       mapping = aes(x     = as.factor(legend_x_cord),
                                     y     = legend_y_cord,
                                     label = as.character(legend_label))) +
      geom_point(size = 5)  +
      geom_text(position = position_nudge(x = 0)) +
      theme_classic() +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank()
      )
    
    leg <- ggplotly(dim_leg +
                      theme_classic() +
                      theme(legend.position = "none",
                            axis.line=element_blank(),
                            axis.text.x=element_blank(),
                            axis.text.y=element_blank(),
                            axis.ticks=element_blank()) +
                      xlab("") +
                      ylab(""))
    sp <- subplot(main, leg, titleY = TRUE, titleX = TRUE)
    sp %>% plotly::toWebGL()
  })
  
  output$featurePlot <- renderPlotly({
    req(input$plot_gene_heatmap)
    # Returns a feature plot - a heatmap of the dimensionality reduction overlayed with expression of th egene of interest
    #req(input$plot_gene_heatmap)
    loaded_plot_data <- loaded_data()
    feature_plot = Seurat::FeaturePlot(object = loaded_plot_data,
                               features = input$plot_gene_heatmap,
                               cols = c("grey", "blue")) +
      theme(legend.position = "none") +
      ggtitle("")
    
    # Below, we are creating a custom color scale to diplay on the right side of the feature plot
    # this scale depicts the transcripts per 10K (as all data has been normalized with a scale factor of 10,000)
    # This helps in correlating expression in the feature plot to the supplemental spreadsheets created for each publication.
    data <- SeuratObject::GetAssayData(object = loaded_plot_data, slot = "data") # Acquire the RNA assay data
    max_expression <- max(expm1(data[input$plot_gene_heatmap, ]))  # Find the cell with the maximum expression of the gene of interest (plot_gene_heatmap)
    
    if(max_expression > 0) {
      expression_sequenced <- data.frame(TP.10k = 1:round(max_expression))
    } else {
      # If there is no expression of a gene, we still need to create a dataframe that can be converted into a
      # legend so our plots look consistent, regardless of gene expression.
      expression_sequenced <- data.frame(TP.10k = 1:2)
    }
    
    heatmap_legend <- ggplot(expression_sequenced) +
      geom_tile(aes(x = 1, y = TP.10k, fill = TP.10k)) +
      scale_x_continuous(limits=c(0,2),breaks=1, labels = "TP.10K") +
      theme_classic() +
      theme(legend.position = "none") +
      xlab("") +
      ylab("")
    
    if(max_expression > 0) {
      heatmap_legend <- heatmap_legend + scale_fill_gradient(low = 'lightgrey', high = 'blue')
    } else {
      heatmap_legend <- heatmap_legend + scale_fill_gradient(low = 'lightgrey', high = 'lightgrey')
    }
    
    feature_plot <- Seurat::HoverLocator(plot = feature_plot,
                                 information = SeuratObject::FetchData(object = loaded_plot_data,
                                                         vars = c("seurat_clusters", "donor"))) %>% plotly::toWebGL()
    
    subplot(feature_plot,
            ggplotly(heatmap_legend),
            widths = c(0.9, 0.1),
            titleY = TRUE,
            titleX = TRUE)
    
  })
  
  output$featurePlotHelper <- renderUI({
    # The helper UI for the heatmap in which the user can select which gene should be heat-map-ified
    conditionalPanel(
      condition = "input.tabset1 == 'Feature Plot'",
      selectizeInput("plot_gene_heatmap",
                     label = h3("Genes to Plot"),
                     choices = NULL,
                     multiple = FALSE,
                     options= list(maxOptions = 100)
      )
    )
  })
  
  output$dimredPlotHelper <- renderUI({
    # The helper UI for the heatmap in which the user can select which gene should be heat-map-ified
    conditionalPanel(
      condition = "input.tabset1 == 'Dimensionality Reduction'",
      selectizeInput(session, 
                     "plot_meta",
                     label = h3("Variables to Plot"),
                     choices = NULL,
                     multiple = FALSE,
                     options= list(maxOptions = 100)
      )
    )
  })
  
  
  observeEvent(input$dataSelect, {
    choicesVec <- all_genes()$genes
    updateSelectizeInput(session, 
                         "plot_gene_heatmap",
                         choices = sort(choicesVec),
                         selected = NULL,
                         server=TRUE,
                         options = list(dropdownParent = 'body',
                                        openOnFocus = FALSE,
                                        items = c(),
                                        score = getScore()
                         )
    )
  })
  
  observeEvent(input$dataSelect, {
    choicesVec <- all_meta()$variables
    updateSelectizeInput(session, "plot_meta",
                         choices = choicesVec,
                         selected = NULL,
                         server=TRUE,
                         options = list(dropdownParent = 'body',
                                        openOnFocus = FALSE,
                                        items = c()
                         )
    )
  })
  
  # https://stackoverflow.com/questions/52039435/force-selectize-js-only-to-show-options-that-start-with-user-input-in-shiny
  getScore <- function() {
    # the updateSelectizeInput for displaying genes in the heatmaps and violin plots
    # is finicky for genes with short names. For example, when trying to querry expression
    # of the gene C2, hundreds of genes have C2 in their name. Therefore, I found this
    # stack-overflow post that allows one to only return items that start with the input
    # string
    return(I("function(search)
              {
               var score = this.getScoreFunction(search);
               return function(item)
                      {
                        return item.label
                        .toLowerCase()
                        .startsWith(search.toLowerCase()) ? 1 : 0;
                      };
              }"
    )
    )
  }
  


  #######################
  ### Embedded images ###
  #######################
  output$CellPilot <- renderImage({
    return(list(
      src = "./www/CellPilot.png",
      filetype = "image/png",
      width = 346.8, #2312
      height = 401.0, #2673
      alt = "error: www directory not found"
    ))
  }, deleteFile = FALSE)




})
