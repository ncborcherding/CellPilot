library(shiny)
library(ggplot2)
library(magrittr)
library(Seurat)
library(plotly)
library(stringr)

#Increasing size for download purposes
options(shiny.maxRequestSize = 1000 * 1024^2)  # 1 GB limit

shinyAppServer <- shinyServer(function(session, input, output) {
  # Allow session to reconnect if it gets disconnected
  session$allowReconnect(TRUE)
  
   "%!in%" <- Negate("%in%")
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
     filename = function() {
       # Use the selected input to determine the file name dynamically
       paste0(input$dataSelect, ".rds")
     },
     content = function(file) {
       # Define the full path to the file in the /data/ directory
       file_path <- file.path("data", paste0(input$dataSelect, ".rds"))
       
       # Initialize the progress bar
       progress <- shiny::Progress$new()
       on.exit(progress$close())
       progress$set(message = "Preparing download...", value = 0)
       
       # Check if the file exists
       if (!file.exists(file_path)) {
         stop("The selected file does not exist.")
       }
       
       # Get the size of the file (in MB) to adjust progress and timing
       file_size <- file.info(file_path)$size / (1024^2)  # Convert bytes to MB
       
       # Determine the number of steps and sleep time based on file size
       if (file_size <= 10) {
         steps <- 5
         sleep_time <- 0.5
       } else if (file_size <= 100) {
         steps <- 10
         sleep_time <- 1
       } else {
         steps <- 20
         sleep_time <- 2
       }
       
       # Simulate download preparation steps
       for (i in 1:steps) {
         Sys.sleep(sleep_time)  # Simulate processing
         progress$inc(1 / steps, detail = paste("Step", i, "of", steps))
       }
       
       # Copy the file from /data/ to the target location for download
       file.copy(file_path, file)
     }
   )
   
   
  
  output$download_meta<- downloadHandler(
    filename = function(){
      paste0(input$dataSelect, "_meta.data.csv")
    }, 
    content = function(fname){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Downloading data...", value = 0)
      
      # Simulate steps in data download
      for (i in 1:10) {
        Sys.sleep(10)  # Simulate download step
        progress$inc(1 / 10)  # Increment progress
      }
      
      write.csv(loaded_data()@meta.data, fname)
    }
  )
  
  ######################################
  #Grabbing Variables to Adaptively Plot
  ######################################
  
  all_meta <- reactive({
    req(input$dataSelect)
    meta.headers <- colnames(loaded_data()@meta.data)
    meta.headers <- meta.headers[meta.headers %in% c("seurat_clusters", "donor", "tissue",  "timepoint", "orig.ident", "db.class", "CD4.annot", "CD8.annot", "DICE.labels", "DICE.pruned.labels", "CTgene", "CTnt", "CTaa", "CTstrict", "TCRB.epitope", "TCRA.epitope" )]
    meta.headers <- meta.headers[match( c("seurat_clusters", "donor", "tissue",  "timepoint", "orig.ident", "db.class", "CD4.annot", "CD8.annot", "DICE.labels", "DICE.pruned.labels", "CTgene", "CTnt", "CTaa", "CTstrict", "TCRB.epitope", "TCRA.epitope" ), meta.headers)]
    meta.headers <- data.frame(meta.headers)
    colnames(meta.headers) <- "variables"
    meta.headers
  })
  
  observeEvent(input$dataSelect, {
    choicesVec <- all_meta()$variables
    updateSelectizeInput(session, "plot_meta",
                         choices = choicesVec,
                         selected = NULL,
                         server = TRUE,
                         options = list(dropdownParent = 'body',
                                        openOnFocus = FALSE,
                                        items = c(),
                                        score = getScore()
                         )
    )
  })
  
  dr.use <- reactive({
    req(input$dataSelect)
    dr <- names(loaded_data()@reductions)[1]
    dr
  })
  
  all_genes <- reactive({
    # Load in the csv file containing all gene names used in the analysis. Used for quick accessibility
    req(input$dataSelect)
    genes <- rownames(loaded_data()@assays$RNA@data)
    if(any(Matrix::rowSums(loaded_data()@assays$RNA@data) == 0)) {
      genes <- genes[-as.vector(which(Matrix::rowSums(loaded_data()@assays$RNA@data) == 0))]
    }
    genes <- data.frame(genes)
    colnames(genes) <- "genes"
    genes
  })
  
  observeEvent(input$dataSelect, {
    choicesVec <- all_genes()$genes
    updateSelectizeInput(session, 
                         "plot_gene_heatmap",
                         choices = sort(choicesVec),
                         selected = NULL,
                         server = TRUE,
                         options = list(dropdownParent = 'body',
                                        openOnFocus = FALSE,
                                        items = c(),
                                        score = getScore()
                         )
    )
  })
  
  observeEvent(input$dataSelect, {
    choicesVec <- all_genes()$genes
    updateSelectizeInput(session, 
                         "plot_gene_violin",
                         choices = sort(choicesVec),
                         selected = NULL,
                         server = TRUE,
                         options = list(dropdownParent = 'body',
                                        openOnFocus = FALSE,
                                        items = c(),
                                        score = getScore()
                         )
    )
  })
  
  observeEvent(input$dataSelect, {
    choicesVec <- all_meta()$variables
    updateSelectizeInput(session, "plot_meta_violin",
                         choices = choicesVec,
                         selected = NULL,
                         server = TRUE,
                         options = list(dropdownParent = 'body',
                                        openOnFocus = FALSE,
                                        items = c(),
                                        score = getScore()
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
  
  #################
  #Adaptive Menus
  ################
  
  output$dimredPlotHelper <- renderUI({
    conditionalPanel(
      condition = "input.tabset1 == 'Dimensionality Reduction'",
      selectizeInput("plot_meta",
                     label = h3("Variables to Plot"),
                     choices = NULL,
                     multiple = FALSE,
                     options= list(maxOptions = 20)
      )
    )
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
  
  output$violinPlotHelper <- renderUI({
    # The helper UI for the violin plot. Multiple genes can be input if the violin plot tab is selected within tabset 2.
    conditionalPanel(
      condition = "input.tabset1 == 'Violin Plot'",
      selectizeInput('plot_gene_violin',
                     label = h3("Violin genes"),
                     choices = NULL,
                     multiple= FALSE,
                     options= list(maxOptions = 100)
      )
      
    )
    
  })
  
  output$violinMetaHelper <- renderUI({
    # The helper UI for the violin plot. Multiple genes can be input if the violin plot tab is selected within tabset 2.
    conditionalPanel(
      condition = "input.tabset1 == 'Violin Plot'",
      selectizeInput('plot_meta_violin',
                     label = h3("Grouping Variables"),
                     choices = NULL,
                     multiple= FALSE,
                     options= list(maxOptions = 20)
      )
      
    )
    
  })
  
  
  #####################
  #Plotting Functions
  ####################
  
  #Dim Red Plot
  output$umapPlot <- renderPlotly({
    loaded_plot_data <- loaded_data()
    
    # Next we create a legend for the right side of the plot that depicts the color scheme
    legend_label <- stringr::str_sort(levels(as.factor(loaded_plot_data@meta.data[,input$plot_meta])), numeric = TRUE)
    legend_x_cord <- rep(1, length(legend_label))
    legend_y_cord <- rev(seq(1:length(legend_label)))
    manual_legend_data <- data.frame(legend_x_cord, legend_y_cord, legend_label)
    
    if(length(legend_label) <= 35) {
      colors_use <- colorRampPalette(RColorBrewer::brewer.pal(11, "Paired"))(length(legend_label))
    } else {
      colors_use <- colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlBu"))(length(legend_label))
    }
    
    names(colors_use) <- legend_label
    dim_plot <- Seurat::DimPlot(object = loaded_plot_data,
                                group.by = input$plot_meta,
                                reduction = dr.use(),
                                raster = FALSE) + 
      scale_color_manual(values = colors_use)
    
    if(input$plot_meta %in% c("TCRB.epitope", "TCRA.epitope")) {
      #Recreating HoverLocator() to add size and alpha
      plot.build <- suppressWarnings(expr = Seurat:::GGpointToPlotlyBuild(
        plot = dim_plot,
        information = SeuratObject::FetchData(object = loaded_plot_data,
                                              vars = c("donor", "tissue", "timepoint", input$plot_meta))
      ))
      plot.build$alpha <- ifelse(plot.build$color == "grey50", 0.2, 1)
      plot.build$size <- ifelse(plot.build$color == "grey50", 1, 3)
      
      xaxis <- list(title = names(x = data)[1],
                    showgrid = FALSE,
                    zeroline = FALSE,
                    showline = TRUE)
      
      yaxis <- list(title = names(x = data)[2],
                    showgrid = FALSE,
                    zeroline = FALSE,
                    showline = TRUE)
      
      title = list(color = 'black')
      plotbg = 'white'
      
      main = plot_ly(data = plot.build,
                  x = ~x,
                  y = ~y,
                  type = 'scatter',
                  mode = 'markers',
                  color = ~I(color),
                  marker = list(opacity = ~alpha, 
                                size = ~size),
                  hoverinfo = 'text',
                  text = ~feature)
      
      main <- plotly::layout(main,
                          xaxis = xaxis,
                          yaxis = yaxis,
                          title = dim_plot$labels$title,
                          titlefont = title,
                          paper_bgcolor = plotbg,
                          plot_bgcolor = plotbg)
    } else {
        main <- Seurat::HoverLocator(plot = dim_plot,
                                 information = SeuratObject::FetchData(object = loaded_plot_data,
                                                                       vars = c("donor", "tissue", "timepoint", input$plot_meta)))
    }
    
    if (length(legend_label) <= 30 && length(legend_label) > 1) {
    
    
      dim_leg <- ggplot(data = manual_legend_data,
                         mapping = aes(x     = as.factor(legend_x_cord),
                                       y     = legend_y_cord,
                                       label = as.character(legend_label))) +
        geom_point(size = 3, color = colors_use)  +
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
      sp <- subplot(main, leg, titleY = TRUE, titleX = TRUE, widths = c(0.8, 0.2))
      sp %>% plotly::toWebGL()
    } else {
      main
    }
  })
  
  #Feature Plot
  output$featurePlot <- renderPlotly({
    req(input$plot_gene_heatmap)
    # Returns a feature plot - a heatmap of the dimensionality reduction overlayed with expression of th egene of interest
    loaded_plot_data <- loaded_data()
    feature_plot = Seurat::FeaturePlot(object = loaded_plot_data,
                               features = input$plot_gene_heatmap,
                               reduction = dr.use(),
                               raster = FALSE) + 
      scale_color_gradientn(colors = rev(RColorBrewer::brewer.pal(11, "RdYlBu")))
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
      scale_x_continuous(limits=c(0,2),breaks=1, labels = "Expr") +
      theme_classic() +
      theme(legend.position = "none") +
      xlab("") +
      ylab("")
    
    if(max_expression > 0) {
      heatmap_legend <- heatmap_legend + scale_fill_gradientn(colors = rev(
        RColorBrewer::brewer.pal(11, "RdYlBu")))
    } else {
      heatmap_legend <- heatmap_legend + scale_fill_gradient(low = 'lightgrey', high = 'lightgrey')
    }
    
    feature_plot <- Seurat::HoverLocator(plot = feature_plot,
                                 information = SeuratObject::FetchData(object = loaded_plot_data,
                                                         vars = c("seurat_clusters", "donor", "tissue", "timepoint"))) %>% plotly::toWebGL()
    
    subplot(feature_plot,
            ggplotly(heatmap_legend),
            widths = c(0.9, 0.1),
            titleY = TRUE,
            titleX = TRUE)
    
  })
  
  output$violinPlot <- renderPlotly({
    req(input$plot_gene_violin)
    # Returns a feature plot - a heatmap of the dimensionality reduction overlayed with expression of th egene of interest
    loaded_plot_data <- loaded_data()
                    
    legend_label <- stringr::str_sort(levels(as.factor(loaded_plot_data@meta.data[,input$plot_meta_violin])), numeric = TRUE)
    legend_x_cord <- rep(1, length(legend_label))
    legend_y_cord <- rev(seq(1:length(legend_label)))
    manual_legend_data <- data.frame(legend_x_cord, legend_y_cord, legend_label)
    
    #Adding discrete reorder of x-axis here
    loaded_plot_data@meta.data[,input$plot_meta_violin] <- factor(loaded_plot_data@meta.data[,input$plot_meta_violin], levels = legend_label)
    
    colors_use <- colorRampPalette(RColorBrewer::brewer.pal(11, "Paired"))(length(legend_label))
    
    
    violin_plot = Seurat::VlnPlot(object = loaded_plot_data,
                                  features = input$plot_gene_violin, 
                                  group.by = input$plot_meta_violin) + 
      scale_fill_manual(values = colors_use)+
      scale_x_discrete(limits = rev) + 
      theme(legend.position = "none",
          axis.title.y = element_blank()) +
      coord_flip() + 
      ggtitle("")
    
    ggplotly(violin_plot)
  })
  
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
