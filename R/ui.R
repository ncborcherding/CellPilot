#' Shiny app server
#'
#' @export
#'
#' @importFrom shiny fluidPage fluidRow column uiOutput tabPanel textOutput h1 h2 h3 h4 h5 br p a imageOutput
#' @importFrom plotly plotlyOutput
#' @importFrom shinydashboard dashboardSidebar sidebarMenu menuItem dashboardBody tabItems tabItem tabBox box dashboardPage dashboardHeader
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyFiles shinyDirButton
#' @importFrom DT dataTableOutput

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "menutab",
    menuItem(text = "Dashboard",
             tabName = "dashboard",
             icon = icon("dashboard")
    ),
    shinyDirButton("directory", "Select Seurat object directory", "Please select a folder"),
    uiOutput("dataSelect"),
    menuItem(text = "CellPilot",
             tabName = "CellPilot",
             icon = icon("file-code-o")
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
              tabBox(
                id = "tabset1",
                height = "700px",
                tabPanel("Dimensionality Reduction",
                         textOutput("instructUser"),
                         textOutput("directoryWarning"),
                         plotlyOutput("umapPlot", height = "650px") %>% withSpinner(type = 6, color = "#1F6BFF")
                ),
                tabPanel("Heatmap",
                         plotlyOutput("featurePlot", height = "650px")
                ),
                tabPanel("Differential Expression (Group 1)",
                         plotlyOutput("dePlot1", height = "650px")
                )
              ),
              tabBox(
                id = "tabset2",
                height = "700px",
                tabPanel("Violin Plot",
                         plotOutput("violin_plot", height = "650px")

                ),
                tabPanel("Recluster",
                         plotlyOutput("recluster_plot", height = "650px")
                ),
                tabPanel("Differential Expression (Group 2)",
                         plotlyOutput("dePlot2", height = "650px")
                )
              )
            ),
            fluidRow(
              box(
                column(6,
                       uiOutput("umapHelper"),
                       uiOutput("featurePlotHelper"),
                       uiOutput("deHelper"),
                       uiOutput("deHelper2"),
                       uiOutput("deHelper6"),
                       uiOutput("deHelper3"),
                       uiOutput("deHelper5")
                )
              ),
              box(
                column(6,
                       uiOutput("violinHelper"),
                       uiOutput("violinDownload"),
                       uiOutput("reclusterHelper"),
                       uiOutput("reclusterHelper9"),
                       uiOutput("reclusterHelper3"),
                       uiOutput("reclusterHelper6"),
                       uiOutput("reclusterHelper7"),
                       uiOutput("reclusterHelper8"),
                       uiOutput("reclusterHelper2"),
                       uiOutput("reclusterHelper4"),
                       uiOutput("reclusterHelper5"),
                       uiOutput("DE_group_1"),
                       uiOutput("deHelper4")
                ),
                column(6,
                       uiOutput("DE_group_2")
                )
              )
            ),
            fluidRow(
              box(
                plotlyOutput("delta_plotly")
              ),
              box(
                plotlyOutput("cluster_visualization")
              )
            ),

            fluidRow(
              DT::dataTableOutput("DE_table")
            )
    ),

    tabItem(tabName = "CellPilot",
            h3(strong("Fly through single-cell data with CellPilot")),
            fluidRow(
              column(6,
                     br(),
                     p("CellPilot is a interactive platform for single-cell data for Seurat v5 that allows for the 
                       analysis of data, production of figure-quality graphs, and downloading of data.", style = "font-size:20px"),
                     br(),
                     p("The data available for analysis and download in CellPilot is derived from the ",
                       a(href = "https://www.researchsquare.com/article/rs-3304466/v1", "prepint."), 
                       "The data as a whole is comprised of peripheral blood and/or lymph node samples from 
                       HLA-DPB1*04-restricted patients that underwent the BNT162b2 mRNA vaccine series or natural infection. 
                       Please refer to the manuscript to identify the data of interest.",
                       style = "font-size:20px"),
                     br(),
                     p("The software is based on ", strong("cellcuratoR"), " by Drew Voigt, more information can be found in the following ", 
                       a(href = "https://pubmed.ncbi.nlm.nih.gov/32910939/", "manuscript."), 
                       "Code and documentation are available via the ",
                       a(href = "https://github.com/ncborcherding/CellPilot", "GitHub repository"), 
                       "where inquiries and feature requests can be made or",
                       a("by email.", href="mailto:ncborch@gmail.com"), style = "font-size:20px"),
                       style = "font-size:20px"
              ),
              column(6,
                     imageOutput("CellPilot", width = "100%")
              )

            )
    )
  )
)

# Put them together into a dashboardPage
shinyAppUI <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "CellPilot"
  ),
  sidebar,
  body
)
