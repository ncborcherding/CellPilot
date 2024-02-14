library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(plotly)

rds.files <- list.files("./data", pattern = "\\.rds$")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "menutab",
    menuItem(text = "Dashboard",
             tabName = "dashboard",
             icon = icon("dashboard")
             ),
    menuItem(text = "About CellPilot",
             tabName = "CellPilot",
             icon = icon("plane-departure")
    ),
   selectInput("dataSelect", "Choose a data file:", 
                         choices = c("","Figure1", "Figure2", "Figure3", "Figure7"), 
                         multiple = FALSE),
   menuItem(text = "Download Data", 
            menuSubItem(downloadButton('download_SO',
                    "Seurat Object", 
                    style ="padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
                    icon = NULL,),
            menuSubItem(downloadButton('download_meta',
                  "Meta Data",
                  style ="padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
                  icon = NULL)
   )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
              tabBox(width = 8,
                id = "tabset1",
                height = "700px",
                tabPanel("Dimensionality Reduction",
                         plotlyOutput("umapPlot", height = "650px") %>% withSpinner(type = 6, color = "#1F6BFF")
                         ),
                tabPanel("Feature Plot",
                         plotlyOutput("featurePlot", height = "650px")),
                tabPanel("Violin Plot",
                         plotlyOutput("violinPlot", height = "650px")
                         )
                ),
              tabBox(width = 4,
                     id = "options1",
                     tabPanel( uiOutput("dimredPlotHelper"),
                               uiOutput("featurePlotHelper"),
                               uiOutput("violinPlotHelper"),
                               uiOutput("violinMetaHelper"))
              )
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
                       Please refer to the manuscript to identify the data of interest. Aligned data will be available on
                        Zenodo at the ", a(href = "https://zenodo.org/records/10257572", "10257572"), " repository.",
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
  dashboardHeader(title = "CellPilot"),
  sidebar,
  body
)

