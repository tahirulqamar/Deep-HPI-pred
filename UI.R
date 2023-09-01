library(shiny)
library(shinythemes)
library(markdown)
library(shinyalert)
library(visNetwork)
library(igraph)
library(tidygraph)
library(ggraph)
library(dplyr)
library(magrittr)
library(DOSE)
library(httr)
library(XML)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(dplyr)

ui <- navbarPage(
  title = div(class = "navbar-brand-container",
              "Deep-HPI-pred: Deep Learning based Host-Pathogen Interaction Prediction",
              tags$img(src = "logo.png", height = "180px", style = "float: right; margin-left: 25px;")
  ),
  theme = shinytheme("sandstone"),
  windowTitle = "Deep-HPI-pred: Deep Learning based Host-Pathogen Interaction Prediction",
  tags$head(
    tags$style(HTML("
      body {
        background-size: cover;
        color: black;
      }
      .navbar.navbar-default {
        background-color: #8282cc !important;
        display: flex;
        align-items: center;
      }
      .navbar .navbar-brand {
        font-family: 'Franklin Gothic Heavy';
        color: white;
        font-size: 29px;
      }
      .nav.navbar-nav > li > a {
        color: white !important;
        font-weight: bold;
      }
      .well {
        background-image: linear-gradient(to right, #f8f9fa, #e6e6e6);
        border: 1px solid #ddd;
        box-shadow: 2px 2px 5px rgba(0, 0, 0, 0.15);
        font-family: 'Cordia New', Courier, monospace;
      }
      .btn {
        background-color: #8282cc !important;
        color: white !important;
      }
    "))
  ),
  useShinyalert(), 
  tabPanel("Home",
           sidebarLayout(
             sidebarPanel(
               titlePanel("Quick Links"),
               tags$ul(
                 tags$li(tags$a(href = "https://github.com/tahirulqamar/Deep-HPI-pred", "GitHub")),
                 tags$li(tags$a(onClick = 'Shiny.setInputValue("show_faq", Math.random(), {priority: "event"})', "FAQ")),
                 tags$li(tags$a(onClick = 'Shiny.setInputValue("show_contact", Math.random(), {priority: "event"})', "Contact Us"))
               )
             ),
             mainPanel(
               column(12, 
                      wellPanel(
                        titlePanel("Welcome to Deep-HPI-pred"),
                        p("Deep-HPI-pred applet integrates both the detection and visualization of host-pathogen interaction networks. It is designed to help researchers better understand both model and non-model host-pathogen systems."),
                        p("By using our web service, you can:"),
                        tags$ul(
                          tags$li("Detect and visualize host-pathogen interaction networks"),
                          tags$li("Generate meaningful hypotheses for your research"),
                          tags$li("Design appropriate experiments"),
                          tags$li("Benefit from our ongoing updates and improvements")
                        ),
                        p("Whether you're studying host-pathogen interactions in a lab setting or you're a bioinformatician interested in computational approaches, Deep-HPI-Pred is here to support your research goals."),
                        p("To get started, please select a function from the menu or visit our documentation for a detailed guide.")
                      )
               )
             )
           )),
  tabPanel("Browse Search",
           fluidRow(
             column(6,
                    wellPanel(
                      titlePanel("Upload Your Data"),
                      fileInput('file1', 'Choose CSV File', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                      selectInput("id_type", "Input Format:", choices = c("UNIPROTKB")),
                      actionButton("go", "Train and Predict")
                    )
             ),
             column(6,
                    wellPanel(
                      titlePanel("Load Demo Data"),
                      actionButton("demo", "Load Demo Data"),
                      actionButton("clear", "Clear Data"),
                      tableOutput("demo_data"),
                      visNetworkOutput("demo_network", height = "800px")
                    )
             )
           )),
  tabPanel("Results",
           fluidRow(
             column(6,
                    wellPanel(
                      titlePanel("Network Visualization"),
                      visNetworkOutput("network", height = "800px")
                    ),
                    wellPanel(
                      h3("Legend:"),
                      tags$ul(
                        tags$li(span(style = "color: blue;", "Host Protein")),
                        tags$li(span(style = "color: red;", "Pathogen Protein"))
                      )
                    )
             ),
             column(6,
                    downloadButton("downloadData", "Download Data"),
                    tableOutput("predicted_interactions_network")
             )
           )),
  tabPanel("GO Analysis",
           fluidRow(
             column(12, # Using full width
                    wellPanel(
                      titlePanel("GO Enrichment Analysis"),
                      selectInput("plotType", "Select plot type:", choices = c("Network Graph")),
                      plotOutput("go_plot", height = "900px", width = "1500px"),  # or any other value
                      downloadButton("downloadPlot", "Download Plot"),  # Button to download the plot
                      tableOutput("go_table")
                    )
             )
           )),
  tabPanel("Documentation", 
           includeHTML("documentation.html"))
)
