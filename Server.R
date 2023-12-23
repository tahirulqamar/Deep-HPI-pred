server <- function(input, output, session) {
  observeEvent(input$show_faq, {
    shinyalert::shinyalert(
      title = "Frequently Asked Questions",
      text = paste0(
        "1. How to use the Deep-HPI-pred app?\n",
        "Answer: To use the application, select an option from the menu and follow the provided steps. You can either upload your own data or use the demo data provided.\n\n",
        
        "2. Which data formats are accepted?\n",
        "Answer: The app currently accepts data in CSV format. Make sure your file is properly formatted according to the instructions in the documentation.\n\n",
        
        "3. How can I interpret the results?\n",
        "Answer: Our application provides various visualizations to help interpret the results. Refer to the 'Documentation' tab for a detailed guide on how to interpret the results.\n\n",
        
        "4. Can I download my results?\n",
        "Answer: Yes, the application provides an option to download your results in various formats for further offline analysis.\n\n",
        
        "5. What if I face an issue or error while using the app?\n",
        "Answer: If you encounter any issues while using the application, feel free to contact us via the 'Contact' link."
      ),
      type = "info"
    )
  })
  observeEvent(input$show_contact, {
    shinyalert::shinyalert(
      title = "Contact",
      text = paste0(
        "For any questions or concerns, feel free to reach out to us. Contact details:<br><br>",
        "<div style='font-size: 14px; line-height: 1.5;'>",
        "<span style='font-weight: bold; color: #2c3e50;'>Dr. Muhammad Tahir ul Qamar:</span> ",
        "<a href='mailto:m.tahirulqamar@hotmail.com' style='color: #3498db;'>m.tahirulqamar@hotmail.com</a><br>",
        
        "<span style='font-weight: bold; color: #2c3e50;'>Ms. Fatima Noor:</span> ",
        "<a href='mailto:fatimanoor1122@yahoo.com' style='color: #3498db;'>fatimanoor1122@yahoo.com</a><br>",
        
        "<span style='font-weight: bold; color: #2c3e50;'>Dr. Xi-Tong Zhu:</span> ",
        "<a href='mailto:z724@qq.com' style='color: #3498db;'>z724@qq.com</a><br>",
        
        "<span style='font-weight: bold; color: #2c3e50;'>Dr. Yi-Xiong Guo:</span> ",
        "<a href='mailto:guoyixiong@webmail.hzau.edu.cn' style='color: #3498db;'>guoyixiong@webmail.hzau.edu.cn</a><br>",
        
        "<span style='font-weight: bold; color: #2c3e50;'>Prof. Ling-Ling Chen:</span> ",
        "<a href='mailto:llchen@gxu.edu.cn' style='color: #3498db;'>llchen@gxu.edu.cn</a>",
        "</div>"
      ),
      type = "info",
      html = TRUE
    )
  })
  observeEvent(input$show_cite, {
    shinyalert(
      title = "Citation",
      text = "ul Qamar, M. T., Noor, F., Guo, Y. X., Zhu, X. T., & Chen, L. L. (2023). Deep-HPI-pred: an R-Shiny applet for network-based classification and prediction of Host-Pathogen protein-protein interactions. Computational and Structural Biotechnology Journal.",
      type = "info"
    )
  })
  rv <- reactiveValues(data = NULL, demo = NULL, go_annotations = NULL)
  
  observeEvent(input$file1, {
    # If no file is uploaded, return NULL
    req(input$file1)
    
    # Write the uploaded file to the disk as 'input.csv'
    write.csv(read.csv(input$file1$datapath), file = "input.csv", row.names = FALSE)
  })
  
  observeEvent(input$demo, {
    rv$demo <- data.frame(
      pathogen_protein = c(
        "P62277", "P62278", "P62279", 
        "P62280", "Q7Z3B4", "P25398", 
        "Q08050", "Q5VT06", "O95793", 
        "Q7Z3B4", "P25398", "Q08050",
        "Q5VT06", "O95793", "B2R4R0",
        "P50748"
      ), 
      host_protein = c(
        "Q1HVI8", "P05954", "Q2HRD4", 
        "I6TAH8", "Q1HVI8", "P05954", 
        "Q2HRD4", "I6TAH8", "Q99IB8",
        "P14079", "B0FAN4", "I6TAH8",
        "Q99IB8", "P05954", "Q2HRD4",
        "I6TAH8"
      )
    )
    
  })
  
  
  observeEvent(input$clear, {
    rv$demo <- NULL
    rv$data <- NULL
    rv$go_annotations <- NULL
  })
  
  observeEvent(input$go, {
    # Show modal dialog while the job is running
    showModal(
      modalDialog(
        title = "Please be patient",
        tags$div(
          style = "text-align: center; font-family: 'Arial';",
          tags$h3("your query is being processed...", style = "color: #000000;"),
          tags$div(
            style = "display: inline-block; margin-top: 10px;",
            tags$div(id = "loading-dots",
                     tags$span(style = "color: #FF0000;", "●"),
                     tags$span(style = "color: #00FF00;", "●"),
                     tags$span(style = "color: #0000FF;", "●")
            )
          )
        ),
        footer = NULL,
        closable = FALSE,
        easyClose = FALSE,
        size = "m",  # Change the size argument to "m"
        background = "#b300b3",
        tags$style(HTML("
        .modal-dialog {
          margin-top: 10%;
        }
      ")),
        tags$script(HTML('
        var dots = $("#loading-dots span");
        setInterval(function() {
          dots.each(function() {
            if ($(this).text().length >= 3) {
              $(this).text("●");
            } else {
              $(this).text($(this).text() + "●");
            }
          });
        }, 500);
      '))
      )
    )
    
    # Set job_running to TRUE
    rv$job_running <- TRUE
    
    # Add your model training and prediction code here
    isolate({
      source("Source.R")
      rv$data <- read.csv("probabilityinteractions.csv")
      rv$go_annotations <- go_annotations_selected  # <- Load GO annotations here
    })
    
    # Set job_running back to FALSE
    rv$job_running <- FALSE
    
    # Close the modal dialog
    removeModal()
    
    # Switch to the Results tab
    updateTabsetPanel(session, "navbarPage", selected = "Results")
  })
  
  
  output$demo_data <- renderTable({
    req(rv$demo)
    rv$demo
  })
  
  output$demo_network <- renderVisNetwork({
    req(rv$demo)
    nodes <- data.frame(id = unique(c(rv$demo$pathogen_protein, rv$demo$host_protein)), 
                        label = unique(c(rv$demo$pathogen_protein, rv$demo$host_protein)), 
                        color = c(rep("red", length(unique(rv$demo$pathogen_protein))), rep("blue", length(unique(rv$demo$host_protein)))), 
                        title = unique(c(rv$demo$pathogen_protein, rv$demo$host_protein)))
    edges <- data.frame(from = rv$demo$pathogen_protein, to = rv$demo$host_protein, 
                        color = "black", 
                        title = paste0("From ", rv$demo$pathogen_protein, " to ", rv$demo$host_protein))
    visNetwork(nodes, edges, height = "800px") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visInteraction(navigationButtons = TRUE, dragNodes = FALSE) %>%
      visEdges(arrows = 'to') %>%
      visLegend()
  })
  
  output$predicted_interactions_network <- renderTable({
    req(rv$data)
    rv$data
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("probabilityinteractions.csv")
    },
    content = function(file) {
      write.csv(rv$data, file, row.names = FALSE)
    }
  )
  
  output$network <- renderVisNetwork({
    req(rv$data)
    nodes <- data.frame(id = unique(c(rv$data$pathogen_protein, rv$data$host_protein)), 
                        label = unique(c(rv$data$pathogen_protein, rv$data$host_protein)), 
                        color = c(rep("red", length(unique(rv$data$pathogen_protein))), rep("blue", length(unique(rv$data$host_protein)))), 
                        title = unique(c(rv$data$pathogen_protein, rv$data$host_protein)))
    edges <- data.frame(from = rv$data$pathogen_protein, to = rv$data$host_protein, 
                        color = "black", 
                        title = paste0("From ", rv$data$pathogen_protein, " to ", rv$data$host_protein))
    visNetwork(nodes, edges, height = "800px") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visInteraction(navigationButtons = TRUE, dragNodes = FALSE) %>%
      visEdges(arrows = 'to') %>%
      visLegend()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("results-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$data, file)
    }
  )
  
  output$go_table <- renderTable({
    req(rv$go_annotations)
    rv$go_annotations
  })
  
  
  generate_go_plot <- function(go_data, plot_type) {
    # Generate a network graph using igraph
    net <- graph_from_data_frame(go_data, directed = FALSE)
    V(net)$type <- bipartite_mapping(net)$type # Mark nodes as 'protein' or 'GO term'
    V(net)$type <- ifelse(V(net)$type, "GO term", "Protein")  # Change FALSE and TRUE to "Protein" and "GO term"
    V(net)$color <- ifelse(V(net)$type == "Protein", "lightblue", "lightcoral") # Color nodes by type
    p <- ggraph(net, layout = 'nicely') +  # changed layout to 'nicely'
      geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name))) +
      geom_node_point(size = 5, aes(color = type)) +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3, check_overlap = TRUE, repel = TRUE) + # added repel = TRUE
      theme_void() +
      theme(plot.margin = margin(1,1,1,1, "cm"))  # added plot margin
    print(p)
    return(p)
  }
  
  
  
  output$go_plot <- renderPlot({
    req(rv$go_annotations)
    plot_type <- input$plotType
    generate_go_plot(rv$go_annotations, plot_type)
  }, width = 1200, height = 900)
  
  
  # Download handler for the plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("GO-plot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      print(generate_go_plot(rv$go_annotations, input$plotType))
      dev.off()
    },
    contentType = "image/png"
  )
  
  output$downloadGoTable <- downloadHandler(
    filename = function() {
      paste("GO-table-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$go_annotations, file)
    }
  )
}

shinyApp(ui = ui, server = server)
