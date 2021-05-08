library(readxl)
library(readxl)
library(shiny)
library(lubridate)
library(dplyr)
library(DT)
library(data.table)
library(plotly)
library(shinydashboard)
library(shinyjs)
library(rintrojs)
library(shinyalert)
library(factoextra)
library(ggplot2)
if (interactive()) {
  ui <- fluidPage(mainPanel(tabItem(tabName = "superstore",
                                    fluidRow(
                                      column(3, fileInput(
                                        "file1", "Choose excel File", accept = c(".xls")
                                      )),
                                      column(
                                        9,
                                        tabsetPanel(
                                          id = "data_id",
                                          tabPanel(
                                            title = "full_data",
                                            Value = "tab1",
                                            box(
                                              id = "data_hundred",
                                              title = "100 records",
                                              width = 12,
                                              target = 'row',
                                              align = 'center',
                                              solidHeader = T,
                                              div(style = 'overflow-x: scroll',
                                                  DT::dataTableOutput("contents")),
                                              actionButton(inputId = "gotot1", label = "Graph Analysis")
                                            )
                                          ),
                                          tabPanel(
                                            title = "plot_data",
                                            value = "tab2",
                                            box(
                                              hr(),
                                              fluidRow(column(
                                                3,
                                                selectInput(
                                                  "Category_Type",
                                                  label = "Category",
                                                  choices = c("Furniture",
                                                              "Office Supplies",
                                                              "Technology")
                                                )
                                              )),
                                              plotlyOutput("plot1"),
                                              width = "100%",
                                              height = "100%"
                                            ),
                                            downloadButton("downloadData", "Download"),
                                            actionButton(inputId = "gotot2", label = "Analyse with K means clustering")
                                          ),
                                          tabPanel(
                                            title = "K means Clustering",
                                            value = "spc",
                                            box(
                                              hr(),
                                              fluidRow(
                                                column(3, selectInput(
                                                  "Basis",
                                                  label = "Select Segment",
                                                  choices = c("Quantity",
                                                              "Sales")
                                                )),
                                                column(
                                                  3,
                                                  selectInput(
                                                    "Cluster_Type",
                                                    label = "Select Level",
                                                    choices = c("Sales",
                                                                "Profit",
                                                                "Quantity",
                                                                "NS",
                                                                "NO",
                                                                "Nspc")
                                                  )
                                                ),
                                                column(
                                                  3,
                                                  numericInput(
                                                    inputId = 'clusters',
                                                    label =  'Clusters',
                                                    3,
                                                    min = 1,
                                                    max = 9
                                                  )
                                                )
                                              ),
                                              plotOutput("kmeans"),
                                              width = "100%",
                                              height = "100%"
                                            )
                                          )
                                        )
                                      )
                                    ))))
  server <- function(input, output, session) {
    rv <- reactiveValues()
    output$contents <- DT::renderDataTable({
      file <- input$file1
      ext <- tools::file_ext(file$datapath)
      req(file)
      validate(need(ext == "xls", "Please upload a xls file"))
      full_data <- data.frame(read_excel(paste(file$datapath)))
      head(full_data, 100)
    })
    observeEvent(input$gotot1, {
      updateTabsetPanel(session, "data_id", selected = "tab2")
    })
    output$plot1 <- renderPlotly({
      file <- input$file1
      ext <- tools::file_ext(file$datapath)
      req(file)
      validate(need(ext == "xls", "Please upload a xls file"))
      full_data <- read_excel(paste(file$datapath))
      temp <- full_data %>%
        select(`Order Date`, Sales, Category, Profit) %>%
        filter(Category == input$Category_Type) %>%
        mutate(Order_Date = format(as.Date(`Order Date`), "%Y-%m"))
      temp <- temp[complete.cases(temp),]
      test_check <- temp %>%
        select(Order_Date, Sales, Category, Profit) %>%
        mutate(mon_yr = format(Order_Date), "%Y-%m")
      test_check <- test_check %>%
        group_by(mon_yr) %>%
        summarise(sum_sales = sum(Sales),
                  total_profit = sum(Profit))
      if (exists("test_check")) {
        rv$data <- test_check
      }
      # dload <- reactive({test_check})
      old <- list(side = "left",
                  title = "Total Sales")
      new <- list(overlaying = "y",
                  side = "right",
                  title = "Total Profit")
      plot_ly(rv$data) %>%
        add_trace(
          x = ~ mon_yr,
          y = ~ sum_sales,
          type = "bar",
          name = "Total Sales"
        ) %>%
        add_lines(
          x = ~ mon_yr,
          y = ~ total_profit,
          yaxis = "y2",
          name = "Total Profit"
        ) %>%
        layout(
          yaxis2 = new,
          yaxis = old,
          xaxis = list(title = "Month Year")
        )
    })
    output$downloadData <- downloadHandler(
      filename = paste0(input$Category_Type," Data.csv"),
      content = function(file) {
        write.csv(rv$data, file, row.names = FALSE)
      }
    )
    observeEvent(input$gotot2, {
      updateTabsetPanel(session, "data_id", selected = "spc")
    })
    output$kmeans <- renderPlot({
      file <- input$file1
      ext <- tools::file_ext(file$datapath)
      req(file)
      validate(need(ext == "xls", "Please upload a xls file"))
      full_data <- read_excel(paste(file$datapath))
      full_data <-
        full_data %>% filter(year(`Order Date`) == 2018 |
                               year(`Order Date`) == 2019)
      full_data <- full_data[complete.cases(full_data),]
      # kfit <- kmeans(scaling,n)
      # plot(temp,col = kfit$cluster)
      selected_data <- reactive({
        if (input$Cluster_Type == "Sales" |
            input$Cluster_Type == "Profit" |
            input$Cluster_Type == "Quantity") {
          full_data[, c(input$Basis, input$Cluster_Type)]
        } else if (input$Cluster_Type == "NS" |
                   input$Cluster_Type == "Nspc") {
          full_data <- customer_level %>%
            select(`Customer ID`, Profit, Sales, Quantity) %>%
            group_by(`Customer ID`) %>%
            summarise(
              Profit = sum(Profit),
              NS = sum(Sales),
              Sales = sum(Sales),
              Nspc = sum(Quantity),
              Quantity = sum(Quantity)
            )
          full_data[, c(input$Basis, input$Cluster_Type)]
        } else if (input$Cluster_Type == "NO") {
          full_data <- Order_level %>%
            select(`Order ID`, Profit, Sales, Quantity) %>%
            group_by(`Order ID`) %>%
            summarise(
              total_profit = sum(Profit),
              NO = sum(Sales),
              Sales = sum(Sales),
              Quantity = sum(Quantity)
            )
          full_data[, c(input$Basis, input$Cluster_Type)]
        }
      })
      clusters1 <- reactive({
        kmeans(selected_data(), input$clusters)
      })
      palette(
        c(
          "#E41A1C",
          "#377EB8",
          "#4DAF4A",
          "#984EA3",
          "#FF7F00",
          "#FFFF33",
          "#A65628",
          "#F781BF",
          "#999999"
        )
      )
      par(mar = c(5.1, 4.1, 0, 1))
      plot(
        selected_data(),
        col = clusters1()$cluster,
        pch = 20,
        cex = 3
      )
      points(
        clusters1()$centers,
        pch = 4,
        cex = 4,
        lwd = 4
      )
    })
  }
  shinyApp(ui, server)
}
