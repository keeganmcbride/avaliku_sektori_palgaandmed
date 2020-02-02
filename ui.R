ui <- fluidPage(theme = shinytheme("spacelab"),
                titlePanel(title = "Avaliku sektori palgaandmed"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "dataset",
                                label = "Vali andmehulk",
                                choices = datasetList
                  ),
                  downloadButton("downloadData","Laadi alla: CSV")
                  ),
                  mainPanel(
                    DT::DTOutput("dataview"),
                    tags$br(),
                    tags$br(),
                    tags$h4("Andmed: https://opendata.riik.ee/andmehulgad/avaliku-sektori-palgad/")
                    # d3tree2Output("treemap") %>% withSpinner(color = "#3F1A84", type = 4)
                    
                  )
                ))