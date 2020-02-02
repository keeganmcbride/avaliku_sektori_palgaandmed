
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  datasetInput <- reactive({
    switch(input$dataset,
           "kov_palk_2018" = kov_palk_2018,
           "kov_palk_2019" = kov_palk_2019,
           "riik_palk_2018" = riik_palk_2018,
           "riik_palk_2019" = riik_palk_2019,
           "kov_kogu_palk_2018" = kov_kogu_palk_2018,
           "riik_kogu_palk_2018" = riik_kogu_palk_2018 ) 
  })


output$dataview <- renderDataTable({
  DT::datatable(datasetInput(), filter = 'top', options= list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Estonian.json', autoWidth=TRUE)))
})


treemapData <- reactive({

    
      
    if(input$dataset %in% c("kov_palk_2018", "kov_palk_2019","riik_palk_2018","riik_palk_2019")){
      data <- datasetInput()[input[["dataview_rows_all"]], ] %>% group_by(Asutus, Struktuuriüksus) %>% summarise(Total = sum(Põhipalk, na.rm = T))
    }
    else{
      data <- datasetInput()[input[["dataview_rows_all"]], ] %>% group_by(Asutus, Struktuuriüksus) %>% summarise(Total = sum(Kokku, na.rm = T))
      
  } 
 
  
  treemap <- treemap(data,
                     index=c("Asutus", "Struktuuriüksus"),
                     vSize="Total",
                     vColor = "Total",
                     palette = "Set3",
                     type="index")
  treemap
  
})


output$treemap <- renderD3tree2({
  if(input$dataset %in% c("kov_palk_2018", "kov_palk_2019","riik_palk_2018","riik_palk_2019")){
    
  d3tree2(treemapData(), rootname = "Total Monthly Salary")
  }
    else {
      d3tree2(treemapData(), rootname = "Total Salary Yearly")
      
    
    }
})

output$test <- renderTable({
  treemapData()
})




output$downloadData <- downloadHandler(
  filename = function(){
    paste("palk_download", ".csv", sep = "")
  },
  content = function(file){
      write.csv2(datasetInput()[input[["dataview_rows_all"]], ], file, dec = ",", row.names = FALSE)
    }
)
  
})
