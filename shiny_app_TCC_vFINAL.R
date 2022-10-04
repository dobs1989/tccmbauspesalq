library(shiny)
library(tidyr)
library(dplyr)
library(DT)
library(openxlsx)

getlist <- function(df, colnums){
  data <- df
  if(length(colnums)==1){
    column_names <- colnames(data)[colnums]
  } else{
    selected_data <- data[,colnums]
    column_names <- colnames(selected_data)
  }
  return(column_names)
}

callback <- c(
  "table.on('dblclick.dt', 'thead th', function(e) {",
  "  var $th = $(this);",
  "  var index = $th.index();",
  "  var colname = $th.text(), newcolname = colname;",
  "  var $input = $('<input type=\"text\">')",
  "  $input.val(colname);",
  "  $th.empty().append($input);",
  "  $input.on('change', function(){",
  "    newcolname = $input.val();",
  "    if(newcolname != colname){",
  "      $(table.column(index).header()).text(newcolname);",
  "      Shiny.setInputValue('change_colname', [colname, newcolname]);",
  "    }",
  "    $input.remove();",
  "  }).on('blur', function(){",
  "    $(table.column(index).header()).text(newcolname);",
  "    $input.remove();",
  "  });",
  "});"
)

ui <- fluidPage(
  verbatimTextOutput("colnames"),
  # App title ----
  titlePanel("Transform & Loading"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ";"),
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      selectInput("col", "Column to search:", NULL),
      textInput("old", "Replace:"),
      textInput("new", "By:"),
      actionButton("replace", "Replace!"),
      DTOutput("table"),
      textOutput('preview'),
      tableOutput('table2')

      # downloadButton("download", "Download"),
    ),
    mainPanel(
      DTOutput("table1")
    )
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize=100*1024^2)
  options(encoding = 'latin1')
  my_data <- reactiveVal(NULL)
  
  observeEvent(input$file1, {
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    my_data(read.csv2(file$datapath, 
                     header = input$header, 
                     sep = input$sep,
                     quote = input$quote,
                     na.strings=c("","NA"))%>%fill(names(.)[1:2],.direction = "down"))
    updateSelectInput(session, "col", choices = names(my_data()))
  })
  
  observeEvent(input$replace, {
    req(input$col)
    dat <- req(my_data())
    traf <- if (is.character(dat[[input$col]])) as.character else identity
    my_data(dat %>%
              mutate(!!rlang::sym(input$col) := 
                       gsub(input$old,
                            input$new,
                               dat[[input$col]]) %>% 
                       traf()))
  })
  # 
  
  output$table1 <- renderDT(req(my_data()),callback = JS(callback),
                            server = FALSE,
      # filter="top",
    extensions = c("Buttons"),
    options = list(
      dom = 'Bfrtip',
      
        buttons = list(
          list(extend = "excel", text = "Download Current Page", filename = "page",
               exportOptions = list(
                 modifier = list(page = "current")
               )
          ),
          list(extend = "excel", text = "Download Full Results", filename = "data",
               exportOptions = list(
                 modifier = list(page = "all"))))
      
    
  ))
  
  
  # output$table1 <- renderDT(server = FALSE,
  #   req(my_data()),
  #   filter="top",
  #   options = list(
  #     paging = TRUE,
  #     searching = TRUE,
  #     fixedColumns = TRUE,
  #     autoWidth = TRUE,
  #     ordering = TRUE,
  #     dom = 'Bfrtip',
  #     selection = "none",
  #     editable = TRUE, 
  #     extensions = "KeyTable",
  #     options = list(
  #       keys = TRUE
  #     ),
  #     buttons = list(
  #       list(extend = "csv", text = "Download Current Page", filename = "page",
  #            exportOptions = list(
  #              modifier = list(page = "current")
  #            )
  #       ),
  #       list(extend = "csv", text = "Download Full Results", filename = "data",
  #            exportOptions = list(
  #              modifier = list(page = "all")
  #            )
  #       )
  #     )
  #   ),
  #   
  #   class = "display"
  #   
  # )

      

  
  # output$download <- downloadHandler(
  #   filename = function() {
  #     paste0(input$dataset, ".xlsx")
  #   },
  #   content = function(file) {
  #     write.csv(data(), file)
  #   }
  # )
  
}

shinyApp(ui = ui, server = server)
