
# Define server function

shinyServer <- function(input, output, session) {
  
  #------------------------Move from landing page to additional tabs--------------  
  observe({
    if(input$button_one == 0){
      return()
    }
    isolate({
      updateTabsetPanel(session, "page_tab", select = navtab1)
    })
  })
  
  observe({
    if(input$button_two == 0){
      return()
    }
    isolate({
      updateTabsetPanel(session, "page_tab", select = navtab2)
    })
  })
  
  observe({
    if(input$button_three == 0){
      return()
    }
    isolate({
      updateTabsetPanel(session, "page_tab", select = navtab3)
    })
  })
  
  #------------------------ANALYSIS TAB------------------------------------
  
  # XX
  
  #------------------------STATE SPACE TAB---------------------------------
  
  # XX
  
}
