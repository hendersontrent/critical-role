# Define UI for web application

shinyUI(navbarPage(theme = "corp-styles.css", 
                   title = div(img(src = "orbisant_logo.png", height = '30px', hspace ='30'),
                               ""),
                   position = c("static-top"), windowTitle = "Critical Role: A Statistical Exploration",
                   id = "page_tab",
                   
                   
                   #---------------------------------------------Landing page for overall app----------------------                 
                   tabPanel(navtab0,
                            tags$head(
                              tags$link(rel = "stylesheet", type = "text/css", href = "corp-styles.css")
                            ),
                            
                            setBackgroundImage(src = "mn_light.png"),
                            
                            fluidRow(style = "padding-top: 0", 
                                     HTML("
                                          <center>
                                          <img src= 'cr_logo.png', height = '175px'>
                                          </center>
                                          "  
                                     )
                            ),
                            
                            p(),
                            fluidRow(
                              column(2),
                              column(8, style = "min-height: 150px;",
                                     HTML(splash_page_content)
                              ),
                              column(2)
                            ),
                            fluidRow(
                              column(1),
                              column(1),
                              column(2, style = "border-style: dotted; border-color: #A0E7E5; min-height: 400px;", 
                                     fluidRow(HTML(welcome_box_1), style = "min-height: 300px; margin-left: 20px; margin-right: 20px;"),
                                     fluidRow(align = "center", actionButton("button_one", "VIEW ANALYSIS", 
                                                                             style = "color: #A0E7E5; background-color: #ffffff; border-color: #A0E7E5"))
                              ),
                              column(1),
                              
                              column(2, style = "border-style: dotted; border-color: #FD62AD; min-height: 400px;",
                                     fluidRow(HTML(welcome_box_2), style = "min-height: 300px; margin-left: 20px; margin-right: 20px;"),
                                     fluidRow(align = "center", actionButton("button_two", "VIEW MODELLING", 
                                                                             style = "color: #FD62AD; background-color: #ffffff; border-color: #FD62AD"))
                              ),
                              column(1),
                              
                              column(2, style = "border-style: dotted; border-color: #05445E; min-height: 400px;",
                                     fluidRow(HTML(welcome_box_3), style = "min-height: 300px; margin-left: 20px; margin-right: 20px;"),
                                     fluidRow(align = "center", actionButton("button_three", "VIEW ABOUT SECTION", 
                                                                             style = "color: #05445E; background-color: #ffffff; border-color: #05445E"))
                              ),
                              column(1),
                              column(1)
                              
                            )
                            
                   ),
                   
                   #----------------------Affordability index header--------------------------
                   tabPanel(navtab1,
                            fluidRow(h1("Character Analysis")
                            )
                   ),
                   
                   #----------------------Affordability index header--------------------------
                   tabPanel(navtab2,
                            fluidRow(h1("State Space Modelling")
                            )
                   ),
                   
                   #----------------------Help page header------------------------------------
                   tabPanel(navtab3,
                            fluidRow(h1("About")
                            ),
                            includeMarkdown("./md/about.Rmd")
                   ),
                   
                   
                   fluidRow(style = "height: 50px;"),
                   fluidRow(style = "height: 50px; color: white; background-color: #05445E; text-align: center;line-height: 50px;", HTML(footer)),
                   fluidRow(style = "height: 50px;")
                   
)
)
