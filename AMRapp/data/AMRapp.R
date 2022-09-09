#Run AMRapp - Interactive AMR visualisation tool

#' @title Run AMRapp
#' @description Run the AMR trend visualisation tool
#' 

### pulling in the bespoke functions for AMRapp
source("R/packages.r", echo = TRUE)
source("R/data_import.r", echo = TRUE)
source("R/pcentR_ci_plot.r", echo = TRUE)
source("R/pcentR_plot.r", echo = TRUE)
source("R/pcentRandT_plot.r", echo = TRUE)
source("R/pcentRandTci_plot.r", echo = TRUE)
source("R/pcentT_ci_plot.r", echo = TRUE)
source("R/pcentT_plot.r", echo = TRUE)
source("R/rollpcentRandT_plot.r", echo = TRUE)
source("R/stack_plot_function.r", echo = TRUE)
source("R/optchoice.r", echo = TRUE)

## bringing in the AMRapp supplied testfile to ensure functionality
testfile <- data_import("data/Amrapp_template_testdata.csv")
abxlist <- as.list(x = unique(testfile$antimicrobial))
genus_list <- as.list(x = unique(testfile$genus))
#d<- optchoice(genus_list,1)





shinyApp(
  ui = navbarPage("Dashboard", theme = shinytheme("flatly"),
                  tabPanel("Data import",
                           sidebarLayout(
                             sidebarPanel(

                                 fileInput(
                                   inputId = "filedata", 
                                   label = "Upload File",
                                   accept = c("text/csv", "text/comma-separated-values,text/plain",
                                                                ".csv"),
                                   buttonLabel = "Browse",
                                   placeholder = "No file selected")
# ADD IN A CLEAR DATA OPTION TO ENABLE A NEW DATA SET UPLOAD WITHOUT CLOSING BROWSER                 
                               ), # sidepanel
 
### ~~~~~~~~~~ main panel for the first tab ~~~~~~~~~~~~~~~~~~~~ ####                
                            mainPanel(
                  #              # Import data progress summary
                                tags$b("Data overview"),
                                DTOutput(outputId = "datatable",  width = "100%", height = "auto")
                              ) # main panel tab
                          )  ##sidebar layout

                   ), # tab panel 1

### ~~~~~~~~~~~ TAB 2 - Trend plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
                  tabPanel("Trend plot",
                    sidebarLayout(
                      sidebarPanel(
                        tags$h3("Trend graph"),
                               
                        radioButtons(
                          inputId = "tndchce", 
                          label = "Plot type: ",
                          choices = list("bar", "line")
                          ),
                               
                        tags$h4("Antibiotic choice :"),
                        
                        selectInput(
                          inputId = "filter2", 
                          label = "Antibiotic: ",
                          choices = c(abxlist) 
                          ),
                           
                               
                        tags$h4("Pathogen choice :"),
                        
                        selectInput(
                          inputId = "filter1", 
                          label = "Organism: ",
                          choices = c(genus_list) 
                          ),
                        
                               
                        sliderInput(
                          inputId = "slider1", 
                          label = h3("Date range select"), 
                          min = as.Date("2018-01-01","%Y-%m-%d"),    #set as earliest month/date in the data
                          max = as.Date("2022-12-01","%Y-%m-%d"),  #set as latest month/date in the data set 
                          value = c(as.Date("2018-01-01"), as.Date("2022-12-01")),
                                    timeFormat="%Y-%m-%d"   #pre-selected range
                          ),
                             
                        actionButton(
                          inputId = "plotButton", 
                          label = "Plot graph" 
                          )
                ),
                             
### ~~~~~~~~~~~ main panel for the second tab ~~~~~~~~~~~~~~~~~ ###                             
                mainPanel(
                  h2("Trend graph"),
                   plotlyOutput('stacked_bar'),
                   DTOutput("stackdatatable"),
                   dygraphOutput("resline_plot"),
                   dygraphOutput("testline_plot")
                  
                      ) # main panel
                    ) # sidebar panel
                  ) # tab panel 2

  ), # UI



####~~~~~~~~~~~~~~ Server ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
  server = function(input, output) { 
    
    options(shiny.maxRequestSize=50*1024^2)
 
# the dummy data supplied with the app, this will be replaced one a user uploads their own
    output$data <- renderTable({
      fread("data/testfile.csv")         
    })
    
    
### bring in the data as selected by user in tab 1 menu
    data <- reactive({
      req(input$filedata, file.exists(input$filedata$datapath))
      df <- fread(input$filedata$datapath, select = c("genus"="character", "species"="character", 
                                     "Date"="character", "antimicrobial"="character", 
                                     "result"="character", "age"="integer", 
                                     "sex"="character", "pt_id"="character"))   
      return(df)
      

      
    })
    
    
    output$abxlist <- renderText({
      req(data())
      a<- as.list(x = unique(data()$antimicrobial))
      return(a)
    }) 
    

    output$genus_list <- renderText({
      req(data())
      b <- as.list(x = unique(data()$genus))
      return(b)      
    })


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create 
  #   output$abxlist <- observeEvent(input$filedata, {
  #     req(data()) 
  #     renderMenu({
  #     a<-  abx_list(data())
  #     
  #     dropdownMenu(type = "list", .list = a)
  #       
  #     })
  # })
  #   
 
     
## Getting the data table for displaying in the main panel of tab 1    
    output$datatable <- renderDT({
      req(data())
      data()
    })
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   


    #output$abx_choice <- renderPrint({ input$filter2 })
    
### function to identify the value from the list being selected from in the SelectInput format
# requires a list and an input which equates to a value
# e.g. the select input field allows choice from the drop down and stores the numerical 
# value of the position
    optchoice<- function(list, input) {
      l <- length(list)
      if (input <= l) { 
        filter <- list[[input]]
      }
      else {
        # If 
        filter <- list[[1]]              
      }
    }
    #d<- optchoice(genus_list,1)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
#   output$barplot <- renderPlotly({
#    if (input$trndchoice == "line") { data <- citable}
#    if (input$tndchoice == "bar" ) { data <- testfile}
#      plot_ly(x = imported[, input$xcol], y = df[, input$ycol], z = df[, input$zcol],
#              color = df$clusters) %>%
#        add_markers()
#   })
 
    
    
    
    
      observeEvent(input$plotButton,{
          # if(length(data())==0) {
          #   output$error1 <- renderText({
          #     "No data uploaded yet"
          #   })
          # }
          # 
         output$stacked_bar <- renderPlotly(
          req(data()),
 
          if(input$tndchoice == "1") {
            f1    <- opt(genus_list, input$filter1) 
            f2    <- opt(abx_list, input$filter2)     
            
            sp <- stack_plot_function(data(),f1,f2)
            return(sp)
          }  
        )
        
      ## Getting the data table for displaying in the main panel of tab 1    
      output$stackdatatable <- renderDT({
        req(data())
        f1    <- opt(genus_list, input$filter1) 
        f2    <- opt(abx_list, input$filter2)            
        data() %>% 
          filter(genus == f1 & antimicrobial == f2) %>%
          ## grouping into year and month and susceptibility result
          group_by(result=as.factor(result), 
                   year_month=make_date(year=year(as.Date(Date, format = "%d/%m/%Y")),
                                        month=month(as.Date(Date, format = "%d/%m/%Y")))) 
      })      
      
      
          #f1 <- optchoice(genus_list, input$filter1),

          #f2 <- optchoice(abxlist, input$filter2),



                    
   #       data() %>%
   #        ## filtering the genus and antimicrobial selected
   #        filter(genus == f1 & antimicrobial == f2) %>%
   #        ## grouping into year and month and susceptibility result
   #        group_by(result=as.factor(result), 
   #                 year_month=make_date(year=year(as.Date(Date, format = "%d/%m/%Y")),
   #                                      month=month(as.Date(Date, format = "%d/%m/%Y")))) %>% 
   #        ## calculating a total of each category by month    
   # #       summarise( n=n_distinct(pt_id), .groups = "keep") %>% 
   #        plot_ly(x=~year_month,
   #                y=~n,
   #              color = ~result, 
   #                text = ~paste("No. of cases=",n, "<br>", "Month=", year_month, "<br>", "Result=", result),
   #                hoverinfo = "text"
   #        ) %>%
   #        add_bars() %>%
   #        layout(title=paste("No. of", f2, "susceptiiblity test results for", f1, "spp."),
   #               xaxis = list(title="Year and Month"),
   #               yaxis = list(title = "No. testss"),
   #               barmode = "stack")
       
        
        

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
        
    }) ## observe event plotButton

        
    } # end of server
)
