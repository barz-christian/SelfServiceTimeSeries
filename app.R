# This is an app for self service time series analysis
# author : christian barz
# date : 16.06.2019 (last time this line was updated)


library(prophet) # for time series analysis
library(shiny) # for building the web application
library(dplyr)
library(ggplot2)
library(parsedate)

# Define UI ----
ui <- fluidPage(
  navbarPage("times series self service", # title
             
             tabPanel(title = "load data", # panel to load data
                      sidebarLayout(
                        sidebarPanel(
                          # load data
                          fileInput(inputId = "file", # variable how to access the file
                                    label = "Choose your CSV File", 
                                    multiple = FALSE, # if multiple files are allowed
                                    accept =".csv"      
                          ),
                          tags$hr(),
                          # checkbox if file has header
                          checkboxInput("header", "File contains head line", TRUE),
                          tags$hr(),
                          
                          # select number of showns rows
                          checkboxInput("disp", "Display head of data only", TRUE),
                          
                          # set width of the side bar
                          width = 2
                        ),
                        mainPanel(
                          tableOutput("data") # show up loaded data
                        )
                      )
                      ),
             tabPanel("data preparation",
                      sidebarPanel(
                        # select  time variable = ds
                        textInput(inputId = "ds",
                                  label =  "Name of time variable" 
                        ),
                        # select Predictor = y
                        textInput(inputId = "y",
                                  label =  "Name of target variable" 
                        ),
                        # apply selection, i.e. restrict data to ds and y
                        actionButton(inputId = "restrict",label = "apply Input"),
                        # set width of the side bar
                        width = 2
                      ),
                      mainPanel(
                        h3("Modified data"), 
                        h5("(only the first five lines are shown)"),
                        tableOutput("datamodified") # show head of restricted data
                      )
                      ),
             tabPanel("create model",
                      sidebarPanel(
                        # input the number of time intervals (see selectInput below) we want to forecast
                        numericInput(inputId = "periods", 
                                     label = "periods", 
                                     value = 12),
                        # select the time intervals for the forcast
                        selectInput(inputId = "frequenz", 
                                  label = "frequency", 
                                  choices = c("day" = "day", "week" = "week", "month" = "month", "quarter" = "quarter", "year" = "year"
                                              #, 1(1 sec), 60(1 minute) or 3600(1 hour)
                                              ),
                                  selected = "month"),
                        # checkbox to force certain kinds of seasonalities
                        checkboxGroupInput(inputId = "forceSeasonality", 
                                      label = "Force Seasonality", 
                                      choices = list("yearly" = "yearly", "weekly" = "weekly", "daily" = "daily" )),
                        # action button to create a model an produce the analysis
                        actionButton(inputId = "createModel",
                                     label = "Create Model"),
                        # set width of the sidebar panel
                        width = 2
                        ),
                      mainPanel(
                        h3("Trend and seasonality: "), # headline to distinguish
                        plotOutput("analysis"),
                        h3("fitting and forecast: "), # headline to distinguish
                        plotOutput("forecast")
                      )
                      ),
             tabPanel("save results ...")
  )
  
  
  
)

# Define server logic ----
server <- function(input, output, session) {
  
  # upload the data
  rawdata <- eventReactive(eventExpr = input$file,{
    read.csv(input$file$datapath, header = input$header) 
  })
  # show/inspect the upload
  output$data <- renderTable(
    if(input$disp)
    {rawdata() %>% head()}
    else{rawdata()}
    )
  
  #restrict data to the time variable input$ds and the target
  datamodified <- eventReactive(input$restrict,
                               {
                                 rawdata() %>%
                                   select(one_of(c(input$ds, input$y))) 
                               }
                               )
  # show/inspect head of the restricted data
  output$datamodified <- renderTable(datamodified() %>% head())
  
  # create the model and the plot of the components
  observeEvent(input$createModel , {

    df <- datamodified() 
    names(df) <- c("ds", "y") # change variable names
    df$ds <- parsedate::parse_date(df$ds) # parse date

    week <- ifelse("weekly" %in% input$forceSeasonality, TRUE, "auto")
    year <- ifelse("yearly" %in% input$forceSeasonality, TRUE, "auto")
    day <- ifelse("daily" %in% input$forceSeasonality, TRUE, "auto")
    
     m <- prophet(df,
                  yearly.seasonality = year,
                  weekly.seasonality = week,
                  daily.seasonality = day)
     
     future <- make_future_dataframe(m,
                                     freq = input$frequenz, 
                                     periods = input$periods)
     forecast <- predict(m, future)
     output$forecast <- renderPlot(plot(plot(m,forecast)))
     output$analysis <- renderPlot(prophet_plot_components(m, forecast))
  })
  
  # stop R-session, when application is closed
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
  