library(shiny)
library(tidyverse)
library(magrittr)
library(shinythemes)
library(shinyjs)

# taken from Our World in Data GitHub Repository
vaccines = read_csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")) %>% drop_na() 
#delete ISO code
vaccines=vaccines[c(-2,-7,-8,-10)]

vaccines %<>% mutate_at(c("date", "location"), as.factor)
vaccines_dates = levels(vaccines$date) %>% str_sort()
vaccines_countries = levels(vaccines$location)


myHeader = div(id="advanced",
               useShinyjs(),
               selectInput(
                   inputId = "selected_date",
                   label = "Select the date(s)",
                   multiple = TRUE,
                   choices = vaccines_dates,
                   selected = c(vaccines_dates[1])
               ),
               selectInput(
                   inputId = "selected_location",
                   label = "Select the location(s)",
                   multiple = TRUE,
                   choices = vaccines_countries,
                   selected = c(vaccines_countries[1])
               ),
               downloadButton("report", "Generate report")
)

dataPanel = tabPanel("Data",
                     tableOutput("data")
)


# Define UI for application 
ui = navbarPage("Arturo's shiny App",
                dataPanel,
                header = myHeader,
                theme = shinytheme("united"),
                id = "navBar"
)

# Define server logic 
server = function(input, output, session) { 
    
    vaccines_date = reactive({vaccines %>%
            filter(date %in% input$selected_date, location %in% input$selected_location)})
    
    output$data = renderTable(vaccines_date());
    
}

# Run the application 
shinyApp(ui = ui, server = server)