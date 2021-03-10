library(shiny)
library(tidyverse)
library(magrittr)
library(shinythemes)
library(shinyjs)

# taken from Our World in Data GitHub Repository
vaccines = read_csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")) %>% drop_na() 
#delete ISO code
vaccines=vaccines[c(-2,-7,-8,-10)]
vaccines=na.omit(vaccines)
maxPop = vaccines$total_vaccinations %>% max(na.rm=TRUE)

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


plotlyPanel = tabPanel("Dynamic plot",
                        plotly::plotlyOutput("plotly")
)


# Define UI for application 
ui = navbarPage("Arturo's shiny App",
                 dataPanel,
                 plotlyPanel,
                 header = myHeader,
                 theme = shinytheme("superhero"),
                 id = "navBar"
)

# Define server logic 
server = function(input, output, session) { 
    
    vaccines_date = reactive({vaccines %>%
            filter(date %in% input$selected_date, location %in% input$selected_location)})
    
    output$data = renderTable(vaccines_date());
    
    output$plot = renderPlot(
        ggplot(vaccines_date(), aes(x=location, y=total_vaccinations, fill=date))
        + geom_bar(stat="identity", position=position_dodge())
    )
    
    output$plotly = plotly::renderPlotly(
        ggplot(vaccines_date(), aes(x=location, y=total_vaccinations, fill=date))
        + geom_bar(stat="identity", position=position_dodge())
    )
    
    #refresh page when moving through sections
    observe({
            shinyjs::show("advanced")
    })
    
    output$report = downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.pdf",
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport = file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params = list(
                selected_date = isolate(input$selected_date),
                selected_location = isolate(input$selected_location)
            )
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)