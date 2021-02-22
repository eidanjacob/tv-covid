library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)
cases <- read_excel("tvcsd-cases.xlsx", 
                    col_types = c("date", "text", "text", 
                                  "date"), na = "NA")

ui <- fluidPage(
    
    titlePanel("TVCSD Covid Cases"),
    
    sidebarLayout(
        sidebarPanel(
            checkboxInput("altGraph", "Facet By Location", value = TRUE)
        ),
        
        mainPanel(
            plotOutput("casesPlot")
        )
    )
)

server <- function(input, output) {
    
    output$casesPlot <- renderPlot({
        plot_out = NULL
        if(input$altGraph){
            plot_out = cases %>% 
                filter(!is.na(Last_In_Building)) %>%
                ggplot(aes(x = Last_In_Building, fill = Role)) + geom_histogram() +
                facet_wrap(~ Location) + theme_bw()
        } else {
            plot_out = cases %>% 
                filter(!is.na(Last_In_Building)) %>%
                ggplot(aes(x = Last_In_Building, fill = Location)) + geom_histogram() + 
                scale_fill_brewer(type = "qual", palette = "Set3") + theme_bw()
        }
        plot_out + xlab("Last Date in Building") + ylab("Count of Cases") + 
            ggtitle("TVCSD Covid-19 Cases")
        
    },
    width = 800,
    height = 800)
}

# Run the application 
shinyApp(ui = ui, server = server)
