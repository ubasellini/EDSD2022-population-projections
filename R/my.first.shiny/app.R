#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

my.years <- unique(dta.swe.l$Year)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("My first shiny app"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "edsd",
                        label = "Year",
                        min = min(my.years),
                        max = max(my.years),
                        value = min(my.years),
                        step = 5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=YearF)) +
            geom_bar(data = subset(dta.swe.l, Year == input$edsd),
                     stat = "identity",position = "dodge",color = "black") +
            coord_flip() +
            theme_bw() +
            ggtitle("Swedish female population") +
            scale_y_continuous(labels = comma)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
