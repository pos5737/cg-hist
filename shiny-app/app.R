#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggridges)

# load data
df <- read_rds("data/parties.rds") %>%
    glimpse()

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
    # Application title
    titlePanel("Histogram Challenge"),
    # Show a plot of the generated distribution

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(10,
               plotOutput("distPlot")
        )),
    column(10,
        wellPanel(
            h3("Change the Electoral System"),
            p("The parameters below change the electoral systems shown in the 
             histogram above. Clark and Golder about the same number of parties 
              in SMD systems, regardless of social heterogeneity. But in PR systems,
              Clark and Golder expect more parties as social heterogeneity increases."),
            radioButtons("electoral_system", "Electoral System:",
                         c("Single-Member District" = "Single-Member District",
                           "Small-Magnitude PR" = "Small-Magnitude PR",
                           "Large-Magnitude PR" = "Large-Magnitude PR")),
            hr(),
            h3("Change the Plot Parameters"),
            p("The parameters below adjust the plot parameters, such as binwidth and color."),
            sliderInput("enep_bins",
                        "Number of (equal-width) bins for ENEP:",
                        min = 2,
                        max = 100,
                        value = 30),
            sliderInput("eneg_bins",
                        "Number of (equal-data) bins for ENEG:",
                        min = 2,
                        max = 10,
                        value = 3),
            radioButtons("type", "Color Palette Type",
                         c("Sequential" = "seq",
                           "Diverging" = "div",
                           "Qualitative" = "qual")),      
            numericInput("palette", "Palette Number", value = 2, width = "120px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        min <- min(df$eneg)
        max <- max(df$eneg)
        df2 <- df %>%
            mutate(eneg_cut = cut_number(eneg, n = input$eneg_bins)) %>%
            filter(electoral_system == input$electoral_system)
        df3 <- df2 %>%
            select(-electoral_system)
        ggplot(df2, aes(x = enep, y = eneg_cut, height = ..density..)) + 
            # geom_density_ridges2(data = df3, 
            #                      stat = "binline",
            #                      color = NA,
            #                      fill = "black", 
            #                      alpha = 0.1) + 
            geom_density_ridges2(aes(fill = eneg_cut),
                                 stat = "binline", alpha = 0.5, bins = input$enep_bins) + 
            scale_fill_brewer(type = input$type, palette = input$palette) + 
            scale_x_continuous(limits = c(min, max)) + 
            theme_minimal() +
            labs(x = "Effective Number of Political Parties",
                 y = "Effective Number of Ethnic Groups") + 
            #facet_grid(cols = vars(electoral_system)) + 
            guides(fill = FALSE) + 
            theme(text = element_text(size = 18))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

