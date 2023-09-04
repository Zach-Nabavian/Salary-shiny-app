

library(shiny)
library(tidyverse)

# Read csv file

data <- read.csv("salary data.csv")
data <- as_tibble(data)
str(data)

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Salary Data Since 2003"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("Salary", 
                        "Choose Sector:", choices = c("overall_salary", "service_salary", "construction_salary",
                                                      "laborer_salary", "manager_salary", "tech_salary")),
            checkboxInput("Inflation", "Adjust for Inflation")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot")
        )
    )
)

# Define server logic
server <- function(input, output) {

    output$plot <- renderPlot({
        chosen_salary <- data[[input$Salary]]
        chosen_inflation_salary <- paste0(input$Salary, "_adj")
        adjusted_salary <- data[[chosen_inflation_salary]]
        if(input$Inflation) {
            ggplot(data, aes(x = Year)) +
                geom_line(aes(y = chosen_salary, color = "Base")) +
                geom_line(aes(y = adjusted_salary, color = "Adjusted")) +
                labs(y = input$Salary) +
                theme(text = element_text(size = 20)) + 
                scale_color_manual(name = "Salary", values = c(Base = "blue", Adjusted = "red")) 
        }
        else {
            ggplot(data, aes(x = Year)) +
                geom_line(aes(y = chosen_salary, color = "Base")) +
                labs(y = input$Salary) +
                theme(text = element_text(size = 20)) + 
                scale_color_manual(name = "Salary", values = c(Base = "blue"))
        }
    }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
