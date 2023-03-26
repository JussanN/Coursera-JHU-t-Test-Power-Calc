#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(plotly)

power_two_sample <- function(sample_size, effect_size, test_alt){
  p=power.t.test(
    n = sample_size,
    delta = effect_size,
    sd = 1,
    sig.level = 0.05,
    type = "two.sample",
    alternative = test_alt
  )

  return(p$power)

}

vPowerOut <- vector()
vSampleSizeOut <- vector()
vEffectSizeOut <- vector()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Two Sample T-test Power Calculator"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "test_alt",
                        label = "Choose the t test alternative",
                        choices = c("Two sided" = "two.sided", "One sided" = "one.sided"),
                        selected = "two.sided"),
            radioButtons("variable", "Select an input variable:",
                         choices = c("Sample size" = "sample_size", "Effect size" = "effect_size"),
                         inline = TRUE),

            conditionalPanel(condition = "input.variable === 'sample_size'",
                             numericInput(inputId = "sample_size",
                                          label = "Sample size",
                                          value = 50)),
            conditionalPanel(condition = "input.variable === 'effect_size'",
                             numericInput(inputId = "effect_size",
                                          label = "Effect size",
                                          value = 0.5)),

            br(),
            h3("How to use this Power calculator:"),
            p("This calculator computes the power for a experiment with two T distributed groups according to the power.t.test() and based on the left side panel."),
            h4("The steps are:"),
            p("1 - Choose the t test Alternative between One side and Two Sided;"),
            p("2 - Select the input variable between Sample Size and Effect Size;"),
            p("3 - Define the value the input variable."),
            p("If the Sample Size is selected as input variable, the calculator will provide a power curve for different Effect sizes."),
            p("If the Effect Size is selected as input variable, the calculator will provide a power curve for different Sample sizes.
              For this power calculator, the Standard deviation is fixed to 1, so the Effect size is the difference between mean under the alternative hyphotesis and mean under the null hyphotesis."),
            p("The power curve can be visualized in the main panel.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput(outputId = "plot"),
           h3("How to read the Power curve's chart."),
           p("The power of a hypothesis test is the probability of making the correct decision if the alternative hypothesis is true. That is, the power of a hypothesis test is the probability of
             rejecting the null hypothesis when the alternative hypothesis is the hypothesis that is true."),
           p("The Power curve above provides the relation between the Power vs Effect Size or Sample Size."),
           p("Pass over your mouse on the curve to visualize the values"),
           p("It's recommended to select the Effect Size or Sample Size for a Power equal to or higher than 80% (0.8)."),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot <- renderPlotly({

    test_alt <- input$test_alt

    if(input$variable == 'sample_size'){
      for(effect_size in seq(0, 5, 0.02)){
        power <- power_two_sample(sample_size = input$sample_size, effect_size = effect_size, test_alt = test_alt)
        vPowerOut <- append(vPowerOut, power)
        vEffectSizeOut <- append(vEffectSizeOut, effect_size)
      }
      power_data <- data.frame(vEffectSizeOut, vPowerOut)
      plot_ly(data = power_data, x = ~vEffectSizeOut, y = ~vPowerOut, type = 'scatter', mode = 'lines') %>%
        add_lines(y = 0.8,line = list(color = "grey")) %>%
        layout(showlegend = F, title = 'Power curve', xaxis = list(title = 'Effect size'),  yaxis = list(title = 'Power', range = c(0,1)))

    } else {
        for(sample_size in seq(0, 100, 2)){
          power <- power_two_sample(sample_size = sample_size, effect_size = input$effect_size, test_alt = test_alt)
          vPowerOut <- append(vPowerOut, power)
          vSampleSizeOut <- append(vSampleSizeOut, sample_size)
        }
        power_data <- data.frame(vSampleSizeOut, vPowerOut)
        plot_ly(data = power_data, x = ~vSampleSizeOut, y = ~vPowerOut, type = 'scatter', mode = 'lines') %>%
          add_lines(y = 0.8,line = list(color = "grey")) %>%
          layout(showlegend = F, title = 'Power curve', xaxis = list(title = 'Sample size'),  yaxis = list(title = 'Power', range = c(0,1)))
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)
