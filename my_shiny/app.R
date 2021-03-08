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
list_choices <-  unique(msleep$vore)[!is.na(unique(msleep$vore))]
names(list_choices) <- paste(unique(msleep$vore)[!is.na(unique(msleep$vore))],"vore",sep="")

# Define UI for application that draws a histogram
ui <- navbarPage("Shiny app",
    tabPanel("msleep",
    fluidPage(

    # Application title
    
   
    sidebarLayout(
        sidebarPanel(
            selectInput("select", label = h3("Plot by type of alimentation"), 
                        choices = character(0),
                        selected = 1)
        ),
        mainPanel(
            plotOutput(outputId = "plot")
        )
))),
tabPanel("Random generator",
         sidebarLayout(position = "right",
                       sidebarPanel(
                           #Select means a combo box
                           selectInput("dist", label = h3("Select the distribution"), 
                                       choices = list(Normal="rnorm", Uniform="runif", Exponential="rexp"), #text="value"-->Normal="rnow"
                                       selected = 1), #default the frist one is the selected
                           #Slider: que se deliza paa elegir la opción 
                           sliderInput("n_sample", label = h3("Number of samples"), 
                                       min = 10, max = 100, value = 50), #max and minm value del slider; value="50" ( es el current vaue, valor inicial)
                           sliderInput("n_bins", label = h3("Number of bins"), min = 1, 
                                       max = 50, value = 30)
                       ), # sidebarPanel
                       mainPanel(
                           #Create a div with nothing inside --> server side that creates the content
                           plotOutput(outputId = "pulpo")
                       ) # mainPanel
         ) # sidebarLayout

),
tabPanel("References",
         p(tags$button(class="btn btn-default", 
                       `data-toggle`="collapse", 
                       `data-target`="#collapseExample",
                       "References")),
         div(class="collapse", id="collapseExample",
             div(class="card card-body",
                 includeMarkdown("references.md")
             ))
))
col_scale <- scale_colour_discrete(limits = list_choices)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    updateSelectInput(session, "select",
                      choices = list_choices,
                      selected = tail(list_choices, 1)
    )
    output$plot <- renderPlot({
        ggplot(msleep %>% filter(vore == input$select), aes(bodywt, sleep_total, colour = vore)) +
            scale_x_log10() +
            col_scale+
            geom_point() + facet_wrap(~ vore, nrow = 2)
    })
    #comand that generates the sample
    #reactive: input changes, when sample changes
    #Takes the input distribution
    #paste/paste0--> combine things: rnorm(input) --> a string
    # You want to "eval" it --> obtaining 20 normal objects (rnorm(20))--> what you want to plot
    #Since cmd comes from a reactive values is not a value is a function
    cmd = reactive(eval(parse(text=paste(input$dist,"(",input$n_sample,")",sep=""))));
    output$pulpo<-renderPlot(hist(cmd()))
}


# Run the application 
shinyApp(ui = ui, server = server)
