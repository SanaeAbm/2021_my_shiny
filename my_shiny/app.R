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
library(shinyjs)

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
                                       min = 10, max = 100, value = 50), #max and minm value del slider; value="50" ( es el current value, valor inicial)
                           fluidRow( #page that is 12 columns
                               h3(style = "margin-left: 20px; margin-bottom: 0px;", "Number of bins"),
                               column(2,#columns to do:
                                      div(style = "margin-top: 37px", checkboxInput("auto_bins", label = "auto", value = TRUE))
                               ),
                               column(10,#10 columns to do:
                                      sliderInput("n_bins", label="", min = 1, max = 50, value = 30)
                               )
                           )
                       ), # sidebarPanel
                       mainPanel(
                           #Create a div with nothing inside --> server side that creates the content
                           tabsetPanel(type = "tabs",
                                       #tabPanel("Título pestaña", what to do)
                                       tabPanel("Plot", plotOutput("histPlot")),#plot hist
                                       tabPanel("Summary", verbatimTextOutput("histSummary")), #some text
                                       tabPanel("Table", tableOutput("histTable")) #plot a table
                           )
                       ))

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
),
useShinyjs()#inject the java srcript library n your html file
#Now, shiny app is using some inyect js



)
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
    #samples= reactive(eval(parse(text=paste(input$dist,"(",input$n_sample,")",sep=""))));
    #Other way to do it: get distibutions and then the samples:
    
    samples <- reactive({
        dist <- eval(parse(text=paste(input$dist)))
        dist(input$n_sample)
    })
    
    
    
    
    #We need observe because the element "input$ato_bins" is a function: changes with user interaction
    #Observe is a reactive function that can get value that input gives to it
    #Whenever auto_bins is changed, this fuunction is called
    #Observe doesnt return any value
    #when auto_bins is true we desable the n_bins
    #Css class: disable and enable --> in the shinyjs()
    observe(if(input$auto_bins) disable("n_bins") else enable("n_bins") )
    
    #If the botton is pressed: plots hist w/out bins
    #Otherwise, if clicked, give number of bins:
    output$histPlot<- renderPlot(
        #if(input$auto_bins) hist(samples()) 
        #else hist(cmd(), breaks=input$n_bins)
        hist(samples(), main="Random Generation",
             #If there is auto a use struges and if not  I use bins
        breaks = if(!input$auto_bins) {input$n_bins} else {"Sturges"})
    );
    output$histSummary <- renderPrint(summary(samples()))
    output$histTable <- renderTable(samples())
}



# Run the application 
shinyApp(ui = ui, server = server)
