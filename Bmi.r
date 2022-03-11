library(shiny)
library(shinythemes)
ui <- navbarPage(theme = shinytheme(theme = "united"),  title = "BMI Calculator",
                 
                 
                 tabPanel(title = "BMI",
                          
                          wellPanel(
                          tags$p(style="font-family:sans-serif; font-size:160%",
                                 "Body mass index (BMI) is a value derived from the mass (weight) and height of a person.", tags$br(),
                                  "The BMI is defined as the body mass divided by the square of the body height, and is expressed in units of kg/m2,", tags$br(),
                                  "resulting from mass in kilograms and height in metres.", tags$br(), 
                                  "The BMI may be determined using a table or chart which displays BMI as a function of mass and height using contour", tags$br(),
                                  "lines or colours for different BMI categories, and which may use other units of measurement", tags$br(),
                                  "The BMI is a convenient rule of thumb used to broadly categorize a person as underweight, normal weight, overweight,", tags$br(),
                                  "or obese based on tissue mass (muscle, fat, and bone) and height.", tags$br(),
                                  "Major adult BMI classifications are underweight (under 18.5 kg/m2), normal weight (18.5 to 24.9), overweight (25 to 29.9),", tags$br(),
                                  "and obese (30 or more).[1] When used to predict an individual's health, rather than as a statistical measurement for groups,", tags$br(),
                                  "the BMI has limitations that can make it less useful than some of the alternatives, especially when applied to individuals with", tags$br(),
                                  "abdominal obesity, short stature, or unusually high muscle mass."),
                          
                          tags$p("Source: ", tags$a(href="https://en.wikipedia.org/wiki/Body_mass_index", "Wikipedia"))
                          ),
                          tags$img(height = 500, width = 700, src="categories.jpg", style = "border: 5px solid black")
                           ),
                 tabPanel(title = "Calculate",
                           fluidRow(
                             column(width = 4,
                             radioButtons(inputId = "unit", label = h2("Choose weight unit"), choices = list("Kilogram (Kg)" = 1, "Pounds (lbs)" = 2), selected = 1),
                             textInput(inputId = "weight", label = "Enter Weight", value = 0),
                             # textOutput("opWeight")
                           ),
                             column(width = 4, offset = 3,
                                    h2("Select Height"),
                                    sliderInput(inputId = "feet", label = "Feet", min = 3, max = 8, value = 5, step = 1),
                                    sliderInput(inputId = "inch", label = "Inches", min = 0, max = 11, value = 0, step = 1),
                                    # textOutput("opHeight")
                                    )),
                          fluidRow(
                            column(width = 4, offset = 4,
                                   actionButton(inputId = "calculate", label = "Calculate BMI"),
                                   verbatimTextOutput("ans")
                                   )
                          )
                           )
  
)

server <- function(input, output, session) {
  rv <- reactiveValues(w = 0, h = 0)
  
  observeEvent(input$calculate, {
    
    if (input$unit == 1) {
       rv$w <- as.numeric(input$weight)
    } else {
        rv$w <- as.numeric(input$weight) / 2.205
    }

    rv$h <- rv$h + (as.numeric(input$feet) / 3.281)
    rv$h <- rv$h + (as.numeric(input$inch) / 39.37)
  })
  
  output$ans <- renderPrint({
    bmi <- (rv$w / (rv$h * rv$h))
    return(bmi)
  })
  
  
}

shinyApp(ui, server)