# shiny
shiny for web applications

Exercise 1 The star wars example\
You should create a shiny app with the following on its main panel:\
h1("First level title"), h2("Second level title"), h3("Third level title"), h4("Fourth level title"), h5("Fifth level title"), h6("Sixth level title")\
and run this app.

Exercise 2\
You should run the example “04_mpg ”.\
Modify this app to generate box plots around the species of the iris dataset. Add a checkbox to exclude the setosa data

Exercise 3\
Run examples 1, 2 and 4 to help with this exercise\
Write an app that will allow the user to select a variable from the mtcars data set. The app will then create a scatterplot of mpg against that selected variable.The user should also be able to change the point type in the app.

Exercise 4\
Run examples 4 and 5 to help with this exercise.\
Create a shiny app that shows a plot pf the data in the mpg dataset (available in ggpllot2). The app should allow the user to select either city mileage (cty) or\ highway mileage (hwy) to plot on the y-axis; the x-axis should show the displacement of the cars (displ). The selection may be done using either drop menu or radio\ buttons. You should add three sliders to the app to allow the user to\
(i) select the number of points (min = 1, max = 234) to plot,\
(ii) specify the x-limits on the plot, and\
(iii) specify the y-limits on the plot.


solution\
(i)

library(shiny)\
ui <- fluidPage(\
  titlePanel("My Star Wars App"),\
  sidebarLayout(position = "left",\
                sidebarPanel("sidebar panel"),\
                mainPanel(\
                  h1("First level title"),\
                  h2("Second level title"),\
                  h3("Third level title"),\
                  h4("Fourth level title"),\
                  h5("Fifth level title"),\
                  h6("Sixth level title")\
                )\
  )\
)\
server <- function(input,output){}\
shinyApp(ui, server)


(ii)

ui <- fluidPage(
  titlePanel("Iris Data"),\
  sidebarLayout(\
    sidebarPanel(\
      selectInput("Species","Select Species:",\
                  choices=c("setosa", "versicolor", "virginica")),\
      checkboxInput("setosa","show setosa", FALSE)\
    ),\
    mainPanel(\
      h3(textOutput("caption")),\
      plotOutput("speciesplot")\
    )
  
)

server <- function(input,output){
  
  mydata <- reactive({iris[iris$Species == input$Species,]})\
  output$caption <- renderText({\
    mydata()\
  })
  
  output$speciesplot <- renderPlot({
    boxplot(as.formula(mydata()),\
            data = iris,\
            outline = input$setosa,\
            col = "#75AADB", pch = 19)\
  })
  
}

shinyApp(ui, server) 


(iii)

mpgData <- mtcars\
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))\
ui <- fluidPage(\
  titlePanel("Miles Per Gallon"),\
  sidebarLayout(\
    sidebarPanel(\
      selectInput("variable", "Variable:",\
                  c("Cylinders" = "cyl",\
                    "Transmission" = "am",\
                    "Gears" = "gear")),
      
    ),
    mainPanel(
      h3(textOutput("caption")),
      plotOutput("mpgPlot")
      
    )\
  )\
)

server <- function(input, output) {
  formulaText <- reactive({
    paste("mpg ~", input$variable)\
  })\
  output$caption <- renderText({
    formulaText()\
  })\
  output$mpgPlot <- renderPlot({
    plot(as.formula(formulaText()),\
            data = mpgData,\
            outline = input$outliers,\
            col = "#75AADB", pch = 19)\
  })
  
}


shinyApp(ui, server)


(iv)

library(ggplot2)\
ui <- fluidPage(\
  titlePanel("Miles Per Gallon"),\
  sidebarLayout(\
    sidebarPanel(\
      sliderInput("points", "Points:",min = 1,max = 234, value = c(25, 50)),\
      sliderInput("xlim", "Xlimits:",min = 1,max = 234, value = c(25, 50)),\
      sliderInput("ylim", "Ylimits:",min = 1,max = 234, value = c(25, 50)),\
      radioButtons("variable", "Variable:",
                   choices = c("City miles" = "cty",
                               "highway" = "hwy",
                               "displacement" = "displ")),
    ),\
    mainPanel(\
      plotOutput("mpgPlot")
      
    )
  )\
)

server <- function(input, output) {\
  sliderValues <- reactive({\
    Value = as.character(c(input$points,\
                           input$xlim,\
                           input$ylim))\
  }),\
  output$mpgPlot <- renderPlot({\
    ggplot(mpg, aes(x=displ, y=cty)) + geom_point(),\
    ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()\
  })
  
shinyApp(ui, server)  

