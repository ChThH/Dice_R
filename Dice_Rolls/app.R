#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(
        title = HTML("<h1>Dice Rolling Simulator</h1><h3>(with optional reroll for values)</h3>")
        ),

    # Sidebar with a text input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("num",
                      "Number of dice:",
                        value = 2),
            textInput("sides",
                      "Amount of sides on dice",
                      value = 6),
            textInput("reroll",
                      "Reroll if equals (optional):"),
            actionButton("roll", "Roll Dice!")
        ),
        mainPanel(
            h3("Results of Dice Rolling"),
            br(),
            HTML(paste(tags$span(htmlOutput('results'), style="color:blue"))),
            br(),br(),
            h4("Some Stats on your dice roll"),
            HTML(paste("Sum:",    tags$span(htmlOutput('sum'), style='color:green'))),
            HTML(paste("Mean:",   tags$span(htmlOutput('mean'), style='color:purple'))),
            HTML(paste("Median:", tags$span(htmlOutput('median'), style='color:red'))),
            HTML(paste("Mode:",   tags$span(htmlOutput('mode'), style='color:brown')))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    dice <- function(num, sides){
            sample(as.integer(sides), num, TRUE)
    }
        
    dice_reroll <- function(num, sides, reroll, times=0){
        rolls <- dice(num, sides)
        if ((length(sides) == 1 && all(1:sides %in% reroll)) ||
            (length(sides) != 1 && all(sides %in% reroll))){
            print('Reroll values cannot be identical to all sides.')
        } else if (times == 0){
            while (any(rolls %in% reroll)){
                rolls[rolls %in% reroll] <- dice(length(rolls[rolls %in% reroll]),sides)
            } 
        } else {
            for(time in times){
                rolls[rolls %in% reroll] <- dice(length(rolls[rolls %in% reroll]),sides)
            }
        }
        return(rolls)
    }
    
    mode_freq <- function(dice_result){
        uq <- unique(dice_result)
        uq[which.max(tabulate(match(dice_result,uq)))]
    }
    
    results <- reactive({
        input$roll
        isolate({
        dice_reroll(num = input$num, 
                    sides = input$sides, 
                    reroll = input$reroll)
        })
    })
    
    mode_freq <- function(dice_result){
        uq <- unique(dice_result)
        tab <- tabulate(match(dice_result,uq)) 
        sort(uq[tab == max(tab)])
    }
    
    output$results <- renderText(results())
    output$sum     <- renderText(sum(results()))
    output$mean    <- renderText(mean(results()))
    output$median  <- renderText(median(results()))
    output$mode    <- renderText(mode_freq(results()))
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
