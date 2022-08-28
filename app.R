library(lubridate)
library(shiny)
library(shinyWidgets)
library(beepr)
ui <- fluidPage(
    setBackgroundColor(
        color = c("#4568dc", "#b06ab3"),
        gradient = "radial",
        direction = "top","right"
    ),
    titlePanel("Saif's Pomodoro Timer", windowTitle = "Tomatoes"),
    
    hr(),
    actionButton('start','Start'),
    actionButton('stop','Stop'),
    actionButton('reset','Reset'),
    numericInput('seconds','Seconds:',value=3,min=0,max=99999,step=1),
    textOutput('timeleft'),
    
    

 )

server <- function(input, output, session) {
    
    # Initialize the timer, 3 seconds, not active.
    timer <- reactiveVal(3)
    active <- reactiveVal(FALSE)
    
    # Output the time left.
    output$timeleft <- renderText({
        paste("Time left: ", seconds_to_period(timer()))
    })
    
    # observer that invalidates every second. If timer is active, decrease by one.
    observe({
        invalidateLater(1000, session)
        isolate({
            if(active())
            {
                timer(timer()-1)
                if(timer()<1)
                {
                    active(FALSE)
                    showModal(modalDialog(
                        title = "One tomato down",
                        "Countdown completed, good job!"
                        
                    ))

                    beep(sound = filename<- "Sword Draw Sound Effect.wav", expr = NULL)
                    }
            }
        })
    })
    
    # observers for actionbuttons
    observeEvent(input$start, {active(TRUE)})
    observeEvent(input$stop, {active(FALSE)})
    observeEvent(input$reset, {timer(input$seconds)})
    
}

shinyApp(ui, server)