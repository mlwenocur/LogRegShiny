library(shiny)

shinyUI(pageWithSidebar(
    headerPanel('Example plot'),
    sidebarPanel(
        numericInput('sampsPerTrial', 'Input number of samples per trial', 100, min=20, max =20000, step = 10),
        submitButton('Submit'),
        selectInput('selectedModel', 'Model Selector',
                           c('Polynomial Model' = 'poly',
                             'Harmonic Model' = 'harmonic'),
                           'harmonic'),
                
        sliderInput('mu', 'Guess at the mean', value = 70, min=62, 
                    max = 74, step = 0.05)
    ),
    mainPanel(
        h4('Num samples per trial'),
        verbatimTextOutput('trialSamples'),
        plotOutput('newHist')
    )
    
))

