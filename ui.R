library(shiny)

shinyUI(pageWithSidebar(
    headerPanel('Logistic Regression Trainer'),
    sidebarPanel(
        numericInput('sampsPerTrial', 'A: Input sample size', 100, min=20, max =20000, step = 10),
        selectInput('selectedModel', 'B: Model Selector',
                           c('Polynomial Model' = 'poly',
                             'Harmonic Model' = 'harmonic'),
                           'poly'),
        numericInput('minX', 'C: Input X min value', 0.3),
        numericInput('maxX', 'D: Input X max value', 5),
        numericInput('userSeed', 'E: Input seed', 222),
        selectInput('grView', 'F: Graph Type', c('Log Odds Graph' = 'rho',
                                              'Probability Graph' = 'prob'), 
                    'prob'),        

        submitButton('G: Submit')
    ),
    mainPanel(
        textOutput('trialSamples'),
        textOutput('modelHeader'),
        plotOutput('newHist')
    )
    
))

