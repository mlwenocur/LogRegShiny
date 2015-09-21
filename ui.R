library(shiny)

shinyUI(pageWithSidebar(
    headerPanel('Logistic Regression Trainer'),
    sidebarPanel(
        numericInput('userSeed', 'Input seed', 222),
        numericInput('minX', 'Input X min value', 0.3),
        numericInput('maxX', 'Input X max value', 5),
        numericInput('sampsPerTrial', 'Input number of samples per trial', 100, min=20, max =20000, step = 10),
        selectInput('selectedModel', 'Model Selector',
                           c('Polynomial Model' = 'poly',
                             'Harmonic Model' = 'harmonic'),
                           'poly'),
        selectInput('grView', 'Graph Type', c('Log Odds Graph' = 'rho',
                                              'Probability Graph' = 'prob'), 
                    'prob'),
        submitButton('Submit')
    ),
    mainPanel(
        textOutput('trialSamples'),
        textOutput('modelHeader'),
        plotOutput('newHist')
    )
    
))

