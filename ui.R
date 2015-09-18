library(shiny)

shinyUI(pageWithSidebar(
    headerPanel('Logistic Regression Trainer'),
    sidebarPanel(
        numericInput('sampsPerTrial', 'Input number of samples per trial', 100, min=20, max =20000, step = 10),
        selectInput('selectedModel', 'Model Selector',
                           c('Polynomial Model' = 'poly',
                             'Harmonic Model' = 'harmonic'),
                           'harmonic'),
        submitButton('Submit')
    ),
    mainPanel(
        textOutput('trialSamples'),
        textOutput('modelHeader'),
        plotOutput('newHist'),
        plotOutput('newRho')
    )
    
))

