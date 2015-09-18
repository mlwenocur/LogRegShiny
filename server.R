library(shiny)
library(UsingR)
require("reshape2")
require("ggplot2")
require('grid')
require('gridExtra')

source('logRegTrainer.R')

RhoForLinear <- function(x) (-2  + 0.5 * x + 0.1 * x^2)
ProbForLinear <- function(x) (1 + exp(2  - 0.5 * x - 0.1 * x^2)) ^ -1
set.seed(123);

genLinModSampData <- function(nSamps, minX = 0.3, maxX = 5){
    X <- runif(nSamps, minX, maxX); 
    X2 <- X^2
    yVals <- sapply(X, function(x){rbinom(1, 1, probForLinear(x))})
    z = data.frame(yVals = yVals, X = X, X2 = X2)
    return(z)
}

GetEmpiricalQuantities <- function(dFrame){
    mod1 <- glm(yVals ~ X + X2, family = binomial, dFrame)
    X <- dFrame$X
    cfs <- coefficients(mod1)
    fmt <- paste0('Fitted Coefficients: %4.3f %4.3f %4.3f',
                  '\n                     actual: -2.000 0.500 0.100')
    empir_VS_actual <- sprintf(fmt, cfs[1], cfs[2], cfs[3])
    fitRho <- cfs[1] + cfs[2] * X + cfs[3] * X^2; 
    fitProb <- (1 + exp(-fitRho)) ^ -1
    return(list(summaryString = empir_VS_actual, fitProb = fitProb, 
                fitRho = fitRho))
}

GenProbPlotFromX <- function(samples, empirQuants){
    summaryText <- empirQuants$summaryString
    my_grob = grobTree(textGrob(summaryText, x = 0.25, y = 0.1, hjust=0, 
                                gp = gpar(col = "blue", fontsize = 12, 
                                          fontface = "italic")))
    X <- samples$X
    fittedProb <- empirQuants$fitProb
    probCmp <- melt(data.frame(X, fittedProb, exactProb = ProbForLinear(X)), id = "X")
    ggplot(data = probCmp, aes(x = X, y = value, color = variable)) + 
        geom_line(size = 1.5) + 
        coord_fixed(ratio = 5) + 
        labs(x='Debt to Income Ratio', y = 'Probability of Default') +
        annotation_custom(my_grob)    
}

GenRhoPlotFromX <- function(samples, empirQuants){
    summaryText <- empirQuants$summaryString
    my_grob = grobTree(textGrob(summaryText, x = 0.25, y = 0.1, hjust=0, 
                                gp = gpar(col = "blue", fontsize = 12, 
                                          fontface = "italic")))
    X <- samples$X
    fittedRho <- empirQuants$fitRho
    probCmp <- melt(data.frame(X, fittedRho, exactRho = RhoForLinear(X)), id = "X")
    ggplot(data = probCmp, aes(x = X, y = value, color = variable)) + 
        geom_line(size = 1.5) + 
        coord_fixed(ratio = 1) + 
        labs(x='Debt to Income Ratio', y = 'Log Odds of Default') +
        annotation_custom(my_grob)    
}

shinyServer(function(input, output) {
    samples <- NULL
    empirQuants <- NULL
    defModelDescr <- "Model: rho(x) = -2  + 0.5 * x + 0.1 * x^2"
    harmModDescr <- "Model: rho(x) = sin(x) + cos(x) + 2cos(2x) + 2sin(2x)"
    
    output$trialSamples <- renderPrint({cat(
        paste('Samples per trial =', input$sampsPerTrial))
        })
    output$modelHeader <- renderPrint({cat(
        ifelse(input$selectedModel == 'poly', defModelDescr, harmModDescr))
    })
    output$newHist <- renderPlot({
        samples <<- genLinModSampData(input$sampsPerTrial)
        empirQuants <<- GetEmpiricalQuantities(samples)
        probGraph <- GenProbPlotFromX(samples, empirQuants)
        probGraph
        
    })
    output$newRho <- renderPlot({
        z <- input$sampsPerTrial
        rhoGraph <- GenRhoPlotFromX(samples, empirQuants)
        rhoGraph
    })
    
})


