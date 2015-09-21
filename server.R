library(shiny)
library(UsingR)
require("reshape2")
require("ggplot2")
require('grid')
require('gridExtra')

source('logRegTrainer.R')

RhoForLinear <- function(x) (-2  + 0.5 * x + 0.1 * x^2)
ProbForLinear <- function(x) (1 + exp(2  - 0.5 * x - 0.1 * x^2)) ^ -1

xPolyLabel <- 'Debt to Income Ratio'
yPolyProbLabel <- 'Prob of Default'
yPolyRhoLabel  <- 'Log Odds of Default'

xHarmLabel <- 'Cyclic Time'
yHarmProbLabel <- 'Prob of Calling In Sick'
yHarmRhoLabel <- 'Log Odds of Calling In Sick'

set.seed(123);

GenLinModSampData <- function(nSamps, minX = 0.3, maxX = 5){
    set.seed(123)
    X <- runif(nSamps, minX, maxX); 
    X2 <- X^2
    yVals <- sapply(X, function(x){rbinom(1, 1, ProbForLinear(x))})
    z = data.frame(yVals = yVals, X = X, X2 = X2)
    return(z)
}

GetHarmModSampData <- function(extMods){
    mod <- extMods[[1]]$model
    yVals <- mod$y
    X <- extMods[[1]]$X
    z = data.frame(yVals = yVals, X = X)
    return(z)
}

GetEmpiricalQuantities <- function(dFrame){
    mod1 <- glm(yVals ~ X + X2, family = binomial, dFrame)
    X <- dFrame$X
    cfs <- coefficients(mod1)
    fmt <- paste0('Fitted Coefficients: %3.2f %3.2f %3.2f',
                  '\n                     actual: -2.00 0.50 0.10')
    empir_VS_actual <- sprintf(fmt, cfs[1], cfs[2], cfs[3])
    fitRho <- cfs[1] + cfs[2] * X + cfs[3] * X^2; 
    fitProb <- (1 + exp(-fitRho)) ^ -1
    exactRho <- -2.0 + 0.5 * X + 0.1 * X^2
    exactProb <- (1 + exp(-exactRho))  ^ -1
    return(list(summaryString = empir_VS_actual, fitProb = fitProb, 
                fitRho = fitRho, exactProb = exactProb, exactRho = exactRho))
}
GetEmpQuantsHarmonic <- function(extMods){
    mod <- extMods[[1]]$model
    X <- extMods[[1]]$X
    cfs <- coefficients(mod)
    fmt <- paste0('Fitted Coefficients: %3.2f %3.2f %3.2f %3.2f',
                  '\n                     actual: 1.00 3.00 2.00 4.00')   
    empir_VS_actual <- sprintf(fmt, cfs[1], cfs[2], cfs[3], cfs[4])
    #fitProb <- mod$fitted.values
    #fitRho <- log(fitProb / (1 - fitProb))
    fitRho <- cfs[1] * sin(X) + cfs[2] * cos(X) + 
                  cfs[3] * sin(2 * X) + cfs[4] * cos(2 * X)
    fitProb <- (1 + exp(-fitRho)) ^ -1
    exactRho <- sin(X) + 3 * cos(X) + 2 * sin(2 * X) + 4 * cos(2 * X)
    exactProb <- (1 + exp(-exactRho)) ^ -1
    return(list(summaryString = empir_VS_actual, fitProb = fitProb, 
                fitRho = fitRho, exactProb = exactProb, exactRho = exactRho))
}

GenProbPlotFromX <- function(samples, empirQuants, xlabel, ylabel){
    summaryText <- empirQuants$summaryString
    my_grob = grobTree(textGrob(summaryText, x = 0.25, y = 0.1, hjust=0, 
                                gp = gpar(col = "blue", fontsize = 12, 
                                          fontface = "italic")))
    X <- samples$X
    fittedProb <- empirQuants$fitProb
    exactProb <- empirQuants$exactProb
    ratio.display <- 1
    rangeY <- max(fittedProb)-min(fittedProb)
    rangeY <- max(rangeY, max(exactProb) - min(exactProb))
    ratio.values <- (max(X)-min(X))/rangeY
    probCmp <- melt(data.frame(X, fittedProb, exactProb), id = "X")
    ggplot(data = probCmp, aes(x = X, y = value, color = variable)) + 
        geom_line(size = 1.5) + coord_fixed(ratio = 5) + 
        labs(x = xlabel, y = ylabel) + annotation_custom(my_grob)  +
        coord_fixed(ratio.values / ratio.display) 
}

GenRhoPlotFromX <- function(samples, empirQuants, xlabel, ylabel){
    summaryText <- empirQuants$summaryString
    my_grob = grobTree(textGrob(summaryText, x = 0.25, y = 0.1, hjust=0, 
                                gp = gpar(col = "blue", fontsize = 12, 
                                          fontface = "italic")))
    X <- samples$X
    fittedRho <- empirQuants$fitRho
    exactRho <- empirQuants$exactRho
    ratio.display <- 1
    rangeY <- max(fittedRho)-min(fittedRho)
    rangeY <- max(rangeY, max(exactRho) - min(exactRho))
    ratio.values <- (max(X)-min(X))/rangeY
    rhoCmp <- melt(data.frame(X, fittedRho, exactRho), id = "X")
    ggplot(data = rhoCmp, aes(x = X, y = value, color = variable)) + 
        geom_line(size = 1.5) + 
        coord_fixed(ratio = 1) + 
        labs(x=xlabel, y = ylabel) +
        annotation_custom(my_grob) +
        coord_fixed(ratio.values / ratio.display)
       
}

shinyServer(function(input, output) {
    modelFunc <- NULL
    samples <- NULL
    empirQuants <- NULL
    xLabel <- NULL
    yProbLabel <- NULL
    yRhoLabel <- NULL
    
    defModelDescr <- "Model: rho(x) = -2  + 0.5 * x + 0.1 * x^2"
    harmModDescr <- "Model: rho(x) = sin(x) + 3cos(x) + 2sin(2x) + 4cos(2x)"
    
    output$trialSamples <- renderPrint({cat(
        paste('Samples per trial =', input$sampsPerTrial))
        })
    output$modelHeader <- renderPrint({cat(
        ifelse(input$selectedModel == 'poly', defModelDescr, harmModDescr))
    })
    output$newHist <- renderPlot({
        if (input$selectedModel == 'poly')
        {
            xLabel <<- xPolyLabel
            yProbLabel <<- yPolyProbLabel
            yRhoLabel <<- yPolyRhoLabel
            
            samples <<- GenLinModSampData(input$sampsPerTrial,
                                          input$minX, input$maxX)
            empirQuants <<- GetEmpiricalQuantities(samples)
        }
        else {
            xLabel <<- xHarmLabel
            yProbLabel <<- yHarmProbLabel
            yRhoLabel <<- yHarmRhoLabel
            modelFunc <- CreateSampleSession()
            extMods <- SimLogRegTrials(modelFunc, input$minX, input$maxX, 
                                       1, input$sampsPerTrial)
            empirQuants <<- GetEmpQuantsHarmonic(extMods)
            samples <<- GetHarmModSampData(extMods)
        }
        if (input$grView == 'prob')
        {
            GenProbPlotFromX(samples, empirQuants, 
                                      xLabel, yProbLabel)
        }
        else {
            GenRhoPlotFromX(samples, empirQuants, 
                            xLabel, yRhoLabel)
        }
    })
    
    
})


