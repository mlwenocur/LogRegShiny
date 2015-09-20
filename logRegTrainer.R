
library(ggplot2)
require(gridExtra)
require(stats, quietly = TRUE)
RETURN_WT = 501
RETURN_VAL = 500


GraphProbAndRho <- function(funcs, mn, mx, by = 0.01, plotIt = FALSE){
    x <- seq(mn, mx, by)
    y <- sapply(x, function(a){v = funcs$GenCovariates(a); return(funcs$GenProbFromCoVars(v))})
    rho <- sapply(y, function(a) GetRho(a))
    df = data.frame(x=x, prob = y, logOdds = rho)
    p <- ggplot(df, aes(x,prob)) + geom_line() + ggtitle("Probability as a function of x")
    p1 <- ggplot(df, aes(x,logOdds)) + geom_line() + ggtitle('Log(odds) as a function of x')
    graphList <- list(probPlot = p, rhoPlot = p1)
    if (plotIt) grid.arrange(ga$probPlot, ga$rhoPlot, ncol=2)
    return(graphList)
}
# Generate a power function object
CreatePowerFunc <- function(expo, coeff)
{
    columnTag <- paste0("x_to_", expo)
    PowerFunc <- function(xVals, retType){
        if (retType == RETURN_WT){
            #print(paste('column tag is', columnTag))
            return(xVals[[columnTag]] * coeff)
        }
        else {
            rval <- list(xVals ^ expo)
            names(rval) <- c(columnTag)
            return(rval)
        }
        
    }
    IsFactor <- function(){return(FALSE)}
    GetName <- function(){ return(columnTag)}
    SetVal <- function(x){}
    GetCoef <- function(){return(coeff)}
    powerObj <- c(generator = PowerFunc, getName = GetName,
                  setVal = SetVal, resetVal = SetVal,
                  isFactor = IsFactor, getCoef = GetCoef)
}
CreateTag <- function(funcName, param, varName){
    if (param == 1)
    {
        tag <- paste0(funcName,'_', varName)
        return(tag)
    }
    if (param >= 0){
        tag <- paste0(funcName, '_', param, varName)
        return(tag)
    }
    else {
        tag <- paste0(funcName, '_._', abs(param), varName)
        return(tag)
    }    
    
}

WrapQuantFuncs <- function(f, param, coeff, columnTag){
    PowerFunc <- function(xVals, retType){
        if (retType == RETURN_WT){
            return(xVals[[columnTag]] * coeff)
        }
        else {
            rval <- list(f(param * xVals))
            names(rval) <- c(columnTag)
            return(rval)
        }
        
    }
    IsFactor <- function(){return(FALSE)}
    GetName <- function(){ return(columnTag)}
    SetVal <- function(x){}
    GetCoef <- function(){return(coeff)}
    powerObj <- c(generator = PowerFunc, getName = GetName,
                  setVal = SetVal, resetVal = SetVal,
                  isFactor = IsFactor, getCoef = GetCoef)
}
CreateExpoFunc <- function(param, coeff)
{
    tag <- CreateTag('exp', param, 'x')
    expFunc <- WrapQuantFuncs(exp, param, coeff, tag)
}

CreateSinFunc <- function(param, coeff)
{
    tag <- CreateTag('sin', param, 'PIx')
    expFunc <- WrapQuantFuncs(sinpi, param, coeff, tag)
}

CreateCosFunc <- function(param, coeff)
{
    tag <- CreateTag('cos', param, 'PIx')
    expFunc <- WrapQuantFuncs(cospi, param, coeff, tag)
}

SimLogRegTrials <- function(generators, minx, maxx, numTrials, seqLen,
                            independentTrials = TRUE, seed = 123){
    set.seed(seed)
    GenCovariates <- generators$GenCovariates
    GenProbFromCoVars <- generators$GenProbFromCoVars
    GetCovStatus <- generators$GetCovStatus
    xCum <- c()
    yCum <- c()
    models <- lapply(1:numTrials, function(j){
        z <- runif(seqLen, minx, maxx)
        xVals <- lapply(z, function(a) GenCovariates(a))
        yVals <- GenRandSampLogit(xVals, GenProbFromCoVars)
        if (independentTrials == FALSE){
            xCum <<- c(xCum, xVals)
            yCum <<- c(yCum, yVals)
            xVals <- xCum
            yVals <- yCum
        }
        xt <- sapply(names(xVals[[1]]), function(j) j = sapply(xVals, function(a)a[[j]]))
        
        xt <- data.frame(xt)
        noIntercept <- TRUE
        for (fname in names(xt)){
            if (GetCovStatus(fname)){
                noIntercept <<- FALSE
                xt[[fname]] <- as.factor(xt[[fname]])
            }
        }
        if (noIntercept == FALSE){
            model <- glm(data = xt, yVals ~ ., family = binomial)
            extMod <- list(X = z, model = model)
        }
        else {
            model <- glm(data = xt, yVals ~ . - 1, family = binomial)
            extMod <- list(X = z, model = model)
        }
    })
    return(models)
}

DefineFactorFunc <- function(facName, factorLevels, factorProbs, 
                             factorWts, interactData = NA){
    selection <- NA
    fixedVal <- NA
    g <- function(fixedChoice){
        if (!(fixedChoice %in% factorLevels)) fixedChoice = 1
        fixedVal <<- fixedChoice
    }
    ResetVal <- function(){fixedVal <<- NA}
    
    GetName <- function(){return(facName)}
    
    IsFactor <- function(){return(TRUE)}
    
    f <- function(x, retType){
        if (retType == RETURN_VAL){
            if (is.na(fixedVal) == FALSE){
                selection <<- fixedVal
            }
            else {
                selection <<- which.max(rmultinom(1, 1, factorProbs))
            }
            
            val <- factorLevels[selection]
            retVal <- list(val)
            names(retVal) <- c(facName)
            return(retVal)
        }
        else {
            retVal <- factorWts[selection]
            if (anyNA(interactData) == FALSE){
                v <- sapply(interactData, function(elem){
                    score <- elem$score$generator
                    cof   <- elem$cof
                    newScore <- score(x, retType) * cof
                    return(newScore)
                })
                vsum <- sum(v)
                retVal <- retVal * (1 + vsum)
            }
            return(retVal)
        }
    }
    rval <- c(setVal = g, generator = f, getName = GetName,
              resetVal = ResetVal, isFactor = IsFactor)
    return(rval)
}

GenRandSampLogit <- function(xVals, probGenerator){
    samp <- sapply(xVals, function(x){rbinom(1, 1, probGenerator(x))})
    
    return(as.integer(samp))
}


# Create functions to generate covariates and success probability
# generator.
RegressorFuncsGen <- function(funcList, wts = rep(1, length(funcList))){
    flistLen <- length(funcList)
    GenCovariates <- function(x){
        v <- sapply(funcList, function(f)f$generator(x, RETURN_VAL))
        return(v)
    } 
    GenFixedCovariates <- function(x, factorSelections) {
        
    }
    GetCovStatus <- function(funcName){
        for (j in funcList){
            if (funcName == j$getName()){
                return(j$isFactor())
            }
        }
        print('NO SUCH NAME!!')
        return(FALSE)
    }
    GenProbFromCoVars <- function(xVal){
        v <- sapply(funcList, function(f)f$generator(xVal, RETURN_WT))
        linearTerm <- sum(v * wts)
        p <- (1 + exp(-linearTerm))^ {-1}
        return(p)   
    }
    GetCoefficients <- function(){
        v <- sapply(funcList, function(f)f$getCoef())
        return(v)
    }
    SetFixedValues <- function(fixVals){
        v <- sapply(funcList, function(f)f$setVal(fixVals))
    }
    ResetFixedVals <- function(){
        v <- sapply(funcList, function(f)f$resetVal())
    }
    return(c(GenCovariates = GenCovariates, 
             GenProbFromCoVars = GenProbFromCoVars,
             ResetFixedVals = ResetFixedVals,
             SetFixedValues = SetFixedValues,
             GetCovStatus = GetCovStatus,
             GetCoefficients = GetCoefficients))
}

PostProcessModel <- function(lmd, modelFuncs, gatherConfIntStats = TRUE)
{
    cvnom <- names(lmd)
    j = 1
    y_xTable <- lmd$model
    fitProbs <- lmd$fitted.values
    numObs <- dim(lmd$model)[1]
    xobs <- lapply(1:numObs, function(j){as.list(y_xTable[j,])})
    GenProbs <-  modelFuncs$GenProbFromCoVars
    correctProbs <- sapply(xobs, GenProbs)
    crctDecision <- as.integer(correctProbs >= 0.5)
    crctModPerf  <- sum(crctDecision == y_xTable$yVals)
    estiDecision <- as.integer(fitProbs >= 0.5)
    estiModPerf  <- sum(estiDecision == y_xTable$yVals)
    classifCmp <- data.frame(exactModPred = crctModPerf,
                             estiModPred  = estiModPerf)
    if (gatherConfIntStats) {
        suppressMessages(cfi <- data.frame(confint(lmd)))
        cfiSpread <- list()
        for (k in rownames(cfi)){
            for (j in colnames(cfi)){
                cfiSpread[paste0(k, '_', j)] <- cfi[k, j]
            }
        }
        cfi <- cfiSpread
    }
    else cfi <- NULL
    coefList <- as.list(coefficients(lmd))
    cn <- names(coefList)
    cn <- gsub('$', '_X5', cn)
    names(coefList) <- cn
    
    
    modelStats <- list(classifCmp = classifCmp, coefList = coefList, cfi = cfi)
    return(modelStats)
}

PostProcessAllModels <- function(models, modelFuncs, gatherConfIntStats = TRUE){
    numMods <- length(models)
    cfi <- data.frame()
    cfList <- data.frame()
    classPerf <- data.frame()
    for (j in 1:numMods) {
        print(j)
        modStats <- PostProcessModel(models[[j]], modelFuncs,
                                     gatherConfIntStats)
        cfi <- rbind(cfi, modStats$cfi)
        cfList <- rbind(cfList, modStats$coefList)
        classPerf <- rbind(classPerf, modStats$classifCmp)
    }
    cfData <- cbind(cfList, cfi)
    cfData <- cfData[, order(names(cfData))]
    names(cfData) <- gsub('_X5', '', names(cfData))
    return(cbind(classPerf, cfData))
}

CreateSampleSession <- function(cfs = c(1, 3, 2, 4)){
    jRoot <- CreatePowerFunc(0.5, cfs[1])
    jRootL <- list(score = jRoot, cof = -0.5)
    
    jSqr <- CreatePowerFunc(2, cfs[2])
    jSqrL <- list(score = jSqr, cof = -1)
    
    jLin <- CreatePowerFunc(1, cfs[2])
    jLinL <- list(score = jLin, cof = -0.5)
    
    interactData <- list(jRootL, jSqrL)
    
    jSin  <- CreateCosFunc(1, cfs[1])
    jCos  <- CreateSinFunc(1, cfs[2])
    jSin2 <- CreateCosFunc(2, cfs[3])
    jCos2 <- CreateSinFunc(2, cfs[4])
    jExp  <- CreateExpoFunc(-1, 2)
    jCube <- CreatePowerFunc(3, cfs[3])
    
    factorProbs <- c(1,1,3,1)
    factorWts   <- c(0.5, 0.25, 1, 2)
    facName  <- 'TestFactor'
    myFactor <- DefineFactorFunc(facName, 1:2, factorProbs[1:2], factorWts,
                                 NA)#interactData)
    
    farray <- list(jSin, jCos, jSin2, jCos2)#, myFactor)
    simFunc <- RegressorFuncsGen(farray)
    return(simFunc)
    
    
    
}

#Utility function for supporting user defined ones


ConvertStringToFunc <-function(funcAsString, paramsAsString){
    eval(parse(text=paramsAsString))
    f <- eval(parse(text = funcAsString))
    return(f)
}
