versionCore <- function()
{
    tmp <- .C("versionCore",
            libVersion = paste(rep(".",times=256),collapse=""),
            PACKAGE="CORElearn"
    )
    tmp$libVersion
}
destroyModels <-function(model=NULL)
{
    if (is.null(model)) {
        destroyCore()
        initCore()
    }
    else {
        modelID <- model$modelID
        tmp <- .C("destroyOneCoreModel",
                as.integer(modelID),
                PACKAGE="CORElearn"
        )
    }
    invisible(NULL) 
}
helpCore <- function(name=NULL) {
    #optData <- optionData()  
    optdat <- optionData()$All
    if (is.null(name)) {
        for (opt in optdat[,"name"])
            printOptLine(opt) 
    }
    else {
        name <- match.arg(name, optdat[,"name"])
        printOptLine(name) 
    }
}
CoreModel <- function(formula, data, model=c("rf","rfNear","tree","knn","knnKernel","bayes","regTree"), ..., costMatrix=NULL)
{
    if (!inherits(formula,"formula")) stop("First argument must be a formula.");
    model <- match.arg(model) ; 
    isRegression <- model == "regTree"
    if (isRegression && !is.null(costMatrix))
        warning("For regression problems parameter costMatrix is ignored.");  
    
    dat <- model.frame(formula, data=data, na.action=na.pass);
    if (!isRegression && !inherits(dat[[1]],"factor")) {
        dat[[1]] <- factor(dat[[1]]);
        cat("Changing dependent variable to factor with levels:",levels(dat[[1]]),"\n");
    }
    class.lev <- levels(dat[[1]]);
    noClasses <- length(class.lev);
    if (!isRegression && is.null(costMatrix)) {
        ## create and fill uniform costs matrix
        costMatrix <- 1 - diag(noClasses);
    }
    # get formula explicitly to allow storage of all terms and their manipulation
    trms <- attr(dat,"terms")
    attributes(trms) <- NULL
    formulaExpanded <- as.formula(trms)
    
    aux <- prepare.Data(dat, formulaExpanded, dependent=TRUE,skipNAcolumn=TRUE,skipEqualColumn=TRUE);
    discnumvalues <- aux$discnumvalues;
    discdata <- aux$discdata;
    numdata <- aux$numdata;
    terms <- attr(dat,"terms");
    attr(terms,"intercept") <- NULL; 
    discAttrNames <- dimnames(discdata)[[2]]
    discValCompressed <- aux$disccharvalues
    discValues <- aux$discValues
    numAttrNames <- dimnames(numdata)[[2]]
    skipNames <- dimnames(aux$skipmap)
    discmap <- aux$discmap;
    nummap <- aux$nummap;    
    skipmap <- aux$skipmap
    options <- prepare.Options(...);
    checkOptionsValues(options) ;
    optRemain <- checkModelOptions(model, options) ;
    if (length(optRemain) > 0) warning("Unused options:", paste(names(optRemain), collapse=", "));
    options <- convert.Options(options)
    options <- c(options, action=model)
    tmp <- .C("buildCoreModel",
            noInst = aux$noInst,
            noDiscrete = ncol(discdata),
            noDiscreteValues = as.integer(discnumvalues),
            discreteData = as.integer(discdata), # vector of length noInst*noDiscrete, column-wise
            noNumeric = ncol(numdata),
            numericData = as.double(numdata), # vector of length noInst*noNumeric, column-wise
            costs = as.double(costMatrix),
            discAttrNames = discAttrNames,
            discValNames = discValCompressed,
            numAttrNames = numAttrNames,
            numOptions = length(options),
            optionsName = names(options),
            optionsVal = options,
            modelID = integer(1),
            noClasses = integer(1),
            priorClassProb = numeric(256),
            avgTrainPrediction=numeric(1),
            NAOK=TRUE,
            PACKAGE="CORElearn"
    );
    if (tmp$modelID == -1) {
        return(NULL)
    }
    res <- list(modelID=tmp$modelID, terms=terms, class.lev=class.lev, model=model, formula=aux$formulaOut,
            noClasses = tmp$noClasses, priorClassProb = tmp$priorClassProb[1:tmp$noClasses],avgTrainPrediction = tmp$avgTrainPrediction,
            noNumeric = tmp$noNumeric, noDiscrete=tmp$noDiscrete, discAttrNames = discAttrNames,
            discValNames = discValues, numAttrNames = numAttrNames,   
            discmap = discmap, nummap = nummap, skipmap = skipmap
    )
    class(res) <- "CoreModel"
    res
}
predict.CoreModel <- function(object, newdata, ..., costMatrix=NULL,  type=c("both","class","probability"))
{
    model <- object
    rm(object)
    type<-match.arg(type)
    modelID <- model$modelID;
    isRegression <- model$model == "regTree"
    class.lev <- model$class.lev;
    noClasses <- length(class.lev);
    #terms <- delete.response(model$terms);
    dat <- model.frame(model$formula, data=newdata, na.action=na.pass);
    aux <- prepare.Data(dat[,-1], model$formula, dependent=FALSE,class.lev, skipNAcolumn=FALSE, skipEqualColumn=FALSE);
    noInst <- aux$noInst
    discnumvalues <- aux$discnumvalues;
    discdata <- aux$discdata;
    numdata <- aux$numdata;
    if (!isRegression && is.null(costMatrix)) 
        costMatrix <- 1 - diag(noClasses); ## create and fill uniform costs matrix
    options <- prepare.Options(...);
    checkOptionsValues(options) ;
    optRemain <- checkPredictOptions(model, options)
    if (length(optRemain) > 0) warning("Unused options:", paste(names(optRemain), collapse=", "));       
    
    tmp <- .C("predictWithCoreModel",
            as.integer(modelID),
            noInst = noInst,
            discreteData = as.integer(discdata), # vector of length noInst*noDiscrete, columnwise
            numericData = as.double(numdata), # vector of length noInst*noNumeric, columnwise
            costs = as.double(costMatrix),
            predicted = integer(noInst),
            prob = double(noInst*noClasses),
            predictedReg = double(noInst),
            numOptions = length(options),
            optionsName = names(options),
            optionsVal = options,
            NAOK=TRUE,
            PACKAGE="CORElearn"
    );
    if (model$model == "regTree") {
        returnList <- tmp$predictedReg
    }
    else {
        code <- tmp$predicted;
        code[code==0] <- NA;
        pred <- factor(class.lev[code],levels=class.lev);
        prob <- matrix(tmp$prob, nrow=noInst, ncol=noClasses);
        if (type == "both")
            returnList <- list(class=pred,probabilities=prob)
        else if (type=="class")
            returnList <- pred
        else if (type == "probability")
            returnList <- prob
    }
    returnList
}
attrEval <- function(formula, data, costMatrix = NULL, estimator, ...)
{
    ## find the type of estimator
    isRegression <- FALSE ;
    estDsc <- infoCore(what="attrEval");
    estIndex <- match(estimator, estDsc, nomatch=-1);
    if (estIndex == -1) {
        estDscReg <- infoCore(what="attrEvalReg");
        estIndex <- match(estimator, estDscReg, nomatch=-1);
        if (estIndex == -1) 
            stop("Invalid estimator parameter")
        else 
            isRegression = TRUE ;
    }
    
    if (!inherits(formula,"formula")) stop("First argument must be a formula.");
    dat <- model.frame(formula, data=data, na.action=na.pass);
    if (!isRegression && !inherits(dat[[1]],"factor")) {
        dat[[1]] <- factor(dat[[1]]);
        cat("Changing dependent variable to factor with levels:",levels(dat[[1]]),"\n");
    }
    class.lev <- levels(dat[[1]]);
    noClasses <- length(class.lev)
    if (!isRegression && is.null(costMatrix)) {
        ## create and fill uniform costs matrix
        costMatrix <- 1 - diag(noClasses);
    }
    # get formula explicitly to allow storage of all terms and their manipulation
    trms <- attr(dat,"terms")
    attributes(trms) <- NULL
    formulaExpanded <- as.formula(trms)
    
    aux <- prepare.Data(dat,formulaExpanded,dependent=TRUE,skipNAcolumn=TRUE,skipEqualColumn=TRUE);
    discnumvalues <- aux$discnumvalues;
    discdata <- aux$discdata;
    discmap <- aux$discmap;
    numdata <- aux$numdata;
    nummap <- aux$nummap;
    skipmap<-aux$skipmap
    discAttrNames <- dimnames(discdata)[[2]]
    discValCompressed <- aux$disccharvalues
    discValues <- aux$discValues
    numAttrNames <- dimnames(numdata)[[2]]    
    options <- prepare.Options(...);
    checkOptionsValues(options) ;
    optRemain <- checkEstimatorOptions(estimator, options, isRegression) ;
    if (length(optRemain) > 0) warning("Unused options:", paste(names(optRemain), collapse=", "));
    if (isRegression) {
        tmp <- .C("estimateCoreReg",
                noInst= aux$noInst,
                noDiscrete = ncol(discdata),
                noDiscreteValues = as.integer(discnumvalues),
                discreteData = as.integer(discdata), # vector of length noInst*noDiscrete, columnwise
                noNumeric = ncol(numdata),
                numericData = as.double(numdata), # vector of length noInst*noNumeric, columnwise
                discAttrNames = discAttrNames,
                discValNames = discValCompressed,
                numAttrNames = numAttrNames,
                numOptions = length(options),
                optionsName = names(options),
                optionsVal = options,
                selEst = estIndex,
                estDisc = double(ncol(discdata)),
                estNum = double(ncol(numdata)),
                NAOK=TRUE,
                PACKAGE="CORElearn"
        );
        # assumes length(estNum) == noNumeric, but estNum[1] for predictor is not used
        if (nummap[1] != 1) stop("no dependent variable in prepared regression data"); 
    }
    else {
        tmp <- .C("estimateCore",
                noInst = aux$noInst,
                noDiscrete = ncol(discdata),
                noDiscreteValues = as.integer(discnumvalues),
                discreteData = as.integer(discdata), # vector of length noInst*noDiscrete, columnwise
                noNumeric = ncol(numdata),
                numericData = as.double(numdata), # vector of length noInst*noNumeric, columnwise
                costs = as.double(costMatrix),
                discAttrNames = discAttrNames,
                discValNames = discValCompressed,
                numAttrNames = numAttrNames,            
                numOptions = length(options),
                optionsName = names(options),
                optionsVal = options,
                selEst = estIndex,
                estDisc = double(ncol(discdata)),
                estNum = double(ncol(numdata)),
                NAOK=TRUE,
                PACKAGE="CORElearn"
        );
        # assumes length(estDisc) == noDiscrete, but estDist[1] for class is not used
        if (discmap[1] != 1) stop("no class in prepared data"); # for debugging only
    }
    est <- double(length(discmap) + length(nummap)+length(skipmap));
    est[discmap] <- tmp$estDisc;
    est[nummap] <- tmp$estNum;
    names(est)[discmap] <- discAttrNames
    names(est)[nummap] <- numAttrNames   
    est[-c(1,skipmap)];
}
rfAttrEval <- function(model) {
    if (model$model != "rf") stop("Only random forest model can evaluate attributes with this function.");
    modelID <- model$modelID
    tmp <- .C("rfAttrEval",
            modelID = as.integer(modelID),
            est = double(model$noDiscrete+model$noNumeric),    
            PACKAGE="CORElearn"
    )
    est <- double(length(model$discmap) + length(model$nummap)+length(model$skipmap));
    est[model$discmap] <- tmp$est[1:length(model$discmap)];
    est[model$nummap] <- tmp$est[(length(model$discmap)+1) : (length(model$discmap) + length(model$nummap)) ];
    names(est)[model$discmap] <- model$discAttrNames
    names(est)[model$nummap] <- model$numAttrNames   
    est[-c(1,model$skipmap)];
}

ordEval <- function(formula, data, file=NULL, rndFile=NULL, variant=c("allNear","attrDist1","classDist1"), ...)
{
    if (!inherits(formula,"formula")) stop("First argument must be a formula.");
    variant <- match.arg(variant)
    variantIdx=match(variant,eval(formals()$variant),nomatch=-1)
    dat <- model.frame(formula, data=data, na.action=na.pass);
    if (!inherits(dat[[1]],"factor")) {
        dat[[1]] <- factor(dat[[1]]);
    }
    class.lev <- levels(dat[[1]]);
    # get formula explicitly to allow storage of all terms and their manipulation
    trms <- attr(dat,"terms")
    attributes(trms) <- NULL
    formulaExpanded <- as.formula(trms)    
    aux <- prepare.Data(dat, formulaExpanded,dependent=TRUE, discreteOrdered=TRUE,skipNAcolumn=TRUE,skipEqualColumn=TRUE);
    discnumvalues <- aux$discnumvalues;
    discdata <- aux$discdata;
    discmap <- aux$discmap;
    discAttrNames <- dimnames(discdata)[[2]]
    discValNames <- aux$disccharvalues
    options <- prepare.Options(...);
    checkOptionsValues(options) ;
    optRemain <- checkOrdEvalOptions(options)   
    if (length(optRemain) > 0) warning("Unused options:", paste(names(optRemain), collapse=", "));
    noAttr <- ncol(discdata) - 1
    maxAttrValues <- max(discnumvalues[-1])+1	
    statNames<-getStatNames() ;
    noStats <- length(statNames)  ## we get 8 statistics about random normalizers
    tmp <- .C("ordEvalCore",
            noInst =  aux$noInst,
            noDiscrete = ncol(discdata),
            noDiscreteValues = as.integer(discnumvalues),
            discreteData = as.integer(discdata), # vector of length noInst*noDiscrete, columnwise
            discAttrNames = discAttrNames,
            discValNames = discValNames,
            numOptions = length(options),
            optionsName = names(options),
            optionsVal = options,
            reinfPos = double(noAttr * maxAttrValues),
            reinfNeg = double(noAttr * maxAttrValues),
            anchor = double(noAttr * maxAttrValues),
            rndReinfPos = double(noAttr * maxAttrValues * noStats),
            rndReinfNeg = double(noAttr * maxAttrValues * noStats),
            rndAnchor = double(noAttr * maxAttrValues * noStats),
            noAV = integer(noAttr * maxAttrValues),
            file = as.character(file),
            rndFile = as.character(rndFile),
            variant = as.integer(variantIdx),
            NAOK=TRUE,
            PACKAGE="CORElearn"
    )
    attrNames <- names(dat)[-1]
    attrMap <- (discmap[-1]) - 1
    avNames <- c(1:(maxAttrValues-1),"all")
    avMap <- 1:(maxAttrValues-1)
    reinfPos <- matrix(tmp$reinfPos, nrow=noAttr, ncol=maxAttrValues,dimnames=list(attrNames,avNames));
    reinfNeg <- matrix(tmp$reinfNeg, nrow=noAttr, ncol=maxAttrValues,dimnames=list(attrNames,avNames));
    anchor <- matrix(tmp$anchor, nrow=noAttr, ncol=maxAttrValues,dimnames=list(attrNames,avNames));
    noAV <- matrix(tmp$noAV, nrow=noAttr, ncol=maxAttrValues,dimnames=list(attrNames,avNames));
    rndReinfPos <- array(tmp$rndReinfPos, dim=c(noAttr, maxAttrValues,noStats), dimnames=list(attrNames, avNames, statNames));
    rndReinfNeg <- array(tmp$rndReinfNeg, dim=c(noAttr, maxAttrValues,noStats), dimnames=list(attrNames, avNames, statNames)) ;
    rndAnchor <- array(tmp$rndAnchor, dim=c(noAttr, maxAttrValues,noStats), dimnames=list(attrNames, avNames, statNames));
    res<-list(reinfPosAV=reinfPos[attrMap,avMap], reinfNegAV=reinfNeg[attrMap,avMap], anchorAV=anchor[attrMap,avMap], noAV = noAV[attrMap,avMap],
            reinfPosAttr=reinfPos[attrMap,maxAttrValues], reinfNegAttr=reinfNeg[attrMap,maxAttrValues], anchorAttr=anchor[attrMap,maxAttrValues],
            noAVattr = noAV[attrMap,maxAttrValues], 
            rndReinfPosAV=rndReinfPos[attrMap,avMap,], rndReinfNegAV=rndReinfNeg[attrMap,avMap,], rndAnchorAV=rndAnchor[attrMap,avMap,],
            rndReinfPosAttr=rndReinfPos[attrMap,maxAttrValues,], rndReinfNegAttr=rndReinfNeg[attrMap,maxAttrValues,], rndAnchorAttr=rndAnchor[attrMap,maxAttrValues,],
            attrNames= attrNames, valueNames=aux$discValues[-1], noAttr=length(attrNames),ordVal=maxAttrValues-1,variant=variant,file=file, rndFile=rndFile,
            formula=aux$formulaOut
    );
    class(res) <- "ordEval"  
    return(res)
}
plotInstEval<-function(oeInstFile, oeInstRndFile,  noAttr, ...) {
    inst<-read.table(oeInstFile,header=FALSE,sep=",",colClasses="character",strip.white=TRUE,na.strings=c("NA","?"))
    instNorm<-read.table(oeInstRndFile,header=FALSE,sep=",",colClasses="character",strip.white=TRUE,na.strings=c("NA","?"))
    #noAttr <- length(ordEvalData$noAVattr)
    #ordVal <- ncol(ordEvalData$reinfPosAV)
    statNames<-getStatNames() ;
    noStats <- length(statNames)
    ord<-list()
    noInst <- nrow(inst)/(noAttr+1)
    for (i in 1:noInst) {
        className <- as.character(trimSpaces(inst[(i-1)*(noAttr+1)+1, 1]))
        classValue <- as.character(trimSpaces(inst[(i-1)*(noAttr+1)+1, 2]))
        attrName <- as.character(trimSpaces(inst[((i-1)*(noAttr+1)+2):(i*(noAttr+1)), 1]))
        valueName <- as.character(trimSpaces(inst[((i-1)*(noAttr+1)+2):(i*(noAttr+1)), 2]))
        reinfPos <- as.numeric(inst[((i-1)*(noAttr+1)+2):(i*(noAttr+1)), 3])
        reinfNeg <- as.numeric(inst[((i-1)*(noAttr+1)+2):(i*(noAttr+1)), 4])
        anchor <- as.numeric(inst[((i-1)*(noAttr+1)+2):(i*(noAttr+1)), 5])
        rndReinfPos<-list()
        rndReinfNeg<-list()
        rndAnchor<-list()
        for (iA in 1:noAttr){
            rndReinfPos[[iA]]<-as.numeric(instNorm[(i-1)*(noAttr+1)+1+iA,2:(1+noStats)])
            rndReinfNeg[[iA]]<-as.numeric(instNorm[(i-1)*(noAttr+1)+1+iA,(2+noStats):(1+2*noStats)])
            rndAnchor[[iA]]<-as.numeric(instNorm[(i-1)*(noAttr+1)+1+iA,(2+2*noStats):(1+3*noStats)])
        }
        ord[[i]]<-list(className=className,classValue=classValue,attributeName=attrName,valueName=valueName,reinfPos=reinfPos,reinfNeg=reinfNeg,anchor=anchor,
                rndReinfPos=rndReinfPos,rndReinfNeg=rndReinfNeg,rndAnchor=rndAnchor)
    } 
    oeInst(ord, noAttr, ...)
}
plotOrdEval<-function(file, rndFile=NULL, ...){
    # read data from files and transform the two tables to internal object as returned by ordEval
    ord<-read.table(file,header=TRUE,sep=",",strip.white=TRUE)
    if (!is.null(rndFile))
        ordNorm<-read.table(rndFile,header=TRUE,sep=",",strip.white=TRUE)
    ## extract number of attributes and values from first column
    name <- ord[,1]
    dup <- duplicated(name)
    for (i in 2:length(dup))
        if (dup[i])
            break ;
    ordVal <- i-3
    noAttr <- length(unique(name)) - ordVal
    statNames<-getStatNames()
    noStats <- length(statNames)  ## we get 8 statistics about random normalizers
    attrNames <- c()
    avNames <- c(1:ordVal)
    for (iA in 1:noAttr) {
        attrNames[iA] <- as.character(ord[(iA-1)*(ordVal+1)+1,1]) 
    }
    reinfPosAV <- matrix(0, nrow=noAttr, ncol=ordVal,dimnames=list(attrNames,avNames));
    reinfNegAV <- matrix(0, nrow=noAttr, ncol=ordVal,dimnames=list(attrNames,avNames));
    anchorAV <- matrix(0, nrow=noAttr, ncol=ordVal,dimnames=list(attrNames,avNames));
    noAV <- matrix(0, nrow=noAttr, ncol=ordVal,dimnames=list(attrNames,avNames));
    reinfPosAttr <- array(0,dim=c(noAttr),dimnames=list(attrNames));
    reinfNegAttr <- array(0, dim=c(noAttr),dimnames=list(attrNames));
    anchorAttr <- array(0, dim=c(noAttr),dimnames=list(attrNames));
    noAVattr <- array(0, dim=c(noAttr),dimnames=list(attrNames));
    rndReinfPosAV <- array(0, dim=c(noAttr, ordVal,noStats), dimnames=list(attrNames, avNames, statNames));
    rndReinfNegAV <- array(0, dim=c(noAttr, ordVal,noStats), dimnames=list(attrNames, avNames, statNames)) ;
    rndAnchorAV <- array(0, dim=c(noAttr, ordVal,noStats), dimnames=list(attrNames, avNames, statNames));
    rndReinfPosAttr <- array(0, dim=c(noAttr, noStats), dimnames=list(attrNames,  statNames));
    rndReinfNegAttr <- array(0, dim=c(noAttr, noStats), dimnames=list(attrNames,  statNames)) ;
    rndAnchorAttr <- array(0, dim=c(noAttr,noStats), dimnames=list(attrNames,  statNames));
    valueNames<-list()
    for (iA in 1:noAttr) {
        #attrNames[iA] <- ord[(iA-1)*(ordVal+1)+1,1]
        valueNames[[iA]] <- as.character(ord[(2+(iA-1)*(ordVal+1)):(iA*(ordVal+1)),1])
        noAV[iA,] <- ord[(2+(iA-1)*(ordVal+1)):(iA*(ordVal+1)),5]       
        for(i in 1:ordVal) {
            reinfPosAV[iA,i]  <- ord[(iA - 1) * (ordVal + 1) + i + 1, 2]
            reinfNegAV[iA,i] <- ord[(iA - 1) * (ordVal + 1) + i + 1, 3]
            anchorAV[iA,i]   <- ord[(iA - 1) * (ordVal + 1) + i + 1, 4]
            
            if (!is.null(rndFile)) {
                rndReinfPosAV[iA,i,] <- as.numeric(ordNorm[(iA - 1) * (ordVal + 1) + i + 1, 2:(noStats+1)])
                rndReinfNegAV[iA,i,]  <- as.numeric(ordNorm[(iA - 1) * (ordVal + 1) + i + 1, (2+noStats):(1+2*noStats)])
                rndAnchorAV[iA,i,]      <- as.numeric(ordNorm[(iA - 1) * (ordVal + 1) + i + 1, (2+2*noStats):(1+3*noStats)])
            }
        }
        i <- 0
        
        noAVattr[iA] <- ord[(1+(iA-1)*(ordVal+1)),5]               
        
        reinfPosAttr[iA]  <- ord[(iA - 1) * (ordVal + 1) + i + 1, 2]
        reinfNegAttr[iA] <- ord[(iA - 1) * (ordVal + 1) + i + 1, 3]
        anchorAttr[iA]   <- ord[(iA - 1) * (ordVal + 1) + i + 1, 4]
        
        if (!is.null(rndFile)) {
            rndReinfPosAttr[iA,] <- as.numeric(ordNorm[(iA - 1) * (ordVal + 1) + i + 1, 2:(noStats+1)])
            rndReinfNegAttr[iA,]  <- as.numeric(ordNorm[(iA - 1) * (ordVal + 1) + i + 1, (2+noStats):(1+2*noStats)])
            rndAnchorAttr[iA,]      <- as.numeric(ordNorm[(iA - 1) * (ordVal + 1) + i + 1, (2+2*noStats):(1+3*noStats)])
        }
    }
    oeObj <- list(reinfPosAV=reinfPosAV, reinfNegAV=reinfNegAV, anchorAV=anchorAV, noAV = noAV,
            reinfPosAttr=reinfPosAttr, reinfNegAttr=reinfNegAttr, anchorAttr=anchorAttr, noAVattr = noAVattr,
            rndReinfPosAV=rndReinfPosAV, rndReinfNegAV=rndReinfNegAV, rndAnchorAV=rndAnchorAV,
            rndReinfPosAttr=rndReinfPosAttr, rndReinfNegAttr=rndReinfNegAttr, rndAnchorAttr=rndAnchorAttr,
            attrNames= attrNames, valueNames=valueNames, noAttr=length(attrNames),ordVal=ordVal,variant=NULL,file=file, rndFile=rndFile      
    ) 
    class(oeObj) <- "ordEval"
    plot(oeObj,...)  ## call of plot.ordEval
}

plot.ordEval<-function(x, graphType=c("avBar", "attrBar", "avSlope"), ...) {
    
    graphType<-match.arg(graphType)
    
    if (graphType=="avSlope")
        avVisObject(x,  ...)
    else if (graphType=="avBar" )
        avNormBarObject(x, ...)
    else if (graphType=="attrBar")
        attrNormBarObject(x, ...)
    invisible(x)
}

printOrdEval<-function(x) {
    object <- x
    maxAttrChars <- max(nchar(c(object$attrNames,"Attribute")))
    maxAVChars <- max(nchar(c(unlist(object$valueNames),"Value")))
    header <- paste(sprintf("%*s %*s",maxAttrChars,"Attribute",maxAVChars,"Value"),
            sprintf("%6s %6s  %6s %6s  %6s %6s","Up","p-val","Down","p-val","Anchor","p-val"),sep="  ")
    cat(header,"\n")
    for (a in 1:object$noAttr) {
        line <- paste(sprintf("%*s %*s",maxAttrChars,object$attrNames[a],maxAVChars,"all"),
                sprintf("%6.4f %6.4f  %6.4f %6.4f  %6.4f %6.4f", object$reinfPosAttr[a],object$rndReinfPosAttr[a,"p-value"],object$reinfNegAttr[a],object$rndReinfNegAttr[a,"p-value"],object$anchorAttr[a],object$rndAnchorAttr[a,"p-value"]),
                sep="  ")
        cat(line,"\n")
        
        for (v in 1:object$ordVal){
            line <- paste(sprintf("%*s %*s",maxAttrChars," ",maxAVChars,object$valueNames[[a]][v]),
                    sprintf("%6.4f %6.4f  %6.4f %6.4f  %6.4f %6.4f", object$reinfPosAV[a,v],object$rndReinfPosAV[a,v,"p-value"],object$reinfNegAV[a,v],object$rndReinfNegAV[a,v,"p-value"],object$anchorAV[a,v],object$rndAnchorAV[a,v,"p-value"]),
                    sep="  ")
            cat(line,"\n")
        }
    }
}


modelEval <- function(model, correctClass, predictedClass, predictedProb=NULL, costMatrix=NULL, priorClProb = NULL, avgTrainPrediction = NULL, beta=1) {
    if (is.null(model)) {
        if (!is.null(avgTrainPrediction)) {
            return(modelEvaluationReg.Core(correctClass,predictedClass,avgTrainPrediction))
        }
        else { 
            return(modelEvaluationClass.Core(correctClass,predictedClass,predictedProb,costMatrix,priorClProb,beta))
        }
    } 
    if (model$model == "regTree") {
        if (is.null(avgTrainPrediction))
            avgTrainPrediction <- model$avgTrainPrediction
        return(modelEvaluationReg.Core(correctClass,predictedClass,model$avgTrainPrediction))
    }
    else {
        if (is.null(priorClProb))
            priorClProb <- model$priorClassProb
        return(modelEvaluationClass.Core(correctClass,predictedClass,predictedProb,costMatrix,priorClProb,beta))
    }
}   
modelEvaluationClass.Core <- function(correctClass, predictedClass, predictedProb=NULL, costMatrix=NULL, priorClassProb=NULL, beta=1) {
    # some data validity checks
    if (!inherits(correctClass,"factor")) {
        correctClass<-factor(correctClass)
    }
    if (length(correctClass[is.na(correctClass)])>0)
         stop("Correct class should not contain NA values.")
    noClasses <- length(levels(correctClass)) ;
    if (!inherits(predictedClass,"factor")) {
        predictedClass<-factor(predictedClass)
    }
    if (length(predictedClass[is.na(predictedClass)])>0)
        stop("Predicted class should not contain NA values.")
    noInst <- length(correctClass) ;
    if (is.null(predictedProb)){
        ## create and fill the prediction matrix
        predictedProb <- matrix(0, nrow=noInst, ncol=noClasses)
        for (i in 1:noInst)
            predictedProb[i, predictedClass[i]] <- 1.0
    }
    if (is.null(costMatrix)) {
        ## create and fill uniform costs matrix
        costMatrix <- 1 - diag(noClasses);
    }
    if (is.null(priorClassProb))
        priorClassProb <- table(correctClass)/noInst ;
    
    tmp <- .C("modelEvaluate",
            noInst = length(correctClass),
            correctClass = as.integer(correctClass),
            predictedClass = as.integer(predictedClass),
            predictedProb = as.double(predictedProb),
            costMatrix = as.double(costMatrix),
            noClasses = as.integer(noClasses), 
            priorClassProb = as.double(priorClassProb),
            accuracy = double(1),
            avgCost = double(1),
            infScore = double(1),
            auc = double(1),
            predMatrix = integer(noClasses * noClasses),
            sensitivity = double(1),
            specificity = double(1),
            brier = double(1),
            kappa = double(1), 
            precision = double(1),
            Gmean = double(1),             
            NAOK=TRUE,
            PACKAGE="CORElearn"
    )
    recall = tmp$sensitivity
    denominator <- (beta*beta * recall + tmp$precision)
    if (denominator == 0)
        Fmeasure <- 0
    else
        Fmeasure = (1+beta*beta)*recall*tmp$precision / denominator
    predMx <- matrix(tmp$predMatrix, nrow = noClasses, ncol=noClasses, dimnames = list(levels(correctClass),levels(correctClass)))
    list(accuracy = tmp$accuracy, averageCost = tmp$avgCost, informationScore = tmp$infScore,
            AUC = tmp$auc, predictionMatrix = predMx, sensitivity = tmp$sensitivity,
            specificity = tmp$specificity, brierScore = tmp$brier, kappa = tmp$kappa,
            precision = tmp$precision, recall = tmp$sensitivity, Fmeasure = Fmeasure, Gmean = tmp$Gmean)
}
modelEvaluationReg.Core <- function(correct, predicted, avgTrainPredicted) {
    noInst <- length(correct) ;
    tmp <- .C("modelEvaluateReg",
            noInst = length(correct),
            correct = as.double(correct),
            predicted = as.double(predicted),
            avgPredicted = as.double(avgTrainPredicted), 
            MSE = double(1),
            RMSE = double(1),
            MAE = double(1),
            RMAE = double(1),
            NAOK=TRUE,
            PACKAGE="CORElearn"
    )
    list(MSE = tmp$MSE, RMSE = tmp$RMSE, MAE = tmp$MAE, RMAE = tmp$RMAE)
}
paramCoreIO <- function(model, fileName, io=c("read","write")) {
    io = match.arg(io)
    tmp <- .C("optionsInOut",
            modelID = as.integer(model$modelID),
            fileName=as.character(fileName),
            io=as.character(io),
            NAOK=FALSE,
            PACKAGE="CORElearn"
    )
    invisible(tmp)
}
saveRF <- function(model, fileName) {
    if (model$model != "rf") stop("Only random forest model can be saved at the moment.");
    modelID <- model$modelID
    tmp <- .C("saveRF",
            modelID = as.integer(modelID),
            fileName=as.character(fileName),
            PACKAGE="CORElearn"
    )
    invisible(tmp)
}
loadRF <- function(formula, data, fileName) {
    model="rf"
    if (!inherits(formula,"formula")) stop("First argument must be a formula.");
    dat <- model.frame(formula, data=data, na.action=na.pass);
    if (!inherits(dat[[1]],"factor")) {
        dat[[1]] <- factor(dat[[1]]);
        cat("Changing dependent variable to factor with levels:",levels(dat[[1]]),"\n");
    }
    class.lev <- levels(dat[[1]]);
    noClasses <- length(class.lev);
    terms <- attr(dat,"terms");
    attr(terms,"intercept") <- NULL; 
    
    tmp <- .C("readRF",
            fileName=as.character(fileName),
            modelID = integer(1),
            PACKAGE="CORElearn"
    );
    if (tmp$modelID == -1) {
        return(NULL)
    }
    res <- list(modelID=tmp$modelID, terms=terms, class.lev=class.lev, model=model, formula=formula)
    class(res) <- "CoreModel"
    res
}
getRFsizes <- function(model) {
    if (model$model != "rf") stop("The model must be a random forest.");
    .Call("exportSizesRF", as.integer(model$modelID), PACKAGE="CORElearn")
}   
getCoreModel <- function(model) {
    if (model$model != "rf") stop("The model must be a random forest.");
    .Call("exportModel", as.integer(model$modelID), PACKAGE="CORElearn")    
}

calibrate <- function(correctClass, predictedProb, class1=1, method = c("isoReg","binIsoReg","binning","chiMerge"), weight=NULL,noBins=10){
    noClasses <- length(levels(correctClass)) ;
    method <- match.arg(method)
    methodIdx = match(method, eval(formals()$method), nomatch=-1)
    noInst <- length(correctClass) ;
    if (is.null(weight)) {
        weight<-numeric(noInst)
        weight[]<-1
    }
    # class1 can be either class name (factor) or its index
    if (is.factor(class1))
        class1idx<-match(class1, levels(correctClass),nomatch=-1)
    else {
        class1idx<-class1
        class1<-factor(levels(correctClass)[class1idx],levels=levels(correctClass))       
    }
    # convert true class to a vector of 0 and 1
    tc<-integer(length(correctClass))
    tc[]<-0
    tc[correctClass==class1]<-1
    
    tmp <- .C("calibrate",
            methodIdx = as.integer(methodIdx),
            noInst = as.integer(noInst),
            correctClass = as.integer(tc),
            predictedProb = as.double(predictedProb),
            weight=as.double(weight),
            noBins=as.integer(noBins),
            noIntervals = integer(1),
            interval = double(noInst),
            calProb = double(noInst),
            NAOK=TRUE,
            PACKAGE="CORElearn"
    )
    list(interval = tmp$interval[1:tmp$noIntervals], calProb = tmp$calProb[1:tmp$noIntervals])
}

