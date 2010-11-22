prepare.Data <- function(data, formulaIn, dependent, class.lev=NULL, discreteOrdered=FALSE, skipNAcolumn=TRUE, skipEqualColumn=TRUE)
{
    if (dependent) { # shell we fill in the column with the dependent data 
		if (inherits(data[[1]],"ordered")) {
			data[[1]] <- factor(data[[1]],ordered=FALSE); # protect against transformation to numeric
			cat("Changing dependent variable to unordered factor.\n");
		}
        if (length(data[[1]][is.na(data[[1]])])>0)
            warning("Instances with dependent variable equal to NA are left out.")
		data <- data[!is.na(data[[1]]),]
	} 
    else {
        if (is.null(class.lev))  {## regression
           predictionColumn <- double(length=nrow(data))
           predictionColumn[] <- NA
           data <- cbind(prediction=predictionColumn,data)
       }
       else {
           predictionColumn <- double(length=nrow(data))
           predictionColumn[] <- NA           
		   data <- cbind(prediction=factor(predictionColumn,levels=class.lev),data);
       }
	}
    col.names <- names(data)
	discnumvalues <- integer(0);
	disccharvalues <- c();
    discValues <- list()
	discdata <- matrix(nrow=nrow(data),ncol=0);
	numdata <- matrix(nrow=nrow(data),ncol=0);
	discmap <- integer(0);
	nummap <- integer(0);
    skipmap <- integer(0)
	for (i in seq(along=data)) {
        # check validity of columns
        if (length(data[[i]][is.na(data[[i]])])==nrow(data)) {
            if (i > 1) {
                if (skipNAcolumn) {
                  skipmap <- c(skipmap,i) 
                  warning(sprintf("Variable %s has all values equal to NA and has been skipped.",names(data)[i]))  
                  formulaIn <- update.formula(formulaIn, paste(". ~ . - ",names(data)[i],sep=""))
                  next
                }
                else {
                    warning(sprintf("Variable %s has all values equal to NA.",names(data)[i]))                  
                }
          }
        }
        else {
          sc <- sort(data[[i]],na.last=NA)
          if (nrow(data) > 1 && (length(sc) <= 1 || sc[1]==sc[length(sc)]) ) { # all equal
               if (skipEqualColumn) {
                  skipmap <- c(skipmap,i)  
                  warning(sprintf("Variable %s has all values equal to NA and has beeen skipped.",names(data)[i]))
                  formulaIn <- update.formula(formulaIn, paste(". ~ . - ",names(data)[i],sep=""))
                  next
              }
              else {
                  warning(sprintf("Variable %s has all values equal to NA.",names(data)[i]))
              }
          }
        }
		if (inherits(data[[i]],"character") || (!inherits(data[[i]],"factor") && discreteOrdered==TRUE)) {
			data[[i]] <- factor(data[[i]]);
		}
		if (inherits(data[[i]],"factor") && (discreteOrdered==TRUE || !inherits(data[[i]],"ordered"))) {
			column <- as.integer(data[[i]]);
			column[is.na(column)] <- as.integer(0);
			column <- matrix(column,ncol=1,dimnames=list(NULL,col.names[i]))
			discnumvalues <- c(discnumvalues,length(levels(data[[i]])));
			disccharvalues <- c(disccharvalues,paste(levels(data[[i]]),collapse="\x1F"));
            discdata <- cbind(discdata,column);
			discmap <- c(discmap,i);
            discValues[[length(discmap)]] <- levels(data[[i]])
            
		} else {
			column <- matrix(as.double(data[[i]]),ncol=1,dimnames=list(NULL,col.names[i]))
			numdata <- cbind(numdata,column);
			nummap <- c(nummap,i);
		}
	}
	numdata[is.na(numdata)] <- as.double(NA)
	if (length(discnumvalues) != ncol(discdata)) stop("internal problem 1 in prepare dataa"); # for debugging only
	if (length(discmap) != ncol(discdata)) stop("internal problem 2 in prepare data"); # for debugging only
	if (length(nummap) != ncol(numdata)) stop("internal problem 3 in prepare data"); # for debugging only
    if (nrow(data) != nrow(numdata)) stop("internal problem 4 in prepare data"); # for debugging only
    if (nrow(data) != nrow(discdata)) stop("internal problem 5 in prepare data"); # for debugging only
    #if (ncol(data) != ncol(discdata) + ncol(numdata)) stop("internal problem 4 in prepare data"); # for debugging only
	list(discnumvalues=discnumvalues,disccharvalues=disccharvalues,discdata=discdata,discmap=discmap,
            numdata=numdata,nummap=nummap,discValues=discValues, noInst=nrow(data),skipmap=skipmap, 
            formulaOut=formulaIn);
}
get.formula <- function(class.name)
{
	as.formula(paste(class.name,"~ ."));
}
infoCore<-function(what=c("attrEval","attrEvalReg")) {
    what <- match.arg(what)  
    switch (what,
      attrEval =
        c("ReliefFequalK", "ReliefFexpRank", "ReliefFbestK", "Relief", "InfGain", "GainRatio", "MDL", "Gini",
        "MyopicReliefF", "Accuracy", "ReliefFmerit", "ReliefFdistance", "ReliefFsqrDistance",
        "DKM", "ReliefFexpC", "ReliefFavgC", "ReliefFpe", "ReliefFpa", "ReliefFsmp", "GainRatioCost", "DKMcost",
        "ReliefKukar", "MDLsmp","ImpurityEuclid", "ImpurityHellinger",
        "UniformDKM","UniformGini","UniformInf","UniformAccuracy",
        "EqualDKM", "EqualGini","EqualInf", "EqualHellinger","DistHellinger","DistAUC", "DistAngle", "DistEuclid" 
      ),
      attrEvalReg =  c("RReliefFequalK", "RReliefFexpRank", "RReliefFbestK","RReliefFwithMSE","MSEofMean","MSEofModel","MAEofModel",
          "RReliefFdistance","RReliefFsqrDistance")
    )
}

prepare.Options <- function(...)
{
	optionsList <- list(...);
	for (i in seq(along=optionsList)) {
		if (is.logical(optionsList[[i]])) {
			optionsList[[i]] <- if (optionsList[[i]]) "Y" else "N";
		} else if (is.numeric(optionsList[[i]])) {
			optionsList[[i]] <- as.character(optionsList[[i]]);
		} else if (!is.character(optionsList[[i]])) {
			stop(paste("wrong type of option",names(optionsList)[i],"=",optionsList[[i]]));
		}
	}
	opt <- unlist(optionsList)
    # convert integer values of estimators to their character descriptors
    for (est in c("selectionEstimator","constructionEstimator")) {
      idx = match(est, names(opt),nomatch=-1)
      if (idx > 0) {
        warn.save <- getOption("warn")
        options(warn=-1)
        ival <- as.numeric(opt[idx])
        options(warn=warn.save)
        if (! is.na(ival) ) {
            est = infoCore(what="attrEval")
            opt[idx] <- est[ival]
        }
      }
    }  
    opt                
}
 convert.Options <- function(opt) {
    # convert character description of of estimators to their character descriptors
    for (est in c("selectionEstimator","constructionEstimator")) {
      idx = match(est, names(opt),nomatch=-1)
      if (idx > 0) {
         estIdx = match(opt[idx], infoCore(what="attrEval"), nomatch=-1)       
         opt[idx] <- estIdx
      }
    }
    for (est in c("selectionEstimatorReg","constructionEstimatorReg")) {
      idx = match(est, names(opt),nomatch=-1)
      if (idx > 0) {
         estIdx = match(opt[idx], infoCore(what="attrEvalReg"), nomatch=-1)       
         opt[idx] <- estIdx
      }
    }
    opt
 }
optionData <- function() {
    estDsc <- infoCore(what="attrEval")
    estList <- paste(estDsc,collapse=",")
    estDscReg <- infoCore(what="attrEvalReg")
    estListReg <- paste(estDscReg,collapse=",")
    optAllList <- list(
    ## follows description of all parameters 
    ## the format is list with 5 components, some may be empty "":
    ## 1. name of parameter
    ## 2. parameter type: logical, integer, numeric, character
    ## 3. default value
    ## 4. lower limit
    ## 5. upper limit
    ## Option data follow
    ## \section{Attribute/feature evaluation}
    list("binaryEvaluation", "logical", FALSE, FALSE, TRUE),
    list("binaryEvaluateNumericAttributes", "logical", TRUE, FALSE, TRUE),
    list("multiclassEvaluation", "integer", 1, 1, 4),
    list("attrEvaluationInstances", "integer", 0, 0, Inf),
    list("minNodeWeightEst", "numeric", 2, 0, Inf),
    list("ReliefIterations", "integer", 0, -2, Inf),
    list("numAttrProportionEqual", "numeric", 0.04, 0, 1),
    list("numAttrProportionEqual", "numeric", 0.1, 0, 1),
    list("kNearestEqual", "integer", 10, 0, Inf),
    list("kNearestExpRank", "integer", 70, 0, Inf),
    list("quotientExpRankDistance", "numeric", 20, 0, Inf),
    ## \section{Algorithm ordEval}
    list("ordEvalNoRandomNormalizers", "integer", 0, 0, Inf),
    list("ordEvalBootstrapNormalize", "logical", FALSE, FALSE, TRUE),
    list("ordEvalNormalizingPercentile", "numeric", 0.025, 0, 0.5),
    list("attrWeights", "character", "", "", ""),
    ## \section{Decision/regression tree construction}
    list("selectionEstimator", "character", "MDL", estList, ""),
    list("selectionEstimatorReg", "character", "RReliefFexpRank", estListReg, ""),
    list("minReliefEstimate", "numeric", 0, -1, 1),
    list("minInstanceWeight", "numeric", 0.05, 0, 1),
    ## \section{Stop tree building}
    list("minNodeWeightTree", "numeric", 5, 0, Inf),
    list("minNodeWeightRF", "numeric", 2, 0, Inf),
    list("relMinNodeWeight", "numeric", 0, 0, 1),
    list("majorClassProportion", "numeric", 1, 0, 1),
    list("rootStdDevProportion", "numeric", 0, 0, 1),
    ## \section{Models in the tree leaves}
    list("modelType", "integer", 1, 1, 4),
    list("modelTypeReg", "integer", 1, 1, 8),
    list("kInNN", "integer", 10, 0, Inf),
    list("nnKernelWidth", "numeric", 2, 0, Inf),
    list("bayesDiscretization", "integer", 2, 1, 2),
    list("bayesEqFreqIntervals", "integer", 4, 1, Inf),
    ## \section{Constructive induction aka. feature construction}
    list("constructionMode", "integer", 15, 1, 15),
    list("constructionDepth", "integer", 0, 0, Inf),
    list("noCachedInNode", "integer", 5, 0, Inf),
    list("constructionEstimator", "character", "MDL", estList, ""),
    list("constructionEstimatorReg", "character", "RReliefFexpRank", estListReg, ""),
    list("beamSize", "integer", 20, 1, Inf),
    list("maxConstructSize", "integer", 3, 1, Inf),
    ## \section{Attribute discretization}
    list("discretizationLookahead", "integer", 3, 0, Inf),
    list("discretizationSample", "integer", 50, 0, Inf),
    ## \section{Tree pruning}
    list("selectedPruner", "integer", 1, 0, 1),
    list("selectedPrunerReg", "integer", 2, 0, 4),
    list("mdlModelPrecision", "numeric", 0.1, 0, Inf),
    list("mdlErrorPrecision", "numeric", 0.01, 0, Inf),
    list("mEstPruning", "numeric", 2, 0, Inf),
    list("alphaErrorComplexity", "numeric", 0, 0, Inf),
    ## \section{Prediction}
    list("mEstPrediction", "numeric", 0, 0, Inf),
    ## \section{Random forests}
    list("rfNoTrees", "integer", 100, 1, Inf),
    list("rfNoSelAttr", "integer", 0, -2, Inf),
    list("rfMultipleEst", "logical", FALSE, FALSE, TRUE),
    list("rfkNearestEqual", "integer", 30, 0, Inf),
    list("rfPropWeightedTrees", "numeric", 0, 0, 1),
    list("rfPredictClass", "logical", FALSE, FALSE, TRUE),
    ## \section{General tree ensembles}
    list("rfSampleProp", "numeric", 0, 0, 1),
    list("rfNoTerminals", "integer", 0, 0, Inf),
    list("rfRegType", "integer", 2, 0, 2),
    list("rfRegLambda", "numeric", 0, 0, Inf),
    ## \section{Read data directly from files}
    list("domainName", "character", "", "", ""),
    list("dataDirectory", "character", "", "", ""),
    list("NAstring", "character", "?", "", ""),
    ## \section{Miscellaneous}
    list("maxThreads", "integer", 0, 0, Inf)
    )
    optAll <- data.frame()
    optLogical <- data.frame()
    optInteger <- data.frame()
    optNumeric <- data.frame()
    optCharacter <- data.frame()
    for (i in seq(along=optAllList)) {
        optRow <- optAllList[[i]]
        if (optRow[[2]] == "logical") {
            optDsc <- list(default=optRow[[3]])
            optLogical <- rbind(optLogical,data.frame(optDsc,stringsAsFactors=FALSE))
            optIndex <- nrow(optLogical)
        } else if (optRow[[2]] == "integer") {
            optDsc <- list(default=optRow[[3]],lower=optRow[[4]],upper=optRow[[5]])
            optInteger <- rbind(optInteger,data.frame(optDsc,stringsAsFactors=FALSE))
            optIndex <- nrow(optInteger)
        } else if (optRow[[2]] == "numeric") {
            optDsc <- list(default=optRow[[3]],lower=optRow[[4]],upper=optRow[[5]])
            optNumeric <- rbind(optNumeric,data.frame(optDsc,stringsAsFactors=FALSE))
            optIndex <- nrow(optNumeric)
        } else if (optRow[[2]]=="character") {
            if (optRow[[4]] != "" && length(grep(",",optRow[[4]]))==0) {
                warning(paste("Character option",optRow[[1]],"with a single value",optRow[[4]]))
            }
            optDsc <- list(default=optRow[[3]],type=optRow[[4]])
            optCharacter <- rbind(optCharacter,data.frame(optDsc,stringsAsFactors=FALSE))
            optIndex <- nrow(optCharacter)
        } else {
            warning(paste("Unknown type of option",optRow[[2]]))
        }
        optDsc <- list(name=optRow[[1]],type=optRow[[2]],index=optIndex);
        optAll <- rbind(optAll,data.frame(optDsc,stringsAsFactors=FALSE))
    }
    list(All=optAll,Logical=optLogical,Integer=optInteger,Numeric=optNumeric,Character=optCharacter)
}

# prepare the structure in advance for speed
optData <- optionData()  

getStatNames <- function() {
    return(c("median", "Q1", "Q3", "lowPercentile", "highPercentile", "mean", "stdDev", "p-value","exp")) 
}
checkOptionsValues <- function(options) {
	optNames <- names(options)
	occurs <- rep(FALSE,times=nrow(optData$All))
	for (i in seq(along=options)) {
		j <- match(optNames[i], optData$All$name, nomatch=-1)
		if (j == -1) {
			warning(paste("unrecognised option",optNames[i]),call.=FALSE)
		}
		else if (occurs[j]) {
			warning(paste("option",optNames[i],"used more than once"),call.=FALSE)
		}
		else {
			occurs[j] <- TRUE
			if (optData$All$type[j] == "numeric") {
				warn.save <- getOption("warn")
				options(warn=-1)
				nval <- as.numeric(options[i])
				options(warn=warn.save)
				if (is.na(nval)) {
					warning(paste("option",optNames[i],"should be numeric"),call.=FALSE)
				}
				else {
					k <- optData$All$index[j];
					lower <- optData$Numeric$lower[k]
					upper <- optData$Numeric$upper[k]
					if (nval < lower || nval > upper) {
						 warning(sprintf("option %s should be in [%f, %f]", optNames[i], lower, upper), call.=FALSE)
					}
				}
			}
			else if (optData$All$type[j] == "logical") {
				cval <- toupper(as.character(options[i]))
				if ( ! cval %in% c("Y","N")) {
					warning(paste("option",optNames[i],"should be TRUE, \"Y\", FALSE or \"N\""),call.=FALSE)
				}
			}
			else if (optData$All$type[j] == "integer") {
				warn.save <- getOption("warn")
				options(warn=-1)
				ival <- as.numeric(options[i])
				options(warn=warn.save)
				if (is.na(ival) || ival != trunc(ival)) {
					warning(paste("option",optNames[i],"should be integer"),call.=FALSE)
				}
				else {
					k <- optData$All$index[j];
					lower <- optData$Integer$lower[k]
					upper <- optData$Integer$upper[k]
					if (ival < lower || ival > upper) {
						warning(sprintf("option %s should be in [%d, %d]", optNames[i], lower, upper), call.=FALSE)
					}
				}
			}
			else if (optData$All$type[j] == "character") {
				k <- optData$All$index[j];
                if (optData$Character$type[k] != "") {
				   values <- strsplit(optData$Character$type[k],",")[[1]];
				   if ( ! options[i] %in% values) {
					  warning(paste("option",optNames[i],"should be one of:",optData$Character$type[k]),call.=FALSE)
                   }
				}
			}
		}
	}
}
checkEstimatorOptions <- function(estimator, options, isRegression) {
	errMsg <- NULL ;
	optNames <- names(options);
	estDsc <- infoCore(what="attrEval");
    estDscReg <- infoCore(what="attrEvalReg");
   	if (isRegression)
       estimator <- match.arg(estimator, estDscReg)
    else
       estimator <- match.arg(estimator, estDsc);
	## options allowed for all estimators,
	commonOpts <- c("attrEvaluationInstances","minNodeWeightEst","binaryEvaluation","binaryEvaluateNumericAttributes","maxThreads")
	for (i in 1:length(commonOpts)) {
		idx <- match(commonOpts[i], optNames, nomatch=-1);
		if (idx >0)
			optNames = optNames[-idx];
	}
    # options controlling extension of two-class estimators to multiclass 
    twoClassOpts <- c("multiclassEvaluation")
    for (i in 1:length(twoClassOpts)) {
        idx <- match(twoClassOpts[i], optNames, nomatch=-1);
        if (idx >0)
            optNames = optNames[-idx];
    }
    ## options allowed for Relief and its derivatives
	ReliefEst <- c(estDsc[grep("^Relief", estDsc)], estDscReg[grep("^RRelief", estDscReg)]) 
	if (estimator %in% ReliefEst) {
		ReliefOpts = c("ReliefIterations","numAttrProportionEqual","numAttrProportionDifferent")
		for (i in 1:length(ReliefOpts)) {
			idx <- match(ReliefOpts[i], optNames, nomatch=-1);
			if (idx >0)
				optNames = optNames[-idx];
		}
		## more checks to follow
		kNearEqualEst <- c("ReliefFequalK", "RReliefFequalK") ;
		expRankEst <- c("ReliefFexpRank","ReliefFdistance","ReliefFsqrDistance","ReliefFexpC","ReliefFavgC",
						"ReliefFpe","ReliefFpa","ReliefFsmp", 
                        "RReliefFexpRank", "RReliefFwithMSE", "RReliefFdistance","RReliefFsqrDistance"
                        );
		if (estimator %in% kNearEqualEst) {
			kNearEqualOpts <- c("kNearestEqual");
			for (i in 1:length(kNearEqualOpts)) {
				idx <- match(kNearEqualOpts[i], optNames, nomatch=-1);
				if (idx >0)
					optNames <- optNames[-idx];
			}
		}
		else if (estimator %in% expRankEst) {
			expRankOpts <- c("kNearestExpRank","quotientExpRankDistance")
			for (i in 1:length(expRankOpts)) {
				idx <- match(expRankOpts[i], optNames, nomatch=-1);
				if (idx >0)
					optNames <- optNames[-idx];
			}
		}
	}
	options[optNames]
}
checkModelOptions <- function(model, options) {
    optNames <- names(options);
    ## first check options by models, later handle special cases
    discretizationOpts <- c("selectionEstimator","discretizationLookahead","discretizationSample")
    discretizationOptsReg <- c("selectionEstimatorReg","discretizationLookahead","discretizationSample") # currently not used
    bayesOpts <- c(discretizationOpts,"bayesDiscretization","bayesEqFreqIntervals")
    knnOpts <- c("kInNN")
    knnKernelOpts <- c("kInNN","nnKernelWidth")
    miscOpts <-c("maxThreads") 
    treeModelOpts<-c("modelType",bayesOpts,knnOpts,knnKernelOpts,miscOpts)
    treeModelOptsReg<-c("modelTypeReg",knnKernelOpts,miscOpts)
    treeStopOpts <- c("minNodeWeightTree","minNodeWeightRF","relMinNodeWeight","majorClassProportion","minInstanceWeight")  
    treeStopOptsReg <- c("minNodeWeightTree","minNodeWeightRF","relMinNodeWeight","minInstanceWeight","rootStdDevProportion")  
    treePruneOpts <- c("selectedPruner","mEstPruning","mdlModelPrecision","mdlErrorPrecision")
    treePruneOptsReg <- c("selectedPrunerReg","mEstPruning","mdlModelPrecision","mdlErrorPrecision")
    treeConstructOpts <- c("constructionEstimator","constructionMode","constructionDepth","beamSize","maxConstructSize","noCachedInNode")
    treeConstructOptsReg <- c("constructionEstimatorReg","constructionMode","constructionDepth","beamSize","maxConstructSize","noCachedInNode")
    treeOpts <- unique(c("selectionEstimator",treeModelOpts,treeStopOpts,treePruneOpts,treeConstructOpts))
    rfOpts <- unique(c("selectionEstimator",treeStopOpts,discretizationOpts,miscOpts,"rfNoTrees", "rfNoSelAttr","rfMultipleEst","rfPropWeightedTrees","rfPredictClass","rfSampleProp","rfNoTerminals","rfRegType","rfRegLambda"))
    rfNearOpts <- c(rfOpts,"rfkNearestEqual")
    regOpts <- unique(c("selectionEstimatorReg",treeModelOptsReg,treeStopOptsReg,treePruneOptsReg,treeConstructOptsReg))
    opts = switch(model, rf=rfOpts, rfNear=rfNearOpts, bayes=bayesOpts, knn=knnOpts, knnKernel=knnKernelOpts, tree=treeOpts, regTree=regOpts)
    for (i in 1:length(opts)) {
      idx <- match(opts[i], optNames, nomatch=-1);
      if (idx >0)
         optNames = optNames[-idx]; 
    }
    if (model %in% c("rf","rfNear","tree")) {
        selEstIdx = match("selectionEstimator",names(options),nomatch=-1)
        if (selEstIdx==-1)
          selEst = optDefault("selectionEstimator")
        else
           selEst = options[selEstIdx]
        optRemain <- checkEstimatorOptions(selEst, options[optNames], FALSE) ;
    }
    else if (model == "regTree") {
        selEstIdx = match("selectionEstimatorReg",names(options),nomatch=-1)
        if (selEstIdx==-1)
          selEst = optDefault("selectionEstimatorReg")
        else
           selEst = options[selEstIdx]       
        optRemain <- checkEstimatorOptions(selEst, options[optNames], TRUE) ;
    }
    else {
        optRemain <- options[optNames]
    }
    optRemain
}
checkPredictOptions <- function(model, options) {
    optNames <- names(options);
    ## first check options by models, later handle special cases
    miscOpts <-c("maxThreads") 
    bayesOpts <- c()
    knnOpts <- c("kInNN")
    knnKernelOpts <- c("kInNN","nnKernelWidth")
    treeOpts <- c(knnKernelOpts,"mEstPrediction",miscOpts)
    rfOpts <- c("rfPredictClass",miscOpts)
    rfNearOpts <- c(rfOpts,"rfkNearestEqual")
    regOpts <- treeOpts 
    
    opts = switch(model$model, rf=rfOpts, rfNear=rfNearOpts, bayes=bayesOpts, knn=knnOpts, knnKernel=knnKernelOpts, tree=treeOpts, regTree=regOpts)
    for (i in seq(along=opts)) {
      idx <- match(opts[i], optNames, nomatch=-1);
      if (idx >0)
         optNames = optNames[-idx]; 
    }
    options[optNames] ;
}
checkOrdEvalOptions <- function(options) {
    optNames <- names(options);
    miscOpts <-c("maxThreads") 
    ordEvalOpts <- c(miscOpts,"ordEvalNoRandomNormalizers","ordEvalBootstrapNormalize","ordEvalNormalizingPercentile","attrWeights") #,"ordEvalConfidenceInterval")
    opts = ordEvalOpts
    for (i in 1:length(opts)) {
      idx <- match(opts[i], optNames, nomatch=-1);
      if (idx >0)
         optNames = optNames[-idx]; 
    }
    optRemain <- checkEstimatorOptions("ReliefFequalK", options[optNames], FALSE) ;
    optRemain
}
checkDataOptions <- function(options) {
    optNames <- names(options);
    opts <- c("domainName","dataDirectory","NAstring")
    for (i in 1:length(opts)) {
      idx <- match(opts[i], optNames, nomatch=-1);
      if (idx >0)
         optNames = optNames[-idx]; 
    }
    options[optNames]
}
optDefault <- function(optName) {
    optdat <- optData$All
    allIdx = match(optName, optdat[,"name"])
    type = optdat[allIdx,"type"]
    tabIdx = optdat[allIdx,"index"]
    switch (type, integer=optData$Integer[tabIdx,"default"],numeric=optData$Numeric[tabIdx,"default"],
                  logical=optData$Logical[tabIdx,"default"],character=optData$Character[tabIdx,"default"])
}
preparePlot<-function(fileName="Rplot", ...)
{
    interactive=FALSE
    fileType = unlist(strsplit(fileName,"\\."))
    if (is.na(fileType[2]))
        interactive = TRUE
    else if (tolower(fileType[2])=="pdf")
        pdf(file = fileName, paper="default", ...)
    else if (tolower(fileType[2])=="ps") 
        postscript(file = fileName, paper="default", horizontal=FALSE, encoding="ISOLatin1.enc",...)
    else if (tolower(fileType[2])=="emf" && .Platform$OS.type == "windows") 
        win.metafile(file = fileName,...)
    else if (tolower(fileType[2])=="jpg")
        jpeg(filename = fileName, ...)
    else if (tolower(fileType[2])=="tif")
        tiff(filename = fileName, ...)
    else if (tolower(fileType[2])=="bmp")
        bmp(filename = fileName, ...)
    else if (tolower(fileType[2])=="png")
        png(filename = fileName, ...)
    else if (tolower(fileType[2])=="tiff")
        bitmap(file = fileName, type="tiff24nc", ...)
    else interactive = TRUE
    if(interactive && dev.cur() == 1) # opens the default screen device on this platform if no device is open 
        dev.new() 
    invisible()
}
