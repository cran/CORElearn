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
           predictionColumn<-NA
           data <- cbind(prediction=predictionColumn,data)
       }
       else {
		   data <- cbind(prediction=factor(NA,levels=class.lev),data);
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
        "MyopicReliefF", "Accuracy", "BinAccuracy", "ReliefFmerit", "ReliefFdistance", "ReliefFsqrDistance",
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
    estDsc <- infoCore(what="attrEval");
    estList <- paste(estDsc,collapse=",")
    estDscReg <- infoCore(what="attrEvalReg");
    estListReg <- paste(estDscReg,collapse=",")
    	optAllList <- list(
    ## follows description of all parameters 
    ## the format is list with 6 components, some may be empty "":
    ## 1. name of parameter
    ## 2. parameter type:logical, integer, numeric, character
    ## 3. default value
    ## 4.lower limit
    ## 5. upper limit
    ## 5. description 
	list("binaryAttributes","logical",FALSE, FALSE, TRUE,
		"treat all attributes as binary?"),
	list("binarySplitNumericAttributes","logical", TRUE, FALSE, TRUE,
		"treat numerical attributes as binary?"),
    list("multiclassEvaluation","integer", 1, 1, 4,
        "multi-class extension for two-class-only evaluation measures",
        "1=average over all-pairs", 
        "2=best of all-pairs", 
        "3=average over one-against-all",
        "4=best of one-against-all"),
    list("attrEvaluationInstances","integer", 0, 0, Inf,
		"number of instances for attribute evaluation (0=all available)"),
	list("ReliefIterations","integer", 0, -2, Inf,
		"number of iterations for all variants of Relief",
			"0=DataSize",
			"-1=ln(DataSize)",
			"-2=sqrt(DataSize)"),
	list("numAttrProportionEqual","numeric", 0.04, 0, 1,
		"used in ramp function, proportion of numerical attribute's range to consider two values equal"),
	list("numAttrProportionEqual","numeric", 0.1, 0, 1,
		"used in ramp function, proportion of numerical attribute's range to consider two values different"),
	list("kNearestEqual","integer", 10, 0, Inf,
		"number of neighbours to consider in equal k-nearest attribute evaluation"),
	list("kNearestExpRank","integer", 70, 0, Inf,
		"number of neighbours to consider in exponential rank distance attribute evaluation"),
	list("quotientExpRankDistance","numeric", 20, 0, Inf,
		"quotient in exponential rank distance attribute evaluation"),
	## ordEval algorithm
    list("ordEvalNoRandomNormalizers","integer", 0, 0, Inf,
		"number of randomly shuffled attributes for normalization of each attribute",
			"0=no normalization"),
	list("ordEvalBootstrapNormalize","logical",FALSE, FALSE, TRUE, 
       "are features used for normalization constructed with bootstrap sampling or random permutation?"),
#    list("ordEvalConfidenceInterval","integer", 1, 1, 3,
#        "type of confidence interval for ordEval random normalization",
#        "1=two sided",
#        "2=upper",
#        "3=lower"),
    list("ordEvalNormalizingPercentile","numeric", 0.025, 0, 1,
        "defines the length of confidence interval obtained with random normalization. The default value gives 95% confidence interval."),
    list("attrWeights","character","","","",
		 "weights of the attributes in the ordEval distance measure"),
 ## stop building the tree
    list("minNodeWeight","numeric", 2, 0, Inf,
		"minimal number of instances (weight) of a tree node to split it further"),
	list("relMinNodeWeight","numeric", 0, 0, 1.0,
		"minimal proportion of training instances in a tree node to split it further"),
	list("majorClassProportion","numeric", 1.0, 0, 1.0,
		"proportion of majority class in a classification tree node to stop splitting it"),
    list("rootStdDevProportion","numeric", 0.0, 0, 1.0,
		"proportion of root's standard deviation in a regression tree node to stop splitting it"),
    	## tree building
    list("selectionEstimator","character","MDL",estList,"",
		"estimator for selection of attributes and binarization in classification"),
    list("selectionEstimatorReg","character","RReliefFexpRank",estListReg,"",
		"estimator for selection of attributes and binarization in regression"),
    list("minReliefEstimate","numeric", 0, -1.0, 1.0,
		"for variants of Relief attribute estimator the minimal evaluation of attribute to consider it useful"),
	list("minInstanceWeight","numeric", 0.05, 0, 1.0,
		"minimal weight of an instance to use it further"),
	## models in the leaves of the tree
    list("modelType","integer", 1, 1, 4,
		"type of models used in classification tree leaves",
			"1=majority class",
			"2=k-nearest neighbours",
			"3=k-nearest neighbors with kernel",
			"4=naive Bayes"),
    list("modelTypeReg","integer", 1, 1, 8,
		"type of models used in regression tree leaves",
			"1=mean predicted value",
			"2=median predicted value",
			"3=linear by MSE",
			"4=linear by MDL",
			"5=linear as in M5",
			"6=kNN",
			"7=Gaussian kernel regression",
			"8=locally weighted linear regression"),
   	list("kInNN","integer", 10, 0, Inf,
		"number of neighbours in k-nearest neighbours models",
			"0=all"),
	list("nnKernelWidth","numeric", 2, 0, Inf,
		"kernel width in k-nearest neighbours models"),
	## naive bayes as the model
    list("bayesDiscretization","integer", 2, 1, 2,
		"type of discretization for naive Bayes models",
			"1=greedy with selection estimator",
			"2=equal frequency"),
	list("bayesEqFreqIntervals","integer", 4, 1, Inf,
		"number of intervals in equal frequency discretization for naive Bayes models"),
	## feature construction
    list("constructionMode","integer", 15, 1, 15,
		"sum of constructive operators",
			"1=single",
			"2=conjunction",
			"4=addition",
			"8=multiplication",
			"all=1+2+4+8=15"),
	list("constructionDepth","integer", 0, 0, Inf,
		"maximal depth of the tree for constructive induction",
			"0=do not do construction",
			"1=only at root",
			"..."),
	list("noCachedInNode","integer", 5, 0, Inf,
		"number of cached attributes in each node where construction was performed"),
	list("constructionEstimator","character","MDL",estList,"",
		"estimator for constructive induction in classification"),
    list("constructionEstimator","character","RReliefFexpRank",estListReg,"",
		"estimator for constructive induction in regression"),
    list("beamSize","integer", 20, 1, Inf,
		"size of the beam in constructive induction"),
	list("maxConstructSize","integer", 3, 1, Inf,
		"maximal size of constructs in constructive induction"),
	## attribute discretization
    list("discretizationLookahead","integer", 3, 0, Inf,
		"number of times current discretization can be worse than the best",
			"0=try all possibilities"),
	list("discretizationSample","integer", 50, 0, Inf,
		"maximal number of points to try discretization",
			"0=all sensible"),
	## tree pruning
    list("selectedPruner","integer", 1, 0, 1,
		"decision tree pruning method used",
			"0=none",
			"1=with m-estimate"),
    list("selectedPrunerReg","integer", 2, 0, 4,
		"regression tree pruning method used",
			"0=none",
			"1=MDL",
			"2=with m-estimate",
			"3=as in M5",
			"4=error complexity as in CART (fixed alpha)"),
   	list("mdlModelPrecision","numeric", 0.1, 0, Inf,
		"precision of model coefficients in MDL tree pruning"),
	list("mdlErrorPrecision","numeric", 0.01, 0, Inf,
		"precision of errors in MDL tree pruning"),
	list("mEstPruning","numeric", 2, 0, Inf,
		"m-estimate for tree pruning"),
    list("alphaErrorComplexity","numeric", 0, 0, Inf,
		"alpha for error complexity pruning"),
    ## prediction
    list("mEstPrediction","numeric", 0, 0, Inf,
		"m-estimate for prediction"),
	## random forests
    list("rfNoTrees","integer", 100, 1, Inf,
		"number of trees in the random forest"),
	list("rfNoSelAttr","integer", 0, -2, Inf,
		"number of randomly selected attributes in the node",
			"0=sqrt(numOfAttr)",
			"-1=log_2(numOfAttr)+1",
			"-2=all"),
	list("rfMultipleEst","logical",FALSE, FALSE, TRUE,
		"use multiple attribute estimators in the forest?"),
	list("rfkNearestEqual","integer", 30, 0, Inf,
		"number of nearest intances for weighted random forest classification",
			"0=no weighing"),
	list("rfPropWeightedTrees","numeric", 0, 0, 1.0,
		"proportion of trees where attribute probabilities are weighted with their quality"),
	list("rfPredictClass","logical",FALSE, FALSE, TRUE,
		"individual trees predict with majority class (otherwise with class distribution)?"),
	## construction of general tree ensembles
    list("rfSampleProp","numeric", 0, 0, 1.0,
		"proportion of the training set to be used in learning",
			"0=bootstrap replication"),
	list("rfNoTerminals","integer", 0, 0, Inf,
		"maximal number of leaves in each tree",
			"0=build the whole tree"),
	## regularization
    list("rfRegType","integer", 2, 0, 2,
		"type of regularization",
			"0=no regularization",
			"1=global regularization",
			"2=local regularization"),
	list("rfRegLambda","numeric", 0, 0, Inf,
		"regularization parameter lambda",
			"0=no regularization"),
    ## data from files
    list("domainName","character","","","",
		"name of a problem to read from files with suffixes .dsc, .dat, .names, .data, .cm, and .costs"),
    list("dataDirectory","character","","","",
		"folder where data files are stored"),   
    list("NAstring","character","?","","",
		"character string which represents NA values in the data files")     
	)
	optAll <- data.frame()
	optLogical <- data.frame()
	optInteger <- data.frame()
	optNumeric <- data.frame()
	optCharacter <- data.frame()
	for (i in seq(along=optAllList)) {
		optRow <- optAllList[[i]]
		comment <- paste("comment: ",optRow[[6]],"\n",sep="")
		if (length(optRow) >= 7) {
			for (i in 7:length(optRow)) {
				comment <- paste(comment,"    ",optRow[[i]],"\n",sep="")
			}
		}
		if (optRow[[2]] == "logical") {
			optDsc <- list(default=optRow[[3]],description=comment)
			optLogical <- rbind(optLogical,data.frame(optDsc,stringsAsFactors=FALSE))
			optIndex <- nrow(optLogical)
		} else if (optRow[[2]] == "integer") {
			optDsc <- list(default=optRow[[3]],lower=optRow[[4]],upper=optRow[[5]],description=comment)
			optInteger <- rbind(optInteger,data.frame(optDsc,stringsAsFactors=FALSE))
			optIndex <- nrow(optInteger)
		} else if (optRow[[2]] == "numeric") {
			optDsc <- list(default=optRow[[3]],lower=optRow[[4]],upper=optRow[[5]],description=comment)
			optNumeric <- rbind(optNumeric,data.frame(optDsc,stringsAsFactors=FALSE))
			optIndex <- nrow(optNumeric)
		} else if (optRow[[2]]=="character") {
			if (optRow[[4]] != "" && length(grep(",",optRow[[4]]))==0) {
				warning(paste("Character option",optRow[[1]],"with a single value",optRow[[4]]))
			}
			optDsc <- list(default=optRow[[3]],type=optRow[[4]],description=comment)
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
						 warning(sprintf("option %s should be in [%lf, %lf]", optNames[i], lower, upper), call.=FALSE)
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
	commonOpts <- c("attrEvaluationInstances","binaryAttributes","binarySplitNumericAttributes")
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
    discretizationOptsReg <- c("selectionEstimatorReg","discretizationLookahead","discretizationSample")
    bayesOpts <- c(discretizationOpts,"bayesDiscretization","bayesEqFreqIntervals")
    knnOpts <- c("kInNN")
    knnKernelOpts <- c("kInNN","nnKernelWidth")
    treeModelOpts<-c("modelType",bayesOpts,knnOpts,knnKernelOpts)
    treeModelOptsReg<-c("modelTypeReg",knnKernelOpts)
    treeStopOpts <- c("minNodeWeight","relMinNodeWeight","majorClassProportion","minInstanceWeight")  
    treeStopOptsReg <- c("minNodeWeight","relMinNodeWeight","minInstanceWeight","rootStdDevProportion")  
    treePruneOpts <- c("selectedPruner","mEstPruning","mdlModelPrecision","mdlErrorPrecision")
    treePruneOptsReg <- c("selectedPrunerReg","mEstPruning","mdlModelPrecision","mdlErrorPrecision")
    treeConstructOpts <- c("constructionEstimator","constructionMode","constructionDepth","beamSize","maxConstructSize","noCachedInNode")
    treeConstructOptsReg <- c("constructionEstimatorReg","constructionMode","constructionDepth","beamSize","maxConstructSize","noCachedInNode")
    treeOpts <- unique(c("selectionEstimator",treeModelOpts,treeStopOpts,treePruneOpts,treeConstructOpts))
    rfOpts <- c("selectedEstimator",treeStopOpts,discretizationOpts, "rfNoTrees", "rfNoSelAttr","rfMultipleEst","rfPropWeightedTrees","rfPredictClass","rfSampleProp","rfNoTerminals","rfRegType","rfRegLambda")
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
    bayesOpts <- c()
    knnOpts <- c("kInNN")
    knnKernelOpts <- c("kInNN","nnKernelWidth")
    treeOpts <- c(knnKernelOpts,"mEstPrediction")
    rfOpts <- c("rfPredictClass")
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
    ordEvalOpts <- c("ordEvalNoRandomNormalizers","ordEvalBootstrapNormalize","ordEvalNormalizingPercentile","attrWeights") #,"ordEvalConfidenceInterval")
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
    optdat <- optionData()$All
    allIdx = match(optName, optdat[,"name"])
    type = optdat[allIdx,"type"]
    tabIdx = optdat[allIdx,"index"]
    switch (type, integer=optData$Integer[tabIdx,"default"],numeric=optData$Numeric[tabIdx,"default"],
                  logical=optData$Logical[tabIdx,"default"],character=optData$Character[tabIdx,"default"])
}
printOptLine <- function(name) {
    optData <- optionData()
    optdat <- optData$All
	allIdx = match(name, optdat[,"name"])
	type = optdat[allIdx,"type"]
	tabIdx = optdat[allIdx,"index"]
	cat("option=", name, "\n", sep="")
	cat("type=", type, sep="")
                    
    if (type == "integer") {
    lower = optData$Integer[tabIdx,"lower"]
    upper = optData$Integer[tabIdx,"upper"]
    default = optData$Integer[tabIdx,"default"]
    description = optData$Integer[tabIdx,"description"]
    cat(", min_value=", lower, ", max_value=", upper,", default_value=", default,"\n", sep="");
  }
  else if (type == "numeric") {
    lower = optData$Numeric[tabIdx,"lower"]
    upper = optData$Numeric[tabIdx,"upper"]
    default = optData$Numeric[tabIdx,"default"]
    description = optData$Numeric[tabIdx,"description"]
    cat(", min_value=", lower, ", max_value=", upper,", default_value=", default,"\n", sep="");
  }
  else if (type == "logical") {
    default = optData$Logical[tabIdx,"default"]
    description = optData$Logical[tabIdx,"description"]
    cat(", default_value=", default,"\n", sep="");
  }
  else if (type == "character") {
    default = optData$Character[tabIdx,"default"]
    description = optData$Character[tabIdx,"description"]
    cat(", default_value=", default,"\n", sep="");
  }
	cat(description,"\n\n");
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
