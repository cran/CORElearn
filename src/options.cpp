/********************************************************************
*
*   Name:              modul foptions (feature tree options)
*
*   Description:  reads the configuration file and interactively
*                 sets parameters for LFC (lookahead feature
*                 construction) tree
*
*********************************************************************/


#include <stdio.h>      // reading options and configuration file
#include <string.h>     // building menu items
#include <stdio.h>      // converting strings to doubleing point
#include <stdlib.h>
#include <time.h>

#include "general.h"

#if defined(UNIX)
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#else
#include <process.h>
#include <errno.h>
#endif


//#include "ftree.h"
#include "utils.h"
#include "menu.h"
#include "error.h"
#include "options.h"

extern char VersionString[] ;
extern int NoEstimators ;
extern int NoEstimatorsReg ;
extern estDsc estName[] ;
extern estDsc estNameReg[] ;

char keySeparators[] = "=" ;
char commentSeparators[] = "#%" ;


void Options::setDefault(void) {
    optionFile[0] = '\0' ;
	strcpy(action, "none") ;
    domainName[0] = '\0' ;
    splitIdx = 0 ;
    strcpy(dataDirectory, "data") ;
    strcpy(resultsDirectory, "results") ;
    strcat(dataDirectory, strDirSeparator) ;  // we attach separator: / or backslash
    strcat(resultsDirectory, strDirSeparator) ;
    strcpy(NAstring,"?") ;
    numberOfSplits = 10 ;
	splitSelection = CROSS_VALIDATION ;
    trainProportion = 0.9 ;
    rndSeedSplit = -1 ;

    attrEvaluationInstances = 0 ; // means all
    binaryAttributes = mFALSE ;
    binarySplitNumericAttributes = mTRUE ;
	multiclassEvaluation = 1 ; // average of all pairs

    ReliefIterations = 0 ; // means all
	numAttrProportionEqual = 0.04 ;
    numAttrProportionDifferent = 0.1 ;
    minReliefEstimate = 0.0 ;
    kNearestEqual = 10 ;
    kNearestExpRank = 70 ;
    quotientExpRankDistance = 20.0 ;

	ordEvalNoRandomNormalizers = 0 ;
	ordEvalBootstrapNormalize = mFALSE ;
	//oeCI = ciTwoSided ;
	ordEvalNormalizingPercentile = 0.025 ;
    // attrWeights remain uninstatiated

	minInstanceWeight = 0.05 ;
	selectionEstimator = estMDL ;
    constructionEstimator =estMDL;
    selectionEstimatorReg = estRReliefFexpRank ; // RReliefF with distance density
    constructionEstimatorReg = estRReliefFexpRank ;

	estOn.create(NoEstimators+1,mFALSE) ;
    estOn[estReliefFexpRank] = mTRUE ; // ReliefFexpRank
    estOn[estMDL] = mTRUE ;

	estOnReg.create(NoEstimatorsReg+1,mFALSE) ;
    estOnReg[ estRReliefFexpRank ] = mTRUE ;
    estOnReg[ estMSEofMean ] = mTRUE ;

	minNodeWeight = 5.0 ;
    relMinNodeWeight = 0.0;
    majorClassProportion = 1.0 ;
    rootStdDevProportion = 0.0 ;

    selectedPruner = 1 ; // m-estimate
    selectedPrunerReg = 2 ; // m-estimate pruning
	mEstPruning = 2.0 ;
    mEstPrediction = 0.0 ;
    mdlModelPrecision = 0.10 ;
    mdlErrorPrecision = 0.01 ;
    alphaErrorComplexity = 0.0 ; // breiman's error complexity pruning

	modelType = 1 ;  // majority class
    modelTypeReg = 3 ;  // linear as in M5
    kInNN = 10 ;
    nnKernelWidth = 2.0 ;
	bayesDiscretization = 2 ; // equal frequency discretization
    bayesEqFreqIntervals = 4 ;

	constructionMode = cSINGLEattribute+cCONJUNCTION+cSUM+cPRODUCT ;  // single + conjunctions + addition + multiplication
    constructionDepth = 0 ; // 0 - no construction by default, 1- only at the root
    beamSize=20 ;
    maxConstructSize = 3;
    noCachedInNode = 5 ;

	discretizationLookahead = 3 ;
    discretizationSample = 50 ;

    rfNoTrees = 100 ;
    rfNoSelAttr = 0; // meaning square root of the number of attributes
    rfMultipleEst = mFALSE ;
    rfkNearestEqual = 30 ;
    rfPropWeightedTrees = 0.0 ; // no weighting
    rfPredictClass = mFALSE;
	rfAttrEvaluate = mFALSE ;
	rfSampleProp = 0.0 ; // 0.0 = bootstrrap replication
    rfNoTerminals = 0 ; //0 = build whole tree
    rfRegType = 0 ; // no regularization
    rfRegLambda = 0.0 ; // lambda for regularization
    rfRndSeed = -1 ; // random seed for random forest


    printTreeInDot = mFALSE ;
    outProbDistr = mFALSE ;
    #if defined(UNIX)
       strcpy(defaultEditor, "vi") ;
    #endif
    #if defined(MICROSOFT)
       strcpy(defaultEditor, "notepad.exe" ) ;
    #endif
}

// ************************************************************
//
//                 processOptions
//                 --------------
//
//           interactively lets user change
//             parameters
//
// ************************************************************
void Options::processOptions(void)
{
   char FileName[MaxFileNameLen] ;
   char *tempStr = getenv("TMP") ;
   if (tempStr != NULL)
     strcpy(FileName, tempStr) ;
   else
     strcpy(FileName, ".") ;
   strcat(FileName, strDirSeparator) ;
   strcat(FileName, "tmpOptions.par") ;
   writeConfig(FileName) ;
   char CommandStr[2 * MaxFileNameLen] ;
   tempStr = getenv("EDITOR") ;
   if (tempStr != NULL)
      strcpy(CommandStr, tempStr) ;
   else
      strcpy(CommandStr, defaultEditor) ;
#if defined(MICROSOFT)
#if defined(MINGW)
#undef intptr_t
#define intptr_t int
#endif
   intptr_t childUID = _spawnlp(_P_WAIT, CommandStr, CommandStr, FileName, NULL) ;
   if (childUID == -1)  {
     // error
      char buf[2048] ;
      sprintf(buf, "Cannot run editor %s because: ",CommandStr) ;
      switch (errno) {
          case E2BIG: strcat(buf,"Argument list exceeds 1024 bytes") ;
                      break ;
          case EINVAL:strcat(buf,"mode argument is invalid") ;
                      break ;
          case ENOENT:strcat(buf,"File or path is not found") ;
                      break ;
          case ENOEXEC:strcat(buf,"Specified file is not executable or has invalid executable-file format") ;
                      break ;
          case ENOMEM:strcat(buf,"Not enough memory is available to execute new process") ;
                      break ;
          case EACCES:strcat(buf,"Permission denied") ;
                      break ;
          default:strcat(buf,"unknown error code") ;
      }
	  strcat(buf,"\nThe cause of this may be antivirus software preventing execution. \nPlease use commands Load/Save parameters and edit parameter file with your own text editor.") ;
      merror(buf,"") ;
  }
#endif
#if defined(UNIX)
  pid_t childUID = fork() ;
  switch (childUID)  {
     case -1:// error
             merror("Cannot run the editor", CommandStr) ;
			 break ;
	 case 0: // child
		     execlp(CommandStr, CommandStr, FileName, NULL) ;
             break ;
	 default: // parent
              waitpid(childUID, NULL, 0) ;
			  break ;
  }

#endif


   readConfig(FileName) ;

}



//************************************************************
//
//                      readConfig
//                      ----------
//
//      reads parameters for feature tree from given file
//
//************************************************************
int Options::readConfig(char* ConfigName)
{
	int i,len;
	FILE *from ;
    if ((from=fopen(ConfigName,"r"))==NULL) {
        merror("Cannot open configuration file ",ConfigName) ;
        return 0 ;
    }

	char buf[MaxNameLen]  ;
	while (fgets(buf,MaxNameLen,from) != NULL) {
		len = (int)strlen(buf) ;
		for (i=0; i<len; i++) {
			if (buf[i] == '\n' || buf[i] == '\r' || strchr(commentSeparators, buf[i]) != NULL)
			buf[i] = '\0' ;
		}
		strTrim(buf) ;
		if (buf[0] != '\0')
			assignOption(buf) ;
	}
	fclose(from) ;
	return 1 ;
}

//************************************************************
//
//                      readConfigFromString
//                      ----------
//
//      reads parameters for feature tree from given string
//
//************************************************************
int Options::readConfigFromString(char* optionsString)
{
	char optSeparators[] = ", " ;
	char buf[MaxNameLen]  ;
	int strIdx ;

    char *token = myToken(optionsString, strIdx, optSeparators);
    while (token != 0) {
    	strcpy(buf, token) ;
    	strTrim(buf) ;
		if (buf[0] != '\0')
			assignOption(buf) ;
        token = myToken(optionsString, strIdx, optSeparators) ;
	}
	return 1 ;
}

//************************************************************
//
//                      optionsFromStrings
//                      ----------
//
//      assigns given options
//
//************************************************************
int Options::optionsFromStrings(int noOptions, marray<char* > &optionsName, marray<char* > &optionsVal)
{
	char optBuf[MaxNameLen] ;
    for (int i=0 ; i < noOptions ; i++) {
     	sprintf(optBuf,"%s=%s",optionsName[i],optionsVal[i]) ;
     	assignOption(optBuf) ;
    }
	return 1 ;
}



//************************************************************
//
//                      writeConfig
//                      -----------
//
//      writes parameters for feature tree to given file
//
//************************************************************
int Options::writeConfig(char* ConfigName)
{
    FILE *to ;
    if ((to=fopen(ConfigName,"w"))==NULL)
    {
       merror("Cannot create configuration file ",ConfigName) ;
       return 0 ;
    }
    outConfig(to) ;
    if (ferror(to))  {
      merror("Cannot write parameters to configuration file", ConfigName) ;
      fclose(to) ;
      return 0;
    }

    fclose(to) ;
    return 1 ;

}



void Options::outConfig(FILE *to)
{
    fprintf(to, "# Options file for %s\n", VersionString) ;
	fprintf(to, "# Note the conventions:\n");
    fprintf(to, "# each option is on a separate line, the order of options is not important\n") ;
	fprintf(to, "# everything after # character is ignored\n") ;
	fprintf(to, "# if # is the first character, entire line is ignored\n") ;
    fprintf(to, "# the format of options is\n") ;
	fprintf(to, "# keyword=keyValue\n") ;
	fprintf(to, "#\n") ;

    fprintf(to, "# ---------- File and data options ----------\n") ;

    // Domain name
	fprintf(to,"domainName=%s  # domain name\n",domainName ) ;

    // Data directory
	fprintf(to,"dataDirectory=%.*s  # data directory\n", strlen(dataDirectory)-1, dataDirectory) ;

    // Results directory
	fprintf(to,"resultsDirectory=%.*s  # results directory\n", strlen(resultsDirectory)-1, resultsDirectory) ;

    // Definiton of train/test data splits
    fprintf(to,"# Types of supported splits to training/testing data:  \n") ;
    fprintf(to,"# 0-read from files, 1-cross validation, 2-stratified cross-validation,\n") ;
    fprintf(to,"# 3-leave one out CV, 4-all data is for training, 5-random split to train/test\n") ;
	fprintf(to, "splitSelection=%d  # definiton of train/test data splits\n", splitSelection) ;

    // Number of of iterations (data split to work on)
	fprintf(to, "numberOfSplits=%d  # number of data splits\n", numberOfSplits) ;

    // Train proportion
	fprintf(to,"trainProportion=%f  # the proportion of training instances in case of random split to train/test\n", trainProportion) ;

    // random seed for split
	fprintf(to,"rndSeedSplit=%ld  # random seed for data split determination (0-take from clock)\n", rndSeedSplit) ;

    // Split index
	fprintf(to,"splitIdx=%d  # in case of work on single split, the index of that split\n", splitIdx) ;


    // estimators

    fprintf(to, "# ---------- Estimation of attributes options ----------\n") ;

    // Treat all attributes as binary
	fprintf(to,"binaryAttributes=%s  # treat attributes as binary\n", (binaryAttributes ? "Y" : "N")) ;

    // Treat numerical attribute splits as binary in applicable measures
	fprintf(to,"binarySplitNumericAttributes=%s  # treat numerical attributes' splits as binary\n", (binarySplitNumericAttributes ? "Y" : "N")) ;

    // multi-class extension for two-class-only evaluation measures
	fprintf(to,"multiclassEvaluation=%d  # multi-class extension for two-class-only evaluation measures (1-average of all-pairs, 2-best of all-pairs, 3-average of one-against-all, 4-best of one-against-all)\n", multiclassEvaluation) ;

	// Number of examples  for estimation
	fprintf(to,"attrEvaluationInstances=%d  # number of instances for attribute evaluation (0 means all)\n", attrEvaluationInstances) ;


    // switches for clasification estimation
    fprintf(to, "# Classification estimators \n") ;
    for (int estIdx = 1 ; estIdx <= NoEstimators ; estIdx++)
	   fprintf(to, "est%s=%s  # %s\n" , estName[estIdx].brief, (estOn[estIdx] ? "Y" : "N"), estName[estIdx].dsc) ;
    // switches for regression estimation
    fprintf(to, "# Regressionn estimators \n") ;
    for (int estIdxReg = 1 ; estIdxReg <= NoEstimatorsReg ; estIdxReg++)
	   fprintf(to, "est%s=%s  # %s\n" , estNameReg[estIdxReg].brief, (estOnReg[estIdxReg] ? "Y" : "N"), estNameReg[estIdxReg].dsc) ;


    fprintf(to, "# ---------- ReliefF options ----------\n") ;

     // number of iterations for ReliefF's estimation
	fprintf(to,"ReliefIterations=%d  # number of iterations for all variants of Relief  (0-TrainSize, -1-ln(TrainSize), -2-sqrt(TrainSize))\n",ReliefIterations) ;

    // Default proportion of numeric attribute to consider value equal
	fprintf(to,"numAttrProportionEqual=%f  # proportion of numerical attribute's range to consider values equal\n",numAttrProportionEqual) ;

    // Default proportion of numeric attribute to consider value different
	fprintf(to,"numAttrProportionDifferent=%f  # proportion of numerical attribute's range to consider values different\n",numAttrProportionDifferent) ;

    // Number of neighbours to consider - k
	fprintf(to, "kNearestEqual=%d  # number of neighbours to consider in equal k nearest evaluation\n",kNearestEqual) ;

    // Number of neighbours in  distance density estimation
	fprintf(to, "kNearestExpRank=%d  # number of neighbours to consider in exponential rank distance evaluation\n",kNearestExpRank) ;

    // Quotient in Gaussian function of distance density
	fprintf(to, "quotientExpRankDistance=%f  # quotient in exponential rank distance evaluation\n",quotientExpRankDistance) ;

        // ordEval
    fprintf(to, "# ---------- ordEval algorithm ----------\n") ;

    // number of randomly shuffled attributes for normalization of each attribute
	fprintf(to,"ordEvalNoRandomNormalizers=%d  # number of randomly shuffled attributes for normalization of each attribute\n",ordEvalNoRandomNormalizers) ;

	// bootstrap sampling or permutation for random normalizers
	fprintf(to,"ordEvalBootstrapNormalize=%s  # bootstrap sampling or permutation for random normalizers\n", (ordEvalBootstrapNormalize ? "Y" : "N")) ;

	// confidence interval
//	fprintf(to, "# types of confidence intervals for ordEval random normalization") ;
//	fprintf(to,"# 1-two sided, 2-upper, 3-lower") ;
//	fprintf(to,"ordEvalConfidenceInterval=%d  # type of confidence interval for ordEval random normalization\n",(int)oeCI) ;

	// the alpha for confidence interval
	fprintf(to,"ordEvalNormalizingPercentile=%lf  # the percentile defining the length of confidence interval obtained with random normalization", ordEvalNormalizingPercentile) ;

	// attribute weights
	fprintf(to, " # weights of the attributes in the distance measure, 0 means no weighting, the format is: a;w_1,w_2,...w_a\n") ;
	fprintf(to,"attrWeights=%d   ", attrWeights.filled()) ;
	for (int iA = 1 ; iA < attrWeights.filled() ; ++iA)
	     fprintf(to,"%f ", attrWeights[iA]) ;


    fprintf(to, "\n# ---------- Stopping options ----------\n") ;

    // minimal leaf's weight
	fprintf(to,"minNodeWeight=%.2f  # minimal weight of a tree node\n", minNodeWeight) ;

    // Proportion of all examples in a node to stop
	fprintf(to,"relMinNodeWeight=%f  # minimal proportion of training instances in a tree node to stop\n",relMinNodeWeight) ;

    // Majority class proportion in a node
	fprintf(to,"majorClassProportion=%f  # proportion of majority class in a tree node\n",majorClassProportion) ;

    // Proportion of standard deviation to stop
	fprintf(to,"rootStdDevProportion=%f  # proportion of root's standard deviation in a node\n",rootStdDevProportion) ;


    fprintf(to, "# ---------- Building  options ----------\n") ;

	// selected estimator
    fprintf(to, "# Available classification estimators:") ;
    const int maxLineFilled = 80 ;
    int ei, lineFilled = maxLineFilled +1 ;
    for (ei = 1 ; ei <= NoEstimators ; ei++) {
    	if (lineFilled > maxLineFilled) {
    		// go to new line
    		fprintf(to,"\n#\t") ;
    		lineFilled = 0 ;
    	}
    	fprintf(to, "%2d-%s,", ei, estName[ei].dsc);
    	lineFilled += 3 + strlen(estName[ei].dsc) ;
    }
	fprintf(to, "\nselectionEstimator=%d  # estimator for selection of attributes and binarization in classification (1-%d)\n" , selectionEstimator, NoEstimators) ;

    fprintf(to, "# Available regression estimators:") ;
    lineFilled = maxLineFilled +1 ;
    for (ei = 1 ; ei <= NoEstimatorsReg ; ei++) {
     	if (lineFilled > maxLineFilled) {
     		// go to new line
     		fprintf(to,"\n#\t") ;
     		lineFilled = 0 ;
     	}
     	fprintf(to, "%2d-%s,", ei, estNameReg[ei].dsc);
     	lineFilled += 3 + strlen(estNameReg[ei].dsc) ;
     }
 		fprintf(to, "\nselectionEstimatorReg=%d  # estimator for selection of attributes and binarization in regression (1-%d)\n" , selectionEstimatorReg, NoEstimatorsReg) ;

    // Minimal ReliefF's estimate of attribute to consider it further
	fprintf(to,"minReliefEstimate=%f  # in case of any Relief's variant the minimal evaluation of attribute to considerd it useful\n",minReliefEstimate) ;

	// Minimal probabillity of example to consider it
	fprintf(to,"minInstanceWeight=%.2f  # minimal weight of an instance\n",minInstanceWeight) ;

    // Type of classification models used in the leafs
    fprintf(to, "# Available classification models: \n") ;
	fprintf(to, "#\t1-majority class, 2-k-nearest neighbours, 3-k-nearest neighbors with kernel, 4-simple Bayes\n") ;
	fprintf(to,"modelType=%d  # type of classification models used in tree leaves (1-4)\n", modelType) ;

    // Type of regression models used in the leafs
    fprintf(to,"# Available regression models: \n") ;
    fprintf(to,"#\t1-mean predicted value, 2-median predicted value, 3-linear by MSE, 4-linear by MDL,\n");
    fprintf(to,"#\t5-linear as in M5, 6-kNN, 7-Gaussian kernel regression, 8-locally weighted linear regression\n") ;
	fprintf(to,"modelTypeReg=%d  # type of regression models used in the leafs (1-8)\n", modelTypeReg) ;

    // k in k-nearest neighbour models
	fprintf(to,"kInNN=%d  # number of neighbours in k-nearest neighbours models (0-all)\n", kInNN) ;

    // kernel  in kNN models
	fprintf(to,"nnKernelWidth=%f  # kernel width in k-nearest neighbours models\n", nnKernelWidth) ;

    // type of discretization for simple Bayes
	fprintf(to, "bayesDiscretization=%d  # type of discretization for naive Bayes models (1-greedy with selection estimator, 2-equal frequency)\n", bayesDiscretization) ;

	// number of intervals for equal frequency discretization for simple Bayes models
	fprintf(to, "bayesEqFreqIntervals=%d  # number of intervals in equal frequency discretization for naive Bayes models\n", bayesEqFreqIntervals) ;


    fprintf(to, "# ---------- Constructive induction options ----------\n") ;

    // which constructive operators to use
	fprintf(to,"constructionMode=%d  # constructive operators sum (1-single, 2-conjunction, 4-addition, 8-multiplication, e.g., all-1+2+4+8 i.e. 15) \n", constructionMode) ;

    // depth to which to perform  constructive induction
	fprintf(to,"constructionDepth=%d  # maximal depth (height) of the tree to do construction (0-do not do construction, 1-only at root, ...)\n", constructionDepth) ;

    // depth to which to perform  constructive induction
	fprintf(to,"noCachedInNode=%d  # number of cached attributes in each node where construction was performed\n", noCachedInNode) ;

    // construction estimator for classification
	fprintf(to, "constructionEstimator=%d  # estimator for constructive induction (1-%d)\n" , constructionEstimator, NoEstimators) ;

	// construction estimator for regression
	fprintf(to, "constructionEstimatorReg=%d  # estimator for constructive induction (1-%d)\n" , constructionEstimatorReg, NoEstimatorsReg) ;

    // beam size for beam search
	fprintf(to,"beamSize=%d  # size of the beam\n",beamSize) ;

    // maximal size of constructs
	fprintf(to,"maxConstructSize=%d  # maximal size of constructs\n", maxConstructSize) ;


    // Number of times current discretization can be worse than the best
	fprintf(to,"discretizationLookahead=%d  # Number of times current discretization can be worse than the best (0-try all possibilities)\n",discretizationLookahead) ;

    // Maximal number of points to try discretization (binarization)
	fprintf(to,"discretizationSample=%d  # maximal number of points to try discretization (0 means all sensible)\n",discretizationSample) ;


    fprintf(to, "# ---------- Pruning  options ----------\n") ;

    // selected pruner for classification
	fprintf(to, "selectedPruner=%d  # pruning method used in classification (0-none, 1-with m-estimate)\n", selectedPruner) ;

    // selected pruner for regression
	fprintf(to, "selectedPrunerReg=%d  # pruning method used in regression (0-none, 1-MDL, 2-with m-estimate, 3-as in M5, 4-error complexity as in CART (fixed alpha))\n", selectedPrunerReg) ;

	// Precision of model coefficients in MDL pruning procedure
	fprintf(to, "mdlModelPrecision=%f  # precision of model coefficients in MDL pruning\n",mdlModelPrecision) ;

    // Precision of error coefficients in MDL
	fprintf(to, "mdlErrorPrecision=%f  # precision of errors in MDL pruning\n",mdlErrorPrecision) ;

    // m - estimate for pruning
	fprintf(to,"mEstPruning=%f  # m-estimate for pruning\n",mEstPruning) ;

    // alpha for error complexity pruning
	fprintf(to,"alphaErrorComplexity=%f  # alpha for error complexity pruning\n",alphaErrorComplexity) ;


    fprintf(to, "# ---------- Random forest options ----------\n") ;

    // number of trees in forest
	fprintf(to,"rfNoTrees=%d  # number of trees in the random forest\n",rfNoTrees) ;

    // Number of randomly selected attributes in the node
	fprintf(to,"rfNoSelAttr=%d  # number of randomly selected attributes in the node (0-sqrt(numOfAttr), -1-log_2(numOfAttr)+1, -2-all)\n",rfNoSelAttr) ;

    // Use multiple estimators
	fprintf(to,"rfMultipleEst=%s  # use multiple estimators in the forest\n",(rfMultipleEst ? "Y" : "N")) ;

    // Number of nearest instances for weighted rf classification
	fprintf(to,"rfkNearestEqual=%d  # number of nearest intances for weighted random forest classification (0-no weighting)\n",rfkNearestEqual) ;

    // Proportion of trees where attribute probabilities are weighted with ReliefF
	fprintf(to,"rfPropWeightedTrees=%f  # proportion of trees where attribute probabilities are weighted\n",rfPropWeightedTrees) ;

    // Predict with majority class, otherwise use class distribution
	fprintf(to,"rfPredictClass=%s  # predict with majority class (otherwise with class distribution)\n",(rfPredictClass ? "Y" : "N")) ;

    // Evaluate attributes with out-of-bag evaluation
	fprintf(to,"rfAttrEvaluate=%s  # evaluate attributes with random forest out-of-bag evaluation\n",(rfAttrEvaluate ? "Y" : "N")) ;

	// Proportion of the training examples to be used in learning (0.0-bootstrap replication)
	fprintf(to,"rfSampleProp=%f  #proportion of the training set to be used in learning (0.0-bootstrap replication)\n",rfSampleProp) ;

	// Number of leaves in the individual trees (0-build a whole tree)
	fprintf(to,"rfNoTerminals=%d  # number of leaves in each tree (0-build the whole tree)\n",rfNoTerminals) ;

	// Type of regularization (0-no regularization, 1-global regularization, 2-local regularization)
	fprintf(to,"rfRegType=%d  # type of regularization (0-no regularization, 1-global regularization, 2-local regularization)\n",rfRegType) ;

	// Regularization parameter Lambda
	fprintf(to,"rfRegLambda=%f  # regularization parameter lambda\n",rfRegLambda) ;

    // random seed for forest
	fprintf(to,"rfRndSeed=%ld  # random seed for random forest (0-take from clock)\n", rfRndSeed) ;


    fprintf(to, "# ---------- Other  options ----------\n") ;

	// m - estimate for prediction
	fprintf(to,"mEstPrediction=%f  # m-estimate for prediction\n",mEstPrediction) ;

    // print tree also in dot format
	fprintf(to,"printTreeInDot=%s  # print tree also in dot format\n", (printTreeInDot ? "Y" : "N")) ;

    // output probability distribution
	fprintf(to,"outProbDistr=%s  # output class probability distribution for predicted instances\n", (outProbDistr ? "Y" : "N")) ;

	// Editor for options
	fprintf(to,"defaultEditor=%s  # editor for options file\n",defaultEditor) ;

    // Missing values indicator
	fprintf(to,"NAstring=%s  # string indicating missing value",NAstring) ;

 }


void Options::parseOption(char *optString, char *keyword, char *key) {
    int strIdx = 0 ;
    strTrim(optString) ;
    char *token = myToken(optString, strIdx, keySeparators);
    strcpy(keyword, token) ;
    strTrim(keyword) ;
   	token = myToken(optString, strIdx, commentSeparators);
	if (token != 0) {
		strcpy(key, token) ;
		strTrim(key) ;
	}
	else {
		key[0] = '\0' ;
		merror("Option has no value: ", keyword) ;
	}
 }


 void Options::assignOption(char *optString) {
    char  keyword[MaxNameLen], key[MaxNameLen] ;

	parseOption(optString, keyword, key) ;
	assignOption(keyword, key) ;
 }


 void Options::assignOption(char *keyword, char *key) {
    char errBuf[MaxNameLen];
	int temp ;
	double dtemp ;

	//data options

	if (strcmp(keyword, "action")==0 || strcmp(keyword, "a")==0) {
		strcpy(action, key) ;
	}
	else if (strcmp(keyword, "optionFile")==0 || strcmp(keyword, "o")==0) {
		printf("\nReading configuration file %s . . .", key) ;
 		fflush(stdout) ;
		if (readConfig(key))
			printf(" done.") ;
  	 	fflush(stdout) ;
	}
	else if (strcmp(keyword, "domainName")==0) {
		// domain name
		strcpy(domainName, key) ;
	}
	else if (strcmp(keyword, "dataDirectory")==0) {
        // data directory
        strcpy(dataDirectory,key) ;
        temp = (int)strlen(dataDirectory) ;
       if (dataDirectory[temp-1] != DirSeparator) {
          dataDirectory[temp] = DirSeparator ;
          dataDirectory[temp+1] = '\0' ;
       }
	}
	else if (strcmp(keyword, "resultsDirectory")==0) {
       // Results directory
       strcpy(resultsDirectory, key) ;
       temp = (int)strlen(resultsDirectory) ;
       if (resultsDirectory[temp-1] != DirSeparator) {
          resultsDirectory[temp] = DirSeparator ;
          resultsDirectory[temp+1] = '\0' ;
       }
	}
	else if (strcmp(keyword, "splitSelection")==0) {
       // Definiton of train/test data splits
       sscanf(key, "%d", &temp) ;
       if (temp >= 0 && temp <=5)
        splitSelection = (splitSelectionType)temp ;
       else
		   merror("splitSelection (definiton of train/test data splits) should be one of supported (0-5)", "") ;
	}
	else if (strcmp(keyword, "numberOfSplits")==0) {
       // Number of data splits to work on
       sscanf(key,"%d", &temp) ;
       if (temp > 0)
 	  	 numberOfSplits = temp ;
       else
	 	 merror("numberOfSplits (number of data splits) should be positive", "") ;
	}
	else if (strcmp(keyword, "trainProportion")==0) {
       // train proportion
       sscanf(key,"%lf", &dtemp) ;
       if (dtemp > 0.0 && dtemp < 1.0)
          trainProportion = dtemp ;
       else
          merror("trainProportion (the proportion of training instances in random split) should be between 0 and 1","") ;
	}
	else if (strcmp(keyword, "rndSeedSplit")==0) {
       // Random seed for data splits
       sscanf(key,"%ld", &rndSeedSplit) ;
       if (rndSeedSplit == 0)
           rndSeedSplit = -(long)time(NULL) ;
	}
	else if (strcmp(keyword, "splitIdx")==0) {
       // Split index
       sscanf(key, "%d", &temp) ;
       if (temp>=0)
          splitIdx = temp ;
       else
          merror("splitIdx (data split index) should be positive", "") ;
	}

	// Estimator options
	else if (strcmp(keyword, "binaryAttributes")==0) {
	   // Treat all attributes as binary
       if (key[0] == 'y' || key[0] == 'Y')
          binaryAttributes = mTRUE ;
       else if (key[0] == 'n' || key[0] == 'N')
           binaryAttributes = mFALSE ;
       else
		   merror("binaryAttributes (treat attributes as binary) should be on or off (Y or N)", "") ;
	}
    else if (strcmp(keyword, "binarySplitNumericAttributes")==0) {
	    // Treat numeric attribute splits as binary in applicable measures
        if (key[0] == 'y' || key[0] == 'Y')
           binarySplitNumericAttributes = mTRUE ;
        else if (key[0] == 'n' || key[0] == 'N')
           binarySplitNumericAttributes = mFALSE ;
        else
			merror("binarySplitNumericAttributes (treat numerical attributes' splits as binary) should be on or off (Y or N)", "") ;
	}
    else if (strcmp(keyword, "attrEvaluationInstances")==0) {
       // number of examples  for attribute estimations
       sscanf(key,"%d", &temp) ;
       if (temp >= 0)
         attrEvaluationInstances = temp ;
       else
          merror("attrEvaluationInstances (number of instances for attribute evaluation) should be non-negative", "") ;
	}
    else if (strcmp(keyword, "multiclassEvaluation")==0) {
       //
       sscanf(key,"%d", &temp) ;
       if (temp >= 1 && temp <= 4)
         multiclassEvaluation = temp ;
       else
          merror("multiclassEvaluation (multi-class extension for two-class-only evaluation measures) should be 1, 2, 3, or 4", "") ;
	}
	else {
        // switches for classification estimation
        booleanT estSwitch = mFALSE ;
		char estKeyword[MaxNameLen] ;
        for (int estIdx = 1 ; estIdx <= NoEstimators ; estIdx++)  {
		   sprintf(estKeyword, "est%s",estName[estIdx].brief) ;
		   if (strcmp(keyword, estKeyword)==0) {
			   estSwitch =mTRUE ;
               if (key[0] == 'y' || key[0] == 'Y')
                  estOn[estIdx] = mTRUE ;
               else  if (key[0] == 'n' || key[0] == 'N')
                  estOn[estIdx] = mFALSE ;
               else {
                 sprintf(errBuf, "est%s (attribute estimator \"%s\") should be on (y, Y) or off (n, N)", estName[estIdx].brief, estName[estIdx].dsc) ;
                 merror(errBuf, "") ;
               }
			   break ;
		   }
		}
       // switches for regression estimation
	   if (!estSwitch) {
         for (int estIdx = 1 ; estIdx <= NoEstimatorsReg ; estIdx++)  {
	       sprintf(estKeyword, "est%s",estNameReg[estIdx].brief) ;
	       if (strcmp(keyword, estKeyword)==0) {
		     estSwitch =mTRUE ;
             if (key[0] == 'y' || key[0] == 'Y')
                estOnReg[estIdx] = mTRUE ;
             else  if (key[0] == 'n' || key[0] == 'N')
                estOnReg[estIdx] = mFALSE ;
             else {
               sprintf(errBuf, "est%s (attribute estimator \"%s\") should be on (y, Y) or off (n, N)", estNameReg[estIdx].brief, estNameReg[estIdx].dsc) ;
               merror(errBuf, "") ;
             }
		     break ;
	       }
	     }
	   }
	if (!estSwitch) {

	//  ReliefF options

	if (strcmp(keyword, "ReliefIterations")==0) {
       // Number of iterations in ReliefF's main loop
       sscanf(key,"%d", &temp) ;
       if (temp >= -2)
          ReliefIterations = temp ;
       else
          merror("ReliefIterations (number of iterations for all variants of Relief) should be larger or equal to -2", "") ;
	}
    else if (strcmp(keyword, "numAttrProportionEqual")==0) {
       // numerical attribute proportion equal
       sscanf(key,"%lf", &dtemp) ;
       if (dtemp >= 0.0 && dtemp <= 1.0)
          numAttrProportionEqual = dtemp ;
       else
          merror("numAttrProportionEqual (proportion of numerical attribute's range to consider values equal) should be between 0 and 1","") ;
	}
    else if (strcmp(keyword, "numAttrProportionDifferent")==0) {
       // numAttrProportionDifferent
       sscanf(key,"%lf", &dtemp) ;
       if (dtemp >= 0 && dtemp <= 1.0)
          numAttrProportionDifferent = dtemp ;
       else
          merror("numAttrProportionDifferent (proportion of numerical attribute's range to consider values different) should be between 0 and 1","") ;
	}
    else if (strcmp(keyword, "kNearestEqual")==0) {
       // Number of neighbours to consider - kEqual
       sscanf(key,"%d", &temp) ;
       if (temp >= 0)
          kNearestEqual = temp ;
       else
          merror("kNearestEqual (number of neighbours to consider in equal k nearest evaluation) should be nonnegative","") ;
	}
    else if (strcmp(keyword, "kNearestExpRank")==0) {
       // Number of neighbours to consider - kExpRank
       sscanf(key,"%d", &temp) ;
       if (temp >= 0)
          kNearestExpRank = temp ;
       else
          merror("kNearestExpRank (number of neighbours to consider in exponential rank distance evaluation) should be nonnegative","") ;
	}
    else if (strcmp(keyword, "quotientExpRankDistance")==0) {
       // quotient in Gaussian function at exponential rank distance
       sscanf(key,"%lf", &dtemp) ;
       if (dtemp > 0.0 )
          quotientExpRankDistance = dtemp ;
       else
           merror("quotientExpRankDistance (quotient in exponential rank distance evaluation) should be positive", "") ;
	}

	// ordEval algorithm

	else if (strcmp(keyword, "ordEvalNoRandomNormalizers")==0) {
       // number of randomly shuffled attributes to be used for normalization of each attribute in ordEval algorithm
       sscanf(key,"%d", &temp) ;
       if (temp >= 0)
          ordEvalNoRandomNormalizers = temp ;
       else
          merror("ordEvalNoRandomNormalizers (number of randomly shuffled attributes to be used for normalization of each attribute in ordEval algorithm) should be nonnegative", "") ;
	}
	else if (strcmp(keyword, "ordEvalBootstrapNormalize")==0) {
	   // Treat all attributes as binary
       if (key[0] == 'y' || key[0] == 'Y')
    	   ordEvalBootstrapNormalize = mTRUE ;
       else if (key[0] == 'n' || key[0] == 'N')
    	   ordEvalBootstrapNormalize = mFALSE ;
       else
		   merror("ordEvalBootstrapNormalize (bootstrap or permutation for random normalization) should be on or off (Y or N)", "") ;
	}
//	else if (strcmp(keyword, "ordEvalConfidenceInterval")==0) {
//       // type of confidence interval for ordEval normalization
//       sscanf(key,"%d", &temp) ;
//       if (temp >= 1 && temp <=3)
//          ordEvalconfidenceInterval = (oeConfidenceInterval) temp ;
//       else
//          merror("ordEvalConfidenceInterval (type of confidence interval for ordEval random normalization) should be 1, 2, or 3", "") ;
//	}
	else if (strcmp(keyword, "ordEvalNormalizingPercentile")==0) {
       // the length of confidence interval obtained with random normlization
       sscanf(key,"%lf", &dtemp) ;
       if (dtemp >= 0 && dtemp <= 1.0)
          ordEvalNormalizingPercentile = dtemp ;
       else
          merror("ordEvalNormalizingPercentile (the percentile defining the confidence interval obtained with random normalization in ordEval) should be between 0 and 1", "") ;
	}
	else if (strcmp(keyword, "attrWeights")==0) {
       // weights of the attributes in the distance measure
       sscanf(key,"%d", &temp) ;
	   if (temp > 0) {
		   attrWeights.create(temp+1) ;
		   double w = 0.0;
		   attrWeights.addEnd(w) ; // class has no weight, we just skip it
		   int idx = 0 ;
		   char delimiters[] = " ,;" ;
		   char *token = myTokenMDskip(key, idx, delimiters); // we skip first, which is the number of weights
		   token = myTokenMDskip(key, idx, delimiters); // get next one
		   while (token != 0) {
			   sscanf(token, "%lf", &w) ;
			   attrWeights.addEnd(w) ;
			   token = myTokenMDskip(key, idx, delimiters); // get next one
		   }
		   if (attrWeights.filled() != temp+1){
			  attrWeights.destroy() ;
              merror("number of attrWeights (weights of the attributes in the distance measure) is different than declared", "") ;
		   }
	   }
	}
	// stoping options

    else if (strcmp(keyword, "minNodeWeight")==0) {
       // Minimal weight of a node to split
       sscanf(key,"%lf", &dtemp) ;
       if (dtemp >= 0.0)
         minNodeWeight = dtemp ;
       else
          merror("minNodeWeight (minimal weight of a tree node) should be non-negative","") ;
	}
    else if (strcmp(keyword, "relMinNodeWeight")==0) {
       // Proportion of all examples in a node to stop
       sscanf(key,"%lf", &dtemp) ;
       if (dtemp >= 0.0 && dtemp <=1.0)
          relMinNodeWeight = dtemp ;
       else
          merror("relMinNodeWeight (minimal proportion of training instances in a tree node) should be between 0 and 1","") ;
	}
    else if (strcmp(keyword, "majorClassProportion")==0) {
       // Majority class proportion in a node
       sscanf(key,"%lf", &dtemp) ;
       if (dtemp >= 0.0 && dtemp <=1.0)
          majorClassProportion = dtemp ;
       else
         merror("majorClassProportion (proportion of majority class in a tree node) should be between 0 and 1", "") ;
	}
    else if (strcmp(keyword, "rootStdDevProportion")==0) {
       // Proportion of root's standard deviation to create a leaf
       sscanf(key,"%lf", &dtemp) ;
       if (dtemp >= 0.0 && dtemp <=1.0)
          rootStdDevProportion = dtemp ;
       else
          merror("rootStdDevProportion (proprtion of root's standard deviation in a tree node) should be between 0 and 1", "") ;
	}

    // Building options

    else if (strcmp(keyword, "selectionEstimator")==0) {
	   // selection estimator
       sscanf(key,"%d", &temp) ;
       if (temp > 0 && temp <= NoEstimators)
         selectionEstimator = temp ;
	   else {
         sprintf(errBuf, "selectionEstimator (estimator for selection of attributes and binarization in classification) should be one of existing (1-%d)", NoEstimators) ;
		 merror(errBuf, "") ;
	   }
	}
    else if (strcmp(keyword, "selectionEstimatorReg")==0) {
	   // selection estimator
       sscanf(key,"%d", &temp) ;
       if (temp > 0 && temp <= NoEstimatorsReg)
         selectionEstimatorReg = temp ;
	   else {
         sprintf(errBuf, "selectionEstimatorReg (estimator for selection of attributes and binarization in regression) should be one of existing (1-%d)", NoEstimatorsReg) ;
		 merror(errBuf, "") ;
	   }
	}
    else if (strcmp(keyword, "minReliefEstimate")==0) {
      // Minimal ReliefF's estimate of attribute to consider it further
      sscanf(key,"%lf", &dtemp) ;
      if (dtemp >= -1.0 && dtemp <= 1.0)
         minReliefEstimate = dtemp ;
      else
        merror("minReliefEstimate (minimal Relief's estimate of an attribute) should be in [-1, 1]", "") ;
	}
    else if (strcmp(keyword, "minInstanceWeight")==0) {
  	   // Minimal weight of an instance
       sscanf(key,"%lf", &dtemp) ;
       if (dtemp > 0.0 && dtemp <= 1.0)
         minInstanceWeight = dtemp ;
       else
         merror("minInstanceWeight (minimal weight of an instance) should be between 0 and 1", "") ;
	}
    else if (strcmp(keyword, "modelType")==0) {
       // Type of classification models used in the leafs (1-majority class, 2-kNN, 3-kNN with kernel, 4-simple Bayes):
       sscanf(key,"%d", &temp) ;
       if (temp >= 1 && temp <= 4)
         modelType = temp ;
       else
         merror("modelType (type of classification models used in the leafs) should be 1-4", "") ;
	}
    else if (strcmp(keyword, "modelTypeReg")==0) {
       // Type of regression models used in the leafs
       sscanf(key,"%d", &temp) ;
       if (temp >= 1 && temp <= 8)
         modelTypeReg = temp ;
       else
         merror("modelTypeReg (type of regression models used in the leafs) should be 1-8", "") ;
	}
    else if (strcmp(keyword, "kInNN")==0) {
       // k in kNN models
       sscanf(key,"%d", &temp) ;
       if (temp >= 0 )
         kInNN = temp ;
       else
         merror("kInNN (number of neighbours in k-nearest neighbours models) should be positive", "") ;
	}
    else if (strcmp(keyword, "nnKernelWidth")==0) {
       // kernel width in kNN models
       sscanf(key,"%lf", &dtemp) ;
       if (dtemp > 0.0)
          nnKernelWidth = dtemp ;
       else
         merror("nnKernelWidth (kernel width in k-nearest neighbours models) should be positive","") ;
	}
    else if (strcmp(keyword, "bayesDiscretization")==0) {
       // type or discretizationn for simple bayes
       sscanf(key,"%d", &temp) ;
       if (temp >= 1 && temp <= 2)
          bayesDiscretization = temp ;
       else
         merror("bayesDiscretization (discretization for naive Bayes) should be 1 or 2", "") ;
	}
    else if (strcmp(keyword, "bayesEqFreqIntervals")==0) {
  	   // number of intervals for equal frequency discretization for simple Bayes models
       sscanf(key,"%d", &temp) ;
       if (temp > 1)
         bayesEqFreqIntervals = temp ;
       else
          merror("bayesEqFreqIntervals (number of intervals in equal-frequency discretization for naive Bayes models) should be greater than 1", "") ;
	}
    // Constructive induction options

    else if (strcmp(keyword, "constructionMode")==0) {
  	   // which constructive operators to use
       sscanf(key,"%d", &temp) ;
       if (temp >= 0 && temp <= cSINGLEattribute+cCONJUNCTION+cSUM+cPRODUCT)
           constructionMode = temp | cSINGLEattribute ;  // cSINGLEattribute MUST be included
       else
          merror("constructionMode (selection of construction operatorts) contains unknown operators", "") ;
	}
    else if (strcmp(keyword, "constructionDepth")==0) {
       // depth  constructive induction
       sscanf(key,"%d", &temp) ;
       if (temp >= 0)
          constructionDepth = temp ;
       else
          merror("constructionDepth (depth of the tree where construction is applied) should be non-negative", "") ;
	}
    else if (strcmp(keyword, "noCachedInNode")==0) {
       // how many attributes to cache in each node
       sscanf(key,"%d", &temp) ;
       if (temp >= 0)
          noCachedInNode = temp ;
       else
           merror("noCachedInNode (number of cached constructs in eaach construction node) should be non-negative", "") ;
	}
    else if (strcmp(keyword, "constructionEstimator")==0) {
       // construction estimator
       sscanf(key,"%d", &temp) ;
       if (temp > 0 && temp <= NoEstimators)
          constructionEstimator = temp ;
	   else {
         sprintf(errBuf, "constructionEstimator (estimator of constructs in classification) should be one of existing (1-%d)", NoEstimators) ;
		 merror(errBuf, "") ;
	   }
	}
    else if (strcmp(keyword, "constructionEstimatorReg")==0) {
       // construction estimator
       sscanf(key,"%d", &temp) ;
       if (temp > 0 && temp <= NoEstimatorsReg)
          constructionEstimatorReg = temp ;
	   else {
         sprintf(errBuf, "constructionEstimatorReg (estimator of constructsin regression) should be one of existing (1-%d)", NoEstimatorsReg) ;
		 merror(errBuf, "") ;
	   }
	}
    else if (strcmp(keyword, "beamSize")==0) {
       // beam size for beam search
       sscanf(key,"%d", &temp) ;
       if (temp > 0 )
          beamSize = temp ;
       else
          merror("beamSize (size of the beam in constructive induction) should be greater than 0", "") ;
	}
    else if (strcmp(keyword, "maxConstructSize")==0) {
      // maximal size of constructs
      sscanf(key,"%d", &temp) ;
      if (temp > 0 )
         maxConstructSize = temp ;
      else
         merror("maxConstructSize (maximal size of constructs) should be greater than 0", "") ;
	}
    else if (strcmp(keyword, "discretizationLookahead")==0) {
       // Number of times current discretization can be worse than the best
       sscanf(key,"%d", &temp) ;
       if (temp >= 0)
         discretizationLookahead = temp ;
       else
         merror("discretizationLookahead (number of times current discretization can be worse than the best) should be non-negative", "") ;
	}
    else if (strcmp(keyword, "discretizationSample")==0) {
       // Maximal number of points to try discretization with Relief
      sscanf(key,"%d", &temp) ;
      if (temp >= 0)
        discretizationSample = temp ;
      else
         merror("discretizationSample (maximal number of points to try discretization with Relief) should be non-negative","") ;
	}

    // Pruning options

    else if (strcmp(keyword, "selectedPruner")==0) {
       // selected pruner in classification (0-none, 1-m-estimate)
       sscanf(key,"%d", &temp) ;
       if (temp >= 0 && temp <= 1)
          selectedPruner = temp ;
       else
         merror("selectedPruner (the tree pruning method in classification) should be one of existing (0-1)", "") ;
	}
    else if (strcmp(keyword, "selectedPrunerReg")==0) {
       // selected pruner in regression (0-none, 1-MDL, 2-m-estimate, 3-M5, 4-Error complexity (fixed alpha))
       sscanf(key,"%d", &temp) ;
       if (temp >= 0 && temp <= 2)
          selectedPrunerReg = temp ;
       else
         merror("selectedPrunerReg (the tree pruning method in regression) should be one of existing (0-4)", "") ;
	}
    else if (strcmp(keyword, "mdlModelPrecision")==0) {
      // Precision of the model coefficients in MDL
      sscanf(key,"%lf", &dtemp) ;
      if (dtemp > 0.0 )
         mdlModelPrecision = dtemp ;
      else
         merror("mdlModelPrecision (precision of the model coefficients in MDL pruning) should be positive","") ;
	}
    else if (strcmp(keyword, "mdlErrorPrecision")==0) {
      // Precision of the error in MDL
      sscanf(key,"%lf", &dtemp) ;
      if (dtemp > 0.0 )
         mdlErrorPrecision = dtemp ;
      else
         merror("mdlErrorPrecision (precision of the error in MDL pruning) should be positive","") ;
	}
    else if (strcmp(keyword, "mEstPruning")==0) {
       // m - estimate for pruning
       sscanf(key,"%lf", &dtemp) ;
       if (dtemp>=0)
         mEstPruning = dtemp ;
       else
          merror("mEstPruning (m-estimate for pruning) should be non-negative","") ;
	}
    else if (strcmp(keyword, "alphaErrorComplexity")==0) {
       // m - estimate for pruning
       sscanf(key,"%lf", &dtemp) ;
       alphaErrorComplexity = dtemp ;
	}

    // Random forests options

    else if (strcmp(keyword, "rfNoTrees")==0) {
      // number of trees in the forest
      sscanf(key,"%d", &temp) ;
      if (temp>0)
        rfNoTrees = temp ;
      else
         merror("rfNoTrees (number of trees in the random forest) should be positive","") ;
	}
    else if (strcmp(keyword, "rfNoSelAttr")==0) {
      // Number of randomly selected attributes in the node
      sscanf(key,"%d", &temp) ;
      if (temp>=-2)
         rfNoSelAttr = temp ;
      else
        merror("rfNoSelAttr (number of randomly selected attributes in tree nodes) should be >=-2","") ;
	}
    else if (strcmp(keyword, "rfMultipleEst")==0) {
      // Use multiple estimators
	  if (key[0] == 'y' || key[0] == 'Y')
	     rfMultipleEst = mTRUE ;
	  else if (key[0] == 'n' || key[0] == 'N')
	     rfMultipleEst = mFALSE ;
	  else
		  merror("rfMultipleEst (use of multiple estimators in the forest) should be on or off (Y or N)","") ;
	}
    else if (strcmp(keyword, "rfkNearestEqual")==0) {
      // Number of nearest instances for weighted rf classification
      sscanf(key,"%d", &temp) ;
      if (temp>=0)
        rfkNearestEqual = temp ;
      else
        merror("rfkNearestEqual (number of nearest instances for random forest weighting) should be nonnegative","") ;
	}
    else if (strcmp(keyword, "rfPropWeightedTrees")==0) {
      // proportion of trees where attribute probabilities are weighted with ReliefF
	  sscanf(key,"%lf", &dtemp) ;
      if (dtemp >=0 && dtemp <= 1.0)
	    rfPropWeightedTrees = dtemp ;
	  else
		merror("rfPropWeightedTrees (proportion of trees where attribute probabilities are weighted with ReliefF) should be between 0 and 1","") ;
	}
    else if (strcmp(keyword, "rfPredictClass")==0) {
      // Predict with majority class, otherwise use class distribution
	  if (key[0] == 'y' || key[0] == 'Y')
	    rfPredictClass = mTRUE ;
	  else if (key[0] == 'n' || key[0] == 'N')
	    rfPredictClass = mFALSE ;
	  else
		  merror("rfPredictClass (predict with majority class) should be on or off (Y or N)", "") ;
	}
    else if (strcmp(keyword, "rfAttrEvaluate")==0) {
      // Evaluate attributes with out-of-bag evaluation
	  if (key[0] == 'y' || key[0] == 'Y')
	    rfAttrEvaluate = mTRUE ;
	  else if (key[0] == 'n' || key[0] == 'N')
	     rfAttrEvaluate = mFALSE ;
	  else
		  merror("rfAttrEvaluate (evaluate attributes with random forest out-of-bag evaluation) should be on or off (Y or N)", "") ;
	}
    else if (strcmp(keyword, "rfSampleProp")==0) {
	   // proportion of the training examples to be used in learning (0.0-bootstrap replication)
	   sscanf(key,"%lf", &dtemp) ;
       if (dtemp >= 0.0 && dtemp <= 1.0)
	     rfSampleProp = dtemp ;
	   else
		  merror("rfSampleProp (proportion of the the training examples to be used in learning should be between 0.0 and 1.0", "") ;
	}
    else if (strcmp(keyword, "rfNoTerminals")==0) {
      // Number of leaves in the individual trees (0-build a whole tree)
      sscanf(key,"%d", &temp) ;
      if (temp >= 0)
         rfNoTerminals = temp ;
      else
		 merror("rfNoTerminals (number of leaves in each tree) should be nonnegative","") ;
	}
	// Type of regularization (0-no regularization, 1-global regularization, 2-local regularization)
    else if (strcmp(keyword, "rfRegType")==0) {
       sscanf(key,"%d", &temp) ;
       if (temp >= 0 && temp <= 2)
          rfRegType = temp ;
       else
		  merror("rfRegType (type of regularization) should be 0, 1, or 2","") ;
	}
    else if (strcmp(keyword, "rfRegLambda")==0) {
  	  // Regularization parameter Lambda
	  sscanf(key,"%lf", &dtemp) ;
      if (dtemp >= 0.0)
	     rfRegLambda = dtemp ;
	  else
		 merror("rfRegLambda (regularization parameter lambda) should be nonnegative", "") ;
	}
    else if (strcmp(keyword, "rfRndSeed")==0) {
       // Random seed for random forests
       sscanf(key,"%ld", &rfRndSeed) ;
       if (rfRndSeed == 0)
          rfRndSeed = -(long)time(NULL) ;
	}

	// Other options

    else if (strcmp(keyword, "mEstPrediction")==0) {
	  // m - estimate for prediction
	  sscanf(key,"%lf", &dtemp) ;
      if (dtemp>=0)
        mEstPrediction = dtemp ;
      else
        merror("mEstPrediction (m-estimate for prediction) should be nonnegative","") ;
	}
    else if (strcmp(keyword, "printTreeInDot")==0) {
 	  // print tree in dot format as well
	  if (key[0] == 'y' || key[0] == 'Y')
		 printTreeInDot = mTRUE ;
	  else if (key[0] == 'n' || key[0] == 'N')
		 printTreeInDot = mFALSE ;
	  else
		 merror("printTreeInDot (print tree also in dot format) should be on or off (Y or N)", "") ;
	}
    else if (strcmp(keyword, "outProbDistr")==0) {
	  // output class probability distribution
	  if (key[0] == 'y' || key[0] == 'Y')
		 outProbDistr = mTRUE ;
	  else if (key[0] == 'n' || key[0] == 'N')
		 outProbDistr = mFALSE ;
	  else
		 merror("outProbDistr (output class probability distribution) should be on or off (Y or N)", "") ;
	}
    else if (strcmp(keyword, "defaultEditor")==0) {
      //  default editor
      if (strlen(key) > 0)
		 strcpy(defaultEditor, key) ;
	}
    else if (strcmp(keyword, "NAstring")==0) {
	  //  missing value indicator
      if (strlen(key) > 0)
		strcpy(NAstring, key) ;
	}
	else {
  	   merror("unrecognized option", keyword) ;
	}
	}
    }
}
