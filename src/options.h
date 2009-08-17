#if !defined(OPTIONS_H)
#define OPTIONS_H

#include "general.h"
#include "contain.h"

enum splitSelectionType {FROM_FILES=0, CROSS_VALIDATION=1, STRATIFIED_CV=2,
                         LOOCV=3, ALL_TRAINING=4, RANDOM_SPLIT=5 } ;

class Options {
public:
   Options(void) { setDefault() ; }

   // command line options
   char optionFile[MaxFileNameLen] ;
   char action[MaxNameLen] ;

   // data options
   char domainName[MaxFileNameLen] ;
   char dataDirectory[MaxPath] ;
   char resultsDirectory[MaxPath] ;
   char NAstring[MaxNameLen] ;
   int splitIdx ;
   int numberOfSplits ;
   splitSelectionType splitSelection ;
   double trainProportion ;
   long int rndSeedSplit ;


   // building options
   double minInstanceWeight ; // minimal probability of example to take it into further consideration
   double minReliefEstimate ; // minimal ReliefF's estimation to consider attribute worthy
   int selectionEstimator, constructionEstimator ;
   int selectionEstimatorReg, constructionEstimatorReg ;


   // attribute evaluation
   int attrEvaluationInstances ;  // maximal examples for estimation
   booleanT binaryAttributes ;
   booleanT binarySplitNumericAttributes ; // are numeric attributes' splits considered binary (or greedily discretized) in applicable measures
   int multiclassEvaluation;
   marray<booleanT> estOnReg;
   marray<booleanT> estOn;

   // ReliefF
   int ReliefIterations ; // number of ReliefF's main loops for estimation
   int kNearestEqual, kNearestExpRank  ;
   double quotientExpRankDistance ;
   double numAttrProportionEqual, numAttrProportionDifferent ;

   // ordEval
   int ordEvalNoRandomNormalizers ;
   booleanT ordEvalBootstrapNormalize ;
   //oeConfidenceInterval oeCI ;
   double ordEvalNormalizingPercentile;
   marray<double> attrWeights ;

   // stopping options
   double minNodeWeight ; // minimum number of examples in a node to split further on
   double relMinNodeWeight ; // minimal proportion of examples in a leaf to spit further
   double majorClassProportion ;
   double rootStdDevProportion ;

   //  models in trees
   int modelType, modelTypeReg ;  // type of models in leaves
   int kInNN ;
   double nnKernelWidth ;

   // constructive induction
   int constructionMode ; // what constructs to consider
   int constructionDepth ;
   int beamSize, maxConstructSize ;
   int noCachedInNode ;

   // discretization
   int discretizationLookahead ; // number of times current discretization can be worse than the best
   int discretizationSample ;
   int bayesDiscretization ;
   int bayesEqFreqIntervals ;

   // pruning
   int selectedPruner ;
   int selectedPrunerReg ;
   double mEstPruning ; // parameter for m-estimate in pruning
   double mEstPrediction ;
   double mdlModelPrecision ;
   double mdlErrorPrecision ;
   double alphaErrorComplexity ;

   // random forest options
   int rfNoTrees  ;
   int rfNoSelAttr ;
   booleanT rfMultipleEst ;
   int rfkNearestEqual ;
   double rfPropWeightedTrees ;
   booleanT rfPredictClass ;
   booleanT rfRandomBinarization ;
   booleanT rfAttrEvaluate ;
   double rfSampleProp ;
   int rfNoTerminals ;
   int rfRegType ;
   double rfRegLambda ;
   long int rfRndSeed ;

   // miscellaneous
   booleanT printTreeInDot ;
   booleanT outProbDistr ;
   char defaultEditor[MaxPath] ;

   // methods
   void setDefault(void) ;
   void processOptions(void) ;
   int readConfig(char* ConfigName) ;
   int readConfigFromString(char* optionsString) ;
   void outConfig(FILE *to) ;
   int writeConfig(char* ConfigName) ;
   void parseOption(char *optString, char *keyword, char *key) ;
   void assignOption(char *optString) ;
   void assignOption(char *keyword, char *key)  ;
   int optionsFromStrings(int noOptions, marray<char* > &optionsName, marray<char* > &optionsVal) ;



} ;

#endif
