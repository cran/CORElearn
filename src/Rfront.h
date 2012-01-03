#if !defined(RFRONT_H)
#define RFRONT_H

extern "C" {
void initCore(int *maxModels) ;
void versionCore(char **version) ;
void buildCoreModel(int *noInst, int *noDiscrete, int *noDiscVal,
		int *discData, int *noNumeric, double *numData, double *costMx,
		char **dscAttrNames, char **dscValNames, char ** nmAttrNames,
		int *noOptions, char **optName, char **optValue,
		int *modelID, int *noClasses, double *priorClassProb, double *avgPrediction
) ;
void destroyOneCoreModel(int* modelID) ;
void predictWithCoreModel(int *modelID, int *noInst, int *discData,
		double *numData, double *costMx, int *returnPred, double *returnProb,
		double *returnPredReg, int *noOptions, char **optName, char **optValue) ;
void destroyCore() ;
void simRcall() ;
void availableEstimatorsCore(char **estBrief) ;
void estimateCore(int *noInst, int *noDiscrete,
		int *noDiscVal, int *discData, int *noNumeric, double *numData, double *costMx,
		char **dscAttrNames, char **dscValNames, char ** nmAttrNames,
        int *noOptions, char **optName, char **optValue,
		int *selectedEstimator, double *discEst, double *numEst) ;
void estimateCoreReg(int *noInst, int *noDiscrete,
		int *noDiscVal, int *discData, int *noNumeric, double *numData,
		char **dscAttrNames, char **dscValNames, char ** nmAttrNames,
        int *noOptions, char **optName, char **optValue,
		int *selectedEstimator, double *discEst, double *numEst) ;
void ordEvalCore(int *noInst, int *noDiscrete, int *noDiscVal, int *discData,
		char **dscAttrNames, char **dscValNames,
		int *noOptions, char **optName, char **optValue,
		double *rePos, double *reNeg, double *anch, double *rndrePos, double *rndreNeg, double *rndAnch,
		int *noAttrVal, char **ordEvalFile, char **ordEvalRndFile, int *variant) ;
void modelEvaluate(int *noInst, int *correctCl, int *predictedCl,
		double *predictedPr, double *costMx, int *noClasses, double *priorClProbability,
		double *accuracy, double *avgCost,
		double *infScore, double *auc, int *predictionMx, double *sensitivity,
		double *specificity, double *brier, double *kappa, double *precision, double *Gmean,
		double *KS, double *TPR, double *FPR) ;
void modelEvaluateReg(int *noInst, double *truePred,
		double *pred, double *avgPrediction, double *MSE, double *RMSE, double *MAE, double *RMAE) ;
void calibrate(int *calMethod, int *noInst, int *correctCl, double *predictedPr, double *wght, int *noBins,
		       int *noIntervals, double *interval, double *calProb);
void rfAttrEval(int *modelID, double *estOut) ;

}

#endif /*RFRONT_H_*/
