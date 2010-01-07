// the frontend file for call from R

#include <stdio.h>
#include <string.h>
#include <time.h>

#include "general.h"  // general constants and data type definitions
// here you specify weather to compile for  Windows or UNIX

#if defined(R_PORT)
#include <R.h>
#include <Rinternals.h>
#else
#define Rprintf printf
#endif
#if defined(DEBUG)
#if defined(MICROSOFT)
#include <malloc.h>  // for heapcheck
#endif
#endif

#include "error.h"    // joint method of reporting errors
#include "dataStore.h"  // frame for data
#include "ftree.h"    // decision tree with feature construction
#include "regtree.h"
#include "rndforest.h"  // random forests
#include "utils.h"    // various utillities eg. computing of std. dev.
#include "estimator.h"
#include "estimatorReg.h"
#include "utils.h"
#include "options.h"
#include "printUtil.h"
#include "calibrate.h"
#include "Rfront.h"

#if defined(DEBUG_NEW)
extern long int SetSize;
#endif

extern int NoEstimators;
extern int NoEstimatorsReg;
extern estDsc estName[];
extern estDsc estNameReg[];
extern char VersionString[];

extern "C" {

marray<dataStore*> allModels; // stores pointers to all the active models, one for each

// on entry to library
void initCore(int *maxModels) {
	destroyCore(); // in case of multiple initializations
	allModels.create(*maxModels, 0);
	allModels.setFilled(*maxModels);
}

//for debugging
void testNA(int *t, double *x, int *a) {
	double y;
	//union ieee_double u;
	if (*t == 1) {
		*x = NAcont;
	} else if (*t == 2) {
		y = 0.0;
		*x = y / y;
	}
	//u.d = *x;
	//printf("%08x  %08x", u.i.p1, u.i.p0); // OK for little endian
	a[0] = isNAcont(*x);
	a[1] = isNaN(*x);
}

void testRPORT(int *a) {
#if defined(R_PORT)
	*a = 1;
#else
	*a = 0;
#endif
}

void testCoreRand(int *n, double *x) {
	testRand(n, x);
}

// library version
void versionCore(char **version) {
	strcpy(version[0], VersionString);
}

// build the model from the data
void buildCoreModel(int *noInst, int *noDiscrete, int *noDiscVal,
		int *discData, int *noNumeric, double *numData, double *costMx,
		char **dscAttrNames, char **dscValNames, char ** nmAttrNames,
		int *noOptions, char **optName, char **optValue, int *modelID,
		int *noClasses, double *priorClassProb, double *avgPrediction) {
#if defined(R_PORT)
	GetRNGstate();
#endif
	// wrap arrays for security reasons
	marray<int> noDiscreteValues, discreteData;
	noDiscreteValues.wrap(*noDiscrete, noDiscVal);
	discreteData.wrap(*noDiscrete * (*noInst), discData);
	marray<double> numericData, costMatrix, priorClProb;
	numericData.wrap(*noNumeric * (*noInst), numData);
	priorClProb.wrap(256, priorClassProb);
	marray<char*> optionsName, optionsValue;
	optionsName.wrap(*noOptions, optName);
	optionsValue.wrap(*noOptions, optValue);
	marray<char *> discAttrNames, discValNames, numAttrNames;
	if (dscAttrNames && dscAttrNames[0]) {
		discAttrNames.wrap(*noDiscrete, dscAttrNames);
		discValNames.wrap(*noDiscrete, dscValNames);
	} else {
		discAttrNames.create(*noDiscrete, 0);
		discValNames.create(*noDiscrete, 0);
	}
	if (nmAttrNames && nmAttrNames[0]) {
		numAttrNames.wrap(*noNumeric, nmAttrNames);
	} else {
		numAttrNames.create(*noNumeric, 0);
	}

	int dummy;
	booleanT isRegression = mFALSE;
	Options *opt = new Options;
	dataStore *model = 0;
	featureTree *dT = 0;
	regressionTree *rT = 0;
	dataStore *data = 0;
	*modelID = allModels.memberPlace(model);
	// if there is no more space in the table
	if (*modelID < 0) {
		Rprintf("maximum number of models reached\n");
		delete opt;
		goto cleanUp;
	}
	// read options
	opt->optionsFromStrings(*noOptions, optionsName, optionsValue);

	if (strcmp(opt->action, "tree") == 0 || strcmp(opt->action, "bayes") == 0
			|| strcmp(opt->action, "knn") == 0 || strcmp(opt->action,
			"knnKernel") == 0 || strcmp(opt->action, "rf") == 0 || strcmp(
			opt->action, "rfNear") == 0)
		isRegression = mFALSE;
	else if (strcmp(opt->action, "regTree") == 0)
		isRegression = mTRUE;
	else {
		merror("unknown action: %s\n", opt->action);
		*modelID = -1;
		delete opt;
		goto cleanUp;
	}

	if (isRegression) {
		allModels[*modelID] = new regressionTree;
		rT = (regressionTree*) allModels[*modelID]; // working model
		data = (dataStore*) rT;
	} else {
		allModels[*modelID] = new featureTree;
		dT = (featureTree*) allModels[*modelID]; // working model
		data = (dataStore*) dT;
	}
	data->opt = opt;

	// data is passed from R
	data->isRegression = isRegression;
	// prepare data, first description than matrixes
	// the data structures are defined in dataStore class
	data->dscFromR(*noDiscrete, noDiscreteValues, *noNumeric, isRegression,
			discAttrNames, discValNames, numAttrNames);
	data->dataFromR(*noInst, discreteData, numericData, mTRUE);
	costMatrix.wrap(data->noClasses * data->noClasses, costMx);
	data->costsFromR(costMatrix);

	// prepare data for training/testing
	data->opt->splitSelection = ALL_TRAINING;
	data->prepareDataSplits();
	data->setDataSplit(data->opt->splitIdx);

	// copy some data to output
	*noClasses = data->noClasses;
	if (!data->isRegression)
		for (int c = 1; c <= *noClasses; c++)
			priorClProb[c - 1] = data->AttrDesc[0].valueProbability[c];

	if (strcmp(data->opt->action, "tree") == 0) {
		dT->learnRF = mFALSE;
		dT->constructTree();
	} else if (strcmp(data->opt->action, "bayes") == 0) {
		dT->learnRF = mFALSE;
		dT->opt->minNodeWeight = dT->NoCases + 1;
		dT->opt->modelType = 4;
		dT->constructTree();
	} else if (strcmp(data->opt->action, "knn") == 0) {
		dT->learnRF = mFALSE;
		dT->opt->minNodeWeight = dT->NoCases + 1;
		dT->opt->modelType = 2;
		dT->constructTree();
	} else if (strcmp(data->opt->action, "knnKernel") == 0) {
		dT->learnRF = mFALSE;
		dT->opt->minNodeWeight = dT->NoCases + 1;
		dT->opt->modelType = 3;
		dT->constructTree();
	} else if (strcmp(data->opt->action, "rf") == 0) {
		dT->learnRF = mTRUE;
		dT->opt->rfkNearestEqual = 0; // force to zero, so that memory can be released
#if !defined(R_PORT)
		randSeed(dT->opt->rfRndSeed);
#endif
		dT->buildForest();
	} else if (strcmp(data->opt->action, "rfNear") == 0) {
		dT->learnRF = mTRUE;
#if !defined(R_PORT)
		randSeed(dT->opt->rfRndSeed);
#endif
		dT->buildForest();
	} else if (strcmp(data->opt->action, "regTree") == 0) {
		rT->constructRegTree();
	} else {
		Rprintf("unknown action: %s\n", dT->opt->action);
		delete allModels[*modelID];
		allModels[*modelID] = 0;
		*modelID = -1;
	}

	if (data->isRegression)
		*avgPrediction = rT->rootAverage;
	// for some models we can destroy the data here
	if ((strcmp(data->opt->action, "tree") == 0 && (data->opt->modelType == 1
			|| data->opt->modelType == 4))
			|| strcmp(data->opt->action, "bayes") == 0)
		data->clearData(mTRUE);

	// unwrap arrays
	cleanUp: noDiscreteValues.unWrap(dummy);
	discreteData.unWrap(dummy);
	numericData.unWrap(dummy);
	priorClProb.unWrap(dummy);

	costMatrix.unWrap(dummy);
	optionsName.unWrap(dummy);
	optionsValue.unWrap(dummy);
	if (dscAttrNames && dscAttrNames[0]) {
		discAttrNames.unWrap(dummy);
		discValNames.unWrap(dummy);
	}
	if (nmAttrNames && nmAttrNames[0])
		numAttrNames.unWrap(dummy);

#if defined(R_PORT)
	PutRNGstate();
#endif
}

void destroyOneCoreModel(int* modelID) {
	// is modelID valid
	if (*modelID >= 0 && *modelID < allModels.len() && allModels[*modelID] != 0) {
		dataStore *data = allModels[*modelID];
		if (data->isRegression)
			delete (regressionTree*) allModels[*modelID];
		else
			delete (featureTree*) allModels[*modelID];
		allModels[*modelID] = 0;
		*modelID = -1;
	}
}

// return class value and its probability for given data with selected model
void predictWithCoreModel(int *modelID, int *noInst, int *discData,
		double *numData, double *costMx, int *returnPred, double *returnProb,
		double *returnPredReg, int *noOptions, char **optName, char **optValue) {
	// is modelID valid
	if (*modelID < 0 || *modelID >= allModels.len() || allModels[*modelID] == 0)
		return;
	dataStore *data = allModels[*modelID]; // working Model

	// wrap arrays for security reasons
	marray<int> discreteData, returnPredicted;
	discreteData.wrap(data->noDiscrete * (*noInst), discData);
	returnPredicted.wrap(*noInst, returnPred);
	marray<double> numericData, costMatrix, returnProbability,
			returnPredictedReg;
	numericData.wrap(data->noNumeric * (*noInst), numData);
	costMatrix.wrap(data->noClasses * data->noClasses, costMx);
	returnProbability.wrap(data->noClasses * (*noInst), returnProb);
	returnPredictedReg.wrap(*noInst, returnPredReg);
	marray<char*> optionsName, optionsValue;
	optionsName.wrap(*noOptions, optName);
	optionsValue.wrap(*noOptions, optValue);

	// read options
	data->opt->optionsFromStrings(*noOptions, optionsName, optionsValue);

	// prepare prediction data, which should have the same description as training one
	data->dataFromR(*noInst, discreteData, numericData, mFALSE);
	data->costsFromR(costMatrix);

	if (data->isRegression)
		((regressionTree*) data)->predictRreg(returnPredictedReg);
	else
		((featureTree*) data)->predictR(returnPredicted, returnProbability);

	// we can destroy the prediction data
	data->clearData(mFALSE);

	int dummy;
	discreteData.unWrap(dummy);
	numericData.unWrap(dummy);
	costMatrix.unWrap(dummy);
	optionsName.unWrap(dummy);
	optionsValue.unWrap(dummy);
	returnPredicted.unWrap(dummy);
	returnProbability.unWrap(dummy);
	returnPredictedReg.unWrap(dummy);
}

// destroy all the models
void destroyCore() {
	int i;
	for (i = 0; i < allModels.len(); i++)
		destroyOneCoreModel(&i);
	allModels.destroy();

#if defined(DEBUG)
#if defined(MICROSOFT)
	/* Check heap status */
	int heapstatus = _heapchk();
	if (heapstatus!= _HEAPOK)
	fprintf(stderr, "\nWARNING: Heap is not OK !!");
	// _HEAPOK, _HEAPEMPTY, _HEAPBADBEGIN, _HEAPBADNODE
#endif
#endif

#if defined(DEBUG_NEW)
	printf("Still alocated memory blocks: %ld\n",SetSize);
#endif
}

void availableEstimatorsCore(char **estBrief) {
	strcpy(estBrief[0], "");
	for (int i = 1; i <= NoEstimators; i++) {
		strcat(estBrief[0], estName[i].brief);
		if (i < NoEstimators)
			strcat(estBrief[0], ",");
	}
}

void estimateCore(int *noInst, int *noDiscrete, int *noDiscVal, int *discData,
		int *noNumeric, double *numData, double *costMx, char **dscAttrNames,
		char **dscValNames, char ** nmAttrNames, int *noOptions,
		char **optName, char **optValue, int *selectedEstimator,
		double *discEst, double *numEst) {
#if defined(R_PORT)
	GetRNGstate();
#endif
	// wrap arrays for security reasons
	marray<int> noDiscreteValues, discreteData;
	noDiscreteValues.wrap(*noDiscrete, noDiscVal);
	discreteData.wrap(*noDiscrete * (*noInst), discData);
	marray<double> numericData, costMatrix;
	numericData.wrap(*noNumeric * (*noInst), numData);
	marray<char*> optionsName, optionsValue;
	optionsName.wrap(*noOptions, optName);
	optionsValue.wrap(*noOptions, optValue);
	marray<double> discreteEst, numericEst;
	numericEst.wrap(*noNumeric, numEst);
	discreteEst.wrap(*noDiscrete, discEst);
	marray<char *> discAttrNames, discValNames, numAttrNames;
	if (dscAttrNames && dscAttrNames[0]) {
		discAttrNames.wrap(*noDiscrete, dscAttrNames);
		discValNames.wrap(*noDiscrete, dscValNames);
	} else {
		discAttrNames.create(*noDiscrete, 0);
		discValNames.create(*noDiscrete, 0);
	}
	if (nmAttrNames && nmAttrNames[0]) {
		numAttrNames.wrap(*noNumeric, nmAttrNames);
	} else {
		numAttrNames.create(*noNumeric, 0);
	}

	int dummy, i;
	featureTree *dT = new featureTree;

	// read options
	dT->opt->optionsFromStrings(*noOptions, optionsName, optionsValue);
	// reset estimation options
	dT->opt->estOn.init(mFALSE);
	dT->opt->estOn[*selectedEstimator] = mTRUE;

	// prepare data, first description than matrixes
	//marray<char *> discAttrNames(*noDiscrete, 0), discValNames(*noDiscrete, 0), numAttrNames(*noNumeric, 0);
	dT->dscFromR(*noDiscrete, noDiscreteValues, *noNumeric, mFALSE,
			discAttrNames, discValNames, numAttrNames);
	dT->dataFromR(*noInst, discreteData, numericData, mTRUE);
	costMatrix.wrap(dT->noClasses * dT->noClasses, costMx);
	dT->costsFromR(costMatrix);
	// prepare data for training/testing
	dT->opt->splitSelection = ALL_TRAINING;
	dT->prepareDataSplits();
	dT->setDataSplit(dT->opt->splitIdx);

	marray<double> weight(dT->NoTrainCases, 1.0);
	attributeCount attrType;
	estimation Estimator(dT, dT->DTraining, weight, dT->NoTrainCases);
	Estimator.estimate(*selectedEstimator, 0, dT->noNumeric, 1, dT->noDiscrete,
			attrType);

	discreteEst[0] = NAcont;
	for (i = 1; i < dT->noDiscrete; i++)
		discreteEst[i] = Estimator.DiscEstimation[i];
	for (i = 0; i < dT->noNumeric; i++)
		numericEst[i] = Estimator.NumEstimation[i];

	// unwrap arrays
	noDiscreteValues.unWrap(dummy);
	discreteData.unWrap(dummy);
	numericData.unWrap(dummy);
	costMatrix.unWrap(dummy);
	optionsName.unWrap(dummy);
	optionsValue.unWrap(dummy);
	numericEst.unWrap(dummy);
	discreteEst.unWrap(dummy);
	if (dscAttrNames && dscAttrNames[0]) {
		discAttrNames.unWrap(dummy);
		discValNames.unWrap(dummy);
	}
	if (nmAttrNames && nmAttrNames[0])
		numAttrNames.unWrap(dummy);

	delete dT;

#if defined(R_PORT)
	PutRNGstate();
#endif
}

void estimateCoreReg(int *noInst, int *noDiscrete, int *noDiscVal,
		int *discData, int *noNumeric, double *numData, char **dscAttrNames,
		char **dscValNames, char **nmAttrNames, int *noOptions, char **optName,
		char **optValue, int *selectedEstimator, double *discEst,
		double *numEst) {
#if defined(R_PORT)
	GetRNGstate();
#endif
	// wrap arrays for security reasons
	marray<int> noDiscreteValues, discreteData;
	noDiscreteValues.wrap(*noDiscrete, noDiscVal);
	discreteData.wrap(*noDiscrete * (*noInst), discData);
	marray<double> numericData, costMatrix;
	numericData.wrap(*noNumeric * (*noInst), numData);
	marray<char*> optionsName, optionsValue;
	optionsName.wrap(*noOptions, optName);
	optionsValue.wrap(*noOptions, optValue);
	marray<double> discreteEst, numericEst;
	numericEst.wrap(*noNumeric, numEst);
	discreteEst.wrap(*noDiscrete, discEst);
	marray<char *> discAttrNames, discValNames, numAttrNames;
	if (dscAttrNames && dscAttrNames[0]) {
		discAttrNames.wrap(*noDiscrete, dscAttrNames);
		discValNames.wrap(*noDiscrete, dscValNames);
	} else {
		discAttrNames.create(*noDiscrete, 0);
		discValNames.create(*noDiscrete, 0);
	}
	if (nmAttrNames && nmAttrNames[0]) {
		numAttrNames.wrap(*noNumeric, nmAttrNames);
	} else {
		numAttrNames.create(*noNumeric, 0);
	}

	int dummy, i;
	regressionTree *rT = new regressionTree;

	// read options
	rT->opt->optionsFromStrings(*noOptions, optionsName, optionsValue);
	// reset estimation options
	rT->opt->estOnReg.init(mFALSE);
	rT->opt->estOnReg[*selectedEstimator] = mTRUE;

	// prepare data, first description than matrixes
	//marray<char *> discAttrNames(*noDiscrete, 0), discValNames(*noDiscrete, 0), numAttrNames(*noNumeric, 0);
	rT->dscFromR(*noDiscrete, noDiscreteValues, *noNumeric, mTRUE,
			discAttrNames, discValNames, numAttrNames);
	rT->dataFromR(*noInst, discreteData, numericData, mTRUE);
	// prepare data for training/testing
	rT->opt->splitSelection = ALL_TRAINING;
	rT->prepareDataSplits();
	rT->setDataSplit(rT->opt->splitIdx);

	marray<double> weight(rT->NoTrainCases, 1.0);
	attributeCount attrType;
	estimationReg Estimator(rT, rT->DTraining, weight, rT->NoTrainCases);
	Estimator.estimate(*selectedEstimator, 1, rT->noNumeric, 0, rT->noDiscrete,
			attrType);

	numericEst[0] = NAcont;
	for (i = 0; i < rT->noDiscrete; i++)
		discreteEst[i] = Estimator.DiscEstimation[i];
	for (i = 1; i < rT->noNumeric; i++)
		numericEst[i] = Estimator.NumEstimation[i];

	// unwrap arrays
	noDiscreteValues.unWrap(dummy);
	discreteData.unWrap(dummy);
	numericData.unWrap(dummy);
	optionsName.unWrap(dummy);
	optionsValue.unWrap(dummy);
	numericEst.unWrap(dummy);
	discreteEst.unWrap(dummy);
	if (dscAttrNames && dscAttrNames[0]) {
		discAttrNames.unWrap(dummy);
		discValNames.unWrap(dummy);
	}
	if (nmAttrNames && nmAttrNames[0])
		numAttrNames.unWrap(dummy);

	delete rT;

#if defined(R_PORT)
	PutRNGstate();
#endif
}

void rfAttrEval(int *modelID, double *estOut) {
	// is modelID valid
	if (*modelID < 0 || *modelID >= allModels.len() || allModels[*modelID] == 0)
		return;
	featureTree *dT = (featureTree*) allModels[*modelID]; // working Model
	dT->learnRF = mTRUE;

	marray<double> attrEst;
	attrEst.wrap(dT->noAttr + 1, estOut);

	dT->varImportance(attrEst);

	int dummy;
	attrEst.unWrap(dummy);

}

void ordEvalCore(int *noInst, int *noDiscrete, int *noDiscVal, int *discData,
		char **dscAttrNames, char **dscValNames, int *noOptions,
		char **optName, char **optValue, double *rePos, double *reNeg,
		double *anch, double *rndrePos, double *rndreNeg, double *rndAnch,
		int *noAttrVal, char **ordEvalFile, char **ordEvalRndFile, int *variant) {
#if defined(R_PORT)
	GetRNGstate();
#endif
	int dummy, i, attrIdx, iA, iV, iS, iVout;
	// wrap arrays for security reasons
	marray<int> noDiscreteValues, discreteData, noAttrValues;
	noDiscreteValues.wrap(*noDiscrete, noDiscVal);
	discreteData.wrap(*noDiscrete * (*noInst), discData);
	int maxAttrValues = noDiscreteValues[1];
	for (i = 2; i < *noDiscrete; i++)
		if (maxAttrValues < noDiscreteValues[i])
			maxAttrValues = noDiscreteValues[i];
	marray<char*> optionsName, optionsValue;
	optionsName.wrap(*noOptions, optName);
	optionsValue.wrap(*noOptions, optValue);
	marray<double> reinfPosOut, reinfNegOut, anchorOut, rndReinfPosOut,
			rndReinfNegOut, rndAnchorOut;
	reinfPosOut.wrap((*noDiscrete - 1) * (maxAttrValues + 1), rePos);
	reinfNegOut.wrap((*noDiscrete - 1) * (maxAttrValues + 1), reNeg);
	anchorOut.wrap((*noDiscrete - 1) * (maxAttrValues + 1), anch);
	rndReinfPosOut.wrap((*noDiscrete - 1) * (maxAttrValues + 1) * noOEstats,
			rndrePos);
	rndReinfNegOut.wrap((*noDiscrete - 1) * (maxAttrValues + 1) * noOEstats,
			rndreNeg);
	rndAnchorOut.wrap((*noDiscrete - 1) * (maxAttrValues + 1) * noOEstats,
			rndAnch);
	noAttrValues.wrap((*noDiscrete - 1) * (maxAttrValues + 1), noAttrVal);
	marray<char *> discAttrNames, discValNames;
	if (dscAttrNames && dscAttrNames[0]) {
		discAttrNames.wrap(*noDiscrete, dscAttrNames);
		discValNames.wrap(*noDiscrete, dscValNames);
	} else {
		discAttrNames.create(*noDiscrete, 0);
		discValNames.create(*noDiscrete, 0);
	}

	featureTree *dT = new featureTree;

	// read options
	dT->opt->optionsFromStrings(*noOptions, optionsName, optionsValue);

	// prepare data, first description than matrixes
	marray<double> numericData;
	marray<char *> numAttrNames(0, 0);
	dT->dscFromR(*noDiscrete, noDiscreteValues, 0, mFALSE, discAttrNames,
			discValNames, numAttrNames);
	dT->dataFromR(*noInst, discreteData, numericData, mTRUE);
	// prepare data for training/testing
	dT->opt->splitSelection = ALL_TRAINING;
	dT->prepareDataSplits();
	dT->setDataSplit(dT->opt->splitIdx);

	// prepare data structures to hold results
	marray<marray<double> > reinfPos(dT->noDiscrete), reinfNeg(dT->noDiscrete),
			anchor(dT->noDiscrete);

	for (attrIdx = 1; attrIdx < dT->noDiscrete; attrIdx++) {
		// for each attribute we need space for its values
		reinfPos[attrIdx].create(dT->AttrDesc[dT->DiscIdx[attrIdx]].NoValues
				+ 1, 0.0);
		reinfNeg[attrIdx].create(dT->AttrDesc[dT->DiscIdx[attrIdx]].NoValues
				+ 1, 0.0);
		anchor[attrIdx].create(dT->AttrDesc[dT->DiscIdx[attrIdx]].NoValues + 1,
				0.0);
	}
	mmatrix<marray<double> > reinfPosRnd(dT->noDiscrete, maxAttrValues + 1),
			reinfNegRnd(dT->noDiscrete, maxAttrValues + 1), anchorRnd(
					dT->noDiscrete, maxAttrValues + 1);
	for (attrIdx = 1; attrIdx < dT->noDiscrete; attrIdx++) {
		for (iV = 0; iV <= dT->AttrDesc[dT->DiscIdx[attrIdx]].NoValues; ++iV) {
			reinfPosRnd(attrIdx, iV).create(noOEstats, 0.0);
			reinfNegRnd(attrIdx, iV).create(noOEstats, 0.0);
			anchorRnd(attrIdx, iV).create(noOEstats, 0.0);
		}
	}

	// call attribute evaluation
	marray<double> weight(dT->NoTrainCases, 1.0);
	estimation Estimator(dT, dT->DTraining, weight, dT->NoTrainCases);
	switch (*variant) {
	case 1:
		Estimator.ordAVdAeqNorm(1, dT->noDiscrete, kEqual, reinfPos, reinfNeg,
				anchor, reinfPosRnd, reinfNegRnd, anchorRnd);
		break;
	case 2:
		Estimator.ordAVdAeqNormAttrDiff1(1, dT->noDiscrete, expRank, reinfPos,
				reinfNeg, anchor, reinfPosRnd, reinfNegRnd, anchorRnd);
		break;
	case 3:
		Estimator.ordAVdAeqNormClDiff1(1, dT->noDiscrete, expRank, reinfPos,
				reinfNeg, anchor, reinfPosRnd, reinfNegRnd, anchorRnd);
		break;
	default:
		merror("ordEvalCore, invalid variant parameter ", "");
		break;
	}
	// initialize output structures
	reinfPosOut.init(NAcont);
	reinfNegOut.init(NAcont);
	anchorOut.init(NAcont);
	rndReinfPosOut.init(NAcont);
	rndReinfNegOut.init(NAcont);
	rndAnchorOut.init(NAcont);

	// count number of active (non missing) attribute values
	marray<marray<int> > noAV;
	dT->countAV(noAV);

	int noAttr = dT->noDiscrete - 1;
	// copy to output structures
	for (iA = 1; iA < dT->noDiscrete; iA++) {
		for (iV = 0; iV <= dT->AttrDesc[dT->DiscIdx[iA]].NoValues; ++iV) {
			if (iV == 0) {
				iVout = maxAttrValues;
				noAttrValues[iA - 1 + iVout * noAttr] = dT->NoTrainCases
						- noAV[iA][iV]; // all but missing
			} else {
				iVout = iV - 1;
				noAttrValues[iA - 1 + iVout * noAttr] = noAV[iA][iV];
			}
			reinfPosOut[iA - 1 + iVout * noAttr] = reinfPos[iA][iV];
			reinfNegOut[iA - 1 + iVout * noAttr] = reinfNeg[iA][iV];
			anchorOut[iA - 1 + iVout * noAttr] = anchor[iA][iV];
			for (iS = 0; iS < noOEstats; iS++) {
				rndReinfPosOut[iA - 1 + iVout * noAttr + iS * noAttr
						* (maxAttrValues + 1)] = reinfPosRnd(iA, iV)[iS];
				rndReinfNegOut[iA - 1 + iVout * noAttr + iS * noAttr
						* (maxAttrValues + 1)] = reinfNegRnd(iA, iV)[iS];
				rndAnchorOut[iA - 1 + iVout * noAttr + iS * noAttr
						* (maxAttrValues + 1)] = anchorRnd(iA, iV)[iS];
			}
		}
	}
	// write to files
	FILE *fout;
	if (ordEvalFile && ordEvalFile[0] && strlen(ordEvalFile[0]) > 0) {
		if ((fout = fopen(ordEvalFile[0], "w")) == NULL) {
			merror("ordEvalCore, cannot open ordEvalFile: ", ordEvalFile[0]);
		} else {
			printAVest(fout, reinfPos, reinfNeg, anchor, dT);
			fclose(fout);
		}
	}
	if (ordEvalRndFile && ordEvalRndFile[0] && strlen(ordEvalRndFile[0]) > 0) {
		if ((fout = fopen(ordEvalRndFile[0], "w")) == NULL) {
			merror("ordEvalCore, cannot open ordEvalRndFile: ",
					ordEvalRndFile[0]);
		} else {
			printAVestRnd(fout, reinfPosRnd, reinfNegRnd, anchorRnd, dT);
			fclose(fout);
		}
	}
	// unwrap arrays
	noDiscreteValues.unWrap(dummy);
	discreteData.unWrap(dummy);
	optionsName.unWrap(dummy);
	optionsValue.unWrap(dummy);
	reinfPosOut.unWrap(dummy);
	reinfNegOut.unWrap(dummy);
	anchorOut.unWrap(dummy);
	rndReinfPosOut.unWrap(dummy);
	rndReinfNegOut.unWrap(dummy);
	rndAnchorOut.unWrap(dummy);
	noAttrValues.unWrap(dummy);
	if (dscAttrNames && dscAttrNames[0]) {
		discAttrNames.unWrap(dummy);
		discValNames.unWrap(dummy);
	}

	delete dT;
#if defined(R_PORT)
	PutRNGstate();
#endif
}

void calibrate(int *calMethod, int *noInst, int *correctCl,
		double *predictedPr, double *wght, int*noBins, int *noIntervals,
		double *interval, double *calProb) {
	// is modelID valid
	//if (*modelID < 0|| *modelID >= allModels.len() || allModels[*modelID] == 0 ||
	//		allModels[*modelID]->isRegression || *noInst <=0)
	//	return;
	//featureTree *dT = (featureTree*)allModels[*modelID]; // working Model

	// wrap arrays
	marray<int> correctClass;
	correctClass.wrap(*noInst, correctCl);
	marray<double> predictedProb, weight;
	predictedProb.wrap(*noInst, predictedPr);
	weight.wrap(*noInst, wght);
	int i, dummy;

	Calibrate cal;
	marray<sort3Rec> y(*noInst);
	for (int i = 0; i < *noInst; i++) {
		y[i].value = correctClass[i];
		y[i].key = predictedProb[i];
		y[i].weight = weight[i];
	}
	y.setFilled(*noInst);
	switch (*calMethod) {
	case 1:
		cal.isoRegCal(y);
		break;
	case 2:
		cal.binIsoCal(y, *noBins);
		break;
	case 3:
		cal.binningCal(y, *noBins);
		break;
	case 4:
		cal.mergeCal(y);
		break;
	default:
		merror("CORElearn C++:", "Invalid calibration method");
	}
	// copy results to output
	*noIntervals = cal.interval.len();
	for (i = 0; i < cal.interval.len(); i++) {
		interval[i] = cal.interval[i];
		calProb[i] = cal.calProb[i];
	}
	//unwrap arrays
	correctClass.unWrap(dummy);
	predictedProb.unWrap(dummy);
	weight.unWrap(dummy);
}

void modelEvaluate(int *noInst, int *correctCl, int *predictedCl,
		double *predictedPr, double *costMx, int *noClasses,
		double *priorClProbability, double *accuracy, double *avgCost,
		double *infScore, double *auc, int *predictionMx, double *sensitivity,
		double *specificity, double *brier, double *kappa, double *precision, double *Gmean) {
	// wrap arrays
	marray<int> correctClass, predictedClass, predictionMatrix;
	correctClass.wrap(*noInst, correctCl);
	predictedClass.wrap(*noInst, predictedCl);
	predictionMatrix.wrap(*noClasses * *noClasses, predictionMx);
	marray<double> predictedProb, costMatrix, priorClProb;
	predictedProb.wrap(*noInst * *noClasses, predictedPr);
	costMatrix.wrap(*noClasses * *noClasses, costMx);
	priorClProb.wrap(*noClasses, priorClProbability);
	mmatrix<double> CostMatrix;
	costMxFromR(*noClasses, costMatrix, CostMatrix);
	int i, j, dummy;

	marray<double> priorClassProb(*noClasses + 1, 0);
	for (i = 1; i <= *noClasses; i++)
		priorClassProb[i] = priorClProb[i - 1];

	marray<marray<double> > probDist(*noInst);
	for (i = 0; i < *noInst; i++) {
		probDist[i].create(*noClasses + 1, 0.0);
		for (j = 1; j <= *noClasses; j++)
			probDist[i][j] = predictedProb[i + (j - 1) * (*noInst)];
	}
	mmatrix<int> predMx(*noClasses + 1, *noClasses + 1, 0);

	modelEval(*noInst, correctClass, probDist, *noClasses, priorClassProb,
			CostMatrix, *accuracy, *avgCost, *infScore, *auc, predMx, *kappa,
			*sensitivity, *specificity, *brier, *precision, *Gmean);

	for (i = 1; i <= *noClasses; i++)
		for (j = 1; j <= *noClasses; j++)
			predictionMatrix[i - 1 + (j - 1) * *noClasses] = predMx(i, j);

	//unwrap arrays
	correctClass.unWrap(dummy);
	predictedClass.unWrap(dummy);
	predictionMatrix.unWrap(dummy);
	predictedProb.unWrap(dummy);
	costMatrix.unWrap(dummy);
	priorClProb.unWrap(dummy);
}

void modelEvaluateReg(int *noInst, double *truePred, double *pred,
		double *avgPrediction, double *MSE, double *RMSE, double *MAE,
		double *RMAE) {

	// wrap arrays
	marray<double> truePrediction, prediction;
	prediction.wrap(*noInst, pred);
	truePrediction.wrap(*noInst, truePred);

	modelEvalReg(*noInst, truePrediction, prediction, *avgPrediction, *MSE,
			*RMSE, *MAE, *RMAE);

	//unwrap arrays
	int dummy;
	prediction.unWrap(dummy);
	truePrediction.unWrap(dummy);
}

// library version
void optionsInOut(int *modelID, char **fileName, char **io) {
	// is modelID valid
	if (*modelID < 0 || *modelID >= allModels.len() || allModels[*modelID] == 0)
		return;
	dataStore *data = allModels[*modelID]; // working Model
	if (strcmp(io[0], "read") == 0) {
		data->opt->readConfig(fileName[0]);
	} else if (strcmp(io[0], "write") == 0) {
		data->opt->writeConfig(fileName[0]);
	} else {
		merror("Unrecognized directive for option processing: ", io[0]);
	}
}

void saveRF(int *modelID, char **fileName) {
	// is modelID valid
	if (*modelID < 0 || *modelID >= allModels.len() || allModels[*modelID] == 0)
		return;
	featureTree *dT = (featureTree*) allModels[*modelID]; // working Model
	dT->learnRF = mTRUE;
	dT->writeRF(fileName[0]);
}

// read RF model from the file
void readRF(char **fileName, int *modelID) {
	featureTree *dT = 0;
	dataStore *model = 0;
	*modelID = allModels.memberPlace(model);
	// if there is no more space in the table
	if (*modelID < 0) {
		Rprintf("maximum number of models reached\n");
		return;;
	}
	allModels[*modelID] = new featureTree;
	dT = (featureTree*) allModels[*modelID]; // working model
	dT->learnRF = mTRUE;

	if (!dT->readForest(fileName[0])) {
		destroyOneCoreModel(modelID);
	}
}

#if defined(R_PORT)
SEXP exportSizesRF(SEXP modelID) {
	int mi;
	mi = INTEGER(modelID)[0];
	// is modelID valid
	if (mi < 0 || mi >= allModels.len() || allModels[mi] == 0)
		return (NULL);
	featureTree *dT = (featureTree*) allModels[mi]; // working Model
	dT->learnRF = mTRUE;
	return (dT->exportSizes());
}

SEXP exportModel(SEXP modelID) {
	int mi;
	mi = INTEGER(modelID)[0];
	// is modelID valid
	if (mi < 0 || mi >= allModels.len() || allModels[mi] == 0)
		return (NULL);
	featureTree *dT = (featureTree*) allModels[mi]; // working Model
	dT->learnRF = mTRUE;
	return (dT->RF2R());
}

#endif

void simRcall() {
	int maxModel = 128;
	initCore(&maxModel);
	int noInst = 10, noNumeric = 2, noDisc = 3;
	int *discData = new int[noInst * noDisc];
	double *numData = new double[noInst * noNumeric];
	int noDiscreteValues[] = { 2, 3, 4 };
	int noClasses = noDiscreteValues[0];
	double *costMx = new double[noClasses * noClasses];
	int i, j;
	for (i = 0; i < noInst; i++) {
		discData[i] = 1 + i % 2;
		discData[noInst + i] = 1 + i % 3;
		discData[2 * noInst + i] = 1 + i % 4;
		numData[i] = 1.0 + 0.1 * i;
		numData[noInst + i] = 2.0 + 0.1 * i;
	}
	for (i = 0; i < noClasses; i++)
		for (j = 0; j < noClasses; j++)
			if (i == j)
				costMx[i + j * noClasses] = 0.0;
			else
				costMx[i + j * noClasses] = 1.0;
	int modelID;
	int noOptions = 4;
	double avgPrediction = 0, *priorClProb = new double[noClasses];
	char const* optionsName[] = { "action", "domainName", "rfNoTrees",
			"rfPredictClass" };
	char const* optionsVal[] = { "rf", "test", "100", "N" };
	buildCoreModel(&noInst, &noDisc, noDiscreteValues, discData, &noNumeric,
			numData, costMx, 0, 0, 0, &noOptions, (char**) optionsName,
			(char**) optionsVal, &modelID, &noClasses, priorClProb,
			&avgPrediction);
	int noPredict = noInst;
	int *pred = new int[noPredict];
	double *prob = new double[noPredict * noDiscreteValues[0]];
	double *regPred = new double[noPredict];
	predictWithCoreModel(&modelID, &noPredict, discData, numData, costMx, pred,
			prob, regPred, &noOptions, (char**) optionsName,
			(char**) optionsVal);

	for (i = 0; i < noPredict; i++) {
		printf("%d  %d  ", i + 1, pred[i]);
		for (j = 0; j < noDiscreteValues[0]; j++)
			printf("%5.3f ", prob[i + j * noPredict]);
		printf("\n");
	}
	destroyOneCoreModel(&modelID);

	int correctCl[] = { 0, 0, 0, 0, 0, 1, 1, 1, 1, 1 };
	double predictedPr[] = { 0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 };
	double weight[] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
	int calMethod = 3, noBins = 5, noIntervals = 0;
	noInst = 10;
	double *interval = new double[noInst];
	double *calProb = new double[noInst];
	calibrate(&calMethod, &noInst, correctCl, predictedPr, weight, &noBins,
			&noIntervals, interval, calProb);

	delete[] interval;
	delete[] calProb;
	delete[] priorClProb;
}

} //extern "C"

