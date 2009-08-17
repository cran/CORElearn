#if !defined(GENERAL_H)
#define GENERAL_H

// includes only one: define MICROSOFT or define UNIX
#include "platform.h"

// to make R package
#define R_PORT
#if defined(MICROSOFT)
// comment for stand-alone Windows version compiled with VisualC++ compiler
#define MINGW
#endif

#if defined(R_PORT)
#include <R.h>
#include <Rinternals.h>
#define printf Rprintf
#endif

// to make additional checks
//#define DEBUG

// to have special memory allocation
//#define DEBUG_NEW
#if defined(DEBUG_NEW)
#include "new_new.h"
#endif

#define RAMP_FUNCTION
//#define WEIGHTED_ORDEVAL_DISTANCE
// define one of the following  options
// to use as distance measure with nearest neighbor in RReliefF (defaults to MANHATTAN)
#define MANHATTAN
// #define EUCLID
//#define MAHALANOBIS

#if defined(UNIX)
  const char DirSeparator = '/' ;
  #define strDirSeparator  "/"
#else
  const char DirSeparator = '\\' ;
  #define strDirSeparator  "\\"
#endif

const int MaxPath = 1024 ;
const int MaxNameLen = 1024;
const int MaxFileNameLen = 512 ;
const int MaxIntLen = 32 ;
const int MaxFeatureStrLen = 2048 ;
const int maxVal4ExhDisc = 7 ; // maximum number of values of attribute for considering exhaustive search for discretization
const int noOEstats = 9 ;

// const char MaxChar = '\xff' ;
enum booleanT { mFALSE=0, mTRUE=1 } ;
typedef char* Pchar ;
typedef int* Pint ;
typedef float* Pfloat ;
typedef double* Pdouble ;
enum attributeCount  {aDISCRETE, aCONTINUOUS} ;
enum nodeType {continuousAttribute=0, discreteAttribute=1, leaf=2}  ;

struct estDsc {
    const char *brief, *dsc ;
} ;
//  estimators
enum estimatorConst { estReliefFkEqual=1,
                      estReliefFexpRank=2,
                      estReliefFbestK=3,
                      estRelief=4,
                      estInfGain=5,
                      estGainRatio=6,
                      estMDL=7,
                      estGini = 8,
                      estMyopicReliefF=9,
                      estAccuracy=10,
                      estBinAccuracy=11,
                      estReliefFmerit=12,
                      estReliefFdistance=13,
                      estReliefFsqrDistance=14,
                      estDKM=15,
                      estReliefFexpC=16,
                      estReliefFavgC=17,
                      estReliefFpe=18,
                      estReliefFpa=19,
                      estReliefFsmp=20,
                      estGainRatioCost=21,
                      estDKMcost=22,
                      estReliefKukar=23,
                      estMDLsmp = 24,
                      estImpurityEuclid=25,
                      estImpurityHellinger=26,
                      estUniformDKM=27,
                      estUniformGini=28,
                      estUniformInf=29,
                      estUniformAccuracy=30,
                      estEqualDKM=31,
                      estEqualGini=32,
                      estEqualInf=33,
                      estEqualHellinger=34,
					  estDistHellinger=35,
                      estDistAUC=36,
                      estDistAngle=37,
                      estDistEuclid=38
} ;

// regression estimators
enum estimatorRegConst { estRReliefFkEqual=1,
						 estRReliefFexpRank=2,
						 estRReliefFbestK=3,
                         estRReliefFwithMSE=4,
                         estMSEofMean=5,
						 estMSEofModel = 6,
                         estMAEofModel=7,
						 estRReliefFdistance=8,
                         estRReliefFsqrDistance=9
} ;

// constructs
enum constructComposition {cSINGLEattribute=1, cCONJUNCTION=2, cSUM=4, cPRODUCT=8, cXofN=16} ;
enum constructNodeType {cnAND=0, cnPLUS=1, cnTIMES=2, cnCONTattribute=3, cnDISCattribute=4, cnCONTattrValue=5, cnDISCattrValue=6} ;

// Testing and setting NA values

const int NAdisc = 0 ;
extern double NAcont ;
int isNAcont(double x) ;


#if defined(MICROSOFT) && !defined(R_PORT) && !defined(MINGW)
#pragma warning (disable : 4996)
#endif

#endif

