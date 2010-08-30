#if !defined(CONSTRUCT_H)
#define CONSTRUCT_H

#include "general.h"
#include "contain.h"

class estimation ; // forward

struct constructNode
{
   constructNodeType nodeType ;
   int attrIdx, valueIdx ;
   double  lowerBoundary, upperBoundary ;
   constructNode *left, *right ;  
} ;

typedef constructNode* PconstructNode ;

// struct binnode ; // forward definition

class construct
{
   friend class featureTree ;
   constructNode *root ;
   //  binnode* TreeNode ;  
   const featureTree* gFT ;

   void destroy(constructNode *node) ;
   void dup(const constructNode *Source, PconstructNode &Target) ;
   int discreteValue(mmatrix<int> &DiscData, mmatrix<double> &NumData, int caseIdx, constructNode* Node) ;
   double continuousValue(mmatrix<int> &DiscData, mmatrix<double> &NumData, int caseIdx, constructNode* Node) ;
   char* description(constructNode *Node)  ;
   int degreesOfFreedom(constructNode *Node) ;
   booleanT containsAttribute(constructNode *Node, int attributeIdx) ;
   void flattenConjunct(constructNode *Node, marray<int> &discAttrIdxs, marray<int> &AttrVals, marray<int> &contAttrIdxs, marray<double> &lowerBndys, marray<double> &upperBndys) ;
   double mdlAux(constructNode *Node) ;
   void flattenContConstruct(constructNode *Node, marray<int> &contAttrIdxs)  ;


public:
   attributeCount countType ;
   constructComposition compositionType ;
   marray<booleanT> leftValues ;
   double splitValue ;
   int noValues ;
   
   construct() { root = 0 ; gFT = 0 ; }
   construct(const featureTree* ft) { root=0 ; init(ft); };
   void init(const featureTree* ft) {destroy(); gFT = ft; }
   ~construct() ;
   construct(construct &Copy) ;
   int operator== (construct &X) ;
   construct& operator= (construct &X) ;
   int operator< (construct &) { return 0; } ;
   int operator> (construct &) { return 0; } ;
   void destroy(void) ;
   void copy(construct &Source) ;
   void descriptionString(char* const Str) ;
   int degreesOfFreedom(void) ;
   int discreteValue(mmatrix<int> &DiscData, mmatrix<double> &NumData, int caseIdx) ;
   double continuousValue(mmatrix<int> &DiscData, mmatrix<double> &NumData, int caseIdx) ;
   void createSingle(int bestIdx, attributeCount count) ;
   void Conjoin(construct &First, construct &Second) ;
   booleanT containsAttribute(construct &AVconstruct) ;
   void flattenConjunct(marray<int> &discAttrIdxs, marray<int> &AttrVals, marray<int> &contAttrIdxs, marray<double> &lowerBndys, marray<double> &upperBndys) ;
   double mdlAux(void) ;
   void add(construct &First, construct &Second) ;
   void multiply(construct &First, construct &Second) ;
   void flattenContConstruct(marray<int> &contAttrIdxs) ;
   double mdlConstructCode()  ;
} ;


#endif

