/*********************************************************************
*   Name:              modul trutil  (tree utillities)
*
*   Description:  utillities for  tree
*
*********************************************************************/

#include <string.h>     // dealing with names
#include <stdlib.h>     // min, max
#include <stdio.h>
#include <float.h>

#include "utils.h"
#include "error.h"
#include "regtree.h"
#include "constrctReg.h"
#include "estimatorReg.h"

//extern regressionTree* GlobalRegressionTree ; // used by ContDataRetriever



//************************************************************
//
//                      printFTree
//                      ----------
//
//   recursively prints the entire feature tree on a given stream;
//   if features are to long, make a abbrevaition and full
//                  description below
//
//************************************************************
void regressionTree::printFTree(FILE *out,  int &FeatureNo,
    marray<binnodeReg*> &FeatureNode, marray<binnodeReg*> &ModelNode,
    int &LeavesNo, binnodeReg *branch, int place)
{
   if (branch)
   {
      if (branch->left) // not the leaf yet
      {
         int fNo = FeatureNo++ ;   // reserve current index


         printFTree(out, FeatureNo, FeatureNode, ModelNode, LeavesNo,
                    branch->left, place+5);

         fprintf(out,"%*sf%d\n",place," ",fNo) ;

         FeatureNode[fNo] = branch ;

         printFTree(out, FeatureNo, FeatureNode, ModelNode, LeavesNo,
                    branch->right, place+5);
      }
      else
      {
         fprintf(out,"%*sl%d\n",place," ",LeavesNo) ;
         ModelNode[LeavesNo] = branch ;
         LeavesNo++ ;
      }
   }
}


//************************************************************
//
//                   printFTreeFile
//                   --------------
//
//             prints the feature tree on a file
//
//************************************************************
void regressionTree::printFTreeFile(char *FileName, int idx,
        int LeavesAfter, int freedomAfter,
        double TestSEafter, double TestRSEafter,
        double TestAEafter, double TestRAEafter)
{
   FILE *to, *toDot=0 ;
   if ((to=fopen(FileName,"w"))==NULL)
   {
       merror("Cannot open tree output file",FileName) ;
       return ;
   }
   opt->outConfig(to) ;
   printLine(to,"-",46) ;
   printResultsHead(to) ;
   printResultLine(to, idx,
                            LeavesAfter, freedomAfter,
                             TestSEafter, TestRSEafter,
                            TestAEafter, TestRAEafter) ;
   printLine(to,"-",46) ;

   int FeatureNo  = 0;
   int noLeaf = noLeaves() ;
   marray<binnodeReg*> FeatureNode(noLeaf) ;
   marray<binnodeReg*> ModelNode(noLeaf) ;
   int LeavesNo  = 0;

   char buf[MaxFeatureStrLen] ;

   printFTree(to, FeatureNo, FeatureNode, ModelNode, LeavesNo, root, 0);

   printLine(to,"-",46) ;

   if (opt->printTreeInDot)
   {
      int fNo = 0, lNo = 0 ;
	  strcpy(buf, FileName) ;
	  strcat(buf, ".dot") ;
      if ((toDot=fopen(buf,"w"))==NULL)
	  {
         merror("Cannot open dot tree output file",buf) ;
	  }
	  else {
          fprintf(toDot, "digraph \"%s\" {\n\tsize = \"7,10\"  /* set appropriately to see the whole graph */\n", FileName) ;

		  printFTreeDot(toDot, root, fNo, lNo) ;
      }
   }

   int i ;
   for (i=0; i<FeatureNo ; i++)
   {
      Feature2Str(FeatureNode[i], buf);
      fprintf(to, "f%d: %s\n", i, buf) ;
	  if (opt->printTreeInDot)
		  fprintf(toDot, "\tf%d [label = \"%s\"]\n", i, buf) ;
   }

   fprintf(to,"\n\nLeaf    weight sqrt(MSE)    MAE    av.pred.   std.dev Model_description \n") ;
   fprintf(to,  "\n--------------------------------------------------------------------\n") ;

   char *ModelDescription ;
   for (i=0 ; i < LeavesNo ; i++)
   {
      ModelDescription = ModelNode[i]->Model.descriptionString() ;
      fprintf(to,"l%-3d: %9.2f %9.2f %9.2f %9.2f %9.2f %s\n",
                  i, ModelNode[i]->weight, sqrt(ModelNode[i]->MSE), ModelNode[i]->MAE,
                  ModelNode[i]->averageClassValue, ModelNode[i]->stdDevClass,
                  ModelDescription ) ;
 	  if (opt->printTreeInDot)
		  fprintf(toDot, "\tl%d [shape = box, label = \"%s\"]\n", i, ModelDescription) ;

      delete [] ModelDescription ;
   }

   printLine(to,"-",46) ;

   fclose(to) ;

   if (opt->printTreeInDot)
   {
      fprintf(toDot, "}\n") ;
      fclose(toDot) ;
   }

}

//************************************************************
//
//                      printFTreeDot
//                      -------------
//
//   recursively prints the entire feature tree on a given stream
//   in a dot format
//
//************************************************************
void regressionTree::printFTreeDot(FILE *outDot,  binnodeReg *branch, int &FeatureNo, int &LeavesNo)
{
   if (branch)
   {
      if (branch->left) // not the leaf yet
      {
         int fNo = FeatureNo++ ;   // reserve current index

         if (branch->left->left) // is left one the leaf
		   fprintf(outDot, "\tf%d -> f%d [label = \"yes\"]\n", fNo, FeatureNo) ;
		 else
		   fprintf(outDot, "\tf%d -> l%d [label = \"yes\"]\n", fNo, LeavesNo) ;

         printFTreeDot(outDot, branch->left, FeatureNo, LeavesNo);

         if (branch->right->left) // is right one the leaf
		   fprintf(outDot, "\tf%d -> f%d [label = \"no\"]\n", fNo, FeatureNo) ;
		 else
		   fprintf(outDot, "\tf%d -> l%d [label = \"no\"]\n", fNo, LeavesNo) ;

         printFTreeDot(outDot, branch->right, FeatureNo, LeavesNo);
	  }
      else  {
         // fprintf(outDot, "\tl%d [shape = box]\n", LeavesNo) ;
         LeavesNo++ ;
      }
   }
}



//************************************************************
//
//                      outDomainSummary
//                      ---------------
//
//     prints various parameters of the data
//
//************************************************************

void regressionTree::outDomainSummary(FILE *to) const
{
    fprintf(to,"\n\n DATA INFO") ;
    fprintf(to,"\n-----------------------------------------------------------") ;
    fprintf(to,"\nDomain name: %s", opt->domainName) ;
    fprintf(to,"\nNumber of all examples: %d", NoCases) ;
    fprintf(to,"\nNumber of discrete attributes: %d", noDiscrete) ;
    fprintf(to,"\nNumber of continuous attributes: %d", noNumeric-1) ;
    fprintf(to,"\nNumber of all attributes: %d", noAttr) ;
    fprintf(to,"\n-----------------------------------------------------------\n") ;
}



//************************************************************
//
//                      test
//                      ----
//
//        performs testing on testing examples
//
//************************************************************
void regressionTree::test(marray<int> &DSet, int SetSize,
                          double &SE, double &RSE,double &AE, double &RAE,
                          FILE *residFile)
{

   double residium ;

   if (SetSize == 0)   {
      merror("regressionTree::test","There is no data set available.");
	  return ;
   }
   // set where the prediction data is
   dData = &DiscData ;
   nData = &NumData ;

   marray<double> prediction(SetSize), truePrediction(SetSize) ;
   for (int i=0; i < SetSize ; i++)      {
         prediction[i] = check(root,DSet[i]) ;  // safe prediction
         truePrediction[i] = NumData(DSet[i],0) ;

         residium = prediction[i] - truePrediction[i] ;
         if (residFile != NULL)
            fprintf(residFile,"%6d, %f\n",DSet[i], residium) ;
   }

   modelEvalReg(SetSize, truePrediction, prediction, root->averageClassValue, SE, RSE, AE, RAE) ;
}



//************************************************************
//
//                      check
//                      -----
//
//        computes classification for single case
//
//************************************************************
double regressionTree::check(binnodeReg *branch, int caseIdx)
{
   double contValue = NAcont;
   char discValue = NAdisc;
   switch (branch->Identification)
   {
           case leaf:
                return branch->Model.predictSafe(branch, caseIdx) ;
           case continuousAttribute:
                contValue = branch->Construct.continuousValue(*dData,*nData,caseIdx) ;
                break ;
           case discreteAttribute:
                discValue = branch->Construct.discreteValue(*dData,*nData,caseIdx) ;
                break ;
           default:
                merror("regressionTree::check", "invalid branch identification") ;
   }
   if ((branch->Identification == continuousAttribute && isNAcont(contValue)) ||
       (branch->Identification == discreteAttribute  && discValue == NAdisc) )
   {   // missing value
       return ( branch->weightLeft * check(branch->left, caseIdx) +
                (branch->weight - branch->weightLeft) * check(branch->right, caseIdx) +
                 opt->mEstPrediction * branch->Model.predictSafe(branch, caseIdx) )
                      / (branch->weight + opt->mEstPrediction) ;
   }
   else
     if ( (branch->Identification == continuousAttribute && (contValue <= branch->Construct.splitValue)) // || fabs(contValue - branch->Construct.splitValue)<epsilon) )
           ||(branch->Identification == discreteAttribute &&  branch->Construct.leftValues[discValue]) )
         // going left
         return  (opt->mEstPrediction * branch->Model.predictSafe(branch, caseIdx) +
                  branch->weightLeft * check(branch->left, caseIdx))
                 / (branch->weightLeft + opt->mEstPrediction) ;
      else // going right
         return  ( opt->mEstPrediction * branch->Model.predictSafe(branch, caseIdx) +
                  (branch->weight - branch->weightLeft) * check(branch->right, caseIdx))
                   / (branch->weight - branch->weightLeft + opt->mEstPrediction);
}


//************************************************************
//
//                      printResultsHead
//                      ----------------
//
//              prints head of results table
//
//************************************************************
void regressionTree::printResultsHead(FILE *to) const
{
   fprintf(to,"\n%3s %5s %6s %8s %5s %8s %5s\n",  "idx","#leaf", "dgFree", "SqrErr", "RSE", "AbsErr", "RAE") ;
   printLine(to,"-",46) ;
}


//************************************************************
//
//                      printResultLine
//                      ---------------
//
//        prints results for one tree into a single line
//
//************************************************************
void regressionTree::printResultLine(FILE *to, int idx,
        int LeavesAfter, int freedomAfter,
        double TestSEafter, double TestRSEafter,
        double TestAEafter, double TestRAEafter) const
{
   fprintf(to,"%3d %5d %6d %8.3f %5.3f %8.3f %5.3f\n",
                             idx,
                                 LeavesAfter, freedomAfter,
                                 TestSEafter, TestRSEafter,
                                 TestAEafter, TestRAEafter) ;
}

//************************************************************
//
//                    printReportFile
//                    ---------------
//
//           prints the report about domain testing
//              with current parameters on a file
//
//************************************************************
void regressionTree::printResultSummary(FILE *to,
       marray<int> &LeavesAfter, marray<int> &freedomAfter,
       marray<double> &TestSEafter, marray<double> &TestRSEafter,
       marray<double> &TestAEafter, marray<double> &TestRAEafter) const
{
   double avgLA, stdLA, avgFA, stdFA, avgSAtest, stdSAtest, avgRSAtest, stdRSAtest ;
   double avgAAtest, stdAAtest, avgRAAtest, stdRAAtest ;

   AvgStd(LeavesAfter, opt->numberOfSplits, avgLA, stdLA) ;
   AvgStd(freedomAfter, opt->numberOfSplits, avgFA, stdFA) ;
   AvgStd(TestSEafter, opt->numberOfSplits, avgSAtest, stdSAtest) ;
   AvgStd(TestRSEafter, opt->numberOfSplits, avgRSAtest, stdRSAtest) ;

   AvgStd(TestAEafter, opt->numberOfSplits, avgAAtest, stdAAtest) ;
   AvgStd(TestRAEafter, opt->numberOfSplits, avgRAAtest, stdRAAtest) ;

   printLine(to,"-",46) ;

   printResultLine(to, -1, int(avgLA+0.5), int(avgFA+0.5),
                     avgSAtest,  avgRSAtest,  avgAAtest, avgRAAtest) ;

   // after pruning
   fprintf(to, "\n\nNumber of leaves after pruning : %.2f(%.2f)\n", avgLA, stdLA) ;
   fprintf(to, "Degrees of freedom after pruning : %.2f(%.2f)\n", avgFA, stdFA) ;
   fprintf(to, "Root of squared error for test sample after pruning : %.2f(%.2f)\n",avgSAtest, stdSAtest) ;
   fprintf(to, "Relative squared error for test sample after pruning : %.2f(%.2f)\n", avgRSAtest, stdRSAtest) ;
   fprintf(to, "Absolute error for test sample after pruning : %.2f(%.2f)\n",avgAAtest, stdAAtest) ;
   fprintf(to, "Relative absolute error for test sample after pruning : %.2f(%.2f)\n", avgRAAtest, stdRAAtest) ;
}




// ************************************************************
//
//                      Feature2Str
//                      -----------
//
//        converts feature (a node) to a description string
//
// ************************************************************
void regressionTree::Feature2Str(binnodeReg *Node, char* const Str)
{
   Node->Construct.descriptionString(Str) ;
}



//************************************************************
//
//                        discretizeGreedy
//                        -----------------
//
//     finds best discretization of continuous attribute with
//      greedy algorithm and returns its estimated quality
//
//************************************************************
double regressionTree::discretizeGreedy(int ContAttrIdx, estimationReg &Estimator, marray<double> &Bounds)
{
   Bounds.setFilled(0) ;

   marray<sortRec> sortedAttr(Estimator.TrainSize) ;
   int i, j, idx ;
   int OKvalues = 0 ;
   for (j=0 ; j < Estimator.TrainSize ; j++)
   {
      if (isNAcont(NumData(j, ContAttrIdx)))
        continue ;
      sortedAttr[OKvalues].key = NumData(j, ContAttrIdx) ;
      sortedAttr[OKvalues].value = j ;
      OKvalues ++ ;
   }
   if (OKvalues <= 1)    // all the cases have missing value of the attribute or only one OK
   {
      // merror("regressionTree::discretizeGreedy", "all values of the attribute are missing or equal") ;
      return - FLT_MAX ;
   }
   sortedAttr.setFilled(OKvalues) ;
   sortedAttr.qsortAsc() ;

   // eliminate duplicates
   int unique = 0 ;
   for (j=1 ; j < OKvalues ; j ++)
   {
     if (sortedAttr[j].key != sortedAttr[unique].key)
     {
       unique ++ ;
       sortedAttr[unique] = sortedAttr[j] ;
     }
   }
   OKvalues = unique ;
   sortedAttr.setFilled(OKvalues) ;

   if (OKvalues <= 1)    // all the cases have missing value of the attribute or only one OK
   {
      // merror("regressionTree::discretizeGreedy", "all values of the attribute are missing or equal") ;
      return - FLT_MAX ;
   }


   int sampleSize ;
   // we use all the available values only if explicitely demanded
   if (opt->discretizationSample==0)
     sampleSize = OKvalues -1;
   else
     sampleSize = Mmin(opt->discretizationSample, OKvalues-1) ;
   marray<int> splits(sampleSize) ;
   randomizedSample(splits, sampleSize, OKvalues-1) ;

//   if (OKvalues-1 > sampleSize)
//   {
//       // do sampling
//       marray<int> sortedCopy(OKvalues) ;
//       for (i=0 ; i < OKvalues ; i++)
//         sortedCopy[i] = i ;
//
//       int upper = OKvalues - 1 ;
//       int selected ;
//       for (i=0 ; i < sampleSize ; i++)
//       {
//          selected = randBetween(0, upper) ;
//          splits[i] = sortedCopy[selected] ;
//          upper -- ;
//          sortedCopy[selected] = sortedCopy[upper] ;
//       }
//   }
//   else
//     for (i=0 ; i < sampleSize ; i++)
//        splits[i] = i ;

   attributeCount bestType ;
   double attrValue ;

   Estimator.adjustTables(0, noDiscrete + sampleSize) ;
   // greedy search

   marray<double> currentBounds(sampleSize) ;
   int currentIdx ;
   double bestEstimate = - FLT_MAX, bound ;
   int currentLimit=0 ; // number of times the current dicretization was
             // worse than the best discretization
   int currentNoValues = 2 ;
   while (currentLimit <= opt->discretizationLookahead && sampleSize > 0 )
   {
     // compute data columns
     for (i=0 ; i < Estimator.TrainSize ; i++)
     {
       attrValue = Estimator.NumValues(i, ContAttrIdx) ;
       idx = 0 ;
       while (idx < currentBounds.filled()  &&  attrValue > currentBounds[idx])
         idx++ ;
       idx ++ ; // changes idx to discrete value
       for (j=0 ; j < sampleSize ; j++)
       {
          if (isNAcont(attrValue))
            Estimator.DiscValues.Set(i, noDiscrete + j, NAdisc) ;
          else
            if (attrValue <= sortedAttr[splits[j]].key)
               Estimator.DiscValues.Set(i, noDiscrete + j, idx) ;
            else
               Estimator.DiscValues.Set(i, noDiscrete + j, idx+1) ;
       }
     }
     for (j=0 ; j < sampleSize ; j++)
        Estimator.prepareDiscAttr(noDiscrete + j, currentNoValues) ;
     // estimate and select best
     currentIdx = Estimator.estimate(opt->selectionEstimatorReg, 1, 1,
                          noDiscrete, noDiscrete+sampleSize, bestType) ;
     bound = (sortedAttr[splits[currentIdx-noDiscrete]].key
              + sortedAttr[splits[currentIdx-noDiscrete]+1].key)/2.0 ;
     currentBounds.addToAscSorted(bound) ;
     if (Estimator.DiscEstimation[currentIdx] > bestEstimate)
     {
       bestEstimate = Estimator.DiscEstimation[currentIdx] ;
       Bounds = currentBounds ;
       currentLimit = 0 ;
     }
     else
        currentLimit ++ ;
     splits[currentIdx-noDiscrete] = splits[--sampleSize] ;
     currentNoValues ++ ;
     // if (currentNoValues >= 126)
     //   break ;
   }
   return bestEstimate ;
}




