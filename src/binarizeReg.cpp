#include <float.h>

#include "general.h"
#include "error.h"
#include "contain.h"
#include "utils.h"
#include "estimatorReg.h"
#include "binpart.h"



// ************************************************************
//
//                       binarizeGeneral
//                       ----------------
//
//    creates binary split of attribute values according to
//   the selected estimator; search is either exhaustive or greedy depending
//   on the number of computations for each
//
// ************************************************************
void estimationReg::binarizeGeneral(int selectedEstimator, constructReg &nodeConstruct, double &bestEstimation, int firstFreeDiscSlot)
{
   if (firstFreeDiscSlot == 0)
	   firstFreeDiscSlot = noDiscrete ;

   int NoValues = nodeConstruct.noValues ;
   nodeConstruct.leftValues.create(NoValues,mFALSE) ;
   
   if (NoValues < 2) 
   {
	  bestEstimation = -FLT_MAX ;
      return ;
   }

   booleanT binaryEvaluationBefore = eopt.binaryEvaluation ;
   eopt.binaryEvaluation = mFALSE ;
 
   attributeCount bestType ;
   int i ;

   if (NoValues == 2) // already binary, but we estimate it
   {
       adjustTables(0, firstFreeDiscSlot + 1) ;
       for (i=0 ; i < TrainSize ; i++)
          DiscValues.Set(i, firstFreeDiscSlot, nodeConstruct.discreteValue(DiscValues,NumValues,i)) ;
          
       prepareDiscAttr(firstFreeDiscSlot, 2) ; 

	   i = estimate(eopt.selectionEstimatorReg, 1, 1, firstFreeDiscSlot, firstFreeDiscSlot+1, bestType) ;
       nodeConstruct.leftValues[1] = mTRUE ;
       bestEstimation =  DiscEstimation[firstFreeDiscSlot] ;
   }
  

   binPartition Generator(NoValues) ;
   char attrValue ;
   int bestIdx ;
   bestEstimation = -FLT_MAX ;

   int noBasicAttr = (noDiscrete+noNumeric-1) ;
   int greedyPositions = NoValues * (NoValues+1)/2 ;
   double exhaustivePositions ;
   if (NoValues < maxVal4ExhDisc) // exhaustive positions would reach more than 2^32 which is way too much
     exhaustivePositions = Generator.noPositions() ;
    else  exhaustivePositions = -1 ;
   if (selectedEstimator == estRReliefFbestK || selectedEstimator == estRReliefFexpRank ||
       selectedEstimator == estRReliefFkEqual || selectedEstimator == estRReliefFwithMSE || 
	   //selectedEstimator == estRReliefFconstr || 
	   selectedEstimator == estRReliefFdistance || selectedEstimator == estRReliefFsqrDistance) // ReliefF estimators
      greedyPositions += (NoValues-1)*noBasicAttr; // we also have to estimate basic attributes in each round (distances)
   if ( (NoValues < maxVal4ExhDisc) && (exhaustivePositions * 0.8 <= greedyPositions || exhaustivePositions < eopt.discretizationSample))
   {
     // exhaustive search
     adjustTables(0,  int(firstFreeDiscSlot + exhaustivePositions)) ;
     marray<marray<booleanT> >  leftValues( (int)exhaustivePositions) ;
     int i, noIncrements = 0 ;
     while (Generator.increment() )
     {
       // save partition
       leftValues[noIncrements] = Generator.leftPartition ;
       // compute data column
       for (i=0 ; i < TrainSize ; i++)
       {
          attrValue = nodeConstruct.discreteValue(DiscValues,NumValues,i) ;
          if (attrValue == NAdisc)
            DiscValues.Set(i, firstFreeDiscSlot + noIncrements, NAdisc) ;
          else
            if (leftValues[noIncrements][attrValue])
              DiscValues.Set(i, firstFreeDiscSlot + noIncrements, 1) ;
            else
              DiscValues.Set(i, firstFreeDiscSlot + noIncrements, 2) ;  
       }
       prepareDiscAttr(firstFreeDiscSlot + noIncrements, 2) ; 
       noIncrements++ ;
     }
  
     // estimate and select best
     bestIdx = estimate(selectedEstimator, 1, 1,
                               firstFreeDiscSlot, firstFreeDiscSlot+noIncrements, bestType) ;
     nodeConstruct.leftValues = leftValues[bestIdx - firstFreeDiscSlot] ; 
     bestEstimation = DiscEstimation[bestIdx] ;
   }
   else
   {
      // greedy search
     adjustTables(0, firstFreeDiscSlot + NoValues) ;
     marray<marray<booleanT> >  leftValues(NoValues) ;
     marray<booleanT> currentBest(NoValues+1, mFALSE) ;
     int j, added ;
     for (int filled=1 ; filled < NoValues ; filled++)
     {
        added = 0 ;
        for (j=1 ; j <= NoValues ; j++)
          if (currentBest[j] == mFALSE)
          {
            currentBest[j] = mTRUE ;
            leftValues[added] = currentBest ;
    
            // compute data column
            for (i=0 ; i < TrainSize ; i++)
            {
               attrValue = nodeConstruct.discreteValue(DiscValues,NumValues,i) ;
               if (attrValue == NAdisc)
                  DiscValues.Set(i, firstFreeDiscSlot + added, NAdisc) ;
               else
                 if (leftValues[added][attrValue])
                   DiscValues.Set(i, firstFreeDiscSlot + added, 1) ;
                 else
                   DiscValues.Set(i, firstFreeDiscSlot + added, 2) ;  
            }
            prepareDiscAttr(firstFreeDiscSlot + added, 2) ;
            
            currentBest[j] = mFALSE ;
            added ++ ;
          }
        bestIdx = estimate(selectedEstimator, 1, 1,
                               firstFreeDiscSlot, firstFreeDiscSlot + added, bestType) ;
        currentBest = leftValues[bestIdx - firstFreeDiscSlot] ; 
        if (DiscEstimation[bestIdx] > bestEstimation)
        {
          bestEstimation = DiscEstimation[bestIdx] ;
          nodeConstruct.leftValues =  currentBest ;
        }
     }
   }
   eopt.binaryEvaluation = binaryEvaluationBefore ;

}


//************************************************************
//
//                       binarizeBreiman
//                       ---------------
//
//    creates binary split of attribute values according to
//   optimal procedure described in Breiman et all, 1984,
//         (for continuous class)
//
//************************************************************
void estimationReg::binarizeBreiman(constructReg &nodeConstruct, double &bestEstimation)
{
   nodeConstruct.leftValues.init(mFALSE) ;
   int NoValues = nodeConstruct.noValues ;
   marray<double> valueClass(NoValues+1, 0.0) ;
   marray<double> valueWeight(NoValues+1, 0.0) ;
   marray<double> squaredValues(NoValues+1, 0.0) ;
   marray<sortRec> sortedMean(NoValues) ;
   int idx, j ;
   double value ;
   // estimationReg of discrete attributtes

   for (j=0 ; j < TrainSize ; j++)
   {
      idx = nodeConstruct.discreteValue(DiscValues,NumValues,j) ;
      value = NumValues(j, 0) ;
      valueClass[idx] += weight[j]* value ;
      valueWeight[idx] += weight[j] ;
      squaredValues[idx] += weight[j] * sqr(value) ;
   }
   double RightWeight = 0.0, RightSquares = 0.0, RightValues = 0.0 ;
   int OKvalues = 0 ;
   for (j=1 ; j <= NoValues ; j++)
   {
      if (valueWeight[j] > epsilon)
      {
        sortedMean[OKvalues].key = valueClass[j] / valueWeight[j] ;
        sortedMean[OKvalues].value = j ;
        OKvalues ++ ;
  
        RightWeight += valueWeight[j] ;
        RightSquares +=  squaredValues[j] ;
        RightValues += valueClass[j] ;
      }
   }
   double totalWeight = RightWeight ;
   sortedMean.setFilled(OKvalues) ;
   sortedMean.qsortAsc() ;
   double estimate, pLeft, variance ;
   bestEstimation = FLT_MAX ;
   int bestIdx = -1 ;
   double LeftWeight = 0.0, LeftSquares = 0.0, LeftValues = 0.0 ;
   int upper = OKvalues - 1 ;
   for (j=0 ; j < upper ; j++)
   {
       idx = sortedMean[j].value ;
       LeftSquares += squaredValues[idx] ;
       LeftValues += valueClass[idx] ;
       LeftWeight += valueWeight[idx] ;
       RightSquares -= squaredValues[idx] ;
       RightValues -= valueClass[idx] ;
       RightWeight -= valueWeight[idx] ;
       pLeft = LeftWeight/totalWeight ;
       variance = LeftSquares/LeftWeight - sqr(LeftValues/LeftWeight) ;
       if (LeftWeight > epsilon && variance > 0.0)
         estimate = pLeft *sqrt(variance) ;
       else
         estimate = 0.0 ;
       variance = RightSquares/RightWeight -sqr(RightValues/RightWeight) ;
       if (RightWeight > epsilon && variance > 0.0 )
          estimate +=  (double(1.0) - pLeft)*sqrt(variance) ;
       if (estimate < bestEstimation)
       {
           bestEstimation = estimate ;
           bestIdx = j ;
       }
   }
   nodeConstruct.leftValues.init(mFALSE) ;

   for (j=0 ; j <= bestIdx ; j++)
      nodeConstruct.leftValues[sortedMean[j].value] = mTRUE ;

   #if defined(DEBUG)
     if ( bestIdx < 0)
        merror("regressionTree::binarizeBreiman","invalid split selected") ;
   #endif

}



//************************************************************
//
//                        bestSplitGeneral
//                        -----------------
//
//            finds best split for continuous attribute with selected estimator
//
//************************************************************
double estimationReg::bestSplitGeneral(int selectedEstimator, constructReg &nodeConstruct, double &bestEstimation, int firstFreeDiscSlot)
{
   if (firstFreeDiscSlot == 0)
	   firstFreeDiscSlot = noDiscrete ;

   marray<sortRec> sortedAttr(TrainSize) ;
   int i, j ;
   int OKvalues = 0 ;
   double attrValue ;
   for (j=0 ; j < TrainSize ; j++)
   {
      attrValue = nodeConstruct.continuousValue(DiscValues,NumValues,j) ;
      if (isNAcont(attrValue))
        continue ;
      sortedAttr[OKvalues].key = attrValue ;
      sortedAttr[OKvalues].value = j ;
      OKvalues ++ ;
   }
   if (OKvalues <= 1)    // all the cases have missing value of the attribute or only one OK
   {
      bestEstimation = - FLT_MAX ;
      return - FLT_MAX ; // smaller than any value, so all examples will go into one branch
   }
   sortedAttr.setFilled(OKvalues) ;
   sortedAttr.qsortAsc() ;
   
   int lastUnique = 0 ;
   for (i=1 ; i < OKvalues ; i++)
   {
      if (sortedAttr[i].key != sortedAttr[lastUnique].key)
      {
         lastUnique ++ ;
         sortedAttr[lastUnique] = sortedAttr[i] ;
      }
   }
   OKvalues = lastUnique+1 ;
    if (OKvalues <= 1)    
   {
      bestEstimation = - FLT_MAX ;
      return - FLT_MAX ; // smaller than any value, so all examples will go into one branch
   }


   int sampleSize ; 
   if (eopt.discretizationSample==0)
     sampleSize = OKvalues -1;
   else
     sampleSize = Mmin(eopt.discretizationSample, OKvalues-1) ;
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
//          selected = randBetween(0,upper) ;
//          splits[i] = sortedCopy[selected] ;
//          upper -- ;
//          sortedCopy[selected] = sortedCopy[upper] ;
//       }
//   }
//   else
//     for (i=0 ; i < sampleSize ; i++)
//        splits[i] = i ;


   attributeCount bestType ;

   adjustTables(0, firstFreeDiscSlot + sampleSize) ;
   for (j=0 ; j < sampleSize ; j++)
   { 
       // compute data column
     for (i=0 ; i < TrainSize ; i++)
     {
       attrValue = nodeConstruct.continuousValue(DiscValues,NumValues,i) ;
       if (isNAcont(attrValue))
         DiscValues.Set(i, firstFreeDiscSlot + j, NAdisc) ;
       else
         if ( attrValue <= sortedAttr[splits[j]].key )
           DiscValues.Set(i, firstFreeDiscSlot + j, 1) ;
         else
           DiscValues.Set(i, firstFreeDiscSlot + j, 2) ;  
     }
     prepareDiscAttr(firstFreeDiscSlot + j, 2) ; 
   }
 
   booleanT binaryEvaluationBefore = eopt.binaryEvaluation ;
   eopt.binaryEvaluation = mFALSE ;

   // estimate and select best
   int bestIdx = estimate(selectedEstimator, 1, 1,
                            firstFreeDiscSlot, firstFreeDiscSlot+sampleSize, bestType) ;
   bestEstimation = DiscEstimation[bestIdx] ;
     
   eopt.binaryEvaluation = binaryEvaluationBefore ;

   return (sortedAttr[splits[bestIdx-firstFreeDiscSlot]].key + sortedAttr[splits[bestIdx-firstFreeDiscSlot]+1].key)/2.0 ;
}





//************************************************************
//
//                        bestMSEsplit
//                        ------------
//
//            finds best split for continuous attribute with standard
//                         deviation
//
//************************************************************
double estimationReg::bestMSEsplit(constructReg &nodeConstruct, double &bestEstimation)
{
  // continuous values
   double dVal, attrValue ;
   marray<sortRec> sortedAttr(TrainSize) ;
   double LeftWeight = 0.0, LeftSquares = 0.0, LeftValues = 0.0 ;
   double RightWeight = 0.0, RightSquares = 0.0, RightValues = 0.0 ;
   int j, idx ;
   int OKvalues = 0 ;
   for (j=0 ; j < TrainSize ; j++)
   {
      attrValue = nodeConstruct.continuousValue(DiscValues,NumValues,j) ;
      if (isNAcont(attrValue))
        continue ;
      sortedAttr[OKvalues].key = attrValue ;
      sortedAttr[OKvalues].value = j ;
      RightWeight += weight[j] ;
      dVal = weight[j] * NumValues(j,0) ;
      RightValues += dVal ;
      dVal *=  NumValues(j,0) ;
      RightSquares += dVal  ;
      OKvalues ++ ;
   }

   double totalWeight = RightWeight ;
   sortedAttr.setFilled(OKvalues) ;
   sortedAttr.qsortAsc() ;
   bestEstimation = FLT_MAX ;
   int bestIdx = -1 ;
   double estimate, pLeft, variance ;

   // int upper = OKvalues  ;
   j=0 ;
   while (j < OKvalues)
   {
      // collect cases with the same value of the attribute - we cannot split between them
      do {
         idx = sortedAttr[j].value ;
         dVal = weight[idx] * NumValues(idx, 0) ;
         LeftValues += dVal ;
         RightValues -= dVal ;
         dVal *= NumValues(idx, 0) ;
         LeftSquares += dVal ;
         RightSquares -= dVal ;
         LeftWeight += weight[idx] ;
         RightWeight -= weight[idx] ;
         j ++ ;
      } while (j < OKvalues && sortedAttr[j].key == sortedAttr[j-1].key)  ;

      // we cannot split with the biggest value
      if (j == OKvalues)
         break ;
      pLeft = LeftWeight/totalWeight ;
      variance = LeftSquares/LeftWeight - sqr(LeftValues/LeftWeight) ;

      if (LeftWeight > epsilon && variance > 0.0)
         estimate = pLeft * sqrt(variance) ;
      else
         estimate = 0.0 ;

      variance = RightSquares/RightWeight -sqr(RightValues/RightWeight) ;
      if (RightWeight > epsilon && variance > 0.0 )
         estimate += (double(1.0) - pLeft) * sqrt(variance) ;
      if (estimate < bestEstimation)
      {
         bestEstimation = estimate ;
         bestIdx = j ;
      }
   }

   if ( bestIdx < 0 )
   {
       // degenerated case
       if (OKvalues > 0)   // all the values are the same
          return sortedAttr[0].key - double(1.0) ;  // smaller then minimum: split will put all the cases
                                           // into one subtree and node will become a leaf
       else  // all the cases have missing value of the attribute
          return - FLT_MAX ;
   }
   else
     return (sortedAttr[bestIdx].key + sortedAttr[bestIdx-1].key)/double(2.0) ;

}



//************************************************************
//
//                        estBinarized
//                        ------------
//
//       estimate attribute as if they were binarized
//
//************************************************************
void estimationReg::estBinarized(int selectedEstimator, int contAttrFrom, int contAttrTo, 
                         int discAttrFrom, int discAttrTo, int firstFreeDiscSlot)
{
   if (firstFreeDiscSlot == 0)
	   firstFreeDiscSlot = noDiscrete ;

   booleanT binaryEvaluationBefore = eopt.binaryEvaluation ;
   eopt.binaryEvaluation = mFALSE ;

   attributeCount bestType ;
   int addedAttr = 0, i, j, NoValues, noPartitions, iDisc, iCont, estIdx ;
   int NoDiscEstimated = discAttrTo - discAttrFrom ;
   int NoContEstimated = contAttrTo - contAttrFrom ;
   marray<int> discFrom(NoDiscEstimated), discTo(NoDiscEstimated), contFrom(NoContEstimated), contTo(NoContEstimated) ;
   char discAttrValue ;

   // estimated size
   adjustTables(0, firstFreeDiscSlot + NoDiscEstimated* 4 + NoContEstimated * eopt.discretizationSample) ;


   for (iDisc=discAttrFrom ; iDisc < discAttrTo; iDisc++)
   {
       NoValues = discNoValues[iDisc] ;
	   estIdx = iDisc - discAttrFrom ; 

       if (NoValues < 2) 
	   {
		  discFrom[estIdx] = discTo[estIdx] = -1 ;
	   }
       else  if (NoValues == 2) // already binary, we estimate it
	   {
		   adjustTables(0, firstFreeDiscSlot + addedAttr + 1) ;
		   for (i=0 ; i < TrainSize ; i++)
			  DiscValues.Set(i, firstFreeDiscSlot + addedAttr, DiscValues(i,iDisc)) ;
          
		   prepareDiscAttr(firstFreeDiscSlot+addedAttr, 2) ; 
           discFrom[estIdx] = firstFreeDiscSlot + addedAttr ;
           discTo[estIdx] = firstFreeDiscSlot + addedAttr + 1 ;
           addedAttr ++ ;
		   continue ;
	   }
	   else {
  
		   binPartition Generator(NoValues) ;
           noPartitions = 0 ;
		   adjustTables(0,  firstFreeDiscSlot + addedAttr + int(Mmin(Generator.noPositions(), (double)(eopt.discretizationSample)))) ;
           discFrom[estIdx] = firstFreeDiscSlot + addedAttr ;
 		   while (Generator.increment() )
		   {
			 // compute data column
			 for (i=0 ; i < TrainSize ; i++)
			 {
			   discAttrValue = DiscValues(i, iDisc) ;
			   if (discAttrValue == NAdisc)
				 DiscValues.Set(i, firstFreeDiscSlot + addedAttr, NAdisc) ;
			   else
				 if (Generator.leftPartition[discAttrValue])
					DiscValues.Set(i, firstFreeDiscSlot + addedAttr, 1) ;
				 else
					DiscValues.Set(i, firstFreeDiscSlot + addedAttr, 2) ;  
			  }
			  prepareDiscAttr(firstFreeDiscSlot + addedAttr, 2) ; 
			  addedAttr++ ;
              noPartitions++ ;
			  if (noPartitions >= eopt.discretizationSample)
				  break ;
			}
            discTo[estIdx] = firstFreeDiscSlot + addedAttr ;

	   }
   }

   marray<sortRec> sortedAttr(TrainSize) ;
   int OKvalues  ;
   double contAttrValue ;
   int sampleSize ; 
   marray<int> splits(TrainSize), sortedCopy(TrainSize) ;

   for (iCont=contAttrFrom ; iCont < contAttrTo; iCont++)
   {

	   estIdx = iCont - contAttrFrom ;
	   contFrom[estIdx] = firstFreeDiscSlot + addedAttr ;
       OKvalues = 0 ;
       
	   for (j=0 ; j < TrainSize ; j++)
	   {
		  contAttrValue = NumValues(j, iCont) ;
		  if (isNAcont(contAttrValue))
			continue ;
		  sortedAttr[OKvalues].key = contAttrValue ;
		  sortedAttr[OKvalues].value = j ;
		  OKvalues ++ ;
	   }
	   if (OKvalues <= 1)    // all the cases have missing value of the attribute or only one OK
	   {
		  contTo[estIdx] = -1 ;
		  continue ;
	   }
	   sortedAttr.setFilled(OKvalues) ;
	   sortedAttr.qsortAsc() ;
   
	   int lastUnique = 0 ;
	   for (i=1 ; i < OKvalues ; i++)
	   {
		  if (sortedAttr[i].key != sortedAttr[lastUnique].key)
		  {
			 lastUnique ++ ;
			 sortedAttr[lastUnique] = sortedAttr[i] ;
		  }
	   }
	   OKvalues = lastUnique+1 ;
	   if (OKvalues <= 1)    
	   {
		  contTo[estIdx] = -1 ;
		  continue ;
	   }


	   if (eopt.discretizationSample==0)
		 sampleSize = OKvalues -1;
	   else
		 sampleSize = Mmin(eopt.discretizationSample, OKvalues-1) ;

      randomizedSample(splits, sampleSize, OKvalues-1) ;

//	   if (OKvalues-1 > sampleSize)  
//	   {
//		   // do sampling
//		   for (i=0 ; i < OKvalues ; i++)
//			 sortedCopy[i] = i ;
//        
//		   int upper = OKvalues - 1 ;
//		   int selected ;
//		   for (i=0 ; i < sampleSize ; i++)
//		   {
//			  selected = randBetween(0, upper) ;
//			  splits[i] = sortedCopy[selected] ;
//			  upper -- ;
//			  sortedCopy[selected] = sortedCopy[upper] ;
//		   }
//	   }
//	   else
//		 for (i=0 ; i < sampleSize ; i++)
//			splits[i] = i ;


	   adjustTables(0, firstFreeDiscSlot + addedAttr+ sampleSize) ;
	   for (j=0 ; j < sampleSize ; j++)
	   { 
		   // compute data column
		 for (i=0 ; i < TrainSize ; i++)
		 {
		   contAttrValue = NumValues(i,iCont) ;
		   if (isNAcont(contAttrValue))
			 DiscValues.Set(i, firstFreeDiscSlot + addedAttr, NAdisc) ;
		   else
			 if ( contAttrValue <= sortedAttr[splits[j]].key )
			   DiscValues.Set(i, firstFreeDiscSlot + addedAttr, 1) ;
			 else
			   DiscValues.Set(i, firstFreeDiscSlot + addedAttr, 2) ;  
		 }
		 prepareDiscAttr(firstFreeDiscSlot + addedAttr, 2) ;
		 addedAttr ++ ;
	   }
   
	   contTo[estIdx] = firstFreeDiscSlot + addedAttr ;

   }
   
   estimate(selectedEstimator, 1, 1, firstFreeDiscSlot, firstFreeDiscSlot + addedAttr, bestType) ;
   int iBin ;
   for (iDisc=discAttrFrom ; iDisc < discAttrTo; iDisc++)
   {
	  estIdx = iDisc - discAttrFrom ;
      DiscEstimation[iDisc] = -FLT_MAX ;
      for (iBin=discFrom[estIdx] ; iBin < discTo[estIdx] ; iBin++)
		  if (DiscEstimation[iBin] > DiscEstimation[iDisc])
			  DiscEstimation[iDisc] = DiscEstimation[iBin] ;
   }

   for (iCont=contAttrFrom ; iCont < contAttrTo; iCont++)
   {
	  estIdx = iCont - contAttrFrom ;
      NumEstimation[iCont] = -FLT_MAX ;
      for (iBin=contFrom[estIdx] ; iBin < contTo[estIdx] ; iBin++)
		  if (DiscEstimation[iBin] > NumEstimation[iCont])
			  NumEstimation[iCont] = DiscEstimation[iBin] ;
   }

   eopt.binaryEvaluation = binaryEvaluationBefore ;
}

