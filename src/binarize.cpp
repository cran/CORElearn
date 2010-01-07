#include <float.h>

#include "general.h"
#include "contain.h"
#include "utils.h"
#include "estimator.h"
#include "binpart.h"
#include "options.h"

//extern Options *opt ;




// ************************************************************
//
//                       binarizeGeneral
//                       ----------------
//
//    creates binary split of attribute values according to the split's  estimate;
//             search is either exhaustive or greedy depending
//                on the number of computations for each
//
// ************************************************************
void estimation::binarizeGeneral(construct &nodeConstruct, double &bestEstimation, int firstFreeDiscSlot)
{

   int i, NoValues = nodeConstruct.noValues ;
   nodeConstruct.leftValues.create(NoValues+1,mFALSE) ;
   attributeCount bestType ;

   if (firstFreeDiscSlot == 0)
      firstFreeDiscSlot = noDiscrete ;


   if (NoValues < 2)
   {
	  bestEstimation = -FLT_MAX ;
      return ;
   }


   booleanT binaryAttributesBefore = fTree->opt->binaryAttributes ;
   fTree->opt->binaryAttributes = mFALSE ;


   if (NoValues == 2) // already binary, but we estimate it
   {
       adjustTables(0, firstFreeDiscSlot + 1) ;
       for (i=0 ; i < TrainSize ; i++)
          DiscValues.Set(i, firstFreeDiscSlot, nodeConstruct.discreteValue(DiscValues,NumValues,i)) ;

       prepareDiscAttr(firstFreeDiscSlot, 2) ;

	   i = estimate(fTree->opt->selectionEstimator, 0, 0, firstFreeDiscSlot, firstFreeDiscSlot+1, bestType) ;
	   nodeConstruct.leftValues[1] = mTRUE ;
       bestEstimation =  DiscEstimation[firstFreeDiscSlot] ;
       return ;
   }


   binPartition Generator(NoValues) ;
   int attrValue ;
   int bestIdx ;
   bestEstimation = -FLT_MAX ;

   int noBasicAttr = (noDiscrete+noNumeric-1) ;
   int greedyPositions = NoValues * (NoValues+1)/2 ;
   int exhaustivePositions ;
   if (NoValues >= maxVal4ExhDisc) // exhaustive positions would reach more than 2^32 which is way too much
     exhaustivePositions = -1 ;
   else
     exhaustivePositions = Generator.noPositions() ;
   if (fTree->opt->selectionEstimator == estReliefFkEqual || fTree->opt->selectionEstimator == estReliefFexpRank ||
       fTree->opt->selectionEstimator == estReliefFbestK || fTree->opt->selectionEstimator == estRelief ||
       fTree->opt->selectionEstimator == estReliefFmerit || fTree->opt->selectionEstimator == estReliefFdistance ||
       fTree->opt->selectionEstimator == estReliefFsqrDistance ||  fTree->opt->selectionEstimator == estReliefFexpC ||
       fTree->opt->selectionEstimator == estReliefFavgC || fTree->opt->selectionEstimator == estReliefFpe ||
       fTree->opt->selectionEstimator == estReliefFpa ||fTree->opt->selectionEstimator == estReliefFsmp ||
       fTree->opt->selectionEstimator == estReliefKukar) // ReliefF estimators
   {
      greedyPositions += (NoValues-1)*noBasicAttr; // we also have to estimate basic attributes in each round (distances)
   }
   if ( (NoValues < maxVal4ExhDisc) &&
	     (exhaustivePositions * 0.8 <= greedyPositions ||
	      exhaustivePositions < fTree->opt->discretizationSample))
   {
     // exhaustive search
     adjustTables(0, firstFreeDiscSlot + exhaustivePositions) ;
     marray<marray<booleanT> >  leftValues(exhaustivePositions) ;
     int noIncrements = 0 ;
     while (Generator.increment() )
     {
       // save partition
       leftValues[noIncrements] = Generator.leftPartition ;
       // compute data column
       for (i=0 ; i < TrainSize ; i++)
       {
          attrValue = nodeConstruct.discreteValue(DiscValues, NumValues, i) ;
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
     bestIdx = estimate(fTree->opt->selectionEstimator, 0, 0,
                               firstFreeDiscSlot, firstFreeDiscSlot+noIncrements, bestType) ;
     nodeConstruct.leftValues =  leftValues[bestIdx - firstFreeDiscSlot] ;
     bestEstimation =  DiscEstimation[bestIdx] ;
   }
   else
   {
      // greedy search
     adjustTables(0, firstFreeDiscSlot + NoValues) ;
     marray<marray<booleanT> >  leftValues(NoValues) ;
     marray<booleanT> currentBest(NoValues+1, mFALSE) ;
     int i, j, added ;
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
               attrValue = nodeConstruct.discreteValue(DiscValues, NumValues, i) ;
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
        bestIdx = estimate(fTree->opt->selectionEstimator, 0, 0,
                               firstFreeDiscSlot, firstFreeDiscSlot + added, bestType) ;
        currentBest = leftValues[bestIdx - firstFreeDiscSlot] ;
        if (DiscEstimation[bestIdx] > bestEstimation)
        {
          bestEstimation = DiscEstimation[bestIdx] ;
          nodeConstruct.leftValues =  currentBest ;
        }
     }
   }
   fTree->opt->binaryAttributes = binaryAttributesBefore ;

}



//************************************************************
//
//                        bestSplitGeneral
//                        ----------------
//
//            finds best split for numeric attribute with selected estimator
//
//************************************************************
double estimation::bestSplitGeneral(construct &nodeConstruct, double &bestEstimation, int firstFreeDiscSlot)
{
   if (firstFreeDiscSlot == 0)
	   firstFreeDiscSlot = noDiscrete ;

   marray<sortRec> sortedAttr(TrainSize) ;
   int i, j ;
   int OKvalues = 0 ;
   double attrValue ;
   for (j=0 ; j < TrainSize ; j++)
   {
      attrValue = nodeConstruct.continuousValue(DiscValues, NumValues, j) ;
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
   sortedAttr.sort(ascSortComp) ;

   // select only unique values
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
   if (fTree->opt->discretizationSample==0)
     sampleSize = OKvalues -1;
   else
     sampleSize = Mmin(fTree->opt->discretizationSample, OKvalues-1) ;
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

   adjustTables(0, firstFreeDiscSlot + sampleSize) ;
   for (j=0 ; j < sampleSize ; j++)
   {
       // compute data column
     for (i=0 ; i < TrainSize ; i++)
     {
       attrValue = nodeConstruct.continuousValue(DiscValues, NumValues, i) ;
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

   booleanT binaryAttributesBefore = fTree->opt->binaryAttributes ;
   fTree->opt->binaryAttributes = mFALSE ;

   // estimate and select best
   int bestIdx = estimate(fTree->opt->selectionEstimator, 0, 0,
                            firstFreeDiscSlot, firstFreeDiscSlot + sampleSize, bestType) ;
   bestEstimation = DiscEstimation[bestIdx] ;

   fTree->opt->binaryAttributes = binaryAttributesBefore ;

   return (sortedAttr[splits[bestIdx-firstFreeDiscSlot]].key + sortedAttr[splits[bestIdx-firstFreeDiscSlot]+1].key)/2.0 ;
}



//************************************************************
//
//                        discretizeGreedy
//                        -----------------
//
//     finds best discretization of numeric attribute with
//      greedy algorithm and returns its estimated quality
//
//************************************************************
double estimation::discretizeGreedy(int ContAttrIdx, marray<double> &Bounds, int firstFreeDiscSlot)
{
   Bounds.setFilled(0) ;

   if (firstFreeDiscSlot == 0)
	   firstFreeDiscSlot = noDiscrete ;

   marray<sortRec> sortedAttr(TrainSize) ;
   int i, j, idx ;
   int OKvalues = 0 ;
   for (j=0 ; j < TrainSize ; j++)
   {
      if (isNAcont(NumValues(j, ContAttrIdx)))
        continue ;
      sortedAttr[OKvalues].key = NumValues(j, ContAttrIdx) ;
      sortedAttr[OKvalues].value = j ;
      OKvalues ++ ;
   }
   if (OKvalues <= 1)    // all the cases have missing value of the attribute or only one OK
   {
      // all values of the attribute are missing or equal
      return - FLT_MAX ;
   }
   sortedAttr.setFilled(OKvalues) ;
   sortedAttr.sort(ascSortComp) ;

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
      // all values of the attribute are missing or equal
      return - FLT_MAX ;
   }

   booleanT binaryAttributesBefore = fTree->opt->binaryAttributes ;
   fTree->opt->binaryAttributes = mFALSE ;

   int sampleSize ;
   // we use all the available values only if explicitely demanded
   if (fTree->opt->discretizationSample==0)
     sampleSize = OKvalues -1;
   else
     sampleSize = Mmin(fTree->opt->discretizationSample, OKvalues-1) ;

   marray<int> splits(sampleSize) ;
   randomizedSample(splits, sampleSize, OKvalues-1) ;

//
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

   adjustTables(0, firstFreeDiscSlot + sampleSize) ;

   // greedy search
   marray<double> currentBounds(sampleSize) ;
   int currentIdx ;
   double bestEstimate = - FLT_MAX, bound ;
   int currentLimit=0 ; // number of times the current dicretization was worse than the best discretization
   int currentNoValues = 2 ;
   while (currentLimit <= fTree->opt->discretizationLookahead && sampleSize > 0 )
   {
     // compute data columns
     for (i=0 ; i < TrainSize ; i++)
     {
       attrValue = NumValues(i, ContAttrIdx) ;
       idx = 0 ;
       while (idx < currentBounds.filled()  &&  attrValue > currentBounds[idx])
         idx++ ;
       idx ++ ; // changes idx to discrete value
       for (j=0 ; j < sampleSize ; j++)
       {
          if (isNAcont(attrValue))
            DiscValues.Set(i, firstFreeDiscSlot + j, NAdisc) ;
          else
            if (attrValue <= sortedAttr[splits[j]].key)
               DiscValues.Set(i, firstFreeDiscSlot + j, idx) ;
            else
               DiscValues.Set(i, firstFreeDiscSlot + j, idx+1) ;
       }
     }
     for (j=0 ; j < sampleSize ; j++)
        prepareDiscAttr(firstFreeDiscSlot + j, currentNoValues) ;
     // estimate and select best
     currentIdx = estimate(fTree->opt->selectionEstimator, 0, 0, firstFreeDiscSlot, firstFreeDiscSlot + sampleSize, bestType) ;
     bound = (sortedAttr[splits[currentIdx - firstFreeDiscSlot]].key
              + sortedAttr[splits[currentIdx - firstFreeDiscSlot]+1].key)/2.0 ;
     currentBounds.addToAscSorted(bound) ;
     if (DiscEstimation[currentIdx] > bestEstimate)
     {
       bestEstimate = DiscEstimation[currentIdx] ;
       Bounds = currentBounds ;
       currentLimit = 0 ;
     }
     else
        currentLimit ++ ;
     splits[currentIdx-firstFreeDiscSlot] = splits[--sampleSize] ;
     currentNoValues ++ ;
     // if (currentNoValues >= 126)
	 // {
	 //  merror("estimation:discretizeGreedy","internal assumption about maximum  number of nominal attribute's values is invalid") ;
     //  break ;
	 // }
   }

   fTree->opt->binaryAttributes = binaryAttributesBefore ;

   return bestEstimate ;
}



//************************************************************
//
//                        discretizeGreedyImpurity
//                        -----------------------
//
//     finds best discretization of numeric attribute with
//      greedy algorithm and returns its estimated quality
//
//************************************************************
// unifished !!!
/*double estimation::discretizeGreedyImpurity(int ContAttrIdx, marray<double> &Bounds)
{
   Bounds.setFilled(0) ;
   int maxBounds =1+ Mmin(fTree->opt->discretizationSample, TrainSize) ;
   marray<sortRec> sortedAttr(TrainSize) ;
   marray<int> noAttrVal(maxBounds, 0) ;
   mmatrix<int> noClassAttrVal(noClasses+1, maxBounds, 0) ;

   int i, j, idx ;
   int OKvalues = 0 ;
   for (j=0 ; j < TrainSize ; j++)
   {
      if (isNAcont(NumValues(j, ContAttrIdx)))
        continue ;
      sortedAttr[OKvalues].key = NumValues(j, ContAttrIdx) ;
      sortedAttr[OKvalues].value = j ;
      noClassAttrVal(NumValues(j, 0), 1) ++ ; // initially we put all in the first column
      OKvalues ++ ;
   }
   if (OKvalues <= 1)    // all the cases have missing value of the attribute or only one OK
   {
      // all values of the attribute are missing or equal
      return - FLT_MAX ;
   }
   int validInstances = OKvalues

   sortedAttr.setFilled(OKvalues) ;
   sortedAttr.sort(ascSortComp) ;

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
      // all values of the attribute are missing or equal
      return - FLT_MAX ;
   }

   booleanT binaryAttributesBefore = fTree->opt->binaryAttributes ;
   fTree->opt->binaryAttributes = mFALSE ;

   int sampleSize ;
   // we use all the available values only if explicitely demanded
   if (fTree->opt->discretizationSample==0)
     sampleSize = OKvalues -1;
   else
     sampleSize = Mmin(fTree->opt->discretizationSample, OKvalues-1) ;

   marray<int> splits(sampleSize) ;
   randomizedSample(splits, sampleSize, OKvalues-1) ;
   splits.setFilled(sampleSize);
   splits.qsortAsc();
   // prepare candidate splits
   double splitCandidate(sampleSize-1) ;
   int ci, noSplitCandidates = sampleSize - 1 ;
   for (ci=0 ; ci<noSplitCandidates;ci++){
	   splitCandidate[ci]=(sortedAttr[splits[ci]].key + sortedAttr[splits[ci+1]].key)/2.0;

   double priorImpurity = (this->*fImpurity)(validInstances, noClassAttrVal, 1) ;

   // greedy search
   marray<double> currentBounds(sampleSize) ;
   int currentIdx ;
   double bestEstimate = - FLT_MAX, bound ;
   int currentLimit=0 ; // number of times the current dicretization was worse than the best discretization
   int currentNoValues = 2 ;
   while (currentLimit <= fTree->opt->discretizationLookahead && sampleSize > 0 )
   {
      for (ci=0 ; ci < noSplitCandidates ; ci++) {

      // compute statistics for all current candidates
      for (i=0 ; i < TrainSize ; i++)
      {
       attrValue = NumValues(i, ContAttrIdx) ;
       idx = 0 ;
       while (idx < currentBounds.filled()  &&  attrValue > currentBounds[idx])
         idx++ ;
       idx ++ ; // changes idx to discrete value
       for (j=0 ; j < sampleSize ; j++)
       {
          if (isNAcont(attrValue))
            DiscValues.Set(i, firstFreeDiscSlot + j, NAdisc) ;
          else
            if (attrValue <= sortedAttr[splits[j]].key)
               DiscValues.Set(i, firstFreeDiscSlot + j, idx) ;
            else
               DiscValues.Set(i, firstFreeDiscSlot + j, idx+1) ;
       }
     }
     for (j=0 ; j < sampleSize ; j++)
        prepareDiscAttr(firstFreeDiscSlot + j, currentNoValues) ;
     // estimate and select best
     currentIdx = estimate(fTree->opt->selectionEstimator, 0, 0, firstFreeDiscSlot, firstFreeDiscSlot + sampleSize, bestType) ;
     bound = (sortedAttr[splits[currentIdx - firstFreeDiscSlot]].key
              + sortedAttr[splits[currentIdx - firstFreeDiscSlot]+1].key)/2.0 ;
     currentBounds.addToAscSorted(bound) ;
     if (DiscEstimation[currentIdx] > bestEstimate)
     {
       bestEstimate = DiscEstimation[currentIdx] ;
       Bounds = currentBounds ;
       currentLimit = 0 ;
     }
     else
        currentLimit ++ ;
     splits[currentIdx-firstFreeDiscSlot] = splits[--sampleSize] ;
     currentNoValues ++ ;
     // if (currentNoValues >= 126)
	 // {
	 //  merror("estimation:discretizeGreedy","internal assumption about maximum  number of nominal attribute's values is invalid") ;
     //  break ;
	 // }
   }

   fTree->opt->binaryAttributes = binaryAttributesBefore ;

   return bestEstimate ;
}

*/


//************************************************************
//
//                        estBinarized
//                        ------------
//
//       estimate attribute as if they were binarized
//
//************************************************************
void estimation::estBinarized(int selectedEstimator, int contAttrFrom, int contAttrTo,
                         int discAttrFrom, int discAttrTo, int firstFreeDiscSlot)
{
   if (firstFreeDiscSlot == 0)
	   firstFreeDiscSlot = noDiscrete ;

   booleanT binaryAttributesBefore = fTree->opt->binaryAttributes ;
   fTree->opt->binaryAttributes = mFALSE ;

   attributeCount bestType ;
   int addedAttr = 0, i, j, NoValues, noPartitions, iDisc, iCont, estIdx ;
   int NoDiscEstimated = discAttrTo - discAttrFrom ;
   int NoContEstimated = contAttrTo - contAttrFrom ;
   marray<int> discFrom(NoDiscEstimated), discTo(NoDiscEstimated), contFrom(NoContEstimated), contTo(NoContEstimated) ;
   int discAttrValue ;

   // estimated size
   adjustTables(0, firstFreeDiscSlot + NoDiscEstimated* 4 + NoContEstimated * fTree->opt->discretizationSample) ;


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
		   adjustTables(0,  firstFreeDiscSlot + addedAttr + Mmin(Generator.noPositions(), (long long)(fTree->opt->discretizationSample))) ;
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
			  if (noPartitions >= fTree->opt->discretizationSample)
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
	   sortedAttr.sort(ascSortComp) ;

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


	   if (fTree->opt->discretizationSample==0)
		 sampleSize = OKvalues -1;
	   else
		 sampleSize = Mmin(fTree->opt->discretizationSample, OKvalues-1) ;

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

   estimate(selectedEstimator, 0, 0, firstFreeDiscSlot, firstFreeDiscSlot + addedAttr, bestType) ;
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

   fTree->opt->binaryAttributes = binaryAttributesBefore ;
}





//************************************************************
//
//                        discretizeEqualFrequency
//                        -----------------------
//
//     discretize numeric attribute with
//      to fixed number of intervals with approximately the same number of examples in each
//
//************************************************************
void estimation::discretizeEqualFrequency(int ContAttrIdx, int noIntervals, marray<double> &Bounds)
{
   Bounds.setFilled(0) ;

   marray<sortRec> sortedAttr(TrainSize) ;
   int j ;
   int OKvalues = 0 ;
   for (j=0 ; j < TrainSize ; j++)
   {
      if (isNAcont(NumValues(j, ContAttrIdx)))
        continue ;
      sortedAttr[OKvalues].key = NumValues(j, ContAttrIdx) ;
      sortedAttr[OKvalues].value = 1 ;
      OKvalues ++ ;
   }
   if (OKvalues <= 1)    // all the cases have missing value of the attribute or only one OK
   {
      // all values of the attribute are missing
      return  ;
   }
   sortedAttr.setFilled(OKvalues) ;
   sortedAttr.sort(ascSortComp) ;

   // eliminate and count duplicates
   int unique = 0 ;
   for (j=1 ; j < OKvalues ; j++)
   {
     if (sortedAttr[j].key != sortedAttr[unique].key)
     {
       unique ++ ;
       sortedAttr[unique] = sortedAttr[j] ;
     }
	 else
		 sortedAttr[unique].value ++ ;
   }
   sortedAttr.setFilled(unique) ;

   if (unique <= 1)
   {
      // all the cases have missing value of the attribute or only one OK
      return  ;
   }
   if (unique -1 <= noIntervals)
   {
	   // all unique values should form boundaries)

	   Bounds.create(unique-1) ;
	   Bounds.setFilled(unique -1) ;
	   for (j=0 ; j < unique-1 ; j++)
		   Bounds[j] = (sortedAttr[j].key + sortedAttr[j+1].key)/2.0 ;
	   return ;
   }

   Bounds.create(noIntervals-1) ;

   int noDesired = int(ceil(double(OKvalues) / noIntervals)) ;
   double boundry ;

   int grouped = 0 ;
   for (j = 0 ; j < unique ; j++)
   {
	   if (grouped + sortedAttr[j].value < noDesired)
	      grouped =+ sortedAttr[j].value ;
	   else {
		   // form new boundry
		   boundry = (sortedAttr[j].key + sortedAttr[j+1].key) / 2.0 ;
           Bounds.addEnd(boundry) ;
 	       grouped = 0 ;
	   }
   }
}

