verifyCoreClass <- function(test, pred, verbose)
{
	stored <- c(
	 188,230,949,934,402,155,195,828,186,970,178, 94,898,320,846,122,644,924,812,911,948,204,301, 70,646,740,292,878, 26,
	 801,909, 36,257,986,869,885,883,187,813,941, 46,241,260,930,957,990,825,657,185,850, 36,710,142,989, 56,182,404,969,
	  26, 38, 73, 63,164, 52,121,  8, 19,994,219,328,256,234,424,811,903,168,273,181,189,971, 20,961,622,937,633,434,320,
	 911,352, 61,533, 42, 59,967,284, 39,966, 75,990,548,111, 38,146,990,788,937,603, 45,991,151, 47,774,175,196,256,208,
	 150,  4, 52, 55,880,119,921,838,940, 56,837,288,996, 37,184, 60,937, 99, 75, 16,130,103,687,753,934,650, 37, 87, 22,
	 338,947,923,238, 62,974,983,969,926, 19,625,486,673,539,201,104,119,506,162,835,689,240, 75,779,514, 86,190, 37,359,
	 194,963,980,905,963,796, 48,976,104, 19,  7,270,647,239, 69,943,935,987,994,210,127,970,266,116,399,896,738,882,443,
	 928,128,  6,637,889,788,958,175,  6,290, 99,485,977, 16, 51,994,805,178,926,202, 90,984,182,115,238, 50,858,775,713,
	 229, 25,211,754,916,652,185, 99,525,937, 48,488,458,782,156,260,991,187,676,143,872,916, 64, 69,113, 16,133,923,272,
	 941, 57, 19,929,391,809,911,331,836,364,184,970, 82,956,647,966,995,916,942,152,904, 48,994,634,222, 87,715,162,963,
	 957,940,121,596,672,146,521, 93,965,134,295, 48,779, 14,854, 19,853,657,760,190,818,869,147, 31,922,298,188,976,928,
	 135,374,976,518,314,477,327, 66,958,477,825, 22,245,459, 21,914, 32,897, 45,660,396,756, 36,790, 32,394,988, 97,765,
	  16,313,582,628,410, 76,  7, 83,701, 61,750, 58, 38, 54, 62,327,917,966,610,366, 17,987, 15,923,987,135,164, 17,850,
	 155,856,516, 12,269, 71,776,923,892, 73,185,906,971,844, 95,795,855,266,795,926,670,902, 92,919,332,962,840,913,582,
	 816,700,393,902,429,637,664, 79,285, 11,986,779,888,870,125,597,995,607,116,164,860,333,  4,875,827,921, 44,837,907,
	 263,798,854, 15,846,823,906,487,472,691,719,946,632, 25,838, 32, 41,872,183,146, 42, 97,139,824,333,703,915,892,938,
	 540,252,122,138,341,326,927,671,853,366,133,569,714,911,974,481,463,674,655, 53,917,283,952,811,308, 52, 74,116,362,
	  80,977,119,618,294, 73, 92,569, 17, 61, 53, 11,932,926,902,260,328,831,833,687,561,643,989,103,884,650,158,913,100,
	 188,270,802,947,929,163,179,939,957,360,502,812,634,111,726,902,930,923,212, 72,813,820,274,994,974,324,980,101,652,
	 989,606,672,986,975, 17,305, 91,253,872,974,886,343,158,838,115, 30,222,929,913,352,182, 17,875,133,314,687,456,982,
	 937, 49,974,333,817,191,223, 33,317,108,896,755,780,996, 66,212,884,976,880,270, 79,106,846,357,154,383,870, 30,943,
	 977,127,963, 18, 29,589,861,107,835,972, 91,166,988, 14,462,957,956,727,874,214,868,400,156,199,903,  5,985,578,607,
	 258,450,878,895,230,762,307,177, 62, 26,989,919,444, 25,970,689,128, 38,544,974,967,979,289,902,302,894,  6,768,969,
	 177,790, 46,106,935,947,899,973, 95,974, 48,331, 30,104, 26, 88,670,178,365,963,134,883,932,365,777,526, 36,171,952,
	 199,658,305,892,442,219,917, 24,846,936,620,454,808,705,  5,891,967,939,195,991, 97, 26,181,904,126,979, 52,455,494,
	  52,933,814,140, 67,230,144,954,986,906, 60,680, 93,889,211,914,154,595,678,525,108, 68, 65,303,273,961, 70,681,523,
	 890,275,967,411, 24,856,100,747,732,587,934,179, 74,201,858,412,461,936, 44,971,110,638,153,993, 58, 68, 24,550,191,
	 960,  4,572, 50,844,129,838, 66,957,749,905,982,130, 35, 54,550,849,961,101,327, 39,947,726, 73, 31,434,244,108,764,
	 957,899,599, 41,988,939, 72,588,252,950,460,607,990, 46,878,689, 78,757,845,218,423,928,126,772, 27,347,474,405,688,
	 513,107, 76, 88,129,928,960,  7,218, 40,198,755,432, 27,971,135,846, 63,649,  8,931,152,826,173,210,177,108, 10,538,
	 989, 12,952,181,768, 13,101, 34,947,104,985,726, 62, 75, 91,181,  6,939, 49,146, 56,773,499,886,439,920,209,971,241,
	 155, 98,623,655,573,771,993,945,194,794, 41,737, 38,940,903,953,216,156, 72, 98,882, 73,603, 98,953,974, 36,634,895,
	 130,275,896,133, 14,196,954,976,919,111,113,799,375, 75,921,185,453,986,993,915, 68,916,769,443,928,827,501,639,633,
	 939,919,122, 45,681,879,545,885,603, 44, 36,131,514,930, 51,762, 78, 13,873,604, 39,126,993,241,125,963,210,709, 42,
	 184,280,506, 87,229,949,957,370, 97,420,883,262,141,944)/1000

	#stored[1:20] <- 0.5 # simulate an error

    sdappr <- 0.095*(stored*(1 - stored))^0.61
	out <- pred$probabilities[, 2]
	err <- (out - stored)/sdappr
	curr <- mean(err^2)
	ok <- curr < 2
	outputResult("verifyCoreClass", ok, verbose, paste("value", curr))
	ok
}

cmp.table <- function(a, b)
{
    aa <- unclass(a)
    bb <- unclass(b)
    if (identical(dim(aa), dim(bb))) {
        return(all(aa == bb))
    } else {
        return(FALSE)
    }
}

verifyMEval <- function(pred, cl, mEval)
{
    accuracy <- mean(pred$class == cl)
    res1 <- accuracy==accuracy
    aux.pred.mat <- table(cl, pred$class)
    res2 <- cmp.table(mEval$predictionMatrix, aux.pred.mat)
    all(c(res1, res2))
}

outputResult <- function(testName, status, verbose, failMessage=NULL)
{
    if (!all(status)) {
		if (is.null(failMessage)) {
			failMessage <- ""
		} else {
			failMessage <- paste(" (", failMessage, ")", sep="")
		}
		cat(testName, " FAILED", failMessage, "\n", sep="")
		if (verbose <= 0)
			stop("test failed")
	}
}

asTxt <- function(ok)
{
	if (all(ok)) {
		"OK"
	} else {
		"FAIL"
	}
}

testCoreClass <- function(verbose=1)
{
	ncases <- 1000
	RNGversion("2.9.2")
	set.seed(12345)
	train <- classDataGen(ncases)
	test <- classDataGen(ncases)
    model <- CoreModel(class ~ ., train, model="rf", rfNoTrees=100)
    pred <- predict(model, test, rfPredictClass=FALSE)
    # consistency of predict output
    res1 <- identical(pred$class == "1", pred$probabilities[,1] >= 0.5)
    # ROC test
    res2 <- verifyCoreClass(test, pred, verbose)
    # modelEval test
    mEval <- modelEval(model, test$class, pred$class, pred$prob)
    res3 <- verifyMEval(pred, test$class, mEval)
    ok <- c(res1, res2, res3)
	outputResult("testCoreClass", ok, verbose, paste(res1, res2, res3))
	all(ok)
}

testCoreAttrEval <- function(verbose=1)
{
    ncases <- 1000
    RNGversion("2.9.2")
    set.seed(0)
    train <- classDataGen(ncases)
    #test <- classDataGen(ncases)
    estReliefF <- attrEval(class ~ ., train, estimator="ReliefFexpRank")
    comp1<-c(0.050083407,0.078033329,0.042187049,0.034625011,0.064074820,0.057376281,
             0.007454174,0.136318607,0.109862236,0.001486706)
    res1 <- isTRUE(all.equal(unname(estReliefF), comp1))
    estMdl <- attrEval(class ~ ., train, estimator="MDL")
    comp2 <- c(0.0430367148311, 0.0726888549425, 0.0315160729442, 0.0280291538191, 0.0581082266459,
            0.0413914181947, -0.0095497415727, 0.1069959122855, 0.1045578033412, 0.0008384525753)
    res2 <- isTRUE(all.equal(unname(estMdl), comp2))
	#res2 <- FALSE # simulate an error
    ok <- c(res1, res2)
	outputResult("testCoreAttrEval", ok, verbose, paste(res1, res2))
	all(ok)
}

testCoreReg <- function(verbose=1)
{
    ncases <- 1000
    RNGversion("2.9.2")
    set.seed(0)
    train <- regDataGen(ncases)
    test<- regDataGen(ncases)
    model <- CoreModel(response~., train, model="regTree")
    pred <- predict.CoreModel(model, test)
    # Model evaluation
    mEval <- modelEval(model, test[["response"]], pred)
    comp1<-c(0.765781, 0.786064, 0.4801148, 0.6943131)
    res1 <- isTRUE(all.equal(c(mEval$MSE,mEval$RMSE,mEval$MAE,mEval$RMAE), comp1,tolerance=1e-6))
    # Attribute evaluation with RReliefFexpRank
    estReliefF <- attrEval(response~., train, estimator="RReliefFexpRank")
    comp2<-c(0.010290964,-0.009417843,-0.024053605, -0.040329099, -0.037327744,
             -0.003113488, -0.036898833,  0.012602801,  0.082472855,  0.163371230) 
    res2 <- isTRUE(all.equal(unname(estReliefF), comp2,tolerance=1e-6))
    # Attribute evaluation with MSEofMean
    comp3 <- c( -0.6673072, -0.6623068, -0.7465442, -0.7448963, -0.7220145, -0.7282959,
               -0.7430563, -0.7198671, -0.6793427, -0.6390350)
    estMSE <- attrEval(response~., train, estimator="MSEofMean")
    res3 <- isTRUE(all.equal(unname(estMSE), comp3,tolerance=1e-6))
    ok <- c(res1, res2, res3)
	outputResult("testCoreReg", ok, verbose)
	all(ok)
}


testCoreOrdEval <- function(verbose=1) {
    ncases <- 1000
    RNGversion("2.9.2")
    set.seed(0)
    train <-  ordDataGen(ncases)
    estOrdEval <- ordEval(class~., train, ordEvalNoRandomNormalizers=0 )
    comp1<-c(0.48737601443, 0.64164648910, 0.31284403670, 0.40484429066,
             0.30640535373, 0.39493899684, 0.25654450262, 0.24069352371)
    res1 <- isTRUE(all.equal(unname(estOrdEval$reinfPosAttr), comp1))
    comp2 <- c(0.47735849057, 0.62565320665, 0.30726515502, 0.37860861335,
               0.27582938389, 0.35876840696, 0.25065160730, 0.24556062912)
    res2 <- isTRUE(all.equal(unname(estOrdEval$reinfNegAttr), comp2))
    comp3<- c(0.47951253974, 0.54837049743, 0.43117158509, 0.45156889495,
              0.43170058641, 0.46538045438, 0.42272646060, 0.43029004614)
    res3 <- isTRUE(all.equal(unname(estOrdEval$anchorAttr), comp3))
    ok <- c(res1, res2, res3)
	outputResult("testCoreOrdEval", ok, verbose)
	all(ok)
}

gener.Reg <- function(m,n)
{
    x <- matrix(runif(m*n),nrow=m,ncol=n)
    data.frame(x,resp=rowSums(x))
}

singleTestNA <- function(t, x)
{
    .C("testNA",
    as.integer(t),
    as.double(x),
    out=integer(2),
    NAOK=TRUE,
    PACKAGE="CORElearn")$out
}

testCoreNA <- function(verbose=1)
{
    a <- matrix(nrow=4, ncol=2)
    a[1,] <- singleTestNA(0, NA) # pass NA to CORElearn
    a[2,] <- singleTestNA(0, NaN) # pass NaN to CORElearn
    a[3,] <- singleTestNA(1, 0) # use internal NAcont
    a[4,] <- singleTestNA(2, 0) # generate NaN
    ok <- a == rbind(c(1,0), c(0,1), c(1,0), c(0,1))
	outputResult("testCoreNA", ok, verbose)
	all(ok)
}

testCoreRPORT <- function(verbose=1)
{
    tmp <- .C("testRPORT", a=as.integer(2), PACKAGE="CORElearn")
    ok <- tmp$a == 1
	outputResult("testCoreRPORT", ok, verbose, tmp$a)
	all(ok)
}

testCoreRand <- function(verbose=1)
{
    n <- 10
    runif(1)
    state <- .Random.seed
    x <- runif(n)
    .Random.seed <<- state
    y <- .C("testCoreRand", as.integer(n), a=double(n), PACKAGE="CORElearn")$a
	ok <- x == y
	outputResult("testCoreRand", ok, verbose, paste(x[1], x[2], y[1], y[2]))
	all(ok)
}

allTests <- function(verbose=1,timed=FALSE)
{
    t1 <- system.time(r1 <- testCoreClass(1))
    cat("testCoreClass()    : ", asTxt(r1), "\n")
    if (timed) cat("Elapsed", t1["elapsed"],"sec\n")
    t2 <- system.time(r2 <- testCoreAttrEval(1))
    cat("testCoreAttrEval() : ", asTxt(r2), "\n")
    if (timed) cat("Elapsed", t2["elapsed"],"sec\n")
    t3 <- system.time(r3 <- testCoreReg(1))
    cat("testCoreReg()      : ", asTxt(r3), "\n")
    if (timed) cat("Elapsed", t3["elapsed"],"sec\n")
    t4 <- system.time(r4 <- testCoreOrdEval(1))
    cat("testCoreOrdEval()  : ", asTxt(r4), "\n")
    if (timed) cat("Elapsed", t4["elapsed"],"sec\n")
    r5 <- testCoreNA(1)
    cat("testCoreNA()       : ", asTxt(r5), "\n")
    r6 <- testCoreRPORT(1)
    cat("testCoreRPORT()    : ", asTxt(r6), "\n")
    r7 <- testCoreRand(1)
    cat("testCoreRand()     : ", asTxt(r7), "\n")
	ok <- c(r1, r2, r3, r4, r5, r6, r7)
	outputResult("allTests", ok, verbose)
	invisible(all(ok))
}

testClassPseudoRandom <- function(s, k, m)
{
	n <- length(s)
	aux <- .C("testClassPseudoRandom",
		n = as.integer(n),
		s = as.integer(s),
		k = as.integer(k),
		m = as.integer(m),
		x = double(k*m),
		PACKAGE="CORElearn")
	matrix(aux$x, nrow=k, ncol=m)
}

testTime <- function()
{
	.C("testTime", x=double(1), PACKAGE="CORElearn")$x
}

