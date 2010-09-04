absDiffElem <- function(w)
{
    s1 <- w[1, 2] - w[1, 3]
    s2 <- w[2, 2] - w[2, 3]
    h <- abs(w[1, 1] - w[2, 1])
    if (sign(s1*s2) >= 0) {
        return(h*(abs(s1) + abs(s2))/4)
    } else {
        return(h*(s1^2 + s2^2)/(abs(s1) + abs(s2))/4)
    }
}

absDiffROC <- function(a1, a2)
{
    b1 <- a1 %*% cbind(c(1, 1), c( - 1, 1))
    b2 <- a2 %*% cbind(c(1, 1), c( - 1, 1))
    v <- sort(c(b1[,1],b2[,1]))
    if (b1[1,1] != 0) {
        b1 <- rbind(c(0, 0), b1)
    }
    if (b1[nrow(b1),1] != 2) {
        b1 <- rbind(b1, c(2, 0))
    }
    if (b2[1,1] != 0) {
        b2 <- rbind(c(0, 0), b2)
    }
    if (b2[nrow(b2),1] != 2) {
        b2 <- rbind(b2, c(2, 0))
    }
    w <- cbind(v,
        approx(x=b1[,1], y=b1[,2], xout=v)$y,
        approx(x=b2[,1], y=b2[,2], xout=v)$y)
    w <- rbind(c(0, 0, 0), w, c(2, 0, 0))
    res <- 0
    for (i in 2:nrow(w)) {
        res <- res + absDiffElem(w[c(i-1,i),])
    }
    as.double(res)
}

pointsCore <- function()
{
    ncases <- 1000
    RNGversion("2.9.2")
    set.seed(4)
    train <- classDataGen(ncases)
    test <- classDataGen(ncases)
    model <- CoreModel(class ~ ., train, model="rf", rfNoTrees=100)
    pred <- predict.CoreModel(model, test, rfPredictClass=FALSE)$probabilities[,1]
    thr <- c(
        0.960,0.940,0.914,0.884,0.849,0.815,0.771,0.717,0.650,0.571,
        0.480,0.381,0.302,0.240,0.180,0.130,0.096,0.070,0.044)
    a <- matrix(nrow=length(thr), ncol=2)
    for (i in 1:length(thr)) {
        v <- test$class[pred >= thr[i]]
        a[i,1] <- sum(v == 2)
        a[i,2] <- sum(v == 1)
    }
    a[,1] <- a[,1]/sum(test$class==2)
    a[,2] <- a[,2]/sum(test$class==1)
    a
}

verifyROC <- function()
{
    current <- pointsCore()
    expect <- rbind(
        c(0.0000, 0.0777),
        c(0.0006, 0.1937),
        c(0.0020, 0.3235),
        c(0.0020, 0.4177),
        c(0.0041, 0.5029),
        c(0.0041, 0.5748),
        c(0.0120, 0.6405),
        c(0.0301, 0.7132),
        c(0.0552, 0.7948),
        c(0.0871, 0.8339),
        c(0.1320, 0.8929),
        c(0.2010, 0.9391),
        c(0.2565, 0.9648),
        c(0.3333, 0.9757),
        c(0.4484, 0.9877),
        c(0.5872, 0.9955),
        c(0.7003, 0.9964),
        c(0.7911, 0.9978),
        c(0.8998, 0.9992))
    d <- absDiffROC(current, expect)
    d <= 0.0037
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

outputResult <- function(verbose, status, out)
{
    if (all(status)) {
        cat("OK\n")
    } else {
        cat("FAILED\n")
        if (verbose >= 2)
		{
            print(out)
			return(all(status))
		}
    }
    invisible(NULL)
}

testCoreClass <- function(verbose=1)
{
    ncases <- 1000
    RNGversion("2.9.2")
    set.seed(0)
    train <- classDataGen(ncases)
    test <- classDataGen(ncases)
    model <- CoreModel(class ~ ., train, model="rf", rfNoTrees=100)
    pred <- predict(model, test, rfPredictClass=FALSE)
    # consistency of predict output
    res1 <- identical(pred$class == "1", pred$probabilities[,1] >= 0.5)
    # ROC test
    res2 <- verifyROC()
    # modelEval test
    mEval <- modelEval(model, test$class, pred$class, pred$prob)
    res3 <- verifyMEval(pred, test$class, mEval)
    ok <- c(res1=res1, res2=res2, res3=res3)
    if (!all(ok) && verbose <= 0)
			stop("test failed")
	outputResult(verbose, ok, ok)
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
    ok <- c(res1=res1, res2=res2)
    if (!all(ok) && verbose <= 0)
			stop("test failed")
	outputResult(verbose, ok, ok)
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
    
    ok <- c(res1=res1, res2=res2, res3=res3)

    if (!all(ok) && verbose <= 0)
			stop("test failed")
	outputResult(verbose, ok, ok)
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
    ok <- c(res1=res1, res2=res2, res3=res3)
    if (!all(ok) && verbose <= 0)
			stop("test failed")
	outputResult(verbose, ok, ok)
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
    if (!all(ok) && verbose <= 0)
			stop("test failed")
	outputResult(verbose, ok, ok)
}

testCoreRPORT <- function(verbose=1)
{
    tmp <- .C("testRPORT", a=as.integer(2), PACKAGE="CORElearn")
    ok <- tmp$a == 1
    if (!all(ok) && verbose <= 0)
			stop("test failed")
	outputResult(verbose, ok, tmp$a)
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
    if (!all(ok) && verbose <= 0)
			stop("test failed")
	outputResult(verbose, ok, rbind(x,y))
}

allTests <- function(verbose=1,timed=FALSE)
{
	v <- max(2, verbose)
    cat("testCoreClass()    : ")
    t1<-system.time(r1 <- testCoreClass(v))
    if (timed) cat("Elapsed",t1["elapsed"],"sec\n")
    cat("testCoreAttrEval() : ")
    t2<-system.time(r2 <- testCoreAttrEval(v))
    if (timed) cat("Elapsed",t2["elapsed"],"sec\n")
    cat("testCoreReg()      : ")
    t3<-system.time(r3 <- testCoreReg(v))
    if (timed) cat("Elapsed",t3["elapsed"],"sec\n")
    cat("testCoreOrdEval()  : ")
    t4<-system.time(r4 <- testCoreOrdEval(v))
    if (timed) cat("Elapsed",t4["elapsed"],"sec\n")
    cat("testCoreNA()       : ")
    r5 <- testCoreNA(v)
    cat("testCoreRPORT()    : ")
    r6 <- testCoreRPORT(v)
    cat("testCoreRand()     : ")
    r7 <- testCoreRand(v)
    cat("allTests()         : ")
	ok <- c(r1, r2, r3, r4, r5, r6, r7)
	if (!all(ok) && verbose <= 0)
		stop("some of the tests failed")
	outputResult(verbose, ok, ok)
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

