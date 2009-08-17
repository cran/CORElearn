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
	d <= 0.003
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

testCore <- function()
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
	# attrEval tests
	estReliefF <- attrEval(class ~ ., train, estimator="ReliefFexpRank", ReliefIterations=30)
	comp6 <- c(0.050083406639, 0.078033329371, 0.042187048571, 0.034625010764, 0.064074820175,
			0.057376280676, 0.007454173703, 0.136318606746, 0.109862236434, 0.001486706426)
	res4 <- isTRUE(all.equal(estReliefF, comp6))
	estMdl <- attrEval(class ~ ., train, estimator="MDL")
	comp7 <- c(0.0430367148311, 0.0726888549425, 0.0315160729442, 0.0280291538191, 0.0581082266459,
			0.0413914181947, -0.0095497415727, 0.1069959122855, 0.1045578033412, 0.0008384525753)
	res5 <- isTRUE(all.equal(estMdl, comp7))
	#return(c(res1, res2, res3, res4, res5))
	if (all(c(res1, res2, res3, res4, res5))) {
		cat("Verify CORElearn: OK\n")
	} else {
		cat("Verify CORElearn: FAILED\n")
	}
}

testReg.Core <- function(seed, ncases, ...)
{
	set.seed(seed)
    train <- regDataGen(ncases)
    test<- regDataGen(ncases)
	cat("\nModel:\n")
	model <- CoreModel(response~., train, model="regTree", ...)
	print(model)
	cat("\nPrediction:\n")
	pred <- predict.CoreModel(model, test)
	print(pred)
	cat("\n")
	cat("\nModel evaluation\n")
	mEval <- modelEval(model, test[["response"]], pred)
	mEval
	estReliefF <- attrEval(response~., train, estimator="RReliefFexpRank", ReliefIterations=30)
	cat("Attribute evaluation with RReliefFexpRank:\n",estReliefF,"\n")
	estMSE <- attrEval(response~., train, estimator="MSEofMean")
	cat("Attribute evaluation with MSEofMean:\n",estMSE,"\n")
	cat("\n")
	invisible(NULL)
}


test.ordEval <- function(seed, ncases, ...)
{
    set.seed(seed)
    train <- ordDataGen(ncases)
	cat("Ordered attribute evaluation\n")
	est <- ordEval(class~., train, ordEvalNoRandomNormalizers=30,... )
	print(est)
	cat("\n")
	invisible(est)
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

testNA <- function()
{
	a <- matrix(nrow=4, ncol=2)
	a[1,] <- singleTestNA(0, NA) # pass NA to CORElearn
	a[2,] <- singleTestNA(0, NaN) # pass NaN to CORElearn
	a[3,] <- singleTestNA(1, 0) # use internal NAcont
	a[4,] <- singleTestNA(2, 0) # generate NaN
	all(a == rbind(c(1,0), c(0,1), c(1,0), c(0,1)))
}

