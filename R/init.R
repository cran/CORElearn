.onLoad <- function(lib, pkg) {
# .First.lib <- function(lib, pkg) {
	library.dynam("CORElearn", pkg, lib)
	initCore(1024) 
}

.Last.lib <- function(libpath) {
	destroyCore()
	library.dynam.unload("CORElearn", libpath)
}

initCore <- function(maxModels=1024)
{
	tmp <- .C("initCore",
			as.integer(maxModels), ## maximal number of models
			PACKAGE="CORElearn"
			)
}

destroyCore <- function()
{
	tmp <- .C("destroyCore",
			PACKAGE="CORElearn"
			)
}

