#==========================================================================
# JGR - Java Gui for R
# Package version: 1.4-2
#
# $Id: JGR.R,v 1.51 2006/05/27 13:07:45 helbigm Exp $
# (C)Copyright 2004-2006 Markus Helbig
#

#==========================================================================
# initialization
#==========================================================================

library(utils)

# library initialization:
.First.lib <- function(lib, pkg) {
    #cat("\nLoading additional JGR support...\n")
  library(rJava)
        je <- as.environment(match("package:JGR", search()))
        assign(".jgr.pkg.path", paste(lib,pkg,sep=.Platform$file.sep), je)

	  # we supply our own JavaGD class
	Sys.putenv("JAVAGD_CLASS_NAME"="org/rosuda/JGR/toolkit/JavaGD")

	# now load rJava for callbacks
	cp<-paste(lib,pkg,"cont","JGR.jar",sep=.Platform$file.sep)
	.jinit(cp)

        if (!.jcall("org/rosuda/JGR/JGR","Z","isJGRmain")) {
          cat("\nPlease use the corresponding JGR launcher to start JGR.\nRun JGR() for details. You can also use JGR(update=TRUE) to update JGR.\n");
          return(TRUE);
        }
  
	# set JGR options unless the user doesn't want us to
	if (Sys.getenv("JGR_NO_OPTIONS")=="")
    	jgr.set.options()

	# set RHome Path in JGR
	invisible(.jcall("org/rosuda/JGR/JGR","V","setRHome",as.character(R.home())))
			
	invisible(.jcall("org/rosuda/JGR/JGR","V","setRLibs",as.character(.libPaths())))
			
	invisible(.jcall("org/rosuda/JGR/JGR","V","setKeyWords",as.character(.refreshKeyWords())))
			
	invisible(.jcall("org/rosuda/JGR/JGR","V","setObjects",as.character(.refreshObjects())))
	
	# set repos
	if (options("repos")=="@CRAN@") options(repos="http://cran.r-project.org")
			
	# set Path to helpFiles
	.refreshHelpFiles()

  	# add PackageInstaller
	jgr.addMenuItem("Packages","Package Installer","installPackages()")
}


package.manager <- function() {
	f <- .jnew("org/rosuda/JGR/JGRPackageManager")
	invisible(.jcall(f,,"setVisible",TRUE))
}

installPackages <-
function (contriburl = NULL, type = "binaries") 
{
    if (type == "binaries" && Sys.info()[["sysname"]] == "Darwin") {
        if (R.version$major >= 2 && R.version$minor >= 2) 
            a <- available.packages(contriburl = contrib.url(getOption("repos"), 
                type = "mac.binary"))
        else if (R.version$major >= 2 && R.version$minor >= 1) 
            a <- CRAN.packages(contriburl = contrib.url(getOption("repos"), 
                type = "mac.binary"))
        else a <- CRAN.packages(contriburl = contrib.url(getOption("CRAN"), 
            type = "mac.binary"))
    }
    else if (!is.null(contriburl)) 
        if (R.version$major >= 2 && R.version$minor >= 2) 
            a <- available.packages(contriburl = contriburl)
        else
        	   a <- CRAN.packages(contriburl = contriburl)
    else if (R.version$major >= 2 && R.version$minor >= 2) 
            a <- available.packages()
        else
        	   a <- CRAN.packages()
    pkgs <- a[, 1]
    if (length(pkgs) > 0) {
        f <- .jnew("org/rosuda/JGR/JGRPackageInstaller", pkgs, 
            type)
        invisible(.jcall(f, , "setVisible", TRUE))
    }
}

object.browser <- function() {
	f <- .jnew("org/rosuda/JGR/JGRObjectManager")
	invisible(.jcall(f,,"setVisible",TRUE))
}

jgr.pager <- function(file, header, title, delete.file) {
	invisible(.jcall("org/rosuda/JGR/toolkit/TextPager",,"launchPager",as.character(file), as.character(header), as.character(title), as.logical(delete.file)))
}

jgr.set.options <- function(..., useJavaGD=TRUE, useJGRpager=TRUE) {
	if (useJavaGD) {
    	require(JavaGD)
		options(device="JavaGD")
	}
	if (useJGRpager) {
    	options(pager=jgr.pager)
	}
}

# add new menus at runtime to JGR Console

jgr.addMenu <- function(name) {
	invisible(.jcall("org/rosuda/JGR/JGR","V","addMenu",as.character(name)))
}

jgr.addMenuItem <- function(menu,name,command) {
	invisible(.jcall("org/rosuda/JGR/JGR","V","addMenuItem",as.character(menu),as.character(name),as.character(command)))
}

jgr.addMenuSeparator <- function(menu) {
	invisible(.jcall("org/rosuda/JGR/JGR","V","addMenuSeparator",as.character(menu)))
}


# update JGR packages

#update.JGR <- function(CRAN = getOption("CRAN"), contriburl = contrib.url(CRAN)) {
#    available <- CRAN.packages(contriburl = contriburl)
#    old <- old.packages(contriburl = contriburl,available = available)
#    update <- NULL
#    if (!is.null(old)) {
#        for (k in 1:nrow(old)) {
#        	  name <- old[k,"Package"]
#        	  if (name == "JGR" || name == "rJava" || name == "JavaGD")
#        	       update <- rbind(update, old[k, ])
#        }
#    }
#    if (length(update) > 0) 
#        install.packages(update[, "Package"], contriburl = contriburl)
#}

#'internal' functions for JGR, without them JGR is not able to survive

.completeCommand <- function (x) 
{
    result <- c()
    if (regexpr('\\$$', x) > -1) {
        r <- names(get(sub('\\$', '', x)))
        for (i in 1:length(r)) {
            result <- c(result,sub("\" +","\"",sub(" +\"$","\"",paste("\"",r[i],"\""))))
        }
    }
    else {
        n <- length(search())
        patt <- paste("^", as.character(x), ".*", sep = "")
        for (i in 1:n) {
            result <- c(result, ls(pos = i, all.names = TRUE, 
                pattern = patt))
        }
    }
    sort(result)
}

.refresh <- function() {
	invisible(.jcall("org/rosuda/JGR/JGR","V","setRLibs",as.character(.libPaths())))
	invisible(.jcall("org/rosuda/JGR/JGR","V","setKeyWords",as.character(.refreshKeyWords())))
	invisible(.jcall("org/rosuda/JGR/JGR","V","setObjects",as.character(.refreshObjects())))
	try(.refreshHelpFiles(TRUE),silent=TRUE)
}

.refreshHelpFiles <- function(silent=FALSE) {
	if (.Platform$OS.type == "windows") {
		try(make.packages.html(.libPaths()))
		try(make.search.html(.libPaths()))
		try(fixup.libraries.URLs(.libPaths()))
	}
	else {
    	if (!silent)
		cat("Creating per-session help links...\n")
		.Script("sh", "help-links.sh", paste(tempdir(), paste(.libPaths(), collapse = " ")))
		make.packages.html()
	}
}

# refresh KeyWords (used by SyntaxHighlighting)

.refreshKeyWords <- function() {
    n <- length(search())
    result <- c()
    for (i in 2:n) {
    	result <- c(result,ls(pos=i,all.names=TRUE))
    }
    result
}

# refresh Objects (used by SyntaxHighlighting and ObjectManager)

.refreshObjects <- function() {
	# currently only use the objects we find in pos=1
	result <- c(ls(pos=1,all.names=TRUE))
	result
}

.getModels <- function() {
    objects <- ls(pos=1)
    result <- c();
    if (length(objects) > 0) for (i in 1:length(objects)) {
    	model <- get(objects[i])
        cls <- class(model)
        if (cls[1] == "lm" || cls[1] == "glm") result <- c(result,c(objects[i],cls[1]))
    }
    result
}

.getFunctionsInWS <- function() {
    objects <- ls(pos=1)
    result <- c();
    if (length(objects) > 0) for (i in 1:length(objects)) {
    	cls <- class(get(objects[i]))[1]
        if (cls == "function") result <- c(result,objects[i])
    }
    result
}

.getDataObjects <- function() {
    objects <- ls(pos=1)
    result <- c();
    if (length(objects) > 0) for (i in 1:length(objects)) {
    	d <- get(objects[i])
    	cls <- class(d)[1]
        if (cls == "data.frame" || cls == "table") result <- c(result,objects[i],cls)
    }
    result
}

.getOtherObjects <- function() {
    objects <- ls(pos=1)
    result <- c();
    if (length(objects) > 0) for (i in 1:length(objects)) {
    	if (objects[i] != "last.warning" && objects[i] != "*tmp*") {
	    	cls <- class(get(objects[i]))[1]
   			if (cls != "data.frame" && cls != "table" && cls != "function") result <- c(result,objects[i],cls)
        }
    }
    result
}

.getContent <- function (o, p = NULL)
{
    result <- c()
    if (class(o) == "table")
        o <- dimnames(o)
    if (class(p) == "table") {
        dn <- o
        for (i in 1:length(dn)) {
            try(result <- c(result, dn[i], class((dn[[i]]))[1]),
                silent = TRUE)
        }
    }
    else if (class(o) == "matrix") {
    	colnames <- colnames(o)
    	for (i in 1:dim(o)[2]) {
    		xname <- colnames[i]
    		if (is.null(xname)) xname <- "null"
            try(result <- c(result, xname, class((o[,i]))[1]),
                silent = TRUE)
        }

    }
    else {
        if (mode(o)=="list") {
	        for (i in 1:length(o)) {
		  		xname <- names(o)[i]
				if (is.null(xname)) xname <- "null"
            	try(result <- c(result, xname, class((o[[i]]))[1]), silent = TRUE)
	        }
        }
    }
    result
}

# copy the content of the specified JavaGD device to another device
.jgr.save.JavaGD.as <- function(useDevice, source, file=NULL, usefile=TRUE, ...)
{
  if (usefile && is.null(file)) {
    file<-file.choose(TRUE)
    if (is.null(file)) return(FALSE)
  }
  if (usefile)
    .javaGD.copy.device(source, useDevice, file=file, ...)
  else
    .javaGD.copy.device(source, useDevice, ...)
  invisible(NULL)
}

JGR <- function(update=FALSE)
  {
    if (!update && .jcall("org/rosuda/JGR/JGR","Z","isJGRmain")) {
      cat("JGR is already running. If you want to re-install or update JGR, use JGR(update=TRUE).\n")
      return(invisible(FALSE))
    }

    if (update) {
      # Win & OS X lanchers insist on site library
      lt <- paste(R.home(),"library",sep=.Platform$file.sep)
      if (.Platform$OS.type == "unix" && .Platform$pkgType != "mac.binary")
        lt <- .libPaths()[1]
      cran <- getOption("repos")[["CRAN"]]
      if (cran == "@CRAN@") cran <- "http://cran.r-project.org/"
      return (install.packages(c("JGR","rJava","JavaGD"), lt, c(cran,"http://www.rosuda.org/")))
    }
    
    # FIXME: we should invoke a start script ...
    if (.Platform$OS.type == "windows") {
      cat("On Windows JGR must be started using the JGR.exe launcher.\nPlease visit http://www.rosuda.org/JGR/ to download it.\n")
      return(invisible(FALSE))
    }

    if (length(grep("darwin",R.version$os))>0) {
      cat("Please use JGR.app launcher to start JGR.\nIt can be downloaded from http://www.rosuda.org/JGR/\n")
      return(invisible(FALSE))
    }

    runs <- paste(.jgr.pkg.path, "cont", "run", sep=.Platform$file.sep)
    if (file.exists(runs)) {
      cat("Starting JGR ...\n")
      system(paste("sh -c",runs))
    } else {
      cat("Please copy the run script from the JGRlinux package to",runs,", e.g.:\ncp run ",runs,"\nJGRlinux can be downloaded from http://www.rosuda.org/JGR/\n")
    }
  }
