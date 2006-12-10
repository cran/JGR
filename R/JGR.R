#==========================================================================
# JGR - Java Gui for R
# Package version: 1.5-7
#
# $Id: JGR.R 127 2007-11-15 19:40:48Z helbig $
# (C)Copyright 2004,2005,2006,2007 Markus Helbig
# (C)Copyright 2004,2006,2007 Simon Urbanek
# Licensed under GPL v2

#==========================================================================
# initialization
#==========================================================================

library(utils)

# library initialization:
.First.lib <- function(lib, pkg) {
  ##cat("\nLoading additional JGR support...\n")
  library(rJava)
  je <- as.environment(match("package:JGR", search()))
  assign(".jgr.pkg.path", paste(lib,pkg,sep=.Platform$file.sep), je)
  assign(".jgr.works", FALSE, je)
  #assign(".jgr.env", new.env(), .je)
  
  ## we supply our own JavaGD class
  .setenv <- if (exists("Sys.setenv")) Sys.setenv else Sys.putenv
  .setenv("JAVAGD_CLASS_NAME"="org/rosuda/JGR/toolkit/JavaGD")

  ## now load rJava for callbacks
  ## strictly speaking we should not need to add JGR, because
  ## the launcher must set the correct classpath anyway
  cp <- paste(lib, pkg, "java", "JGR.jar",sep=.Platform$file.sep)
  .jinit(cp)

  ## next make sure and JRI and iBase are present
  add.classes <- character()
  if (is.jnull(.jfindClass("org/rosuda/JRI/REXP",silent=TRUE)))
    add.classes <- paste(installed.packages()["rJava","LibPath"],"rJava","jri","JRI.jar",sep=.Platform$file.sep)
  if (is.jnull(.jfindClass("org/rosuda/ibase/Common",silent=TRUE)))
    add.classes <- c(add.classes,paste(installed.packages()["iplots","LibPath"],"iplots","java","iplots.jar",sep=.Platform$file.sep))

  ## if any classes are missing or JGR was not started using main method, get out
  ## this should be true only if JGR was loaded into a "regular" R
  if (length(add.classes)>0 || !.jcall("org/rosuda/JGR/JGR","Z","isJGRmain")) {
    cat("\nPlease use the corresponding JGR launcher to start JGR.\nRun JGR() for details. You can also use JGR(update=TRUE) to update JGR.\n\n")
    return(TRUE)
  }

  ## JGR actually works
  assign(".jgr.works", TRUE, je)

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
  if (!.jgr.works) { cat("package.manager() cannot be used outside JGR.\n"); return(invisible(NULL)) }
	f <- .jnew("org/rosuda/JGR/JGRPackageManager")
	invisible(.jcall(f,,"setVisible",TRUE))
}

installPackages <-
function (contriburl = NULL, type = getOption("pkgType")) 
{
  if (!.jgr.works) { cat("installPackages() cannot be used outside JGR.\n"); return(invisible(NULL)) }
    if (type == "mac.binary") {
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
  if (!.jgr.works) { cat("object.browser() cannot be used outside JGR.\n"); return(invisible(NULL)) }
	f <- .jnew("org/rosuda/JGR/JGRObjectManager")
	invisible(.jcall(f,,"setVisible",TRUE))
}

jgr.pager <- function(file, header, title, delete.file) {
  if (!.jgr.works) { cat("jgr.pager() cannot be used outside JGR.\n"); return(invisible(NULL)) }
	invisible(.jcall("org/rosuda/JGR/toolkit/TextPager",,"launchPager",as.character(file), as.character(header), as.character(title), as.logical(delete.file)))
}

jgr.set.options <- function(..., useJavaGD=TRUE, useJGRpager=TRUE) {
  if (!.jgr.works) { cat("jgr.set.options() cannot be used outside JGR.\n"); return(invisible(NULL)) }
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
  if (!.jgr.works) { cat("jgr.addMenu() cannot be used outside JGR.\n"); return(invisible(NULL)) }
	invisible(.jcall("org/rosuda/JGR/JGR","V","addMenu",as.character(name)))
}

jgr.addMenuItem <- function(menu, name, command) {
  if (!.jgr.works) { cat("jgr.addMenuItem() cannot be used outside JGR.\n"); return(invisible(NULL)) }
  if (is.function(command))
    command <- .jgr.register.function(command)
  invisible(.jcall("org/rosuda/JGR/JGR","V","addMenuItem",as.character(menu),as.character(name),as.character(command)))
}

jgr.addMenuSeparator <- function(menu) {
  if (!.jgr.works) { cat("jgr.addMenuSeparator() cannot be used outside JGR.\n"); return(invisible(NULL)) }
	invisible(.jcall("org/rosuda/JGR/JGR","V","addMenuSeparator",as.character(menu)))
}

# creates a 'command' based on a function by calling the function without arguments
.jgr.register.function <- function(fun) {
  if (is.null(.GlobalEnv$.jgr.user.functions)) .GlobalEnv$.jgr.user.functions <- list()
  fnc <- length(.GlobalEnv$.jgr.user.functions)+1
  .GlobalEnv$.jgr.user.functions[[fnc]] <- fun
  paste(".jgr.user.functions[[",fnc,"]]()",sep='')
}

#'internal' functions for JGR, without them JGR is not able to survive

.completeCommand <- function (x) 
{
    result <- c()
    if (regexpr('\\$$', x) > -1) {
         r <- names(get(sub('\\$', '', x)))
         for (i in 1:length(r)) {
             result <- c(result,r[i])
         }    
    }
    else if (regexpr('\\$', x) > -1) {
        r <- names(get(substr(x,0,gregexpr("\\$",x)[[1]][1]-1)))        
        for (i in 1:length(r)) {
            if (regexpr(strsplit(x,'\\$')[[1]][2],r[i]) > -1)
            result <- c(result,r[i])
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
  if (!.jgr.works) { cat(".refresh() cannot be used outside JGR.\n"); return(invisible(NULL)) }
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

.generate.run.script <- function(target=NULL) {
  run.template <- paste(.jgr.pkg.path,"scripts","run.in",sep=.Platform$file.sep)
  rt <- readLines(run.template)
  settings <- c("R_SHARE_DIR", "R_INCLUDE_DIR", "R_DOC_DIR", "R_LIBS", 
                "R_HOME", "JAVA_HOME", "JAVA_LD_PATH", "JAVA_PROG", "RJAVA")
  sl <- list()
  for (i in settings) sl[[i]] <- Sys.getenv(i)
  if (nchar(sl[['JAVA_PROG']])==0) {
    if (nchar(sl[['JAVA_HOME']])>0) {
      jc <- paste(sl[['JAVA_HOME']],"bin","java",sep=.Platform$file.sep)
      if (file.exists(jc)) sl[['JAVA_PROG']] <- jc
    } else sl[['JAVA_PROG']] <- "java"
  }
  if (nchar(sl[['JAVA_LD_PATH']])==0) {
    sl[['JAVA_LD_PATH']] <- Sys.getenv("R_JAVA_LD_LIBRARY_PATH")
    if (nchar(sl[['JAVA_LD_PATH']])==0) {
      sl[['JAVA_LD_PATH']] <- Sys.getenv("LD_LIBRARY_PATH")
    }
  }
  sl[['JAVA_LD_PATH']] <- paste(sl[['JAVA_LD_PATH']],system.file("jri",package="rJava"),sep=.Platform$path.sep)

  sl[['JGR_JAR']] <- system.file("java","JGR.jar",package="JGR")
  sl[['JRI_JAR']] <- system.file("jri","JRI.jar",package="rJava")
  sl[['IPLOTS_JAR']] <- system.file("java","iplots.jar",package="iplots")
  sl[['RJAVA']] <- system.file(package='rJava')
  ## do all the substitutions
  for (i in names(sl))
    rt <- gsub(paste('@',i,'@',sep=''), sl[[i]], rt)

  ## return back the entire file if there is no target
  if (is.null(target)) return(rt)
  
  ## otherwise save into resulting file
  writeLines(rt, target)
}


JGR <- function(update=FALSE)
  {
    if (!update && .jgr.works && .jcall("org/rosuda/JGR/JGR","Z","isJGRmain")) {
      cat("JGR is already running. If you want to re-install or update JGR, use JGR(update=TRUE).\n")
      return(invisible(FALSE))
    }

    if (update) {
      # Win & OS X lanchers insist on site library
      lt <- paste(R.home(),"library",sep=.Platform$file.sep)
      if (.Platform$OS.type == "unix" && .Platform$pkgType != "mac.binary")
        lt <- .libPaths()[1]
      cran <- getOption("repos")
      if (cran == "@CRAN@") cran <- "http://cran.r-project.org/"
      return (install.packages(c("JGR","rJava","JavaGD","iplots"), lt, c(cran,"http://www.rforge.net/")))
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

    runs <- paste(.jgr.pkg.path, "scripts", "run", sep=.Platform$file.sep)
    if (file.exists(runs)) {
      cat("Starting JGR ...\n(You can use",runs,"to start JGR directly)\n")
      system(paste("sh ",runs,"&"))
    } else {
      rs <- .generate.run.script()
      wl <- try(writeLines(rs, runs),silent=TRUE)
      if (inherits(wl,"try-error")) {
        cat("Please consider running JGR() as root to create a start script in",runs,"automatically.\n")
        fn <- tempfile("jgrs")
        wl <- try(writeLines(rs, fn),silent=TRUE)
        if (inherits(wl,"try-error"))
          stop("Cannot create JGR start script. Please run JGR() as root to create a start script ",runs)
        system(paste("chmod a+x '",fn,"'",sep=''))
        cat("Starting JGR ...\n")
        system(paste("sh ",fn,"&"))
        system("sh -c 'sleep 3'") # give the shell some time to read the script
        unlink(fn)
      } else {
        cat("Starting JGR run script. This can be done from the shell as well, just run\n",runs,"\n\n")
        system(paste("chmod a+x '",runs,"'",sep=''))
        system(paste("sh ",runs,"&"))
      }
    }
  }
