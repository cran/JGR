#==========================================================================
# JGR - Java Gui for R
# Package version: 1.7-3
#
# $Id: JGR.R 275 2010-09-24 08:12:00Z helbig $
# (C)Copyright 2004-2010 Markus Helbig
# (C)Copyright 2009,2010 Ian Fellows
# (C)Copyright 2004,2006,2007 Simon Urbanek
# Licensed under GPL v2

#==========================================================================
# initialization
#==========================================================================


# library initialization:
.First.lib <- function(lib, pkg) {
  library(utils)
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
			
  	# add PackageInstaller
	# jgr.addMenuItem("Packages","Package Installer","installPackages()")
}


package.manager <- function() {
  if (!.jgr.works) { cat("package.manager() cannot be used outside JGR.\n"); return(invisible(NULL)) }
	f <- .jcall("org/rosuda/JGR/JGRPackageManager",,"showInstance")
}

installPackages <- function (contriburl = NULL, type = getOption("pkgType")) 
{
    if (!.jgr.works) {
        cat("installPackages() cannot be used outside JGR.\n")
        return(invisible(NULL))
    }
    if (type == "mac.binary") {
        if (R.version$major >= 2 && R.version$minor >= 2) 
            a <- available.packages(contriburl = contrib.url(getOption("repos"), 
															 type = "mac.binary"))
				else if (R.version$major >= 2 && R.version$minor >= 1) 
					a <- available.packages(contriburl = contrib.url(getOption("repos"), 
																type = "mac.binary"))
						else a <- available.packages(contriburl = contrib.url(getOption("CRAN"), 
																		 type = "mac.binary"))
    }
		else if (!is.null(contriburl)) 
			if (R.version$major >= 2 && R.version$minor >= 2) 
				a <- available.packages(contriburl = contriburl)
			else a <- available.packages(contriburl = contriburl)
		else if (R.version$major >= 2 && R.version$minor >= 2) 
			a <- available.packages()
		else a <- available.packages()
		pkgs <- a[, 1]
		if (length(pkgs) > 0) {
			invisible( .jcall("org/rosuda/JGR/JGRPackageInstaller",,"instAndDisplay",pkgs, 
								type))
		}
}

object.browser <- function() {
  if (!.jgr.works) { cat("object.browser() cannot be used outside JGR.\n"); return(invisible(NULL)) }
	f <- .jcall("org/rosuda/JGR/JGRObjectManager",,"showInstance")
}

jgr.pager <- function(file, header, title, delete.file) {
  if (!.jgr.works) { cat("jgr.pager() cannot be used outside JGR.\n"); return(invisible(NULL)) }
	invisible(.jcall("org/rosuda/JGR/toolkit/TextPager",,"launchPager",as.character(file), as.character(header), as.character(title), as.logical(delete.file)))
}

jgr.browser <- function(url, ...) {
  if (!.jgr.works) { cat("jgr.browser() cannot be used outside JGR.\n"); return(invisible(NULL)) }
        invisible(.jcall("org/rosuda/JGR/JGRHelp",, "showURL", as.character(url)[1]))
}

jgr.set.options <- function(..., useJavaGD=TRUE, useJGRpager=TRUE, useJGRbrowser=TRUE, useHTMLHelp=TRUE) {
  if (!.jgr.works) { cat("jgr.set.options() cannot be used outside JGR.\n"); return(invisible(NULL)) }
	if (useJavaGD) {
    	require(JavaGD)
		options(device="JavaGD")
	}
	if (useJGRpager) {
    	options(pager=jgr.pager)
	}
	if (useJGRbrowser) {
    	options(browser=jgr.browser)
	}
	if (useHTMLHelp) {
	options(help_type='html')
	tools:::startDynamicHelp()
	}
}

# add new menus at runtime to JGR Console

jgr.addMenu <- function(name) {
  if (!.jgr.works) { cat("jgr.addMenu() cannot be used outside JGR.\n"); return(invisible(NULL)) }
	invisible(.jcall("org/rosuda/JGR/JGR","V","addMenu",as.character(name)))
}

jgr.insertMenu <- function(name, index) {
	if (!.jgr.works) { cat("jgr.insertMenu() cannot be used outside JGR.\n"); return(invisible(NULL)) }
	invisible(.jcall("org/rosuda/JGR/JGR","V","insertMenu",as.character(name),as.integer(index-1)))
}

jgr.addMenuItem <- function(menu, name, command,silent=TRUE) {
  if (!.jgr.works) { cat("jgr.addMenuItem() cannot be used outside JGR.\n"); return(invisible(NULL)) }
  if (is.function(command))
    command <- .jgr.register.function(command)
  invisible(.jcall("org/rosuda/JGR/JGR","V","addMenuItem",as.character(menu),
				  as.character(name),as.character(command),as.logical(silent)))
}

jgr.insertMenuItem <- function(menu, name, command, index,silent=TRUE) {
	if (!.jgr.works) { cat("jgr.insertMenuItem() cannot be used outside JGR.\n"); return(invisible(NULL)) }
	if (is.function(command))
		command <- .jgr.register.function(command)
	invisible(.jcall("org/rosuda/JGR/JGR","V","insertMenuItem",as.character(menu),
					as.character(name),as.character(command),as.logical(silent),as.integer(index-1)))
}

jgr.addSubMenu <- function(menu, subMenuName, labels, commands) {
	if (!.jgr.works) { cat("jgr.addSubMenu() cannot be used outside JGR.\n"); return(invisible(NULL)) }
	
	invisible(J("org/rosuda/JGR/JGR")$addSubMenu(menu,subMenuName,labels,commands))
}

jgr.insertSubMenu <- function(menu, subMenuName, labels, commands,index) {
	if (!.jgr.works) { cat("jgr.addSubMenu() cannot be used outside JGR.\n"); return(invisible(NULL)) }
	
	invisible(J("org/rosuda/JGR/JGR")$addSubMenu(menu,subMenuName,as.integer(index-1),labels,commands))
}

jgr.addMenuSeparator <- function(menu) {
  if (!.jgr.works) { cat("jgr.addMenuSeparator() cannot be used outside JGR.\n"); return(invisible(NULL)) }
	invisible(.jcall("org/rosuda/JGR/JGR","V","addMenuSeparator",as.character(menu)))
}

jgr.insertMenuSeparator <- function(menu,index) {
	if (!.jgr.works) { cat("jgr.insertMenuSeparator() cannot be used outside JGR.\n"); return(invisible(NULL)) }
	invisible(.jcall("org/rosuda/JGR/JGR","V","insertMenuSeparator",as.character(menu),as.integer(index-1)))
}

jgr.getMenuNames <- function() {
	if (!.jgr.works) { cat("jgr.getMenuNames() cannot be used outside JGR.\n"); return(invisible(NULL)) }
	J("org/rosuda/JGR/JGR")$getMenuNames()
}

jgr.getMenuItemNames <- function(menu) {
	if (!.jgr.works) { cat("jgr.getMenuItemNames() cannot be used outside JGR.\n"); return(invisible(NULL)) }
	J("org/rosuda/JGR/JGR")$getMenuItemNames(as.character(menu))
}

jgr.removeMenu <- function(index) {
	if (!.jgr.works) { cat("jgr.removeMenu() cannot be used outside JGR.\n"); return(invisible(NULL)) }
	J("org/rosuda/JGR/JGR")$removeMenu(as.integer(index-1))
}

jgr.removeMenuItem <- function(menu,index) {
	if (!.jgr.works) { cat("jgr.removeMenuItem() cannot be used outside JGR.\n"); return(invisible(NULL)) }
	J("org/rosuda/JGR/JGR")$removeMenuItem(as.character(menu), as.integer(index-1))
}

# creates a 'command' based on a function by calling the function without arguments
.jgr.register.function <- function(fun) {
  if (is.null(.GlobalEnv$.jgr.user.functions)) .GlobalEnv$.jgr.user.functions <- list()
  fnc <- length(.GlobalEnv$.jgr.user.functions)+1
  .GlobalEnv$.jgr.user.functions[[fnc]] <- fun
  paste(".jgr.user.functions[[",fnc,"]]()",sep='')
}

#'internal' functions for JGR, without them JGR is not able to survive


print.hsearch <- function(x, ...) {
  if (tools:::httpdPort > 0L) {
           path <- file.path(tempdir(), ".R/doc/html")
           dir.create(path, recursive = TRUE, showWarnings = FALSE)
           out <- paste("<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n",
               "<html><head><title>R: help</title>\n", "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=\"UTF-8\">\n",
               "<link rel=\"stylesheet\" type=\"text/css\" href=\"/doc/html/R.css\">\n",
               "</head><body>\n\n<hr>\n", sep = "")
           out <- c(out, "<p>", "Search Result", "</p><br>")
           out <- c(out, "<table width=\"100%\" summary=\"R Package list\">\n",
               "<tr align=\"left\" valign=\"top\">\n", "<td width=\"25%\">Topic</td><td>Package</td><td>Description</td></tr>\n")
           
	  result <- x$matches
	  for (i in 1:dim(result)[1]) {
           links <- paste("<a href=\"http://127.0.0.1:", tools:::httpdPort,"/library/", result[i,3], "/help/", result[i,1], "\">", result[i,1],"</a>", sep = "")
           out <- c(out, paste("<tr align=\"left\" valign=\"top\">\n","<td>",links,"</td><td>",result[i,3],"</td><td>",result[i,2],"</td></tr>\n",sep=""))
	  }
	  out <- c(out, "</table>\n</p>\n<hr>\n</body></html>")
          out
	  writeLines(out, file.path(path, paste(x$pattern,".html",sep="")))
          browseURL(paste("http://127.0.0.1:", tools:::httpdPort,"/doc/html/",x$pattern,".html", sep = ""))
  }
}

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
        if ( "lm" %in% cls || "glm" %in% cls) result <- c(result,c(objects[i],cls[1]))
    }
    result
}

.getFunctionsInWS <- function() {
    objects <- ls(pos=1)
    result <- c();
    if (length(objects) > 0) for (i in 1:length(objects)) {
    	cls <- class(get(objects[i]))
        if ("function" %in% cls) result <- c(result,objects[i])
    }
    result
}

.getDataObjects <- function() {
    objects <- ls(pos=1)
    result <- c();
    if (length(objects) > 0) for (i in 1:length(objects)) {
    	d <- get(objects[i])
    	cls <- class(d)
        if ("data.frame" %in% cls || "table" %in% cls) 
        	result <- c(result,objects[i],cls[1])
    }
    result
}

.getOtherObjects <- function() {
    objects <- ls(pos=1)
    result <- c();
    if (length(objects) > 0) for (i in 1:length(objects)) {
    	if (objects[i] != "last.warning" && objects[i] != "*tmp*") {
	    	cls <- class(get(objects[i]))
   			if (!("data.frame" %in% cls || "table" %in% cls || "function" %in% cls)) 
   				result <- c(result,objects[i],cls[1])
        }
    }
    result
}

.getContent <- function (o, p = NULL)
{
    result <- c()
    if ("table" %in% class(o))
        o <- dimnames(o)
    if ("table" %in% class(p)) {
        dn <- o
        for (i in 1:length(dn)) {
            try(result <- c(result, dn[i], class((dn[[i]]))[1]),
                silent = TRUE)
        }
    }
    else if ("matrix" %in% class(o)) {
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
  jri.jar <- system.file("jri","JRI.jar",package="rJava")
  if (nchar(jri.jar) == 0) stop("JRI is required but missing! Make sure R was configured with --enable-R-shlib and rJava was compiled with JRI support.")
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
  
  
  




