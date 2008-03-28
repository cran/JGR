#==========================================================================
# JGR - Java Gui for R
# Package version: 1.7-15
#
# $Id: JGR.R 350 2013-05-10 14:30:21Z helbig $
# (C)Copyright 2004-2011,2013 Markus Helbig
# (C)Copyright 2009,2012 Ian Fellows
# (C)Copyright 2004,2006,2007,2012 Simon Urbanek
# Licensed under GPL v2

#==========================================================================
# initialization
#==========================================================================

broken.gomp <- function() {
	# Linux may have the same issue, but we only care about OS X so far
	if (length(grep("^darwin", R.version$os)) == 0) 
		return(FALSE)
	isTRUE(try({
		f <- file(R.home("lib/libR.dylib"), "rb")
		gomp <- FALSE
		new.gomp <- FALSE
		on.exit(close(f))
		while (length(r <- readBin(f, "raw", 20 * 1024 * 1024)) > 
			0) {
			if (length(grepRaw("gomp_malloc", r, fixed = TRUE))) 
				gomp <- TRUE
			if (length(grepRaw("gomp_managed_threads", r, fixed = TRUE))) 
				new.gomp <- TRUE
		}
		gomp && !new.gomp
	}, silent = TRUE))
}

.jgr.pkg.path <- NULL
.jgr.works <- FALSE

# library initialization:
.onLoad <- function(lib, pkg) {
	.jgr.pkg.path <<- paste(lib, pkg, sep = .Platform$file.sep)
	.jgr.works <<- FALSE
	
	## we supply our own JavaGD class
	.setenv <- Sys.setenv
	.setenv(JAVAGD_CLASS_NAME = "org/rosuda/JGR/toolkit/JavaGD")
	
	## now load rJava for callbacks
	## strictly speaking we should not need to add JGR, because
	## the launcher must set the correct classpath anyway
	cp <- paste(lib, pkg, "java", "JGR.jar", sep = .Platform$file.sep)
	.jinit(cp)
	
	## next make sure and JRI and iBase are present
	add.classes <- character()
	if (is.jnull(.jfindClass("org/rosuda/JRI/REXP", silent = TRUE))) 
		add.classes <- system.file("jri", "JRI.jar", package = "rJava")
	if (is.jnull(.jfindClass("org/rosuda/ibase/Common", silent = TRUE))) 
		add.classes <- c(add.classes, system.file("java", "iplots.jar", 
			package = "iplots"))
	
	## if any classes are missing or JGR was not started using main method, get out
	if (length(add.classes) > 0 || !.jcall("org/rosuda/JGR/JGR", "Z", "isJGRmain")) return(TRUE)
	
	## JGR actually works
	.jgr.works <<- TRUE
	
	if (Sys.getenv("JGR_NO_OPTIONS") == "") 
		jgr.set.options()
	
	# set RHome Path in JGR
	invisible(.jcall("org/rosuda/JGR/JGR", "V", "setRHome", as.character(R.home())))
	
	invisible(.jcall("org/rosuda/JGR/JGR", "V", "setRLibs", as.character(.libPaths())))
	
	invisible(.jcall("org/rosuda/JGR/JGR", "V", "setKeyWords", 
		as.character(.refreshKeyWords())))
	
	invisible(.jcall("org/rosuda/JGR/JGR", "V", "setObjects", 
		as.character(.refreshObjects())))
	
	# set repos
	if (options("repos") == "@CRAN@") 
		options(repos = "http://cran.r-project.org")
}

.onAttach <- function(libname, pkgname) {
	if (!isTRUE(.jgr.works))
		packageStartupMessage("\nPlease type JGR() to download/launch console. Launchers can also be obtained at http://www.rforge.net/JGR/files/.\n\n")

	rv <- as.numeric(paste(R.version$major, as.integer(R.version$minor), sep = "."))
	if (rv == 2.13 && broken.gomp()) 
		packageStartupMessage("\n\n *** WARNING *** Your R contains old GOMP library which does NOT work with other threads!\nThis will lead to random crashes in R!\nPlease update R to the latest patched version from http://R.research.att.com/\n\n")
}

package.manager <- function() {
	if (!.jgr.works) {
		cat("package.manager() cannot be used outside JGR.\n")
		return(invisible(NULL))
	}
	f <- .jcall("org/rosuda/JGR/JGRPackageManager", , "showInstance")
}

installPackages <- function(contriburl = NULL, type = getOption("pkgType")) {
	if (!.jgr.works) {
		cat("installPackages() cannot be used outside JGR.\n")
		return(invisible(NULL))
	}
	if (type == "mac.binary") {
		if ((R.version$major >= 2 && R.version$minor >= 2) || 
			(R.version$major >= 3)) 
			a <- available.packages(contriburl = contrib.url(getOption("repos"), 
				type = "mac.binary"))
		else if (R.version$major >= 2 && R.version$minor >= 1) 
			a <- available.packages(contriburl = contrib.url(getOption("repos"), 
				type = "mac.binary"))
		else a <- available.packages(contriburl = contrib.url(getOption("CRAN"), 
			type = "mac.binary"))
	}
	else if (!is.null(contriburl)) 
		if ((R.version$major >= 2 && R.version$minor >= 2) || 
			(R.version$major >= 3)) 
			a <- available.packages(contriburl = contriburl)
		else a <- available.packages(contriburl = contriburl)
	else if ((R.version$major >= 2 && R.version$minor >= 2) || 
		(R.version$major >= 3)) 
		a <- available.packages()
	else a <- available.packages()
	pkgs <- a[, 1]
	if (length(pkgs) > 0) {
		invisible(.jcall("org/rosuda/JGR/JGRPackageInstaller", 
			, "instAndDisplay", pkgs, type))
	}
}

object.browser <- function() {
	if (!.jgr.works) {
		cat("object.browser() cannot be used outside JGR.\n")
		return(invisible(NULL))
	}
	f <- .jcall("org/rosuda/JGR/JGRObjectManager", , "showInstance")
}

jgr.pager <- function(file, header, title, delete.file) {
	if (!.jgr.works) {
		cat("jgr.pager() cannot be used outside JGR.\n")
		return(invisible(NULL))
	}
	invisible(.jcall("org/rosuda/JGR/toolkit/TextPager", , "launchPager", 
		as.character(file), as.character(header), as.character(title), 
		as.logical(delete.file)))
}

jgr.browser <- function(url, ...) {
	if (!.jgr.works) {
		cat("jgr.browser() cannot be used outside JGR.\n")
		return(invisible(NULL))
	}
	invisible(.jcall("org/rosuda/JGR/JGRHelp", , "showURL", as.character(url)[1]))
}

jgr.set.options <- function(..., useJavaGD = TRUE, 
	useJGRpager = TRUE, useJGRbrowser = TRUE, useHTMLHelp = TRUE) {
	if (!.jgr.works) {
		cat("jgr.set.options() cannot be used outside JGR.\n")
		return(invisible(NULL))
	}
	if (useJavaGD) {
		require(JavaGD)
		options(device = "JavaGD")
	}
	if (useJGRpager) {
		options(pager = jgr.pager)
	}
	if (useJGRbrowser) {
		options(browser = jgr.browser)
	}
	if (useHTMLHelp) {
		options(help_type = "html")
		tools:::startDynamicHelp()
	}
}

# add new menus at runtime to JGR Console

jgr.addMenu <- function(name) {
	if (!.jgr.works) {
		cat("jgr.addMenu() cannot be used outside JGR.\n")
		return(invisible(NULL))
	}
	invisible(.jcall("org/rosuda/JGR/JGR", "V", "addMenu", as.character(name)))
}

jgr.insertMenu <- function(name, index) {
	if (!.jgr.works) {
		cat("jgr.insertMenu() cannot be used outside JGR.\n")
		return(invisible(NULL))
	}
	invisible(.jcall("org/rosuda/JGR/JGR", "V", "insertMenu", 
		as.character(name), as.integer(index - 1)))
}

jgr.addMenuItem <- function(menu, name, command, silent = TRUE) {
	if (!.jgr.works) {
		cat("jgr.addMenuItem() cannot be used outside JGR.\n")
		return(invisible(NULL))
	}
	if (is.function(command)) 
		command <- .jgr.register.function(command)
	invisible(.jcall("org/rosuda/JGR/JGR", "V", "addMenuItem", 
		as.character(menu), as.character(name), as.character(command), 
		as.logical(silent)))
}

jgr.insertMenuItem <- function(menu, name, command, 
	index, silent = TRUE) {
	if (!.jgr.works) {
		cat("jgr.insertMenuItem() cannot be used outside JGR.\n")
		return(invisible(NULL))
	}
	if (is.function(command)) 
		command <- .jgr.register.function(command)
	invisible(.jcall("org/rosuda/JGR/JGR", "V", "insertMenuItem", 
		as.character(menu), as.character(name), as.character(command), 
		as.logical(silent), as.integer(index - 1)))
}

jgr.addSubMenu <- function(menu, subMenuName, labels, 
	commands) {
	if (!.jgr.works) {
		cat("jgr.addSubMenu() cannot be used outside JGR.\n")
		return(invisible(NULL))
	}
	
	invisible(J("org/rosuda/JGR/JGR")$addSubMenu(menu, subMenuName, 
		labels, commands))
}

jgr.insertSubMenu <- function(menu, subMenuName, labels, 
	commands, index) {
	if (!.jgr.works) {
		cat("jgr.addSubMenu() cannot be used outside JGR.\n")
		return(invisible(NULL))
	}
	
	invisible(J("org/rosuda/JGR/JGR")$addSubMenu(menu, subMenuName, 
		as.integer(index - 1), labels, commands))
}

jgr.addMenuSeparator <- function(menu) {
	if (!.jgr.works) {
		cat("jgr.addMenuSeparator() cannot be used outside JGR.\n")
		return(invisible(NULL))
	}
	invisible(.jcall("org/rosuda/JGR/JGR", "V", "addMenuSeparator", 
		as.character(menu)))
}

jgr.insertMenuSeparator <- function(menu, index) {
	if (!.jgr.works) {
		cat("jgr.insertMenuSeparator() cannot be used outside JGR.\n")
		return(invisible(NULL))
	}
	invisible(.jcall("org/rosuda/JGR/JGR", "V", "insertMenuSeparator", 
		as.character(menu), as.integer(index - 1)))
}

jgr.getMenuNames <- function() {
	if (!.jgr.works) {
		cat("jgr.getMenuNames() cannot be used outside JGR.\n")
		return(invisible(NULL))
	}
	J("org/rosuda/JGR/JGR")$getMenuNames()
}

jgr.getMenuItemNames <- function(menu) {
	if (!.jgr.works) {
		cat("jgr.getMenuItemNames() cannot be used outside JGR.\n")
		return(invisible(NULL))
	}
	J("org/rosuda/JGR/JGR")$getMenuItemNames(as.character(menu))
}

jgr.removeMenu <- function(index) {
	if (!.jgr.works) {
		cat("jgr.removeMenu() cannot be used outside JGR.\n")
		return(invisible(NULL))
	}
	J("org/rosuda/JGR/JGR")$removeMenu(as.integer(index - 1))
}

jgr.removeMenuItem <- function(menu, index) {
	if (!.jgr.works) {
		cat("jgr.removeMenuItem() cannot be used outside JGR.\n")
		return(invisible(NULL))
	}
	J("org/rosuda/JGR/JGR")$removeMenuItem(as.character(menu), 
		as.integer(index - 1))
}

.jgr.register.function <- function(fun) {
	if (is.null(.GlobalEnv$.jgr.user.functions)) 
		.GlobalEnv$.jgr.user.functions <- list()
	fnc <- length(.GlobalEnv$.jgr.user.functions) + 1
	.GlobalEnv$.jgr.user.functions[[fnc]] <- fun
	paste(".jgr.user.functions[[", fnc, "]]()", sep = "")
}



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
			links <- paste("<a href=\"http://127.0.0.1:", tools:::httpdPort, 
				"/library/", result[i, 3], "/help/", result[i, 
				1], "\">", result[i, 1], "</a>", sep = "")
			out <- c(out, paste("<tr align=\"left\" valign=\"top\">\n", 
				"<td>", links, "</td><td>", result[i, 3], "</td><td>", 
				result[i, 2], "</td></tr>\n", sep = ""))
		}
		out <- c(out, "</table>\n</p>\n<hr>\n</body></html>")
		out
		writeLines(out, file.path(path, paste(x$pattern, ".html", 
			sep = "")))
		browseURL(paste("http://127.0.0.1:", tools:::httpdPort, 
			"/doc/html/", x$pattern, ".html", sep = ""))
	}
}

.completeCommand <- function(x) {
	result <- c()
	if (regexpr("\\$$", x) > -1) {
		r <- names(get(sub("\\$", "", x)))
		for (i in 1:length(r)) {
			result <- c(result, r[i])
		}
	}
	else if (regexpr("\\$", x) > -1) {
		r <- names(get(substr(x, 0, gregexpr("\\$", x)[[1]][1] - 
			1)))
		for (i in 1:length(r)) {
			if (regexpr(strsplit(x, "\\$")[[1]][2], r[i]) > -1) 
				result <- c(result, r[i])
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
	if (!.jgr.works) {
		cat(".refresh() cannot be used outside JGR.\n")
		return(invisible(NULL))
	}
	invisible(.jcall("org/rosuda/JGR/JGR", "V", "setRLibs", as.character(.libPaths())))
	invisible(.jcall("org/rosuda/JGR/JGR", "V", "setKeyWords", 
		as.character(.refreshKeyWords())))
	invisible(.jcall("org/rosuda/JGR/JGR", "V", "setObjects", 
		as.character(.refreshObjects())))
}

# refresh KeyWords (used by SyntaxHighlighting)

.refreshKeyWords <- function() {
	n <- length(search())
	result <- c()
	for (i in 2:n) {
		result <- c(result, ls(pos = i, all.names = TRUE))
	}
	result
}

# refresh Objects (used by SyntaxHighlighting and ObjectManager)

.refreshObjects <- function() {
	# currently only use the objects we find in pos=1
	result <- c(ls(pos = 1, all.names = TRUE))
	result
}

.getModels <- function() {
	objects <- ls(pos = 1)
	result <- c()
	if (length(objects) > 0) 
		for (i in 1:length(objects)) {
			model <- get(objects[i])
			cls <- class(model)
			if ("lm" %in% cls || "glm" %in% cls) 
				result <- c(result, c(objects[i], cls[1]))
		}
	result
}

.getFunctionsInWS <- function() {
	objects <- ls(pos = 1)
	result <- c()
	if (length(objects) > 0) 
		for (i in 1:length(objects)) {
			cls <- class(get(objects[i]))
			if ("function" %in% cls) 
				result <- c(result, objects[i])
		}
	result
}

.getDataObjects <- function() {
	objects <- ls(pos = 1)
	result <- c()
	if (length(objects) > 0) 
		for (i in 1:length(objects)) {
			d <- get(objects[i])
			cls <- class(d)
			if ("data.frame" %in% cls || "table" %in% cls) 
				result <- c(result, objects[i], cls[1])
		}
	result
}

.getOtherObjects <- function() {
	objects <- ls(pos = 1)
	result <- c()
	if (length(objects) > 0) 
		for (i in 1:length(objects)) {
			if (objects[i] != "last.warning" && objects[i] != 
				"*tmp*") {
				cls <- class(get(objects[i]))
				if (!("data.frame" %in% cls || "table" %in% cls || 
				"function" %in% cls)) 
				result <- c(result, objects[i], cls[1])
			}
		}
	result
}

.getContent <- function(o, p = NULL) {
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
			if (is.null(xname)) 
				xname <- "null"
			try(result <- c(result, xname, class((o[, i]))[1]), 
				silent = TRUE)
		}
		
	}
	else {
		if (mode(o) == "list") {
			for (i in 1:length(o)) {
				xname <- names(o)[i]
				if (is.null(xname)) 
				xname <- "null"
				try(result <- c(result, xname, class((o[[i]]))[1]), 
				silent = TRUE)
			}
		}
	}
	result
}

# copy the content of the specified JavaGD device to another device
.jgr.save.JavaGD.as <- function(useDevice, source, 
	file = NULL, usefile = TRUE, ...) {
	if (usefile && is.null(file)) {
		file <- file.choose(TRUE)
		if (is.null(file)) 
			return(FALSE)
	}
	if (usefile) 
		JavaGD:::.javaGD.copy.device(source, useDevice, file = file, 
			...)
	else JavaGD:::.javaGD.copy.device(source, useDevice, ...)
	invisible(NULL)
}

.generate.run.script <- function(target = NULL) {
	jri.jar <- system.file("jri", "JRI.jar", package = "rJava")
	if (nchar(jri.jar) == 0) 
		stop("JRI is required but missing! Make sure R was configured with --enable-R-shlib and rJava was compiled with JRI support.")
	run.template <- paste(.jgr.pkg.path, "scripts", "run.in", 
		sep = .Platform$file.sep)
	rt <- readLines(run.template)
	settings <- c("R_SHARE_DIR", "R_INCLUDE_DIR", "R_DOC_DIR", 
		"R_LIBS", "R_HOME", "JAVA_HOME", "JAVA_LD_PATH", "JAVA_PROG", 
		"RJAVA")
	sl <- list()
	for (i in settings) sl[[i]] <- Sys.getenv(i)
	if (nchar(sl[["JAVA_PROG"]]) == 0) {
		if (nchar(sl[["JAVA_HOME"]]) > 0) {
			jc <- paste(sl[["JAVA_HOME"]], "bin", "java", sep = .Platform$file.sep)
			if (file.exists(jc)) 
				sl[["JAVA_PROG"]] <- jc
		}
		else sl[["JAVA_PROG"]] <- "java"
	}
	if (nchar(sl[["JAVA_LD_PATH"]]) == 0) {
		sl[["JAVA_LD_PATH"]] <- Sys.getenv("R_JAVA_LD_LIBRARY_PATH")
		if (nchar(sl[["JAVA_LD_PATH"]]) == 0) {
			sl[["JAVA_LD_PATH"]] <- Sys.getenv("LD_LIBRARY_PATH")
		}
	}
	sl[["JAVA_LD_PATH"]] <- paste(sl[["JAVA_LD_PATH"]], system.file("jri", 
		package = "rJava"), sep = .Platform$path.sep)
	
	sl[["JGR_JAR"]] <- system.file("java", "JGR.jar", package = "JGR")
	sl[["JRI_JAR"]] <- system.file("jri", "JRI.jar", package = "rJava")
	sl[["IPLOTS_JAR"]] <- system.file("java", "iplots.jar", package = "iplots")
	sl[["RJAVA"]] <- system.file(package = "rJava")
	## do all the substitutions
	for (i in names(sl)) rt <- gsub(paste("@", i, "@", sep = ""), 
		sl[[i]], rt)
	
	if (length(grep("darwin", R.version$os))) {
		rt[length(rt)] <- paste("\"${JAVA}\" -Dapple.laf.useScreenMenuBar=true -Dcom.apple.mrj.application.apple.menu.about.name=JGR", 
			substring(rt[length(rt)], 10))
		
	}
	
	## return back the entire file if there is no target
	if (is.null(target)) 
		return(rt)
	
	## otherwise save into resulting file
	writeLines(rt, target)
}


JGR <- function(update = FALSE) {
	if (!update && .jgr.works && .jcall("org/rosuda/JGR/JGR", 
		"Z", "isJGRmain")) {
		cat("JGR is already running. If you want to re-install or update JGR, use JGR(update=TRUE).\n")
		return(invisible(FALSE))
	}
	
	if (update) {
		# Win & OS X lanchers insist on site library
		lt <- paste(R.home(), "library", sep = .Platform$file.sep)
		if (.Platform$OS.type == "unix" && .Platform$pkgType != 
			"mac.binary") 
			lt <- .libPaths()[1]
		cran <- getOption("repos")
		if (cran == "@CRAN@") 
			cran <- "http://cran.r-project.org/"
		return(install.packages(c("JGR", "rJava", "JavaGD", "iplots"), 
			lt, c(cran, "http://www.rforge.net/")))
	}
	
	if (.Platform$OS.type == "windows") {
		.generate.windows.script()
		cat("Starting JGR ...\n\n")
		system("open jgrLaunch.bat")
		return(invisible(TRUE))
	}
	
	if (length(grep("darwin", R.version$os)) > 0) {
		
		cat("Starting JGR ...\n\n")
		.generate.mac.script()
		system("open -a Terminal.app jgrLaunch")
		system("open .")
		return(invisible(TRUE))
	}
	
	runs <- paste(.jgr.pkg.path, "scripts", "run", sep = .Platform$file.sep)
	if (file.exists(runs)) {
		cat("Starting JGR ...\n(You can use", runs, "to start JGR directly)\n")
		system(paste("sh ", runs, "&"))
	}
	else {
		rs <- .generate.run.script()
		wl <- try(writeLines(rs, runs), silent = TRUE)
		if (inherits(wl, "try-error")) {
			cat("Please consider running JGR() as root to create a start script in", 
				runs, "automatically.\n")
			fn <- tempfile("jgrs")
			wl <- try(writeLines(rs, fn), silent = TRUE)
			if (inherits(wl, "try-error")) 
				stop("Cannot create JGR start script. Please run JGR() as root to create a start script ", 
				runs)
			system(paste("chmod a+x '", fn, "'", sep = ""))
			cat("Starting JGR ...\n")
			system(paste("sh ", fn, "&"))
			system("sh -c 'sleep 3'")
			unlink(fn)
		}
		else {
			cat("Starting JGR run script. This can be done from the shell as well, just run\n", 
				runs, "\n\n")
			system(paste("chmod a+x '", runs, "'", sep = ""))
			system(paste("sh ", runs, "&"))
		}
	}
}

.generate.mac.script <- function(launcher_loc = NULL, 
	bit64 = NULL, outfile = "jgrLaunch") {
	rhome <- R.home()
	libs <- paste(.libPaths(), sep = ":", collapse = ";")
	if (is.null(bit64)) 
		bit64 <- .Machine$sizeof.pointer == 8L
	if (is.null(launcher_loc)) {
		root <- system.file(package = "JGR")
		res <- TRUE
		if (bit64) {
			if (!file.exists("JGR-SL.app")) {
				res <- try(download.file("http://www.rforge.net/JGR/web-files/JGR-1.6-SL.dmg", 
				"JGR-1.6-SL.dmg", method = "internal", mode = "wb"))
				if ("try-error" %in% class(res)) 
				cat("\n Could not download launcher. Either you are\nnot connected to the internet, or you do not have permissins to the\nfolder:", 
					getwd())
				system("hdiutil mount JGR-1.6-SL.dmg")
				system("cp -r /Volumes/JGR-1.6-SL/JGR.app JGR-SL.app")
			}
			launcher_loc <- "JGR-SL.app"
		}
		else {
			if (!file.exists("JGR.app")) {
				res <- try(download.file("http://www.rforge.net/JGR/web-files/JGR.dmg", 
				"JGR.dmg", method = "internal", mode = "wb"))
				if ("try-error" %in% class(res)) 
				cat("\n Could not download launcher. Either you are\nnot connected to the internet, or you do not have permissins to the\nfolder:", 
					getwd())
				system("hdiutil mount JGR.dmg")
				system("cp -r /Volumes/JGR/JGR.app JGR.app")
			}
			launcher_loc <- "JGR.app"
		}
	}
	lib_path <- NULL
	for (library_path in .libPaths()) {
		if ("JGR" %in% .packages(lib.loc = library_path, all.available = TRUE)) {
			lib_path <- library_path
			break
		}
	}
	if (is.null(lib_path)) 
		cat("Could not find JGR in library directories")
	cmd <- paste("#!/bin/csh\n\nsetenv R_HOME ", rhome, "\n", 
		"setenv R_LIBS ", lib_path, "\n", "setenv R_LIBS_USER ", 
		libs, "\n", sep = "")
	cmd <- paste(cmd, "\n./", launcher_loc, "/Contents/MacOS/JGR\n\n", 
		sep = "")
	cat("\n\nCopy the following is a launch script for JGR\n\n")
	cat(cmd, "\n")
	cat("\n\n\n")
	cat(cmd, file = outfile)
	system(paste("chmod 755 ", outfile))
	invisible(cmd)
}

.generate.windows.script <- function(launcher_loc = NULL, 
	bit64 = NULL, outfile = "jgrLaunch") {
	win <- Sys.info()[1] == "Windows"
	rhome <- R.home()
	libs <- paste(.libPaths(), sep = ";", collapse = ";")
	
	libs <- gsub("/", "\\\\", libs)
	rhome <- gsub("/", "\\\\", rhome)
	outfile <- paste(outfile, ".bat", sep = "")
	
	
	if (is.null(bit64)) 
		bit64 <- .Machine$sizeof.pointer == 8L
	
	
	if (is.null(launcher_loc)) {
		root <- system.file(package = "JGR")
		res <- TRUE
		if (bit64) {
			if (!file.exists("jgr-1_62-x64.exe")) 
				res <- try(download.file("http://www.rforge.net/JGR/web-files/jgr-1_62-x64.exe", 
				"jgr-1_62-x64.exe", method = "internal", mode = "wb"))
			if ("try-error" %in% class(res)) 
				cat("\n Could not download launcher. Either you are not connected to the internet, or you do not have permissins to the folder:", 
				getwd())
			launcher_loc <- "jgr-1_62-x64.exe"
		}
		else {
			if (!file.exists("jgr-1_62.exe")) 
				res <- try(download.file("http://www.rforge.net/JGR/web-files/jgr-1_62.exe", 
				"jgr-1_62.exe", method = "internal", mode = "wb"))
			if ("try-error" %in% class(res)) 
				cat("\n Could not download launcher. Either you are not connected to the internet, or you do not have permissins to the folder:", 
				getwd())
			launcher_loc <- "jgr-1_62.exe"
		}
		launcher_loc <- gsub("/", "\\\\", launcher_loc)
		
	}
	lib_path <- NULL
	for (library_path in .libPaths()) {
		if ("JGR" %in% .packages(lib.loc = library_path, all.available = TRUE)) {
			lib_path <- library_path
			break
		}
	}
	if (is.null(lib_path)) 
		cat("Could not find JGR in library directories")
	lib_path <- gsub("/", "\\\\", lib_path)
	
	cmd <- paste("set R_HOME=", rhome, "\n", "set R_LIBS=", libs, 
		"\n", sep = "")
	
	cmd <- paste("set R_HOME=", rhome, "\n", "set R_LIBS=", lib_path, 
		"\n", "set R_LIBS_USER=", libs, "\n", sep = "")
	cmd <- paste(cmd, launcher_loc, " --rhome=", rhome, " --libpath=", 
		lib_path, "\n", sep = "")
	cat("\n\nCopy the following into WordPad and save as \"jgrLaunch.bat\"\n\n")
	cat(cmd, "\n")
	cat("\n\n\n")
	cat(cmd, file = outfile)
	invisible(cmd)
}

reformat.code <- function(txt) {
	lns <- strsplit(txt, "\n")[[1]]
	for (i in 1:length(lns)) {
		isBlank <- grepl("^\\s*$", lns[i])
		if (isBlank) {
			lns[i] <- "\n"
			next
		}
		strt <- regexpr("#.*", lns[i])
		if (strt < 0) 
			next
		if (grepl("\"|'.*#.*\"|'", lns[i])) 
			next
		#print(cmt)
		cmt <- substr(lns[i], strt, nchar(lns[i]))
		lns[i] <- sub(cmt, paste("\n.__comment__(\"", cmt, "\")\n", 
			sep = ""), lns[i], fixed = TRUE)
	}
	
	mod_text <- paste(lns, collapse = "\n")
	tidy.block <- function(block.text) {
		#from formatR
		exprs <- base::parse(text = block.text)
		n <- length(exprs)
		res <- character(n)
		for (i in 1:n) {
			dep <- paste(base::deparse(exprs[i]), collapse = "\n")
			res[i] <- substring(dep, 12, nchar(dep) - 1)
		}
		return(res)
	}
	
	tidied <- tidy.block(mod_text)
	tidied <- do.call(c, strsplit(tidied, "\n"))
	for (i in 1:length(tidied)) {
		l <- tidied[i]
		if (grepl("", l, fixed = TRUE)) {
			tidied[i] <- sub("", "", l, fixed = TRUE)
			next
		}
		if (!grepl(".__comment__", l, fixed = TRUE)) 
			next
		l <- sub(".__comment__(\"", "", l, fixed = TRUE)
		l <- sub("\"\\)$|\n", "", l)
		#print(l)
		tidied[i] <- l
	}
	leading <- floor(attr(regexpr(" *", tidied), "match.length")/4)
	for (i in 1:length(tidied)) tidied[i] <- gsub("^ *", paste(rep("\t", 
		leading[i]), "", sep = "", collapse = ""), tidied[i])
	return(paste(tidied, collapse = "\n"))
}


