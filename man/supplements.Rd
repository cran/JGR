\name{jgr.supplements}
\alias{JGR}
\alias{installPackages}
\alias{jgr.pager}
\alias{jgr.set.options}
\alias{object.browser}
\alias{package.manager}
\title{
  Supplemental JGR functions
}
\description{
  The following functions are publicly visible, but with the expection
  of \code{JGR} they are usually invoked from the GUI and rarely
  directly by the user.

  \code{JGR} can be used to update JGR including dependent packages or
  to start JGR from within R. JGR is always started as an independent
  application and the preferred way to start JGR is using launchers that
  are provided from the JGR website \url{http://www.rosuda.org}
}
\usage{
JGR(update = FALSE)
installPackages(contriburl = NULL, type = "binaries")
jgr.pager(file, header, title, delete.file)
jgr.set.options(..., useJavaGD = TRUE, useJGRpager = TRUE)
object.browser()
package.manager()
}
\arguments{
  \item{update}{If set to \code{TRUE} then JGR is updated for the RoSuDa
    repository (internet connection is required).}
  \item{contriburl}{same as in \code{\link{install.packages}}}
  \item{type}{package type, must be either \code{"sources"} or
    \code{"binaries"}}
  \item{file}{file to show}
  \item{header}{header for each file}
  \item{title}{overall title for the display}
  \item{delete.file}{should the files be deleted after display?  Used
    for temporary files.}
  \item{useJavaGD}{if set to \code{TRUE}, \code{JavaGD} is set as the
    default device}
  \item{useJGRpager}{if set to \code{TRUE}, \code{jgr.pager} function is
    used as the default pager}
  \item{...}{additional options to set}
}
\keyword{programming}
