\name{jgr.addMenuItem}
\alias{jgr.addMenuItem}
\title{
  New JGR Console MenuItem
}
\description{
  adds a new MenuItem to specified JGR Console Menu
}
\usage{
jgr.addMenuItem(menu,name,command)
}
\arguments{
  \item{menu}{Name of the menu that this item will be added to}
  \item{name}{Name of the menu item to add}
  \item{command}{R expression(s) as a string to be parsed and evaluated or a function that will be called (without arguments) when the menu item is selected}
}
\value{
  MenuItem
}
\seealso{
  \code{\link{jgr.addMenu}}
  \code{\link{jgr.addMenuSeparator}}
}
\examples{
jgr.addMenu("Workspace")
jgr.addMenuItem("Workspace","Browse","ls()")
jgr.addMenuSeparator("Workspace")
jgr.addMenuItem("Workspace","List Functions",
  function() unlist(lapply(ls(envir=.GlobalEnv),
                           function(x) if (is.function(get(x))) x else NULL )))
}
\keyword{programming}
