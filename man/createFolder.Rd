% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miscUtils.R
\name{createFolder}
\alias{createFolder}
\title{Create a folder}
\usage{
createFolder(path, name)
}
\arguments{
\item{path}{path to directory where folder should be created}

\item{name}{name of folder to be created}
}
\value{
the path to the newly created folder
}
\description{
Creates a folder .name at specified .path
If .name already exists at .path, creates a
folder called .name_copy_x where x is an auto-
incremented integer. Uses forward slashes, yo.
}
\examples{
createFolder("~/", "newFolder")


}
