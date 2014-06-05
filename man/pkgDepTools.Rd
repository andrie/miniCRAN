\name{split_op_version}
\alias{cleanPkgField}
\alias{split_dependencies}
\alias{split_op_version}
\title{Clean package fields.}
\usage{
split_op_version(x)

split_dependencies(x)

cleanPkgField(val)
}
\arguments{
  \item{val}{Value from a field like 'Depends' in a
  package's DESCRIPTION file}
}
\description{
Given the value from a field like 'Depends' in a package's
DESCRIPTION file, return a character vector of package
names with the version restrictions stripped and
\R~removed.
}
\keyword{internal}

