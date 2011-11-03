name := "Linear Algebra"

version := "1.0"

scalaVersion := "2.9.1"

/** Dependencies */
resolvers += "snapshots-repo" at "http://scala-tools.org/repo-snapshots"

libraryDependencies += "junit" % "junit" % "4.8" % "test"

libraryDependencies += "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test"

libraryDependencies += "org.specs2" % "specs2_2.9.1.RC4" % "1.6-SNAPSHOT"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

