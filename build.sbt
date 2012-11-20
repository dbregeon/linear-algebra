name := "Linear Algebra"

version := "1.0"

scalaVersion := "2.9.2"

/** Dependencies */
resolvers += "snapshots-repo" at "http://scala-tools.org/repo-snapshots"

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                    "releases"  at "http://oss.sonatype.org/content/repositories/releases")

libraryDependencies += "junit" % "junit" % "4.8" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"

libraryDependencies += "org.specs2" %% "specs2" % "1.12.1" % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

cucumberStepsBasePackage := "steps"

//testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-v", "10")

seq(cucumberSettings : _*)
