
name := "scala-util-backoff"

organization in ThisBuild := "throwaway.util"

scalacOptions in ThisBuild := Seq("-deprecation", "-feature", "-unchecked", "-Yinline-warnings")

testOptions in Test += Tests.Argument("-oD")

crossScalaVersions in ThisBuild := Seq("2.10.6", "2.11.7")

libraryDependencies in ThisBuild ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.6" % "test" withSources()
, "org.scalacheck" %% "scalacheck" % "1.11.6" % "test" withSources()
, "org.apache.commons" % "commons-math3" % "3.6" % "test" withSources()
)

