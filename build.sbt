lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "it.twinsbrain",
      scalaVersion := "2.12.7"
    )),
    name := "studying_fpinscala"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
