ThisBuild / version := "2022.18.07-1"

ThisBuild / scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "Advent of Code 2021",
  
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.7.0",
      "org.typelevel" %% "cats-effect" % "3.3.12",
    )
  )
