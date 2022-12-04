ThisBuild / scalaVersion := "2.13.8"

lazy val hello = (project in file("."))
  .settings(
    name := "MaxCalories"
  )