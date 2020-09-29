name := "scala-bootcamp-homeworks"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
  "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test,
  "org.tpolecat" %% "doobie-scalatest" % "0.9.0" % Test
)