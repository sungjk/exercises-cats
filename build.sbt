name := "exercises-cats"

version := "0.1"

scalaVersion := "2.13.0"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0-RC1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % Test

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")

libraryDependencies ++= (scalaBinaryVersion.value match {
    case "2.10" =>
        compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full) :: Nil
    case _ =>
        Nil
})
