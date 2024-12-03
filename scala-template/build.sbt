lazy val app = project.in(file("."))
  .settings(
    scalaVersion := "3.3.3",
    libraryDependencies += "org.scala-lang" %% "toolkit" % "0.1.7"
  )
