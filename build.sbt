val scala3Version = "3.6.2"

lazy val root = project
  .in(file("."))
  .settings(
    name                                    := "scala-course-spring",
    version                                 := "0.1.0-SNAPSHOT",
    scalaVersion                            := scala3Version,
    libraryDependencies += "org.scalacheck" %% "scalacheck"       % "1.18.1" % Test,
    libraryDependencies += "org.scalameta"  %% "munit"            % "1.0.0"  % Test,
    libraryDependencies += "org.scalameta"  %% "munit-scalacheck" % "1.0.0"  % Test,
    testFrameworks += new TestFramework("munit.Framework"),
    scalacOptions ++= Seq("-Xfatal-warnings", "-Yexplicit-nulls"),
  )
