name := "scala_pure_random"

version := "0.1"

scalaVersion := "2.13.3"

crossScalaVersions := Seq("2.13.3", "2.12.12")

scalacOptions ++= Seq("-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-Ymacro-annotations", "-language:higherKinds")

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1"

libraryDependencies += "org.typelevel" %% "cats-macros" % "2.1.1"

libraryDependencies += "org.typelevel" %% "cats-kernel" % "2.1.1"

libraryDependencies += "org.typelevel" %% "cats-effect" % "2.1.3"

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"

libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze" % "1.0",
  "org.scalanlp" %% "breeze-natives" % "1.0",
  "org.scalanlp" %% "breeze-viz" % "1.0"
)


libraryDependencies += "org.typelevel" %% "simulacrum" % "1.0.0"

