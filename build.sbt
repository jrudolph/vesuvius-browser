val scalaV = "3.3.1"
val pekkoV = "1.0.1"
val pekkoHttpV = "1.0.0"
val scalaTestV = "3.2.16"

inThisBuild(Def.settings(
  scalaVersion := scalaV,
))

val root = project.in(file("."))
  .aggregate(web)

lazy val web = project.in(file("web"))
  .enablePlugins(SbtTwirl)
  .settings(
    libraryDependencies ++= Seq(
      "org.apache.pekko" %% "pekko-stream" % pekkoV,
      "org.apache.pekko" %% "pekko-http" % pekkoHttpV,
      "org.apache.pekko" %% "pekko-http-spray-json" % pekkoHttpV,
      "org.apache.pekko" %% "pekko-http-caching" % pekkoHttpV,
      "io.spray" %% "spray-json" % "1.3.6",
      "com.typesafe.play" %% "twirl-api" % "1.6.0-RC4",

      "org.scalatest" %% "scalatest" % scalaTestV % "test"
    ),

    // setup docker build
    // use separate dependency and app jars
    assembly / assemblyOption := (assembly / assemblyOption).value.withIncludeScala(false).withIncludeDependency(false),
    assembly / assemblyJarName := "app.jar", // contract with Dockerfile
    assemblyPackageDependency / assemblyJarName := "deps.jar", // contract with Dockerfile
    assemblyMergeStrategy := {
      // merging pekko protobuf and protobuf from google ortools (not sure why they are different, but doesn't matter here)
      case PathList(ps@_*) if ps.last endsWith ".proto" => MergeStrategy.first
      case x => assemblyMergeStrategy.value(x)
    },
  )
