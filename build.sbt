val scalaV = "3.3.1"
val pekkoV = "1.0.1"
val pekkoHttpV = "1.0.0"
val scalaTestV = "3.2.16"

inThisBuild(Def.settings(
  scalaVersion := scalaV,
))

val root = project.in(file("."))
  .aggregate(common, web, worker)

lazy val common = project.in(file("common"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "org.apache.pekko" %% "pekko-stream" % pekkoV,
      "org.apache.pekko" %% "pekko-http" % pekkoHttpV,
      "org.apache.pekko" %% "pekko-http-spray-json" % pekkoHttpV,
      "org.apache.pekko" %% "pekko-http-caching" % pekkoHttpV,
      "io.spray" %% "spray-json" % "1.3.6",

      "org.scalatest" %% "scalatest" % scalaTestV % "test"

    ),
    buildInfoPackage := "net.virtualvoid.vesuvius",
    buildInfoOptions += BuildInfoOption.BuildTime,
    buildInfoKeys ++= Seq(

    )
  )

lazy val worker = project.in(file("worker"))
  .dependsOn(common)
  .settings(
    libraryDependencies ++= Seq(
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

lazy val web = project.in(file("web"))
  .dependsOn(common)
  .enablePlugins(SbtTwirl)
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "twirl-api" % "1.6.0-RC4",
    ),
    Compile / resourceGenerators += (worker / assembly).map(_ :: Nil),

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
