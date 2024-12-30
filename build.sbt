import sbtdynver.GitDescribeOutput

val scalaV = "3.4.3"
val pekkoV = "1.1.2"
val pekkoHttpV = "1.1.0"
val tapirV = "1.11.7"
val scalaTestV = "3.2.19"

inThisBuild(Def.settings(
  scalaVersion := scalaV,
  scalacOptions ++= Seq(
    "-Wunused:all",
    "-deprecation",
    "-no-indent",
    "-Xmax-inlines", "100"
  ),
  run / fork := true
))

val root = project.in(file("."))
  .aggregate(common, web, worker, tiles)

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
    buildInfoKeys ++= Seq[BuildInfoKey](
      BuildInfoKey.map(dynverGitDescribeOutput) {
        case (_, Some(GitDescribeOutput(ref, suffix, dirty))) =>
          "gitCommit" -> suffix.sha
        case (_, None) =>
          "gitCommit" -> "unknown"
      },
      "buildVersion" -> sys.env.getOrElse("BUILD_VERSION", "unknown")
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

lazy val tiles = project.in(file("tiles"))
  .dependsOn(common)
  .settings(
    libraryDependencies ++= Seq(
      "net.java.dev.jna" % "jna" % "5.14.0"
    ),
    // setup docker build
    // use separate dependency and app jars
    assembly / assemblyOption := (assembly / assemblyOption).value.withIncludeScala(false).withIncludeDependency(false),
    assembly / assemblyJarName := "../app.jar", // contract with Dockerfile
    assemblyPackageDependency / assemblyJarName := "../deps.jar", // contract with Dockerfile
    assemblyMergeStrategy := {
      // merging pekko protobuf and protobuf from google ortools (not sure why they are different, but doesn't matter here)
      case PathList(ps@_*) if ps.last endsWith ".proto" => MergeStrategy.first
      case x => assemblyMergeStrategy.value(x)
    },
    run/javaOptions ++= Seq("--add-opens", "java.base/sun.nio.ch=ALL-UNNAMED")
  )

lazy val web = project.in(file("web"))
  .dependsOn(common)
  .enablePlugins(SbtTwirl)
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "twirl-api" % "1.6.0-RC4",
      "com.softwaremill.sttp.tapir" %% "tapir-core" % tapirV,
      "com.softwaremill.sttp.tapir" %% "tapir-pekko-http-server" % tapirV exclude("org.apache.pekko", "pekko-stream_3"),
      "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui-bundle" % tapirV,
      "com.softwaremill.sttp.tapir" %% "tapir-json-spray" % tapirV,
      "ch.qos.logback" % "logback-classic" % "1.3.14" % "runtime",
    ),
    Compile / resourceGenerators += (worker / assembly).map(_ :: Nil),

    // setup docker build
    // use separate dependency and app jars
    assembly / assemblyOption := (assembly / assemblyOption).value.withIncludeScala(false).withIncludeDependency(false),
    assembly / assemblyJarName := "../app.jar", // contract with Dockerfile
    assemblyPackageDependency / assemblyJarName := "../deps.jar", // contract with Dockerfile
    assemblyMergeStrategy := {
      // merging pekko protobuf and protobuf from google ortools (not sure why they are different, but doesn't matter here)
      case PathList(ps@_*) if ps.last endsWith ".proto" => MergeStrategy.first
      case PathList("META-INF", "maven", "org.webjars", "swagger-ui", "pom.properties") =>
        MergeStrategy.singleOrError
      case PathList("META-INF", "resources", "webjars", "swagger-ui", _*)               =>
        MergeStrategy.singleOrError
      case PathList("META-INF", "services", _*)               => // keep slf4j bits for logback
        MergeStrategy.singleOrError
      case PathList("META-INF", _*)                                                     => MergeStrategy.discard // Optional, but usually required
      case x => assemblyMergeStrategy.value(x)
    },
  )
