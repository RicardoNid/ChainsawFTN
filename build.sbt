organizationName := "SYSU"
name := "Chainsaw_Matches"
version := "1.0"
scalaVersion := "2.11.12"
val spinalVersion = "1.4.3"

libraryDependencies ++= Seq(
  "com.github.spinalhdl" % "spinalhdl-core_2.11" % spinalVersion,
  "com.github.spinalhdl" % "spinalhdl-lib_2.11" % spinalVersion,
  compilerPlugin("com.github.spinalhdl" % "spinalhdl-idsl-plugin_2.11" % spinalVersion)
)

libraryDependencies += "default" % "chainsaw_2.11" % "0.1"

fork := true
EclipseKeys.withSource := true
