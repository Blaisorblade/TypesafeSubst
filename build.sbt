lazy val exprSyb = uri("git://github.com/Blaisorblade/scrap-expr-boilerplate.git#master")
lazy val root = project in file(".") dependsOn exprSyb

scalaVersion := "2.10.3"

scalacOptions := Seq("-unchecked")

initialCommands := """
import Subst._
import Lang._
import Examples._
"""
