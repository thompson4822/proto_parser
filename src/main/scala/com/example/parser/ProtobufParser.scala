package com.example.parser

import scala.util.parsing.combinator.RegexParsers

/**
 * Created by steve on 3/1/15.
 */
object Main {
  /*
  option java_package = "com.example.tutorial";
option java_outer_classname = "AddressBookProtos";

   */
  val example =
    """
      |package tutorial;
      |option java_package = "com.example.tutorial";
      |option java_outer_classname = "AddressBookProtos";
      |
      |message Person {
      |    required string name = 1;
      |    required int32 id = 2;
      |    optional string email = 3;
      |
      |    enum PhoneType {
      |        MOBILE = 0;
      |        HOME = 1;
      |        WORK = 2; }
      |
      |    message PhoneNumber {
      |        required string number = 1;
      |        optional PhoneType type = 2 [default = HOME]; }
      |
      |    repeated PhoneNumber phone = 4; }
      |
      |message AddressBook {
      |    repeated Person person = 1; }
    """.stripMargin

  def main(args: Array[String]) {
    val parser = new ProtobufParser
    val result = parser.parseAll(parser.protobuf, example)
    if (result.successful) println(result.get)
  }
}

class ProtobufParser extends RegexParsers {
  lazy val intValue = """(0|[1-9][0-9]*)""".r
  val ident = """([a-zA-Z][a-zA-Z0-9_]*)""".r
  lazy val _package = """package ([a-zA-Z][a-zA-Z0-9_]*);""".r
  lazy val javaPackage = """([a-zA-Z][a-zA-Z0-9_]*(\.[a-zA-Z][a-zA-Z0-9_]*)*)""".r
  lazy val javaOuterClassName = """option java_outer_classname\s*=\s*"([a-zA-Z][a-zA-Z0-9_]*)";""".r


  lazy val singleLineComment = """//.*""".r
  lazy val multiLineComment = """/\*[^*]*\*+(?:[^*/][^*]*\*+)*/""".r
  lazy val defaultValue = """\[default\s*=\s*([^\]]*)\]""".r

  //case class Message(name: String, body: Seq[Statement]) extends Statement

  lazy val protobuf: Parser[ProtobufFile] =
    opt(packageDecl) ~ opt(javaPackageDecl) ~ opt(javaOuterClass) ~ rep(statement) ^^ {
      case pckg ~ jPckge ~ outerClassName ~ statements => ProtobufFile(pckg, jPckge, outerClassName, statements)
    }

  lazy val packageDecl: Parser[String] = _package ^^ { case _package(name) => name }

  lazy val javaPackageDecl: Parser[String] = "option java_package = \"" ~> javaPackage <~ "\";"

  lazy val javaOuterClass: Parser[String] = javaOuterClassName ^^ { case javaOuterClassName(name) => name}

  lazy val message: Parser[Message] =
    ("message" ~> ident) ~ messageBody ^^ { case name ~ body => Message(name = name, body = body)}

  lazy val messageBody: Parser[Seq[Statement]] =
    "{" ~> rep(statement) <~ "}"

  lazy val statement: Parser[Statement] =
    field | enumeration | message

  lazy val enumeration: Parser[Enum] =
    ("enum" ~> ident) ~ ("{" ~> enumerants <~ "}")  ^^ { case name ~ members => Enum(name, members)}

  lazy val enumerants: Parser[Seq[(String, Int)]] =
    rep(enumerant <~ ";") ^^ { case result => result}

  lazy val enumerant: Parser[(String, Int)] =
    ident ~ ("=" ~> intValue) ^^ { case name ~ intValue(value) => (name, value.toInt)}

  lazy val comment = singleLineComment | multiLineComment

  lazy val field =
    modifierType ~ scalarType ~ ident ~ ("=" ~> intValue) ~ (opt(defaultValue) <~ ';') ^^ { case modifier ~ scalar ~ name ~ position ~ default =>
      Field(name = name, scalarType = scalar, modifierType = modifier, order = position.toInt, default = default)
    }

  lazy val modifierType: Parser[ModifierType.Value] =
    ("required" | "optional" | "repeated") ^^ ModifierType.withName

  lazy val scalarType: Parser[String] = ident
/*
  // This is how we might have done it ... had we not had to take user defined types into account!

  lazy val scalarType: Parser[ScalarType.Value] =
    ("float" | "double" | "int32" | "int64" | "uint32" | "uint64" | "sint32"
      | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64" |"bool"
      | "string" | "bytes") ^^ { case scalar => ScalarType.withName(scalar) }
*/


  //case class Field(name: String, scalarType: ScalarType.Value, modifierType: ModifierType.Value, order: Int, default: String) extends Statement

//
}

object ModifierType extends Enumeration {
  val Required = Value(0, "required")
  val Optional = Value(1, "optional")
  val Repeated = Value(2, "repeated")
}

trait Statement { def name: String }

case class ProtobufFile(pckg: Option[String], javaPackage: Option[String], outerClassName: Option[String], body: Seq[Statement])

case class Enum(name: String, enumerants: Seq[(String, Int)]) extends Statement

case class Field(name: String, scalarType: String, modifierType: ModifierType.Value, order: Int, default: Option[String]) extends Statement

case class Message(name: String, body: Seq[Statement]) extends Statement
