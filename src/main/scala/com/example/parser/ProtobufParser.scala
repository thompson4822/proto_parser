package com.example.parser

import com.google.protobuf.WireFormat

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

  val example2 =
    """
      |namespace Common {
      |    enum CurrencyType {
      |        USD
      |    }
      |
      |    enum LegMode { unknown, drive, bike, walk, bus, rail, carshare, rideshare, bikeshare }
      |
      |    enum RelativeDirection { left, right }
      |
      |    enum CompassDirection { N, NE, NW, S, SE, SW, E, W }
      |
      |    struct LegTime {
      |        "waitBegin" : ?Date,
      |        "traversalBegin" : Date,
      |        "traversalEnd" : Date
      |    }
      |
      |    struct Coordinate {
      |        "type" : "Point",
      |        "coordinates" : Seq[Double]
      |    }
      |
      |    struct Cost {
      |        "currency" : CurrencyType,
      |        "average" : ?Decimal,
      |        "low" : ?Double,
      |        "high" : ?Double,
      |        "surgeMultiplier" : ?Double
      |    }
      |
      |    struct Bogus {
      |     "field" : {
      |       "inner1" : Int,
      |       "inner2" : ?String,
      |       "inner3" : {
      |         "innermost" : "fantasy"
      |       }
      |     }
      |    }
      |
      |    struct Context { }
      |
      |}
    """.stripMargin

  val example3 =
    """
      |namespace MySpace includes Common, Corona {
      |    enum CurrencyType { USD }
      |
      |    struct Cost {
      |        "currency" : CurrencyType,
      |        "average" : ?Decimal,
      |        "low" : ?Double,
      |        "high" : ?Double,
      |        "surgeMultiplier" : ?Double
      |    }
      |
      |}
    """.stripMargin

  def main(args: Array[String]) {
    val parser = new MagellanParser
    val result = parser.parseAll(parser.source, example2)
    if (result.successful) println(result.get) else println(result)
  }
}

sealed trait BodyElement

case class Enum(name: String, enumerants: Seq[String]) extends BodyElement

case class Struct(name: String, fields: Seq[Field]) extends BodyElement

case class Namespace(name: String, includes: Option[Seq[String]], definitions: Seq[BodyElement])

sealed trait FieldType

// TODO - add field types here...
case class CompoundFieldType(fields: Seq[Field]) extends FieldType

case class OptionalFieldType(typeName: String) extends FieldType

case class LiteralFieldType(typeValue: String) extends FieldType

case class StandardFieldType(typeName: String) extends FieldType

case class Field(name: String, fieldType: FieldType)

class MagellanParser extends RegexParsers {
  val ident = """([a-zA-Z][a-zA-Z0-9_]*)""".r
  val typeIdent = """([a-zA-Z][a-zA-Z0-9_\[\]]*)""".r
  val stringIdent = """\"([a-zA-Z][a-zA-Z0-9_]*)\"""".r
  lazy val source: Parser[Seq[Namespace]] = rep(namespace)

  lazy val namespace: Parser[Namespace] =
    ("namespace" ~> ident) ~ opt(includesClause) ~ ("{" ~> namespaceBody <~ "}") ^^ {
      case name ~ includes ~ definitions => Namespace(name, includes, definitions)
    }

  lazy val namespaceBody: Parser[Seq[BodyElement]] = rep(bodyElement)

  lazy val bodyElement: Parser[BodyElement] = enum | struct

  lazy val enum: Parser[Enum] = ("enum" ~> ident) ~ enumBody ^^ { case name ~ enumerants => Enum(name, enumerants)}
  
  lazy val enumBody: Parser[Seq[String]] = "{" ~> repsep(ident, ",") <~ "}"

  lazy val struct: Parser[Struct] = ("struct" ~> ident) ~ structBody ^^ { case name ~ fields => Struct(name, fields)}
  
  lazy val structBody: Parser[Seq[Field]] = "{" ~> repsep(field, ",") <~ "}"
  
  lazy val field: Parser[Field] = (stringIdent <~ ":") ~ fieldType ^^ { case name ~ typeParameter => Field(name, typeParameter) }

  lazy val fieldType: Parser[FieldType] =
    stringFieldType | optionalFieldType | standardFieldType | compoundFieldType

  lazy val stringFieldType: Parser[FieldType] = stringIdent ^^ { case string => LiteralFieldType(string) }

  lazy val optionalFieldType: Parser[FieldType] = "?" ~> ident ^^ { case typeParameter => OptionalFieldType(typeParameter)}

  lazy val standardFieldType: Parser[FieldType] = typeIdent ^^ { case typeParameter => StandardFieldType(typeParameter)}

  lazy val compoundFieldType: Parser[FieldType] = structBody ^^ { case typeParameter => CompoundFieldType(typeParameter)}

  lazy val includesClause: Parser[Seq[String]] = "includes" ~> repsep(ident, ",")
}

