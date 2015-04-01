package com.example.parser

import scala.util.parsing.combinator.RegexParsers
import scala.collection.mutable.{Map => MutableMap, Seq => MutableSeq}
/**
 * Created by steve on 3/1/15.
 */
object Main {

  // TODO - the following doesn't take into account instances where the user includes a namespace that
  // doesn't exist. In that case, it is simply ignoreed.
  // The following maps a namespace identifier to all the namespaces that should be searched for a user type.
  def buildMappings(namespaces: Seq[Namespace]): Map[String, Seq[Namespace]] = {
    (for {
      namespace <- namespaces
      name = namespace.name
      includeNames = namespace.includes
      includedNamespaces = namespaces.filter(ns => includeNames.contains(ns.name))
    } yield name -> (namespace +: includedNamespaces)).toMap
  }

  var errorMessage: String = _

  // What structs and enums are available to the given namespace?
  def knownUserTypes(namespace: Namespace, mappings: Map[String, Seq[Namespace]]): Map[String, UserType] = {
    (for {
      ns <- mappings.get(namespace.name).get
      userType <- ns.definitions
    } yield userType.name -> userType).toMap
  }

  // What structs and enums are declared in the given namespace?
  def namespaceUserTypes(namespace: Namespace): Map[String, UserType] = {
    namespace.definitions.map(userType => userType.name -> userType).toMap
  }

  def typeCheck(namespaces: Seq[Namespace], mappings: Map[String, Seq[Namespace]]): Boolean = {
    // Useful function to pass to lower level components. This code knows about the mappings and how to set the error message.
    def checker(knownTypes: Map[String, UserType])(typeName: String): Boolean = {
      val result = knownTypes.keySet.contains(typeName) || primitiveTypes.contains(typeName)
      if(!result)
        errorMessage = s"Error: Not all fields appear to reference legitimate types. Cannot resolve '$typeName'"
      result
    }
    
    // What this segment does is look at each of the structs in a given namespace and ensure that all fields refer to either
    // primitive types or a knownUserType
    namespaces.forall { namespace =>
      val types = knownUserTypes(namespace, mappings)
      types.forall{ case (_, userType) => userType.fieldsValid(checker(types))}
    }
  }
  
  def generateUsing(namespaces: Seq[Namespace], mappings: Map[String, Seq[Namespace]]): Unit = {
    namespaces.foreach { namespace =>
      val types = namespaceUserTypes(namespace)
      types.foreach{ case(_, userType) =>
        userType.generateScalaModel(types)
        userType.generateProtobuf(types)
      }
    }
  }
  
  def main(args: Array[String]) {
    val parser = new MagellanParser
    val result = parser.parseAll(parser.source, example)
    if (result.successful) {
      val ast = result.get
      val mappings = buildMappings(ast)
      if(typeCheck(ast, mappings))
        generateUsing(ast, mappings)
      else
        println(errorMessage)
    } else {
      println(result)
    }
  }
}

sealed trait UserType {
  def name: String

  def fieldsValid(checker: String => Boolean): Boolean

  def generateScalaModel(types: Map[String, UserType])

  def generateScalaSerializer(types: Map[String, UserType])

  def generateScalaTestData(types: Map[String, UserType])

  def generateScalaTests(types: Map[String, UserType])

  def generateProtobuf(types: Map[String, UserType])
}

case class Enum(name: String, enumerants: Seq[String]) extends UserType {
  override def fieldsValid(checker: String => Boolean): Boolean = true

  override def generateScalaModel(types: Map[String, UserType]): Unit = {
    println(s"""
      |object $name extends Enumeration {
      |   type $name = $name.Value
      |   ${enumerants.zipWithIndex.map { case (name, index) => s"""val $name = Value($index, "$name")"""}.mkString("\n   ")}
      |}
    """.stripMargin)
  }

  override def generateProtobuf(types: Map[String, UserType]): Unit = {

  }

  override def generateScalaSerializer(types: Map[String, UserType]): Unit = ???

  override def generateScalaTestData(types: Map[String, UserType]): Unit = ???

  override def generateScalaTests(types: Map[String, UserType]): Unit = ???
}

case class Struct(name: String, parent: Option[String], fields: FieldCollection) extends UserType {
  override def fieldsValid(checker: String => Boolean): Boolean = fields.fields.forall(_.isValidType(checker))

  override def generateScalaModel(types: Map[String, UserType]): Unit = {
    // This guy represents field names to field type names. The first thing to do is to flatten out all of the fields so that
    // only
    val parameters = fields.actualFields
    val parameterString = parameters.map(field => s"${field.name}: ${field.fieldType.toScala}").mkString(", ")
    println(
      s"""
         |case class $name($parameterString)
         |
         |object $name {
         |  implicit object ${name}Converter extends Converter[$name] {
         |
         |    override def convertToProtobuf(a: $name): Array[Byte] = {
         |      val builder = ${name}Proto.newBuilder
         |      // protobuffy stuff to go here
         |      builder.build.toByteArray
         |    }
         |
         |    override def convertFromProtobuf(bytes: Array[Byte]): $name = {
         |      val proto = ${name}Proto.parseFrom(bytes)
         |      $name(
         |        // Protobuffy stuff goes here
         |    }
         |  }
         |}
       """.stripMargin
    )
  }

  override def generateProtobuf(types: Map[String, UserType]): Unit = {

  }

  override def generateScalaSerializer(types: Map[String, UserType]): Unit = ???

  override def generateScalaTestData(types: Map[String, UserType]): Unit = ???

  override def generateScalaTests(types: Map[String, UserType]): Unit = ???
}

// This seems a little redonkulous, but currently compound field type and struct work in a very similar way. This abstraction simplifies resolving the actual
// fields in both cases
case class FieldCollection(fields: Seq[Field]) {
  // Separate the wheat from the chaffe. We don't want to have to handle things like compound types, but just the members of the struct that
  // are optional, standard or repeating types.
  lazy val actualFields: Seq[Field] = {
    def recActualFields(accum: Seq[Field], remainder: Seq[Field]): Seq[Field] = remainder match {
      case Nil => accum
      case Field(_, compound: CompoundFieldType) :: tail => recActualFields(accum ++ compound.fields.actualFields, tail)
      case Field(_, compound: LiteralFieldType) :: tail => recActualFields(accum, tail)
      case field :: tail => recActualFields(accum ++ Seq(field), tail)
    }
    recActualFields(Nil, fields)
  }
}

case class Namespace(name: String, includes: Seq[String], definitions: Seq[UserType])

sealed trait FieldType {
  protected def baseIsValidType(name: String, knownTypes: Map[String, UserType]): Boolean = {
    knownTypes.keySet.contains(name) || primitiveTypes.contains(name)
  }

  def toScala: String

  def toProtobuf: String

  def isValidType(checker: String => Boolean): Boolean
}

// TODO - add field types here...
case class CompoundFieldType(fields: FieldCollection) extends FieldType {
  override def isValidType(checker: String => Boolean): Boolean = fields.fields.forall(_.isValidType(checker))

  override def toScala: String = ???

  override def toProtobuf: String = ???
}

case class RepeatingFieldType(typeName: String) extends FieldType {
  override def isValidType(checker: String => Boolean): Boolean = checker(typeName)

  override def toScala: String = s"Seq[$typeName]"

  override def toProtobuf: String = ???
}

case class OptionalFieldType(typeName: String) extends FieldType {
  override def isValidType(checker: String => Boolean): Boolean = checker(typeName)

  override def toScala: String = s"Option[$typeName]"

  override def toProtobuf: String = ???
}

case class LiteralFieldType(typeValue: String) extends FieldType {
  override def isValidType(checker: String => Boolean): Boolean = true

  override def toScala: String = ???

  override def toProtobuf: String = ???
}

case class StandardFieldType(typeName: String) extends FieldType {
  override def isValidType(checker: String => Boolean): Boolean = checker(typeName)

  override def toScala: String = typeName

  override def toProtobuf: String = ???
}

case class Field(name: String, fieldType: FieldType) {
  def isValidType(checker: String => Boolean): Boolean = fieldType.isValidType(checker)
}

class MagellanParser extends RegexParsers {
  val ident = """([a-zA-Z][a-zA-Z0-9_]*)""".r
  //val stringIdent = """([a-zA-Z][a-zA-Z0-9_]*)""".r
  lazy val source: Parser[Seq[Namespace]] = rep(namespace)

  lazy val namespace: Parser[Namespace] =
    ("namespace" ~> ident) ~ opt(includesClause) ~ ("{" ~> namespaceBody <~ "}") ^^ {
      case name ~ includes ~ definitions => includes.map(i => Namespace(name, i, definitions)).getOrElse(Namespace(name, Nil, definitions))
    }

  lazy val includesClause: Parser[Seq[String]] = "includes" ~> repsep(ident, ",")

  lazy val namespaceBody: Parser[Seq[UserType]] = rep(bodyElement)

  lazy val bodyElement: Parser[UserType] = enum | struct

  lazy val enum: Parser[Enum] = ("enum" ~> ident) ~ enumBody ^^ { case name ~ enumerants => Enum(name, enumerants)}
  
  lazy val enumBody: Parser[Seq[String]] = "{" ~> repsep(ident, ",") <~ "}"

  lazy val struct: Parser[Struct] = ("struct" ~> ident) ~ opt(extendsClause) ~ structBody ^^ { case name ~ parent ~ fields => Struct(name, parent, fields)}

  lazy val extendsClause: Parser[String] = "extends" ~> ident

  lazy val structBody: Parser[FieldCollection] = "{" ~> repsep(field, ",") <~ "}" ^^ { case fields => FieldCollection(fields)}

  lazy val stringIdent: Parser[String] = "\"" ~> ident <~ "\""

  lazy val field: Parser[Field] = (stringIdent <~ ":") ~ fieldType ^^ { case name ~ typeParameter => Field(name, typeParameter) }

  lazy val fieldType: Parser[FieldType] =
    stringFieldType | optionalFieldType | standardFieldType | compoundFieldType | repeatingFieldType

  lazy val stringFieldType: Parser[FieldType] = stringIdent ^^ { case string => LiteralFieldType(string) }

  lazy val optionalFieldType: Parser[FieldType] = "?" ~> ident ^^ { case typeParameter => OptionalFieldType(typeParameter)}

  lazy val standardFieldType: Parser[FieldType] = ident ^^ { case typeParameter => StandardFieldType(typeParameter)}

  lazy val repeatingFieldType: Parser[FieldType] = "[" ~> ident <~ "]" ^^ { case typeParameter => RepeatingFieldType(typeParameter)}

  lazy val compoundFieldType: Parser[FieldType] = structBody ^^ { case typeParameter => CompoundFieldType(typeParameter)}
}

