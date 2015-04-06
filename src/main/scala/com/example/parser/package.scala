package com.example

package object parser {
  val example =
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
      |        "coordinates" : [Double]
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
      |    enum Quality { overlap, offset, bearing }
      |
      |    struct Context { }
      |
      |}
      |
      |namespace SurveyEndpoint includes Common {
      |
      |    struct TripNodes {
      |        "type" : "FeatureCollection",
      |        "features" : [TripNode]
      |    }
      |
      |    struct TripNode {
      |        "type" : "Feature",
      |        "properties" : {
      |            "timezone" : String,
      |            "description" : String
      |        },
      |        "geometry" : Coordinate
      |    }
      |
      |    struct TripLeg {
      |        "departureNode" : Int,
      |        "arrivalNode" : Int,
      |        "pathLength" : Double,
      |        "mode" : LegMode,
      |        "time" : LegTime,
      |        "cost" : Cost
      |    }
      |
      |    struct SurveyEndpointResponse {
      |        "paths" : [Path]
      |    }
      |
      |    struct Path {
      |        "waypoint" : String,
      |        "description" : String,
      |        "strategy" : String,
      |        "tripLength" : Double,
      |        "tripNodes" : TripNodes,
      |        "tripLegs" : [TripLeg],
      |        "tripCost" : Cost,
      |        "totalTripTime" : Int,
      |        "tripBegin" : Date,
      |        "tripEnd" : Date
      |    }
      |}
      |
      |namespace Maneuvers includes Common {
      |
      |    struct Location {
      |        "description" : String,
      |        "streetAddress" : ?String,
      |        "geometry" : Coordinate
      |    }
      |
      |    struct TransitRoute {
      |        "agency" : Int,
      |        "id" : Int,
      |        "name" : String,
      |        "departures" : [Date]
      |    }
      |
      |    struct ManeuverResponse {
      |        "context" : Context,
      |        "maneuvers" : [Maneuver]
      |    }
      |
      |    struct Maneuver {
      |        "type" : String
      |    }
      |
      |    struct BeginManeuver extends Maneuver {
      |        "mode" : LegMode,
      |        "type" : "begin",
      |        "time" : LegTime,
      |        "origin" : Location,
      |        "target" : Location,
      |        "compassDirection" : CompassDirection,
      |        "traversalDuration" : Int,
      |        "pathLength" : Double,
      |        "summary" : String
      |    }
      |
      |    struct TraverseManeuver extends Maneuver {
      |        "mode" : LegMode,
      |        "type" : "traverse",
      |        "target" : Location,
      |        "transitRoute" : TransitRoute,
      |        "time" : LegTime,
      |        "intermediateStopCount" : Int,
      |        "length" : Double,
      |        "summary" : String
      |    }
      |
      |    struct TurnManeuver extends Maneuver {
      |        "mode" : LegMode,
      |        "type" : "turn",
      |        "target" : Location,
      |        "relativeDirection" : RelativeDirection,
      |        "compassDirection" : CompassDirection,
      |        "summary" : String
      |    }
      |
      |    struct BoardManeuver extends Maneuver {
      |        "mode" : LegMode,
      |        "type" : "board",
      |        "time" : LegTime,
      |        "transitRoute" : TransitRoute,
      |        "target" : Location,
      |        "summary" : String
      |    }
      |}
      |
      |namespace PathShape includes Common {
      |
      |    struct PathShapeResponse {
      |        "type": "FeatureCollection",
      |        "features": [PathShape]
      |    }
      |
      |    struct LineString {
      |        "type" : "LineString",
      |        "coordinates" : [Double]
      |    }
      |
      |    struct PathShape {
      |        "type" : "Feature",
      |        "properties" : {
      |            "mode" : LegMode,
      |            "quality" : Quality
      |        },
      |        "geometry" : LineString
      |    }
      |}
      |
      |namespace VehicleStation includes Common {
      |
      |    struct VehicleStationResponse {
      |        "type" : "FeatureCollection",
      |        "features" : [VehicleStation]
      |    }
      |
      |    struct VehicleStation {
      |        "id" : String,
      |        "type" : "Feature",
      |        "geometry" : Coordinate,
      |        "properties" : {
      |            "locationDescription" : String,
      |            "totalVehicles" : Int,
      |            "availableVehicles" : Int,
      |            "vehicles" : [Vehicle]
      |        }
      |    }
      |
      |    struct Vehicle {
      |        "fuelLevel" : Int,
      |        "vin" : String
      |    }
      |}
      |
    """.stripMargin

  val primitiveTypes = Seq(
    "Double",
    "Int",
    "String",
    "Decimal",
    "Boolean",
    "Date"
  )

  var namespaces: Seq[Namespace] = _


  var errorMessage: String = _

  // TODO - the following doesn't take into account instances where the user includes a namespace that
  // doesn't exist. In that case, it is simply ignored.
  // The following maps a namespace identifier to all the namespaces that should be searched for a user type.
  lazy val mappings: Map[String, Seq[Namespace]] = {
    (for {
      namespace <- namespaces
      name = namespace.name
      includeNames = namespace.includes
      includedNamespaces = namespaces.filter(ns => includeNames.contains(ns.name))
    } yield name -> (namespace +: includedNamespaces)).toMap
  }

  // What structs and enums are available to the given namespace?
  def knownUserTypes(namespace: Namespace, mappings: Map[String, Seq[Namespace]]): Map[String, UserType] = {
    (for {
      ns <- mappings.get(namespace.name).get
      userType <- ns.definitions
    } yield userType.name -> userType).toMap
  }

  def isPrimitive(typeName: String): Boolean = primitiveTypes.contains(typeName)

  def isEnum(typeName: String): Boolean = {
    val enums: Seq[Enum] = namespaces.flatMap(_.definitions).collect{ case definition: Enum => definition }
    enums.exists(_.name == typeName)
  }
}
