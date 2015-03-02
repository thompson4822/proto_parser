package com.example.models

import org.scalatest.{Matchers, FunSuite}

/**
 * Created by steve on 3/1/15.
 */
class ConverterSpec extends FunSuite with Matchers {
  val addressBook1 = AddressBook(
    Seq(
      Person("Steve", 1, Seq(PhoneNumber(number = "9705324822", phoneType = Some(PhoneType.HOME))), Some("st123@binter.com"))
    )
  )

  test("Address book can be serialized and deserialized successfully (protobuf)") {
    import ProtobufConverter._
    val serialized = toProtobuf(addressBook1)
    fromProtobuf[AddressBook](serialized) should equal(addressBook1)
  }

  test("Phone number can be serialized/deserialized successfully (json)") {
    import JsonConverter._
    val phone = PhoneNumber(number = "9705324822", phoneType = Some(PhoneType.HOME))
    val serialized = toJson(phone)
    fromJson[PhoneNumber](serialized) should equal (phone)
  }

  test("Address book can be serialized/deserialized successfully (json)") {
    import JsonConverter._
    val json = toJson(addressBook1)
    fromJson[AddressBook](json) should equal (addressBook1)
  }
}
