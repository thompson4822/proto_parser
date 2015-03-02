package com.example.models

import com.example.tutorial._
import scala.collection.JavaConversions._

object JsonConverter {
  import org.json4s.jackson.Serialization.{read, write}
  implicit val formats = org.json4s.DefaultFormats + new org.json4s.ext.EnumNameSerializer(PhoneType)

  trait Converter[T] {
    def convertToJson(element: T): String
    def convertFromJson(json: String): T
  }

  implicit object AddressBookConverter extends Converter[AddressBook] {
    override def convertToJson(element: AddressBook): String = write(element)

    override def convertFromJson(json: String): AddressBook = read[AddressBook](json)
  }

  implicit object PersonConverter extends Converter[Person] {
    override def convertToJson(element: Person): String = write(element)

    override def convertFromJson(json: String): Person = read[Person](json)
  }

  implicit object PhoneNumberConverter extends Converter[PhoneNumber] {
    override def convertToJson(element: PhoneNumber): String = write(element)

    override def convertFromJson(json: String): PhoneNumber = read[PhoneNumber](json)
  }

  def toJson[T: Converter](element: T) = implicitly[Converter[T]].convertToJson(element)
  def fromJson[T: Converter](json: String): T = implicitly[Converter[T]].convertFromJson(json)
}

object ProtobufConverter {
  trait Converter[T] {
    def convertToProtobuf(element: T): Array[Byte]
    def convertFromProtobuf(bytes: Array[Byte]): T
  }

  implicit object AddressBookConverter extends Converter[AddressBook] {
    override def convertToProtobuf(element: AddressBook): Array[Byte] = {
      val builder = AddressBookProtos.AddressBook.newBuilder
      element.contacts.foreach(person => builder.addPerson(AddressBookProtos.Person.parseFrom(toProtobuf(person))))
      builder.build.toByteArray
    }

    override def convertFromProtobuf(bytes: Array[Byte]): AddressBook = {
      val addressBook = AddressBookProtos.AddressBook.parseFrom(bytes)
      AddressBook(addressBook.getPersonList.map(contact => fromProtobuf[Person](contact.toByteArray)))
    }
  }

  implicit object PersonConverter extends Converter[Person] {
    override def convertToProtobuf(element: Person): Array[Byte] = {
      val builder = AddressBookProtos.Person.newBuilder
        .setName(element.name)
        .setId(element.id)
      element.email.map(builder.setEmail)
      element.phoneNumbers.foreach(phoneNumber => builder.addPhone(AddressBookProtos.Person.PhoneNumber.parseFrom(toProtobuf(phoneNumber))))
      builder.build.toByteArray
    }

    override def convertFromProtobuf(bytes: Array[Byte]): Person = {
      val person = AddressBookProtos.Person.parseFrom(bytes)
      val email = if(person.hasEmail) Some(person.getEmail) else None
      Person(name = person.getName, id = person.getId, email = email, phoneNumbers = person.getPhoneList.map(phone => fromProtobuf[PhoneNumber](phone.toByteArray)))
    }
  }

  implicit object PhoneNumberConverter extends Converter[PhoneNumber] {
    override def convertToProtobuf(element: PhoneNumber): Array[Byte] = {
      val builder = AddressBookProtos.Person.PhoneNumber.newBuilder
        .setNumber(element.number)
      element.phoneType.map(phoneType => builder.setType(AddressBookProtos.Person.PhoneType.valueOf(phoneType.id)))
      builder.build.toByteArray
    }

    override def convertFromProtobuf(bytes: Array[Byte]): PhoneNumber = {
      val phoneNumber = AddressBookProtos.Person.PhoneNumber.parseFrom(bytes)
      val phoneType = if(phoneNumber.hasType) Some(PhoneType(phoneNumber.getType.getNumber)) else None
      PhoneNumber(number = phoneNumber.getNumber, phoneType)
    }
  }

  def toProtobuf[T: Converter](element: T) = implicitly[Converter[T]].convertToProtobuf(element)
  def fromProtobuf[T: Converter](bytes: Array[Byte]): T = implicitly[Converter[T]].convertFromProtobuf(bytes)
}

object PhoneType extends Enumeration { val MOBILE, HOME, WORK = Value }

case class AddressBook(contacts: Seq[Person])

case class Person(name: String, id: Int, phoneNumbers: Seq[PhoneNumber], email: Option[String] = None)

case class PhoneNumber(number: String, phoneType: Option[PhoneType.Value])

