package com.example

val starWarsSchema =
  """
      |    schema {
      |        query: QueryType
      |    }
      |
      |    type QueryType {
      |        hero(episode: Episode): Character
      |        human(id : String) : Human
      |        droid(id: ID!): Droid
      |        humans: [Human]
      |        droids: [Droid]
      |    }
      |
      |
      |    enum Episode {
      |        NEWHOPE
      |        EMPIRE
      |        JEDI
      |    }
      |
      |    interface Character {
      |        id: ID!
      |        name: String!
      |        friends: [Character]
      |        appearsIn: [Episode]!
      |    }
      |
      |    type Human implements Character {
      |        id: ID!
      |        name: String!
      |        friends: [Character]
      |        appearsIn: [Episode]!
      |        homePlanet: String
      |    }
      |
      |    type Droid implements Character {
      |        id: ID!
      |        name: String!
      |        friends: [Character]
      |        appearsIn: [Episode]!
      |        primaryFunction: String
      |    }
    """.stripMargin

class MySuite extends munit.FunSuite with SimpleParser {
  test("Parse !") {
    parse(nonnull, "!") match
      case Success(matched, _) =>
        assertEquals(matched.str, "!")
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)
  }

  test("Parse type any") {
    parse(typeany, "Character") match
      case Success(matched, _) =>
        assertEquals(matched.str, "Character")
        assertEquals(matched.nonull, false)
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)

    parse(typeany, "Character!") match
      case Success(matched, _) =>
        assertEquals(matched.str, "Character")
        assertEquals(matched.nonull, true)
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)
  }

  test("Parse array") {
    parse(array, "[Character]") match
      case Success(matched, _) =>
        assertEquals(matched.t.str, "Character")
        assertEquals(matched.t.nonull, false)
        assertEquals(matched.nonull, false)
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)

    parse(array, "[Character!]") match
      case Success(matched, _) =>
        assertEquals(matched.t.str, "Character")
        assertEquals(matched.t.nonull, true)
        assertEquals(matched.nonull, false)
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)

    parse(array, "[Character!]!") match
      case Success(matched, _) =>
        assertEquals(matched.t.str, "Character")
        assertEquals(matched.t.nonull, true)
        assertEquals(matched.nonull, true)
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)
  }

  test("Parse simple field definition") {
    parse(fielddef, "hero: Character") match
      case Success(matched, _) =>
        assertEquals(matched, FIELD_DEF("hero", TYPE("Character", false)))
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)
  }

  test("Parse simple non null field definition") {
    parse(fielddef, "hero: Character!") match
      case Success(matched, _) =>
        assertEquals(matched.str, "hero")
        assertEquals(matched, FIELD_DEF("hero", TYPE("Character", true)))
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)
  }

  test("Parse simple array field definition") {
    parse(fielddef, "hero: [Character]!") match
      case Success(matched, _) =>
        assertEquals(
          matched,
          FIELD_DEF("hero", ARRAY(TYPE("Character", false), true))
        )
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)
  }

  test("Parse simple non null array field definition") {
    parse(fielddef, "hero: [Character!]!") match
      case Success(matched, _) =>
        assertEquals(
          matched,
          FIELD_DEF("hero", ARRAY(TYPE("Character", true), true))
        )
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)
  }

  test("Parse interface definition") {
    val ifaceSchema =
      """
          |    interface Character {
          |        id: ID!
          |        name: String!
          |        friends: [Character]
          |        appearsIn: [Episode]!
          |    }
        """.stripMargin
    parse(iface, ifaceSchema) match
      case Success(matched, _) =>
        assertEquals(matched.str, "Character")
        assertEquals(
          matched.fields,
          List(
            FIELD_DEF("id", TYPE("ID", true)),
            FIELD_DEF("name", TYPE("String", true)),
            FIELD_DEF("friends", ARRAY(TYPE("Character", false), false)),
            FIELD_DEF("appearsIn", ARRAY(TYPE("Episode", false), true))
          )
        )
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)
  }
}
