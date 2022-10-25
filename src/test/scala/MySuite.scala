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
        assertEquals(matched.str, "hero")
        matched.t match
          case TYPE(str, nonull) =>
            assertEquals(str, "Character")
            assertEquals(nonull, false)
          case ARRAY(t, nonull) =>
            fail("Should not be an arary here")
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)
  }

  test("Parse simple non null field definition") {
    parse(fielddef, "hero: Character!") match
      case Success(matched, _) =>
        assertEquals(matched.str, "hero")
        matched.t match
          case TYPE(str, nonull) =>
            assertEquals(str, "Character")
            assertEquals(nonull, true)
          case ARRAY(t, nonull) =>
            fail("Should not be an arary here")
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)
  }

  test("Parse simple array field definition") {
    parse(fielddef, "hero: [Character]!") match
      case Success(matched, _) =>
        assertEquals(matched.str, "hero")
        matched.t match
          case TYPE(str, nonull) =>
            fail("Should not be an arary here")
          case ARRAY(t, nonull) =>
            assertEquals(t.str, "Character")
            assertEquals(t.nonull, false)
            assertEquals(nonull, true)
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)
  }

  test("Parse simple non null array field definition") {
    parse(fielddef, "hero: [Character!]!") match
      case Success(matched, _) =>
        assertEquals(matched.str, "hero")
        matched.t match
          case TYPE(str, nonull) =>
            fail("Should not be an arary here")
          case ARRAY(t, nonull) =>
            assertEquals(t.str, "Character")
            assertEquals(t.nonull, true)
            assertEquals(nonull, true)
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)
  }
}
