package com.example.gqlparser

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

class GQLParserSuite extends munit.FunSuite with SimpleParser {
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
        assertEquals(matched, FIELD_DEF("hero", List(), TYPE("Character", false)))
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)
  }

  test("Parse simple non null field definition") {
    parse(fielddef, "hero: Character!") match
      case Success(matched, _) =>
        assertEquals(matched.str, "hero")
        assertEquals(matched, FIELD_DEF("hero", List(), TYPE("Character", true)))
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)
  }

  test("Parse simple array field definition") {
    parse(fielddef, "hero: [Character]!") match
      case Success(matched, _) =>
        assertEquals(
          matched,
          FIELD_DEF("hero", List(), ARRAY(TYPE("Character", false), true))
        )
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)
  }

  test("Parse simple non null array field definition") {
    parse(fielddef, "hero: [Character!]!") match
      case Success(matched, _) =>
        assertEquals(
          matched,
          FIELD_DEF("hero", List(), ARRAY(TYPE("Character", true), true))
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
            FIELD_DEF("id", List(), TYPE("ID", true)),
            FIELD_DEF("name", List(), TYPE("String", true)),
            FIELD_DEF("friends", List(), ARRAY(TYPE("Character", false), false)),
            FIELD_DEF("appearsIn", List(), ARRAY(TYPE("Episode", false), true))
          )
        )
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)
  }

  test("Parse type definition") {
    val typedefSchema =
      """
          |    type Human implements Character {
          |        id: ID!
          |        name: String!
          |        friends: [Character]
          |        appearsIn: [Episode]!
          |        homePlanet: String
          |    }
        """.stripMargin
    parse(typedef, typedefSchema) match
      case Success(matched, _) =>
        assertEquals(matched.str, "Human")
        assertEquals(matched.iface, Some("Character"))
        assertEquals(
          matched.fields,
          List(
            FIELD_DEF("id", List(), TYPE("ID", true)),
            FIELD_DEF("name", List(), TYPE("String", true)),
            FIELD_DEF("friends", List(), ARRAY(TYPE("Character", false), false)),
            FIELD_DEF("appearsIn", List(), ARRAY(TYPE("Episode", false), true)),
            FIELD_DEF("homePlanet", List(), TYPE("String", false)),
          )
        )
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)
  }

  test("Parse query type definition") {
    val typedefSchema =
      """
      |    type QueryType {
      |        hero(episode: Episode): Character
      |        human(id : String) : Human
      |        droid(id: ID!): Droid
      |        humans: [Human]
      |        droids: [Droid]
      |    }
        """.stripMargin
    parse(typedef, typedefSchema) match
      case Success(matched, _) =>
        assertEquals(matched.str, "QueryType")
        assertEquals(matched.iface, None)
        assertEquals(
          matched.fields,
          List(
            FIELD_DEF("hero", List(ARG_DEF("episode", TYPE("Episode", false))), TYPE("Character", false)),
            FIELD_DEF("human", List(ARG_DEF("id", TYPE("String", false))), TYPE("Human", false)),
            FIELD_DEF("droid", List(ARG_DEF("id", TYPE("ID", true))), TYPE("Droid", false)),
            FIELD_DEF("humans", List(), ARRAY(TYPE("Human", false), false)),
            FIELD_DEF("droids", List(), ARRAY(TYPE("Droid", false), false)),
          )
        )
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)
  }

  test("Parse enum definition") {
    val typedefSchema =
      """
      |    enum Episode {
      |        NEWHOPE
      |        EMPIRE
      |        JEDI
      |    }
        """.stripMargin
    parse(enumdef, typedefSchema) match
      case Success(matched, _) =>
        assertEquals(matched.str, "Episode")
        assertEquals(
          matched.fields,
          List(
            "NEWHOPE",
            "EMPIRE",
            "JEDI"
          )
        )
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)
  }

  test("Parse schema definition") {
    val typedefSchema =
      """
      |    schema {
      |        query: QueryType
      |    }
        """.stripMargin
    parse(schemadef, typedefSchema) match
      case Success(matched, _) =>
        assertEquals(
          matched.fields,
          List(
            FIELD_DEF("query", List(), TYPE("QueryType", false)),
          )
        )
      case Failure(msg, _) => fail("FAILURE: " + msg)
      case Error(msg, _)   => fail("ERROR: " + msg)
  }
}
