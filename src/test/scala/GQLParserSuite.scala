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

trait SuiteHelper extends munit.FunSuite with GQLSchemaParser {
  def checkResult[A](res: ParseResult[A], f: A => Unit): Unit =
    res match
      case Success(matched, _) => f(matched)
      case Failure(msg, _)     => fail("FAILURE: " + msg)
      case Error(msg, _)       => fail("ERROR: " + msg)
}

class GQLParserSuite extends SuiteHelper {
  test("Parse !") {
    checkResult(
      parse(nonnull, "!"),
      { matched => assertEquals(matched.str, "!") }
    )
  }

  test("Parse type any") {
    checkResult(
      parse(typeany, "Character"),
      matched =>
        assertEquals(matched.str, "Character")
        assertEquals(matched.nonull, false)
    )

    checkResult(
      parse(typeany, "Character!"),
      matched =>
        assertEquals(matched.str, "Character")
        assertEquals(matched.nonull, true)
    )
  }

  test("Parse array") {
    checkResult(
      parse(array, "[Character]"),
      matched =>
        assertEquals(matched.t.str, "Character")
        assertEquals(matched.t.nonull, false)
        assertEquals(matched.nonull, false)
    )

    checkResult(
      parse(array, "[Character!]"),
      matched =>
        assertEquals(matched.t.str, "Character")
        assertEquals(matched.t.nonull, true)
        assertEquals(matched.nonull, false)
    )

    checkResult(
      parse(array, "[Character!]!"),
      matched =>
        assertEquals(matched.t.str, "Character")
        assertEquals(matched.t.nonull, true)
        assertEquals(matched.nonull, true)
    )
  }

  test("Parse simple field definition") {
    checkResult(
      parse(fielddef, "hero: Character"),
      matched =>
        assertEquals(
          matched,
          FIELD_DEF("hero", List(), TYPE("Character", false))
        )
    )
  }

  test("Parse simple non null field definition") {
    checkResult(
      parse(fielddef, "hero: Character!"),
      matched =>
        assertEquals(matched.str, "hero")
        assertEquals(
          matched,
          FIELD_DEF("hero", List(), TYPE("Character", true))
        )
    )
  }

  test("Parse simple array field definition") {
    checkResult(
      parse(fielddef, "hero: [Character]!"),
      matched =>
        assertEquals(
          matched,
          FIELD_DEF("hero", List(), ARRAY(TYPE("Character", false), true))
        )
    )
  }

  test("Parse simple non null array field definition") {
    checkResult(
      parse(fielddef, "hero: [Character!]!"),
      matched =>
        assertEquals(
          matched,
          FIELD_DEF("hero", List(), ARRAY(TYPE("Character", true), true))
        )
    )
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

    checkResult(
      parse(iface, ifaceSchema),
      matched =>
        assertEquals(matched.str, "Character")
        assertEquals(
          matched.fields,
          List(
            FIELD_DEF("id", List(), TYPE("ID", true)),
            FIELD_DEF("name", List(), TYPE("String", true)),
            FIELD_DEF(
              "friends",
              List(),
              ARRAY(TYPE("Character", false), false)
            ),
            FIELD_DEF("appearsIn", List(), ARRAY(TYPE("Episode", false), true))
          )
        )
    )
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

    checkResult(
      parse(typedef, typedefSchema),
      matched =>
        assertEquals(matched.str, "Human")
        assertEquals(matched.iface, Some("Character"))
        assertEquals(
          matched.fields,
          List(
            FIELD_DEF("id", List(), TYPE("ID", true)),
            FIELD_DEF("name", List(), TYPE("String", true)),
            FIELD_DEF(
              "friends",
              List(),
              ARRAY(TYPE("Character", false), false)
            ),
            FIELD_DEF("appearsIn", List(), ARRAY(TYPE("Episode", false), true)),
            FIELD_DEF("homePlanet", List(), TYPE("String", false))
          )
        )
    )
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

    checkResult(
      parse(typedef, typedefSchema),
      matched =>
        assertEquals(matched.str, "QueryType")
        assertEquals(matched.iface, None)
        assertEquals(
          matched.fields,
          List(
            FIELD_DEF(
              "hero",
              List(ARG_DEF("episode", TYPE("Episode", false))),
              TYPE("Character", false)
            ),
            FIELD_DEF(
              "human",
              List(ARG_DEF("id", TYPE("String", false))),
              TYPE("Human", false)
            ),
            FIELD_DEF(
              "droid",
              List(ARG_DEF("id", TYPE("ID", true))),
              TYPE("Droid", false)
            ),
            FIELD_DEF("humans", List(), ARRAY(TYPE("Human", false), false)),
            FIELD_DEF("droids", List(), ARRAY(TYPE("Droid", false), false))
          )
        )
    )
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

    checkResult(
      parse(enumdef, typedefSchema),
      matched =>
        assertEquals(matched.str, "Episode")
        assertEquals(
          matched.fields,
          List(
            "NEWHOPE",
            "EMPIRE",
            "JEDI"
          )
        )
    )
  }

  test("Parse schema definition") {
    val typedefSchema =
      """
      |    schema {
      |        query: QueryType
      |    }
        """.stripMargin

    checkResult(
      parse(schemadef, typedefSchema),
      matched =>
        assertEquals(
          matched.fields,
          List(
            FIELD_DEF("query", List(), TYPE("QueryType", false))
          )
        )
    )
  }
}

val extendedSchema =
  """
      |
      |    external source LocalPsql {
      |        kind: PostgreSQL
      |        host: localhost
      |        port: 5432
      |        user: root
      |        password: root
      |        database: users
      |    }
      |
      |    external type User {
      |        source: LocalPsql
      |        table: users
      |    }
      |
      |    external source CompanyZenDesk {
      |        kind: ZenDesk
      |        host: api.zendesk.com
      |        token: $371OEUAU5Eueae_eu
      |    }
      |
      |    external type Ticket {
      |        source: CompanyZenDesk
      |        entity: ticket
      |    }
      |
      |    relation {
      |        from: User
      |        to: Ticket
      |        field: tickets
      |        kind: one-to-many
      |        on: User.id == Ticket.author_id
      |    }
      |
    """.stripMargin

class GQLQuerySuite extends SuiteHelper with GQLQueryParser {
  test("Parse simple query") {
    val input =
      """
      |{
      |  hero {
      |    name
      |  }
      |}
        """.stripMargin

    checkResult(
      parse(query, input),
      matched =>
        assertEquals(matched.head.str, "hero")
        assertEquals(matched.head.children.head.str, "name")
    )
  }

  test("Parse simple query with ARGS") {
    val input =
      """
      |{
      |  human(id: "1000") {
      |    name
      |    height
      |  }
      |}
        """.stripMargin

    checkResult(
      parse(query, input),
      matched =>
        assertEquals(matched.head.str, "human")
        assertEquals(
          matched.head.args,
          List(
            ARG_DEF("id", TYPE("1000", false))
          )
        )
        assertEquals(matched.head.children.head.str, "name")
        assertEquals(matched.head.children.last.str, "height")
    )
  }
}
