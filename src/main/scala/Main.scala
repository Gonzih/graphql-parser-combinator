package com.example

import scala.util.parsing.combinator._

case class WordFreq(word: String, count: Int):
  override def toString = "Word <" + word + "> " +
                          "occurs with frequency " + count
end WordFreq

sealed trait GQLToken
sealed trait GQLType extends GQLToken

case class IDENTIFIER(str: String) extends GQLToken
case class NON_NULL(str: String) extends GQLToken
case class COLON(str: String) extends GQLToken

case class OPEN_BRACKET(str: String) extends GQLToken
case class CLOSE_BRACKET(str: String) extends GQLToken
case class OPEN_BRACE(str: String) extends GQLToken
case class CLOSE_BRACE(str: String) extends GQLToken

case class TYPE(str: String, nonull: Boolean) extends GQLType
case class ARRAY(t: TYPE, nonull: Boolean) extends GQLType
case class FIELD_DEF(str: String, t: GQLType) extends GQLToken

trait SimpleParser extends RegexParsers:
  def identifier: Parser[IDENTIFIER] =
    """[a-zA-Z_][a-zA-Z0-9_]*""".r ^^ { IDENTIFIER(_) }
  def nonnull: Parser[NON_NULL] =
    """\!""".r ^^ { NON_NULL(_) }
  def colon: Parser[COLON] =
    """:""".r ^^ { COLON(_) }

  def typeany: Parser[TYPE] =
    identifier ~ opt(nonnull) ^^ { case t ~ nn => TYPE(t.str, nn.isDefined) }

  def openbracket: Parser[OPEN_BRACKET] =
    """\[""".r ^^ { OPEN_BRACKET(_) }
  def closebracket: Parser[CLOSE_BRACKET] =
    """\]""".r ^^ { CLOSE_BRACKET(_) }
  def openbrace: Parser[OPEN_BRACE] =
    """\{""".r ^^ { OPEN_BRACE(_) }
  def closebrace: Parser[CLOSE_BRACE] =
    """\}""".r ^^ { CLOSE_BRACE(_) }

  def array: Parser[ARRAY] =
    openbracket ~ typeany ~ closebracket ~ opt(nonnull) ^^ { case _ ~ t ~ _ ~ nn => ARRAY(t, nn.isDefined) }

  def array_or_type: Parser[GQLType] =
    array | typeany ^^ { case t => t }

  def fielddef: Parser[FIELD_DEF] =
    identifier ~ colon ~ array_or_type ^^ { case id ~ _ ~ t => FIELD_DEF(id.str, t) }
end SimpleParser

// object SimpleParser extends SimpleParser:
  // def run(input: String): ParseResult[GQLToken] =
    // parse(freq, input)
// end SimpleParser
