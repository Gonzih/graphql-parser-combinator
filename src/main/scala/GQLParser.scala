package com.example.gqlparser

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
case class OPEN_PAREN(str: String) extends GQLToken
case class CLOSE_PAREN(str: String) extends GQLToken

case class TYPE(str: String, nonull: Boolean) extends GQLType
case class ARRAY(t: TYPE, nonull: Boolean) extends GQLType
case class ARG_DEF(str: String, t: GQLType) extends GQLToken
case class FIELD_DEF(str: String, args: List[ARG_DEF], t: GQLType)
    extends GQLToken

case class INTERFACE(str: String, fields: List[FIELD_DEF]) extends GQLType
case class TYPE_DEF(
    str: String,
    iface: Option[String],
    fields: List[FIELD_DEF]
) extends GQLType

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
  def openparen: Parser[OPEN_PAREN] =
    """\(""".r ^^ { OPEN_PAREN(_) }
  def closeparen: Parser[CLOSE_PAREN] =
    """\)""".r ^^ { CLOSE_PAREN(_) }

  def array: Parser[ARRAY] =
    openbracket ~ typeany ~ closebracket ~ opt(nonnull) ^^ {
      case _ ~ t ~ _ ~ nn => ARRAY(t, nn.isDefined)
    }

  def array_or_type: Parser[GQLType] =
    array | typeany ^^ { case t => t }

  def argdef: Parser[ARG_DEF] =
    identifier ~ colon ~ array_or_type ^^ { case id ~ _ ~ t =>
      ARG_DEF(id.str, t)
    }

  def argsdef: Parser[List[ARG_DEF]] =
    openparen ~ repsep(
      identifier ~ colon ~ array_or_type,
      ","
    ) ~ closeparen ^^ { case _ ~ args ~ _ =>
      args.map({ case id ~ _ ~ t => ARG_DEF(id.str, t) })
    }

  def fielddef: Parser[FIELD_DEF] =
    identifier ~ opt(argsdef) ~ colon ~ array_or_type ^^ { case id ~ args ~ _ ~ t =>
      FIELD_DEF(id.str, args.toList.flatten, t)
    }

  def iface: Parser[INTERFACE] =
    """interface""" ~ identifier ~ openbrace ~ rep1(fielddef) ~ closebrace ^^ {
      case _ ~ id ~ _ ~ fields ~ _ => INTERFACE(id.str, fields)
    }

  def typedef: Parser[TYPE_DEF] =
    """type""" ~ identifier ~ opt(
      """implements""" ~ identifier
    ) ~ openbrace ~ rep1(fielddef) ~ closebrace ^^ {
      case _ ~ id ~ Some(_ ~ iface) ~ _ ~ fields ~ _ =>
        TYPE_DEF(id.str, Some(iface.str), fields)
      case _ ~ id ~ None ~ _ ~ fields ~ _ => TYPE_DEF(id.str, None, fields)
    }

end SimpleParser

// object SimpleParser extends SimpleParser:
// def run(input: String): ParseResult[GQLToken] =
// parse(freq, input)
// end SimpleParser
