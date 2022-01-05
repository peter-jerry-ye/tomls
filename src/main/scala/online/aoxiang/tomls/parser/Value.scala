package online.aoxiang.tomls.parser

import cats.parse.Parser
import cats.parse.Rfc5234._
import online.aoxiang.tomls.ast._
import Parsers._
import cats._
import cats.data._
import cats.implicits._

object PValue {
  val parser: Parser[TValue] = Parser.recursive(pval => {
    val ws_comment_newline = (wsp | (PComment.parser.?.with1 *> newline)).rep0
    val array_values: Parser[NonEmptyList[TValue]] =
      (pval.surroundedBy(ws_comment_newline)).backtrack.repSep(Parser.char(',')) <* Parser.char(',').?
    val pInlineArray = OptionT(
      (array_values.? <* ws_comment_newline).with1
        .between(Parser.char('[') ~ ws, ws.with1 ~ Parser.char(']'))
    ).map(NonEmptyChain.fromNonEmptyList(_).toChain).getOrElse(Chain.empty).map(InlineArray(_))

    def insertValue(
        value: TValue,
        path: NonEmptyList[String],
        newVal: TValue,
        cumulPath: Chain[String] = Chain.empty
    ): EitherT[Eval, String, TTable] = {
      path match {
        case NonEmptyList(hd, Nil) =>
          value match {
            case InlineTable(table) =>
              EitherT.leftT(s"Can't modify a defined table at: ${cumulPath.append(hd).mkString_(".")}")
            case IntermediateTable(table) =>
              if (table.contains(hd))
                EitherT.leftT(s"Can't modify a defined value at: ${cumulPath.append(hd).mkString_(".")}")
              else EitherT.rightT(IntermediateTable(table + (hd -> newVal)))
            case _ => EitherT.leftT(s"Can't modify a defined value at: ${cumulPath.append(hd).mkString_(".")}")
          }
        case NonEmptyList(hd, hl :: tl) =>
          value match {
            case InlineTable(table) =>
              EitherT.leftT(s"Can't modify a defined at: ${cumulPath.append(hd).mkString_(".")}")
            case IntermediateTable(table) => (
              EitherT(
                Eval.defer(
                  insertValue(
                    table.getOrElse(hd, IntermediateTable(Map.empty)),
                    NonEmptyList(hl, tl),
                    newVal,
                    cumulPath :+ hd
                  ).value
                )
              ).map(v => IntermediateTable(table.updated(hd, v)))
            )
            case _ => EitherT.leftT(s"Can't modify a defined value at: ${cumulPath.append(hd).mkString_(",")}")
          }
      }
    }

    val keyval: Parser[(NonEmptyList[String], TValue)] = PKey.parser ~ (Parser.char('=').surroundedBy(ws) *> pval)
    val keyvals = keyval.repSep(Parser.char(',').surroundedBy(ws).backtrack)
    val pInlineTable = keyvals.?.with1
      .between(Parser.char('{') ~ ws, ws.with1 ~ Parser.char('}'))
      .map(_.map(_.toList).getOrElse(Nil))
      .map(pairs =>
        pairs.foldM[[X] =>> EitherT[Eval, String, X], TTable](IntermediateTable(Map.empty))((value, pair) =>
          insertValue(value, pair._1, pair._2)
        )
      )
      .flatMap(result => result.value.value.fold(e => Parser.failWith(e), v => Parser.pure(InlineTable(v.value))))

    PBoolean.parser.backtrack | PTime.parser.backtrack | PFloat.parser.backtrack | PInteger.parser.backtrack
      | PString.parser | pInlineTable | pInlineArray
  })
}
