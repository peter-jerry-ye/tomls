package online.aoxiang.tomls.parser

import cats.parse.Parser
import cats.parse.Rfc5234._
import online.aoxiang.tomls.ast._
import Parsers._
import cats.data.NonEmptyList
import cats.data.EitherT
import cats.Eval
import cats.implicits._

object PValue {
  val parser: Parser[TValue] = Parser.recursive(pval => {
    val ws_comment_newline = (wsp | (PComment.parser.?.with1 *> newline)).rep0
    val array_values: Parser[NonEmptyList[TValue]] =
      (pval.surroundedBy(ws_comment_newline)).backtrack.repSep(Parser.char(',')) <* Parser.char(',').?
    val pInlineArray = (array_values.? <* ws_comment_newline).with1
      .between(Parser.char('[') ~ ws, ws.with1 ~ Parser.char(']'))
      .map(_.map(_.toList).getOrElse(Nil))
      .map(InlineArray(_))

    def insertValue(
        value: TValue,
        path: List[String],
        newVal: TValue,
        cumulPath: List[String]
    ): EitherT[Eval, String, TValue] = {
      path match {
        case Nil => EitherT(Eval.now(Left("Something's wrong. Please submit an issue.")))
        case hd :: Nil =>
          EitherT(Eval.now(value match {
            case InlineTable(table) =>
              if (cumulPath.nonEmpty) Left(s"Can't modify an inline table at: ${cumulPath.mkString(",")}")
              else Right(InlineTable(table + (hd -> newVal)))
            case TableTable(table) =>
              if (table.contains(hd)) Left(s"Can't modify an existing value at: ${cumulPath.mkString(",")}")
              else Right(TableTable(table + (hd -> newVal)))
            case _ => Left(s"Not a table at: ${cumulPath.mkString(",")}")
          }))
        case hd :: tl =>
          value match {
            case InlineTable(table) =>
              if (cumulPath.nonEmpty)
                EitherT(Eval.now(Left(s"Can't modify an inline table at: ${cumulPath.mkString(",")}")))
              else {
                if (table.contains(hd)) {
                  EitherT(Eval.defer(insertValue(table(hd), tl, newVal, cumulPath :+ hd).value))
                    .flatMapF(v => Eval.now(Right(InlineTable(table.updated(hd, v)))))
                } else {
                  EitherT(Eval.defer(insertValue(TableTable(Map.empty), tl, newVal, cumulPath :+ hd).value))
                    .flatMapF(v => Eval.now(Right(InlineTable(table.updated(hd, v)))))
                }
              }
            case TableTable(table) => (
              if (table.contains(hd)) {
                EitherT(Eval.defer(insertValue(table(hd), tl, newVal, cumulPath :+ hd).value))
                  .flatMapF(v => Eval.now(Right(TableTable(table.updated(hd, v)))))
              } else {
                EitherT(Eval.defer(insertValue(TableTable(Map.empty), tl, newVal, cumulPath :+ hd).value))
                  .flatMapF(v => Eval.now(Right(TableTable(table.updated(hd, v)))))
              }
            )
            case _ => EitherT(Eval.now(Left(s"Not a table at: ${cumulPath.mkString(",")}")))
          }
      }
    }

    val keyval: Parser[(NonEmptyList[String], TValue)] = PKey.parser ~ (Parser.char('=').surroundedBy(ws) *> pval)
    val keyvals = keyval.repSep(Parser.char(',').surroundedBy(ws).backtrack)
    val pInlineTable = keyvals.?.with1
      .between(Parser.char('{') ~ ws, ws.with1 ~ Parser.char('}'))
      .map(_.map(_.toList).getOrElse(Nil))
      .map(pairs =>
        pairs.foldM[[X] =>> EitherT[Eval, String, X], TValue](InlineTable(Map.empty))((value, pair) =>
          insertValue(value, pair._1.toList, pair._2, List.empty)
        )
      )
      .flatMap(result => result.value.value.fold(e => Parser.failWith(e), v => Parser.pure(v)))

    PBoolean.parser.backtrack | PTime.parser.backtrack | PFloat.parser.backtrack | PInteger.parser.backtrack
      | PString.parser | pInlineTable | pInlineArray
  })
}
