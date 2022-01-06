package online.aoxiang.tomls.parser

import cats.parse.Parser
import Parsers._
import cats.data.NonEmptyList
import cats.data.NonEmptyChain
import cats.data.Chain
import cats.implicits._
import cats.parse.Parser0
import cats.data.EitherT
import cats.Eval

object PToml {
  def parser: Parser0[IntermediateTable] = {
    val std_table = PKey.parser.between(Parser.char('[') ~ ws, ws.with1 ~ Parser.char(']')).map(l => StdTable(l.toList))
    val array_table = PKey.parser.between(Parser.string("[[") ~ ws, ws.with1 ~ Parser.string("]]")).map(ArrTable(_))

    val keyval: Parser[(NonEmptyList[String], TValue)] =
      PKey.parser ~ (Parser.char('=').surroundedBy(ws) *> PValue.parser)
    val sep = ((ws ~ PComment.parser.?).with1 *> newline <* ws).backtrack.rep
    val expression = array_table.backtrack | std_table | keyval
    val toml = expression
      .repSep0(sep)
      .between(
        ((ws ~ PComment.parser.?).with1 *> newline).backtrack.rep0 ~ ws,
        ((ws ~ PComment.parser.?).with1 *> newline).backtrack.rep0 ~ ws ~ PComment.parser.?
      )

    def foldExpressions(list: List[Object]): NonEmptyChain[PToml] = {
      list.foldLeft(NonEmptyChain[PToml](StdTable(List.empty)))((chain, obj) =>
        obj match {
          case t: PToml => chain :+ t
          case (key, value): (NonEmptyList[String], TValue) => {
            val (init, last) = chain.initLast
            last match {
              case ArrTable(keys, values) =>
                NonEmptyChain.fromChainAppend(init, ArrTable(keys, values :+ (key, value)))
              case StdTable(keys, values) =>
                NonEmptyChain.fromChainAppend(init, StdTable(keys, values :+ (key, value)))
            }
          }
        }
      )
    }

    def insertTable(
        value: IntermediateTable,
        table: PToml
    ): EitherT[Eval, String, IntermediateTable] = {
      extension (t: StandardTable | IntermediateTable)
        def updated(v: Map[String, TValue]): StandardTable | IntermediateTable = {
          t match {
            case _: StandardTable     => StandardTable(v)
            case _: IntermediateTable => IntermediateTable(v)
          }
        }
      def insertStdTable(
          value: StandardTable | IntermediateTable,
          keys: List[String],
          values: Chain[(NonEmptyList[String], TValue)],
          cumulPath: Chain[String]
      ): EitherT[Eval, String, StandardTable | IntermediateTable] = keys match {
        case Nil =>
          values
            .foldM(StandardTable(value.value))((t, v) =>
              insertValue(t, v._1, v._2, cumulPath).map(v => v.asInstanceOf[StandardTable])
            )
        case hd :: tl => {
          val updatedValue: Eval[Either[String, TValue]] = Eval.defer {
            value.value.getOrElse(hd, IntermediateTable(Map.empty)) match {
              case subtable: IntermediateTable =>
                insertStdTable(subtable, tl, values, cumulPath :+ hd).value
              case subtable: StandardTable =>
                if (tl.isEmpty)
                  Eval.now(Left(s"Can't modify a defined table at: ${cumulPath.append(hd).mkString_(",")}"))
                else insertStdTable(subtable, tl, values, cumulPath :+ hd).value
              case subarray: TableArray => {
                if (tl.isEmpty)
                  Eval.now(Left(s"Can't modify a defined table array at: ${cumulPath.append(hd).mkString_(",")}"))
                else {
                  val (init, last) = subarray.tables.initLast
                  insertStdTable(last, tl, values, cumulPath :+ hd)
                    .map(v => TableArray(NonEmptyChain.fromChainAppend(init, v.asInstanceOf[StandardTable])))
                    .value
                }
              }
              case _: InlineTable =>
                Eval.now(Left(s"Can't modify an inline table at: ${cumulPath.append(hd).mkString_(",")}"))
              case _ =>
                Eval.now(Left(s"Can't modify a defined value at: ${cumulPath.append(hd).mkString_(",")}"))
            }
          }
          EitherT(updatedValue).map(v => value.updated(value.value.updated(hd, v)))
        }
      }

      def insertArrayTable(
          value: StandardTable | IntermediateTable,
          keys: List[String],
          values: Chain[(NonEmptyList[String], TValue)],
          cumulPath: Chain[String]
      ): EitherT[Eval, String, TTable] = keys match {
        case Nil =>
          value match {
            case table: (IntermediateTable | StandardTable) =>
              values
                .foldM(StandardTable(table.value))((t, v) =>
                  insertValue(t, v._1, v._2, cumulPath).map(v => v.asInstanceOf[StandardTable])
                )
                .map(identity)
          }
        case hd :: Nil =>
          value.value.get(hd) match {
            case Some(TableArray(tables)) => {
              EitherT(Eval.defer {
                insertArrayTable(StandardTable(Map.empty), Nil, values, cumulPath :+ hd)
                  .map(v => TableArray(tables.append(v.asInstanceOf[StandardTable])))
                  .value
              }).map(v => value.updated(value.value + (hd -> v)))
            }
            case None => {
              EitherT(Eval.defer {
                insertArrayTable(StandardTable(Map.empty), Nil, values, cumulPath :+ hd)
                  .map(v => TableArray(NonEmptyChain(v.asInstanceOf[StandardTable])))
                  .value
              }).map(v => value.updated(value.value + (hd -> v)))
            }
            case _ => EitherT.leftT(s"Can't modify a defined table at: ${cumulPath.append(hd).mkString_(",")}")
          }
        case hd :: tl => {
          val updatedValue: Eval[Either[String, TValue]] = Eval.defer {
            value.value.getOrElse(hd, IntermediateTable(Map.empty)) match {
              case subtable: IntermediateTable => insertArrayTable(subtable, tl, values, cumulPath :+ hd).value
              case subtable: StandardTable     => insertArrayTable(subtable, tl, values, cumulPath :+ hd).value
              case subarray: TableArray => {
                val (init, last) = subarray.tables.initLast
                insertArrayTable(last, tl, values, cumulPath :+ hd)
                  .map(v => TableArray(NonEmptyChain.fromChainAppend(init, v.asInstanceOf[StandardTable])))
                  .value
              }
              case _: InlineTable =>
                Eval.now(Left(s"Can't modify an inline table at: ${cumulPath.append(hd).mkString_(",")}"))
              case _ =>
                Eval.now(Left(s"Can't modify a defined value at: ${cumulPath.append(hd).mkString_(",")}"))
            }
          }
          EitherT(updatedValue).map(v => value.updated(value.value.updated(hd, v)))
        }
      }

      def insertValue(
          value: TValue,
          keys: NonEmptyList[String],
          newVal: TValue,
          cumulPath: Chain[String]
      ): EitherT[Eval, String, TValue] = {
        keys match {
          case NonEmptyList(hd, Nil) =>
            value match {
              case IntermediateTable(table) =>
                if (table.contains(hd))
                  EitherT.leftT(s"Can't modify a defined value at: ${cumulPath.append(hd).mkString_(".")}")
                else EitherT.rightT(StandardTable(table + (hd -> newVal)))
              case StandardTable(table) =>
                if (table.contains(hd))
                  EitherT.leftT(s"Can't modify a defined value at: ${cumulPath.append(hd).mkString_(".")}")
                else EitherT.rightT(StandardTable(table + (hd -> newVal)))
              case TableArray(tables) => {
                val (init, StandardTable(last)) = tables.initLast
                if (last.contains(hd))
                  EitherT.leftT(s"Can't modify a defined value at: ${cumulPath.append(hd).mkString_(".")}")
                else
                  EitherT.rightT(TableArray(NonEmptyChain.fromChainAppend(init, StandardTable(last + (hd -> newVal)))))
              }
              case _ => EitherT.leftT(s"Can't modify a defined value at: ${cumulPath.append(hd).mkString_(".")}")
            }
          case NonEmptyList(hd, hl :: tl) =>
            value match {
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
                ).map(v => StandardTable(table.updated(hd, v)))
              )
              case StandardTable(table) => (
                EitherT(
                  Eval.defer(
                    insertValue(
                      table.getOrElse(hd, StandardTable(Map.empty)),
                      NonEmptyList(hl, tl),
                      newVal,
                      cumulPath :+ hd
                    ).value
                  )
                ).map(v => StandardTable(table.updated(hd, v)))
              )
              case TableArray(tables) => {
                val (init, StandardTable(last)) = tables.initLast
                EitherT(
                  Eval.defer(
                    insertValue(
                      last.getOrElse(hd, StandardTable(Map.empty)),
                      NonEmptyList(hl, tl),
                      newVal,
                      cumulPath :+ hd
                    ).value
                  )
                ).map(v => TableArray(NonEmptyChain.fromChainAppend(init, StandardTable(last.updated(hd, v)))))
              }
              case _ => EitherT.leftT(s"Can't modify a defined value at: ${cumulPath.append(hd).mkString_(",")}")
            }
        }
      }

      table match {
        case StdTable(keys, values) =>
          insertStdTable(value, keys, values, Chain.empty).map(x => IntermediateTable(x.value))
        case ArrTable(keys, values) =>
          insertArrayTable(value, keys.toList, values, Chain.empty).map(x => IntermediateTable(x.value))
      }
    }

    toml
      .map(foldExpressions(_))
      .map(tables => tables.foldM(IntermediateTable(Map.empty))((value, table) => insertTable(value, table)))
      .flatMap(result => result.value.value.fold(e => Parser.failWith(e), v => Parser.pure(v)))
  }
}

sealed trait PToml
case class StdTable(keys: List[String], values: Chain[(NonEmptyList[String], TValue)] = Chain()) extends PToml
case class ArrTable(keys: NonEmptyList[String], values: Chain[(NonEmptyList[String], TValue)] = Chain()) extends PToml
