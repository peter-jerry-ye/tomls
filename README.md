## TomlS

A toml parsing library in Scala 3 developped with cats-parse

基于cats-parse的Scala 3写的Toml解析器

支持 Toml v1.0 compliant

### Usage 使用

```scala
resolvers += "aoxiang-repo-snapshots" at "https://repo.aoxiang.online/snapshots"
libraryDependencies += "online.aoxiang" % "tomls" % "0.1.0-SNAPSHOT"
```

#### API

AST 基本抽象语法树：

```scala
sealed trait Toml
case class TString(value: String) extends Toml
case class TDouble(value: Double) extends Toml
case class TLong(value: Long) extends Toml
case class TBool(value: Boolean) extends Toml
case class TZonedDateTime(value: ZonedDateTime) extends Toml
case class TLocalDateTime(value: LocalDateTime) extends Toml
case class TLocalDate(value: LocalDate) extends Toml
case class TLocalTime(value: LocalTime) extends Toml
case class TObject(value: Map[String, Toml]) extends Toml
case class TArray(value: List[Toml]) extends Toml
```

Parser and Show 解析器与序列化：

```scala
object Toml {
  val parser: Parser0[TObject]
  extension (t: TObject) def show: String
}
```

### TODO 待做

- Implement pretty show 实现漂亮的导出功能
- Test compliance 测试兼容性 (Waiting for [official compliance test](https://github.com/toml-lang/compliance) 等待[官方测试](https://github.com/toml-lang/compliance))
- Test performance 测试性能
- Extra functionalities 额外功能