## TomlS

A toml parsing library in Scala 3 developped with cats-parse

基于cats-parse的Scala 3写的Toml解析器

支持 Toml v1.0 compliant

### [WIP] Usage 使用

/!\ Not published yet 尚未发布

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

Parser 解析器：

```scala
object Toml {
  val parser: Parser0[TObject]
}
```

### TODO 待做

- Implement show 实现导出功能
- Publish 发布
- Test compliance 测试兼容性 (Waiting for [official compliance test](https://github.com/toml-lang/compliance) 等待[官方测试](https://github.com/toml-lang/compliance))
- Test performance 测试性能
- Extra functionalities 额外功能