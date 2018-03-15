case class CSV(header: Header, rows: Seq[Record])
case class Header(names: Seq[String])
case class Record(fields: Seq[String])


object CSVParser extends Regexparsers {
  
  
  override def skipWhitespace = false
  
  def file = opt(header <~ CRLF) ~ repsep(record,CRLF) <~ opt(CRLF) ^^ {
    case Some(header) ~ records => CSV(header, records)
    case None ~ records         => CSV(Header(List()),records)
  }
  
  def header = repsep(name, comma) ^^ { names => Header(names.map(_.toString))}
  
  def record = repsep(field,comma) ^^ {fields => Record(fields.map(_.toString))}
  
  def name = field
  
  def field = escaped | nonEscaped
  
  def escaped = doubleQuote ~> (textdata | comma | CR | LF | twoDoubleQuote).* <~ doubleQuote ^^ { _.mkString }
  
  def twoDoubleQuote = doubleQuote ~ doubleQuote ^^ { case d1 + d2 => d1 + d2 }
  
  def commma = ","
  
  def CR = "\r"
  
  def doublequote = "\""
  
  def LF = "\n"
  
  def CRLF = CR ~ LF
  
  def textdata = """[\u0020-\u0021\u0023-\u002B\u007e]""".r
  
  def apply(input: String): Either[String, Any] = parseAll(file, input) match {
    case Success(csvData, next)        => Right(csvData)
    case NoSuccess(errorMessage, next) => Left(s"$errorMessage on line ${next.pos.line} oncolumn}")
  }
  
  
}
