class Token(val value: String)  {
	def tokenType: TokenType.Value = Token.getTokenType(value)
}

object Token {
	private var operators: List[String] = List("+", "-", "*", "/", "^")

	def getTokenType(value: String): TokenType.Value = {
		if (isNumber(value)) TokenType.Number
		else if (isOperator(value)) TokenType.Operator
		else if (value == "(" || value == ")") TokenType.Bracket
		else if (isFunction(value)) TokenType.Function
		else TokenType.Invalid
	}

	private def isNumber(str: String): Boolean = {
		if (str == ".") true
		else {
			try {
				str.toDouble
				true
			} catch {
				case e: NumberFormatException => false
			}
		}
	}

	private def isOperator(str: String): Boolean = operators.contains(str)

	private def isFunction(str: String): Boolean = str match {
		case "sin" | "cos" | "tan" | "ln" | "log" => true
		case _ => false
	}
}