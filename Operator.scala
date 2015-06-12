class Operator(_symbol: String) {
	var symbol: String = _symbol

	def params: Int = 2

	def leftAssoc: Boolean = symbol match {
		case "^" => false
		case _ => true
	}

	def precedence: Int = symbol match {
		case "+" | "-" => 2
		case "*" | "/" => 3
		case "^" => 4
	}

	def applyOperator(x: Double, y: Double): Double = symbol match {
		case "+" => x + y
		case "*" => x * y
		case "-" => y - x
		case "/" => y / x
		case "^" => Math.pow(y, x)
	}

	def apply(params: Array[Token]): Double = params.length match {
		case 2 => applyOperator(params(0).value.toDouble, params(1).value.toDouble)
		case _ => 0.0
	}
}