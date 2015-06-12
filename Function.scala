class Function(_symbol: String) {
	var symbol: String = _symbol

	def params: Int = symbol match {
		case "sin" | "cos" | "tan" | "ln" | "log" => 1
	}

	def apply(params: Array[Token]): Double = params.length match {
		case 1 => applyFunction1(params(0).value.toDouble)
	}

	def applyFunction1(x: Double) = symbol match {
		case "sin" => Math.sin(x)
		case "cos" => Math.cos(x)
		case "tan" => Math.tan(x)
		case "ln" => Math.log(x)
		case "log" => Math.log10(x)
	}
}
