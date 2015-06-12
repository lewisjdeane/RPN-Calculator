import java.util.Stack

object RPN {
	def main(args: Array[String]): Unit = {
		println("\nType 'quit' to leave.\n")

		var input: String = readLine("Enter mathematical expression: ")

		while (input != "quit") {
			println(new RPN(input).calculate)
			input = readLine("\nEnter mathematical expression: ")
		}
	}
}

class RPN(_input: String) {
	var input: String = _input
	var stack: Stack[Token] = new Stack()

	def calculate(): String = {
		var RPNTokenisedArray: Array[Token] = getTokenisedArray
		var output: String = ""

		var validRPN: Boolean = true

		for (t <- RPNTokenisedArray) {
			if (t.tokenType == TokenType.Invalid) {
				validRPN = false
				output = "'" +t.value + "'" + " is not valid syntax in the mathematical expression given."
			}
		}

		var hasErrors: Boolean = false

		if (validRPN) {
			for (token <- RPNTokenisedArray; if (!hasErrors)) token.tokenType match {
				case TokenType.Invalid => hasErrors = true
				case TokenType.Number => stack.push(token)
				case TokenType.Operator => {
					var operator: Operator = new Operator(token.value)
					var opParams: Int = operator.params

					if (opParams <= stack.size()) {
						var tokenParams: Array[Token] = new Array[Token](opParams)

						for (i <- 0 until opParams) tokenParams(i) = stack.pop()

						stack.push(new Token(operator.apply(tokenParams).toString))
					} else {
						hasErrors = true
						output = "Insufficient parameters for " + token.value + ". Operators require two parameters."
					}
				}
				case TokenType.Function => {
					var function: Function = new Function(token.value)
					var funcParams: Int = function.params

					if (funcParams <= stack.size()) {
						var tokenParams: Array[Token] = new Array[Token](funcParams)

						for (i <- 0 until funcParams) tokenParams(i) = stack.pop()

						stack.push(new Token(function.apply(tokenParams).toString))
					} else {
						hasErrors = true
						output = "Insufficient parameters for " + token.value + ". Functions require one parameter."
					}
				}
			}

			if (!hasErrors) {
				if (stack.size() > 1) output = "Not enough operators entered for the expression given."
				else output = stack.peek.value
			}
		}

		output
	}

	private def getTokenisedArray: Array[Token] = input.split(" ").map(x => new Token(x))
}