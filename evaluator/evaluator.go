package evaluator

import (
	"go-interpreter/ast"
	"go-interpreter/object"
)

var (
	NULL	= &object.Null{}
	TRUE	= &object.Boolean{Value: true}
	FALSE	= &object.Boolean{Value: false}
)

// Eval uses the ast.Node to travers the AST. It starts at the top, receiving an *ast.Program.
// Then it traverses every node that it's in it.
func Eval(node ast.Node) object.Object {
	switch node := node.(type) {
	case *ast.Program:
		return evalStatements(node.Statements)
	case *ast.ExpressionStatement:
		return Eval(node.Expression)
	case *ast.IntegerLiteral:
		return &object.Integer{Value: node.Value}
	case *ast.Boolean:
		return nativeBoolToBooleanObject(node.Value)
	case *ast.PrefixExpression:
		right := Eval(node.Right)
		return evalPrefixExpression(node.Operator, right)
	case *ast.InfixExpression:
		left := Eval(node.Left)
		right := Eval(node.Right)
		return evalInfixExpression(node.Operator, left, right)
	case *ast.BlockStatement:
		return evalStatements(node.Statements)
	case *ast.IfExpression:
		return evalIfExpression(node)
	}

	return nil
}

// evalStatements loops through the given slice of statements, and calls Eval to determine
// the type of node and return an object.Object representation of it.
func evalStatements(stmts []ast.Statement) object.Object {
	var result object.Object
	for _, stmt := range stmts {
		result = Eval(stmt)
	}

	return result
}

// evalPrefixExpression uses the given operator to determine which eval function to use.
func evalPrefixExpression(operator string, right object.Object) object.Object {
	switch operator {
	case "!":
		return evalBangOperatorExpression(right)
	case "-":
		return evalMinusOperatorExpression(right)
	default:
		return NULL
	}
}

// evalBangOperatorExpression uses the given object to negate it.  
func evalBangOperatorExpression(right object.Object) object.Object {
	switch right {
	case TRUE:
		return FALSE
	case FALSE:
		return TRUE
	case NULL:
		return TRUE
	default:
		return FALSE
	}
}

// evalMinusOperatorExpression checks if the operand is an integer. If it isn't,
// returns NULL. But if it is, we extract the value of the *object.Integer. Then
// allocates a new object to wrap the negated version of the value.
func evalMinusOperatorExpression(right object.Object) object.Object {
	if right.Type() != object.INTEGER_OBJ {
		return NULL
	}

	value := right.(*object.Integer).Value
	return &object.Integer{Value: -value}
}

// evalInfixExpression checks whether or not the left and right operands are integers.
// If they are calls evalIntegerInfixExpression, otherwise returns NULL.
func evalInfixExpression(operator string, left, right object.Object) object.Object {
	switch {
	case left.Type() == object.INTEGER_OBJ && right.Type() == object.INTEGER_OBJ:
		return evalIntegerInfixExpression(operator, left, right)
	case operator == "==":
		return nativeBoolToBooleanObject(left == right)
	case operator == "!=":
		return nativeBoolToBooleanObject(left != right)
	default:
		return NULL
	}
}

// evalIntegerInfixExpression checks whether or not the left and right operands are integers.
// It then uses the given operator to perform the following arithmetic operations: 'addition',
// 'subtraction', 'multiplication' and 'division'. Or the following relational operations: 'less-than',
// 'greater-than', 'equal', 'not-equal'.
func evalIntegerInfixExpression(operator string, left, right object.Object) object.Object {
	leftVal := left.(*object.Integer).Value
	rightVal := right.(*object.Integer).Value

	switch operator {
	case "+":
		return &object.Integer{Value: leftVal + rightVal}
	case "-":
		return &object.Integer{Value: leftVal - rightVal}
	case "*":
		return &object.Integer{Value: leftVal * rightVal}
	case "/":
		return &object.Integer{Value: leftVal / rightVal}
	case "<":
		return nativeBoolToBooleanObject(leftVal < rightVal) 
	case ">":
		return nativeBoolToBooleanObject(leftVal > rightVal)
	case "==":
		return nativeBoolToBooleanObject(leftVal == rightVal)
	case "!=":
		return nativeBoolToBooleanObject(leftVal != rightVal)
	default:
		return NULL
	}
}

// evalIfExpression first evaluates the condition contained inside the if-expression.
// It then uses isTruthy to determine the result of the given condition, or checks
// to see if there is an else branch. If neither of those are met it returns NULL.
func evalIfExpression(ie *ast.IfExpression) object.Object {
	condition := Eval(ie.Condition)

	if isTruthy(condition) {
		return Eval(ie.Consequence)
	} else if (ie.Alternative) != nil {
		return Eval(ie.Alternative)
	} else {
		return NULL
	}
} 

// isTruthy uses the given object to determine the result of the condition contained
// in the if-else-expression.
func isTruthy(obj object.Object) bool {
	switch obj {
	case NULL:
		return false
	case TRUE:
		return true
	case FALSE:
		return false
	default:
		return true
	}
}

// nativeBoolToBooleanObject returns an *object.Boolean with its value set to TRUE or FALSE
// depending on the given input.
func nativeBoolToBooleanObject(input bool) *object.Boolean {
	if input {
		return TRUE
	}

	return FALSE
}
