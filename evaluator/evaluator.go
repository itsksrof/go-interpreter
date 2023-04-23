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

// nativeBoolToBooleanObject returns an *object.Boolean with its value set to TRUE or FALSE
// depending on the given input.
func nativeBoolToBooleanObject(input bool) *object.Boolean {
	if input {
		return TRUE
	}

	return FALSE
}
