package evaluator

import (
	"go-interpreter/ast"
	"go-interpreter/object"
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
