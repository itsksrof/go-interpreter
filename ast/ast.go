package ast

import "go-interpreter/token"

type Node interface {
	TokenLiteral() string
}

type Statement interface {
	Node
	statementNode()
}

type Expression interface {
	Node
	expressionNode()
}

type Program struct {
	Statements []Statement
}

// TokenLiteral returns the root node of every AST our parser
// produces. Every valid program is a series of statements.
// These statements are contained in Program.Statements, which
// is just a slice of AST nodes that implement the Statement interface.
func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	} else {
		return ""
	}
}

// LetStatement has 'Name' to hold the identifiers of the binding
// and 'Value' for the expression that produces the value. The two
// methods statementNode and TokenLiteral satisfy the Statement and
// Node interfaces respectively.
type LetStatement struct {
	Token token.Token // the token.LET token
	Name *Identifier
	Value Expression
}

func (ls *LetStatement) statementNode() {}
func (ls *LetStatement) TokenLiteral() string { return ls.Token.Literal }

// Identifier represents the name of a binded variable for later reuse.
type Identifier struct {
	Token token.Token // the token.IDENT token
	Value string
}

func (i *Identifier) expressionNode() {}
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }

// ReturnStatement represents solely the keyword return and an expression.
type ReturnStatement struct {
	Token		token.Token // the token.RETURN token
	ReturnValue Expression
}

func (rs *ReturnStatement) statementNode() {}
func (rs *ReturnStatement) TokenLiteral() string { return rs.Token.Literal }
