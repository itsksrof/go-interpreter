package ast

import (
	"bytes"
	"go-interpreter/token"
)

type Node interface {
	TokenLiteral() string
	String() string
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

// String creates a buffer and writes the return value of each
// statement's String() method to it. And then it returns the buffer
// as a string.
func (p *Program) String() string {
	var out bytes.Buffer

	for _, s := range p.Statements {
		out.WriteString(s.String())
	}

	return out.String()
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
func (ls *LetStatement) String() string {
	var out bytes.Buffer

	out.WriteString(ls.TokenLiteral() + " ")
	out.WriteString(ls.Name.String())
	out.WriteString(" = ")

	if ls.Value != nil {
		out.WriteString(ls.Value.String())
	}

	out.WriteString(";")
	return out.String()
}

// Identifier represents the name of a binded variable for later reuse.
type Identifier struct {
	Token token.Token // the token.IDENT token
	Value string
}

func (i *Identifier) expressionNode() {}
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }
func (i *Identifier) String() string { return i.Value }

// ReturnStatement represents solely the keyword return and an expression.
type ReturnStatement struct {
	Token		token.Token // the token.RETURN token
	ReturnValue Expression
}

func (rs *ReturnStatement) statementNode() {}
func (rs *ReturnStatement) TokenLiteral() string { return rs.Token.Literal }
func (rs *ReturnStatement) String() string {
	var out bytes.Buffer

	out.WriteString(rs.TokenLiteral() + " ")

	if rs.ReturnValue != nil {
		out.WriteString(rs.ReturnValue.String())
	}

	out.WriteString(";")
	return out.String()
}

// ExpressionStatement it's only a wrapper that consists solely of one expression.
type ExpressionStatement struct {
	Token		token.Token // the first token of the expression
	Expression	Expression
}

func (es *ExpressionStatement) statementNode() {}
func (es *ExpressionStatement) TokenLiteral() string { return es.Token.Literal }
func (es *ExpressionStatement) String() string {
	if es.Expression != nil {
		return es.Expression.String()
	}	

	return ""
}

// IntegerLiteral fullfills the ast.Expression interface and has as a Value
// an int64 instead of a string.
type IntegerLiteral struct {
	Token token.Token
	Value int64
}

func (il *IntegerLiteral) expressionNode() {}
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }
func (il *IntegerLiteral) String() string { return il.Token.Literal }

// PrefixExpression has two noteworthy fields. The first being Operator which is
// a string that is going to contain either "-" or "!". And the second being Right
// which contains the expression to the right of the operator.
type PrefixExpression struct {
	Token		token.Token // the prefix token, e.g. !
	Operator	string
	Right		Expression
}

func (pe *PrefixExpression) expressionNode() {}
func (pe *PrefixExpression) TokenLiteral() string { return pe.Token.Literal }
func (pe *PrefixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(pe.Operator)
	out.WriteString(pe.Right.String())
	out.WriteString(")")

	return out.String()
}

// InfixExpression has three noteworthy fields. The first being Left which contains the
// expression to the left of the operator. The second being the operator itself which is
// a string that can contain operators such as "+" or "!=". And the third one being
// Right which contains the expression to the right of the operator.
type InfixExpression struct {
	Token		token.Token // the operator token, e.g. +
	Left		Expression
	Operator	string
	Right		Expression
}

func (ie *InfixExpression) expressionNode() {}
func (ie *InfixExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *InfixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(ie.Left.String())
	out.WriteString(" " + ie.Operator + " ")
	out.WriteString(ie.Right.String())
	out.WriteString(")")

	return out.String()
}

// Boolean fullfills the ast.Expression interface and has as a Value
// a boolean instead of a string.
type Boolean struct {
	Token token.Token
	Value bool
}

func (b *Boolean) expressionNode() {}
func (b *Boolean) TokenLiteral() string { return b.Token.Literal }
func (b *Boolean) String() string { return b.Token.Literal }

// IfExpression fullfills the ast.Expression interface and has three noteworthy fields
// that allow the AST to represent an if-else-conditional. The Condition field which can
// be any expression, the Consequence which points to the consequence of the condition,
// and then Alternative which also points to the consequence of the condition.
type IfExpression struct {
	Token			token.Token // the 'if' token
	Condition		Expression
	Consequence		*BlockStatement
	Alternative		*BlockStatement
}

func (ie *IfExpression) expressionNode() {}
func (ie *IfExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *IfExpression) String() string {
	var out bytes.Buffer

	out.WriteString("if")
	out.WriteString(ie.Condition.String())
	out.WriteString(" ")
	out.WriteString(ie.Consequence.String())

	if ie.Alternative != nil {
		out.WriteString("else ")
		out.WriteString(ie.Alternative.String())
	}

	return out.String()
}

// BlockStatement fullfills the ast.Expression interface and represents
// a series of statements.
type BlockStatement struct {
	Token		token.Token // the '{' token
	Statements	[]Statement
}

func (bs *BlockStatement) expressionNode() {}
func (bs *BlockStatement) TokenLiteral() string { return bs.Token.Literal }
func (bs *BlockStatement) String() string {
	var out bytes.Buffer

	for _, s := range bs.Statements {
		out.WriteString(s.String())
	}

	return out.String()
}
