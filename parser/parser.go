package parser

import (
	"go-interpreter/ast"
	"go-interpreter/lexer"
	"go-interpreter/token"
)

// Parser has three fields: 'l' which is a pointer to an instance
// of the lexer. 'curToken' and 'peekToken' which act exactly like
// the two 'pointers' our lexer has: 'position' and 'readPosition'.
// But instead of pointing to a character in the input, they point
// to the current and next token.
type Parser struct {
	l *lexer.Lexer

	curToken token.Token	// current token under examination
	peekToken token.Token	// next token after the current token
}

// New initialize a *Parser with a given lexer instance, and
// reads two tokens so curToken and peekToken are both set.
func New(l *lexer.Lexer) *Parser {
	p := &Parser{l: l}

	// Read two tokens, so curToken and peekToken are both set
	p.nextToken()
	p.nextToken()

	return p
}

// nextToken sets the current token under examination to the next
// token, and sets the next token to the lexer's next available token.
func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) ParseProgram() *ast.Program {
	return nil
}
