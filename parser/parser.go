package parser

import (
	"fmt"
	"go-interpreter/ast"
	"go-interpreter/lexer"
	"go-interpreter/token"
)

// Parser has four fields: 'l' which is a pointer to an instance
// of the lexer. 'errors' which holds possible parsing errors.
// 'curToken' and 'peekToken' which act exactly like the two 'pointers'
// our lexer has: 'position' and 'readPosition'. But instead of pointing
// to a character in the input, they point to the current and next token.
type Parser struct {
	l		*lexer.Lexer
	errors	[]string

	curToken	token.Token	// current token under examination
	peekToken	token.Token	// next token after the current token
}

// New initialize a *Parser with a given lexer instance, and
// reads two tokens so curToken and peekToken are both set.
func New(l *lexer.Lexer) *Parser {
	p := &Parser{
		l: l,
		errors: []string{},
	}

	// Read two tokens, so curToken and peekToken are both set
	p.nextToken()
	p.nextToken()

	return p
}

// Errors returns a slice of strings that can contain parsing errors.
func (p *Parser) Errors() []string {
	return p.errors
}

// nextToken sets the current token under examination to the next
// token, and sets the next token to the lexer's next available token.
func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

// ParseProgram first constructs the root node of the AST, then iterates
// over every token in the input until it encounters an EOF token. It does
// this repeatedly by calling nextToken which advances both p.curToken and p.peekToken.
// In every iteration it calls parseStatement whose job is to parse a statement. If
// parseStatement returned something other than nil, its return value is added to the
// Statements slice of the AST root node. When nothing is left to parse the root node
// is returned.
func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for !p.curTokenIs(token.EOF) {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		p.nextToken()
	}

	return program
}

// parseStatement determines which parsing function needs to be executed,
// based on the type of the current token under examination. If the current
// token under examination does not match any of the possible cases nil is returned.
func (p *Parser) parseStatement() ast.Statement {
	switch p.curToken.Type {
		case token.LET:
			return p.parseLetStatement()
		case token.RETURN:
			return p.parseReturnStatement()
		default:
			return nil
	}
}

// parseLetStatement constructs an *ast.LetStatement node with the token it's
// currently sitting on (token.LET) and then advances the tokens while making
// assertions to the next token with calls to expectPeek. First it expects a token.IDENT
// token, which it then uses to construct an *ast.Identifier node. Then it expects
// a token.ASSIGN and finally jumps over the expression following the token.ASSIGN
// until it encounters a token.SEMICOLON.
func (p *Parser) parseLetStatement() *ast.LetStatement {
	stmt := &ast.LetStatement{Token: p.curToken}

	if !p.expectPeek(token.IDENT) {
		return nil
	}

	stmt.Name = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	if !p.expectPeek(token.ASSIGN) {
		return nil
	}

	if !p.curTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

// parseReturnStatement constructs an *ast.ReturnStatement node with the current token
// it's sitting on (token.RETURN). It then brings the parser in place for the expression
// that comes next by calling nextToken() and finally jumps over the expression until
// it encounters a token.SEMICOLON.
func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{Token: p.curToken}

	p.nextToken()

	for !p.curTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

// curTokenIs compares the type of the current token under examination
// against the passed token type.
func (p *Parser) curTokenIs(t token.TokenType) bool {
	return p.curToken.Type == t
}

// peekTokenIs compares the type of the next token under examination
// against the passed token type.
func (p *Parser) peekTokenIs(t token.TokenType) bool {
	return p.peekToken.Type == t
}

// expectPeek checks the type of the peekToken and only if the type is
// correct does it advance the tokens by calling nextToken.
func (p *Parser) expectPeek(t token.TokenType) bool {
	if p.peekTokenIs(t) {
		p.nextToken()
		return true
	} else {
		p.peekError(t)
		return false
	} 
}

// peekError appends an error message to the parser errors string slice.
func (p *Parser) peekError(t token.TokenType) {
	msg := fmt.Sprintf("expected next token to be %s, got %s instead", t, p.peekToken.Type)
	p.errors = append(p.errors, msg)
}