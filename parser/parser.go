package parser

import (
	"fmt"
	"go-interpreter/ast"
	"go-interpreter/lexer"
	"go-interpreter/token"
	"strconv"
)

// The following constants get assigned the values 1 to 7. Which will
// be later used to deterime which operator has precedence over another.
const (
	_ int = iota
	LOWEST
	EQUALS      // ==
	LESSGREATER // > or <
	SUM         // +
	PRODUCT     // *
	PREFIX      // -X or !X
	CALL        // myFunction(x)
	INDEX       // array[index]
)

// precedences associates token types with their precedence.
var precedences = map[token.TokenType]int{
	token.EQ:       EQUALS,
	token.NOT_EQ:   EQUALS,
	token.LT:       LESSGREATER,
	token.GT:       LESSGREATER,
	token.PLUS:     SUM,
	token.MINUS:    SUM,
	token.SLASH:    PRODUCT,
	token.ASTERISK: PRODUCT,
	token.LPAREN:   CALL,
	token.LBRACKET: INDEX,
}

// Both functions return an ast.Expression but only infixParseFn takes an argument,
// which is another ast.Expression. This argument is "left side" of the infix operator
// that is being parsed.
type (
	// prefixParseFn gets called when we encounter the associated token type
	// in a prefix position. Example: ++1.
	prefixParseFn func() ast.Expression

	// infixParseFn gets called when when we encounter the associated token type
	// in an infix position. Example: 1 + 1.
	infixParseFn func(ast.Expression) ast.Expression
)

// Parser has four fields: 'l' which is a pointer to an instance
// of the lexer. 'errors' which holds possible parsing errors.
// 'curToken' and 'peekToken' which act exactly like the two 'pointers'
// our lexer has: 'position' and 'readPosition'. But instead of pointing
// to a character in the input, they point to the current and next token.
type Parser struct {
	l      *lexer.Lexer
	errors []string

	curToken  token.Token // current token under examination
	peekToken token.Token // next token after the current token

	prefixParseFns map[token.TokenType]prefixParseFn
	infixParseFns  map[token.TokenType]infixParseFn
}

// New initialize a *Parser with a given lexer instance, and
// reads two tokens so curToken and peekToken are both set.
func New(l *lexer.Lexer) *Parser {
	p := &Parser{
		l:      l,
		errors: []string{},
	}

	p.prefixParseFns = make(map[token.TokenType]prefixParseFn)
	p.registerPrefix(token.IDENT, p.parseIdentifier)
	p.registerPrefix(token.INT, p.parseIntegerLiteral)
	p.registerPrefix(token.BANG, p.parsePrefixExpression)
	p.registerPrefix(token.MINUS, p.parsePrefixExpression)
	p.registerPrefix(token.TRUE, p.parseBoolean)
	p.registerPrefix(token.FALSE, p.parseBoolean)
	p.registerPrefix(token.LPAREN, p.parseGroupedExpression)
	p.registerPrefix(token.IF, p.parseIfExpression)
	p.registerPrefix(token.FUNCTION, p.parseFunctionLiteral)
	p.registerPrefix(token.STRING, p.parseStringLiteral)
	p.registerPrefix(token.LBRACKET, p.parseArrayLiteral)

	p.infixParseFns = make(map[token.TokenType]infixParseFn)
	p.registerInfix(token.EQ, p.parseInfixExpression)
	p.registerInfix(token.NOT_EQ, p.parseInfixExpression)
	p.registerInfix(token.LT, p.parseInfixExpression)
	p.registerInfix(token.GT, p.parseInfixExpression)
	p.registerInfix(token.PLUS, p.parseInfixExpression)
	p.registerInfix(token.MINUS, p.parseInfixExpression)
	p.registerInfix(token.SLASH, p.parseInfixExpression)
	p.registerInfix(token.ASTERISK, p.parseInfixExpression)
	p.registerInfix(token.LPAREN, p.parseCallExpression)
	p.registerInfix(token.LBRACKET, p.parseIndexExpression)

	// Read two tokens, so curToken and peekToken are both set
	p.nextToken()
	p.nextToken()

	return p
}

// Errors returns a slice of strings that can contain parsing errors.
func (p *Parser) Errors() []string {
	return p.errors
}

// registerPrefix is a helper method to add entries to the prefixParseFns map.
func (p *Parser) registerPrefix(tokenType token.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

// registerInfix is a helper method to add entries to the infixParseFns map.
func (p *Parser) registerInfix(tokenType token.TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

// nextToken sets the current token under examination to the next
// token, and sets the next token to the lexer's next available token.
func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

// parseIdentifier returns the current token in the Token field and the literal
// value of the token in the Value field.
func (p *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
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
		return p.parseExpressionStatement()
	}
}

// parseLetStatement constructs an *ast.LetStatement node with the token it's
// currently sitting on (token.LET) and then advances the tokens while making
// assertions to the next token with calls to expectPeek. First it expects a token.IDENT
// token, which it then uses to construct an *ast.Identifier node. Then it expects
// a token.ASSIGN and finally goes over the expression following the token.ASSIGN
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

	p.nextToken()
	stmt.Value = p.parseExpression(LOWEST)

	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

// parseReturnStatement constructs an *ast.ReturnStatement node with the current token
// it's sitting on (token.RETURN). It then brings the parser in place for the expression
// that comes next by calling nextToken() and finally goes over the expression until
// it encounters a token.SEMICOLON.
func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{Token: p.curToken}

	p.nextToken()
	stmt.ReturnValue = p.parseExpression(LOWEST)

	for p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

// parseExpressionStatement constructs an *ast.ExpressionStatement node with the current token
// it's sitting on. It then calls parseExpression with the constant LOWEST and checks for an
// optional semicolon, if it encounters one it sets it as the curToken, if not we continue as normal.
func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	// Only for debugging purposes
	//defer untrace(trace("parseExpressionStatement"))

	stmt := &ast.ExpressionStatement{Token: p.curToken}

	stmt.Expression = p.parseExpression(LOWEST)

	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

// noPrefixParseFnError is a small helper method that adds a formatted error message to our
// parser's error field.
func (p *Parser) noPrefixParseFnError(t token.TokenType) {
	msg := fmt.Sprintf("no prefix parse function for %s found", t)
	p.errors = append(p.errors, msg)
}

// parseExpression checks whether or not we have a parsing function associated
// to p.curToken.Type in the prefix position. If we do it calls the parsing fuction.
// If not it returns nil. Then in the loop's body the method tries to find infixParseFns
// for the next token. If it finds such a function, it calls it, passing the expression
// returned by prefixParseFns as an argument. And does this again and again until it
// encounters a token that has a lower precedence.
func (p *Parser) parseExpression(precedence int) ast.Expression {
	// Only for debugging purposes
	//defer untrace(trace("parseExpression"))

	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.curToken.Type)
		return nil
	}

	leftExp := prefix()

	for !p.peekTokenIs(token.SEMICOLON) && precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp
		}

		p.nextToken()
		leftExp = infix(leftExp)
	}

	return leftExp
}

// parseIntegerLiteral constructs an *ast.IntegerLiteral node with the current token
// it's sitting on. It then calls strconv.ParseInt to convert the current token literal
// to an int64 and returns the previously constructed node.
func (p *Parser) parseIntegerLiteral() ast.Expression {
	// Only for debugging purposes
	//defer untrace(trace("parseIntegerLiteral"))

	lit := &ast.IntegerLiteral{Token: p.curToken}

	value, err := strconv.ParseInt(p.curToken.Literal, 0, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as integer", p.curToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}

	lit.Value = value
	return lit
}

// parseStringLiteral constructs a *ast.StringLiteral node with the current token it's
// sitting on, and sets the Value to the current token literal.
func (p *Parser) parseStringLiteral() ast.Expression {
	// Only for debugging purposes
	//defer untrace(trace("parseStringLiteral"))
	return &ast.StringLiteral{Token: p.curToken, Value: p.curToken.Literal}
}

// parseArrayLiteral constructs a *ast.ArrayLiteral node with the current token it's
// sitting on, and calls p.parseExpressionList to parse a list of comma separated arguments.
func (p *Parser) parseArrayLiteral() ast.Expression {
	// Only for debugging purposes
	//defer untrace(trace("parseArrayLiteral"))
	array := &ast.ArrayLiteral{Token: p.curToken}
	array.Elements = p.parseExpressionList(token.RBRACKET)
	return array
}

// parseExpressionList constructs a slice of ast.Expression. It starts by checking if the next
// token under examination is the given end token.TokenType. If it is returns an empty ast.Expression
// slice. Otherwise it advances the tokens and uses the p.parseExpression to append a new argument to
// the ast.Expression slice. Then checks whether the next token is a token.COMMA. If it is starts a
// loop, advances the tokens, and uses p.parseExpression to append a new argument to the ast.Expression
// slice. If the condition of the loop evaluates to false, it checks that the next token is the given
// end token.TokenType and returns either nil or the ast.Expression slice.
func (p *Parser) parseExpressionList(end token.TokenType) []ast.Expression {
	list := []ast.Expression{}

	if p.peekTokenIs(end) {
		p.nextToken()
		return list
	}

	p.nextToken()
	list = append(list, p.parseExpression(LOWEST))

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		p.nextToken()
		list = append(list, p.parseExpression(LOWEST))
	}

	if !p.expectPeek(end) {
		return nil
	}

	return list
}

// parseBoolean constructs an *ast.Boolean node with the current token it's sitting on.
// It then uses the p.curTokenIs method inside the *ast.Boolean Value field to determine
// whether it is TRUE or FALSE.
func (p *Parser) parseBoolean() ast.Expression {
	// Only for debugging purposes
	//defer untrace(trace("parseBoolean"))
	return &ast.Boolean{Token: p.curToken, Value: p.curTokenIs(token.TRUE)}
}

// parseGroupedExpression advances the current token and checks if the next token under
// examination is not a token.RPAREN, if it is returns the expression otherwise returns nil.
func (p *Parser) parseGroupedExpression() ast.Expression {
	// Only for debugging purposes
	// defer untrace(trace("parseGroupedExpression"))
	p.nextToken()
	exp := p.parseExpression(LOWEST)

	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	return exp
}

// parseIfExpression constructs an *ast.IfExpression node with the current token it's sitting on.
// It uses the expectPeek method extensively to jump through the tokens and check whether they are
// the expected ones or not. Finally it advances just enough so that parseBlockStatement sits on the
// token.LBRACE. It also allows an optional token.ELSE but doesn't add a parser error if there is none.
func (p *Parser) parseIfExpression() ast.Expression {
	// Only for debugging purposes
	// defer untrace(trace("parseIfExpression"))
	expression := &ast.IfExpression{Token: p.curToken}

	if !p.expectPeek(token.LPAREN) {
		return nil
	}

	p.nextToken()
	expression.Condition = p.parseExpression(LOWEST)

	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	if !p.expectPeek(token.LBRACE) {
		return nil
	}

	expression.Consequence = p.parseBlockStatement()

	if p.peekTokenIs(token.ELSE) {
		p.nextToken()

		if !p.expectPeek(token.LBRACE) {
			return nil
		}

		expression.Alternative = p.parseBlockStatement()
	}

	return expression
}

// parseFunctionLiteral constructs *ast.FunctionLiteral node with the current token it's sitting on.
// It uses the method p.parseFunctionParameters to construct a slice of ast.Identifier, and the
// p.parseBlockStatement method to construct an ast.BlockStatement.
func (p *Parser) parseFunctionLiteral() ast.Expression {
	lit := &ast.FunctionLiteral{Token: p.curToken}

	if !p.expectPeek(token.LPAREN) {
		return nil
	}

	lit.Parameters = p.parseFunctionParameters()

	if !p.expectPeek(token.LBRACE) {
		return nil
	}

	lit.Body = p.parseBlockStatement()
	return lit
}

// parseFunctionParameters constructs a slice of ast.Identifier. It starts by checking if the
// next token under examination is a token.RPAREN. If it is returns an empty ast.Identifier slice.
// Otherwise it advances the tokens and builds a new ast.Identifier with the current token under examinationm
// and appends it to the ast.Identifier slice. Then checks whether the next token is a token.COMMA. If it is
// starts a loop, advances the tokens, and builds and appends a new ast.Identifier to the ast.Identifier slice.
// If the condition of the loop evaluates to false, it checks that the next token is a token.RPAREN, and returns
// either nil or the ast.Identifier slice.
func (p *Parser) parseFunctionParameters() []*ast.Identifier {
	identifiers := []*ast.Identifier{}

	if p.peekTokenIs(token.RPAREN) {
		p.nextToken()
		return identifiers
	}

	p.nextToken()

	ident := &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
	identifiers = append(identifiers, ident)

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		p.nextToken()
		ident := &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
		identifiers = append(identifiers, ident)
	}

	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	return identifiers
}

// parseBlockStatement constructs an *ast.BlockStatement node with the current token it's sitting on.
// It calls parseStatement until it encounters either a token.RBRACE which signifies the end of a
// block statement or a token.EOF, which tells us that there's no more tokens left to parse.
func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{Token: p.curToken}
	block.Statements = []ast.Statement{}

	p.nextToken()

	for !p.curTokenIs(token.RBRACE) && !p.curTokenIs(token.EOF) {
		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
		p.nextToken()
	}

	return block
}

// parseCallExpression receives the already parsed function as argument and uses
// it to construct a *ast.CallExpression node. It calls p.parseCallArguments to
// parse the argument list.
func (p *Parser) parseCallExpression(function ast.Expression) ast.Expression {
	exp := &ast.CallExpression{Token: p.curToken, Function: function}
	exp.Arguments = p.parseExpressionList(token.RPAREN)
	return exp
}

func (p *Parser) parseIndexExpression(left ast.Expression) ast.Expression {
	exp := &ast.IndexExpression{Token: p.curToken, Left: left}

	p.nextToken()
	exp.Index = p.parseExpression(LOWEST)

	if !p.expectPeek(token.RBRACKET) {
		return nil
	}

	return exp
}

// parsePrefixExpression constructs an *ast.PrefixExpression node with the current token
// it's sitting on. Then advances the current token and calls parseExpression with the prefix
// precedence, parseExpression then checks the registered prefix parsing functions and finds
// parseIntegerLiteral, which builds an *ast.IntegerLiteral node and returns it. parseExpression
// returns this newly constructed node and parsePrefixExpression uses it to fill the Right field
// of *ast.PrefixExpression.
func (p *Parser) parsePrefixExpression() ast.Expression {
	// Only for debugging purposes
	//defer untrace(trace("parsePrefixExpression"))

	expression := &ast.PrefixExpression{Token: p.curToken, Operator: p.curToken.Literal}

	p.nextToken()

	expression.Right = p.parseExpression(PREFIX)
	return expression
}

// parseInfixExpression uses an ast.Expression argument to construct an *ast.InfixExpression node
// with the argument being in the Left field. Then it assigns the precedence of the current token
// before advancing the tokens by calling nextToken and filling the Right field of the node with
// another call to parseExpression.
func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	// Only for debugging purposes
	//defer untrace(trace("parseInfixExpression"))

	expression := &ast.InfixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Left:     left,
	}

	precedence := p.curPrecedence()
	p.nextToken()
	expression.Right = p.parseExpression(precedence)

	return expression
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
	}

	p.peekError(t)
	return false
}

// peekPrecedence returns the precedence associated with the token type
// of p.peekToken. If it doesn't find a precedence it defaults to LOWEST.
func (p *Parser) peekPrecedence() int {
	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}

	return LOWEST
}

// curPrecedence returns the precedence associated with the token type
// of p.curToken. If it doesn't find a precedence it defaults to LOWEST.
func (p *Parser) curPrecedence() int {
	if p, ok := precedences[p.curToken.Type]; ok {
		return p
	}

	return LOWEST
}

// peekError appends an error message to the parser errors string slice.
func (p *Parser) peekError(t token.TokenType) {
	msg := fmt.Sprintf("expected next token to be %s, got %s instead", t, p.peekToken.Type)
	p.errors = append(p.errors, msg)
}
