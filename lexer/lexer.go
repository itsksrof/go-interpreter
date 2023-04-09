package lexer

import "go-interpreter/token"

type Lexer struct {
	input			string
	position		int		// current position in input (points to current char)
	readPosition	int		// current reading position in input (after current char)
	ch				byte	// current char under examination
}

// New initializes a *Lexer with a given string and calls lexer.readChar
// to either update the current character under examination to the next
// character in the string, or determine if we have reached the end of the string.
func New(input string) *Lexer {
	l := &Lexer{input: input}
	l.readChar()
	return l
}

// NextToken looks at the current character under examination and returns
// a token depending on which character it is. Before returning the token
// we advance our pointers into the string so when we call NextToken() again
// the lexer.ch fields is already updated.
func (l *Lexer) NextToken() token.Token {
	var tok token.Token

	l.skipWhitespace()

	switch l.ch {
	case '=':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.EQ, Literal: literal}
		} else {
			tok = newToken(token.ASSIGN, l.ch)
		}
	case '+':
		tok = newToken(token.PLUS, l.ch)
	case '-':
		tok = newToken(token.MINUS, l.ch)
	case '!':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.NOT_EQ, Literal: literal}
		} else {
			tok = newToken(token.BANG, l.ch)
		}
	case '/':
		tok = newToken(token.SLASH, l.ch)
	case '*':
		tok = newToken(token.ASTERISK, l.ch)
	case '<':
		tok = newToken(token.LT, l.ch)
	case '>':
		tok = newToken(token.GT, l.ch)
	case ';':
		tok = newToken(token.SEMICOLON, l.ch)
	case ',':
		tok = newToken(token.COMMA, l.ch)
	case '(':
		tok = newToken(token.LPAREN, l.ch)
	case ')':
		tok = newToken(token.RPAREN, l.ch)
	case '{':
		tok = newToken(token.LBRACE, l.ch)
	case '}':
		tok = newToken(token.RBRACE, l.ch)
	case 0:
		tok.Literal = ""
		tok.Type = token.EOF
	default:
		if isLetter(l.ch) {
			tok.Literal = l.readIdentifier()
			tok.Type = token.LookupIdent(tok.Literal)
			return tok
		} else if isDigit(l.ch) {
			tok.Type = token.INT
			tok.Literal = l.readNumber()
			return tok
		} else {
			tok = newToken(token.ILLEGAL, l.ch)
		}
	}

	l.readChar()
	return tok
}

// skipWhitespace checks if the current character under examination is a whitespace
// and advances our lexer position until it finds a non-whitespace character. 
func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
		l.readChar()
	}
}

// peekChar reads the next character. If it's empty returns a "NUL"
// ASCII character. If it's not empty returns the character.
func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	} else {
		return l.input[l.readPosition]
	}
}

// readChar first checks whether or not we have reached the end of the string,
// if that's the case sets lexer.ch to 0 which is the ASCII code for the "NUL"
// character, or if we haven't reached the end of the string sets the lexer.ch
// to the next character by using lexer.readPosition.
func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = l.input[l.readPosition]
	}

	l.position = l.readPosition
	l.readPosition += 1
}

// newToken takes a token.TokenType and a character (byte)
// to initialize and return a new token.Token struct.
func newToken(tokenType token.TokenType, ch byte) token.Token {
	return token.Token{Type: tokenType, Literal: string(ch)}
}

// readIdentifier reads an identifier and advances our lexer position
// until it encounters a non-letter-character.
func (l *Lexer) readIdentifier() string {
	position := l.position
	for isLetter(l.ch) {
		l.readChar()
	}

	return l.input[position:l.position]
}

// isLetter checks whether the given argument is a letter or not.
// This function has a huge impact on our interpreter because in here
// we are determining the characters that we want to consider as letters.
//
// For example you can see that we are checking if ch equals '_' meaning
// that we will treat '_' as a letter, which will allow us to declare variable
// names like 'foo_bar'.
func isLetter(ch byte) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

// readNumber reads a number and advances our lexer position until
// it encounters a non-digit-character.
func (l *Lexer) readNumber() string {
	position := l.position
	for isDigit(l.ch) {
		l.readChar()
	}

	return l.input[position:l.position]
}

// isDigit checks whether the given argument is a digit or not.
func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}
