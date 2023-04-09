package token

type TokenType string

type Token struct {
	Type TokenType
	Literal string
}

const (
	ILLEGAL	= "ILLEGAL"
	EOF		= "EOF"

	// Identifiers + literals
	IDENT	= "IDENT" // add, foobar, x, y, ...
	INT		= "INT" // 123456

	// Operators
	ASSIGN		= "="
	PLUS		= "+"
	MINUS		= "-"
	BANG		= "!"
	ASTERISK	= "*"
	SLASH		= "/"

	LT = "<"
	GT = ">"

	EQ		=	"=="
	NOT_EQ	=	"!="

	// Delimiters
	COMMA		= ","
	SEMICOLON	= ";"

	LPAREN = "("
	RPAREN = ")"
	LBRACE = "{"
	RBRACE = "}"

	// Keywords
	FUNCTION	= "FUNCTION"
	LET			= "LET"
	TRUE		= "TRUE"
	FALSE		= "FALSE"
	IF			= "IF"
	ELSE		= "ELSE"
	RETURN		= "RETURN"
)

var keywords = map[string]TokenType{
	"fn": FUNCTION,
	"let": LET,
	"true": TRUE,
	"false": FALSE,
	"if": IF,
	"else": ELSE,
	"return": RETURN,
}

// LookupIdent checks the keywords map to see whether the given identifier is a
// keyword. If it is returns the keyword's TokenType constant. If it isn't returns
// token.IDENT which is the TokenType for all user-defined identifiers.
func LookupIdent(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}

	return IDENT
}
