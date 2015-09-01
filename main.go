package main

import (
	"os"
	"bufio"
	"unicode"
	"fmt"
	"strconv"
)

type TokenType int

const (
	ErrorToken TokenType = iota
	KeywordToken
	IdentToken
	IntegerToken
)

type Keyword int

const (
	VarKeyword Keyword = iota // var
	AssignKeyword // =
	AddKeyword // +
	SubKeyword // -
	MulKeyword // *
	ModKeyword // %
	SemiColonKeyword // ;
)

type Token struct {
	Type TokenType
	Payload interface{}
	Line, Column int
}

type StateFunc func (l *Lexer) StateFunc

type Lexer struct {
	Input *bufio.Reader
	buffer string
	lastRuneWidth int
	Line, Column int
	State StateFunc
	Tokens chan *Token
}

func (l *Lexer) Emit() string {
	var n string
	b := l.buffer
	l.buffer = n
	return b
}

func (l *Lexer) Next() (rune, error) {
	r, w, err := l.Input.ReadRune()
	if err != nil {
		return r, err
	}
	l.lastRuneWidth = w
	l.buffer += string(r)
	if r == '\n' {
		l.Line++
		l.Column = 0
	} else {
		l.Column++
	}
	return r, nil
}

type RunePredicate func (rune) bool

// Reads up to unaccepted rune
func (l *Lexer) NextUpTo(pred RunePredicate) (r rune, err error) {
	for {
		r, err = l.Peek()
		if err != nil || !pred(r) {
			return r, err
		}
		l.Next()
	}
}

// Ignores up to unaccepted rune
func (l *Lexer) IgnoreUpTo(pred RunePredicate) (r rune, err error) {
	for {
		r, err = l.Peek()
		if err != nil || !pred(r) {
			return r, err
		}
		l.Ignore()
	}
}

func (l *Lexer) Ignore() (rune, error) {
	r, err := l.Next()
	if err != nil {
		return r, err
	}
	l.buffer = l.buffer[:len(l.buffer)-l.lastRuneWidth]
	return r, nil
}

func (l *Lexer) Back() error {
	err := l.Input.UnreadRune()
	if err != nil {
		return err
	}
	l.Column--
	l.buffer = l.buffer[:len(l.buffer)-l.lastRuneWidth]
	return nil
}

func (l *Lexer) Peek() (rune, error) {
	r, err := l.Next()
	if err != nil {
		return r, err
	}
	err = l.Back()
	if err != nil {
		return r, err
	}
	return r, nil
}

func (l *Lexer) emitToken(t *Token) {
	l.Tokens<- t
}

// utility whitespace lex function
func (l *Lexer) whitespace() error {
	r, err := l.Peek()
	if err != nil {
		return err
	}
	for unicode.Is(unicode.White_Space, r) {
		l.Next()
		r, err = l.Peek()
		if err != nil {
			return err
		}
	}
	l.Emit() // dump
	return nil
}

func floatState(l *Lexer) StateFunc {
	return nil
}

func numberState(l *Lexer) StateFunc {
	base := 10

	// possible prefix
	r, err := l.Peek()
	if err != nil {
		return nil
	}
	if r == '0' {
		l.Next()
		r, err := l.Next()
		if err != nil {
			return nil
		}
		switch r {
		case 'x':
			l.Emit()
			base = 16 // hexadecimal
		case 'c':
			l.Emit()
			base = 8 // octal
		case 'b':
			l.Emit()
			base = 2 // binary
		default:
			l.Back()
		}
	}

	r, _ = l.NextUpTo(func(r rune) bool {
		// allow '_' separator
		if base == 16 {
			return unicode.Is(unicode.Hex_Digit, r) || r == '_'
		} else {
			return unicode.IsDigit(r) || r == '_'
		}
	})

	if r == '.' {
		// floating point number
		return floatState
	} else {
		i, err := strconv.ParseInt(l.Emit(), base, 64)
		if err != nil {
			l.emitToken(&Token {
				Type: ErrorToken,
				Payload: fmt.Sprint(err),
				Line: l.Line,
				Column: l.Column,
			})
			return startState
		}
		l.emitToken(&Token {
			Type: IntegerToken,
			Payload: i,
			Line: l.Line,
			Column: l.Column,
		})
		return expressionState
	}
}

func expressionState(l *Lexer) StateFunc {
	if l.whitespace() != nil {
		return nil
	}
	r, err := l.Peek()
	if err != nil {
		return nil
	}
	switch {
	case unicode.IsDigit(r):
		return numberState
	case r == '+' || r == '-' || r == '*' || r == '%':
		l.Next()
		var k Keyword
		switch r {
		case '+':
			k = AddKeyword
		case '-':
			k = SubKeyword
		case '*':
			k = MulKeyword
		case '%':
			k = ModKeyword
		}
		l.emitToken(&Token {
			Type: KeywordToken,
			Payload: k,
			Line: l.Line,
			Column: l.Column,
		})
		return expressionState
	case r == ';':
		l.Next()
		l.emitToken(&Token {
			Type: KeywordToken,
			Payload: SemiColonKeyword,
			Line: l.Line,
			Column: l.Column,
		})
		return startState
	default:
		l.Next()
		l.Tokens<- (&Token {
			Type: ErrorToken,
			Payload: fmt.Sprintf("Unexpected rune '%c' in expression", r),
			Line: l.Line,
			Column: l.Column,
		})
		return startState
	}
}

// = 1+3*a
func assignExpressionState(l *Lexer) StateFunc {
	if l.whitespace() != nil {
		return nil
	}
	r, err := l.Ignore()
	if err != nil {
		return nil
	}
	if r != '=' {
		// error, missing equals sign
		l.emitToken(&Token {
			Type: ErrorToken,
			Payload: fmt.Sprintf("Expected '=', found %c\n", r),
			Line: l.Line,
			Column: l.Column,
		})
		return startState
	} else {
		l.emitToken(&Token {
			Type: KeywordToken,
			Payload: AssignKeyword,
			Line: l.Line,
			Column: l.Column,
		})
		return expressionState
	}
}

func varIdentState(l *Lexer) StateFunc {
	l.NextUpTo(func (r rune) bool {
		return unicode.IsLetter(r) || unicode.IsDigit(r) || r == '_'
	})
	l.emitToken(&Token {
		Type: IdentToken,
		Payload: l.Emit(),
		Line: l.Line,
		Column: l.Column,
	})
	return assignExpressionState
}

func varState(l *Lexer) StateFunc {
	if l.whitespace() != nil {
		return nil
	}
	r, err := l.Peek()
	if err != nil {
		return nil
	}
	if unicode.IsLetter(r) || r == '_' {
		return varIdentState
	}
	return nil
}

func keywordState(l *Lexer) StateFunc {
	l.NextUpTo(unicode.IsLetter)
	l.emitToken(&Token {
		Type: KeywordToken,
		Payload: VarKeyword,
		Line: l.Line,
		Column: l.Column,
	})
	if l.Emit() == "var" {
		return varState
	}
	return nil
}

func startState(l *Lexer) StateFunc {
	if l.whitespace() != nil {
		return nil
	}
	r, err := l.Peek()
	if err != nil {
		return nil
	}
	if unicode.IsLetter(r) {
		return keywordState
	}
	return nil
}

func (l *Lexer) Run() {
	for l.State != nil {
		l.State = l.State(l)
	}
	close(l.Tokens)
}

// Prompting Reader
type Repl struct {
	Input *bufio.Reader
	Output *bufio.Writer
	lexer *Lexer
}

func (r *Repl) Init() chan *Token {
	r.lexer = &Lexer {
		State: startState,
		Input: bufio.NewReader(r),
		Tokens: make(chan *Token),
	}
	go r.lexer.Run()
	return r.lexer.Tokens
}

func (r *Repl) Read(p []byte) (n int, err error) {
	for i := 0; i < len(p); i++ {
		b, err := r.Input.ReadByte()
		if err != nil {
			return i, err
		}
		p[i] = b
		if b == '\n' {
			return i, nil
		}
	}
	return len(p), nil
}

type SyntaxTree struct {
	Tok *Token
	Left, Right *SyntaxTree
}

// recursive descent parser
// TODO: make a state machine with each
// state returning two subsequent states,
// one for each child.
type Parser struct {
	Input chan *Token
	tree *SyntaxTree
}

func (p *Parser) parseIdent() *SyntaxTree {
	tok := <-p.Input
	if tok.Type != IdentToken {
		fmt.Printf("Expected ident, not found!\n");
	}
	return &SyntaxTree {
		Tok: tok,
	}
}

func (p *Parser) parseExpression() *SyntaxTree {
	return nil
}

func (p *Parser) parseAssignment() {
	a := <-p.Input
	if a.Type != KeywordToken || a.Payload != AssignKeyword {
		fmt.Printf("Expected assignment, '=' not found!\n");
	}
}

func (p *Parser) parseKeyword() *SyntaxTree {
	t := new(SyntaxTree)
	t.Tok = <-p.Input
	if t.Tok.Type == KeywordToken {
		if t.Tok.Payload == VarKeyword {
			t.Right = p.parseIdent()
			p.parseAssignment()
			t.Left = p.parseExpression()
			return t
		}
	} else {
		fmt.Printf("Expected keyword, not found!\n");
	}
	return nil
}

func (p *Parser) Run() {
	p.tree = p.parseKeyword()
}

func (t *SyntaxTree) String() string {
	return fmt.Sprintf("{%v: %s, %s}", t.Tok, t.Right, t.Left)
}

func main() {
	r := Repl {
		Input: bufio.NewReader(os.Stdin),
		Output: bufio.NewWriter(os.Stdout),
	}
	p := Parser {
		Input: r.Init(),
	}
	p.Run()
	fmt.Println(p.tree)
}
