package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
	"unicode"
)

type TokenType int

const (
	ErrorToken TokenType = iota
	EOFToken
	KeywordToken
	OperatorToken
	IdentToken
	IntegerToken
)

type Keyword int

const (
	beginKeywords Keyword = iota
	VarKeyword            // var
	AssignKeyword         // =
	endKeywords
)

var keywords = map[Keyword]string{
	VarKeyword:       "var",
	AssignKeyword:    "=",
}

type Operator int

const (
	beginOperators       Operator = iota
	AddOperator                   // +
	SubOperator                   // -
	MulOperator                   // *
	ModOperator                   // %
	DivOperator                   // /
	LeftParenOperator             // (
	RightParenOperator            // )
	LeftBracketOperator           // [
	RightBracketOperator          // ]
	LeftBraceOperator             // {
	RightBraceOperator            // }
	SemicolonOperator             // ;
	NotOperator // !
	endOperators
)

var operators = map[Operator]string{
	AddOperator:          "+",
	SubOperator:          "-",
	MulOperator:          "*",
	ModOperator:          "%",
	DivOperator:          "/",
	LeftParenOperator:    "(",
	RightParenOperator:   ")",
	LeftBracketOperator:  "[",
	RightBracketOperator: "]",
	LeftBraceOperator:    "{",
	RightBraceOperator:   "}",
	SemicolonOperator:    ";",
	NotOperator: "!",
}

type Token struct {
	Type         TokenType
	Payload      interface{}
	Line, Column int
}

type StateFunc func(l *Lexer) StateFunc

type Lexer struct {
	Input         *bufio.Reader
	buffer        string
	lastRuneWidth int
	Line, Column  int
	State         StateFunc
	Tokens        chan *Token
}

func (t *Token) String() string {
	switch t.Type {
	case ErrorToken:
		return fmt.Sprintf("err@%d:%d->%s", t.Line, t.Column, t.Payload)
	case EOFToken:
		return "EOF"
	case KeywordToken:
		return keywords[t.Payload.(Keyword)]
	case OperatorToken:
		return operators[t.Payload.(Operator)]
	case IdentToken:
		return t.Payload.(string)
	case IntegerToken:
		return fmt.Sprintf("%d", t.Payload)
	default:
		return "unknown"
	}
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

type RunePredicate func(rune) bool

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
	l.Tokens <- t
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
		r := strings.NewReplacer("_", "")
		i, err := strconv.ParseInt(r.Replace(l.Emit()), base, 64)
		if err != nil {
			l.emitToken(&Token{
				Type:    ErrorToken,
				Payload: fmt.Sprint(err),
				Line:    l.Line,
				Column:  l.Column,
			})
			return startState
		}
		l.emitToken(&Token{
			Type:    IntegerToken,
			Payload: i,
			Line:    l.Line,
			Column:  l.Column,
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
	default:
		l.Next()
		t := &Token{
			Type:   OperatorToken,
			Line:   l.Line,
			Column: l.Column,
		}
		for k, o := range operators {
			if string(r) == o {
				t.Payload = k
			}
		}
		if t.Payload == nil {
			l.emitToken(&Token{
				Type:    ErrorToken,
				Payload: fmt.Sprintf("Unknown operator '%c'", r),
				Line:    l.Line,
				Column:  l.Column,
			})
			return nil
		} else {
			l.emitToken(t)
			if t.Payload == SemicolonOperator {
				return startState
			} else {
				return expressionState
			}
		}
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
		l.emitToken(&Token{
			Type:    ErrorToken,
			Payload: fmt.Sprintf("Expected '=', found %c", r),
			Line:    l.Line,
			Column:  l.Column,
		})
		return startState
	} else {
		l.emitToken(&Token{
			Type:    KeywordToken,
			Payload: AssignKeyword,
			Line:    l.Line,
			Column:  l.Column,
		})
		return expressionState
	}
}

func varIdentState(l *Lexer) StateFunc {
	l.NextUpTo(func(r rune) bool {
		return unicode.IsLetter(r) || unicode.IsDigit(r) || r == '_'
	})
	l.emitToken(&Token{
		Type:    IdentToken,
		Payload: l.Emit(),
		Line:    l.Line,
		Column:  l.Column,
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
	l.emitToken(&Token{
		Type:    KeywordToken,
		Payload: VarKeyword,
		Line:    l.Line,
		Column:  l.Column,
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
	l.emitToken(&Token{
		Type:   EOFToken,
		Line:   l.Line,
		Column: l.Column,
	})
	close(l.Tokens)
}

// Prompting Reader
type Repl struct {
	Input  *bufio.Reader
	Output *bufio.Writer
	lexer  *Lexer
}

func (r *Repl) Init() chan *Token {
	r.lexer = &Lexer{
		State:  startState,
		Input:  bufio.NewReader(r),
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
	Tok         *Token
	Left, Right *SyntaxTree
}

type PStateFunc func(*Parser) PStateFunc

type Parser struct {
	Input chan *Token
	root  *SyntaxTree
	front []*SyntaxTree
	State PStateFunc
	trees chan *SyntaxTree
}

func parseIdent(p *Parser) PStateFunc {
	tok := <-p.Input
	if tok.Type != IdentToken {
		fmt.Println(&Token{
			Type: ErrorToken,
			Payload: fmt.Sprintf("Expected ident, found %v", tok),
			Line: tok.Line,
			Column: tok.Column,
		})
		return nil
	} else {
		tr := p.front[len(p.front)-1]
		tr.Left = &SyntaxTree{
			Tok: tok,
		}
		return parseAssignment
	}
}

var precedence = map[Operator]int{
	AddOperator: 3,
	SubOperator: 3,
	MulOperator: 4,
	ModOperator: 4,
	DivOperator: 4,
}

const (
	RightAssociative = iota
	LeftAssociative
)

var associativity = map[Operator]int{
	AddOperator: LeftAssociative,
	SubOperator: LeftAssociative,
	MulOperator: LeftAssociative,
	ModOperator: LeftAssociative,
	DivOperator: LeftAssociative,
}

func (p *Parser) shuntingYard() chan *Token {
	var ops []*Token
	out := make(chan *Token)
	go func() {
		for tok := range p.Input {
			if tok.Type == IntegerToken {
				out <- tok
			} else if tok.Type == OperatorToken {
				if tok.Payload == RightParenOperator {
					success := false
					for len(ops) > 0 {
						op := ops[len(ops)-1]
						if op.Payload == LeftParenOperator {
							success = true
							break
						} else {
							ops = ops[:len(ops)-1]
							out <- op
						}
					}
					if !success {
						fmt.Println(&Token{
							Type: ErrorToken,
							Payload: "Unmatched parenthesis",
							Line: tok.Line,
							Column: tok.Column,
						})
					}
				} else if tok.Payload == SemicolonOperator {
					for len(ops) > 0 {
						op := ops[len(ops)-1]
						ops = ops[:len(ops)-1]
						if op.Payload != LeftParenOperator {
							out <- op
						}
					}
					break
				} else {
					for len(ops) > 0 && ((associativity[tok.Payload.(Operator)] == LeftAssociative && precedence[tok.Payload.(Operator)] <= precedence[ops[len(ops)-1].Payload.(Operator)]) || (associativity[tok.Payload.(Operator)] == RightAssociative && precedence[tok.Payload.(Operator)] < precedence[ops[len(ops)-1].Payload.(Operator)])) {
						op := ops[len(ops)-1]
						ops = ops[:len(ops)-1]
						out <- op
					}
					ops = append(ops, tok)
				}
			}
		}
		close(out)
	}()
	return out
}

func parseExpression(p *Parser) PStateFunc {
	var trees []*SyntaxTree
	for a := range p.shuntingYard() {
		s := &SyntaxTree{
			Tok: a,
		}
		if a.Type == IntegerToken {
			trees = append(trees, s)
		} else if a.Type == OperatorToken {
			if len(trees) >= 2 {
				s.Right = trees[len(trees)-1]
				s.Left = trees[len(trees)-2]
				trees = trees[:len(trees)-2]
				trees = append(trees, s)
			} else {
				// too many operators error
				fmt.Println(&Token{
					Type: ErrorToken,
					Payload: fmt.Sprintf("Extraneous operator %s", a),
					Line: a.Line,
					Column: a.Column,
				})
				return nil
			}
		}
	}
	if len(trees) > 0 {
		p.front[len(p.front)-1].Right = trees[len(trees)-1]
	} else {

	}
	return parseKeyword
}

func parseAssignment(p *Parser) PStateFunc {
	a := <-p.Input
	if a.Type == KeywordToken && a.Payload == AssignKeyword {
		return parseExpression
	} else {
		fmt.Println(&Token{
			Type: ErrorToken,
			Payload: fmt.Sprintf("Expected assignment, found %s", a),
			Line: a.Line,
			Column: a.Column,
		})
		return nil
	}
}

func parseKeyword(p *Parser) PStateFunc {
	if p.root != nil {
		p.trees <- p.root
	}
	t := &SyntaxTree{
		Tok: <-p.Input,
	}
	if t.Tok.Type == KeywordToken {
		if t.Tok.Payload == VarKeyword {
			p.root = t
			p.front = append(p.front, p.root)
			return parseIdent
		} else {
			fmt.Printf("%d:%d->Expected keyword, found unknown keyword '%s'.\n", t.Tok.Line, t.Tok.Column, t.Tok.Payload)
			return nil
		}
	} else if t.Tok.Type == EOFToken {
		return nil
	} else {
		fmt.Printf("%d:%d->Expected keyword, found %v.\n", t.Tok.Line, t.Tok.Column, t.Tok)
		return nil
	}
}

func (p *Parser) Run() {
	p.trees = make(chan *SyntaxTree)
	go func() {
		for p.State != nil {
			p.State = p.State(p)
		}
		close(p.trees)
	}()
}

func (t *SyntaxTree) String() string {
	if t.Left != nil && t.Right != nil {
		return fmt.Sprintf("(%v %s %s)", t.Tok, t.Left, t.Right)
	} else {
		return fmt.Sprint(t.Tok)
	}
}

func main() {
	r := Repl{
		Input:  bufio.NewReader(os.Stdin),
		Output: bufio.NewWriter(os.Stdout),
	}
	p := Parser{
		Input: r.Init(),
		State: parseKeyword,
	}
	p.Run()
	for t := range p.trees {
		fmt.Println(t)
	}
}
