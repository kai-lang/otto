package main

import (
	"os"
	"bufio"
	"unicode"
	"fmt"
)

type TokenType int

const (
	KeywordToken TokenType = iota
	IdentToken
	IntegerToken
)

type Token struct {
	Type TokenType
	Payload interface{}
	Line, Column int
}

type StateFunc func (l *Lexer) StateFunc

type Lexer struct {
	// TODO: replace text/scanner with own scanner
	Input *bufio.Reader
	buffer string
	lastRuneWidth int
	Line, Column int
	currentState StateFunc
	tokens []*Token
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

func (l *Lexer) Back() error {
	err := l.Input.UnreadRune()
	if err != nil {
		return err
	}
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

func varState(l *Lexer) StateFunc {
	fmt.Println("var state!")
	return nil
}

func keywordState(l *Lexer) StateFunc {
	r, _ := l.Peek()
	for unicode.IsLetter(r) {
		l.Next()
		r, _ = l.Peek()
	}
	str := l.Emit()
	l.tokens = append(l.tokens, &Token {
		Type: KeywordToken,
		Payload: str,
		Line: l.Line,
		Column: l.Column,
	})
	if str == "var" {
		return varState
	}
	return nil
}

func startState(l *Lexer) StateFunc {
	r, _ := l.Peek()
	if unicode.IsLetter(r) {
		return keywordState
	}
	return nil
}

func (l *Lexer) NextToken() *Token {
	for l.currentState != nil {
		l.currentState = l.currentState(l)
		if len(l.tokens) > 0 {
			t := l.tokens[len(l.tokens)-1]
			l.tokens = l.tokens[:len(l.tokens)-1]
			return t
		}
	}
	return nil
}

func main() {
	l := Lexer { currentState: startState, Input: bufio.NewReader(os.Stdin) }
	for t := l.NextToken(); t != nil; t = l.NextToken() {
		fmt.Println(t)
	}
}
