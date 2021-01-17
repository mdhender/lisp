/*
 * LISP - an implementation of Building LISP
 *
 * Copyright (c) 2021 Michael D Henderson
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package ch04

import (
	"bytes"
	"errors"
	"testing"
)

func TestLex(t *testing.T) {
	for _, tt := range []struct {
		name   string
		input  string
		expect []string
		err    error
	}{
		{"a", "(foo bar)", []string{"(", "foo", "bar", ")"}, ErrorEOF},
		{"b", " ( ) ", []string{"(", ")"}, ErrorEOF},
		{"c", " ", []string{}, ErrorEOF},
	} {
		input := []byte(tt.input)
		var tokens [][]byte
		for len(input) != 0 {
			token, rest, err := lex(input)
			if err != nil {
				if tt.err == nil {
					t.Errorf("%s: unexpected error: %+v\n", tt.name, err)
				} else if !errors.Is(err, tt.err) {
					t.Errorf("%s: expected %+v: got %+v\n", tt.name, tt.err, err)
				}
				break
			}
			tokens = append(tokens, token)
			input = rest
		}
		if len(tt.expect) != len(tokens) {
			t.Errorf("%s: expected %d tokens: got %d\n", tt.name, len(tt.expect), len(tokens))
		}
		var got, expect [][]byte
		if len(tokens) < len(tt.expect) {
			got = make([][]byte, len(tt.expect), len(tt.expect))
			expect = make([][]byte, len(tt.expect), len(tt.expect))
		} else {
			got = make([][]byte, len(tokens), len(tokens))
			expect = make([][]byte, len(tokens), len(tokens))
		}
		for i, tok := range tt.expect {
			expect[i] = []byte(tok)
		}
		for i, tok := range tokens {
			got[i] = tok
		}
		for i := range expect {
			if expect[i] == nil {
				t.Errorf("%s: token %d: expected nil: got %q\n", tt.name, i, string(got[i]))
			} else if got[i] == nil {
				t.Errorf("%s: token %d: expected %q: got nil\n", tt.name, i, string(expect[i]))
			} else if !bytes.Equal(expect[i], got[i]) {
				t.Errorf("%s: token %d: expected %q: got %q\n", tt.name, i, string(expect[i]), string(got[i]))
			}
		}
	}
}

func TestMakeInt(t *testing.T) {
	for _, tt := range []struct {
		name    string
		integer int64
	}{
		{"a", 42},
	} {
		a := make_int(tt.integer)
		if !(a.kind == AtomKind_Integer) {
			t.Errorf("%s: expected AtomKind_Integer: got %d\n", tt.name, a.kind)
		}
		if !(tt.integer == a.integer) {
			t.Errorf("%s: expected %d: got %d\n", tt.name, tt.integer, a.integer)
		}
	}
}

func TestMakeSym(t *testing.T) {
	for _, tt := range []struct {
		name   string
		symbol string
	}{
		{"snake", "snake"},
	} {
		s := []byte(tt.symbol)
		a := make_sym(s)
		if !(a.kind == AtomKind_Symbol) {
			t.Errorf("%s: expected AtomKind_Symbol: got %d\n", tt.name, a.kind)
		}
		if !bytes.Equal(s, a.symbol) {
			t.Errorf("%s: expected %q: got %q\n", tt.name, tt.symbol, string(a.symbol))
		}
	}
}

func TestParseInteger(t *testing.T) {
	for _, tt := range []struct {
		name  string
		input string
		token string
		rest  string
	}{
		{"a", "(foobar)", "", "(foobar)"},
		{"b", "42)", "42", ")"},
		{"c", "-42)", "-42", ")"},
		{"d", "+42)", "+42", ")"},
		{"e", "+foobar", "", "+foobar"},
	} {
		token, rest := parse_integer([]byte(tt.input))
		if tt.token != string(token) {
			t.Errorf("%s: expected token %q: got %q\n", tt.name, tt.token, string(token))
		}
		if tt.rest != string(rest) {
			t.Errorf("%s: expected rest %q: got %q\n", tt.name, tt.rest, string(rest))
		}
	}
}

func TestParseSymbol(t *testing.T) {
	for _, tt := range []struct {
		name  string
		input string
		token string
		rest  string
	}{
		{"a", "(foo bar)", "", "(foo bar)"},
		{"b", "foo bar)", "foo", " bar)"},
		{"c", " bar)", "", " bar)"},
		{"d", "bar)", "bar", ")"},
		{"e", ")", "", ")"},
	} {
		token, rest := parse_symbol([]byte(tt.input))
		if tt.token != string(token) {
			t.Errorf("%s: expected token %q: got %q\n", tt.name, tt.token, string(token))
		}
		if tt.rest != string(rest) {
			t.Errorf("%s: expected rest %q: got %q\n", tt.name, tt.rest, string(rest))
		}
	}
}

func TestRead(t *testing.T) {
	for _, tt := range []struct {
		name   string
		input  string
		expect string
		rest   string
		err    error
	}{
		{"a", "42", "42", "", nil},
		{"b", "(foo bar)", "(foo bar)", "", nil},
		{"c", " ( foo  bar ) ", "(foo bar)", " ", nil},
		{"d", "( s (t . u) v . (w . NIL))(foo)", "(s (t . u) v w)", "(foo)", nil},
		{"e", "()", "NIL", "", nil},
	} {
		a, rest, err := read_expr([]byte(tt.input))
		if err != nil {
			if tt.err == nil {
				t.Errorf("%s: unexpected error: %+v\n", tt.name, err)
			} else if !errors.Is(err, tt.err) {
				t.Errorf("%s: expected %+v: got %+v\n", tt.name, tt.err, err)
			}
			continue
		}
		if got := a.String(); tt.expect != got {
			t.Errorf("%s: expected %q: got %q\n", tt.name, tt.expect, got)
		}
		if tt.rest != string(rest) {
			t.Errorf("%s: expected rest %q: got %q\n", tt.name, tt.rest, string(rest))
		}
	}
}

func TestStringer(t *testing.T) {
	for _, tt := range []struct {
		name   string
		atom   Atom
		expect string
	}{
		{"a", make_int(42), "42"},
		{"b", make_sym([]byte("FOO")), "FOO"},
		{"c", cons(make_sym([]byte("X")), make_sym([]byte("Y"))), "(X . Y)"},
		{"d", cons(make_int(1), cons(make_int(2), cons(make_int(3), Atom{}))), "(1 2 3)"},
	} {
		a := tt.atom.String()
		if tt.expect != a {
			t.Errorf("%s: expected %q: got %q\n", tt.name, tt.expect, a)
		}
	}
}
