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
	for _, tc := range []struct {
		name   string
		input  string
		expect []string
		err    error
	}{
		{"a", "(foo bar)", []string{"(", "foo", "bar", ")"}, Error_EOF},
		{"b", " ( ) ", []string{"(", ")"}, Error_EOF},
		{"c", " ", []string{}, Error_EOF},
	} {
		input := []byte(tc.input)
		var tokens [][]byte
		for len(input) != 0 {
			token, rest, err := lex(input)
			if err != nil {
				if tc.err == nil {
					t.Errorf("%s: unexpected error: %+v\n", tc.name, err)
				} else if !errors.Is(err, tc.err) {
					t.Errorf("%s: expected %+v: got %+v\n", tc.name, tc.err, err)
				}
				break
			}
			tokens = append(tokens, token)
			input = rest
		}
		if len(tc.expect) != len(tokens) {
			t.Errorf("%s: expected %d tokens: got %d\n", tc.name, len(tc.expect), len(tokens))
		}
		var got, expect [][]byte
		if len(tokens) < len(tc.expect) {
			got = make([][]byte, len(tc.expect), len(tc.expect))
			expect = make([][]byte, len(tc.expect), len(tc.expect))
		} else {
			got = make([][]byte, len(tokens), len(tokens))
			expect = make([][]byte, len(tokens), len(tokens))
		}
		for i, tok := range tc.expect {
			expect[i] = []byte(tok)
		}
		for i, tok := range tokens {
			got[i] = tok
		}
		for i := range expect {
			if expect[i] == nil {
				t.Errorf("%s: token %d: expected nil: got %q\n", tc.name, i, string(got[i]))
			} else if got[i] == nil {
				t.Errorf("%s: token %d: expected %q: got nil\n", tc.name, i, string(expect[i]))
			} else if !bytes.Equal(expect[i], got[i]) {
				t.Errorf("%s: token %d: expected %q: got %q\n", tc.name, i, string(expect[i]), string(got[i]))
			}
		}
	}
}

func TestMakeInt(t *testing.T) {
	for _, tc := range []struct {
		name    string
		integer int64
	}{
		{"a", 42},
	} {
		a := make_int(tc.integer)
		if !(a.kind == AtomKind_Integer) {
			t.Errorf("%s: expected AtomKind_Integer: got %d\n", tc.name, a.kind)
		}
		if !(tc.integer == a.integer) {
			t.Errorf("%s: expected %d: got %d\n", tc.name, tc.integer, a.integer)
		}
	}
}

func TestMakeSym(t *testing.T) {
	for _, tc := range []struct {
		name   string
		symbol string
	}{
		{"snake", "snake"},
	} {
		s := []byte(tc.symbol)
		a := make_sym(s)
		if !(a.kind == AtomKind_Symbol) {
			t.Errorf("%s: expected AtomKind_Symbol: got %d\n", tc.name, a.kind)
		}
		if !bytes.Equal(s, a.symbol) {
			t.Errorf("%s: expected %q: got %q\n", tc.name, tc.symbol, string(a.symbol))
		}
	}
}

func TestParseInteger(t *testing.T) {
	for _, tc := range []struct {
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
		token, rest := parse_integer([]byte(tc.input))
		if tc.token != string(token) {
			t.Errorf("%s: expected token %q: got %q\n", tc.name, tc.token, string(token))
		}
		if tc.rest != string(rest) {
			t.Errorf("%s: expected rest %q: got %q\n", tc.name, tc.rest, string(rest))
		}
	}
}

func TestParseSymbol(t *testing.T) {
	for _, tc := range []struct {
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
		token, rest := parse_symbol([]byte(tc.input))
		if tc.token != string(token) {
			t.Errorf("%s: expected token %q: got %q\n", tc.name, tc.token, string(token))
		}
		if tc.rest != string(rest) {
			t.Errorf("%s: expected rest %q: got %q\n", tc.name, tc.rest, string(rest))
		}
	}
}

func TestRead(t *testing.T) {
	for _, tc := range []struct {
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
		a, rest, err := read_expr([]byte(tc.input))
		if err != nil {
			if tc.err == nil {
				t.Errorf("%s: unexpected error: %+v\n", tc.name, err)
			} else if !errors.Is(err, tc.err) {
				t.Errorf("%s: expected %+v: got %+v\n", tc.name, tc.err, err)
			}
			continue
		}
		if got := a.String(); tc.expect != got {
			t.Errorf("%s: expected %q: got %q\n", tc.name, tc.expect, got)
		}
		if tc.rest != string(rest) {
			t.Errorf("%s: expected rest %q: got %q\n", tc.name, tc.rest, string(rest))
		}
	}
}

func TestStringer(t *testing.T) {
	for _, tc := range []struct {
		name   string
		atom   Atom
		expect string
	}{
		{"a", make_int(42), "42"},
		{"b", make_sym([]byte("FOO")), "FOO"},
		{"c", cons(make_sym([]byte("X")), make_sym([]byte("Y"))), "(X . Y)"},
		{"d", cons(make_int(1), cons(make_int(2), cons(make_int(3), Atom{}))), "(1 2 3)"},
	} {
		a := tc.atom.String()
		if tc.expect != a {
			t.Errorf("%s: expected %q: got %q\n", tc.name, tc.expect, a)
		}
	}
}

func TestEnvCreate(t *testing.T) {
	for _, tc := range []struct {
		name string
		atom Atom
	}{
		{"nil", Atom{}},
		{"integer", make_int(42)},
		{"pair", cons(make_int(1), make_int(2))},
		{"symbol", make_sym([]byte("snake"))},
	} {
		env := env_create(tc.atom)
		if env.kind != AtomKind_Pair {
			t.Errorf("%s: expected env.kind %d: got %d\n", tc.name, AtomKind_Pair, env.kind)
			continue
		}
		if car(env).kind != tc.atom.kind {
			t.Errorf("%s: expected car(env).kind %d: got %d\n", tc.name, tc.atom.kind, car(env).kind)
		} else {
			switch tc.atom.kind {
			case AtomKind_NIL:
				// nothing else to check for NIL
			case AtomKind_Integer:
				if car(env).integer != tc.atom.integer {
					t.Errorf("%s: expected car(env).integer %d: got %d\n", tc.name, tc.atom.integer, car(env).integer)
				}
			case AtomKind_Pair:
				if car(env).pair != tc.atom.pair {
					t.Errorf("%s: expected car(env).pair %p: got %p\n", tc.name, tc.atom.pair, car(env).pair)
				}
			case AtomKind_Symbol:
				if !bytes.Equal(car(env).symbol, tc.atom.symbol) {
					t.Errorf("%s: expected car(env).symbol %q: got %q\n", tc.name, string(tc.atom.symbol), string(car(env).symbol))
				}
			}
		}
		if cdr(env).kind != AtomKind_NIL {
			t.Errorf("%s: expected cdr(env).kind %d: got %d\n", tc.name, AtomKind_NIL, cdr(env).kind)
		}
	}
}
