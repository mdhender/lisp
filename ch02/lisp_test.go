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

package ch02

import (
	"bytes"
	"testing"
)

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
