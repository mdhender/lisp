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
	"reflect"
	"testing"
)

func TestMakeInt(t *testing.T) {
	for _, tc := range []struct {
		id    string
		integer int
	}{
		{"a", 42},
	} {
		a := make_int(tc.integer)
		integer, ok := a.(int)
		if !ok {
			t.Errorf("%s: expected integer: got %v\n", tc.id, reflect.TypeOf(a))
			continue
		}
		if tc.integer != integer {
			t.Errorf("%s: expected %d: got %d\n", tc.id, tc.integer, integer)
		}
	}
}

func TestMakeSym(t *testing.T) {
	for _, tc := range []struct {
		id   string
		symbol []byte
	}{
		{"snake", []byte("snake")},
	} {
		i := New()
		a := i.make_sym([]byte(tc.symbol))
		symbol, ok := a.(*Symbol)
		if !ok {
			t.Errorf("%s: expected symbol: got %v\n", tc.id, reflect.TypeOf(a))
			continue
		}
		if !bytes.Equal(tc.symbol, symbol.name) {
			t.Errorf("%s: expected %q: got %q\n", tc.id, string(tc.symbol), string(symbol.name))
		}
	}
}

func TestStringer(t *testing.T) {
	i := New()
	for _, tc := range []struct {
		id   string
		atom   Atom
		expect string
	}{
		{"a", make_int(42), "42"},
		{"b", i.make_sym([]byte("FOO")), "FOO"},
		{"c", cons(i.make_sym([]byte("X")), i.make_sym([]byte("Y"))), "(X . Y)"},
		{"d", cons(make_int(1), cons(make_int(2), cons(make_int(3), NIL{}))), "(1 2 3)"},
	} {
		s := string(atob(tc.atom))
		if tc.expect != s {
			t.Errorf("%s: expected %q: got %q\n", tc.id, tc.expect, s)
		}
	}
}
