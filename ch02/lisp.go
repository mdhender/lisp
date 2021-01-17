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
	"fmt"
	"strings"
)

func Version() string {
	return "chapter-01"
}

// AtomKind is an enum for the type of data stored in an Atom.
type AtomKind int

// enums for AtomKind
const (
	AtomKind_NIL AtomKind = iota
	AtomKind_Pair
	AtomKind_Symbol
	AtomKind_Integer
)

// Atom is our most primitive unit of storage.
type Atom struct {
	kind    AtomKind
	integer int64
	pair    *Pair
	symbol  []byte
}

// Pair is a "cons" cell containing two Atoms.
type Pair struct {
	atom [2]Atom
}

// nilp is a predicate that returns true if the argument is NIL.
func nilp(a Atom) bool { return a.kind == AtomKind_NIL }

// car returns the car of a pair. panics if p is not a pair.
func car(p Atom) Atom { return p.pair.atom[0] }

// cdr returns the cdr of a pair. panics if p is not a pair.
func cdr(p Atom) Atom { return p.pair.atom[1] }

// cons returns a Pair from the heap.
func cons(car, cdr Atom) Atom {
	return Atom{kind: AtomKind_Pair, pair: &Pair{atom: [2]Atom{car, cdr}}}
}

// make_int returns a new Atom with the given integer value.
func make_int(x int64) Atom {
	return Atom{kind: AtomKind_Integer, integer: x}
}

// sym_table is part of the environment and holds the list of currently defined symbols.
var sym_table Atom

// make_sym returns an Atom with the given symbol value.
// If the symbol already exists, we return a copy of it from the symbol table.
func make_sym(s []byte) Atom {
	for p := sym_table; !nilp(p); p = cdr(p) {
		if bytes.Equal(car(p).symbol, s) {
			return car(p)
		}
	}
	a := Atom{kind: AtomKind_Symbol, symbol: make([]byte, len(s), len(s))}
	copy(a.symbol, s)
	sym_table = cons(a, sym_table)
	return a
}

// print_expr relies on the Atom's stringer to return a text representation of the atom.
func print_expr(a Atom) {
	fmt.Printf("%s", a.String())
}

// String implements the stringer interface.
func (a Atom) String() string {
	switch a.kind {
	case AtomKind_NIL:
		return "NIL"
	case AtomKind_Integer:
		return fmt.Sprintf("%d", a.integer)
	case AtomKind_Symbol:
		return string(a.symbol)
	case AtomKind_Pair:
		sb := strings.Builder{}
		sb.WriteString("(")
		sb.WriteString(car(a).String())
		a = cdr(a)
		for !nilp(a) {
			if a.kind != AtomKind_Pair {
				sb.WriteString(" . ")
				sb.WriteString(a.String())
				break
			}
			sb.WriteString(" ")
			sb.WriteString(car(a).String())
			a = cdr(a)
		}
		sb.WriteString(")")
		return sb.String()
	}
	panic(fmt.Sprintf("assert(atom.kind != %d)", a.kind))
}
