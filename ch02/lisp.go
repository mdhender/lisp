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
	"reflect"
)

type Interpreter struct {
	// sym_table is part of the environment and holds the list of currently defined symbols.
	sym_table Atom
}

func New() *Interpreter {
	return &Interpreter{
		sym_table: NIL{},
	}
}

func (i *Interpreter) Version() string {
	return "chapter-02"
}

// Atom is our basic unit of storage.
type Atom interface{}

// Pair is a pair of two atoms.
type Pair struct {
	car Atom
	cdr Atom
}

type NIL struct{}

func (n NIL) ToExpr() []byte {
	return []byte{'N', 'I', 'L'}
}

type SExpr interface {
	ToExpr() []byte
}

type Symbol struct {
	name []byte
}

func (s *Symbol) ToExpr() []byte {
	return s.name
}

// car returns the car of a pair.
// panics if input is not a pair.
func car(a Atom) Atom {
	p, ok := a.(*Pair)
	if !ok {
		panic("assert(car arg is a pair)")
	}
	return p.car
}

// cdr returns the cdr of a pair.
// panics if input is not a pair.
func cdr(a Atom) Atom {
	p, ok := a.(*Pair)
	if !ok {
		panic("assert(cdr arg is a pair)")
	}
	return p.cdr
}

// pairp is a predicate that returns true if the argument is a pair.
func pairp(a Atom) bool {
	_, ok := a.(*Pair)
	return ok
}

// nilp is a predicate that returns true if the argument is NIL.
func nilp(a Atom) bool {
	_, ok := a.(NIL)
	return ok
}

// cons returns a pair from the heap.
func cons(car, cdr Atom) Atom {
	return &Pair{car: car, cdr: cdr}
}

// make_int returns a new Atom with the given integer value.
func make_int(x int) Atom {
	return x
}

// make_sym returns an Atom with the given symbol value.
// If the symbol already exists, we return a copy of it from the symbol table.
func (i *Interpreter) make_sym(name []byte) Atom {
	for p := i.sym_table; !nilp(p); p = cdr(p) {
		a := car(p)
		if st, ok := a.(*Symbol); ok && bytes.Equal(name, st.name) {
			return a
		}
	}
	a := &Symbol{name: name}
	i.sym_table = cons(a, i.sym_table)
	return a
}

// print_expr relies on the Atom's stringer to return a text representation of the atom.
func print_expr(a Atom) {
	fmt.Printf("%s", string(atob(a)))
}

func atob(a Atom) []byte {
	if s, ok := a.(SExpr); ok {
		return s.ToExpr()
	}
	switch a.(type) {
	case int:
		integer, _ := a.(int)
		return []byte(fmt.Sprintf("%d", integer))
	case *Symbol:
		symbol, _ := a.(*Symbol)
		return symbol.name
	case *Pair:
		b := append([]byte{'('}, atob(car(a))...)
		a = cdr(a)
		for !nilp(a) {
			if !pairp(a) {
				b = append(b, ' ', '.', ' ')
				b = append(b, atob(a)...)
				break
			}
			b = append(b, ' ')
			b = append(b, atob(car(a))...)
			a = cdr(a)
		}
		return append(b, ')')
	}
	panic(fmt.Sprintf("assert(atom.type != %v)", reflect.TypeOf(a)))
}
