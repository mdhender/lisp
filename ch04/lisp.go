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
	"fmt"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"
)

func Version() string {
	return "chapter-04"
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

var ErrorEOF = errors.New("end-of-input")
var ErrorSyntax = errors.New("syntax error")

// sym_table is part of the environment and holds the list of currently defined symbols.
var sym_table Atom

// car returns the car of a pair. panics if p is not a pair.
func car(p Atom) Atom { return p.pair.atom[0] }

// cdr returns the cdr of a pair. panics if p is not a pair.
func cdr(p Atom) Atom { return p.pair.atom[1] }

// cons returns a Pair from the heap.
func cons(car, cdr Atom) Atom {
	return Atom{kind: AtomKind_Pair, pair: &Pair{atom: [2]Atom{car, cdr}}}
}

// lex returns the next token along with the remaining input.
// it returns end-of-input if there is no input left to read.
func lex(input []byte) (token []byte, rest []byte, err error) {
	// skip leading whitespace and return an error no more input
	if input = bytes.TrimLeft(input, " \t\n"); len(input) == 0 {
		return nil, nil, ErrorEOF
	}
	// accept an open or close paren
	if input[0] == '(' || input[0] == ')' {
		return input[:1], input[1:], nil
	}
	// otherwise return all characters up to the first delimiter
	offset := bytes.IndexAny(input, "() \t\n")
	if offset == -1 {
		return input, nil, nil
	}
	return input[:offset], input[offset:], nil
}

// make_int returns a new Atom with the given integer value.
func make_int(x int64) Atom {
	return Atom{kind: AtomKind_Integer, integer: x}
}

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

// nilp is a predicate that returns true if the argument is NIL.
func nilp(a Atom) bool { return a.kind == AtomKind_NIL }

func parse_integer(input []byte) (token, rest []byte) {
	lenToken := 0
	for lenToken < len(input) {
		r, w := utf8.DecodeRune(input[lenToken:])
		if unicode.IsDigit(r) {
			lenToken += w
		} else if lenToken == 0 && r == '+' {
			lenToken += w
		} else if lenToken == 0 && r == '-' {
			lenToken += w
		} else {
			break
		}
	}
	if lenToken == 0 || (lenToken == 1 && (input[0] == '+' || input[0] == '-')) {
		return nil, input
	}
	return input[:lenToken], input[lenToken:]
}

func parse_simple(input []byte) (a Atom, rest []byte, err error) {
	if token, rest := parse_integer(input); token != nil {
		i, err := strconv.ParseInt(string(token), 10, 0)
		return make_int(i), rest, err
	}
	if token, rest := parse_symbol(input); token != nil {
		if bytes.Equal(token, []byte("NIL")) {
			return Atom{}, rest, nil
		}
		return make_sym(token), rest, nil
	}
	panic("!")
}

func parse_symbol(input []byte) (token, rest []byte) {
	lenToken := 0
	for lenToken < len(input) {
		r, w := utf8.DecodeRune(input[lenToken:])
		if unicode.IsLetter(r) {
			lenToken += w
		} else if unicode.IsSpace(r) {
			break
		} else if r == '+' || r == '-' || r == '_' {
			lenToken += w
		} else {
			break
		}
	}
	if lenToken == 0 || (lenToken == 1 && (input[0] == '+' || input[0] == '-')) {
		return nil, input
	}
	return input[:lenToken], input[lenToken:]
}

// print_expr relies on the Atom's stringer to return a text representation of the atom.
func print_expr(a Atom) {
	fmt.Printf("%s", a.String())
}

// read_expr reads the next expression from the input.
// that can be an atom, list, or NIL.
func read_expr(input []byte) (a Atom, rest []byte, err error) {
	token, rest, err := lex(input)
	if err != nil {
		return Atom{}, rest, err
	} else if token[0] == '(' {
		return read_list(rest)
	} else if token[0] == ')' {
		return Atom{}, rest, ErrorSyntax
	}
	// the parser wants to see only the current token and we
	// want to ensure that it parsed the entire token.
	a, excess, err := parse_simple(token)
	if len(excess) != 0 {
		// if we get here, there's a problem with the lex function.
		panic("assert(parse(token) should consume entire token)")
	}
	return a, rest, err
}

// read_list returns a list. it assumes that it is called
// immediately after an opening paren is read in.
// it will read both proper and improper lists.
func read_list(input []byte) (a Atom, rest []byte, err error) {
	var result, tail, item Atom
	for {
		// read a token from the input in the hope that it will
		// be ')' or '.', which we can immediately handle.
		if token, rest, err := lex(input); err != nil {
			return Atom{}, rest, err
		} else if token[0] == ')' {
			// found the end of the list
			return result, rest, nil
		} else if token[0] == '.' && len(token) == 1 {
			// a single dot signals an improper (a/k/a dotted) list,
			// but it must not be the first thing in the list!
			if nilp(tail) {
				return Atom{}, token, ErrorSyntax
			}
			if item, rest, err = read_expr(rest); err != nil {
				return Atom{}, rest, err
			}
			// append to the result
			scdr(tail, item)
			// expect a closing paren
			token, rest, err = lex(rest)
			if err != nil || len(token) == 0 || token[0] != ')' {
				return Atom{}, rest, ErrorSyntax
			}
			return result, rest, nil
		}

		// ignore the token we just lexed and pass the entire input
		// buffer in to read the next expression. we have to because
		// that token could have started a list.
		item, rest, err = read_expr(input)
		if err != nil {
			return Atom{}, rest, err
		} else if nilp(tail) {
			// first item
			result = cons(item, Atom{})
			tail = result
		} else {
			scdr(tail, cons(item, Atom{}))
			tail = cdr(tail)
		}

		// make the rest of the input available for the next loop
		input = rest
	}
}

// scdr set the cdr of a pair. panics if p is not a pair.
func scdr(p, a Atom) { p.pair.atom[1] = a }

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
