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

//revive:disable:exported until fixed

package ch04

import (
	"bytes"
	"errors"
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"
)

func Version() string {
	return "chapter-04"
}

// AtomKind is an enum for the type of data stored in an Atom.

// enums for AtomKind

// Atom is our most primitive unit of storage.

// Pair is a "cons" cell containing two Atoms.

// car returns the car of a pair. panics if p is not a pair.

// cdr returns the cdr of a pair. panics if p is not a pair.

// cons returns a Pair from the heap.

// make_int returns a new Atom with the given integer value.

// make_sym returns an Atom with the given symbol value.
// If the symbol already exists, we return a copy of it from the symbol table.

// nilp is a predicate that returns true if the argument is NIL.

// print_expr relies on the Atom's stringer to return a text representation of the atom.

type AtomType int

const (
	AtomType_Nil AtomType = iota
	AtomType_Pair
	AtomType_Symbol
	AtomType_Integer
	AtomType_Builtin
	AtomType_Closure
	AtomType_Macro
)

func (at AtomType) String() string {
	switch at {
	case AtomType_Nil:
		return "AtomType_Nil"
	case AtomType_Pair:
		return "AtomType_Pair"
	case AtomType_Symbol:
		return "AtomType_Symbol"
	case AtomType_Integer:
		return "AtomType_Integer"
	case AtomType_Builtin:
		return "AtomType_Builtin"
	case AtomType_Closure:
		return "AtomType_Closure"
	case AtomType_Macro:
		return "AtomType_Macro"
	}
	panic(fmt.Sprintf("assert(atom.type != %d)", at))
}

var Error_OK error
var Error_Args = errors.New("args")
var Error_EOF = errors.New("end-of-input")
var Error_Syntax = errors.New("syntax")
var Error_Type = errors.New("type")
var Error_Unbound = errors.New("unbound")

type Atom struct {
	type_ AtomType

	value struct {
		integer int
		pair    *Pair
		symbol  *Symbol
		builtin Builtin
	}
}

type Builtin func(args Atom, result *Atom) error

type Pair struct {
	atom [2]Atom
}

type Symbol struct {
	value string
	name  []byte
}

func car(p Atom) Atom     { return p.value.pair.atom[0] }
func cdr(p Atom) Atom     { return p.value.pair.atom[1] }
func nilp(atom Atom) bool { return atom.type_ == AtomType_Nil }

// func setcar(p, atom Atom) { p.value.pair.atom[0] = atom }
// func setcdr(p, atom Atom) { p.value.pair.atom[1] = atom }

var (
	nil_ = Atom{type_: AtomType_Nil}
	// sym_table is part of the environment and holds the list of currently defined symbols.
	sym_table = Atom{type_: AtomType_Nil}
	// symbols for faster comparison
	sym_apply    Atom
	sym_define   Atom
	sym_defmacro Atom
	sym_if       Atom
	sym_lambda   Atom
	sym_quote    Atom
	sym_t        Atom
)

func cons(car_val, cdr_val Atom) Atom {
	var p Atom
	p.type_ = AtomType_Pair
	p.value.pair = &Pair{}
	p.value.pair.atom[0] = car_val
	p.value.pair.atom[1] = cdr_val
	return p
}

func make_int(x int) Atom {
	var a Atom
	a.type_ = AtomType_Integer
	a.value.integer = x
	return a
}

func strcmp(s1, s2 []byte) int {
	if bytes.Equal(s1, s2) {
		return 0
	}
	return -1
}

func strdup(s []byte) []byte {
	dst := make([]byte, len(s))
	copy(dst, s)
	return dst
}

func make_sym(s []byte) Atom {
	p := sym_table
	for !nilp(p) {
		a := car(p)
		if strcmp(a.value.symbol.name, s) == 0 {
			return a
		}
		p = cdr(p)
	}

	var a Atom
	a.type_ = AtomType_Symbol
	a.value.symbol = &Symbol{value: string(s), name: strdup(s)}
	sym_table = cons(a, sym_table)

	return a
}

func make_builtin(fn Builtin) Atom {
	var a Atom
	a.type_ = AtomType_Builtin
	a.value.builtin = fn
	return a
}

func isbuiltin(p Atom) bool {
	return p.type_ == AtomType_Builtin
}
func isclosure(p Atom) bool {
	return p.type_ == AtomType_Closure
}
func ispair(p Atom) bool {
	return p.type_ == AtomType_Pair
}
func issymbol(p Atom) bool {
	return p.type_ == AtomType_Symbol
}
func make_closure(env, args, body Atom, result *Atom) error {
	if !listp(body) {
		return Error_Syntax
	}

	// Check argument names are all symbols
	for p := args; !nilp(p); p = cdr(p) {
		if issymbol(p) {
			break
		} else if !ispair(p) || !issymbol(car(p)) {
			fmt.Println(161)
			return Error_Type
		}
	}

	*result = cons(env, cons(args, body))
	result.type_ = AtomType_Closure

	return Error_OK
}

func print_expr(atom Atom) {
	switch atom.type_ {
	case AtomType_Nil:
		fmt.Print("nil")
	case AtomType_Pair:
		fmt.Print("(")
		print_expr(car(atom))
		atom = cdr(atom)
		for !nilp(atom) {
			if atom.type_ == AtomType_Pair {
				fmt.Print(" ")
				print_expr(car(atom))
				atom = cdr(atom)
			} else {
				fmt.Print(" . ")
				print_expr(atom)
				break
			}
		}
		fmt.Print(")")
	case AtomType_Symbol:
		fmt.Printf("%s", string(atom.value.symbol.name))
	case AtomType_Integer:
		fmt.Printf("%d", atom.value.integer)
	case AtomType_Builtin:
		fmt.Printf("#<BUILTIN:%v>", atom.value.builtin)
	case AtomType_Closure:
		print_expr(cdr(atom))
	default:
		panic(fmt.Sprintf("assert(atom.type != %d)", atom.type_))
	}
}

// String implements the stringer interface.
func (atom Atom) String() string {
	switch atom.type_ {
	case AtomType_Nil:
		return "nil"
	case AtomType_Pair:
		sb := strings.Builder{}
		sb.WriteByte('(')
		sb.WriteString(car(atom).String())
		for atom = cdr(atom); !nilp(atom); atom = cdr(atom) {
			if !ispair(atom) {
				sb.WriteString(" . ")
				sb.WriteString(atom.String())
				break
			}
			sb.WriteByte(' ')
			sb.WriteString(car(atom).String())
		}
		sb.WriteByte(')')
		return sb.String()
	case AtomType_Symbol:
		return fmt.Sprintf("%s", atom.value.symbol.name) //string(atom.value.symbol.name))
	case AtomType_Integer:
		return fmt.Sprintf("%d", atom.value.integer)
	case AtomType_Builtin:
		return fmt.Sprintf("#<BUILTIN:%v>", atom.value.builtin)
	case AtomType_Closure:
		return cdr(atom).String()
	default:
		panic(fmt.Sprintf("assert(atom.type != %d)", atom.type_))
	}
}

type buffer struct {
	line, col int
	buffer    []byte
}

func (b buffer) IsEOF() bool {
	return len(b.buffer) == 0
}

func (b buffer) Read() (Atom, buffer, error) {
	if b.IsEOF() {
		return nil_, b, Error_EOF
	}

	line, col := b.line, b.col
	var err error
	var stack []Atom
	for !b.IsEOF() {
		// consume whitespace and comments
		r, w := utf8.DecodeRune(b.buffer)
		if unicode.IsSpace(r) { // whitespace
			if r == '\n' {
				b.line, b.col = b.line+1, 0
			}
			b.col, b.buffer = b.col+1, b.buffer[w:]
			continue
		} else if r == ';' { // comments
			for !b.IsEOF() {
				r, w = utf8.DecodeRune(b.buffer)
				if r == '\n' {
					break
				}
				b.col, b.buffer = b.col+1, b.buffer[w:]
			}
			continue
		}

		var lexeme struct {
			line, col int
			value     []byte
		}
		lexeme.line, lexeme.col = b.line, b.col
		lexeme.value = append(lexeme.value, b.buffer[:w]...)
		b.col, b.buffer = b.col+1, b.buffer[w:]

		var expr Atom
		switch r {
		case '(':
			// push a new empty list on to the stack.
			// when we pop it, we must use only the cdr() of it.
			stack = append(stack, cons(nil_, nil_))
			continue
		case ')':
			if len(stack) == 0 {
				return nil_, b, fmt.Errorf("%d:%d: unmatched ')': %w", lexeme.line, lexeme.col, Error_Syntax)
			}
			// pop expression from top of stack and append it to the new top of stack
			expr, stack = cdr(stack[len(stack)-1]), stack[:len(stack)-1]
		case '\'':
			var quoted Atom
			if quoted, b, err = b.Read(); err != nil {
				return nil_, b, err
			}
			expr = cons(make_sym([]byte("quote")), cons(quoted, nil_))
		case '`':
			var quasiQuoted Atom
			if quasiQuoted, b, err = b.Read(); err != nil {
				return nil_, b, err
			}
			expr = cons(make_sym([]byte("quasiquote")), cons(quasiQuoted, nil_))
		case ',':
			name := "unquote"
			if r, w = utf8.DecodeRune(b.buffer); r == '@' {
				b.col, b.buffer = b.col+1, b.buffer[w:]
				name = "unquote-splicing"
			}
			var unquoted Atom
			if unquoted, b, err = b.Read(); err != nil {
				return nil_, b, err
			}
			expr = cons(make_sym([]byte(name)), cons(unquoted, nil_))
		default:
			// must be either an identifier or a value
			for !b.IsEOF() {
				if unicode.IsSpace(rune(b.buffer[0])) || bytes.IndexByte([]byte("()'`,"), b.buffer[0]) != -1 {
					break
				}
				lexeme.value = append(lexeme.value, b.buffer[0])
				b.col, b.buffer = b.col+1, b.buffer[1:]
			}

			if bytes.Equal(lexeme.value, []byte{'.'}) { // Improper list
				if len(stack) == 0 { // dot must be in a list
					return nil_, b, fmt.Errorf("%d:%d: dot not in list: %w", b.line, b.col, Error_Syntax)
				}
				// dot must be followed by a valid expression
				if expr, b, err = b.Read(); err != nil {
					return nil_, b, err
				}
				// and then the token after that expression must be a close paren
				b = b.SkipSpacesAndComments()
				if r, w = utf8.DecodeRune(b.buffer); r != ')' {
					return nil_, b, fmt.Errorf("%d:%d: dot not final in list: %w", b.line, b.col, Error_Syntax)
				}
				// set the cdr of whatever is on top of the stack to this expression
				tail := stack[len(stack)-1]
				for !nilp(cdr(tail)) {
					tail = cdr(tail)
				}
				tail.value.pair.atom[1] = expr
				continue
			} else if bytes.Equal(lexeme.value, []byte{'n', 'i', 'l'}) { // the 'nil' keyworkd
				expr = nil_
			} else if integer, err := strconv.Atoi(string(lexeme.value)); err == nil { // an integer
				expr = make_int(integer)
			} else { // symbol
				expr = make_sym(lexeme.value)
			}
		}

		if len(stack) == 0 { // stand alone expression
			return expr, b, nil
		}
		// append to expression on top of stack
		list_append(stack[len(stack)-1], expr)
	}

	return nil_, b, fmt.Errorf("%d:%d: unexpected end of input: %w", line, col, Error_Syntax)
}

// consume whitespace and comments
func (b buffer) SkipSpacesAndComments() buffer {
	for {
		r, w := utf8.DecodeRune(b.buffer)
		if unicode.IsSpace(r) { // whitespace
			if r == '\n' {
				b.line, b.col = b.line+1, 0
			}
			b.col, b.buffer = b.col+1, b.buffer[w:]
		} else if r == ';' { // comments
			for !b.IsEOF() {
				if r, w = utf8.DecodeRune(b.buffer); r == '\n' {
					break
				}
				b.col, b.buffer = b.col+1, b.buffer[w:]
			}
		} else {
			return b
		}
	}
}

func (b buffer) dump(i int) {
	if b.IsEOF() {
		fmt.Printf("[buffer] %4d/%3d %4d %s\n", b.line, b.col, i, "empty")
	} else if len(b.buffer) < 20 {
		fmt.Printf("[buffer] %4d/%3d %4d %q\n", b.line, b.col, i, string(b.buffer))
	} else {
		fmt.Printf("[buffer] %4d/%3d %4d %q\n", b.line, b.col, i, string(b.buffer[:20]))
	}
}

// env_create adds creates a new environment
func env_create(parent Atom) Atom {
	return cons(parent, nil_)
}

// env_get returns a binding from an environment that looks like
//   (parent-env bindings...)
// where bindings is a list of binding, which look like
//   (symbol . value)
func env_get(env, symbol Atom, result *Atom) error {
	for bs := cdr(env); !nilp(bs); bs = cdr(bs) {
		if b := car(bs); car(b).value.symbol == symbol.value.symbol {
			*result = cdr(b)
			return Error_OK
		}
	}

	parent := car(env)
	if nilp(parent) {
		return fmt.Errorf("symbol %q: %w", string(symbol.value.symbol.name), Error_Unbound)
	}
	return env_get(parent, symbol, result)
}

func env_set(env, symbol, value Atom) error {
	for bs := cdr(env); !nilp(bs); bs = cdr(bs) {
		if b := car(bs); car(b).value.symbol == symbol.value.symbol {
			b.value.pair.atom[1] = value // setcdr
			return Error_OK
		}
	}

	env.value.pair.atom[1] = cons(cons(symbol, value), cdr(env)) // setcdr
	return Error_OK
}

func listp(expr Atom) bool {
	for !nilp(expr) {
		if expr.type_ != AtomType_Pair {
			return false
		}
		expr = cdr(expr)
	}
	return true
}

func copy_list(list Atom) Atom {
	if nilp(list) {
		return nil_
	}

	a := cons(car(list), nil_)
	list = cdr(list)

	for p := a; !nilp(list); p = cdr(p) {
		p.value.pair.atom[1] = cons(car(list), nil_) // setcdr
		list = cdr(list)
	}

	return a
}

func apply(fn, args Atom, result *Atom) error {
	if isbuiltin(fn) {
		return fn.value.builtin(args, result)
	} else if !isclosure(fn) {
		fmt.Println(417)
		return Error_Type
	}

	env, arg_names, body := env_create(car(fn)), car(cdr(fn)), cdr(cdr(fn))

	// Bind the arguments
	for !nilp(arg_names) {
		if arg_names.type_ == AtomType_Symbol {
			env_set(env, arg_names, args)
			args = nil_
			break
		} else if nilp(args) {
			return fmt.Errorf("588: %w", Error_Args)
		}
		env_set(env, car(arg_names), car(args))
		arg_names, args = cdr(arg_names), cdr(args)
	}

	if !nilp(args) {
		return fmt.Errorf("595: %w", Error_Args)
	}

	// Evaluate the body
	for ; !nilp(body); body = cdr(body) {
		if err := eval_expr(car(body), env, result); err != nil {
			return err
		}
	}

	return Error_OK
}

func builtin_car(args Atom, result *Atom) error {
	if nilp(args) || !nilp(cdr(args)) {
		return fmt.Errorf("610: %w", Error_Args)
	} else if nilp(car(args)) {
		*result = nil_
	} else if !ispair(car(args)) {
		fmt.Println(args)
		return fmt.Errorf("690: %w", Error_Type)
	} else {
		*result = car(car(args))
	}

	return Error_OK
}

func builtin_cdr(args Atom, result *Atom) error {
	if nilp(args) || !nilp(cdr(args)) {
		return fmt.Errorf("625: %w", Error_Args)
	} else if nilp(car(args)) {
		*result = nil_
	} else if !ispair(car(args)) {
		fmt.Println(471)
		return Error_Type
	} else {
		*result = cdr(car(args))
	}

	return Error_OK
}

func builtin_cons(args Atom, result *Atom) error {
	if nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args))) {
		return fmt.Errorf("640: %w", Error_Args)
	}
	*result = cons(car(args), car(cdr(args)))
	return Error_OK
}

func isinteger(p Atom) bool {
	return p.type_ == AtomType_Integer
}

func builtin_add(args Atom, result *Atom) error {
	if nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args))) {
		return fmt.Errorf("652: %w", Error_Args)
	}
	a, b := car(args), car(cdr(args))
	if !isinteger(a) || !isinteger(b) {
		return Error_Type
	}
	*result = make_int(a.value.integer + b.value.integer)
	return Error_OK
}

func builtin_subtract(args Atom, result *Atom) error {
	if nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args))) {
		return fmt.Errorf("664: %w", Error_Args)
	}
	a, b := car(args), car(cdr(args))
	if !isinteger(a) || !isinteger(b) {
		fmt.Println(510)
		return Error_Type
	}
	*result = make_int(a.value.integer - b.value.integer)
	return Error_OK
}

func builtin_multiply(args Atom, result *Atom) error {
	if nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args))) {
		return fmt.Errorf("675: %w", Error_Args)
	}
	a, b := car(args), car(cdr(args))
	if !isinteger(a) || !isinteger(b) {
		fmt.Println(523)
		return Error_Type
	}
	*result = make_int(a.value.integer * b.value.integer)
	return Error_OK
}

func builtin_divide(args Atom, result *Atom) error {
	if nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args))) {
		return fmt.Errorf("690: %w", Error_Args)
	}
	a, b := car(args), car(cdr(args))
	if !isinteger(a) || !isinteger(b) {
		fmt.Println(536)
		return Error_Type
	}
	*result = make_int(a.value.integer / b.value.integer)
	return Error_OK
}

func builtin_numeq(args Atom, result *Atom) error {
	if nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args))) {
		return fmt.Errorf("703: %w", Error_Args)
	}
	a, b := car(args), car(cdr(args))
	if !isinteger(a) || !isinteger(b) {
		fmt.Println(549)
		return Error_Type
	} else if a.value.integer == b.value.integer {
		*result = sym_t
	} else {
		*result = nil_
	}
	return Error_OK
}

func builtin_less(args Atom, result *Atom) error {
	if nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args))) {
		return fmt.Errorf("719: %w", Error_Args)
	}
	a, b := car(args), car(cdr(args))
	if !isinteger(a) || !isinteger(b) {
		fmt.Println(565)
		return Error_Type
	} else if a.value.integer < b.value.integer {
		*result = sym_t
	} else {
		*result = nil_
	}
	return Error_OK
}

func builtin_apply(args Atom, result *Atom) error {
	if nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args))) {
		return fmt.Errorf("735: %w", Error_Args)
	}
	fn, args := car(args), car(cdr(args))
	if !listp(args) {
		return Error_Syntax
	}
	return apply(fn, args, result)
}

func builtin_eq(args Atom, result *Atom) error {
	if nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args))) {
		return fmt.Errorf("746: %w", Error_Args)
	}

	a, b := car(args), car(cdr(args))
	*result = nil_ // assume the worst
	if a.type_ == b.type_ {
		switch a.type_ {
		case AtomType_Nil:
			*result = sym_t
		case AtomType_Pair:
			if a.value.pair == b.value.pair {
				*result = sym_t
			}
		case AtomType_Closure:
			if a.value.pair == b.value.pair {
				*result = sym_t
			}
		case AtomType_Macro:
			if a.value.pair == b.value.pair {
				*result = sym_t
			}
		case AtomType_Symbol:
			if a.value.symbol == b.value.symbol {
				*result = sym_t
			}
		case AtomType_Integer:
			if a.value.integer == b.value.integer {
				*result = sym_t
			}
		case AtomType_Builtin:
			// todo: figure this one out
			// eq = (a.value.builtin == b.value.builtin)
			panic(fmt.Sprintf("assert(atom.type != %d)", a.type_))
		default:
			panic(fmt.Sprintf("assert(atom.type != %d)", a.type_))
		}
	}

	return Error_OK
}

func builtin_pairp(args Atom, result *Atom) error {
	if nilp(args) || !nilp(cdr(args)) {
		return fmt.Errorf("789: %w", Error_Args)
	} else if car(args).type_ == AtomType_Pair {
		*result = sym_t
	} else {
		*result = nil_
	}
	return Error_OK
}

func load_file(env Atom, name string, showExpressions bool) error {
	fmt.Printf("Reading %q...\n", name)
	text, err := ioutil.ReadFile(name)
	if err != nil {
		return err
	}
	b := buffer{line: 1, col: 1, buffer: text}
	for !b.IsEOF() {
		b = b.SkipSpacesAndComments()
		line, col := b.line, b.col
		expr, bb, err := b.Read()
		if err != nil {
			return err
		}
		b = bb
		if showExpressions {
			fmt.Printf("repl> %s\n", expr.String())
		}
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			print_err(err)
			fmt.Printf("Error in expression:\n%4d/%2d> %s\n", line, col, expr.String())
		} else {
			fmt.Printf("%s\n", result.String())
		}
	}
	return nil
}

// will panic if list is not a list
func list_append(list, expr Atom) {
	tail := list
	for !nilp(cdr(tail)) {
		tail = cdr(tail)
	}
	tail.value.pair.atom[1] = cons(expr, nil_)
}

func list_get(list Atom, k int) Atom {
	for ; k != 0; k-- {
		list = cdr(list)
	}
	return car(list)
}

func list_set(list Atom, k int, value Atom) {
	for ; k != 0; k-- {
		list = cdr(list)

	}
	list.value.pair.atom[0] = value // setcar
}

func list_reverse(list *Atom) {
	tail := nil_
	for !nilp(*list) {
		p := cdr(*list)
		list.value.pair.atom[1] = tail // setcdr
		tail = *list
		*list = p
	}
	*list = tail
}

func make_frame(parent, env, tail Atom) Atom {
	op, args, body := nil_, nil_, nil_
	return cons(parent, cons(env, cons(op, cons(tail, cons(args, cons(body, nil_))))))
}

func eval_do_exec(stack, expr, env *Atom) error {
	*env = list_get(*stack, 1)
	body := list_get(*stack, 5)
	*expr = car(body)
	body = cdr(body)
	if nilp(body) { // Finished function; pop the stack
		*stack = car(*stack)
	} else {
		list_set(*stack, 5, body)
	}

	return Error_OK
}

func eval_do_bind(stack, expr, env *Atom) error {
	body := list_get(*stack, 5)
	if !nilp(body) {
		return eval_do_exec(stack, expr, env)
	}

	op, args := list_get(*stack, 2), list_get(*stack, 4)
	*env = env_create(car(op))
	arg_names := car(cdr(op))
	body = cdr(cdr(op))
	list_set(*stack, 1, *env)
	list_set(*stack, 5, body)

	// Bind the arguments
	for !nilp(arg_names) {
		if issymbol(arg_names) {
			env_set(*env, arg_names, args)
			args = nil_
			break
		}
		if nilp(args) {
			return fmt.Errorf("not enough arguments: %w", Error_Args)
		}
		env_set(*env, car(arg_names), car(args))
		arg_names = cdr(arg_names)
		args = cdr(args)
	}
	if !nilp(args) {
		return fmt.Errorf("911: %w", Error_Args)
	}

	list_set(*stack, 4, nil_)

	return eval_do_exec(stack, expr, env)
}

func eval_do_apply(stack, expr, env, result *Atom) error {
	op, args := list_get(*stack, 2), list_get(*stack, 4)

	if !nilp(args) {
		list_reverse(&args)
		list_set(*stack, 4, args)
	}

	if issymbol(op) {
		if op.value.symbol == sym_apply.value.symbol { // Replace the current frame
			*stack = car(*stack)
			*stack = make_frame(*stack, *env, nil_)
			op, args = car(args), car(cdr(args))
			if !listp(args) {
				return Error_Syntax
			}
			list_set(*stack, 2, op)
			list_set(*stack, 4, args)
		}
	}

	if isbuiltin(op) {
		*stack = car(*stack)
		*expr = cons(op, args)
		return Error_OK
	} else if !isclosure(op) {
		fmt.Printf("%d: atom.type is %s (%v)\n", 773, op.type_.String(), op)
		return Error_Type
	}

	return eval_do_bind(stack, expr, env)
}

func ismacro(p Atom) bool {
	return p.type_ == AtomType_Macro
}

func eval_do_return(stack, expr, env, result *Atom) error {
	*env = list_get(*stack, 1)
	op := list_get(*stack, 2)
	body := list_get(*stack, 5)

	if !nilp(body) { // Still running a procedure; ignore the result
		return eval_do_apply(stack, expr, env, result)
	}

	var args Atom
	if nilp(op) { // Finished evaluating operator
		op = *result
		list_set(*stack, 2, op)

		if ismacro(op) { // Don't evaluate macro arguments
			args = list_get(*stack, 3)
			*stack = make_frame(*stack, *env, nil_)
			op.type_ = AtomType_Closure
			list_set(*stack, 2, op)
			list_set(*stack, 4, args)
			return eval_do_bind(stack, expr, env)
		}
	} else if issymbol(op) { // Finished working on special form
		if op.value.symbol == sym_define.value.symbol {
			sym := list_get(*stack, 4)
			env_set(*env, sym, *result)
			*stack = car(*stack)
			*expr = cons(sym_quote, cons(sym, nil_))
			return Error_OK
		} else if op.value.symbol == sym_if.value.symbol {
			args = list_get(*stack, 3)
			if nilp(*result) {
				*expr = car(cdr(args))
			} else {
				*expr = car(args)
			}
			*stack = car(*stack)
			return Error_OK
		} else { // Store evaluated argument
			args = list_get(*stack, 4)
			list_set(*stack, 4, cons(*result, args))
		}
	} else if ismacro(op) { // Finished evaluating macro
		*expr = *result
		*stack = car(*stack)
		return Error_OK
	} else { // Store evaluated argument
		args = list_get(*stack, 4)
		list_set(*stack, 4, cons(*result, args))
	}

	args = list_get(*stack, 3)
	if nilp(args) { // No more arguments left to evaluate
		return eval_do_apply(stack, expr, env, result)
	}

	// Evaluate next argument
	*expr = car(args)
	list_set(*stack, 3, cdr(args))
	return Error_OK
}

func eval_expr(expr, env Atom, result *Atom) error {
	var err error
	stack := nil_
	for err == nil {
		if issymbol(expr) {
			err = env_get(env, expr, result)
		} else if !ispair(expr) {
			*result = expr
		} else if !listp(expr) {
			return Error_Syntax
		} else {
			op, args := car(expr), cdr(expr)

			if issymbol(op) { // Handle special forms
				if op.value.symbol == sym_quote.value.symbol {
					if nilp(args) || !nilp(cdr(args)) {
						return fmt.Errorf("1030: %w", Error_Args)
					}
					*result = car(args)
				} else if op.value.symbol == sym_define.value.symbol {
					if nilp(args) || nilp(cdr(args)) {
						return fmt.Errorf("1035: %w", Error_Args)
					}
					sym := car(args)
					if ispair(sym) {
						err = make_closure(env, cdr(sym), cdr(args), result)
						sym = car(sym)
						if !issymbol(sym) {
							return fmt.Errorf("1013: %s: %w", sym.type_.String(), Error_Type)
						}
						env_set(env, sym, *result)
						*result = sym
					} else if issymbol(sym) {
						if !nilp(cdr(cdr(args))) {
							return fmt.Errorf("1049: %w", Error_Args)
						}
						stack = make_frame(stack, env, nil_)
						list_set(stack, 2, op)
						list_set(stack, 4, sym)
						expr = car(cdr(args))
						continue
					} else {
						return fmt.Errorf("1027: %s: %w", op.type_.String(), Error_Type)
					}
				} else if op.value.symbol == sym_lambda.value.symbol {
					if nilp(args) || nilp(cdr(args)) {
						return fmt.Errorf("1062: %w", Error_Args)
					}
					err = make_closure(env, car(args), cdr(args), result)
				} else if op.value.symbol == sym_if.value.symbol {
					if nilp(args) || nilp(cdr(args)) || nilp(cdr(cdr(args))) || !nilp(cdr(cdr(cdr(args)))) {
						return fmt.Errorf("1067: %w", Error_Args)
					}
					stack = make_frame(stack, env, cdr(args))
					list_set(stack, 2, op)
					expr = car(args)
					continue
				} else if op.value.symbol == sym_defmacro.value.symbol {
					var name, macro Atom

					if nilp(args) || nilp(cdr(args)) {
						return fmt.Errorf("1078: %w", Error_Args)
					}

					if !ispair(car(args)) {
						return Error_Syntax
					}

					name = car(car(args))
					if !issymbol(name) {
						return fmt.Errorf("1054: %s: %w", name.type_.String(), Error_Type)
					}

					if err = make_closure(env, cdr(car(args)), cdr(args), &macro); err == nil {
						macro.type_ = AtomType_Macro
						*result = name
						env_set(env, name, macro)
					}
				} else if op.value.symbol == sym_apply.value.symbol {
					if nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args))) {
						return fmt.Errorf("1099: %w", Error_Args)
					}

					stack = make_frame(stack, env, cdr(args))
					list_set(stack, 2, op)
					expr = car(args)
					continue
				} else { // Handle function application
					stack = make_frame(stack, env, args)
					expr = op
					continue
				}
			} else if isbuiltin(op) {
				err = op.value.builtin(args, result)
			} else { // Handle function application
				stack = make_frame(stack, env, args)
				expr = op
				continue
			}
		}

		if nilp(stack) {
			break
		}

		if err == nil {
			err = eval_do_return(&stack, &expr, &env, result)
		}
	}

	return err
}

func print_err(err error) {
	if err == nil || err == Error_OK {
		return
	}
	if errors.Is(err, Error_Args) {
		fmt.Printf("Wrong number of arguments: %+v\n", err)
		return
	}
	if errors.Is(err, Error_Syntax) {
		fmt.Printf("Syntax error: %+v\n", err)
		return
	}
	if errors.Is(err, Error_Type) {
		fmt.Printf("Wrong type: %+v\n", err)
		return
	}
	if errors.Is(err, Error_Unbound) {
		fmt.Printf("Symbol not bound: %+v\n", err)
		return
	}
	panic(fmt.Sprintf("assert(error != %v)", err))
}

func Init() Atom {
	env := env_create(nil_)
	sym_table = nil_

	// Set up the initial environment
	sym_apply = make_sym([]byte("apply"))
	sym_define = make_sym([]byte("define"))
	sym_defmacro = make_sym([]byte("defmacro"))
	sym_if = make_sym([]byte("if"))
	sym_lambda = make_sym([]byte("lambda"))
	sym_quote = make_sym([]byte("quote"))
	sym_t = make_sym([]byte("t"))

	env_set(env, make_sym([]byte("car")), make_builtin(builtin_car))
	env_set(env, make_sym([]byte("cdr")), make_builtin(builtin_cdr))
	env_set(env, make_sym([]byte("cons")), make_builtin(builtin_cons))
	env_set(env, make_sym([]byte("+")), make_builtin(builtin_add))
	env_set(env, make_sym([]byte("-")), make_builtin(builtin_subtract))
	env_set(env, make_sym([]byte("*")), make_builtin(builtin_multiply))
	env_set(env, make_sym([]byte("/")), make_builtin(builtin_divide))
	env_set(env, sym_t, sym_t)
	env_set(env, make_sym([]byte("=")), make_builtin(builtin_numeq))
	env_set(env, make_sym([]byte("<")), make_builtin(builtin_less))
	env_set(env, make_sym([]byte("apply")), make_builtin(builtin_apply))
	env_set(env, make_sym([]byte("eq?")), make_builtin(builtin_eq))
	env_set(env, make_sym([]byte("pair?")), make_builtin(builtin_pairp))

	return env
}

func Run(script string) {
	env := env_create(nil_)

	// Set up the initial environment
	sym_apply = make_sym([]byte("apply"))
	sym_define = make_sym([]byte("define"))
	sym_defmacro = make_sym([]byte("defmacro"))
	sym_if = make_sym([]byte("if"))
	sym_lambda = make_sym([]byte("lambda"))
	sym_quote = make_sym([]byte("quote"))
	sym_t = make_sym([]byte("t"))

	env_set(env, make_sym([]byte("car")), make_builtin(builtin_car))
	env_set(env, make_sym([]byte("cdr")), make_builtin(builtin_cdr))
	env_set(env, make_sym([]byte("cons")), make_builtin(builtin_cons))
	env_set(env, make_sym([]byte("+")), make_builtin(builtin_add))
	env_set(env, make_sym([]byte("-")), make_builtin(builtin_subtract))
	env_set(env, make_sym([]byte("*")), make_builtin(builtin_multiply))
	env_set(env, make_sym([]byte("/")), make_builtin(builtin_divide))
	env_set(env, sym_t, sym_t)
	env_set(env, make_sym([]byte("=")), make_builtin(builtin_numeq))
	env_set(env, make_sym([]byte("<")), make_builtin(builtin_less))
	env_set(env, make_sym([]byte("apply")), make_builtin(builtin_apply))
	env_set(env, make_sym([]byte("eq?")), make_builtin(builtin_eq))
	env_set(env, make_sym([]byte("pair?")), make_builtin(builtin_pairp))

	load_file(env, script, false)
}
