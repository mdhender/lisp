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

func TestData(t *testing.T) {
	for _, tc := range []struct {
		name   string
		atom   Atom
		expect string
	}{
		{"a", make_int(42), "42"},
		{"b", make_sym([]byte("foo")), "foo"},
		{"c", cons(make_sym([]byte("x")), make_sym([]byte("y"))), "(x . y)"},
		{"d", cons(make_int(1), cons(make_int(2), cons(make_int(3), nil_))), "(1 2 3)"},
	} {
		a := tc.atom.String()
		if tc.expect != a {
			t.Errorf("%s: expected %q: got %q\n", tc.name, tc.expect, a)
		}
	}
}

func TestParser(t *testing.T) {
	for _, tc := range []struct {
		name   string
		input  string
		expect string
		rest   string
		err    error
	}{
		{"a", "(foo bar)", "(foo bar)", "", nil},
		{"b", ")", "", "", Error_Syntax},
		{"c", "(x .", "", "", Error_EOF},
		{"d", "(x . 5 8)", "", "", Error_Syntax},
		{"e", "42", "42", "", nil},
		{"f", " ( foo  bar ) ", "(foo bar)", " ", nil},
		{"g", "( s (t . u) v . (w . nil))(foo)", "(s (t . u) v w)", "(foo)", nil},
		{"h", "()", "nil", "", nil},
	} {
		expr, rest, err := buffer{buffer: []byte(tc.input)}.Read()
		if err != nil {
			if tc.err == nil {
				t.Errorf("%s: unexpected error: %+v\n", tc.name, err)
			} else if !errors.Is(err, tc.err) {
				t.Errorf("%s: expected %+v: got %+v\n", tc.name, tc.err, err)
			}
			continue
		}
		if got := expr.String(); tc.expect != got {
			t.Errorf("%s: expected %q: got %q\n", tc.name, tc.expect, got)
		}
		if !bytes.Equal([]byte(tc.rest), rest.buffer) {
			t.Errorf("%s: expected rest %q: got %q\n", tc.name, tc.rest, string(rest.buffer))
		}
	}
}

func TestExpressions(t *testing.T) {
	// Specification: Expressions

	// When given a new environment
	env := Init()
	// And the input is 42
	input := buffer{buffer: []byte("foo")}
	// Then evaluating the expression should raise Error_Unbound
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("expressions: unexpected error: %+v\n", err)
	} else {
		var result Atom
		err = eval_expr(expr, env, &result)
		if !errors.Is(err, Error_Unbound) {
			t.Errorf("expressions: expected %+v: got %+v\n", Error_Unbound, err)
		}
	}

	// When given a new environment
	env = Init()
	// And the input is (quote foo)
	input = buffer{buffer: []byte("(quote foo)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("expressions: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return foo
		expected := "foo"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("expressions: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("expressions: expected %q: got %q\n", expected, got)
		}
	}
}

func TestBuiltins(t *testing.T) {
	// Specification: Built-ins

	// When given a new environment
	env := Init()
	// And the input is (car nil)
	input := buffer{buffer: []byte("(car nil)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("builtins: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return nil
		expected := "nil"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("builtins: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("builtins: expected %q: got %q\n", expected, got)
		}
	}

	// When given a new environment
	env = Init()
	// And the input is (car '(1 . 2))
	input = buffer{buffer: []byte("(car '(1 . 2))")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("builtins: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return 2
		expected := "1"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("builtins: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("builtins: expected %q: got %q\n", expected, got)
		}
	}

	// When given a new environment
	env = Init()
	// And the input is (cdr nil)
	input = buffer{buffer: []byte("(cdr nil)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("builtins: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return nil
		expected := "nil"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("builtins: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("builtins: expected %q: got %q\n", expected, got)
		}
	}

	// When given a new environment
	env = Init()
	// And the input is (cdr '(1 2))
	input = buffer{buffer: []byte("(cdr '(1 . 2))")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("builtins: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return 2
		expected := "2"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("builtins: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("builtins: expected %q: got %q\n", expected, got)
		}
	}

	// When given a new environment
	env = Init()
	// And the input is (cons 1 2)
	input = buffer{buffer: []byte("(cons 1 2)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("builtins: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return (1 . 2)
		expected := "(1 . 2)"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("builtins: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("builtins: expected %q: got %q\n", expected, got)
		}
	}

	// When given a new environment
	env = Init()
	// And the input is (define foo 1)
	input = buffer{buffer: []byte("(define foo 1)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("builtins: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return foo
		expected := "foo"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("builtins: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("builtins: expected %q: got %q\n", expected, got)
		}
	}

	// When given the previous environment
	// And the input is (define bar 2)
	input = buffer{buffer: []byte("(define bar 2)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("builtins: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return bar
		expected := "bar"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("builtins: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("builtins: expected %q: got %q\n", expected, got)
		}
	}

	// When given the previous environment
	// And the input is (cons foo bar)
	input = buffer{buffer: []byte("(cons foo bar)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("builtins: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return (1 . 2)
		expected := "(1 . 2)"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("builtins: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("builtins: expected %q: got %q\n", expected, got)
		}
	}

	// When given the previous environment
	// And the input is (define baz (quote (a b c)))
	input = buffer{buffer: []byte("(define baz (quote (a b c)))")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("builtins: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return baz
		expected := "baz"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("builtins: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("builtins: expected %q: got %q\n", expected, got)
		}
	}

	// When given the previous environment
	// And the input is (car baz)
	input = buffer{buffer: []byte("(car baz)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("builtins: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return a
		expected := "a"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("builtins: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("builtins: expected %q: got %q\n", expected, got)
		}
	}

	// When given the previous environment
	// And the input is (cdr baz)
	input = buffer{buffer: []byte("(cdr baz)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("builtins: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return (b c)
		expected := "(b c)"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("builtins: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("builtins: expected %q: got %q\n", expected, got)
		}
	}
}

func TestArithmetic(t *testing.T) {
	// Specification: Arithmetic

	// When given a new environment
	env := Init()
	// And the input is (+ 1 1)
	input := buffer{buffer: []byte("(+ 1 1)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("arithmetic: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return 2
		expected := "2"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("arithmetic: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("arithmetic: expected %q: got %q\n", expected, got)
		}
	}

	// When given the prior environment
	// And the input is (define x (* 6 9))
	input = buffer{buffer: []byte("(define x (* 6 9))")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("arithmetic: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return x
		expected := "x"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("arithmetic: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("arithmetic: expected %q: got %q\n", expected, got)
		}
	}

	// When given the prior environment
	// And the input is x
	input = buffer{buffer: []byte("x")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("arithmetic: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return 54
		expected := "54"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("arithmetic: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("arithmetic: expected %q: got %q\n", expected, got)
		}
	}

	// When given the prior environment
	// And the input is (- x 12)
	input = buffer{buffer: []byte("(- x 12)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("arithmetic: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return 42
		expected := "42"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("arithmetic: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("arithmetic: expected %q: got %q\n", expected, got)
		}
	}
}

func TestLambda(t *testing.T) {
	// Specification: Lambda

	// When given a new environment
	env := Init()
	// And the input is (define square (lambda (x) (* x x)))
	input := buffer{buffer: []byte("(define square (lambda (x) (* x x)))")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("lambda: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return square
		expected := "square"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("lambda: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("lambda: expected %q: got %q\n", expected, got)
		}
	}

	// When given the prior environment
	// And the input is (square 3)
	input = buffer{buffer: []byte("(square 3)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("lambda: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return 9
		expected := "9"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("lambda: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("lambda: expected %q: got %q\n", expected, got)
		}
	}

	// When given the prior environment
	// And the input is (square (square 2))
	input = buffer{buffer: []byte("(square (square 2))")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("lambda: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return 16
		expected := "16"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("lambda: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("lambda: expected %q: got %q\n", expected, got)
		}
	}

	// When given a new environment
	env = Init()
	// And the input is ((lambda (x) (- x 2)) 7)
	input = buffer{buffer: []byte("((lambda (x) (- x 2)) 7)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("lambda: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return 5
		expected := "5"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("lambda: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("lambda: expected %q: got %q\n", expected, got)
		}
	}

	// When given a new environment
	env = Init()
	// And the input is (define make-adder (lambda (x) (lambda (y) (+ x y))))
	input = buffer{buffer: []byte("(define make-adder (lambda (x) (lambda (y) (+ x y))))")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("lambda: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return make-adder
		expected := "make-adder"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("lambda: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("lambda: expected %q: got %q\n", expected, got)
		}
	}

	// When given the prior environment
	// And the input is (define add-two (make-adder 2))
	input = buffer{buffer: []byte("(define add-two (make-adder 2))")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("lambda: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return add-two
		expected := "add-two"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("lambda: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("lambda: expected %q: got %q\n", expected, got)
		}
	}

	// When given the prior environment
	// And the input is (add-two 5)
	input = buffer{buffer: []byte("(add-two 5)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("lambda: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return 7
		expected := "7"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("lambda: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("lambda: expected %q: got %q\n", expected, got)
		}
	}
}

func TestBooleans(t *testing.T) {
	// Specification: Booleans

	// When given a new environment
	env := Init()
	// And the input is (if t 3 4)
	input := buffer{buffer: []byte("(if t 3 4)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("booleans: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return 3
		expected := "3"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("booleans: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("booleans: expected %q: got %q\n", expected, got)
		}
	}

	// When given a new environment
	env = Init()
	// And the input is (if nil 3 4)
	input = buffer{buffer: []byte("(if nil 3 4)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("booleans: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return 4
		expected := "4"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("booleans: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("booleans: expected %q: got %q\n", expected, got)
		}
	}

	// When given a new environment
	env = Init()
	// And the input is (if 0 t nil)
	input = buffer{buffer: []byte("(if 0 t nil)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("booleans: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return t
		expected := "t"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("booleans: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("booleans: expected %q: got %q\n", expected, got)
		}
	}

	// When given a new environment
	env = Init()
	// And the input is (= 3 3)
	input = buffer{buffer: []byte("(= 3 3)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("booleans: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return t
		expected := "t"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("booleans: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("booleans: expected %q: got %q\n", expected, got)
		}
	}

	// When given a new environment
	env = Init()
	// And the input is (< 11 4)
	input = buffer{buffer: []byte("(< 11 4)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("booleans: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return nil
		expected := "nil"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("booleans: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("booleans: expected %q: got %q\n", expected, got)
		}
	}

	// When given a new environment
	env = Init()
	// And the input is (define fact (lambda (x) (if (= x 0) 1 (* x (fact (- x 1))))))
	input = buffer{buffer: []byte("(define fact (lambda (x) (if (= x 0) 1 (* x (fact (- x 1))))))")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("booleans: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return fact
		expected := "fact"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("booleans: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("booleans: expected %q: got %q\n", expected, got)
		}
	}

	// When given the prior environment
	// And the input is (fact 10)
	input = buffer{buffer: []byte("(fact 10)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("booleans: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return 3628800
		expected := "3628800"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("booleans: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("booleans: expected %q: got %q\n", expected, got)
		}
	}
}

func TestSyntacticSugar(t *testing.T) {
	// Specification: SyntacticSugar

	// When given a new environment
	env := Init()
	// And the input is 'expr
	input := buffer{buffer: []byte("'expr'")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
	} else {
		// Then printing the expression should return (quote expr)
		expected := "(quote expr)"
		if got := expr.String(); got != expected {
			t.Errorf("syntacticSugar: expected %q: got %q\n", expected, got)
		} else {
			// And evaluating the expression should return expr
			expected = "expr"
			var result Atom
			if err = eval_expr(expr, env, &result); err != nil {
				t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
			} else if got := result.String(); got != expected {
				t.Errorf("syntacticSugar: expected %q: got %q\n", expected, got)
			}
		}
	}

	// When given a new environment
	env = Init()
	// And the input is 'foo
	input = buffer{buffer: []byte("'foo'")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
	} else {
		// Then printing the expression should return (quote foo)
		expected := "(quote foo)"
		if got := expr.String(); got != expected {
			t.Errorf("syntacticSugar: expected %q: got %q\n", expected, got)
		} else {
			// And evaluating the expression should return expr
			expected = "foo"
			var result Atom
			if err = eval_expr(expr, env, &result); err != nil {
				t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
			} else if got := result.String(); got != expected {
				t.Errorf("syntacticSugar: expected %q: got %q\n", expected, got)
			}
		}
	}

	// When given a new environment
	env = Init()
	// And the input is '(+ 1 2)
	input = buffer{buffer: []byte("'(+ 1 2)'")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
	} else {
		// Then printing the expression should return (quote (+ 1 2))
		expected := "(quote (+ 1 2))"
		if got := expr.String(); got != expected {
			t.Errorf("syntacticSugar: expected %q: got %q\n", expected, got)
		} else {
			// And evaluating the expression should return (+ 1 2)
			expected = "(+ 1 2)"
			var result Atom
			if err = eval_expr(expr, env, &result); err != nil {
				t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
			} else if got := result.String(); got != expected {
				t.Errorf("syntacticSugar: expected %q: got %q\n", expected, got)
			}
		}
	}

	// When given a new environment
	env = Init()
	// And the input is '(a . b)
	input = buffer{buffer: []byte("'(a . b)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
	} else {
		// Then printing the expression should return (quote (a . b))
		expected := "(quote (a . b))"
		if got := expr.String(); got != expected {
			t.Errorf("syntacticSugar: expected %q: got %q\n", expected, got)
		} else {
			// And evaluating the expression should return (a . b)
			expected = "(a . b)"
			var result Atom
			if err = eval_expr(expr, env, &result); err != nil {
				t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
			} else if got := result.String(); got != expected {
				t.Errorf("syntacticSugar: expected %q: got %q\n", expected, got)
			}
		}
	}

	// When given a new environment
	env = Init()
	// And the input is (define x '(a b c))
	input = buffer{buffer: []byte("(define x '(a b c))")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return x
		expected := "x"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("syntacticSugar: expected %q: got %q\n", expected, got)
		}
	}

	// When given the prior environment
	// And the input is x
	input = buffer{buffer: []byte("x")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return (a b c)
		expected := "(a b c)"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("syntacticSugar: expected %q: got %q\n", expected, got)
		}
	}

	// When given the prior environment
	// And the input is 'x
	input = buffer{buffer: []byte("'x")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return x
		expected := "x"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("syntacticSugar: expected %q: got %q\n", expected, got)
		}
	}

	// When given the prior environment
	// And the input is (define foo 'bar)
	input = buffer{buffer: []byte("(define foo 'bar)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return foo
		expected := "foo"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("syntacticSugar: expected %q: got %q\n", expected, got)
		}
	}

	// When given the prior environment
	// And the input is foo
	input = buffer{buffer: []byte("foo")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return bar
		expected := "bar"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("syntacticSugar: expected %q: got %q\n", expected, got)
		}
	}

	// When given the prior environment
	// And the input is ''()
	input = buffer{buffer: []byte("''()")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return (quote nil)
		expected := "(quote nil)"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("syntacticSugar: expected %q: got %q\n", expected, got)
		}
	}

	// When given a new environment
	env = Init()
	// And the input is (define (square x) (* x x))
	input = buffer{buffer: []byte("(define (square x) (* x x))")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return square
		expected := "square"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("syntacticSugar: expected %q: got %q\n", expected, got)
		}
	}

	// When given the prior environment
	// And the input is (square (square 3))
	input = buffer{buffer: []byte("(square (square 3))")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return 81
		expected := "81"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("syntacticSugar: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("syntacticSugar: expected %q: got %q\n", expected, got)
		}
	}
}

func TestVariadics(t *testing.T) {
	// Specification: Variadics

	// When given a new environment
	env := Init()
	// And the input is ((lambda (a . b) a) 1 2 3)
	input := buffer{buffer: []byte("((lambda (a . b) a) 1 2 3)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("variadics: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return 1
		expected := "1"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("variadics: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("variadics: expected %q: got %q\n", expected, got)
		}
	}

	// When given a new environment
	env = Init()
	// And the input is ((lambda (a . b) b) 1 2 3)
	input = buffer{buffer: []byte("((lambda (a . b) b) 1 2 3)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("variadics: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return (2 3)
		expected := "(2 3)"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("variadics: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("variadics: expected %q: got %q\n", expected, got)
		}
	}

	// When given a new environment
	env = Init()
	// And the input is ((lambda args args) 1 2 3)
	input = buffer{buffer: []byte("((lambda args args) 1 2 3)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("variadics: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return (1 2 3)
		expected := "(1 2 3)"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("variadics: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("variadics: expected %q: got %q\n", expected, got)
		}
	}

	// When given a new environment
	env = Init()
	// And the input is (define (sum-list xs) (if xs (+ (car xs) (sum-list (cdr xs))) 0))
	input = buffer{buffer: []byte("(define (sum-list xs) (if xs (+ (car xs) (sum-list (cdr xs))) 0))")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("variadics: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return sum-list
		expected := "sum-list"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("variadics: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("variadics: expected %q: got %q\n", expected, got)
		}
	}

	// When given the prior environment
	// And the input is (define (sum-list xs) (if xs (+ (car xs) (sum-list (cdr xs))) 0))
	input = buffer{buffer: []byte("(sum-list '(1 2 3))")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("variadics: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return 6
		expected := "6"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("variadics: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("variadics: expected %q: got %q\n", expected, got)
		}
	}

	// When given the prior environment
	// And the input is (define (add . xs) (sum-list xs))
	input = buffer{buffer: []byte("(define (add . xs) (sum-list xs))")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("variadics: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return add
		expected := "add"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("variadics: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("variadics: expected %q: got %q\n", expected, got)
		}
	}

	// When given the prior environment
	// And the input is (add 1 2 3)
	input = buffer{buffer: []byte("(add 1 2 3)")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("variadics: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return 6
		expected := "6"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("variadics: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("variadics: expected %q: got %q\n", expected, got)
		}
	}

	// When given the prior environment
	// And the input is (add 1 (- 4 2) (/ 9 3))
	input = buffer{buffer: []byte("(add 1 (- 4 2) (/ 9 3))")}
	if expr, _, err := input.Read(); err != nil {
		t.Errorf("variadics: unexpected error: %+v\n", err)
	} else {
		// Then evaluating the expression should return 6
		expected := "6"
		var result Atom
		if err = eval_expr(expr, env, &result); err != nil {
			t.Errorf("variadics: unexpected error: %+v\n", err)
		} else if got := result.String(); got != expected {
			t.Errorf("variadics: expected %q: got %q\n", expected, got)
		}
	}
}
