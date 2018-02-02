
**Shelisp** is a very short program that provides mechanisms for composing and running Unix shell (particularly bash) commands and constructs from Common Lisp.

Essentially, it provides a '!' syntax that you can use to run commands and a '[ ]' embedded mode where you can enter bash scripts and obtain the standard output as a lisp string.

Lisp expressions can be included in any command or script using a '?' syntax.

The new version also includes a 'sh' macro that allows to call unix utilities directly with a syntax familiar to the lisp users.


It was written by Alexandru Dan Corlan and distributed on http://dan.corlan.net/shelisp/.

---

# Documentation

From [http://dan.corlan.net/shelisp/shelisp_manual_3_2.pdf](http://dan.corlan.net/shelisp/shelisp_manual_3_2.pdf)


## The bang (`!`) escapes to shell

    !ls

`?` is the "lisp escape". It is followed by an s-expression which is
read, executed and printed (with princ) and the printed result replaces the
`?` and the expression in the shell command. It can be any Lisp expression.

    !echo ?(+ 2 3) zuzu
    5zuzu

## Embedded bash scripts

Anything between square brackets is interpreted as a shell script.

~~~
[echo hi there!]
"hi there!
"
~~~

    (dotimes (i 7) (princ [echo ?i ]) )

## Switching to shell mode: double bang (`!!`)

If you enter a double bang (`!!`) then the prompter is changed to "$" and you can
issue unescaped shell command until you start a line with `!!` again—then
you revert to `lisp mode`.

Constructs with `?` are honored and are read and evaluated immediately
by Lisp. Results of commands are printed immediately after being issued.

```
CL-USER> !!
$ ls
Makefile
shelisp.lisp
shelisp_mn.aux
shelisp_mn.log
shelisp_mn.pdf
shelisp_mn.tex
shelisp_sc.aux
shelisp_sc.log
shelisp_sc.pdf
shelisp_sc.tex
shelisp.tex
spec.txt
$ cat somefile.txt
33.34 is 5.77408 squared.
$ echo "I am almost sure that " ‘cat somefile.txt‘
I am almost sure that 33.34 is 5.77408 squared.
$ !!
$
NIL
CL-USER>
```

Notice how purely Lisp commands, such as variable assignement (bind-
ings), can be escaped with `#` characters as bash comments.

## Run scripts as Lisp calls

The function `script` takes as argument a string and executes it as a
bash script, returning the standard output of the script as a string.

    (script "ls")

## Templates

A template is a string introduced with `#[` and ended with `]#`. It is treated
like an usual string, however `?`-preceded lisp expressions are evaluated and
their result printed inside the string.

For example:

~~~lisp
(defvar *title* "Title of an empty page")
...
(prin1 #[Content-type: text/html
<html>
<head><title> ?*title* </title></head>
<body></body>
</html>
]#)
~~~

Will print to `*standard-output*`:

```
Content-type: text/html
<html>
<head><title>Title of an empty page</title></head>
<body></body>
</html>6
```

## Storable templates

One problem with templates is that we might desire to run them at a later
time, in a different context. For example, we might want to define a variable
with a generic web-page template and then generate actual web pages at
later times, with various contents.

We use `#{` and `}#` for this purpose. In the example below notice that each
time the value of variable A is evaluated, the BB in the evaluation context
is used.

~~~lisp
*(setf bb 9)
9
* (setf a #{ plus: ?bb :sulp }#)
(MIXED-TEMPLATE " plus: " BB " :sulp ")
* (setf bb 10)
10
* (eval a)
" plus: 10 :sulp "
* (setf bb 22)
22
* (eval a)
" plus: 22 :sulp "
* (defun calc-a (bb) (eval a))
CALC-A
* (calc-a 88)
" plus: 88 :sulp "
* (eval a)
" plus: 22 :sulp "
~~~

## The sh macro

This macro aims to provide the use of shell commands with the same syn-
tax as lisp forms, presumably for cases when arguments to commands are
supposed to represent substantial computations in Common Lisp. It allows
calling shell scripts with more lisp-like syntax.

Example call:

    (sh ls -a)

Issues a `ls -a` shell call and returns a string with the standard output.

On the other hand:

    (sh printf "%6.2f" (sqrt 45))

Will print the square root of 45 with two digits in a string.
The first argument after sh, the command name is not evaluated. More-
over, it is converted to lowcase letters, as the lisp reader, by default, interns
symbols in all upcase (printf is interned as PRINTF). Also, symbol argu-
ments starting with ‘-’ are converted to lower case and not interpreted.

All other arguments are interpreted. You can use strings for command
names and any other expression for switches and arguments.

If not sure, use strings. For example, to say `ls -A` enter:

    (sh ls "-A")

If the result of an expression is a symbol, keep in mind that it will be
upper cased by the reader by default.

For example:

~~~lisp
(defvar zz ’(a b c))
(sh echo (cadr zz))
~~~

Will return a string starting with B followed by a newline.

Thus, the macro provides the shortest and most strightforward syntax
for common commands and styles.

## Technical details

Expressions preceded with ‘?’ in the embedded shell scripts (with the ‘[]’
syntax) are evaluated in the context where they appear, at ’eval’ time; ex-
pressions in the ‘bang’ context (with the ‘!’ or the ‘!!’ syntax) are evaluated at
read time, in the context of the last top level form before the form containing
the bangs. This is because the bangs are intended for shell-only commands,
normally given at the top level (command line) with immediate results. The
embedded scripts are supposed to become part of functions or more complex
forms, are parsed at read time and prepared to be executed at runtime.

In the ‘bang’ forms only simple shell commands can be issued as the
reader does not detect the circumstances when a construct (such as ca ‘case’)
occupies more than one line. In the embedded form or with the script
command, any script can be executed.
