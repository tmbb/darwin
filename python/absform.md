8 The Abstract Format
======================

This section describes the standard representation of parse trees for Erlang programs as Erlang terms. This representation s known as the **abstract format**. Functions dealing with such parse trees are [compile:forms/1,2](../../man/compile.html#forms-1) and functions in the following modules:

* [`epp(3)`](../../man/epp.html)
* [`erl\_eval(3)`](../../man/erl_eval.html)
* [`erl\_lint(3)`](../../man/erl_lint.html)
* [`erl\_parse(3)`](../../man/erl_parse.html)
* [`erl\_pp(3)`](../../man/erl_pp.html)
* [`io(3)`](../../man/io.html

The functions are also used as input and output for parse transforms, see the [`compile(3)`](../../man/compile.html) module.
We use the function `Rep` to denote the mapping from an Erlang source construct `C` to its abstract format representation `R`, and write `R = Rep(C)`.

The word `LINE` in this section represents an ineger, and denotes the number of the line in the source file where the construction occurred. Several instances of `LINE` in the same construction can denote different lines.

As operators are not terms in their ow right, when operators are mentioned below, the representation of an operator is to be taken to be the atom with a printname consisting of the same characters as the operator.

### [](https://github.com/erlang/otp/dit/maint/erts/doc/src/absform.xml#L75 "Found an issue with the documentation? Fix it by clicking here!")[8.1. Module Declarations and Forms](#module-declarations-and-forms)

A module declaration consists of a sequence of forms, which are either function declarations or attributes.

* If D is a module declaration consisting of the forms `F\_1`, ..., `F\_k`, then Rep(D) = `[Rep(F\_1), ..., Rep(F\_k)]`.

* If F is an attribute `-export([Fun\_1/A\_1, ..., Fun\_k/A\_k])`, then Rep(F) = `{attribute,LINE,export,[{Fun\_1,A\_1}, ..., {Fun\_k,A\_k}]}`.

* If F is an attribute `-import(Mod,[Fun\_1/A\_1, ..., Fun\_k/A\_k])`, then Rep(F) = `{attribute,LINE,import,{Mod,[{Fun\_1,A\_1}, ..., {Fun\_k,A\_k}]}}`.

* If F is an attribute `-module(Mod)`, then Rep(F) = `{attribute,LINE,module,Mod}`.

* If F is an attribute `-file(File,Line)`, then Rep(F) = `{attribute,LINE,file,{File,Line}}`.

* If F is a function declaration `Name Fc\_1 ; ... ; Name Fc\_k`, where each `Fc\_i` isa function clause with a pattern sequence of the same length `Arity`, then Rep(F) = `{function,LINE,Name,Arity,[Rep(Fc\_1), ...,Rep(Fc\_k)]}`.

* If F is a function specification `-Spec Name Ft\_1; ...; Ft\_k`, where `Spec` is either the atom `spec` or the atom `callback`, and each `Ft\_i` is a possiblyconstrained function type with an argument sequence of the same length `Arity`, then Rep(F) = `{attribute,Line,Spec,{{Name,Arity},[Rep(Ft\_1), ..., Rep(Ft\_k)]}}`.

* If F is a function specification `-spec Mod:Name Ft\_1; ...; Ft\_k`, where each `Ft\_i` is a possibly constrained function type with an argument sequence of the samelength `Arity`, then Rep(F) = `{attribute,Line,spec,{{Mod,Name,Arity},[Rep(Ft\_1), ..., Rep(Ft\_k)]}}`.

* If F is a record declaration `-record(Name,{V\_1, ..., V\_k})`, where each `V\_i` isa record field, then Rep(F) = `{attribute,LINE,record,{Name,[Rep(V\_1), ..., Rep(V\_k)]}}`. For Rep(V), see below.

* If F is a type declaration `-Type Name(V\_1, ..., V\_k) :: T`, where `Type` is eitherthe atom `type` or the atom `opaque`, each `V\_i` is a variable, and T is a type, then Rep(F) = `{attribute,LINE,Type,{Name,Rep(T),[Rep(V\_1), ..., Rep(V\_k)]}}`.

* If F is a wild attribute `-A(T)`, then Rep(F) = `{attribute,LINE,A,T}`.

#### [](https:/github.com/erlang/otp/edit/maint/erts/doc/src/absform.xml#L148 "Found an issue with the documentation? Fix it by clicking here!")[Record Fields](#record-fields)

Each field in a record declaration can have an optional, explicit, default initializer expression, and an optional type.

* If V is `A`, then Rep(V) = `{record\_field,LINE,Rep(A)}`.

* If V is `A = E`, where `E` is an expression, then Rep(V) = `{record\_field,LINE,Rep(A),Rep(E)}`.

* If V is `A :: T`, where `T` is a type, then Rep(V) = `{typed\_record\_field,{record\_field,LINE,Rep(A)},Rep(T)}`.

* If V is `A = E :: T`, where `E` is an expression and `T` is a type, then Rep(V) = `{typed\_record\_field,{record\_field,LINE,Rep(A),Rep(E)},Rep(T)}`.

#### [](https://github.com/erlang/otp/edit/maint/erts/doc/src/absform.xl#L176 "Found an issue with the documentation? Fix it by clicking here!")[Representation of Parse Errors and End-of-File](#representation-of-parse-errors-and-end-of-file)

In addition to the representations of forms, the list thatrepresents a module declaration (as returned by functions in [`epp(3)`](../../man/epp.html) and [`erl\_parse(3)`](../../man/erl_parse.html)) can contain the following:
* Tuples `{error,E}` and `{warning,W}`, denoting syntactically incorrect forms and warnings.

* `{eof,LOCATION}`, denoting an end-of-stream encountered before a complete form hadbeen parsed. The word `LOCATION` represents an integer, and denotes the number of the last line in the source file.


### [](https://gihub.com/erlang/otp/edit/maint/erts/doc/src/absform.xml#L201 "Found an issue with the documentation? Fix it by clicking here!")[8.2. Atomic Literals](#atomic-literals)

There are five kinds of atomic literals, which are represented in the same way in patterns, expressions, and guards:

* If L is an atom literal, then Rep(L) = `{atom,LINE,L}`.

* If L is a character literal, then Rep(L) = `{char,LINE,L}`.

* If L is a float literal, then Rep(L) = `{float,LINE,L}`.

* If L is an integer literal, then Rep(L) = `{integer,LINE,L}`.

* If L is a string literal consisting of the characters `C\_1`, ..., `C\_k`, then Rep(L) = `{string,LINE,[C\_1, ..., C\_k]}`.

Notice that negative integer and float literals do not occur as such; they are parsed as an application of the unary negation operator.

### [](https://github.com/erlang/otp/edit/maint/erts/doc/src/absform.xml#L231 "Found an issue with the documentation? Fix it by clicking here!")[8.3. Patterns](#patterns)

If Ps is a sequence of patterns `P\_1, ..., P\_k`, then Rep(Ps) = `[Rep(P\_1), ..., Rep(P\_k)]`. Such sequences occur as the list of arguments to a function or fun.

Individual patterns are represented as follows:

* If P is an atomic literal `L`, then Rep(P) = Rep(L).

* If P is a bitstring pattern `<<P\_1:Size\_1/TSL\_1, ..., P\_k:Size\_k/TSL\_k>>`, where each `Size\_i` is an expression that can be evaluated to an integer, and each `TSL\_i` is a type specificer list, then Rep(P) = `{bin,LINE,[{bin\_element,LINE,Rep(P\_1),Rep(Size\_1),Rep(TSL\_1)}, ..., {bin\_element,LINE,Rep(P\_k),Rep(Size\_k),Re(TSL\_k)}]}`. For Rep(TSL), see below. An omitted `Size\_i` is represented by `default`. An omitted `TSL\_i` is represented by `default`.

* If P is a compound pattern `P\_1 = P\_2`, then Rep(P) = `{match,LINE,Rep(P\_1),Rep(P\_2)}`.

* If P is a cons pattern `[P\_h | P\_t]`, then Rep(P) = `{cons,LINE,Rep(P\_h),Rep(P\_t)}`.

* If P is a map pattern `#{A\_1, ..., A\_k}`, where each `A\_i` is an association`P\_i\_1 := P\_i\_2`, then Rep(P) = `{map,LINE,[Rep(A\_1), ..., Rep(A\_k)]}`. For Rep(A), see below.

* If P is a nil pattern `[]`, then Rep(P) = `{nil,LINE}`.

* If P is an operator pattern `P\_1 Op P\_2`, where `Op` is a binary operator (this is either an occurrence of `++` applied to a literal string or character list, or anoccurrence of an expression that can be evaluated to a number at compile time), then Rep(P) = `{op,LINE,Op,Rep(P\_1),Rep(P\_2)}`.

* If P is an operator pattern `Op P\_0`, where `Op` is a unary operator (this is anoccurrence of an expression that can be evaluated to a number at compile time), then Rep(P) = `{op,LINE,Op,Rep(P\_0)}`.

* If P is a parenthesized pattern `( P\_0 )`, then Rep(P) = `Rep(P\_0)`, that is, parenthesized patterns cannot be distinguished from their bodies.

* If P is a record field index pattern `#Name.Field`, where `Field` is an atom, then Rep(P) = `{record\_index,LINE,Name,Rep(Field)}`.

* If P is a record pattern `#Name{Field\_1=P\_1, ..., Field\_k=P\_k}`, where each`Field\_i` is an atom or `\_`, then Rep(P) = `{record,LINE,Name,[{record\_field,LINE,Rep(Field\_1),Rep(P\_1)}, ..., {record\_field,LINE,Rep(Field\_k),Rep(P\_k)}]}`.

* If P is a tuple pattern `{P\_1, ..., P\_k}`, then Rep(P) = `{tuple,LINE,[Rep(P\_1), ..., Rep(P\_k)]}`.

* If P is a universal pattern `\_`, then Rep(P) = `{var,LINE,'\_'}`.

* If P is a variable pattern `V`, then Rep(P) = `{var,LINE,A}`, where A is an atom with a printname consisting of the same characters as `V`.

Notice that every pattern has the same source form as some expression, and is represented in the same way as the corresponding expression.

### [](https://github.com/erlang/otp/edit/maint/erts/doc/src/absform.xml#L319 "Found an issue with the documentation? Fix it by clicking here!")[8.4. Expressions](#expressions)

A body B is a non-empty sequence of expressions `E\_1, ..., E\_k`, and Rep(B) = `[Rep(E\_1), ..., Rep(E\_k)]`

An expression E is one of the following:
* If E is an atomic literal `L`, then Rep(E) = Rep(L).

* If E is a bitstring comprehension `<<E\_0 || Q\_1, ..., Q\_k>>`, where each `Q\_i` isa qualifier, then Rep(E) = `{bc,LINE,Rep(E\_0),[Rep(Q\_1), ..., Rep(Q\_k)]}`. For Rep(Q), see below.

* If E is a bitstring constructor `<<E\_1:Size\_1/TSL\_1, ..., E\_k:Size\_k/TSL\_k>>`, where each `Size\_i` is an expression and each `TSL\_i` is a type specificer list, then Rep(E) = `{bin,LINE,[{bin\_element,LINE,Rep(E\_1),Rep(Size\_1),Rep(TSL\_1)}, ..., {bin\_element,LINE,Rep(E\_k),Rep(Size\_k),Rep(TSL\_k)}]}`. For Rep(TSL), see below. Anomitted `Size\_i` is represented by `default`. An omitted `TSL\_i` is represented by `default`.

* If E is a block expression `begin B end`, where `B` is a body, then Rep(E) = `{block,LINE,Rep(B)}`.

* If E is a case expression `case E\_0 of Cc\_1 ; ... ; Cc\_k end`, where `E\_0` is anexpression and each `Cc\_i` is a case clause, then Rep(E) = `{'case',LINE,Rep(E\_0),[Rep(Cc\_1), ..., Rep(Cc\_k)]}`.

* If E is a catch expression `catch E\_0`, then Rep(E) = `{'catch',LINE,Rep(E\_0)}`.

* If E is a cons skeleton `[E\_h | E\_t]`, then Rep(E) = `{cons,LINE,Rep(E\_h),Rep(E\_t)}`.

* If E is a fun expression `fun Name/Arity`, then Rep(E) = `{'fun',LINE,{function,Name,Arity}}`.

* If E is a fun expression `fun Module:Name/Arity`, then Rep(E) = `{'fun',LINE{function,Rep(Module),Rep(Name),Rep(Arity)}}`. (Before Erlang/OTP R15: Rep(E) = `{'fun',LINE,{function,Module,Name,Arity}}`.)

* If E is a fun expression `fun Fc\_1 ; ... ; Fc\_k end`, where each `Fc\_i` is a function clause, then Rep(E) = `{'fun',LINE,{clauses,[Rep(Fc\_1), ..., Rep(Fc\_k)]}}`.

* If E is a fun expression `fun Name Fc\_1 ; ... ; Name Fc\_k end`, where `Name` is avariable and each `Fc\_i` is a function clause, then Rep(E) = `{named\_fun,LINE,Name,[Rep(Fc\_1), ..., Rep(Fc\_k)]}`.

* If E is a function call `E\_0(E\_1, ..., E\_k)`, then Rep(E) = `{call,LINE,Rep(E\_0),[Rep(E\_1), ..., Rep(E\_k)]}`.

* If E is a function call `E\_m:E\_0(E\_1, ..., E\_k)`, then Rep(E) = `{call,LINE,{remote,LINE,Rep(E\_m),Rep(E\_0)},[Rep(E\_1), ..., Rep(E\_k)]}`.

* If E is an if expression `if Ic\_1 ; ... ; Ic\_k end`, where each `Ic\_i` is an if clause, then Rep(E) = `{'if',LINE,[Rep(Ic\_1), ..., Rep(Ic\_k)]}`.

* If E is a list comprehension `[E\_0 || Q\_1, ..., Q\_k]`, where each `Q\_i` is aqualifier, then Rep(E) = `{lc,LINE,Rep(E\_0),[Rep(Q\_1), ..., Rep(Q\_k)]}`. For Rep(Q), see below.

* If E is a map creation `#{A\_1, ..., A\_k}`, where each `A\_i` is an association`E\_i\_1 => E\_i\_2`, then Rep(E) = `{map,LINE,[Rep(A\_1), ..., Rep(A\_k)]}`. For Rep(A), see below.

* If E is a map update `E\_0#{A\_1, ..., A\_k}`, where each `A\_i` is an association`E\_i\_1 => E\_i\_2` or `E\_i\_1 := E\_i\_2`, then Rep(E) = `{map,LINE,Rep(E\_0),[Rep(A\_1), ..., Rep(A\_k)]}`. For Rep(A), see below.

* If E is a match operator expression `P = E\_0`, where `P` is a pattern, then Rep(E) = `{match,LINE,Rep(P),Rep(E\_0)}`.

* If E is nil, `[]`, then Rep(E) = `{nil,LINE}`.

* If E is an operator expression `E\_1 Op E\_2`, where `Op` is a binary operator other than match operator `=`, then Rep(E) = `{op,LINE,Op,Rep(E\_1),Rep(E\_2)}`.

* If E is an operator expression `Op E\_0`, where `Op` is a unary operator, then Rep(E) = `{op,LINE,Op,Rep(E\_0)}`.

* If E is a parenthesized expression `( E\_0 )`, then Rep(E) = `Rep(E\_0)`, that is, parenthesized expressions cannot be distinguished from their bodies.

* If E is a receive expression `receive Cc\_1 ; ... ; Cc\_k end`, where each `Cc\_i` is a case clause, then Rep(E) = `{'receive',LINE,[Rep(Cc\_1), ..., Rep(Cc\_k)]}`.

* If E is a receive expression `receive Cc\_1 ; ... ; Cc\_k after E\_0 -> B\_t end`,where each `Cc\_i` is a case clause, `E\_0` is an expression, and `B\_t` is a body, then Rep(E) = `{'receive',LINE,[Rep(Cc\_1), ..., Rep(Cc\_k)],Rep(E\_0),Rep(B\_t)}`.

* If E is a record creation `#Name{Field\_1=E\_1, ..., Field\_k=E\_k}`, where each`Field\_i` is an atom or `\_`, then Rep(E) = `{record,LINE,Name,[{record\_field,LINE,Rep(Field\_1),Rep(E\_1)}, ..., {record\_field,LINE,Rep(Field\_k),Rep(E\_k)}]}`.

* If E is a record field access `E\_0#Name.Field`, where `Field` is an atom, then Rep(E) = `{record\_field,LINE,Rep(E\_0),Name,Rep(Field)}`.

* If E is a record field index `#Name.Field`, where `Field` is an atom, then Rep(E) = `{record\_index,LINE,Name,Rep(Field)}`.

* If E is a record update `E\_0#Name{Field\_1=E\_1, ..., Field\_k=E\_k}`, where each`Field\_i` is an atom, then Rep(E) = `{record,LINE,Rep(E\_0),Name,[{record\_field,LINE,Rep(Field\_1),Rep(E\_1)}, ..., {record\_field,LINE,Rep(Field\_k),Rep(E\_k)}]}`.

* If E is a tuple skeleton `{E\_1, ..., E\_k}`, then Rep(E) = `{tuple,LINE,[Rep(E\_1), ..., Rep(E\_k)]}`.

* If E is a try expression `try B catch Tc\_1 ; ... ; Tc\_k end`, where `B` is a bodyand each `Tc\_i` is a catch clause, then Rep(E) = `{'try',LINE,Rep(B),[],[Rep(Tc\_1), ..., Rep(Tc\_k)],[]}`.

* If E is a try expression `try B of Cc\_1 ; ... ; Cc\_k catch Tc\_1 ; ... ; Tc\_n end`, where `B` is a body, each `Cc\_i` is a case clause, and each `Tc\_j` is a catch clause,then Rep(E) = `{'try',LINE,Rep(B),[Rep(Cc\_1), ..., Rep(Cc\_k)],[Rep(Tc\_1), ..., Rep(Tc\_n)],[]}`.

* If E is a try expression `try B after A end`, where `B` and `A` are bodies, then Rep(E) = `{'try',LINE,Rep(B),[],[],Rep(A)}`.

* If E is a try expression `try B of Cc\_1 ; ... ; Cc\_k after A end`, where `B` and`A` are a bodies, and each `Cc\_i` is a case clause, then Rep(E) = `{'try',LINE,Rep(B),[Rep(Cc\_1), ..., Rep(Cc\_k)],[],Rep(A)}`.

* If E is a try expression `try B catch Tc\_1 ; ... ; Tc\_k after A end`, where `B` and`A` are bodies, and each `Tc\_i` is a catch clause, then Rep(E) = `{'try',LINE,Rep(B),[],[Rep(Tc\_1), ..., Rep(Tc\_k)],Rep(A)}`.

* If E is a try expression `try B of Cc\_1 ; ... ; Cc\_k catch Tc\_1 ; ... ; Tc\_n after A end`, where `B` and `A` are a bodies, each `Cc\_i` is a case clause, and each`Tc\_j` is a catch clause, then Rep(E) = `{'try',LINE,Rep(B),[Rep(Cc\_1), ..., Rep(Cc\_k)],[Rep(Tc\_1), ..., Rep(Tc\_n)],Rep(A)}`.

* If E is a variable `V`, then Rep(E) = `{var,LINE,A}`, where `A` is an atom with a printname consisting of the same characters as `V`.

#### [](https://github.com/erlang/otp/edit/maint/erts/doc/src/absform.xml#L536 "Found an issue with the documentation? Fixit by clicking here!")[Qualifiers](#qualifiers)

A qualifier Q is one of the following:

* If Q is a filter `E`, where `E` is an expression, then Rep(Q) = `Rep(E)`.

* If Q is a generator `P <- E`, where `P` is a pattern and `E` is an expression, then Rep(Q) = `{generate,LINE,Rep(P),Rep(E)}`.

* If Q is a bitstring generator `P <= E`, where `P` is a pattern and `E` is an expression, then Rep(Q) = `{b\_generate,LINE,Rep(P),Rep(E)}`.


#### [](https://github.com/erlang/otp/edit/mait/erts/doc/src/absform.xml#L558 "Found an issue with the documentation? Fix it by clicking here!")[Bitstring Element Type Specifiers](#bitstring-element-type-specifiers)

A type specifier list TSL for a bitstring element is a sequence of type specifiers `TS\_1 - ... - TS\_k`, and Rep(TSL) = `[Rep(TS\_1), ..., Rep(TS\_k)]`.

* If TS is a type specifier `A`, where `A` is an atom, then Rep(TS) = `A`.

* If TS is a type specifier `A:Value`, where `A` is an atom and `Value` is an integer, then Rep(TS) = `{A,Value}`.

#### [](https://github.com/erlang/otp/edit/maint/erts/doc/src/absform.xml#L577 "Found an issue with the documentation? Fix itby clicking here!")[Associations](#associations)

An association A is one of the following:

* If A is an association `K => V`, then Rep(A) = `{map\_field\_assoc,LINE,Rep(K),Rep(V)}`.

* If A is an association `K := V`, then Rep(A) = `{map\_field\_exact,LINE,Rep(K),Rep(V)}`.

### [](https://github.com/erlang/otp/edit/maint/erts/doc/src/absform.xml#L594 "Found an issue with the documentation? Fix it by clicking here!")[8.5. Clauses]#clauses)

There are function clauses, if clases, case clauses, and catch clauses.

A clause C is one of the following:
* If C is a case clause `P -> B`, where `P` is a pattern and `B` is a body, then Rep(C) = `{clause,LINE,[Rep(P)],[],Rep(B)}`.

* If C is a case clause `P when Gs -> B`, where `P` is a pattern, `Gs` is a guard sequence, and `B` is a body, then Rep(C) = `{clause,LINE,[Rep(P)],Rep(Gs),Rep(B)}`.

* If C is a catch clause `P -> B`, where `P` is a pattern and `B` is a body, then Rep(C) = `{clause,LINE,[Rep({throw,P,\_})],[],Rep(B)}`, that is, a catch clause with an explicit exception class `throw` and with or without an explicit stacktrace variable`\_` cannot be distinguished from a catch clause without an explicit exception class and without an explicit stacktrace variable.

* If C is a catch clause `X : P -> B`, where `X` is an atomic literal or a variable pattern, `P` is a pattern, and `B` is a body, then Rep(C) = `{clause,LINE,[Rep({X,P,\_})],[],Rep(B)}`, that is, a catch clause with an explicit exception class and with anexplicit stacktrace variable `\_` cannot be distinguished from a catch clause with an explicit exception class and without an explicit stacktrace variable.

* If C is a catch clause `X : P : S -> B`, where `X` is an atomic literal or a variablepattern, `P` is a pattern, `S` is a variable, and B is a body, then Rep(C) = `{clause,LINE,[Rep({X,P,S})],[],Rep(B)}`.

* If C is a catch clause `P when Gs -> B`, where `P` is a pattern, `Gs` is a guard sequence, and `B` is a body, then Rep(C) = `{clause,LINE,[Rep({throw,P,\_})],Rep(Gs),Rep(B)}`, that is, a catch clause with an explicit exception class `throw` and with orwithout an explicit stacktrace variable `\_` cannot be distinguished from a catch clause without an explicit exception class and without an explicit stacktrace variable.

* If C is a catch clause `X : P when Gs -> B`, where `X` is an atomic literal or a variable pattern, `P` is a pattern, `Gs` is a guard sequence, and `B` is a body, then Rep(C) = `{clause,LINE,[Rep({X,P,\_})],Rep(Gs),Rep(B)}`, that is, a catch clause with an explicit exception class and with an explicit stacktrace variable `\_` cannot bedistinguished from a catch clause with an explicit exception class and without an explicit stacktrace variable.


* If C is a catch clause `X : P : S when Gs -> B`, where `X` is an atomic literal or avariable pattern, `P` is a pattern, `Gs` is a guard sequence, `S` is a variable, and `B` is a body, then Rep(C) = `{clause,LINE,[Rep({X,P,S})],Rep(Gs),Rep(B)}`.

* If C is a function clause `( Ps ) -> B`, where `Ps` is a pattern sequence and `B` is a body, then Rep(C) = `{clause,LINE,Rep(Ps),[],Rep(B)}`.


* If C is a function clause `( Ps ) when Gs -> B`, where `Ps` is a pattern sequence,`Gs` is a guard sequence and `B` is a body, then Rep(C) = `{clause,LINE,Rep(Ps),Rep(Gs),Rep(B)}`.

* If C is an if clause `Gs -> B`, where `Gs` is a guard sequence and `B` is a body, then Rep(C) = `{clause,LINE,[],Rep(Gs),Rep(B)}`.

### [](https://github.com/erlang/otp/edit/maint/erts/doc/src/absform.xml#L688 "Found an issue with the documentation? Fix it by clicking here!")[8.6. Guards](#guards)

A guard sequence Gs is a sequence of guards `G\_1; ...; G\_k`, and Rep(Gs) = `[Rep(G\_1), ..., Rep(G\_k)]`. If the guard sequence is empty, then Rep(Gs) = `[]`.
A guard G is a non-empty sequence of guard tests `Gt\_1, ..., Gt\_k`, and Rep(G) = `[Rep(Gt\_1), ..., Rep(Gt\_k)]`

A guard test Gt is one of the following:
* If Gt is an atomic literal `L`, then Rep(Gt) = Rep(L).


* If Gt is a bitstring constructor `<<Gt\_1:Size\_1/TSL\_1, ..., Gt\_k:Size\_k/TSL\_k>>`, where each `Size\_i` is a guard test and each `TSL\_i` is a type specificer list, then Rep(Gt) = `{bin,LINE,[{bin\_element,LINE,Rep(Gt\_1),Rep(Size\_1),Rep(TSL\_1)}, ..., {bin\_element,LINE,Rep(Gt\_k),Rep(Size\_k),Rep(TSL\_k)}]}`. For Rep(TSL), seeabove. An omitted `Size\_i` is represented by `default`. An omitted `TSL\_i` is represented by `default`.

* If Gt is a cons skeleton `[Gt\_h | Gt\_t]`, then Rep(Gt) = `{cons,LINE,Rep(Gt\_h),Rep(Gt\_t)}`.

* If Gt is a function call `A(Gt\_1, ..., Gt\_k)`, where `A` is an atom, then Rep(Gt) = `{call,LINE,Rep(A),[Rep(Gt\_1), ..., Rep(Gt\_k)]}`.


* If Gt is a function call `A\_m:A(Gt\_1, ..., Gt\_k)`, where `A\_m` is the atom`erlang` and `A` is an atom or an operator, then Rep(Gt) = `{call,LINE,{remote,LINE,Rep(A\_m),Rep(A)},[Rep(Gt\_1), ..., Rep(Gt\_k)]}`.


* If Gt is a map creation `#{A\_1, ..., A\_k}`, where each `A\_i` is an association`Gt\_i\_1 => Gt\_i\_2`, then Rep(Gt) = `{map,LINE,[Rep(A\_1), ..., Rep(A\_k)]}`. For Rep(A), see above.


* If Gt is a map update `Gt\_0#{A\_1, ..., A\_k}`, where each `A\_i` is an association`Gt\_i\_1 => Gt\_i\_2` or `Gt\_i\_1 := Gt\_i\_2`, then Rep(Gt) = `{map,LINE,Rep(Gt\_0),[Rep(A\_1), ..., Rep(A\_k)]}`. For Rep(A), see above.

* If Gt is nil, `[]`, then Rep(Gt) = `{nil,LINE}`.

* If Gt is an operator guard test `Gt\_1 Op Gt\_2`, where `Op` is a binary operator other than match operator `=`, then Rep(Gt) = `{op,LINE,Op,Rep(Gt\_1),Rep(Gt\_2)}`.

* If Gt is an operator guard test `Op Gt\_0`, where `Op` is a unary operator, then Rep(Gt) = `{op,LINE,Op,Rep(Gt\_0)}`.

* If Gt is a parenthesized guard test `( Gt\_0 )`, then Rep(Gt) = `Rep(Gt\_0)`, that is, parenthesized guard tests cannot be distinguished from their bodies.


* If Gt is a record creation `#Name{Field\_1=Gt\_1, ..., Field\_k=Gt\_k}`, where each`Field\_i` is an atom or `\_`, then Rep(Gt) = `{record,LINE,Name,[{record\_field,LINE,Rep(Field\_1),Rep(Gt\_1)}, ..., {record\_field,LINE,Rep(Field\_k),Rep(Gt\_k)}]}`.

* If Gt is a record field access `Gt\_0#Name.Field`, where `Field` is an atom, then Rep(Gt) = `{record\_field,LINE,Rep(Gt\_0),Name,Rep(Field)}`.

* If Gt is a record field index `#Name.Field`, where `Field` is an atom, then Rep(Gt) = `{record\_index,LINE,Name,Rep(Field)}`.

* If Gt is a tuple skeleton `{Gt\_1, ..., Gt\_k}`, then Rep(Gt) = `{tuple,LINE,[Rep(Gt\_1), ..., Rep(Gt\_k)]}`.

* If Gt is a variable pattern `V`, then Rep(Gt) = `{var,LINE,A}`, where A is an atom with a printname consisting of the same characters as `V`.

Notice that every guard test has the same source form as some expression, and is represented in the same way as the corresponding expression.
### [](https://github.com/erlang/otp/edit/maint/erts/doc/src/absform.xml#L795 "Found an issue with the documentation? Fix it by clicking here!")[8.7. Types](#types)
* If T is an annotated type `A :: T\_0`, where `A` is a variable, then Rep(T) = `{ann\_type,LINE,[Rep(A),Rep(T\_0)]}`.

* If T is an atom, a character, or an integer literal L, then Rep(T) = Rep(L).

* If T is a bitstring type `<<\_:M,\_:\_*N>>`, where `M` and `N` are singleton integer types, then Rep(T) = `{type,LINE,binary,[Rep(M),Rep(N)]}`.

* If T is the empty list type `[]`, then Rep(T) = `{type,Line,nil,[]}`, that is, the empty list type `[]` cannot be distinguished from the predefined type `nil()`.

* If T is a fun type `fun()`, then Rep(T) = `{type,LINE,'fun',[]}`.

* If T is a fun type `fun((...) -> T\_0)`, then Rep(T) = `{type,LINE,'fun',[{type,LINE,any},Rep(T\_0)]}`.

* If T is a fun type `fun(Ft)`, where `Ft` is a function type, then Rep(T) = `Rep(Ft)`. For Rep(Ft), see below.

* If T is an integer range type `L .. H`, where `L` and `H` are singleton integer types, then Rep(T) = `{type,LINE,range,[Rep(L),Rep(H)]}`.

* If T is a map type `map()`, then Rep(T) = `{type,LINE,map,any}`.

* If T is a map type `#{A\_1, ..., A\_k}`, where each `A\_i` is an association type, then Rep(T) = `{type,LINE,map,[Rep(A\_1), ..., Rep(A\_k)]}`. For Rep(A), see below.

* If T is an operator type `T\_1 Op T\_2`, where `Op` is a binary operator (this is anoccurrence of an expression that can be evaluated to an integer at compile time), then Rep(T) = `{op,LINE,Op,Rep(T\_1),Rep(T\_2)}`.

* If T is an operator type `Op T\_0`, where `Op` is a unary operator (this is anoccurrence of an expression that can be evaluated to an integer at compile time), then Rep(T) = `{op,LINE,Op,Rep(T\_0)}`.

* If T is `( T\_0 )`, then Rep(T) = `Rep(T\_0)`, that is, parenthesized types cannot be distinguished from their bodies.

* If T is a predefined (or built-in) type `N(T\_1, ..., T\_k)`, then Rep(T) = `{type,LINE,N,[Rep(T\_1), ..., Rep(T\_k)]}`.

* If T is a record type `#Name{F\_1, ..., F\_k}`, where each `F\_i` is a record fieldtype, then Rep(T) = `{type,LINE,record,[Rep(Name),Rep(F\_1), ..., Rep(F\_k)]}`. For Rep(F), see below.

* If T is a remote type `M:N(T\_1, ..., T\_k)`, then Rep(T) = `{remote\_type,LINE,[Rep(M),Rep(N),[Rep(T\_1), ..., Rep(T\_k)]]}`.

* If T is a tuple type `tuple()`, then Rep(T) = `{type,LINE,tuple,any}`.

* If T is a tuple type `{T\_1, ..., T\_k}`, then Rep(T) = `{type,LINE,tuple,[Rep(T\_1), ..., Rep(T\_k)]}`.

* If T is a type union `T\_1 | ... | T\_k`, then Rep(T) = `{type,LINE,union,[Rep(T\_1), ..., Rep(T\_k)]}`.

* If T is a type variable `V`, then Rep(T) = `{var,LINE,A}`, where `A` is an atom witha printname consisting of the same characters as `V`. A type variable is any variable except underscore (`\_`).

* If T is a user-defined type `N(T\_1, ..., T\_k)`, then Rep(T) = `{user\_type,LINE,N,[Rep(T\_1), ..., Rep(T\_k)]}`.


#### [](https://ithub.com/erlang/otp/edit/maint/erts/doc/src/absform.xml#L902 "Found an issue with the documentation? Fix it by clicking here!")[Function Types](#function-types

A function type Ft is one of the following:

* If Ft is a constrained function type `Ft\_1 when Fc`, where `Ft\_1` is a functiontype and `Fc` is a function constraint, then Rep(T) = `{type,LINE,bounded\_fun,[Rep(Ft\_1),Rep(Fc)]}`. For Rep(Fc), see below.


* If Ft is a function type `(T\_1, ..., T\_n) -> T\_0`, where each `T\_i` is a type,then Rep(Ft) = `{type,LINE,'fun',[{type,LINE,product,[Rep(T\_1), ..., Rep(T\_n)]},Rep(T\_0)]}`.


#### [](https://githubcom/erlang/otp/edit/maint/erts/doc/src/absform.xml#L923 "Found an issue with the documentation? Fix it by clicking here!")[Function Constraints](#function-constraints)
A function constraint Fc is a non-empty sequence of constraints `C\_1, ..., C\_k`, and Rep(Fc) = `[Rep(C\_1), ..., Rep(C\_k)]`.

* If C is a constraint `V :: T`, where `V` is a type variable and `T` is a type, then Rep(C) = `{type,LINE,constraint,[{atom,LINE,is\_subtype},[Rep(V),Rep(T)]]}`.
#### [](https://gitub.com/erlang/otp/edit/maint/erts/doc/src/absform.xml#L938 "Found an issue with the documentation? Fix it by clicking here!")[Association Types](#association-types)

* If A is an association type `K => V`, where `K` and `V` are types, then Rep(A) = `{type,LINE,map\_field\_assoc,[Rep(K),Rep(V)]}`.

* If A is an association type `K := V`, where `K` and `V` are types, then Rep(A) = `{type,LINE,map\_field\_exact,[Rep(K),Rep(V)]}`.


#### [](https://githb.com/erlang/otp/edit/maint/erts/doc/src/absform.xml#L954 "Found an issue with the documentation? Fix it by clicking here!")[Record Field Types](#record-field-types)

* If F is a record field type `Name :: Type`, where `Type` is a type, then Rep(F) = `{type,LINE,field\_type,[Rep(Name),Rep(Type)]}`.
### [](https://github.com/erlang/otp/edit/maint/erts/do/src/absform.xml#L965 "Found an issue with the documentation? Fix it by clicking here!")[8.8. The Abstract Format after Preprocessing](#the-abstract-format-after-preprocessing)

The compiation option `debug\_info` can be specified to the compiler to have the abstract code stored in the `abstract\_code` chunk in the Beam file (for debugging purposes).
As from Erlang/OTP R9C, the `abstract\_code` chunk contains `{raw\_abstract\_v1,AbstractCode}`, where `AbstractCode` is the abstract code as described in this section.

In OTP releases before R9C, the abstract code after some more processing was stored in the Beam file. The first element of the tuple would be either `abstract\_v1` (in OTP R7B) or `abstract\_v2` (in OTP R8B).

