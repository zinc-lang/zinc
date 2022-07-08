# Intro to type theory, type checking, and type inference

Adapted from the theory portion of [this blog post](https://mukulrathi.com/create-your-own-programming-language/intro-to-type-checking/)

## Basic rules

literal expressions are very easy to infer the type of.

$\vdash \textbf{true} : bool$

$\vdash \textbf{false} : bool$

$\vdash n : int$ for any integer $n$

## Typing environments

When it gets harder is when we have to determine the types of variables.

What is the type of x below?

$$\vdash x : ?$$

To find out we need a _typing environment_, represented as $\Gamma$ (Gamma). Think of $\Gamma$ as a look up function into the environment given an identifier.

We declare that $\textbf{x}$ is of type $t$:

$$\Gamma \vdash \textbf{x} : t$$

So now with:

$$\Gamma \vdash x : ?$$

if $\Gamma (x) = t$ we can say:

$$\Gamma \vdash \textbf{x} : t$$

We can then stack this into one rule, this is an _inference rule_.

$$\frac {\Gamma (x) = t} {\Gamma \vdash x : t}$$

It basically says if we can find that $x$ is of type $t$ within the environment $\Gamma$ then $x$ is of type $t$. Not very useful by itself.

## Inference rules

The stacked 'fraction' is a representation of deductive reasoning, ie. if this then that. If the top holds true then we _infer_ that the bottom also holds.

For example, if we know $e_{1}$ and $e_{2}$ to be `int`s, then we the sum of them is also an `int`.

$$
\frac{
    \Gamma \vdash e_{1} : int \quad  \Gamma \vdash e_{2} : int
}{
    \Gamma \vdash e_{1} + e_{2} : int
}
$$

We write our type system by stacking these type inference rules.

Lets type-check the expression `x + 1` where we know `x` to be an `int`.

$$
\frac{
    \frac{
        \frac{}{\Gamma(x) = int}
    }{
        \Gamma \vdash x : int
    }
    \quad
    \frac{}{\Gamma \vdash 1 : int}
}{
    \Gamma \vdash x + 1 : int
}
$$

If `x` was a `bool`, then $\Gamma(x) = int$ would not hold true, therefore none of the others below hold true, the type-checker would raise an error, as the type don't match up.

## Axioms

It is convention to represent the facts (_axioms_) with a line above them. You can think of them as the "best case".

$$\frac{}{\Gamma \vdash 1 : int}$$

They have nothing on top, so they require nothing else to be true for itself to be true. So they are always true.

The structure created by our _proof_ looks like a tree. It lays out our reasoning - you just need to follow the steps; why is `x + 1` an `int`? Well `x` and `1` are `int`s. Why is `x` an `int`? etc. You get the point. So we call what have constructed here a _proof tree_.

## What about an `if-else`?

```rs
let x = if condition get_something() else get_something_else();
```

The `condition` _has_ to be a bool. Then what about the branches, we can only give `x` one type, so the overall expression can only have one type, and since we are _statically_ type checking, we don't know which path will execute at runtime. So `get_something` and `get_something_else` need to return the same type, regardless of which branch executes.

In this case we can write this,
where $e_{1}$ is `condition`,
$e_{2}$ is `get_something()` and
$e_{3}$ is `get_something_else()`.

$$
\frac{
    \Gamma \vdash e_{1} : bool \quad
    \Gamma \vdash e_{2} : t \quad
    \Gamma \vdash e_{3} : t
}{
    \Gamma \vdash \textbf{if} \ e_{1} \{e_{2}\} \ \textbf{else} \ \{e_{3}\} : t
}
$$

What this says is that if $e_{1}$ is a `bool` and both `get_something()` and `get_something_else()` are of the same type $t$ (this exact type is irrelevant in this case), then the whole `if-else` expression is of type $t$. Which, although not shown here, is the type `x` will get.

## Typing the overall program

To show that the program is typed we can say:

$$\{\} \vdash program : t$$

Notably in place of $\Gamma$ we have $\{\}$, this is because initially $\Gamma = \{\}$ - it's empty, as no variables have been defined yet. So then how do we __add variables__ to $\Gamma$?

We add them by declaring them in our syntax, eg. `let x = e1`, Now $\Gamma$ has been updated with `x`, receiving the type of `e1`.

Take

```rs
let x = e1
e2
```

We type-check this in the order that the program executes:

* Get the type of $e_{1}$, lets say $t_{1}$
* Then _extend_ the environment $\Gamma$ with the new mapping $x : t_{1}$
* Use this extended environment (which we write as $\Gamma, x : t$) to type-check the leading code
* Type-check $e_{2}$ to give it type $t_{2}$

The whole rule thus looks like:

$$
\frac{
    \Gamma \vdash e_{1} : t_{1} \quad
    \Gamma, e_{1} : t_{1} \vdash e_{2} : t_{2}
}{
    \Gamma \vdash \textbf{let} \ x = e_{1}; e_{2} : t_{2}
}
$$
