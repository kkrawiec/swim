# SWIM: Synthesis with Metaheuristics - Genetic Programming in Scala

Krzysztof (Chris) Krawiec, <krawiec at cs.put.poznan.pl>
Sept 2016

SWIM is a compact library that implements the basic functionality of [Genetic Programming (GP)](#fg), a popular stochastic approach to program synthesis. I developed its early version in the process of preparing my recent [book](#bps) on behavioral program synthesis using GP. 

GP is an evolutionary algorithm (EA) working with a population of programs, represented as combinatorial structures (AST-like trees, instruction sequences, graphs - depending on the 'genre' of GP). In each iteration, programs in the population are evaluated (assessed how well they realize the required functionality), and the well-performing ones are selected and modified using search operators (mutated and crossed-over). In this process, the quality of programs tends to gradually improve and ultimately the sought program is usually synthesized after a number of generations. 

To realize the functionality of evolutionary algorithm, SWIM relies on the FUEL library https://github.com/kkrawiec/fuel, which implements the evolutionary computation workflow in Scala. SWIM provides the GP-specific components for that workflow, i.e., mainly data structures for representing programs, random generation of initial candidate programs, search operators, and evaluation.

The entities manipulated in SWIM are *candidate solutions* that implement the `Program` trait. Currently, SWIM offers one concrete implementation of `Program` (`tree.Op`), which follows the *tree-based* paradigm of GP: programs are expression trees, where inner tree nodes typically represent instructions, while tree leaves fetch input data (e.g., in prefix notation `(* (+ x 2) x)`). This paradigm is most convenient for evolving side effect-free expressions, but in SWIM it can be also used with imperative programs with side-effects.

SWIM implements *typed GP*, i.e., instructions can return values of different types. The space of feasible solutions (syntactically correct programs) is defined by a grammar (`Grammar` case class). A grammar is a list of productions that define all permissible ways in which programs can be constructed. Building grammars in SWIM is straightforward; an example of a grammar for simple arithmetic expressions, with one input variable `x` and three constants: 
```
    val g = Grammar(Map[Any, Seq[Any]](
      'S -> Seq(
        'x,
        0, 1, -1,
        '+ -> ('S, 'S),
        '- -> ('S, 'S),
        '* -> ('S, 'S),
        '/ -> ('S, 'S))))
```
Grammars are essential to generate syntactically correct candidate programs and to manipulate them in a way that preserves their syntactic correctness. 

SWIM can be used for single-type problems too; in such cases, the grammar hosts only one nonterminal symbol, the starting symbol of the grammar. This is the most popular mode of operation of GP, used in symbolic regression (`app.Regression`), synthesis of Boolean functions (`app.Boolean`), and some other domains (e.g., synthesis of algebraic expressions, `app.Algebra`). 

Examples of multi-type problems included in SWIM are: the toy problem of synthesizing a program that calculates the maximum of a pair of numbers (`app.Min2`) and synthesizing a program that determines the position of an integer in a sorted array (`app.ArraySearch5`). Both these examples come from the SyGuS contest (http://www.sygus.org/). 

Grammars define only program syntax. The semantics of particular instructions are defined in a separate class, which should implement the `Domain` trait. A domain works as program interpreter, which for tree-based GP can be conveniently implemented using recursion and pattern matching (see `app.MinDomain` for an example).  

The default definition of program's evaluation (*fitness*) is the number of tests passed by a program - see `tree.SimpleGP.Discrete`. A test is a pair composed of program input and the corresponding desired program output (see `Test`). SWIM follows the convention of FUEL and defines fitness as a minimized objective, i.e., the number of tests *failed* by a program. 

To launch a GP run in SWIM, one needs to provide a grammar defining language syntax, a corresponding domain defining program semantics, and the set of tests that defines program correctness. Given these three, launching a GP run requires just one function call; see for instance `app.Min2` or `swim.app.TestGPMyProblem`.

Testing GP on a given benchmark is even simpler; an example of applying GP to a 6-bit multiplexer problem: 
```
object TestGPBool extends IApp('benchmark -> "mux6", 'maxGenerations -> 100) {
  RunExperiment(SimpleGP.Discrete(BooleanBenchmark()))
}
```

Past studies in GP showed that conventional fitness as defined above has certain downsides, so SWIM offers also [*implicit fitness sharing*](#ifs) (`eval.IFS`) and [*lexicase selection*](#lexi) (`eval.Lexicase`) as alternative evaluation/selection methods. 

Like FUEL, SWIM supports both sequential and parallel (multi-threaded) evaluation of candidate solutions. The latter is default; see `app.ArraySearch5` how to set evaluation mode. 

SWIM largely follows the good practices of functional programming: virtually all objects are immutable, and inheritance is used sparingly.  

The `app.Tests` file provides a few commented examples of configuring and running GP in SWIM.  

## Prerequisites

The FUEL library: https://github.com/kkrawiec/fuel 

## Credits

Much of the inspiration for this library comes from chatting with Jerry Swan, whom I'm deeply indebted for his advice and encouragement. The feedback from my student Iwo Błądek was also very helpful.

## How to cite 

If you decide to use FUEL and like it, please cite my book, *Behavioral Program Synthesis with Genetic Programming*  published by Springer. You can find out more about the book [here](http://www.cs.put.poznan.pl/kkrawiec/bps/). 

~~~~~{.bib}
@book{KrawiecBPS2015,
    title = {Behavioral Program Synthesis with Genetic Programming},
    author = {Krzysztof Krawiec},
    publisher = {Springer International Publishing},
    series = {Studies in Computational Intelligence},
    year = 2016,
    volume = {618},
    doi = {10.1007/978-3-319-27565-9},
    isbn = {978-3-319-27563-5},
    url = { http://www.springer.com/gp/book/9783319275635 },
    note = { http://www.cs.put.poznan.pl/kkrawiec/bps }
 }
~~~~~


## Bibliography

<a name="bps">[1]</a>: Krawiec, K., "Behavioral Program Synthesis with Genetic Programming", Springer International Publishing, 2015. 

<a name="fg">[2]</a>: Poli, R.; Langdon, W. B. & McPhee, N. F., "A field guide to genetic programming", Published via http://lulu.com and freely available at http://www.gp-field-guide.org.uk, 2008

<a name="lexi">[3]</a>: Helmuth, T.; Spector, L. & Matheson, J., "Solving Uncompromising Problems with Lexicase Selection", IEEE Transactions on Evolutionary Computation, 2015, 19, 630-643

<a name="ifs">[4]</a>: Smith, R.E.; Forrest, S.; & Perelson A.S, “Searching for diverse, cooperative populations with genetic algorithms”. Evolutionary Computation, 1993, 1.2 

<a name="ifs2">[5]</a>: McKay, R. I., "An Investigation of Fitness Sharing in Genetic Programming", The Australian Journal of Intelligent Information Processing Systems, 2001, 7, 43-51

