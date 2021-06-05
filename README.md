# OPG_Parser

**🔪 Operator Precedence Grammar Parser, in Coq.**

Course Project for *Compiler Principle (SJTU-CS308-2021)*
## Features

- Grammar Parser (in Haskell)
- Grammar Checker (in Coq)
- Operator Precedence Analysis Table Generator (in Coq)
- File IO and Pretty Printer (in Haskell)
- Shell (in Haskell)

## Demo

The file "in1.txt" contains an operator precedence grammar, shown as follows:

```
E -> E + T | T
T -> T * F | F
F -> ( E ) | i
```

The program can parse the grammar and output its analysis table:

[![2N1Y90.png](https://z3.ax1x.com/2021/06/05/2N1Y90.png)](https://imgtu.com/i/2N1Y90)

The file "in1.txt_output.md" contains the analysis table:

|      | +    | *    | (    | )    | i    | $    |
| ---- | ---- | ---- | ---- | ---- | ---- | ---- |
| +    | >    | <    | <    | >    | <    | >    |
| *    | >    | >    | <    | >    | <    | >    |
| (    | <    | <    | <    | =    | <    |      |
| )    | >    | >    |      | >    |      | >    |
| i    | >    | >    |      | >    |      | >    |
| $    | <    | <    | <    |      | <    | =    |

## How to build 

To build the program, you need to install **Colourista** via stack
```bash
  stack install colourista
```
Then you can use GHCi/GHC to run/compile the program
```bash
  stack runhaskell Main.hs
```

or

```bash
  stack ghc -- -O2 Main.hs
```

or

```bash
  make
```

## Tests

Test 1: 

```
E -> E + T | T
T -> T * F | F
F -> ( E ) | i
```

Test 2:

```
E -> E + E | E * E | ( E ) | id
```

Test 3:

```
E -> E + T | T
T -> T * F | F
F -> ( E E ) | i
```

Here is the result:

[![2N1t3V.png](https://z3.ax1x.com/2021/06/05/2N1t3V.png)](https://imgtu.com/i/2N1t3V)

## License

[MIT](https://choosealicense.com/licenses/mit/)

## Acknowledgements

- [Colourista](https://github.com/kowainik/colourista): Convenient interface for printing colourful messages
- [Coq Proof Assistant](https://coq.inria.fr/): A formal proof management system
