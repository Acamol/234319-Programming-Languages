g           ::= <rule> <comment>
rule        ::= lhs "::=" rhs
lhs         ::= identifier
rhs         ::= identifier
                | terminal
                | "<" rhs ">"
                | "{" rhs "}"
                | "(" rhs ")"
                | "[" rhs "]"
                | rhs "|" rhs
                | rhs "=>" rhs
                | rhs "=" rhs
                | rhs <comment> EOL <rhs>
identifier  ::= letter <identifer1>
identifier1 ::= character <identifier1>
terminal    ::= "'" character <character> "'" | '"' character <character> '"'
letter      ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
digit       ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
symbol      ::= "_" | "'" |
character   ::= letter | digit | symbol
comment     ::= <letter | digit | symbol> <comment>
