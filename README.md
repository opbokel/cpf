# CPF

A Haskell library with all operations that you probably will ever need regarding CPF (CADASTRO DE PESSOA FÍSICA.). The Brazilian social security code.

The documentation in Portuguese is more complete, so I am linking it instead of the ones I found in english. Auto translation can be used for non english speakers. https://pt.wikipedia.org/wiki/Cadastro_de_Pessoas_F%C3%ADsicas
https://www.macoratti.net/alg_cpf.htm


This library has the ambition to be added as a Haskell Cabal Package and make it available for everybody. It should be fully working as is, but the next steps are:

- Proper documentation on every exported function. Including an html generated page.
- Write automate and regular tests
- Submit it as described here: https://hackage.haskell.org/upload

A quick overview of the functionalities:

Read a CPF from any string (or even other showable data structures). Most common case of use is reading a cpf from user input. This is done by extracting the digits.

Get any digit from a CPF, or get the CPF back as digits.

From a 9 digits CPF, calculate the check digits, always returning a valid cpf.

From a 11 digits CPF, check if it is valid

Generate random valid CPFs

String and pretty string conversions:
ex : "123.456.789-01"

And much more…

Proper docs soon to come.




