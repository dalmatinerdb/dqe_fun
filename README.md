dqe_fun
=====

DQE functions

A function has the following parameter:

* name (name of the function) (binary)
* return type
* parameters (a list of types)
* list_arameters (type)
* function module/function (atom/atom tuple)

Notable is:
- functions with different parameters are different functions
- possible types are `histogram`, `realized`, `integer`, `float`
- list parameters can be either `histogram` or `realized`, `histogram_c` or `realized_c` they will contain more then one. The `_ac` indicates associative and commutative qualities.

Build
-----

    $ rebar3 compile
