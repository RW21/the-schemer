```racket
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))
```

- S-expression
  - Symbolic expressions
  - All atoms are S-expression.
  - `()` is a special S-expressoin.
  - `()` is not an atom.
  
- `car`
  - `car` of `(a b c)` is a.
  - The primitive `car` is defined only for non-empty lists.
  
- `cdr` is second- in list.

- Always ask `null?` as the first question in expressin any function.

