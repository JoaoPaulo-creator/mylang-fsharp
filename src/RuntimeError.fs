module RuntimeError

open AST
exception RuntimeError of Token * string
