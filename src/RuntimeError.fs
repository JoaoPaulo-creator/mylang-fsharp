module RuntimeError

open Token
exception RuntimeError of Token * string
