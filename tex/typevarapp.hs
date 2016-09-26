m a -> (a -> m b) -> m b

TypeFun
	(TypeVarApp
		(TV (Ident {unpackIdent = "m"}))
		[TypeVar (TV (Ident {unpackIdent = "a"}))]
	)
	(TypeFun
		(TypeFun (TypeVar (TV (Ident {unpackIdent = "a"})))
			(TypeVarApp
				(TV (Ident {unpackIdent = "m"}))
				[TypeVar (TV (Ident {unpackIdent = "b"}))]
			)
		)
		(TypeVarApp
			(TV (Ident {unpackIdent = "m"}))
			[TypeVar (TV (Ident {unpackIdent = "b"}))]
		)
	)