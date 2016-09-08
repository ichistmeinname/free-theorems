class Monad m where
	(>>=) :: m a -> (a -> m b) -> m b

test :: Monad m => m a -> m a

[ClassDecl 
	(Class {
		superClasses = [],
		className = Ident {unpackIdent = "Monad"},
		classVar = TV (Ident {unpackIdent = "m"}),
		classFuns = [
			Signature {...}
		]}
	),
	TypeSig
		(Signature {
			signatureName =
				Ident {unpackIdent = "test"},
				signatureType =
					TypeAbs
						(TV (Ident {unpackIdent = "m"}))
						[TC (Ident {unpackIdent = "Monad"})]
						(TypeFun
							(TypeVarApp
								(TV (Ident {unpackIdent = "m"}))
								[TypeVar (TV (Ident {unpackIdent = "a"}))]
							)
							(TypeVarApp
								(TV (Ident {unpackIdent = "m"}))
								[TypeVar (TV (Ident {unpackIdent = "a"}))]
							)
						)}
		)
]