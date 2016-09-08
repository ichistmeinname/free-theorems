data Test a = Test String a

[DataDecl
	(Data {
		dataName =
			Ident {
				unpackIdent = "Test"},
		dataVars = [
			TV (Ident {unpackIdent = "a"})
		],
		dataCons = [
			DataCon {
				dataConName = Ident {unpackIdent = "Test"},
				dataConTypes = [
				Unbanged {
					withoutBang =
						TypeCon (Con (Ident {unpackIdent = "String"})) []},
				Unbanged {
					withoutBang =
						TypeVar (TV (Ident {unpackIdent = "a"}))}
				]}
		]}
	)
]