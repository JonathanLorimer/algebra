ghcid:
	hpack .
	cabal new-configure
	ghcid -o ghcid.txt --command 'cabal new-repl lib:algebra' --allow-eval --warnings

