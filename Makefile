.PHONY: ide stylish-haskell hlint

ide:
	ghcid --command "stack ghci --ghci-options=-fno-code"

watch-docs:
	stack haddock --no-haddock-deps --file-watch --fast

docs:
	stack haddock --no-haddock-deps --fast --open

stylish-haskell:
	stylish-haskell src/**/**/*.hs test/*.hs test/**/**/**/*.hs -i

hlint:
	hlint src test
