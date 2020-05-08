HASKELL_SRC=$(shell find src/ -type f -name '*.hs')
HASKELL_TEST=$(shell find test/ -type f -name '*.hs')
HASKELL_EXAMPLES=$(shell find examples/ -type f -name '*.hs')

.PHONY: ide
ide:
	ghcid --command "stack ghci --ghci-options=-fno-code"

.PHONY: watch-docs
watch-docs:
	stack haddock servant-docs-simple --no-haddock-deps --file-watch --fast

.PHONY: docs
docs:
	stack haddock servant-docs-simple --no-haddock-deps --fast --open

.PHONY: stylish-haskell
stylish-haskell: $(HASKELL_SRC) $(HASKELL_TEST) $(HASKELL_EXAMPLES)
	stylish-haskell $^ -i

.PHONY: hlint
hlint:
	hlint -h .hlint.yaml test src examples
