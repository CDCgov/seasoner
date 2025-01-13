RE = R --vanilla --quiet -e

.PHONY: test docs vignettes check build

test:
	$(RE) "devtools::test()"

docs:
	$(RE) "devtools::document()"

vignettes:
	$(RE) "devtools::build_vignettes()"

check:
	$(RE) "devtools::check()"

build:
	$(RE) "devtools::build()"
