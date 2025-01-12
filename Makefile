RE = R --vanilla --quiet -e

test:
	$(RE) "devtools::test()"

docs:
	$(RE) "devtools::document()"

check:
	$(RE) "devtools::check()"

build:
	$(RE) "devtools::build()"
