RE = R --vanilla --quiet -e

test:
	$(RE) "devtools::test()"

docs:
	$(RE) "devtools::document()"
