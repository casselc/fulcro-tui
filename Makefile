.PHONY: tests deploy

# Run all *-spec namespaces via kaocha (dot output) with guardrails throwing on schema violations.
tests:
	clojure -M:test:kaocha

# Build the jar and deploy it to Clojars. Runs the tests first.
# Credentials come from CLOJARS_USERNAME/CLOJARS_PASSWORD or ~/.m2/settings.xml.
deploy:
	clojure -T:build deploy
