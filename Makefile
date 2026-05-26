.PHONY: tests deploy

# Run all *-spec namespaces via kaocha (dot output) with guardrails throwing on schema violations.
tests:
	clojure -M:test:kaocha

# Build, GPG-sign, and deploy to Clojars via Maven (pom.xml).
# Credentials come from the <server id="clojars"> entry in ~/.m2/settings.xml.
deploy:
	rm -rf target
	mvn deploy
