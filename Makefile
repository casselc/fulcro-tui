.PHONY: tests bb-tests deploy

# Run all *-spec namespaces via kaocha (dot output) with guardrails throwing on schema violations.
tests:
	clojure -M:test:kaocha

# Run the babashka-compatible specs under babashka itself (see the `test` task in bb.edn).
# Keeps the bb-target code honestly green on bb, not just on the JVM.
bb-tests:
	bb test

# Build, GPG-sign, and deploy to Clojars via Maven (pom.xml).
# Credentials come from the <server id="clojars"> entry in ~/.m2/settings.xml.
deploy:
	rm -rf target
	mvn deploy
