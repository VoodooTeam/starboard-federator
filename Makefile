ifneq ($(wildcard IHP/.*),)
IHP = IHP/lib/IHP
else
IHP = $(shell dirname $$(which RunDevServer))/../lib/IHP
endif

CSS_FILES += ${IHP}/static/vendor/bootstrap.min.css
CSS_FILES += ${IHP}/static/vendor/flatpickr.min.css
CSS_FILES += static/app.css

JS_FILES += ${IHP}/static/vendor/jquery-3.2.1.slim.min.js
JS_FILES += ${IHP}/static/vendor/timeago.js
JS_FILES += ${IHP}/static/vendor/popper.min.js
JS_FILES += ${IHP}/static/vendor/bootstrap.min.js
JS_FILES += ${IHP}/static/vendor/flatpickr.js
JS_FILES += ${IHP}/static/helpers.js
JS_FILES += ${IHP}/static/vendor/morphdom-umd.min.js
JS_FILES += ${IHP}/static/vendor/turbolinks.js
JS_FILES += ${IHP}/static/vendor/turbolinksInstantClick.js
JS_FILES += ${IHP}/static/vendor/turbolinksMorphdom.js

include ${IHP}/Makefile.dist


local-env: start-infra-cluster build-db-migrate build-deploy-local start-target-cluster

start-target-cluster:
	./scripts/startTestStack.sh
	kubectl apply -f ./scripts/create-sa.yml
	echo "==="
	./scripts/get-cluster-info.sh

start-infra-cluster:
	./scripts/startInfraStack.sh

build-db-migrate:
	docker build -t localhost:5000/db-migrator -f ./scripts/Dockerfile.db-migration .
	docker push localhost:5000/db-migrator

build-deploy-local:
	docker build -t localhost:5000/starboard-federator .
	docker push localhost:5000/starboard-federator
	kubectl apply -f ./scripts/deploy/
