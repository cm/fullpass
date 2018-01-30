.PHONY: ui

console:
	@rebar3 release; _build/default/rel/cmrtc/bin/cmrtc console

dist:
	@cd apps/$(app)/ui; yarn build; cp dist/app.js ../../$(app)/priv

weekonekt:
	@cd apps/weekonekt/ui; yarn start;

fullpass:
	@cd apps/fullpass/ui; yarn start;

push:
	@git add .; git commit -am 'latest changes'; git push

admin:
	@cd ui/admin; yarn start
