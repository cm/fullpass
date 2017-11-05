console:
	@rebar3 as $(p) release; _build/$(p)/rel/cmrtc/bin/cmrtc console

dist:
	@cd apps/$(app)/ui; yarn build; cp dist/app.js ../../$(app)/priv

weekonekt:
	@cd apps/weekonekt/ui; yarn start;

fullpass:
	@cd apps/fullpass/ui; yarn start;

push:
	@git add .; git commit -am 'latest changes'; git push

