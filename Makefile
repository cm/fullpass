console:
	@rebar3 as $(p) release; _build/$(p)/rel/cmrtc/bin/cmrtc console

dist:
	@cd apps/$(app)/ui; yarn build; cp dist/app.js ../../$(app)/priv

ui:
	@cd apps/$(app)/ui; yarn start;

push:
	@git commit -am '$(msg)'; git push

