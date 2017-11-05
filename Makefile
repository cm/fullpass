console:
	@rebar3 as $(p) release; _build/$(p)/rel/cmrtc/bin/cmrtc console

ui_dev:
	@cd apps/cmui/ui; yarn start

ui_dist:
	@cd apps/cmui/ui; yarn build; cp -rf dist/* ../../$(app)/priv

