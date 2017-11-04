console:
	@rebar3 as $(p) release; _build/$(p)/rel/cmrtc/bin/cmrtc console
ui:
	@cd apps/cmui/ui; yarn start

release:
	@rebar3 release
