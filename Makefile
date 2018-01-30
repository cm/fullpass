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

a: hosts common accounts haproxy erlang app

hosts:
	@ansible-playbook -i inventories/a playbooks/hosts.yml

common:
	@ansible-playbook -i inventories/a playbooks/common.yml

ssl:
	@ansible-playbook -i inventories/a playbooks/ssl.yml

accounts:
	@ansible-playbook -i inventories/a playbooks/accounts.yml

haproxy:
	@ansible-playbook -i inventories/a playbooks/haproxy.yml

go: 
	@ansible-playbook -i inventories/a playbooks/go.yml

cmdb:
	@ansible-playbook -i inventories/a playbooks/cmdb.yml

places:
	@ansible-playbook -i inventories/a playbooks/places.yml

erlang:
	@ansible-playbook -i inventories/a playbooks/erlang.yml

app:
	@ansible-playbook -i inventories/a playbooks/app.yml

admin:
	@cd ui/admin; yarn start

dev: ui_dist push 
	@ansible-playbook -i inventories/a playbooks/dev.yml

ui_dist:
	@cd ui/admin; yarn build; cp dist/app.js ../../apps/admin/priv; cd ../..

ui:
	@ansible-playbook -i inventories/a playbooks/ui.yml 
