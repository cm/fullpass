-module(cmsocial).
-export([cmdb_tables/0, cmevents_plugins/0]).

cmdb_tables() ->
    [{users, set, disc_copies}, 
     {names, bag, disc_copies},
     {passwords, set, disc_copies},
     {connections, bag, ram_copies}, 
     {invites, set, ram_copies}, 
     {invites_sent, bag, ram_copies}, 
     {invites_received, bag, ram_copies}, 
     {contacts, bag, ram_copies},
     {groups, bag, ram_copies}].

cmevents_plugins() ->
    [{cmsocial_group, like, group, group_likes, like, is_liked_by}].

