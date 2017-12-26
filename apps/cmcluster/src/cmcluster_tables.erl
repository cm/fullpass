-module(cmcluster_tables).
-export([spec/0, key/0, do/2]).

spec() -> [].

key() -> cluster_tables.

do(_, S) ->
    {ok, cluster_tables, 
     [ #{ name => Name,
          type => Type,
          media => cmdb:table_copies_to_media(Copies) } 
       || {Name, Type, Copies} <- cmdb:all_tables() ], S}.

