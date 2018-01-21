-module(cmdb_log).
-behaviour(gen_statem).
-export([start_link/0]).
-export([init/1, callback_mode/0, terminate/3]).
-export([ready/3]).
-export([write/1, read/2, tail/1]).
-record(data, {db, env}).
-define(APP, "log").
-define(NS, "cmdb").

write(Term) ->
    gen_statem:cast({?MODULE, node()}, {write, Term}).

read(Since, Count) ->
    gen_statem:call({?MODULE, node()}, {read, Since, Count}).

tail(Count) ->
    gen_statem:call({?MODULE, node()}, {tail, Count}).


callback_mode() ->
    state_functions.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Env, Db} = cmdb_util:open(?APP, ?NS),
    cmkit:log({cmdb_log, node(), started}),
    {ok, ready, #data{db=Db, env=Env}}.

ready(cast, {write, Terms}, #data{db=Db}=Data) ->
    elmdb:put(Db, 
              erlang:term_to_binary(cmkit:now()),
              erlang:term_to_binary(Terms)),
    {keep_state, Data};

ready({call, From}, {tail, Count}, #data{env=Env, db=Db}=Data) ->    
    Entries = cmdb_util:tail(Env, Db, Count),
    {keep_state, Data, {reply, From, Entries}};

ready({call, From}, {read, Since, Count}, #data{env=Env, db=Db}=Data) ->    
    Entries = cmdb_util:iterate(Env, Db, Since, Count),
    {keep_state, Data, {reply, From, Entries}}.

terminate(Reason, _, #data{env=Env}) ->
    cmkit:log({cmdb, closing, ?APP, ?NS, Reason}),
    cmdb_util:close(?APP, ?NS, Env).
