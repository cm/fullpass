-module(cmdb_dets).
-behaviour(gen_statem).
-export([
         start_link/1,
         init/1, 
         callback_mode/0, 
         terminate/3,
         ready/3
        ]).
-record(data, {name, db}).

callback_mode() ->
    state_functions.

start_link(#{ name := Name }=Db) ->
    gen_statem:start_link({local, Name}, ?MODULE, [Db], []).

init([#{name := Name}=Db]) ->
    Filename = data_file(Name),
    case dets:open_file(Name, [{access, read_write},
                               {type, bag},
                               {file, Filename}
                              ]) of 
        {ok, Name} -> 
            cmkit:log({cmdb, Name, dets, started, Filename}),
            {ok, ready, #data{name=Name, db=Db}};
        {error, E} ->
            cmkit:log({cmdb_dets, Name, error, E}),
            {error, E}
    end.

ready({call, From}, {get, K}, #data{name=Name}=Data) ->
    Res = case dets:lookup(Name, K) of 
        {error, R} -> {error, R};
        Objs -> Objs
    end,
    {keep_state, Data, {reply, From, Res}};

ready({call, From}, {put, K, V}, #data{name=Name}=Data) ->
    Res = dets:insert(Name, {K, V}),
    {keep_state, Data, {reply, From, Res}};

ready({call, From}, {put, Pairs}, #data{name=Name}=Data) ->
    Res = dets:insert(Name, Pairs),
    {keep_state, Data, {reply, From, Res}}.

terminate(Reason, _, #data{name=Name}) ->
    cmkit:log({cmdb_dets, node(), terminated, Reason}),
    dets:close(Name),
    ok.

data_file(Name) -> 
    Workdir = cmkit:config(data_dir, cmdb),
    string:join([Workdir, atom_to_list(Name),  "data.db" ], "/").
