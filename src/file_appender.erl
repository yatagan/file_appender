-module(file_appender).
-behaviour(gen_statem).

-export([start/0, append/2]).
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).

-include_lib("kernel/include/logger.hrl").

-define(CLOSE_TIMEOUT, 10000).

% API

start() ->
	gen_statem:start({local, ?MODULE}, ?MODULE, [], []).

append(FileName, String) ->
	gen_statem:call(?MODULE, {append, FileName, String}).

%% gen_statem callbacks

init([]) ->
	{ok, state, #{}}.

callback_mode() -> handle_event_function.

handle_event({call, From}, {append, FileName, String}, _State, Data) ->
	{File, Data1} =
		case Data of
			#{FileName := IO} ->
				{IO, Data};
			_ ->
				{ok, IO} = file:open(FileName, [append]),
				{IO, Data#{FileName => IO}}
		end,

	Res = file:write(File, io_lib:format("~s~n", [String])),

	{keep_state, Data1, [
		{reply, From, Res},
		{{timeout, FileName}, ?CLOSE_TIMEOUT, close}
	]};

handle_event({timeout, FileName}, close, _State, Data) ->
	logger:error("Closing the file ~p", [FileName]),
	ok = file:close(maps:get(FileName, Data)),
	{keep_state, maps:remove(FileName, Data), []}.

terminate(_Reason, _State, _Data) ->
    void.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State ,Data}.
