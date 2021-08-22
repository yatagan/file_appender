-module(file_appender).
-behaviour(gen_statem).

-export([start/0, stop/0]).
-export([append/2, is_open/1]).
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).

-include_lib("kernel/include/logger.hrl").

-define(CLOSE_TIMEOUT, 10000).

% API

start() ->
	gen_statem:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_statem:stop(?MODULE).

append(FileName, String) ->
	gen_statem:call(?MODULE, {append, FileName, String}).

is_open(FileName) ->
	gen_statem:call(?MODULE, {is_open, FileName}).

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
				logger:notice("Opening file ~p", [FileName]),
				{ok, IO} = file:open(FileName, [append]),
				{IO, Data#{FileName => IO}}
		end,

	Res = file:write(File, io_lib:format("~s~n", [String])),

	{keep_state, Data1, [
		{reply, From, Res},
		{{timeout, FileName}, ?CLOSE_TIMEOUT, close}
	]};

handle_event({timeout, FileName}, close, _State, Data) ->
	logger:notice("Closing file ~p", [FileName]),
	ok = file:close(maps:get(FileName, Data)),
	{keep_state, maps:remove(FileName, Data), []};

handle_event({call, From}, {is_open, FileName}, _State, Data) ->
	{keep_state, Data, [{reply, From, maps:is_key(FileName, Data)}]}.

terminate(_Reason, _State, _Data) ->
    void.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State ,Data}.
