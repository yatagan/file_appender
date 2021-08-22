-module(file_appender_tests).

-include_lib("eunit/include/eunit.hrl").

append_test_() ->
	{
		setup, fun startup/0, fun teardown/1,
		[
			fun test_actually_append/0,
			{timeout, 20, fun test_close_after_append/0},
			{timeout, 20, fun test_close_after_multiple_appends/0}
		]
	}.

startup() ->
	?debugMsg("Starting a file_appender"),
	{ok, _} = file_appender:start(),

	?debugMsg("Removing old test files"),
	?cmd("rm -f *.eunit~").

teardown(_) ->
	?debugMsg("Stopping the file_appender"),
	ok = file_appender:stop().

test_actually_append() ->
	FileName = "file0.eunit~",
	?assertMatch(ok, file_appender:append(FileName, "a line")),
	?assertMatch(ok, file_appender:append(FileName, "another line")),

	?assertMatch({ok, <<"a line\nanother line\n">>}, file:read_file(FileName)).

test_close_after_append() ->
	FileName = "file1.eunit~",
	?assertMatch(ok, file_appender:append(FileName, "a line")),
	timer:sleep(11000),
	?assert(not file_appender:is_open(FileName)).

test_close_after_multiple_appends() ->
	FileName = "file2.eunit~",
	?assertMatch(ok, file_appender:append(FileName, "a line")),
	timer:sleep(5000),

	%% reset timer with another append
	?assertMatch(ok, file_appender:append(FileName, "a line 1")),
	timer:sleep(6000),
	?assert(file_appender:is_open(FileName)),

	%% after > 10 sec. from last append the file is closed
	timer:sleep(6000),
	?assert(not file_appender:is_open(FileName)).
