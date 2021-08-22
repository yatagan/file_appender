-module(file_appender_tests).

-include_lib("eunit/include/eunit.hrl").

append_test_() ->
	{timeout,
		60,
		fun() ->
			?debugMsg("Starting the file_appender"),
			{ok, _} = file_appender:start(),

			?assertMatch(ok, file_appender:append("a file", "a line")),

			?assertMatch(ok, file_appender:append("a file", "a line 1")),

			?assertMatch(ok, file_appender:append("a file 2", "a line")),

			timer:sleep(12000)
		end}.
