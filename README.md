file_appender
=====

Run tests
----

    $ rebar3 eunit


Try in REPL
----

    $ rebar3 shell

		> file_appender:start().
		{ok, _Pid}

		> file_appender:append("file.txt~", "a line").
		ok

		%% wait 5 sec. append another line
		> file_appender:append("file.txt~", "a liner").
		ok

		%% wait 9 sec. the file is still open
		%% (14 sec. from the first append)
		> file_appender:is_open("file.txt~").
		true

		%% wait about 2 sec. and the file is closed
		> file_appender:is_open("file.txt~").
		false
