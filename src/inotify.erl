-module(inotify).
-export([start/1, stop/1, parse/1, check/0]).

-type event_type() :: create | delete | isdir | modify | close | rename | undefined.
-type port_handle() :: port().
-type path() :: string().

-spec check() -> boolean().
check() ->
    case os:find_executable("inotifywait") of
        false -> false;
        _Bin -> true
    end.

%% inotifywait ignores STDPIPE on STDIN
-spec start(path() | [path()]) -> port_handle().
start(Paths) ->
    _ = check(),
    Args = [
        "-c", "inotifywait $0 $@ & PID=$!; read a; kill $PID",
        "-m", "-q", "-e", "close_write", "-e", "moved_to", "-e", "create", "-e", "delete", "-r", Paths],
    % Updated port options format for OTP 24 compatibility
    PortOpts = #{
        args => Args,
        line => 16384,
        in => stream,
        out => stream,
        exit_status => true
    },
    erlang:open_port({spawn_executable, os:find_executable("sh")}, maps:to_list(PortOpts)).

-spec stop(port_handle()) -> true.
stop(Port) ->
    erlang:port_close(Port).

-spec parse(string()) -> {path(), [event_type()]}.
parse(Line) ->
    {match, [Dir, Flags1, DirEntry]} = re:run(Line, re(), [{capture, all_but_first, list}]),
    Flags = [event_map(F) || F <- string:tokens(Flags1, ",")],
    Path = Dir ++ DirEntry,
    {Path, Flags}.

-spec event_map(string()) -> event_type().
event_map("CREATE") -> create;
event_map("DELETE") -> delete;
event_map("ISDIR") -> isdir;
event_map("CLOSE_WRITE") -> modify;
event_map("CLOSE") -> close;
event_map("MOVED_TO") -> rename;
event_map(_) -> undefined.

-spec re() -> term().
re() ->
    case get(inotifywait_re) of
        undefined ->
            {ok, R} = re:compile("^(.*/) ([A-Z_,]+) (.*)$", [unicode]),
            put(inotifywait_re, R),
            R;
        V -> V
    end.
