-module(emqx_message_tracing_file_handler).

-export([init/1, destroy/0, notify/1, format/2, filter/2]).

-define(DEFAULT_MAX_NO_BYTES, 100 * 1024 * 1024).
-define(DEFAULT_MAX_NO_FILES, 200).

-define(OWN_KEYS, [level,filters,filter_default,handlers]).

init(Confs) ->
    HandlerConfs = make_logger_handler_configs(Confs),
    case logger:add_handler(?MODULE, logger_std_h, HandlerConfs) of
        {error, {already_exist, HandlerId}} ->
            logger:remove_handler(HandlerId),
            ok = logger:add_handler(?MODULE, logger_std_h, HandlerConfs);
        ok ->
            ok
    end.

destroy() ->
    logger:remove_handler(?MODULE).

notify(EventMsg) when is_map(EventMsg) ->
    {ok, StdHandlerConfs} = logger_config:get(logger, emqx_message_tracing_file_handler),
    logger_std_h:log(make_logger_msg(EventMsg), maps:without(?OWN_KEYS, StdHandlerConfs)).

filter(#{msg := _Msg0, meta := #{?MODULE := true}} = LogEvent, _) ->
    LogEvent;
filter(_, _) ->
    stop.

format(#{msg := {report, Msg0}, meta := _Meta}, _Config) ->
    [emqx_utils_json:encode(Msg0), io_lib:nl()].

%% =============================================================================
make_logger_handler_configs(Confs) ->
    #{
        filter_default => stop,
        filters => [{filter_non_my_log_events, {fun ?MODULE:filter/2, #{}}}],
        level => all,
        formatter => {?MODULE, #{}},
        config => #{
            burst_limit_enable => true,
            burst_limit_max_count => 50000,
            burst_limit_window_time => 1000,
            sync_mode_qlen => 200,
            drop_mode_qlen => 200,
            flush_qlen => 1000,
            filesync_repeat_interval => no_repeat,
            overload_kill_enable => false,
            overload_kill_mem_size => 10 * 1024 * 1024,
            overload_kill_qlen => 20000,
            overload_kill_restart_after => 5000,
            type => file,
            file_check => 10000,
            file => maps:get(filename, Confs, "message_tracing.log"),
            max_no_bytes => maps:get(max_no_bytes, Confs, ?DEFAULT_MAX_NO_BYTES),
            max_no_files => maps:get(max_no_files, Confs, ?DEFAULT_MAX_NO_FILES),
            compress_on_rotate => maps:get(compress_on_rotate, Confs, true)
        }
    }.

make_logger_msg(#{payload := Payload} = EventMsg) ->
    make_log_event(EventMsg#{
        payload => base64:encode(Payload),
        payload_encode_type => base64
    });
make_logger_msg(EventMsg) ->
    make_log_event(EventMsg).

make_log_event(EventMsg) ->
    %% we add a special metadata '?MODULE' so we can discard all log messages
    %% that are not generated by this plugin in filter/2
    #{msg => {report, EventMsg}, meta => #{?MODULE => true}}.
