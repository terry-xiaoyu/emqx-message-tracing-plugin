-module(emqx_message_tracing_plugin).

-include("emqx_message_tracing.hrl").
-include_lib("emqx/include/logger.hrl").

-export([ load_config/0
        , is_enabled/0
        , is_enabled/1
        , enable/0
        , enable/1
        , disable/0
        , disable/1
        , is_payload_compression_enabled/0
        , enable_payload_compression_enabled/0
        , disable_payload_compression_enabled/0
        , notify_no_subscriber_drop/0
        , get_file_handler_configs/0
        ]).

%%--------------------------------------------------------------------
%% Configs
%%--------------------------------------------------------------------

load_config() ->
    ConfFile = filelib:wildcard(filename:join([code:priv_dir(?MODULE), "*.conf"])).,
    ok = emqx_config:init_load(emqx_message_tracing_schema, ConfFile).

enable() ->
    emqx:update_config(?CONFIG_PATH(enable), true).

enable(EventName) ->
    emqx:update_config(?CONFIG_PATH(EventName, enable), true).

disable() ->
    emqx:update_config(?CONFIG_PATH(enable), false).

disable(EventName) ->
    emqx:update_config(?CONFIG_PATH(EventName, enable), false).

is_enabled() ->
    emqx:get_config(?CONFIG_PATH(enable), false).

is_enabled(EventName) ->
    emqx:get_config(?CONFIG_PATH(EventName, enable), false).

enable_payload_compression_enabled() ->
    emqx:update_config(?CONFIG_PATH(payload_compression, enable), true).

disable_payload_compression_enabled() ->
    emqx:update_config(?CONFIG_PATH(payload_compression, enable), false).

is_payload_compression_enabled() ->
    emqx:get_config(?CONFIG_PATH(payload_compression, enable), false).

notify_no_subscriber_drop() ->
    emqx:get_config(?CONFIG_PATH(message_dropped, notify_no_subscriber_drop), false).

get_file_handler_configs() ->
    emqx:get_config(?CONFIG_PATH(file_handler), #{}).
