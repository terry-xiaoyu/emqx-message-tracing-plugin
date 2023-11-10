-module(emqx_message_tracing_proxy).

-include("emqx_message_tracing.hrl").

-export([init/0, destroy/0, notify/1]).

init() ->
    case emqx_message_tracing_plugin:get_file_handler_configs() of
        #{enable := true} = Confs ->
            emqx_message_tracing_file_handler:init(maps:remove(enable, Confs));
        _ ->
            ok
    end.

destroy() ->
    emqx_message_tracing_file_handler:destroy().

notify(EventMsg) ->
    case emqx_message_tracing_plugin:get_file_handler_configs() of
        #{enable := true} ->
            emqx_message_tracing_file_handler:notify(EventMsg);
        _ ->
            ok
    end.
