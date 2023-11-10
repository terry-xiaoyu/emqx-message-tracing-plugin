-module(emqx_message_tracing_plugin_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_message_tracing_plugin_sup:start_link(),
    emqx_message_tracing_hooks:load(application:get_all_env()),
    emqx_message_tracing_plugin:load_config(),
    emqx_ctl:register_command(emqx_message_tracing_plugin, {emqx_message_tracing_plugin_cli, cmd}),
    ok = emqx_message_tracing_proxy:init(),
    {ok, Sup}.

stop(_State) ->
    emqx_ctl:unregister_command(emqx_message_tracing_plugin),
    emqx_message_tracing_proxy:destroy(),
    emqx_message_tracing_hooks:unload().
