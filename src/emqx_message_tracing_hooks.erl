-module(emqx_message_tracing_hooks).

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/emqx_hooks.hrl").
-include("emqx_message_tracing.hrl").

%% for logging
-include_lib("emqx/include/logger.hrl").

-export([ load/1
        , unload/0
        ]).

%% Client Lifecycle Hooks
-export([ on_client_connect/3
        , on_client_connack/4
        , on_client_disconnected/4
        , on_session_subscribed/4
        , on_session_unsubscribed/4
        ]).

%% Session Lifecycle Hooks
-export([ on_session_discarded/3
        , on_session_takenover/3
        , on_session_terminated/4
        ]).

%% Message Pubsub Hooks
-export([ on_message_publish/2
        , on_message_puback/5
        , on_message_delivered/3
        , on_message_acked/3
        , on_message_dropped/4
        ]).

%% Called when the plugin application start
load(Env) ->
    hook('client.connect',      {?MODULE, on_client_connect, [Env]}),
    hook('client.connack',      {?MODULE, on_client_connack, [Env]}),
    hook('client.disconnected', {?MODULE, on_client_disconnected, [Env]}),
    hook('session.subscribed',  {?MODULE, on_session_subscribed, [Env]}),
    hook('session.unsubscribed',{?MODULE, on_session_unsubscribed, [Env]}),
    hook('session.discarded',   {?MODULE, on_session_discarded, [Env]}),
    hook('session.takenover',   {?MODULE, on_session_takenover, [Env]}),
    hook('session.terminated',  {?MODULE, on_session_terminated, [Env]}),
    hook('message.publish',     {?MODULE, on_message_publish, [Env]}),
    hook('message.puback',      {?MODULE, on_message_puback, [Env]}),
    hook('message.delivered',   {?MODULE, on_message_delivered, [Env]}),
    hook('message.acked',       {?MODULE, on_message_acked, [Env]}),
    hook('message.dropped',     {?MODULE, on_message_dropped, [Env]}).

%% Called when the plugin application stop
unload() ->
    unhook('client.connect',      {?MODULE, on_client_connect}),
    unhook('client.connack',      {?MODULE, on_client_connack}),
    unhook('client.disconnected', {?MODULE, on_client_disconnected}),
    unhook('session.subscribed',  {?MODULE, on_session_subscribed}),
    unhook('session.unsubscribed',{?MODULE, on_session_unsubscribed}),
    unhook('session.discarded',   {?MODULE, on_session_discarded}),
    unhook('session.takenover',   {?MODULE, on_session_takenover}),
    unhook('session.terminated',  {?MODULE, on_session_terminated}),
    unhook('message.publish',     {?MODULE, on_message_publish}),
    unhook('message.puback',      {?MODULE, on_message_puback}),
    unhook('message.delivered',   {?MODULE, on_message_delivered}),
    unhook('message.acked',       {?MODULE, on_message_acked}),
    unhook('message.dropped',     {?MODULE, on_message_dropped}).

on_client_connect(ConnInfo, Props, _Env) ->
    emqx_message_tracing_events:notify_client_connect(ConnInfo, Props),
    {ok, Props}.

on_client_connack(ConnInfo, Rc, Props, _Env) ->
    emqx_message_tracing_events:notify_client_connack(ConnInfo, Rc, Props),
    {ok, Props}.

on_client_disconnected(ClientInfo, Reason, ConnInfo, _Env) ->
    emqx_message_tracing_events:notify_client_disconnected(ClientInfo, ConnInfo, Reason).

on_session_subscribed(ClientInfo, TopicFilter, SubOpts, _Env) ->
    emqx_message_tracing_events:notify_session_subscribed(ClientInfo, TopicFilter, SubOpts),
    ok.

on_session_unsubscribed(ClientInfo, TopicFilter, SubOpts, _Env) ->
    emqx_message_tracing_events:notify_session_unsubscribed(ClientInfo, TopicFilter, SubOpts),
    ok.

on_session_discarded(ClientInfo, SessInfo, _Env) ->
    emqx_message_tracing_events:notify_session_discarded(ClientInfo, SessInfo).

on_session_takenover(ClientInfo, SessInfo, _Env) ->
    emqx_message_tracing_events:notify_session_takenover(ClientInfo, SessInfo).

on_session_terminated(ClientInfo, Reason, SessInfo, _Env) ->
    emqx_message_tracing_events:notify_session_terminated(ClientInfo, SessInfo, Reason).

on_message_publish(Message, _Env) ->
    emqx_message_tracing_events:notify_message_received(Message),
    {ok, Message}.

on_message_puback(_PacketId, Message, _PubRes, Rc, _Env) ->
    emqx_message_tracing_events:notify_message_acked_by_emqx(Message, Rc),
    {ok, Rc}.

on_message_dropped(#message{topic = <<"$SYS/", _/binary>>}, _By, _Reason, _Env) ->
    ok;
on_message_dropped(Message, _, Reason, _Env) ->
    emqx_message_tracing_events:notify_message_dropped(Message, Reason),
    ok.

on_message_delivered(ClientInfo, Message, _Env) ->
    emqx_message_tracing_events:notify_message_sent(Message, ClientInfo),
    {ok, Message}.

on_message_acked(ClientInfo, Message, _Env) ->
    emqx_message_tracing_events:notify_message_acked_by_client(Message, ClientInfo),
    ok.

hook(HookPoint, MFA) ->
    %% use highest hook priority so this module's callbacks
    %% are evaluated before the default hooks in EMQX
    emqx_hooks:add(HookPoint, MFA, _Property = ?HP_HIGHEST).

unhook(HookPoint, MFA) ->
    emqx_hooks:del(HookPoint, MFA).
