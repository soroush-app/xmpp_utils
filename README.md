# xmpp_utils
This library provides fast, atomic and easy-to-use API for encoding/decoding some well-known XMPP data structures (stanza and jid).  


### Examples
```erlang
Erlang/OTP 19 [erts-8.3] [source-d5c06c6] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false]
Eshell V8.3  (abort with ^G)
1> Pkt = <<"<message from='1@domain.zone' to='domain' type='chat' id='12345'><body>foo</body></message>">>.
<<"<message from='1@domain.zone' to='domain' type='chat' id='12345'><body>foo</body></message>">>

2> XMPP_XML = xmpp_utils:parse_pkt(Pkt).
{xmpp_utils_xml,<<"message">>,
                {xmpp_utils_jid,<<"1">>,<<"domain.zone">>,<<>>},
                {xmpp_utils_jid,<<>>,<<"domain">>,<<>>},
                <<"chat">>,<<"12345">>,
                [{xmlel,<<"body">>,[],[{xmlcdata,<<"foo">>}]}]}
%% This is an Erlang record and in code is equal to:
%% #xmpp_utils_xml{kind = <<"message">>
%%                ,from = #xmpp_utils_jid{username = <<"1">>, sever = <<"domain.zone">>, resource = <<>>}
%%                ,to = #xmpp_utils_jid{username = <<>>, server = <<"domain">>, reource = <<>>}
%%                ,type = <<"chat">>
%%                ,id = <<"12345">>
%%                ,children = [#xmlel{name = <<"body">>
%%                                   ,attrs = []
%%                                   ,children = [#xmlcdata{content = <<"foo">>}]}]}
%% Just you need to include "xmpp_utils.hrl"


3> xmpp_utils:make_pkt(XMPP_XML).
<<"<message from='1@domain.zone' to='domain' type='chat' id='12345'><body>foo</body></message>">>

4> xmpp_utils:make_pkt(xmpp_utils:change_from_to(XMPP_XML)).
<<"<message from='domain' to='1@domain.zone' type='chat' id='12345'><body>foo</body></message>">>

5> xmpp_utils:make_pkt(xmpp_utils:set_from_to(<<"u@s/r">>, <<"g@conference.s">>, XMPP_XML)).
<<"<message from='u@s/r' to='g@conference.s' type='chat' id='12345'><body>foo</body></message>">>

6> xmpp_utils:make_pkt(<<"iq">>, <<"me@ourserver/Client1">>, {xmpp_utils_jid, <<"you">>, <<"ourserver">>, <<"Client2">>}, <<"get">>, <<"id1234567890">>, [{xmlel, <<"query">>, [{<<"xmlns">>, <<"urn:xmpp:ping">>}], []}]).
<iq from='me@ourserver/Client1' to='you@ourserver/Client2' type='get' id='id1234567890'><query xmlns='urn:xmpp:ping'/></iq>

7> xmpp_utils:parse_jid(<<"me@server/res">>).
{xmpp_utils_jid,<<"me">>,<<"server">>,<<"res">>}
%% This is an Erlang record and in code is equal to:
%% #xmpp_utils_jid{username = <<"me">>, server = <<"server">>, resource = <<"res">>}

8> xmpp_utils:get_bare_jid(<<"me@server/res">>).       
<<"me@server">>

9> xmpp_utils:get_bare_jid(J).                  
<<"me@server">>

10> xmpp_utils:make_xmpp_error(<<"not-acceptable">>).
{xmlel,<<"error">>,
       [{<<"code">>,<<"406">>},{<<"type">>,<<"modify">>}],
       [{xmlel,<<"not-acceptable">>,
               [{<<"xmlns">>,<<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
               []}]}


11> xmpp_utils:make_xmpp_error_pkt(<<"not-acceptable">>).
<<"<error type='modify' code='406'><not-acceptable xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/></error>">>


12> xmpp_utils:make_xmpp_error_pkt(<<"item-not-found">>, <<"Why item not found">>).
<error type='cancel' code='404'><item-not-found xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/><text xml:lang='en' xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>Why item not found</text></error>

```
