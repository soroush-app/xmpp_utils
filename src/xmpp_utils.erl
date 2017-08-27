%%% --------------------------------------------------------------------
%%% BSD 3-Clause License
%%%
%%% Copyright (c) 2017-2018, Soroush
%%% soroush-app.ir
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright
%%% notice, this list of conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright
%%% notice, this list of conditions and the following disclaimer in the
%%% documentation and/or other materials provided with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its
%%% contributors may be used to endorse or promote products derived from
%%% this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%%% FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% --------------------------------------------------------------------
%% @author   Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version  17.8.27
%% @doc
%%           This module provides fast and easy-to-use API for working
%%           on well-known XMPP data structures.
%% @end
%% ---------------------------------------------------------------------

-module(xmpp_utils).
-author("pouriya.jahanbakhsh@gmail.com").

%% ---------------------------------------------------------------------
%% Exports:





%% API:
-export([parse_pkt/1

        ,parse_xml/1
        ,make_xml/1
        ,make_xml/6
        ,make_xml/7

        ,make_pkt/1
        ,make_pkt/6
        ,make_pkt/7

        ,parse_jid/1
        ,make_jid/1
        ,get_bare_jid/1
        ,get_username/1
        ,get_server/1
        ,get_resource/1

        ,change_from_to/1
        ,set_from_to/3

        ,make_xmpp_error/1
        ,make_xmpp_error_pkt/1
        ,make_xmpp_error/2
        ,make_xmpp_error_pkt/2
        ,make_xmpp_error/4
        ,make_xmpp_error_pkt/4

        ,concat/1]).





%% ---------------------------------------------------------------------
%% Records & Macros & Includes:





%% I need:
%%  #xmpp_utils_xml{}
%%  #xmpp_utils_jid{}
-include("xmpp_utils.hrl").





%% ---------------------------------------------------------------------
%% Types:


-type xmlel() :: #xmlel{}.

-type xmpp_xml() :: #xmpp_utils_xml{}.

-type jid() :: #xmpp_utils_jid{}.

-type kind() :: binary().

-type type() :: binary().

-type id() :: binary().

-type children() :: [] | [xmlel()].





-export_type([xmlel/0
             ,xmpp_xml/0
             ,jid/0
             ,kind/0
             ,type/0
             ,id/0
             ,children/0]).


%% ---------------------------------------------------------------------
%% API:





-spec
parse_pkt(binary()) ->
    xmpp_xml().
parse_pkt(Bin) when erlang:is_binary(Bin) ->
    {ok, #xmlel{}=XMLEl} = exml:parse(Bin),
    parse_xml(XMLEl).








-spec
parse_xml(xmlel()) ->
    xmpp_xml().
parse_xml(#xmlel{name = Name, attrs = Attrs, children = Children}) ->
    #xmpp_utils_xml{kind = Name
                   ,from = parse_jid(get_value(<<"from">>
                                    ,Attrs
                                    ,<<>>))
                   ,to = parse_jid(get_value(<<"to">>
                                  ,Attrs
                                  ,<<>>))
                   ,type = get_value(<<"type">>, Attrs, <<>>)
                   ,id = get_value(<<"id">>, Attrs, <<>>)
                   ,children = Children}.







-spec
make_xml(xmpp_xml()) ->
    xmlel().
make_xml(#xmpp_utils_xml{kind = Kind
                        ,from = From
                        ,to = To
                        ,type = Type
                        ,id = Id
                        ,children = Children})
    when erlang:is_binary(Kind) andalso
         erlang:is_tuple(From) andalso
         erlang:is_tuple(To) andalso
         erlang:is_binary(Type) andalso
         erlang:is_binary(Id) andalso
         erlang:is_list(Children) ->
    make_xml(Kind, From, To, Type, Id, Children, []).







-spec
make_xml(kind()
        ,jid() | binary() | 'undefined'
        ,jid() | binary() | 'undefined'
        ,type() | 'undefined'
        ,id() | 'undefined'
        ,children()) ->
    xmlel().
make_xml(Kind, From, To, Type, Id, Children)
    when erlang:is_binary(Kind) andalso
         (erlang:is_tuple(From) orelse
          erlang:is_binary(From) orelse
          From == undefined) andalso
         (erlang:is_tuple(To) orelse
          erlang:is_binary(To) orelse
          To == undefined) andalso
         (erlang:is_binary(Type) orelse Type == undefined) andalso
         (erlang:is_binary(Id) orelse Id == undefined) andalso
         erlang:is_list(Children) ->
    io:format("salam\n"),
    make_xml(Kind, From, To, Type, Id, Children, []).







-spec
make_xml(kind()
        ,jid() | binary() | 'undefined'
        ,jid() | binary() | 'undefined'
        ,type() | 'undefined'
        ,id() | 'undefined'
        ,children()
        ,[] | [{binary(), binary() | undefined | jid()}]) ->
    xmlel().
make_xml(Kind, From, To, Type, Id, Children, Attrs)
    when erlang:is_binary(Kind) andalso
         (erlang:is_tuple(From) orelse
          erlang:is_binary(From) orelse
          From == undefined) andalso
         (erlang:is_tuple(To) orelse
          erlang:is_binary(To) orelse
          To == undefined) andalso
         (erlang:is_binary(Type) orelse Type == undefined) andalso
         (erlang:is_binary(Id) orelse Id == undefined) andalso
         erlang:is_list(Children) ->
    Fold =
        fun
            ({Key, Val}, Acc) when erlang:is_tuple(Val) ->
                [{Key, make_jid(Val)}|Acc];
            ({_Key, Val}=Item, Acc) when erlang:is_binary(Val) ->
                [Item|Acc];
            ({_Key, undefined}, Acc) ->
                Acc
        end,
    #xmlel{name = Kind
          ,attrs = lists:foldl(Fold
                              ,Attrs
                              ,[{<<"from">>, From}
                               ,{<<"to">>, To}
                               ,{<<"type">>, Type}
                               ,{<<"id">>, Id}])
          ,children = Children}.







-spec
make_pkt(xmpp_xml()) ->
    binary().
make_pkt(#xmpp_utils_xml{kind = Kind
                        ,from = From
                        ,to = To
                        ,type = Type
                        ,id = Id
                        ,children = Children})
    when erlang:is_binary(Kind) andalso
         erlang:is_tuple(From) andalso
         erlang:is_tuple(To) andalso
         erlang:is_binary(Type) andalso
         erlang:is_binary(Id) andalso
         erlang:is_list(Children) ->
    exml:to_binary(make_xml(Kind, From, To, Type, Id, Children, [])).







-spec
make_pkt(kind()
        ,jid() | binary() | 'undefined'
        ,jid() | binary() | 'undefined'
        ,type() | 'undefined'
        ,id() | 'undefined'
        ,children()) ->
    binary().
make_pkt(Kind, From, To, Type, Id, Children)
    when erlang:is_binary(Kind) andalso
         (erlang:is_tuple(From) orelse
          erlang:is_binary(From) orelse
          From == undefined) andalso
         (erlang:is_tuple(To) orelse
          erlang:is_binary(To) orelse
          To == undefined) andalso
         (erlang:is_binary(Type) orelse Type == undefined) andalso
         (erlang:is_binary(Id) orelse Id == undefined) andalso
         erlang:is_list(Children) ->
    exml:to_binary(make_xml(Kind, From, To, Type, Id, Children, [])).







-spec
make_pkt(kind()
        ,jid() | binary() | 'undefined'
        ,jid() | binary() | 'undefined'
        ,type() | 'undefined'
        ,id() | 'undefined'
        ,children()
        ,[] | [{binary(), binary() | undefined | jid()}]) ->
    binary().
make_pkt(Kind, From, To, Type, Id, Children, Attrs)
    when erlang:is_binary(Kind) andalso
         (erlang:is_tuple(From) orelse
          erlang:is_binary(From) orelse
          From == undefined) andalso
         (erlang:is_tuple(To) orelse
          erlang:is_binary(To) orelse
          To == undefined) andalso
         (erlang:is_binary(Type) orelse Type == undefined) andalso
         (erlang:is_binary(Id) orelse Id == undefined) andalso
         erlang:is_list(Children) ->
    exml:to_binary(make_xml(Kind, From, To, Type, Id, Children, Attrs)).







-spec
parse_jid(binary()) ->
    jid().
parse_jid(Bin) when erlang:is_binary(Bin) ->
    % this is compiled pattern for:
    %  ^(?:([^@/<>'\"]+)@)?([^@/<>'\"]+)(?:/([^<>'\"]*))?$
    RegexPattern = {re_pattern
                   ,3
                   ,0
                   ,0
                   ,<<69,82,67,80,217,0,0,0,16,0,0,0,1,0,0,0,255,255,255
                    ,255,255,255,255,255,0,0,0,0,0,0,3,0,0,0,64,0,0,0,0
                    ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                    ,125,0,149,25,140,125,0,47,127,0,39,0,1,107,255,255
                    ,255,255,123,127,255,175,254,255,255,255,255,255,255
                    ,255,255,255,255,255,255,255,255,255,255,255,255,255
                    ,255,255,255,255,100,114,0,39,29,64,114,0,47,127,0
                    ,39,0,2,107,255,255,255,255,123,127,255,175,254,255
                    ,255,255,255,255,255,255,255,255,255,255,255,255,255
                    ,255,255,255,255,255,255,255,255,255,100,114,0,39
                    ,140,125,0,47,29,47,127,0,39,0,3,107,255,255,255,255
                    ,123,255,255,175,255,255,255,255,255,255,255,255,255
                    ,255,255,255,255,255,255,255,255,255,255,255,255,255
                    ,255,255,98,114,0,39,114,0,47,27,114,0,149,0>>},
    case re:run(Bin, RegexPattern, [{capture, all, binary}]) of
        {match, Jid} ->
            case Jid of
                [_, <<>>|_] ->
                    #xmpp_utils_jid{username = <<>>
                                   ,server = Bin
                                   ,resource = <<>>};
                [_, User, Server, Res] ->
                    #xmpp_utils_jid{username = User
                                   ,server = Server
                                   ,resource = Res};
                [_, User, Server] ->
                    #xmpp_utils_jid{username = User
                                   ,server = Server
                                   ,resource = <<>>}
            end;
        nomatch ->
            #xmpp_utils_jid{}
    end.







-spec make_jid(jid()) ->
    binary().
make_jid(#xmpp_utils_jid{username = <<>>
                        ,server = Server
                        ,resource = <<>>})
    when erlang:is_binary(Server) ->
    Server;
make_jid(#xmpp_utils_jid{username = Username
                        ,server = Server
                        ,resource = <<>>})
    when erlang:is_binary(Username) andalso
         erlang:is_binary(Server) ->
    <<Username/binary, $@, Server/binary>>;
make_jid(#xmpp_utils_jid{username = Username
                        ,server = Server
                        ,resource = Resource})
    when erlang:is_binary(Username) andalso
         erlang:is_binary(Server) andalso
         erlang:is_binary(Resource) ->
    <<Username/binary, $@, Server/binary, $/, Resource/binary>>.







-spec
get_bare_jid(jid()) ->
    binary().
get_bare_jid(#xmpp_utils_jid{username = <<>>, server = Server})
    when erlang:is_binary(Server) ->
    Server;
get_bare_jid(#xmpp_utils_jid{username = Username, server = Server})
    when erlang:is_binary(Username) andalso
         erlang:is_binary(Server) ->
    <<Username/binary, $@, Server/binary>>;
get_bare_jid(Bin) when erlang:is_binary(Bin) ->
    get_bare_jid(parse_jid(Bin)).







-spec
get_username(jid()) ->
    binary().
get_username(#xmpp_utils_jid{username = Username})
    when erlang:is_binary(Username) ->
    Username;
get_username(JidBinary) when erlang:is_binary(JidBinary) ->
    get_username(parse_jid(JidBinary)).







-spec
get_server(jid()) ->
    binary().
get_server(#xmpp_utils_jid{server = Server})
    when erlang:is_binary(Server) ->
    Server;
get_server(JidBinary) when erlang:is_binary(JidBinary) ->
    get_server(parse_jid(JidBinary)).







-spec
get_resource(jid()) ->
    binary().
get_resource(#xmpp_utils_jid{resource = Resource})
    when erlang:is_binary(Resource) ->
    Resource;
get_resource(JidBinary) when erlang:is_binary(JidBinary) ->
    get_resource(parse_jid(JidBinary)).







-spec
change_from_to(xmpp_xml()) ->
    xmpp_xml().
change_from_to(#xmpp_utils_xml{from = From, to = To}=X)
    when erlang:is_tuple(From) andalso
         erlang:is_tuple(To) ->
    X#xmpp_utils_xml{from = To, to = From}.







-spec
set_from_to(jid() | binary, jid() | binary(), xmpp_xml()) ->
    xmpp_xml().
set_from_to(From, To, #xmpp_utils_xml{}=X)
    when (erlang:is_binary(From) orelse erlang:is_tuple(From)) andalso
         (erlang:is_binary(To) orelse erlang:is_tuple(To)) ->
    X#xmpp_utils_xml{from = if
                                erlang:is_binary(From) ->
                                    parse_jid(From);
                                true ->
                                    From
                            end
                    ,to = if
                              erlang:is_binary(To) ->
                                  parse_jid(To);
                              true ->
                                  To
                          end}.







-spec
make_xmpp_error(binary()) ->
    xmlel().
make_xmpp_error(Reason) when erlang:is_binary(Reason) ->
    make_xmpp_error(Reason, undefined).







-spec
make_xmpp_error_pkt(binary()) ->
    binary().
make_xmpp_error_pkt(Reason) when erlang:is_binary(Reason) ->
    exml:to_binary(make_xmpp_error(Reason)).







-spec
make_xmpp_error(binary(), binary() | 'undefined') ->
    xmlel().
make_xmpp_error(Reason, ErrorText)
    when erlang:is_binary(Reason) andalso
         (erlang:is_binary(ErrorText) orelse ErrorText == undefined) ->
    {Type, Code} =
        case Reason of
            <<"bad-request">> ->
                {<<"modify">>, 400};
            <<"conflict">> ->
                {<<"cancel">>, 409};
            <<"feature-not-implemented">> ->
                {<<"cancel">>, 501};
            <<"forbidden">> ->
                {<<"auth">>, 403};
            <<"gone">> ->
                {<<"modify">>, 302};
            <<"internal-server-error">> ->
                {<<"wait">>, 500};
            <<"item-not-found">> ->
                {<<"cancel">>, 404};
            <<"jid-malformed">> ->
                {<<"modify">>, 400};
            <<"not-acceptable">> ->
                {<<"modify">>, 406};
            <<"not-allowed">> ->
                {<<"cancel">>, 405};
            <<"not-authorized">> ->
                {<<"auth">>, 401};
            <<"payment-required">> ->
                {<<"auth">>, 402};
            <<"recipient-unavailable">> ->
                {<<"wait">>, 404};
            <<"redirect">> ->
                {<<"modify">>, 302};
            <<"registration-required">> ->
                {<<"auth">>, 407};
            <<"remote-server-not-found">> ->
                {<<"cancel">>, 404};
            <<"remote-server-timeout">> ->
                {<<"wait">>, 504};
            <<"resource-constraint">> ->
                {<<"wait">>, 500};
            <<"service-unavailable">> ->
                {<<"cancel">>, 503};
            <<"subscription-required">> ->
                {<<"auth">>, 407};
            <<"undefined-condition">> ->
                {<<"wait">>, 500};
            <<"unexpected-request">> ->
                {<<"wait">>, 400};
            _Other ->
                {<<"wait">>, 500}
        end,
    make_xmpp_error(Reason, ErrorText, Type, Code).







-spec
make_xmpp_error_pkt(binary(), binary() | 'undefined') ->
    binary().
make_xmpp_error_pkt(Reason, ErrorText)
    when erlang:is_binary(Reason),
         (erlang:is_binary(ErrorText) orelse ErrorText == undefined) ->
    exml:to_binary(make_xmpp_error(Reason, ErrorText)).







-spec
make_xmpp_error(binary()
               ,'undefined' | binary()
               ,binary()
               ,integer()) ->
    xmlel().
make_xmpp_error(Reason, Text, Type, Code)
    when erlang:is_binary(Reason) andalso
         (erlang:is_binary(Text) orelse Text =:= undefined) andalso
         erlang:is_binary(Type) andalso
         erlang:is_integer(Code) ->
    XMLNSAttr = {<<"xmlns">>
                ,<<"urn:ietf:params:xml:ns:xmpp-stanzas">>},
    ErrorChildren =
        [#xmlel{name = Reason
               ,attrs = [XMLNSAttr]
               ,children = []}
        |if
             Text == undefined->
                 [];
             true ->
                 [#xmlel{name = <<"text">>
                        ,attrs = [XMLNSAttr
                                 ,{<<"xml:lang">>, <<"en">>}]
                        ,children = [{xmlcdata, Text}]}]
         end],
    #xmlel{name = <<"error">>
          ,attrs = [{<<"code">>, erlang:integer_to_binary(Code)}
                   ,{<<"type">>, Type}]
          ,children = ErrorChildren}.







-spec
make_xmpp_error_pkt(binary()
                   ,'undefined' | binary()
                   ,binary()
                   ,integer()) ->
    binary().
make_xmpp_error_pkt(Reason, Text, Type, Code)
    when erlang:is_binary(Reason) andalso
         (erlang:is_binary(Text) orelse Text =:= undefined) andalso
         erlang:is_binary(Type) andalso
         erlang:is_integer(Code) ->
    exml:to_binary(make_xmpp_error(Reason, Text, Type, Code)).







-spec
concat([] | [xmpp_xml() | xmlel() | binary()]) ->
    binary().
concat(List) when erlang:is_list(List) ->
    concat(List, <<>>).





%% ---------------------------------------------------------------------
%% Internal functions:





get_value(Key, Attrs, Def) ->
    case lists:keyfind(Key, 1, Attrs) of
        {_, Val} ->
            Val;
        false ->
            Def
    end.







concat([#xmpp_utils_xml{}=X|Rest], Bin) ->
    concat(Rest, <<Bin/binary, (make_pkt(X))/binary>>);
concat([#xmlel{}=X|Rest], Bin) ->
    concat(Rest, <<Bin/binary, (exml:to_binary(X))/binary>>);
concat([Bin2|Rest], Bin) when erlang:is_binary(Bin2) ->
    concat(Rest, <<Bin/binary, Bin2/binary>>);
concat([], Bin) ->
    Bin.