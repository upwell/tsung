-module(ts_icap).

-behavior(ts_plugin).

-include("ts_profile.hrl").
-include("ts_icap.hrl").


-export([init_dynparams/0,
        add_dynparams/4,
        get_message/2,
        session_defaults/0,
        parse/2,
        parse_bidi/2,
        dump/2,
        parse_config/2,
        decode_buffer/2,
        new_session/0]).


%%----------------------------------------------------------------------
%% Function: session_default/0
%% Purpose: default parameters for session
%% Returns: {ok, ack_type = parse|no_ack|local, persistent = true|false} 
%%----------------------------------------------------------------------
session_defaults() ->
    {ok, true}.

decode_buffer(Buffer, #icap{}) ->
    Buffer. % FIXME ?

%%----------------------------------------------------------------------
%% Function: new_session/0
%% Purpose: initialize session information
%% Returns: record or []
%%----------------------------------------------------------------------
new_session() ->
    #icap{}.

%%----------------------------------------------------------------------
%% Function: get_message/1
%% Purpose: Build a message/request ,
%% Args:    record
%% Returns: binary
%%----------------------------------------------------------------------
get_message(Req=#icap_request{url=Url}, #state_rcv{session=S}) ->
    {icap_data(Req), S}.

icap_data(#icap_request{url=Url, method=Method,
        requri=ReqUri, host=Host, port=Port}) ->
    HttpData = ?GET ++ " " ++ Url ++ " HTTP/1.0" ++ ?CRLF ++
                "User-Agent: " ++ ?USER_AGENT ++ ?CRLF ++ ?CRLF,
    HttpLen = length(HttpData),
    list_to_binary(
            [Method, " ", ReqUri, " ", "ICAP/1.0", ?CRLF,
            ["Host: ", Host, ":", integer_to_list(Port), ?CRLF],
            ["Encapsulated: req-hdr=0, null-body=",
                integer_to_list(HttpLen), ?CRLF],
            ["Allow: 204", ?CRLF],
            ?CRLF, %% end of the ICAP headers
            HttpData] %% end of HTTP headers, miss Host: FIXME
        ).

parse_bidi(Data, State) ->
    ts_plugin:parse_bidi(Data,State).

dump(A,B) ->
    ts_plugin:dump(A,B).

%%----------------------------------------------------------------------
%% Function: parse/2
%% Purpose: parse the response from the server and keep information
%%          about the response in State#state_rcv.session
%% Args:    Data (binary), State (#state_rcv)
%% Returns: {NewState, Options for socket (list), Close = true|false}
%%----------------------------------------------------------------------
parse(closed, State) ->
    {State#state_rcv{ack_done = true}, [], true};
%% new response, compute data size (for stats)
parse(Data, State=#state_rcv{session=Icap}) ->
    List = binary_to_list(Data),
    TotalSize = size(Data),
    Header = State#state_rcv.acc ++ List,
    ?LOGF("handebug Header[~p] Size[~p]~n", [Header, TotalSize], ?NOTICE),
    parse_headers(Icap, Header, State#state_rcv.host),
    {State#state_rcv{ack_done = true, datasize=TotalSize}, [], false}.

parse_headers(H, Tail, Host) ->
    case get_line(Tail) of
    {line, Line, Tail2} ->
        parse_headers(parse_line(Line, H, Host), Tail2, Host);
    {lastline, Line, Tail2} ->
        {ok, parse_line(Line, H#icap{partial=false}, Host), Tail2};
    {more} -> %% Partial header
        {more, H#icap{partial=true}, Tail}
    end.

is_nb_space(X) ->
    lists:member(X, [$\s, $\t]).

get_line(L) ->
    get_line(L, true, []).
get_line("\r\n\r\n" ++ Tail, _Cap, Cur) ->
    {lastline, lists:reverse(Cur), Tail};
get_line("\r\n", _, _) ->
    {more};
get_line("\r\n" ++ Tail, Cap, Cur) ->
    case is_nb_space(hd(Tail)) of
        true ->  %% multiline ... continue
            get_line(Tail, Cap,[$\n, $\r | Cur]);
        false ->
            {line, lists:reverse(Cur), Tail}
    end;
get_line([$:|T], true, Cur) -> % ':' separator
    get_line(T, false, [$:|Cur]);%the rest of the header isn't set to lower char
get_line([H|T], false, Cur) ->
    get_line(T, false, [H|Cur]);
get_line([Char|T], true, Cur) when Char >= $A, Char =< $Z ->
    get_line(T, true, [Char + 32|Cur]);
get_line([H|T], true, Cur) ->
    get_line(T, true, [H|Cur]);
get_line([], _, _) -> %% Headers are fragmented ... We need more data
    {more}.

parse_line("icap/1.0 " ++ TailLine, Icap, _Host) ->
    parse_status(TailLine, Icap),
    Icap;
parse_line(_Line, Icap, _Host) ->
    Icap.

parse_status([A,B,C|_], Icap) ->
    Status=list_to_integer([A,B,C]),
    ?DebugF("handebug ICAP Status ~p~n", [Status]),
    ts_mon:add({ count, Status }).

%%----------------------------------------------------------------------
%% Function: parse_config/2
%% Purpose:  parse tags in the XML config file related to the protocol
%% Returns:  List
%%----------------------------------------------------------------------
parse_config(Element, Conf) ->
    ts_config_icap:parse_config(Element, Conf).

%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: we dont actually do anything
%% Returns: #icap_request
%%----------------------------------------------------------------------
add_dynparams(_Bool, _DynData, Param, _HostData) ->
    Param#icap_request{}.

%%----------------------------------------------------------------------
%% Function: init_dynparams/0
%% Purpose:  initial dynamic parameters value
%% Returns:  #dyndata
%%----------------------------------------------------------------------
init_dynparams() ->
    #dyndata{proto=#icap_dyndata{}}.
