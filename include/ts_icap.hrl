%% use by the client to create the request
-record(icap_request, {
          type,
          url,
          requri,
          host,
          port,
          method
         }).

%% 
-record(icap_dyndata, 
        { 
          none
         }
       ).

%% unused
-record(icap,
        {
          status,
          partial
         }
       ).

%%% Supported byte code instructions
%% -define(ECHO, 0).
%% -define(ADD, 1).
%% -define(SUB, 2).
-define(REQUEST, "REQMOD").
-define(GET, "GET").
-define(USER_AGENT, "Tsung").
-define(REQ_SERVICE, "REQ-Service").
-define(RSP_SERVICE, "interscan").

