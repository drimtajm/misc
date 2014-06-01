%%%-------------------------------------------------------------------
%%% @author  <pi@pepparkakehus>
%%% @copyright (C) 2014, 
%%% @doc
%%%
%%% @end
%%% Created :  3 May 2014 by  <pi@pepparkakehus>
%%%-------------------------------------------------------------------
-module(led_matrix).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, demo/0]).
-export([clear/0, set_pixel/1, add_pixel/1, clear_pixel/1]).
-export([add_row/1, add_pixels/1, set_pixels/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(WIDTH, 8).
-define(HEIGHT, 8).
-define(I2C_ADDRESS, 16#20).
-define(ROW_ADDRESS, 16#13).
-define(COLUMN_ADDRESS, 16#12).
-define(START_VALUES, [0 || _X <- lists:seq(0,(?HEIGHT-1))]).
-define(SPEED, 5).

-record(state, {i2c_handle  :: pos_integer(),
	        values      :: list(),
		tref        :: timer:timer_ref()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

clear() ->
    gen_server:call(?SERVER, clear_display).

set_pixel({X, Y}) when is_integer(X),
		       is_integer(Y),
		       X >= 0, X =< 7,
		       Y >= 0, Y =< 7 ->
    gen_server:call(?SERVER, {set_pixel, X, Y}).

add_pixel({X, Y}) when is_integer(X),
		       is_integer(Y),
		       X >= 0, X =< 7,
		       Y >= 0, Y =< 7 ->
    gen_server:call(?SERVER, {add_pixel, X, Y}).

add_pixels(PixelList) ->
    gen_server:call(?SERVER, {add_pixels, PixelList}).

set_pixels(PixelList) ->
    gen_server:call(?SERVER, {set_pixels, PixelList}).

add_row(Y) when is_integer(Y),
		Y >= 0, Y =< 7 ->
    gen_server:call(?SERVER, {add_row, Y}).

clear_pixel({X, Y}) when is_integer(X),
			 is_integer(Y),
			 X >= 0, X =< 7,
			 Y >= 0, Y =< 7 ->
    gen_server:call(?SERVER, {clear_pixel, X, Y}).

demo() ->
    {ok, _Pid} = start_link(),
    loop(set),
    ok = clear(),
    timer:sleep(500),
    loop(add),
    timer:sleep(500),
    loop(clear),
    timer:sleep(500),
    bulle(1),
    timer:sleep(750),
    ok = clear(),
    bulle(10),
    timer:sleep(500),
    sinus(),
    timer:sleep(750),
    clear(),
    circles1(),
    smiley(),
    timer:sleep(2000),
    circles2(),
    timer:sleep(100),
    sun(),
    clear(),
    star(),
    timer:sleep(100),
    cross(),
    timer:sleep(400),
    e(),
    timer:sleep(4000),
    clear(),
    stop().

star() ->
    line(0,0,7,7),
    line(4,0,3,7),
    line(7,0,0,7),
    line(0,3,7,4).

cross() ->
    cross(0, 120).
cross(_, 0) ->
    ok;
cross(7, It) ->
    cross(0, It);
cross(X0, It) ->
    clear(),
    X1 = 7-X0,
    line(X0, 0, X1, 7),
    line(7, X0, 0, X1),
    timer:sleep(50),
    cross(X0+1, It-1).

loop(Operation) ->
    loop(0, 0, Operation).

loop(_X, ?HEIGHT, _Operation) ->
    ok;
loop(?WIDTH, Y, Operation) ->
    loop(0, Y+1, Operation);
loop(X, Y, Operation) ->
    case Operation of
	set   -> ok = set_pixel({X, Y});
	add   -> ok = add_pixel({X, Y});
	clear -> ok = clear_pixel({X, Y})
    end,
    timer:sleep(500 div ?SPEED),
    loop(X+1, Y, Operation).

sinus() ->
    sinus(0, 0),
    ok.

sinus(_X, 50) ->
    ok;
sinus(?WIDTH, X0) ->
    clear(),
    sinus(0, X0 + 1);
sinus(X, X0) ->
    Y0 = math:sin(math:pi()*(X-X0)/4),
    Y = 4 + round(3*Y0),
    add_pixel({X, Y}),
    timer:sleep(10),
    sinus(X+1, X0).

e() ->
    set_pixels(lists:reverse([{2,6},{3,6},{4,6},{5,6},{1,5},{2,5},{6,5},
		{1,4},{2,4},{3,4},{4,4},{5,4},{6,4},
		{1,3},{2,3},{2,2},{3,2},{4,2},{5,2},{6,2}])).

line(X1, Y1, X2, Y2) when Y2 < Y1 ->
    line(X2, Y2, X1, Y1);
line(X1, 0, X2, 7) when X2 == X1 - 1;
			X2 == X1 + 1 ->
    add_pixels([{X1, 0}, {X1, 1}, {X1, 2}, {X1, 3},
		{X2, 4}, {X2, 5}, {X2, 6}, {X2, 7}]);
line(0, 3, 7, 4)  -> 
    add_pixels([{0, 3}, {1, 3}, {2, 3}, {3, 3},
		{4, 4}, {5, 4}, {6, 4}, {7, 4}]);
line(7, 3, 0, 4) -> 
    add_pixels([{7, 3}, {6, 3}, {5, 3}, {4, 3},
		{3, 4}, {2, 4}, {1, 4}, {0, 4}]);
line(2, 0, 5, 7) ->
    add_pixels([{2, 0}, {2, 1}, {3, 2}, {3, 3},
		{4, 4}, {4, 5}, {5, 6}, {5, 7}]);
line(5, 0, 2, 7) ->
    add_pixels([{5, 0}, {5, 1}, {4, 2}, {4, 3},
		{3, 4}, {3, 5}, {2, 6}, {2, 7}]);
line(0, 2, 7, 5) ->
    add_pixels([{0, 2}, {1, 2}, {2, 3}, {3, 3},
		{4, 4}, {5, 4}, {6, 5}, {7, 5}]);
line(7, 2, 0, 5) ->
    add_pixels([{7, 2}, {6, 2}, {5, 3}, {4, 3},
		{3, 4}, {2, 4}, {1, 5}, {0, 5}]);
line(1=X1, 0=Y1, 6, 7) ->
    add_pixels([{X1, Y1},     {X1+1, Y1+1}, {X1+1, Y1+2}, {X1+2, Y1+3},
		{X1+3, Y1+4}, {X1+4, Y1+5}, {X1+4, Y1+6}, {X1+5, Y1+7}]);
line(6=X1, 0=Y1, 1, 7) ->
    add_pixels([{X1, Y1},     {X1-1, Y1+1}, {X1-1, Y1+2}, {X1-2, Y1+3},
		{X1-3, Y1+4}, {X1-4, Y1+5}, {X1-4, Y1+6}, {X1-5, Y1+7}]);
line(0=X1, 1=Y1, 7, 6) ->
    add_pixels([{X1, Y1},     {X1+1, Y1+1}, {X1+2, Y1+1}, {X1+3, Y1+2},
		{X1+4, Y1+3}, {X1+5, Y1+4}, {X1+6, Y1+4}, {X1+7, Y1+5}]);
line(7=X1, 1=Y1, 0, 6) ->
    add_pixels([{X1, Y1},     {X1-1, Y1+1}, {X1-2, Y1+1}, {X1-3, Y1+2},
		{X1-4, Y1+3}, {X1-5, Y1+4}, {X1-6, Y1+4}, {X1-7, Y1+5}]);
line(0=X1, 0=Y1, 7, 7) ->
    add_pixels([{X1, Y1},     {X1+1, Y1+1}, {X1+2, Y1+2}, {X1+3, Y1+3},
		{X1+4, Y1+4}, {X1+5, Y1+5}, {X1+6, Y1+6}, {X1+7, Y1+7}]);
line(7=X1, 0=Y1, 0, 7) ->
    add_pixels([{X1, Y1},     {X1-1, Y1+1}, {X1-2, Y1+2}, {X1-3, Y1+3},
		{X1-4, Y1+4}, {X1-5, Y1+5}, {X1-6, Y1+6}, {X1-7, Y1+7}]).

sun() ->
    sun(1, 30).
sun(_, 0) ->
    ok;
sun(1, It) ->
    set_pixels([{3,0},{3,1},{6,1},{0,2},{3,2},{4,2},{5,2},{1,3},{2,3},{5,3},
		{2,4},{5,4},{6,4},{2,5},{3,5},{4,5},{7,5},{1,6},{4,6},{4,7}]),
    timer:sleep(60),
    sun(2, It-1);
sun(2, It) ->
    set_pixels([{4,0},{1,1},{4,1},{2,2},{3,2},{4,2},{7,2},{2,3},{5,3},{6,3},
		{1,4},{2,4},{5,4},{0,5},{3,5},{4,5},{5,5},{3,6},{6,6},{3,7}]),
    timer:sleep(60),
    sun(3, It-1);
sun(3, It) ->
    set_pixels([{5,0},{1,1},{4,1},{2,2},{3,2},{4,2},{2,3},{5,3},{6,3},{7,3},
		{0,4},{1,4},{2,4},{5,4},{3,5},{4,5},{5,5},{3,6},{6,6},{2,7}]),
    timer:sleep(60),
    sun(4, It-1);
sun(4, It) ->
    set_pixels([{2,0},{3,1},{6,1},{3,2},{4,2},{5,2},{0,3},{1,3},{2,3},{5,3},
		{2,4},{5,4},{6,4},{7,4},{2,5},{3,5},{4,5},{1,6},{4,6},{5,7}]),
    timer:sleep(60),
    sun(1, It-1).

smiley() ->
    circle(4),
    add_pixels(lists:reverse([{2,5},{5,5},{2,3},{5,3},{3,2},{4,2}])).

circles1() ->
    circles(1, grow, 40, startover).
circles2() ->
    circles(4, shrink, 30, donotstartover).
circles(_, _, 0, _) ->
    ok;
circles(5, grow, It, startover) ->
    circles(1, grow, It, startover);
circles(5, grow, It, donotstartover) ->
    circles(4, shrink, It, donotstartover);
circles(0, shrink, It, donotstartover) ->
    circles(1, grow, It, donotstartover);
circles(Size, grow, It, Mode) ->
    circle(Size),
    timer:sleep(50*Size),
    circles(Size+1, grow, It-1, Mode);
circles(Size, shrink, It, Mode) ->
    circle(Size),
    timer:sleep(50*Size),
    circles(Size-1, shrink, It-1, Mode).

circle(1) ->
    set_pixels([{3,3},{4,3},{3,4},{4,4}]);
circle(2) ->
    set_pixels([{3,2},{4,2},{2,3},{5,3},{2,4},{5,4},{3,5},{4,5}]);
circle(3) ->
    set_pixels([{3,1},{4,1},{2,2},{5,2},{1,3},{6,3},
		{1,4},{6,4},{2,5},{5,5},{3,6},{4,6}]);
circle(4) ->
    set_pixels([{2,0},{3,0},{4,0},{5,0},{1,1},{6,1},
		{0,2},{7,2},{0,3},{7,3},{0,4},{7,4},{0,5},{7,5},
		{1,6},{6,6},{2,7},{3,7},{4,7},{5,7}]).

bulle(Speed) ->
    bulle(0, 0, -1, -1, ?WIDTH, ?HEIGHT, x, Speed).

bulle(_X, _Y, MinX, MinY, MaxX, MaxY, _Dir, _Speed)
  when MinX >= MaxX;
       MinY >= MaxY ->
    ok;
bulle(MaxX, Y, MinX, MinY, MaxX, MaxY, x, Speed) ->
    bulle(MaxX - 1, Y, MinX, MinY + 2, MaxX, MaxY, y, Speed);
bulle(X, MaxY,  MinX, MinY, MaxX, MaxY, y, Speed) ->
    bulle(X, MaxY - 1, MinX, MinY, MaxX - 2, MaxY, xm, Speed);
bulle(MinX, Y, MinX, MinY, MaxX, MaxY, xm, Speed) ->
    bulle(MinX + 1, Y, MinX, MinY, MaxX, MaxY - 2, ym, Speed);
bulle(X, MinY, MinX, MinY, MaxX, MaxY, ym, Speed)->
    bulle(X, MinY + 1, MinX + 2, MinY, MaxX, MaxY, x, Speed);
bulle(X, Y, MinX, MinY, MaxX, MaxY, Dir, Speed) ->
    add_pixel({X, Y}),
    timer:sleep(100 div Speed),
    {NewX, NewY} = case Dir of
		       x  -> {X+1, Y};
		       y  -> {X, Y+1};
		       xm -> {X-1, Y};
		       ym -> {X, Y-1}
		   end,
    bulle(NewX, NewY, MinX, MinY, MaxX, MaxY, Dir, Speed).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, Handle} = i2c_interface:open_i2c_bus(?I2C_ADDRESS),
    %% Configure the control device: Set rows and columns to output
    i2c_interface:write_i2c_smbus_byte(Handle, 0, 0),
    i2c_interface:write_i2c_smbus_byte(Handle, 1, 0),
    put(row, 0),
    {ok, TRef} = timer:send_interval(2, display),
    {ok, #state{i2c_handle=Handle,
		values=?START_VALUES,
		tref=TRef}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(clear_display, _From, State) ->
    Reply = clear_display(State),
    {reply, Reply, State#state{values=?START_VALUES}};
handle_call({set_pixel, X, Y}, _From, State) ->
    Values = add_pixel_to_values({X, Y}, ?START_VALUES),
    {reply, ok, State#state{values=Values}};
handle_call({add_pixel, X, Y}, _From, State) ->
    Values = add_pixel_to_values({X, Y}, State#state.values),
    {reply, ok, State#state{values=Values}};
handle_call({add_row, Y}, _From, State) ->
    Values = lists:map(fun (Y0) when Y == Y0 ->
			       16#ff;
			   (Y0) ->
			       lists:nth(Y0+1, State#state.values)
		       end, lists:seq(0,7)),
    {reply, ok, State#state{values=Values}};
handle_call({add_pixels, PixelList}, _From, State) ->
    Values = add_pixels_to_values(PixelList, State#state.values),
    {reply, ok, State#state{values=Values}};
handle_call({set_pixels, PixelList}, _From, State) ->
    Values = add_pixels_to_values(PixelList, ?START_VALUES),
    {reply, ok, State#state{values=Values}};
handle_call({clear_pixel, X, Y}, _From, State) ->
    Values = clear_pixel_from_values({X, Y}, State#state.values),
    {reply, ok, State#state{values=Values}};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Request, _From, State) ->
    {stop, {unknown_request, Request}, State}.

clear_display(#state{i2c_handle=Handle}) ->
    i2c_interface:write_i2c_smbus_byte(Handle, ?ROW_ADDRESS, 16#ff),
    i2c_interface:write_i2c_smbus_byte(Handle, ?COLUMN_ADDRESS, 0),
    ok.

add_pixel_to_values(Pixel, Values) ->
    set_pixel_values(Pixel, 1, Values, 0, []).
clear_pixel_from_values(Pixel, Values) ->
    set_pixel_values(Pixel, 0, Values, 0, []).
set_pixel_values(_Pixel, _PValue, [], ?HEIGHT, Result) ->
    lists:reverse(Result);
set_pixel_values({X, Y}, PValue, [Value | RestValues], Y, Result) ->
    NewResult = case PValue of
		    0 -> [Value band ((1 bsl X) bxor 16#ff) | Result];
		    1 -> [Value bor (1 bsl X) | Result]
		end,
    set_pixel_values({X, Y}, PValue, RestValues, Y+1, NewResult); 
set_pixel_values(Pixel, PValue, [Value | RestValues], Y, Result) ->
    set_pixel_values(Pixel, PValue, RestValues, Y+1, [Value | Result]).

add_pixels_to_values(PixelList, Values) ->
    add_pixels_to_values(PixelList, Values, 0, []).
add_pixels_to_values([], [], ?HEIGHT, Result) ->
    lists:reverse(Result);
add_pixels_to_values([{X, Y} | Rest], [Value | RestValues], Y, Result) ->
    NewValue = Value bor (1 bsl X),
    add_pixels_to_values(Rest, [NewValue | RestValues], Y, Result);
add_pixels_to_values(PixelList, [Value | RestValues], Y, Result) ->
    add_pixels_to_values(PixelList, RestValues, Y+1, [Value | Result]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(display, #state{i2c_handle=Handle,
			    values=Values}=State) ->
    Row=get(row),
    ColumnValue = lists:nth(Row+1, Values),
    case ColumnValue of
	0 -> ok;
	_ ->
	    RowValue = (1 bsl Row) bxor 16#ff,
	    i2c_interface:write_i2c_smbus_byte(Handle, ?COLUMN_ADDRESS, 0),
	    i2c_interface:write_i2c_smbus_byte(Handle, ?ROW_ADDRESS, RowValue),
	    i2c_interface:write_i2c_smbus_byte(Handle, ?COLUMN_ADDRESS, ColumnValue)
    end,
    put(row, ((Row+1) rem 8)),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    timer:cancel(State#state.tref),
    clear_display(State),
    i2c_interface:close_i2c_bus(State#state.i2c_handle),
    case Reason of
	normal -> ok;
	_Else  -> error(Reason)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

