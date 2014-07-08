-module(led_array_test).

-export([go/0, blink_brick/3, check_pins_aux/2]).

-define(CE_PIN, 8).
-define(GREEN_PIN, 25).
-define(ORANGE_PIN, 24).
-define(RED_PIN, 23).
-define(TIMEOUT, 400).
-define(BRICK_PLACE, [{1, [{16#26, 2#11000000}, {16#27, 2#11000000}]},
		      {2, [{16#23, 2#11000000}, {16#24, 2#11000000}]},
		      {3, [{16#20, 2#11000000}, {16#21, 2#11000000}]},
		      {4, [{16#26, 2#00011000}, {16#27, 2#00011000}]},
		      {5, [{16#23, 2#00011000}, {16#24, 2#00011000}]},
		      {6, [{16#20, 2#00011000}, {16#21, 2#00011000}]},
		      {7, [{16#26, 2#00000011}, {16#27, 2#00000011}]},
		      {8, [{16#23, 2#00000011}, {16#24, 2#00000011}]},
		      {9, [{16#20, 2#00000011}, {16#21, 2#00000011}]}]).

check_pins(Handle) ->
    gpio:open_pin(?GREEN_PIN),
    gpio:open_pin(?ORANGE_PIN),
    gpio:open_pin(?RED_PIN),
    gpio:set_input(?GREEN_PIN),
    gpio:set_input(?ORANGE_PIN),
    gpio:set_input(?RED_PIN),
    Pid = proc_lib:spawn_link(?MODULE, check_pins_aux,
			      [Handle, {1, 1, 1}]),
    timer:send_after(10000, Pid, stop),
    receive			      
	{'EXIT', Pid, normal} -> ok
    end,    
    show_brick(Handle, 1, green),
    show_brick(Handle, 3, green),
    show_brick(Handle, 7, green),
    show_brick(Handle, 9, green),
    gpio:close_pin(?GREEN_PIN),
    gpio:close_pin(?ORANGE_PIN),
    gpio:close_pin(?RED_PIN).

check_pins_aux(Handle, State) ->
    receive
	stop -> ok
    after 0 ->
	    Val1 = gpio:read_pin(?GREEN_PIN),
	    Val2 = gpio:read_pin(?ORANGE_PIN),
	    Val3 = gpio:read_pin(?RED_PIN),
	    case State of
		{Val1, Val2, Val3} ->
		    ok;
		_Else ->
		    update_bricks(Handle, State, {Val1, Val2, Val3})
	    end,
	    timer:sleep(50),
	    check_pins_aux(Handle, {Val1, Val2, Val3})
    end.

update_bricks(Handle, {OldVal1, OldVal2, OldVal3}, {Val1, Val2, Val3}) ->
    case Val1 of
	OldVal1 -> ok;
	_       -> toggle_brick(Handle, 1, green, Val1)
    end,
    case Val2 of
	OldVal2 -> ok;
	_       -> toggle_brick(Handle, 2, orange, Val2)
    end,
    case Val3 of
	OldVal3 -> ok;
	_       -> toggle_brick(Handle, 3, red, Val3)
    end,
    ok.

toggle_brick(Handle, Position, Colour, 0) ->
    show_brick(Handle, Position, Colour);
toggle_brick(Handle, Position, Colour, 1) ->
    hide_brick(Handle, Position, Colour).
		    
show_brick(Handle, Place, Colour) ->
    brick_mask(Handle, Place, Colour, show).
hide_brick(Handle, Place, Colour) ->
    brick_mask(Handle, Place, Colour, hide).

blink_brick(Handle, Place, Colour) ->
    {ok, TRef} = timer:send_after(?TIMEOUT, self(), hide),
    show_brick(Handle, Place, Colour),
    blink_brick_aux(Handle, Place, Colour, TRef).

blink_brick_aux(Handle, Place, Colour, TRef0) ->
    Result =
	receive
	    hide ->
		{ok, TRef1} = timer:send_after(?TIMEOUT, self(), show),
		hide_brick(Handle, Place, Colour),
		{ok, TRef1};
	    show ->
		{ok, TRef2} = timer:send_after(?TIMEOUT, self(), hide),
		show_brick(Handle, Place, Colour),
		{ok, TRef2};
	    stop ->
		{ok, cancel} = timer:cancel(TRef0),
		stopped
	end,
    case Result of
	stopped    -> ok;
	{ok, TRef} ->
	    blink_brick_aux(Handle, Place, Colour, TRef)
    end.

brick_mask(Handle, Place, orange, HideOrShow) ->
    brick_mask(Handle, Place, red, HideOrShow),
    brick_mask(Handle, Place, green, HideOrShow);
brick_mask(Handle, Place, Colour, HideOrShow) ->
    Data = proplists:get_value(Place, ?BRICK_PLACE),
    lists:foreach(fun ({Addr0, Value}) ->
			  Addr = case Colour of
				     red   -> Addr0;
				     green -> Addr0 + 8
				 end,
			  Value0 = spi_read(Handle, Addr),
			  NewValue = case HideOrShow of
					 hide -> Value0 band (bnot Value);
					 show -> Value0 bor Value
				     end,
			  spi_write(Handle, Addr, NewValue)
		  end, Data).
				   

spi_read(Handle, Register) ->
    {ok, [_, Value]} =
	spi_interface:transfer_spi_data(Handle, [Register, 0]),
    Value.

spi_write(Handle, Register, Value) ->
    spi_interface:transfer_spi_data(Handle, [Register, Value]).

%%button_checker(Subscriber, Colour, Pin) ->
%%    timer:send_interval(self(), check_again),
%%    gpio:open_pin(Pin),
%%    gpio:set_input(Pin),
%%    ok.

go() ->
    process_flag(trap_exit, true),
    {ok, Handle} = spi_interface:open_spi_bus(?CE_PIN),
    Configuration = spi_read(Handle, 4),
    spi_write(Handle, 1, 0),
    lists:foreach(fun (Address) -> spi_write(Handle, Address, 0) end,
		  lists:seq(16#20, 16#2F)),
    spi_write(Handle, 4, Configuration bor 1),

    lists:foreach(fun (Address) ->
			  case (Address rem 2) of
			      0 -> spi_write(Handle, Address, 16#aa);
			      1 -> spi_write(Handle, Address, 16#55)
			  end,
			  timer:sleep(500)
		  end,
		  lists:seq(16#20, 16#27)),
    lists:foreach(fun (Address) ->
			  case (Address rem 2) of
			      0 -> spi_write(Handle, Address, 16#55);
			      1 -> spi_write(Handle, Address, 16#aa)
			  end,
			  timer:sleep(500)
		  end,
		  lists:seq(16#28, 16#2F)),
    timer:sleep(2000),

    spi_write(Handle, 16#20, 2#00100100),
    spi_write(Handle, 16#28, 2#00100100),
    spi_write(Handle, 16#21, 2#00100100),
    spi_write(Handle, 16#29, 2#00100100),

    spi_write(Handle, 16#22, 2#11111111),
    spi_write(Handle, 16#2A, 2#11111111),

    spi_write(Handle, 16#23, 2#00100100),
    spi_write(Handle, 16#2B, 2#00100100),
    spi_write(Handle, 16#24, 2#00100100),
    spi_write(Handle, 16#2C, 2#00100100),

    spi_write(Handle, 16#25, 2#11111111),
    spi_write(Handle, 16#2D, 2#11111111),

    spi_write(Handle, 16#26, 2#00100100),
    spi_write(Handle, 16#2E, 2#00100100),
    spi_write(Handle, 16#27, 2#00100100),
    spi_write(Handle, 16#2F, 2#00100100),

    timer:sleep(1000),
%%    lists:foreach(fun (Place) ->
%%			  show_brick(Handle, Place, green),
%%			  timer:sleep(500)
%%		  end, lists:seq(1, 9)),
%%    lists:foreach(fun (Place) ->
%%			  Pid = proc_lib:spawn_link(?MODULE,
%%						    blink_brick,
%%						    [Handle, Place, orange]),
%%			  timer:send_after(2000, Pid, stop),
%%			  receive
%%			      {'EXIT', Pid, normal} -> ok
%%			  end,
%%			  hide_brick(Handle, Place, orange)
%%		  end, lists:seq(1, 9)),
    check_pins(Handle),
    spi_write(Handle, 4, Configuration),
    spi_write(Handle, 7, 1),
    timer:sleep(2000),
    spi_write(Handle, 7, 0),
    ok = spi_interface:close_spi_bus(Handle).
