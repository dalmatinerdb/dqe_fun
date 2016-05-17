%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2016, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created :  4 May 2016 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(dqe_fun).

-export([init/0, reg/1, lookup/2]).
-export_type([spec/0]).
-define(TBL, dqe_fun).

-type return_type() :: histogram | metric.
-type list_type() :: return_type() | none.
-type type() :: return_type() | integer | float.
-type function_name() :: binary().

-type realized_value() :: binary().
-type histogram_value() :: binary().
-type fun_state() :: tuple().
-type spec() :: {function_name(), [type()], list_type(), return_type()}.

-type values() :: realized_value() | histogram_value().

-callback spec() ->
     spec().

-callback init(Constants::[integer() | float()]) ->
     fun_state().

-callback chunk(fun_state()) ->
    integer().

-callback run(Parameters::[values()], State :: fun_state()) ->
    {binary(), fun_state()}.

-callback resolution(Constants::[integer() | float()],
                     ResolutionIn::integer()) -> pos_integer().
init() ->
    ets:new(?TBL, [set, public, named_table]).

-spec reg(Module :: module()) -> boolean().
reg(Module) ->
    {Name, Parameters, ListType, ReturnType} = Module:spec(),
    reg(Module, Name, Parameters, ListType, ReturnType).

-spec reg(Module :: module(), Name :: binary(), Params :: [type()],
          ListType :: list_type(), ReturnType :: return_type()) ->
                 ok.
reg(Module, Name, Parameters, ListType, ReturnType) ->
    true = valid_params(Parameters, false),
    Spec = {{Name, Parameters, ListType}, ReturnType, Module},
    ets:insert(?TBL, Spec).

lookup(Name, Parameters) ->
    case lookup(Name, Parameters, none) of
        {error, not_found} ->
            case list_type(Parameters) of
                none ->
                    {error, not_found};
                {ListType, Params1} ->
                    lookup(Name, Params1, ListType)
            end;
        {ok, Element} ->
            {ok, Element}
    end.

lookup(Name, Parameters, ListType) ->
    case ets:lookup(?TBL, {Name, Parameters, ListType}) of
        [] ->
            {error, not_found};
        [Element] ->
            {ok, Element}
    end.

list_type(Parameters) ->
    case lists:reverse(Parameters) of
        [histogram | R] ->
            remove_list(R, histogram);
        [metric | R] ->
            remove_list(R, metric);
        [histogram_list | R] ->
            remove_list(R, histogram);
        [metric_list | R] ->
            remove_list(R, metric);
        _ ->
            none
    end.

remove_list([Type | R], Type) ->
    remove_list(R, Type);
remove_list(L, Type) ->
    {Type, lists:reverse(L)}.



-spec valid_params([type()], boolean()) ->
     boolean().

valid_params([], _) ->
    true;
valid_params([integer | R], HasMetric) ->
    valid_params(R, HasMetric);
valid_params([float | R], HasMetric) ->
    valid_params(R, HasMetric);
valid_params([time | R], HasMetric) ->
    valid_params(R, HasMetric);
valid_params([histogram | R], false) ->
    valid_params(R, true);
valid_params([metric | R], false) ->
    valid_params(R, true);
valid_params([histogram | _R], true) ->
    false;
valid_params([metric | _R], true) ->
    false.

