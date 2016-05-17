%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2016, Heinz Nikolaus Gies
%%% @doc
%%% We use the same term system we use for the _ac version, the only
%%% differene is that instead of calling F on every inout we first
%%% collect all elements for a term then call F on all of them
%%% combined
%%%
%%%
%%% TODO: how to deal with different sized inputs, that could get
%%% messy!
%%% @end
%%% Created :  4 May 2016 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(dqe_fun_list_flow).



-behaviour(dflow).

-export([init/1, describe/1, start/2, emit/3, done/2]).

-record(state, {
          dqe_fun :: atom(),
          fun_state :: dqe_fun:fun_state(),
          count :: pos_integer(),
          acc = gb_trees:empty(),
          child_pos :: dict:dict(),
          term_for_child = dict:new()
         }).

init([Fun, FunState | SubQs]) ->
    SubQs1 = [{make_ref(), SubQ} || SubQ <- SubQs],
    ChildPos = add_pos(SubQs1),
    Count = length(SubQs1),
    {ok, #state{count = Count,
                dqe_fun = Fun,
                child_pos = ChildPos,
                fun_state = FunState}, SubQs1}.

add_pos(E) ->
    add_pos(E, 1, []).

add_pos([], _P, Acc) ->
    dict:from_list(Acc);
add_pos([{C, _} | R], P, Acc) ->
    add_pos(R, P + 1, [{C, P} | Acc]).


start(_, State) ->
    {ok, State}.

describe(#state{dqe_fun = Fun, fun_state = State}) ->
    Fun:describe(State).


emit(Child, Data,
     State = #state{dqe_fun = Fun, fun_state = FunState, term_for_child = TFC,
                    count = Count, acc = Tree, child_pos = ChildPos}) ->
    Idx = dict:fetch(Child, ChildPos),
    Data1 = {Idx, Data},
    TFC1 = dict:update_counter(Child, 1, TFC),
    Term = dict:fetch(Child, TFC1),
    Tree1 = add_to_tree(Term, Data, Tree),
    case shrink_tree(Fun, FunState, Tree1, Count, <<>>) of
        {FunState1, Tree2, <<>>} ->
            {ok, State#state{fun_state = FunState1, acc = Tree2,
                             term_for_child = TFC1}};
        {FunState1, Tree2, Data1} ->
            {emit, Data1,
             State#state{fun_state = FunState1, acc = Tree2,
                         term_for_child = TFC1}}
    end.

done({last, _Child}, State) ->
    {done, State};

done(_, State) ->
    {ok, State}.

add_to_tree(Term, Data, Tree) ->
    case gb_trees:lookup(Term, Tree) of
        none ->
            gb_trees:insert(Term, [Data], Tree);
        {value, Elements} ->
            gb_trees:update(Term, ordsets:add_element(Data, Elements), Tree)
    end.

shrink_tree(Fun, FunState, Tree, Count, Acc) ->
    case gb_trees:is_empty(Tree) of
        true ->
            {FunState, Tree, Acc};
        _ ->
            case gb_trees:smallest(Tree) of
                {Term0, Data} when length(Data) =:= Count ->
                    {Result, FunState1} = Fun:run([E || {_, E} <- Data], FunState),
                    Tree1 = gb_trees:delete(Term0, Tree),
                    shrink_tree(Fun, FunState1, Tree1, Count,
                                <<Acc/binary, Result/binary>>);
                _ ->
                    {Tree, Acc}
            end
    end.
