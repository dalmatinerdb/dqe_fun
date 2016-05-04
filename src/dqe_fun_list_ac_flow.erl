%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2016, Heinz Nikolaus Gies
%%% @doc DFlow element for associative and commutative list functions.
%%% this properties allow us to save some memory by computing data as
%%% it comes in.
%%%
%%% In short it means that:
%%%
%%% -  F([A, B, C]) == F([F([A, B]), C])
%%% -  F([A, B, C]) == F([A, C, B])
%%%
%%% To implement we keep some additional data, for each child we keep
%%% a term (stored in term_for_child). When new data for a child comes
%%% in we add it to the next term for the child, if there is no data
%%% in this term yet we simply store it, if there is data we use F to
%%% combine the existing data in this term with the new childs data.
%%%
%%% @end
%%% Created :  4 May 2016 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(dqe_fun_list_ac_flow).

-behaviour(dflow).

-export([init/1, describe/1, start/2, emit/3, done/2]).

-record(state, {
          dqe_fun :: atom(),
          fun_state :: dqe_fun:fun_state(),
          count :: pos_integer(),
          acc = gb_trees:empty(),
          term_for_child = dict:new()
         }).

init([Fun, FunState | SubQs]) ->
    SubQs1 = [{make_ref(), SubQ} || SubQ <- SubQs],
    Count = length(SubQs1),
    {ok, #state{count = Count,
                dqe_fun = Fun,
                fun_state = FunState}, SubQs1}.

start({_Start, _Count}, State) ->
    {ok, State}.

describe(#state{dqe_fun = Fun, fun_state = State}) ->
    Fun:describe(State).


emit(Child, Data,
     State = #state{dqe_fun = Fun, fun_state = FunState, term_for_child = TFC,
                    count = Count, acc = Tree}) ->
    TFC1 = dict:update_counter(Child, 1, TFC),
    Term = dict:fetch(Child, TFC1),
    {FunState1, Tree1} = add_to_tree(Fun, FunState, Term, Data, Tree),
    case shrink_tree(Tree1, Count, <<>>) of
        {Tree2, <<>>} ->
            {ok, State#state{fun_state = FunState1, acc = Tree2,
                             term_for_child = TFC1}};
        {Tree2, Data1} ->
            {emit, Data1,
             State#state{fun_state = FunState1, acc = Tree2,
                         term_for_child = TFC1}}
    end.

done({last, _Child}, State) ->
    {done, State};

done(_, State) ->
    {ok, State}.

add_to_tree(Fun, FunState, Term, Data, Tree) ->
    case gb_trees:lookup(Term, Tree) of
        none ->
            {FunState, gb_trees:insert(Term, {Data, 1}, Tree)};
        {value, {Sum, Count}} ->
            {FunState1, Sum1} = Fun:run([Sum, Data], FunState),
            {FunState1, gb_trees:update(Term, {Sum1, Count+1}, Tree)}
    end.

shrink_tree(Tree, Count, Acc) ->
    case gb_trees:is_empty(Tree) of
        true ->
            {Tree, Acc};
        _ ->
            case gb_trees:smallest(Tree) of
                {Term0, {Data, FirstCount}} when FirstCount =:= Count ->
                    Tree1 = gb_trees:delete(Term0, Tree),
                    shrink_tree(Tree1, Count, <<Acc/binary, Data/binary>>);
                _ ->
                    {Tree, Acc}
            end
    end.
