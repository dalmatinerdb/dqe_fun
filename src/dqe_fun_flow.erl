-module(dqe_fun_flow).

-behaviour(dflow).

-export([init/2, describe/1, start/2, emit/3, done/2]).

-record(state, {
          dqe_fun :: atom(),
          chunk :: pos_integer(),
          acc = <<>> :: binary(),
          fun_state :: dqe_fun:fun_state()
         }).

init([Fun, FunState], _SubQ) ->
    Chunk = Fun:chunk(FunState),
    {ok, #state{dqe_fun = Fun,
                chunk = Chunk,
                fun_state = FunState}}.

start(_, State = #state{dqe_fun = Fun, chunk = Chunk}) ->
    dflow_span:tag(function, Fun),
    dflow_span:tag(chunk, Chunk),
    {ok, State}.

describe(#state{dqe_fun = Fun, fun_state = State}) ->
    Fun:describe(State).

emit(_Child, Data, State = #state{dqe_fun = Fun, fun_state = FunState,
                                  chunk = 1})
  when is_list(Data) ->
    {Result, FunState1} = Fun:run([Data], FunState),
    {emit, Result, State#state{fun_state = FunState1}};

emit(_Child, Data, State = #state{dqe_fun = Fun, fun_state = FunState,
                                  chunk = ChunkSize, acc = Acc})
  when byte_size(Acc) + byte_size(Data) >= ChunkSize ->
    Size = ((byte_size(Acc) + byte_size(Data)) div ChunkSize) * ChunkSize,
    dflow_span:log("got ~p bytes using ~p", [byte_size(Data), Size]),
    <<ToCompute:Size/binary, Acc1/binary>> = <<Acc/binary, Data/binary>>,
    {Result, FunState1} = Fun:run([ToCompute], FunState),
    {emit, Result, State#state{fun_state = FunState1, acc = Acc1}};

emit(_Child, Data, State = #state{acc = Acc}) ->
    {ok, State#state{acc = <<Acc/binary, Data/binary>>}}.

done(_Child, State = #state{acc = <<>>}) ->
    {done, State};

done(_Child,
     State = #state{dqe_fun = Fun, fun_state = FunState, acc = Acc}) ->
    {Result, FunState1} = Fun:run([Acc], FunState),
    {done, Result, State#state{acc = <<>>, fun_state = FunState1}}.
