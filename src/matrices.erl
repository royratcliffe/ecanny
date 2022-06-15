-module(matrices).

-export([transpose/1]).

%% @type rows(). The type does <em>not</em> include the empty matrix, identical
%% to an empty list; a matrix must possess at least one row and one column.

-type rows() :: [[any()]].

%% @doc Transposes matrix.
%%
%% Recursion ends when all the rows `H' of matrix `T' match the empty list `[]'
%% and this cannot appear as a guard condition because standard library {@link
%% lists:all/2} cannot execute within a guard.

-spec transpose(Rows :: [] | rows()) -> [] | rows().
transpose([]) ->
    [];
transpose(Rows) ->
    transpose_(Rows).

transpose_(Rows) ->
    [lists:map(fun hd/1, Rows) | transpose__(lists:map(fun tl/1, Rows))].

transpose__(Rows) ->
    case lists:all(fun(Row) -> Row == [] end, Rows) of
        true ->
            [];
        false ->
            transpose_(Rows)
    end.
