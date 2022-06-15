-module(matrices).

-export([transpose/1, dimensions/1]).

%% @type rows(). The type does <em>not</em> include the empty matrix, identical
%% to an empty list; a matrix must possess at least one row and one column.
%% @type dimensions(). Number of rows and number of columns, a non-negative
%% integer tuple.

-type rows() :: [[any()]].
-type dimensions() ::
    {NumberOfRows :: non_neg_integer(), NumberOfColumns :: non_neg_integer()}.

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

%% @doc Dimensions of matrix.

-spec dimensions(Rows :: rows()) -> dimensions().
dimensions([Row | Rows]) ->
    dimensions(Rows, 1, length(Row)).

dimensions([], NumberOfRows, NumberOfColumns) ->
    {NumberOfRows, NumberOfColumns};
dimensions([Row | Rows], NumberOfRows, NumberOfColumns)
    when length(Row) == NumberOfColumns ->
    dimensions(Rows, NumberOfRows + 1, NumberOfColumns).
