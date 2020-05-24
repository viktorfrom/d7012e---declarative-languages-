% A solution to the second Prolog assignment in D7012E
% By Viktor From

% Generate subsets subroutine, left to right
subroutine_subsets([], _, _, []).
subroutine_subsets([Head|Tail], I, J, [[Sum, I, J, [Head|Tail]]|Res]) :-
    sum_list([Head|Tail], Sum),
    init([Head|Tail], Init),
    New_J is J - 1,
    subroutine_subsets(Init, I, New_J, Res).

% Generate subsets
generate_subsets([], _, _, []).
generate_subsets(XS, I, J, [X | Res]) :-
    tail(XS, Tail),
    New_I is I + 1,
    subroutine_subsets(XS, I, J, X),
    generate_subsets(Tail, New_I, J, Res).

% Return list without head element
init([_], []).
init([Head | Tail], [Head | I]):-
    init(Tail, I).

% Return sum of list
sum_list([], 0).
sum_list([Head|Tail], Sum) :-
   sum_list(Tail, Rest),
   Sum is Head + Rest.

% Return tail of list
tail([_|X], X).

% Return head of list
head([X|_], X).

% Returns a list of sorted subsets
result([], "Invalid input, list empty!").
result(XS, Res) :-
    length(XS, Len),
    generate_subsets(XS, 1, Len, Subsets),
    sort_subsets(Subsets, Res).

% Returns a list of sortet subsets
sort_subsets(List, Output) :-
    sort(1, @=<, List, Output).

% Return K first elements from list
take(_, 0, [Head|Res]).
take(List, K, [Head|Res]) :- 
    head(List, Head),
    tail(List, Tail),
    New_K is K - 1,
    take(Tail, New_K, Res).

% Return K first elements from list
k_smallest_sets(XS, K, Res) :-
    result(XS, Subsets),
    take(Subsets, K, Res).
