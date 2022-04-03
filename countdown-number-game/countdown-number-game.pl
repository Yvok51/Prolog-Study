%prolog
%
% Michal Tichý

%%
% Countdown number game
% Given a list of positive integers construct an arithmetic expression 
% using addition, subtraction, multiplication and division that 
% evaluates to a given target number.
%
% All partial expressions must be positive.
%
% Example usage: solve([1, 2, 3], 6, plus(1, plus(2, 3))). returns true
%
% Also includes an imprecise solver which finds solutions which are
% within +-100 off the desired target number

%%
% chooseOne(+List, -A, -Rest).
% Choose one random element A from List and return the rest as Rest
%
% Cut in the base case so we get the last element only once
chooseOne([A], A, []) :-
    !.
% Nondeterministicaly either choose the head element or pass over it
chooseOne([A | T], A, T).
chooseOne([X | T], A, [X | Rest]) :-
    chooseOne(T, A, Rest).

%%
% chooseTwo(+List, -A, -B, -Rest).
% Choose two random elements (A, B) from List and return the rest as
% Rest
%
% Cut in the base case so we get the last pair only once
chooseTwo([A, B], A, B, []).
chooseTwo([A, B], B, A, []) :-
    !.
% Nondeterministicaly either choose the head element to be the first
% random element we choose or pass over it
% Include both versions since (A, B) and (B, A) are different
% pairings (this is because division and subtraction are not
% commutative)
chooseTwo([A | T], A, B, Rest) :-
    chooseOne(T, B, Rest).
chooseTwo([B | T], A, B, Rest) :-
    chooseOne(T, A, Rest).
chooseTwo([X | T], A, B, [X | Rest]) :-
    chooseTwo(T, A, B, Rest).


%%
% eval(+Expr, -Value).
% The Value is the result of the evaluation of the Expr expression
eval(Value, Value) :-
    integer(Value).
eval(plus(X, Y), Value) :-
    eval(X, RX),
    eval(Y, RY),
    Value is RX + RY.
eval(multiply(X, Y), Value) :-
    eval(X, RX),
    eval(Y, RY),
    Value is RX * RY.
eval(minus(X, Y), Value) :-
    eval(X, RX),
    eval(Y, RY),
    Value is RX - RY.
eval(divide(X, Y), Value) :-
    eval(X, RX),
    eval(Y, RY),
    Value is RX // RY.

%%
% valid(+A, +B, ?Expr).
% Answers whether A and B expressions can be joined together such that
% Expr expression is composed of them and is valid
%
% I chose for the expressions to only be valid if each intermediate
% result is larger than zero. In subtraction and division this
% requirement is stated explicitly and in addition and multiplication
% the requirement is upheld implicitly as if the operands which form the
% addition/multiplication expression are larger than zero, then so are
% the expressions themselves.
%
% The validity of addition/multiplication expressions is also dependent
% on the left operand being larger than the right one. This helps me in
% reducing the search space while retaining all valid results thanks to
% the commutativity of addition/multiplication.
valid(A, B, plus(A, B)) :-
    eval(A, RA),
    eval(B, RB),
    RA >= RB.
valid(A, B, multiply(A, B)) :-
    eval(A, RA),
    eval(B, RB),
    RA >= RB.
valid(A, B, minus(A, B)) :-
    eval(A, RA),
    eval(B, RB),
    R is RA - RB,
    R > 0.
valid(A, B, divide(A, B)) :-
    eval(A, RA),
    eval(B, RB),
    RB \= 0,
    R is RA // RB,
    R > 0.

%%
% solve_aux(+Numbers, +Target, +PartialSolution, ?Solution).
% Create or check an arithmetic expression, Solution, which is composed
% of numbers from Numbers and evaluates to Target.
% The PartialSolution contains the the partial solution which, when
% added to, should create the final expression Solution
%
% If the PartialSolution evaluates to Target then we have found a
% solution
solve_aux(_, Target, PartialSolution, PartialSolution) :-
    eval(PartialSolution, Target).
% Add another number which is combined with the PartialSolution using an
% arithmetic function
solve_aux(Numbers, Target, PartialSolution, Solution) :-
    chooseOne(Numbers, A, RestOfNumbers),
    valid(A, PartialSolution, NewSolution),
    solve_aux(RestOfNumbers, Target, NewSolution, Solution).
solve_aux(Numbers, Target, PartialSolution, Solution) :-
    chooseOne(Numbers, A, RestOfNumbers),
    valid(PartialSolution, A, NewSolution),
    solve_aux(RestOfNumbers, Target, NewSolution, Solution).
% Add a new arithmetic expression composed of two numbers to the
% PartialSolution
solve_aux(Numbers, Target, PartialSolution, Solution) :-
    chooseTwo(Numbers, A, B, RestOfNumbers),
    valid(A, B, Expr),
    valid(Expr, PartialSolution, NewSolution),
    solve_aux(RestOfNumbers, Target, NewSolution, Solution).

%%
% solve(+A, +N, ?Solution).
% Create or check an arithmetic expression, Solution, which is composed
% of numbers from A and evaluates to N
%
% Pick the first two numbers to start the search with
solve(Numbers, Target, Solution) :-
    chooseTwo(Numbers, A, B, RestOfNumbers),
    valid(A, B, Expr),
    solve_aux(RestOfNumbers, Target, Expr, Solution).

%%
% solve_either(+A, +NOne, +NTwo, ?Solution, -SuccesfullN)
% Solve an arithmetic expression, Solution, which is composed of
% numbers from A and evaluates to either NOne or NTwo and return
% which one it is equal to in SuccessfullN
% If both are succesfull either can be returned as SuccessfullN
solve_either(Numbers, TargetOne, _, Solution, TargetOne) :-
    solve(Numbers, TargetOne, Solution).
solve_either(Numbers, _, TargetTwo, Solution, TargetTwo) :-
    solve(Numbers, TargetTwo, Solution).

%%
% imprecise_solve(+A, +N, ?Solution, -FinalN).
% Solution to 2.2
% Create or check an arithmetic expression, Solution, which is composed
% of numbers from A and evaluates to N or as close to N as possible
% (within a range of N+-100)
% Returns the final value of N it matched in FinalN
imprecise_solve(Numbers, Target, Solution, Target) :-
    solve(Numbers, Target, Solution).
imprecise_solve(Numbers, Target, Solution, FinalTarget) :-
    between(1, 100, OffBy),
    PositiveTarget is Target + OffBy,
    NegativeTarget is Target - OffBy,
    (
        NegativeTarget > 0
    ->
        solve_either(Numbers, PositiveTarget, NegativeTarget, Solution, FinalTarget)
    ;
        solve(Numbers, PositiveTarget, Solution),
        FinalTarget = PositiveTarget
    ).

