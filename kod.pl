require(goal).
start_A_star(InitState, PathCost, N) :-
	score(InitState, 0, 0, InitCost, InitScore),
	search_A_star([node(InitState, nil, nil, InitCost, InitScore)], [], PathCost, N, 0).


search_A_star(Queue, ClosedSet, PathCost, N, Iter) :-
	fetch(Node, Queue, ClosedSet, RestQueue),
	presentn(Queue, N, Iter),
	continue(Node, RestQueue, ClosedSet, PathCost, N, Iter).


continue(node(State, Action, Parent, Cost, _), _, ClosedSet, path_cost(Path,Cost), _, _) :-
	goal(State), !,
	build_path(node(Parent, _, _, _, _), ClosedSet, [Action/State], Path).

continue(Node, RestQueue, ClosedSet, Path, N, Iter) :-
	expand(Node, NewNodes),
	insert_new_nodes(NewNodes, RestQueue, NewQueue),
	NewIter is Iter+1,
	search_A_star(NewQueue, [Node | ClosedSet], Path, N, NewIter).


fetch(node(State, Action,Parent, Cost, Score), [node(State, Action,Parent, Cost, Score) | RestQueue], ClosedSet, RestQueue) :-
	\+ member(node(State, _, _, _, _), ClosedSet), !.

fetch(Node, [_ | RestQueue], ClosedSet, NewRest) :-
	fetch(Node, RestQueue, ClosedSet, NewRest).


expand(node(State, _,_, Cost, _), NewNodes) :-
	findall(
		node(ChildState, Action, State, NewCost, ChildScore),
		(succ(State, Action, StepCost, ChildState), score(ChildState, Cost, StepCost, NewCost, ChildScore)),
		NewNodes
		).


score(State, ParentCost, StepCost, Cost, FScore) :-
	Cost is ParentCost + StepCost,
	hScore(State, HScore),
	FScore is Cost + HScore.


insert_new_nodes([], Queue, Queue).

insert_new_nodes([Node | RestNodes], Queue, NewQueue) :-
	insert_p_queue(Node, Queue, Queue1),
	insert_new_nodes(RestNodes, Queue1, NewQueue).


insert_p_queue(Node, [], [Node]) :- !.

insert_p_queue(
	node(State, Action, Parent, Cost, FScore),
	[node(State1, Action1, Parent1, Cost1, FScore1) | RestQueue],
	[node(State1, Action1, Parent1, Cost1, FScore1) | Rest1]
	) :-
		FScore >= FScore1, !,
		insert_p_queue(node(State, Action, Parent, Cost, FScore), RestQueue, Rest1).

insert_p_queue(
	node(State, Action, Parent, Cost, FScore),
	Queue,
	[node(State, Action, Parent, Cost, FScore) | Queue]).


build_path(node(nil, _, _, _, _), _, Path, Path) :- !.

build_path(node(EndState, _, _, _, _), Nodes, PartialPath, Path) :-
	del(Nodes, node(EndState, Action, Parent, _, _), Nodes1),
	build_path(node(Parent,_,_, _, _), Nodes1, [Action/EndState | PartialPath],Path).


del([X | R],X,R).
del([Y | R],X,[Y | R1]) :-
	X\=Y,
	del(R,X,R1).

%added:

presentn(Queue, N, Iternum) :-
	format("\nDepth ~w: Hello, these are the first N nodes in the queue:\n", [Iternum, N]),
	printn(Queue, N), nl, nl.

printn(_, 0) :- !.
printn([], _).
printn([X | R], N) :-
	format("~w ", X),
	NewN is N-1,
	printn(R,NewN).

ask(Data) :-
	repeat,
	write("yes|no\n"),
	read(Data),
	valid(Data),
	!.

valid(yes).
valid(no).


read_list(ChosenList,N):-
	readln(List),
	process_list(N,0,List,ChosenList).

process_list(_,_,[],[]).
process_list(N,N,_,[]).
process_list(N,Iter,['.'|_],[]):-
	N\=Iter.
process_list(N,Iter,[X|RList],[X|ProcessedList]):-
	X \='.',
	Iter < N,
	NewIter is Iter + 1,
	process_list(N,NewIter,RList,ProcessedList).
