%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2014, 2025 Julien Fischer.
% See the file LICENSE for license details.
%-----------------------------------------------------------------------------%
%
% This module provides tee writer streams which allow stream units to be put
% to multiple underlying writer streams.
%
%-----------------------------------------------------------------------------%

:- module tee.
:- interface.

:- import_module char.
:- import_module io.
:- import_module stream.

%-----------------------------------------------------------------------------%

:- type tee(A, B).

:- func init(A, B) = tee(A, B) <= (output(A, io), output(B, io)).

:- func get_a(tee(A, B)) = A.

:- func get_b(tee(A, B)) = B.

%-----------------------------------------------------------------------------%

% What we would like to do here is:
%
% :- instance stream(tee(A, B), State) <= (output(A, State), output(B, State)).


:- instance stream(tee(A, B), io) <= (output(A, io), output(B, io)).
:- instance output(tee(A, B), io) <= (output(A, io), output(B, io)).

% What we would like to do here is:
%
% :- instance stream(tee(A, B), State) <= (output(A, State), output(B, State)).
% :- instance output(tee(A, B), State) <= (output(A, State), output(B, State)).
%
% But the type class system in Mercury currently doesn't allow us to do that.

:- instance writer(tee(A, B), char, io)
    <= (writer(A, char, io), writer(B, char, io)).
:- instance writer(tee(A, B), float, io)
    <= (writer(A, float, io), writer(B, float, io)).
:- instance writer(tee(A, B), int, io)
    <= (writer(A, int, io), writer(B, int, io)).
:- instance writer(tee(A, B), string, io)
    <= (writer(A, string, io), writer(B, string, io)).

% What we would like to do here is:
%
% :- instance writer(writer(A, B), Unit, State)
%   <= (writer(A, Unit, State), writer(B, Unit, State)).
%
% But the type class system in Mercury doesn't allow us to do that.
%
% Instead we can get almost the same behaviour by doing:

:- type tee_unit(Unit) ---> tee_unit(Unit).

:- instance writer(tee(A, B), tee_unit(Unit), io)
    <= (writer(A, Unit, io), writer(B, Unit, io)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type tee(A, B)
    --->    tee(A, B).

init(A, B) = tee(A, B).

get_a(tee(A, _)) = A.

get_b(tee(_, B)) = B.

%-----------------------------------------------------------------------------%

:- instance stream(tee(A, B), io) <= (output(A, io), output(B, io)) where
[
    ( name(tee(A, B), Name, !State) :-
        name(A, NameA, !State),
        name(B, NameB, !State),
        format("tee(%s, %s)", [s(NameA), s(NameB)], Name)
    )
].

:- instance output(tee(A, B), io) <= (output(A, io), output(B, io)) where
[
    ( flush(tee(A, B), !State) :-
        flush(A, !State),
        flush(B, !State)
    )
].

:- instance writer(tee(A, B), char, io)
    <= (writer(A, char, io), writer(B, char, io)) where
[
    ( put(tee(A, B), Char, !State) :-
        put(A, Char, !State),
        put(B, Char, !State)
    )
].

:- instance writer(tee(A, B), float, io)
    <= (writer(A, float, io), writer(B, float, io)) where
[
    ( put(tee(A, B), Float, !State) :-
        put(A, Float, !State),
        put(B, Float, !State)
    )
].

:- instance writer(tee(A, B), int, io)
    <= (writer(A, int, io), writer(B, int, io)) where
[
    ( put(tee(A, B), Int, !State) :-
        put(A, Int, !State),
        put(B, Int, !State)
    )
].

:- instance writer(tee(A, B), string, io)
    <= (writer(A, string, io), writer(B, string, io)) where
[
    ( put(tee(A, B), String, !State) :-
        put(A, String, !State),
        put(B, String, !State)
    )
].

:- instance writer(tee(A, B), tee_unit(Unit), io)
    <= (writer(A, Unit, io), writer(B, Unit, io)) where
[
    ( put(tee(A, B), tee_unit(Unit), !State) :-
        put(A, Unit, !State),
        put(B, Unit, !State)
    )
].

%-----------------------------------------------------------------------------%
:- end_module tee.
%-----------------------------------------------------------------------------%
