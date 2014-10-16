%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Julien Fischer.
% See the file LICENSE for license details.
%-----------------------------------------------------------------------------%
%
% This module implements string reader streams, allowing Mercury strings to be
% used a source of characters for character reader streams.
%
% The string reader types defined in this module support an unbounded amount of
% putback, but will become less efficient if characters that were not
% originally read from the stream are put back onto it.
%
%-----------------------------------------------------------------------------%

:- module string_reader.
:- interface.

%-----------------------------------------------------------------------------%

:- import_module char.
:- import_module maybe.
:- import_module stream.

%-----------------------------------------------------------------------------%
%
% String readers.
%

    % A string reader: an input stream that is initialised with a Mercury
    % string as a source of characters.
    %
:- type string_reader.reader(T).

    % The state that is updated by operations on string reader streams.
    %
:- type string_reader.state(T).

    % An error type for string reader streams.
    % String readers cannot ever return errors, but instances of the stream
    % type classes need to provide one.
    %
:- type string_reader.error
     --->   string_reader.error.

%-----------------------------------------------------------------------------%
%
% String reader creation.
%

    % init_state(State):
    % Create an initial value of a string reader state.
    % Multiple string readers may be attached to this state.
    %
:- some [T] pred init_state(state(T)::uo) is det.

    % init_reader(MaybeName, Src, Reader, !State):
    %
:- pred init_reader(maybe(string)::in, string::in, reader(T)::out,
    state(T)::di, state(T)::uo) is det.

%-----------------------------------------------------------------------------%
%
% Stream type class instances.
%

:- instance error(error).
:- instance stream(reader(T), state(T)).
:- instance input(reader(T), state(T)).
:- instance reader(reader(T), char, state(T), error).
:- instance putback(reader(T), char, state(T), error).
:- instance unbounded_putback(reader(T), char, state(T), error).
:- instance line_oriented(reader(T), state(T)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.
:- import_module store.

%-----------------------------------------------------------------------------%

:- type state(T)
    --->    state.

:- instance store(state(T)) where [].

%-----------------------------------------------------------------------------%

:- type reader(T)
    --->    reader(
                reader_maybe_name    :: maybe(string),
                reader_src           :: string,
                reader_src_length    :: int,
                reader_mutable_info  :: generic_mutvar(reader_mutable_info, state(T))
            ). 

:- type reader_mutable_info
    --->    reader_mutable_info(
                rmi_unget_chars :: list(char),
                % A list of characters that have been ungetted onto the stream.

                rmi_line_number :: int,   
                % The current line number.

                rmi_last_index  :: int
                % The index of the last character we read from the source
                % string.  A value of -1 indicates that we are at the
                % beginning of the source string (either because no characters
                % have been read, or we "ungot" ourselves to that point.)
            ).

%-----------------------------------------------------------------------------%

    % This type is used to avoid warnings about unresolved polymorphism in
    % the predicate init_state/1.
    %
:- type dummy_state_type
    --->    dummy_state_type.

init_state(state : state(dummy_state_type)).

init_reader(MaybeName, Src, Reader, !State) :-
    string.length(Src, SrcLen),
    InitLineNum = 1,
    MutableInfo = reader_mutable_info([], InitLineNum, -1),    
    store.new_mutvar(MutableInfo, MutableInfoVar, !State),
    Reader = reader(MaybeName, Src, SrcLen, MutableInfoVar).

%-----------------------------------------------------------------------------%

:- instance error(error) where [
    error_message(_) = "<<string reader error>>"
].

:- instance stream(reader(T), state(T)) where [
    ( name(Reader, Name, !State) :-
        MaybeName = Reader ^ reader_maybe_name,
        ( 
            MaybeName = yes(Name)
        ;
            MaybeName = no,
            Name = "<<string reader>>"
        )
    )
].

:- instance input(reader(T), state(T)) where [].

:- instance reader(reader(T), char, state(T), error) where [
    ( get(Reader, Result, !State) :-
        Reader = reader(_MaybeName, Src, SrcLen, MutableInfoVar),
        store.get_mutvar(MutableInfoVar, MutableInfo0, !State),
        MutableInfo0 = reader_mutable_info(UngetChars, LineNum, LastIndex),
        (
            UngetChars = [],
            NextIndex = LastIndex + 1,
            ( if NextIndex < SrcLen then
                Char = string.unsafe_index(Src, NextIndex),
                LineNumPrime = ( if Char = '\n' then LineNum + 1 else LineNum ),
                MutableInfo = reader_mutable_info(UngetChars, LineNumPrime, NextIndex),
                store.set_mutvar(MutableInfoVar, MutableInfo, !State),
                Result = ok(Char)
            else
                Result = eof
            )
        ;
            UngetChars = [Char | UngetCharsPrime],
            LineNumPrime = ( if Char = '\n' then LineNum + 1 else LineNum ),
            MutableInfo = reader_mutable_info(UngetCharsPrime, LineNumPrime, LastIndex),
            store.set_mutvar(MutableInfoVar, MutableInfo, !State),
            Result = ok(Char)
        )
    )
].

:- instance stream.putback(reader(T), char, state(T), error) where [
    ( unget(Reader, Char, !State) :-
        Reader = reader(_MaybeName, Src, SrcLen, MutableInfoVar),
        store.get_mutvar(MutableInfoVar, MutableInfo0, !State),
        MutableInfo0 = reader_mutable_info(UngetChars, LineNum, LastIndex),
        (
            UngetChars = [],
            % Try to unget by rewinding the index into the source string.
            % We can do this if the character we are ungetting is already
            % in that position.
            ( if LastIndex > -1, LastIndex < SrcLen then
                LastChar = string.unsafe_index(Src, LastIndex),
                ( if Char = LastChar then
                    UngetCharsPrime = [],
                    LastIndexPrime = LastIndex - 1
                else
                    UngetCharsPrime = [Char],
                    LastIndexPrime = LastIndex
                )
            else
                UngetCharsPrime = [Char | UngetChars],
                LastIndexPrime = LastIndex
            ) 
        ;
            UngetChars = [_ | _],
            UngetCharsPrime = [Char | UngetChars],
            LastIndexPrime = LastIndex
        ),
        LineNumPrime = ( if Char = '\n' then LineNum - 1 else LineNum ),
        MutableInfo = reader_mutable_info(UngetCharsPrime, LineNumPrime, LastIndexPrime),
        store.set_mutvar(MutableInfoVar, MutableInfo, !State)
    )
].

:- instance unbounded_putback(reader(T), char, state(T), error) where [].

:- instance line_oriented(reader(T), state(T)) where [
    ( get_line(Reader, LineNo, !State) :-
        MutableInfoVar = Reader ^ reader_mutable_info,
        store.get_mutvar(MutableInfoVar, MutableInfo0, !State),
        MutableInfo0 = reader_mutable_info(_, LineNo, _)
    ),
    ( set_line(Reader, LineNo, !State) :-
        MutableInfoVar = Reader ^ reader_mutable_info,
        store.get_mutvar(MutableInfoVar, MutableInfo0, !State),
        MutableInfo0 = reader_mutable_info(UngetChars, _, LastIndex),
        MutableInfo = reader_mutable_info(UngetChars, LineNo, LastIndex),
        store.set_mutvar(MutableInfoVar, MutableInfo, !State)
    )
].

%-----------------------------------------------------------------------------%
:- end_module string_reader.
%-----------------------------------------------------------------------------%
