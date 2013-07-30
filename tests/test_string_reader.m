%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%

:- module test_string_reader.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string_reader.

:- import_module char.
:- import_module list.
:- import_module maybe.
:- import_module stream.
:- import_module string.

main(!IO) :-
    do_test("empty-string1", "", [], [], no, !IO),
    do_test("empty-string2", "", ['a'], [], no, !IO),
    do_test("empty-string3", "", ['a'], [], yes(30), !IO),
    do_test("empty-string4", "", ['\n', '\n'], [], yes(30), !IO),

    do_test("simple1", "ABC", [], [], no, !IO),
    do_test("simple2", "ABC\nDEF\nGHI\n", [], [], no, !IO),

    do_test("putback1", "ABC", [], ['C', 'B', 'A'], no, !IO),
    do_test("putback2", "ABC", [], ['A', 'B', 'C'], no, !IO).

:- pred do_test(string::in, string::in, list(char)::in,
    list(char)::in, maybe(int)::in, io::di, io::uo) is det.

do_test(TestName, Src, PreFoldPBs, PostFoldPBs, MaybeLineNum, !IO) :-
    some [!State] (
        init_state(!:State),
        init_reader(yes(TestName), Src, Reader, !State),
        (
            MaybeLineNum = yes(InitLineNum),
            set_line(Reader, InitLineNum, !State)
        ;
            MaybeLineNum = no
        ),
        list.foldl(unget(Reader), PreFoldPBs, !State),
        get_line(Reader, PreFoldLineNum, !State),
        input_stream_fold(Reader, add_char, [], Result1, !State),
        io.format("%s: %d: ", [s(TestName), i(PreFoldLineNum)], !IO),
        (
            Result1 = ok(List1),
            get_line(Reader, MidLineNum, !State),
            List1Str = string(List1),
            io.format("%s: %d: ", [s(List1Str), i(MidLineNum)], !IO),
            list.foldl(unget(Reader), PostFoldPBs, !State),
            input_stream_fold(Reader, add_char, [], Result2, !State),
            (
                Result2 = ok(List2),
                get_line(Reader, FinalLineNum, !.State, _),
                List2Str = string(List2),
                io.format("%s: %d\n", [s(List2Str), i(FinalLineNum)], !IO)
            ;
                Result2 = error(_, _),
                io.write_string("ERROR(2)\n", !IO)
            )
        ;
            Result1 = error(_, _),
            io.write_string("ERROR(1)\n", !IO)
        )
    ).

:- pred add_char(char::in, list(char)::in, list(char)::out) is det.

add_char(C, Chars, [C | Chars]).

%-----------------------------------------------------------------------------%
:- end_module test_string_reader.
%-----------------------------------------------------------------------------%
