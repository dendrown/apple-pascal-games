(* ------------------------------------------------------------------------ *)
(* A2Textify:                                                               *)
(*                                                                          *)
(* Author: ∂³                                                               *)
(* System: Free Pascal                                                      *)
(*                                                                          *)
(* Converts source files created using a modern(-ish) editor to match the   *)
(* data format used with TEXT files for the Apple II Pascal/Fortran editor: *)
(*                                                                          *)
(*    DLE indent text CR                                                    *)
(*                                                                          *)
(* See section "TEXTFILES" in the Apple Pascal Language Reference Manual.   *)
(* Note that this utility does NOT add the 1024-byte TEXT file header.      *)
(* Neither does it pad the source with nulls to a 1024-byte boundary.       *)
(*                                                                          *)
(* The header is added when using AppleCommander to copy the "textified"    *)
(* source to a disk image. (Make sure to choose type TEXT.) The end padding *)
(* is added by loading the imported SYSTEM.WRK.TEXT into the Apple Pascal   *)
(* editor and then immediately saving it again.  This utility includes a    *)
(* procedure to add the padding (eliminating the need to edit/save the work *)
(* file), but adding the padding causes AppleCommander to generate an error *)
(* when importing the file. The procedure is commented out until I can      *)
(* resolve the issue.                                                       *)
(* ------------------------------------------------------------------------ *)
PROGRAM A2Textify(INPUT, OUTPUT);

TYPE
  CharFile = File of Char;

CONST
  WORK_TEXT = 'system.wrk.text';

  EX_NOINPUT = 66;

VAR
  LineCnt : Integer;
  SrcFile : Text;
  DstFile : CharFile;

  SrcFileName,
  DstFileName,
  ProgramLine : ShortString;


(* ------------------------------------------------------------------------ *)
PROCEDURE WriteLine(VAR Dst : CharFile;
                    Line : ShortString;
                    From : Integer);

VAR
  I : Integer;

BEGIN
  (* Output the line, byte-by-byte, starting from the requested index *)
  For I := From to Length(Line) Do
    Write(Dst, Line[I]);

  Write(DstFile, Chr($0D))              (* Carriage return  *)
END;


(* ------------------------------------------------------------------------ *)
FUNCTION Translate(VAR Src : Text;
                   VAR Dst : CharFile) : BOOLEAN;

VAR
  I      : Integer;
  Indent : Integer;
  Line   : ShortString;   (* Apple Pascal Editor 80-col limit + O(c) length *)

BEGIN
  If NOT EOF(Src) Then
    Begin
      Line := '';
      ReadLn(Src, Line);
      (* First we need to calculate the spaces for an indented line *)
      I := 1;
      Indent := 0;
      While (I <= Length(Line)) AND  (Line[I] = ' ') Do
        Begin
          Indent := Indent + 1;
          I := I + 1;
        End;

      Write(Dst, Chr($10));             (* Data link escape *)
      Write(Dst, Chr($20 + Indent));    (* Spaces to indent *)
      WriteLine(Dst, Line, Indent+1);
      Translate := TRUE
    End
  Else
    Translate := FALSE
END;


(* ------------------------------------------------------------------------ *)
PROCEDURE PadBlock(VAR Dst : CharFile);

VAR
  TxtCnt : LongInt;
  PadCnt  : LongInt;

BEGIN
  TxtCnt := FileSize(Dst);
  WriteLn('Text bytes: ', TxtCnt:3);

  PadCnt := 1024 - (TxtCnt mod 1024);
  WriteLn('Padding 00: ', PadCnt:3);
  While PadCnt > 0 Do
    Begin
      Write(Dst, Chr($00));             (* ASCII NUL *)
      PadCnt := PadCnt - 1
    End
END;


(* ------------------------------------------------------------------------ *)
BEGIN
  DstFileName := WORK_TEXT;
  Write('Source: ');
  ReadLn(SrcFileName);

  WriteLn('Converting for Apple II Pascal: ', SrcFileName, ' => ', DstFileName);

  (* FIXME: We need to check that the file exists to handle any I/O errors *)
  Assign(SrcFile, SrcFileName);
  Reset(SrcFile);

  (* FIXME: We also need confirmation before clobbering the work file *)
  Assign(DstFile, DstFileName);
  Rewrite(DstFile);

  (* The first line has no data-link-escape *)
  LineCnt := 1;
  ProgramLine := '';
  If NOT EOF(SrcFile) Then
    Begin
      ReadLn(SrcFile, ProgramLine);
      WriteLine(DstFile, ProgramLine, 1)
    End
  Else
    Begin
      WriteLn('Empty Source');
      Halt(EX_NOINPUT);
  End;

  (* Copy byte-by-byte from source to destination *)
  While Translate(SrcFile, DstFile) do
    Begin
      Write('.');
      LineCnt := LineCnt + 1;
    End;
  WriteLn;
  WriteLn('Text lines: ', LineCnt:3);
  
  (* End file on a 1KB boundary *)
  { FIXME: Adding the END padding confuses AppleCommander }
  { PadBlock(DstFile); }

  Close(SrcFile);
  Close(DstFile);
  WriteLn;
  WriteLn('Textified!')
END.
