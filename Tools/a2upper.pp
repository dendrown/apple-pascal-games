(* ------------------------------------------------------------------------ *)
(* A2Upper:                                                                 *)
(*                                                                          *)
(* Author: ∂³                                                               *)
(* System: Free Pascal                                                      *)
(*                                                                          *)
(* Converts source files created with a mix of upper & lowercase text to    *)
(* all uppercase in order to:                                               *)
(*  1) support Apple II models that do not have lowercase characters, and   *)
(*  2) match the source code listings in Apple Pascal Games (1981).         *)
(* ------------------------------------------------------------------------ *)
PROGRAM A2Upper(INPUT, OUTPUT);

USES sysutils;

VAR
  LineCnt : Integer;

  SrcFile,
  DstFile : Text;

  TxtLine,
  SrcFileName,
  DstFileName : String;


(* ------------------------------------------------------------------------ *)
PROCEDURE AssignFileName(Tag : String;
                         VAR FileName : String;
                         VAR TxtFile  : Text);

BEGIN
  Write(Tag, ': ');
  ReadLn(FileName);
  Assign(TxtFile, FileName);
END;


(* ------------------------------------------------------------------------ *)
BEGIN
  (* FIXME: We need to check that the file exists to handle any I/O errors
            and we may want to check before auto-clobbering the output file *)
  AssignFileName('Source', SrcFileName, SrcFile);
  AssignFileName('>UPPER', DstFileName, DstFile);
  WriteLn('Converting to uppercase: ', SrcFileName, ' => ', DstFileName);

  Reset(SrcFile);
  Rewrite(DstFile);
  LineCnt := 0;

  While NOT EOF(SrcFile) Do
    Begin
      ReadLn(SrcFile, TxtLine);
      WriteLn(DstFile, UpperCase(TxtLine));
      LineCnt := LineCnt + 1
    End;

  Close(SrcFile);
  Close(DstFile);
  WriteLn;
  WriteLn(LineCnt, ' text lines converted!')
END.
