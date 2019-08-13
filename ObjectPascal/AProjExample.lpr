program AProjExample;

{$APPTYPE CONSOLE}

uses PascalSST, PascalParserSST;

var
  Text: String;
  Pos1, Pos2: PInteger;
  TermType: stPType;
  isFinished, isError: PBoolean;

Function GetText: String;
var
 i: Integer;
begin
 Result := '';
 for i := Pos1^ to Pos2^ do
  Result += Text[i];
end;

function GetType: String;
begin
 case TermType^ of
  ST_NONE:        Result := 'NONE';
  ST_STRING:      Result := 'STRING';
  ST_CODE:        Result := 'CODE';
  ST_ID:          Result := 'ID';
  ST_COMMENT:     Result := 'COMMENT';
  ST_RECORD_END:  Result := 'RECORD_END';
  ST_OUT:         Result := 'OUT';
  else            Result := 'UNKNOWN';
 end;
end;

Procedure LoadFromFile(FileName: String; var SomeText: String);
var
 F: TextFile;
 s: String;
begin
 SomeText := '';
 AssignFile(F, FileName);
 Reset(F);
 while not EOF(F) do
 begin
  ReadLn(F, s);
  SomeText += s;
 end;
 CloseFile(F);
end;

begin
  LoadFromFile('TextExample.sst', Text);
  stBind(Pos1, Pos2, TermType, isFinished, isError);
  stBind(@Text);
  stReset;
  repeat
   stNextTerminal;
   Writeln(GetText + #9 + GetType);
  until isFinished^ or isError^;
  Writeln(stGetInfo);

  WriteLn('Press enter to continue...');
  ReadLn;
end.

