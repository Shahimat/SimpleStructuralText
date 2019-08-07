(*===========================================================================*)
(*                                                                           *)
(*                         The MIT License (MIT)                             *)
(*                                                                           *)
(*       Copyright (c) 2019 Nakhapetyan Gevorg <ngs22071993@gmail.com>       *)
(*         Pascal parser to simple structure text format files (SST)         *)
(*                                                                           *)
(*===========================================================================*)
unit PascalParserSST;

interface

type

  (*
    record - /some ... ;
    id     - /some;
    code   - \67F1\
    string - 'some'
  *)
  stType = (ST_NONE, ST_RECORD, ST_ID, ST_CODE, ST_STRING);
  stPType = ^stType;

Procedure stBind(ValStructText: PString; var ValPos1, ValPos2: PInteger;
  var ValType: stPType; var ValError: PBoolean);
Procedure stReset;
Function  stNextToken: Boolean;
Function  stGetErrorInfo: String;


implementation

type

  stErrorInfo = (STE_NONE);

{$MACRO ON}
{$DEFINE Text :=PText^}

var
  ErrorInfo: stErrorInfo;
  isError: Boolean;
  Pos1, Pos2: Integer;
  CurType: stType;
  PText: PString;

procedure stBind(ValStructText: PString; var ValPos1, ValPos2: PInteger;
  var ValType: stPType; var ValError: PBoolean);
begin
 PText     := ValStructText;
 ValPos1  := @Pos1;
 ValPos2  := @Pos2;
 ValType  := @CurType;
 ValError := @isError;
end;

procedure stReset;
begin
 Pos1 := 0;
 Pos2 := 0;
 CurType := ST_NONE;
 ErrorInfo := STE_NONE;
 isError := False;
end;

function stNextToken: Boolean;
begin

end;

function stGetErrorInfo: String;
begin
 if not isError then
 begin
  Result := 'all right';
  Exit;
 end;
 case ErrorInfo of
  STE_NONE: Result := 'unknown error';
 end;
end;

end.
