(*===========================================================================*)
(*                                                                           *)
(*                         The MIT License (MIT)                             *)
(*                                                                           *)
(*       Copyright (c) 2019 Nakhapetyan Gevorg <ngs22071993@gmail.com>       *)
(*       ObjectPascal parser of simple structure text format files (SST)     *)
(*                                                                           *)
(*===========================================================================*)
unit PascalParserSST;

interface

type

 (*
    record - /some ... ;
    id     - /some
    code   - \6AF1
    string - 'some'
  *)
  stType = (ST_NONE, ST_ID, ST_CODE, ST_STRING, ST_RECORD_END, ST_COMMENT,
    ST_OUT);
  stPType = ^stType;

Procedure stBind(var ValStructText: String; var ValPos1, ValPos2: PInteger;
  var ValType: stPType; var ValError: PBoolean; var SomeType: stPType);
Procedure stReset;
Function  stNextTerminal: Boolean;
Function  stGetErrorInfo: String;


implementation

uses GStack;

type

 STlexeme = (
   STL_NONE, STL_DIGIT, STL_LET16, STL_LETTER, STL_ID_SYMBOLS, STL_CODE_MARK,
   STL_RECORD_BEGIN, STL_RECORD_END, STL_QUOTATION_MARK, STL_TAB
 );

 PSTterm = ^STterm;
 STterm = (
   (*Terminals*)
   STT_NONE, STT_STRING, STT_CODE, STT_ID, STT_RECORD_END, STT_OUT,
   (*Nonterminals (laws)*)
   STN_SST, STN_R, STN_RR, STN_V
 );

 { TLexerST }

 PTLexerST = ^TLexerST;
 TLexerST = class
  private
   Stage: Integer;
   CurType: stType;
   CurLexeme: STlexeme;
   Pos, PosText1, PosText2: Integer;
   PText: PString;
   Err, Exelent: Boolean;
   Function GetChar: Char; Inline;
   Procedure DETECT_NONE;
   Procedure DETECT_STRING;
   Procedure DETECT_CODE;
   Procedure DETECT_ID;
   Procedure DETECT_COMMENT;
   Procedure GetLexeme;
   Procedure Cycle;
   Procedure Step;
   Procedure Back;
   Procedure SetPar(SomeType: stType); Overload;
   Procedure SetPar(SomeLevel: Integer); Overload;
   Procedure SetPar(SomeType: stType; SomeLevel: Integer); Overload;
   Property  C: Char read GetChar;
   Property  CL: STlexeme read CurLexeme;
  public
   Constructor Create;
   Destructor  Destroy;
   Procedure Clear;
   Procedure Reset;
   Procedure Bind(var SomeText: String; var Pos1, Pos2: PInteger;
     var SomeLexeme: stPType);
   Function  NextTerminal: Boolean;
 end;

 { TParserST }

 TParserST = class
  private
   Lex: PTLexerST;
   CurTerm: STterm;
   Err: Boolean;
   type stTermStack = specialize TStack<STterm>;
   var  Stack: stTermStack;
   Function GetCurType: stType; inline;
   Procedure Push(ValType: STterm); Overload; inline;
   Procedure Push(const ValTypes: array of STterm); Overload; inline;
   Procedure Pop;   inline;
   Procedure toSST; inline;
   Procedure toR;   inline;
   Procedure toRR;  inline;
   Procedure toV;   inline;
   Property  CurType: stType read GetCurType;
  public
   Constructor Create;
   Destructor  Destroy;
   Procedure Clear;
   Procedure Bind(SomeLexer: PTLexerST);
   Procedure Reset;
   Function  CheckType: Boolean;
   Function  NextTerminal: Boolean;
 end;

 stInfoType = (STE_RIGHT, STE_ERROR_UNKNOWN);

{ TControllerBlock }

TControllerBlock = class
 private
  isError: Boolean;
  Info: stInfoType;
  Param1, Param2: String;
 public
  Function GetInfo: String;
  Procedure Msg(Message: stInfoType; SomeParam1, SomeParam2: String); Overload;
  Procedure Msg(Message: stInfoType; SomeParam: String); Overload;
  Procedure Msg(Message: stInfoType); Overload;
  //Procedure Bind
end;

{$MACRO ON}

var
  stInfo: stInfoType;
  Lexer:  TLexerST;
  Parser: TParserST;

procedure stBind(var ValStructText: String; var ValPos1, ValPos2: PInteger;
  var ValType: stPType; var ValError: PBoolean; var SomeType: stPType);
begin
 Lexer.Bind(ValStructText, ValPos1, ValPos2, SomeType);
 //ValType  := @CurType;
 //ValError := @isError;
end;

procedure stReset;
begin
 //Pos1 := 0;
 //Pos2 := 0;
 Lexer.Reset;
 Parser.Reset;
 //CurType   := ST_NONE;
 //ErrorInfo := STE_NONE;
 //isError   := FALSE;
end;

function stNextTerminal: Boolean;
begin
 Result := Parser.NextTerminal;
end;

function stGetErrorInfo: String;
begin
 //if not isError then
 //begin
 // Result := 'all right';
 // Exit;
 //end;
 //case ErrorInfo of
 // STE_NONE: Result := 'unknown error';
 //end;
end;

{ TControllerBlock }

function TControllerBlock.GetInfo: String;
begin

end;

procedure TControllerBlock.Msg(Message: stInfoType; SomeParam1,
  SomeParam2: String);
begin
 Info   := Message;
 Param1 := SomeParam1;
 Param2 := SomeParam2;
end;

procedure TControllerBlock.Msg(Message: stInfoType; SomeParam: String);
begin
 Info   := Message;
 Param1 := SomeParam;
 Param2 := '';
end;

procedure TControllerBlock.Msg(Message: stInfoType);
begin
 Info   := Message;
 Param1 := '';
 Param2 := '';
end;

{ TParserST }

function TParserST.GetCurType: stType;
begin
 Result := Lex^.CurType;
end;

procedure TParserST.Push(ValType: STterm);
begin
 Stack.Push(ValType);
end;

procedure TParserST.Push(const ValTypes: array of STterm);
var
 i: Integer;
begin
 For i := High(ValTypes) downto Low(ValTypes) do
  Stack.Push(ValTypes[i]);
end;

procedure TParserST.Pop;
begin
 CurTerm := Stack.Top;
 Stack.Pop;
end;

procedure TParserST.toSST;
begin
 case CurType of
  ST_ID: Push([STN_R, STT_OUT]);
  else Err := TRUE;
 end;
end;

procedure TParserST.toR;
begin
 case CurType of
  ST_ID: Push([STT_ID, STN_RR, STT_RECORD_END]);
  else Err := TRUE;
 end;
end;

procedure TParserST.toRR;
begin
 case CurType of
  ST_ID:     Push([STN_V, STN_RR]);
  ST_CODE:   Push([STN_V, STN_RR]);
  ST_STRING: Push([STN_V, STN_RR]);
  ST_RECORD_END: Exit;
  else Err := TRUE;
 end;
end;

procedure TParserST.toV;
begin
 case CurType of
  ST_ID:     Push(STN_R);
  ST_CODE:   Push(STT_CODE);
  ST_STRING: Push(STT_STRING);
  else Err := TRUE;
 end;
end;

constructor TParserST.Create;
begin
 Stack := stTermStack.Create;
end;

destructor TParserST.Destroy;
begin
 Clear;
 Stack.Destroy;
 Lex := nil;
end;

procedure TParserST.Clear;
begin
 while not Stack.IsEmpty do Stack.Pop;
end;

procedure TParserST.Bind(SomeLexer: PTLexerST);
begin
 Lex := SomeLexer;
end;

procedure TParserST.Reset;
begin
 Clear;
 Push(STN_SST);
end;

function TParserST.CheckType: Boolean;
begin
 Err := False;
 Result := True;
 repeat
  Pop;
  case CurTerm of
   STN_SST: toSST;
   STN_R:   toR;
   STN_RR:  toRR;
   STN_V:   toV;
   STT_STRING:;
   STT_CODE:;
   STT_ID:;
   STT_RECORD_END:;
   STT_OUT:;
  end;
 until True;
 Result := False;
end;

function TParserST.NextTerminal: Boolean;
begin
 Err := FALSE;
 Result := Lex^.NextTerminal;
 if not Result then Exit;
 if CurType = ST_COMMENT then Exit;
 repeat
  Pop;
  case CurTerm of
   STN_SST: toSST;
   STN_R:   toR;
   STN_RR:  toRR;
   STN_V:   toV;
   STT_STRING:     if CurType = ST_STRING     then Exit;
   STT_CODE:       if CurType = ST_CODE       then Exit;
   STT_ID:         if CurType = ST_ID         then Exit;
   STT_RECORD_END: if CurType = ST_RECORD_END then Exit;
   STT_OUT:        if not (CurType = ST_OUT)  then Err := TRUE;
  end;
 until Err;
 Result := FALSE;
end;

{ TLexerST }

function TLexerST.GetChar: Char;
begin
 Result := PText^[Pos];
end;

procedure TLexerST.DETECT_NONE;
begin
 case CL of
  STL_CODE_MARK:
  begin
   SetPar(ST_CODE);
   PosText1 := Pos + 1;
  end;
  STL_RECORD_BEGIN:
  begin
   SetPar(ST_ID);
   PosText1 := Pos + 1;
  end;
  STL_RECORD_END:
  begin
   SetPar(ST_RECORD_END);
   PosText1 := Pos;
   PosText2 := Pos;
   Exelent := TRUE;
  end;
  STL_QUOTATION_MARK:
  begin
   SetPar(ST_STRING);
   PosText1 := Pos + 1;
  end;
  STL_TAB:;
  else
   Err := TRUE;
 end;
end;

procedure TLexerST.DETECT_STRING;
begin
 if CL = STL_QUOTATION_MARK then
 begin
  Exelent  := TRUE;
  PosText2 := Pos - 1;
 end;
end;

procedure TLexerST.DETECT_CODE;
begin
 case stage of
  0: if CL in [STL_DIGIT, STL_LET16] then SetPar(1) else Err := TRUE;
  1: if CL in [STL_DIGIT, STL_LET16] then SetPar(2) else Err := TRUE;
  2:
  case CL of
   STL_DIGIT, STL_LET16: SetPar(1);
   STL_CODE_MARK:
   begin
    Exelent := TRUE;
    PosText2 := Pos - 1;
    Back;
   end;
   STL_QUOTATION_MARK, STL_RECORD_END, STL_TAB, STL_RECORD_BEGIN:
   begin
    Exelent := TRUE;
    PosText2 := Pos - 1;
    Back;
   end;
   else Err := TRUE;
  end;
  else Err := TRUE;
 end;
end;

procedure TLexerST.DETECT_ID;
begin
 case stage of
  0:
  begin
   if C = '*' then
   begin
    SetPar(ST_COMMENT);
    inc(PosText1);
    Exit;
   end;
   case CL of
    STL_ID_SYMBOLS: SetPar(1);
    STL_LET16, STL_LETTER: SetPar(2);
    else Err := TRUE;
   end;
  end;
  1: if CL in [STL_LET16, STL_LETTER] then SetPar(2)
                                      else Err := TRUE;
  2:
  case CL of
   STL_LETTER, STL_LET16, STL_DIGIT:;
   else
    begin
     Exelent := TRUE;
     PosText2 := Pos - 1;
    end;
  end;
  else Err := TRUE;
 end;
end;

procedure TLexerST.DETECT_COMMENT;
begin
 case stage of
  0: if C = '*' then SetPar(1);
  1: if C = '/' then
  begin
   PosText2 := Pos - 2;
   Exelent := TRUE;
  end else SetPar(1);
 end;
end;

procedure TLexerST.GetLexeme;
begin
 case C of
  '0'..'9': CurLexeme := STL_DIGIT;
  'A'..'F': CurLexeme := STL_LET16;
  'G'..'Z','a'..'z','_':
            CurLexeme := STL_LETTER;
  '!','@','#','$','%','^','&','*',':','?':
            CurLexeme := STL_ID_SYMBOLS;
  '\':      CurLexeme := STL_CODE_MARK;
  '/':      CurLexeme := STL_RECORD_BEGIN;
  ';':      CurLexeme := STL_RECORD_END;
  #39:      CurLexeme := STL_QUOTATION_MARK;
  #13,#10,#9,' ':
            CurLexeme := STL_TAB;
  else
            CurLexeme := STL_NONE;
 end;
end;

procedure TLexerST.Cycle;
begin
 Stage   := 0;
 CurType := ST_NONE;
 CurLexeme := STL_NONE;
 Err       := FALSE;
 Exelent   := FALSE;
end;

procedure TLexerST.Step;
begin
 inc(Pos);
 GetLexeme;
end;

procedure TLexerST.Back;
begin
 dec(Pos);
 GetLexeme;
end;

procedure TLexerST.SetPar(SomeType: stType);
begin
 CurType := SomeType;
end;

procedure TLexerST.SetPar(SomeLevel: Integer);
begin
 Stage := SomeLevel;
end;

procedure TLexerST.SetPar(SomeType: stType; SomeLevel: Integer);
begin
 SetPar(SomeType);
 SetPar(SomeLevel);
end;

constructor TLexerST.Create;
begin
 Clear;
end;

destructor TLexerST.Destroy;
begin
 Clear;
end;

procedure TLexerST.Clear;
begin
 Reset;
 PText := nil;
end;

procedure TLexerST.Reset;
begin
 Pos := 0;
 Cycle;
end;

procedure TLexerST.Bind(var SomeText: String; var Pos1, Pos2: PInteger;
  var SomeLexeme: stPType);
begin
 PText := @SomeText;
 Pos1  := @PosText1;
 Pos2  := @PosText2;
 SomeLexeme := @CurType;
 Reset;
end;

function TLexerST.NextTerminal: Boolean;
begin
 Result := FALSE;
 if Err then Exit;
 Cycle;
 repeat
  Step;
  if Pos > Length(PText^) then
  begin
   Result  := FALSE;
   CurType := ST_OUT;
   Exit;
  end;
  case curType of
   ST_NONE:    DETECT_NONE;
   ST_STRING:  DETECT_STRING;
   ST_CODE:    DETECT_CODE;
   ST_ID:      DETECT_ID;
   ST_COMMENT: DETECT_COMMENT;
   else Err := TRUE;
  end;
 until Err or Exelent;
 Result := TRUE;
end;

initialization
  Lexer := TLexerST.Create;
  Parser := TParserST.Create;
  Parser.Bind( @Lexer );

finalization
  Parser.Destroy;
  Lexer.Destroy;

end.
