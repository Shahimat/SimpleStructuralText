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
stType = (ST_NONE, ST_ID, ST_CODE, ST_STRING, ST_RECORD_END, ST_COMMENT, ST_OUT);
stPType = ^stType;

Procedure stBind(var ValPos1, ValPos2: PInteger; var ValType: stPType;
  var ValFinished, ValError: PBoolean); Overload;
Procedure stBind(ValStructText: PString); Overload;
Procedure stReset;
Procedure stNextTerminalCheck;
Procedure stNextTerminal;
Function  stGetInfo: String;


implementation

uses GStack;

type

 STlexeme = (
   STL_NONE, STL_DIGIT, STL_LET16, STL_LETTER, STL_ID_SYMBOLS, STL_CODE_MARK,
   STL_RECORD_BEGIN, STL_RECORD_END, STL_QUOTATION_MARK, STL_TAB
 );

 STterm = (
   (*Terminals*)
   STT_NONE, STT_STRING, STT_CODE, STT_ID, STT_RECORD_END, STT_OUT,
   (*Nonterminals (laws)*)
   STN_SST, STN_R, STN_RR, STN_V
 );

 stPInfoType = ^stInfoType;
 stInfoType  = (STI_RIGHT, STI_ERROR, STI_ERROR_EXPECTED, STI_ERROR_UNTYPE,
   STI_ERROR_STRING_OUT, STI_ERROR_INCOMPLETE);

(* TControllerBlock *)

 PControllerBlock = ^TControllerBlock;
 TControllerBlock = class
  private
   isError: Boolean;
   Info: stInfoType;
   Param1, Param2: String;
   Function ErrorMessages: Boolean;
  public
   Property  Err: Boolean read isError;
   Function  GetInfo: String;
   Procedure Reset;
   Procedure Msg(Message: stInfoType; SomeParam: String); Overload;
   Procedure Msg(Message: stInfoType); Overload;
   Procedure Bind(var SomeErr: PBoolean); Overload;
   Procedure Bind(var SomeInfo: stPInfoType); Overload;
   Procedure Error;
 end;

 (* TLexerST *)

 PTLexerST = ^TLexerST;
 TLexerST = class
  private
   Control: PControllerBlock;
   Stage: Integer;
   CurType: stType;
   CurLexeme: STlexeme;
   Pos, PosText1, PosText2: Integer;
   PText: PString;
   Exelent, isFinished: Boolean;
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
   property  Finished: Boolean read isFinished;
   Procedure Bind(SomeText: PString); Overload;
   Procedure Bind(var SomeFinished: PBoolean); Overload;
   Procedure Bind(var Pos1, Pos2: PInteger; var SomeLexeme: stPType); Overload;
   Procedure BindController(SomeControl: PControllerBlock);
   Procedure NextTerminal;
 end;

 (* TParserST *)

 TParserST = class
  private
   Control: PControllerBlock;
   Lex: PTLexerST;
   CurTerm: STterm;
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
   Procedure BindLexer(SomeLexer: PTLexerST);
   Procedure BindController(SomeControl: PControllerBlock);
   Procedure Reset;
   Procedure NextTerminal;
 end;

var
  Lexer:      TLexerST;
  Parser:     TParserST;
  Controller: TControllerBlock;

procedure stBind(var ValPos1, ValPos2: PInteger; var ValType: stPType;
  var ValFinished, ValError: PBoolean);
begin
 Lexer.Bind(ValPos1, ValPos2, ValType);
 Lexer.Bind(ValFinished);
 Controller.Bind(ValError);
end;

procedure stBind(ValStructText: PString);
begin
 Lexer.Bind( ValStructText );
end;

procedure stReset;
begin
 Lexer.Reset;
 Parser.Reset;
 Controller.Reset;
end;

procedure stNextTerminalCheck;
begin
 Parser.NextTerminal;
end;

procedure stNextTerminal;
begin
 Lexer.NextTerminal;
end;

function stGetInfo: String;
begin
 Result := Controller.GetInfo;
end;

(* TControllerBlock *)

function TControllerBlock.ErrorMessages: Boolean;
begin
 case Info of
  STI_ERROR,
  STI_ERROR_EXPECTED,
  STI_ERROR_UNTYPE,
  STI_ERROR_STRING_OUT,
  STI_ERROR_INCOMPLETE:
        Result := TRUE;
  else  Result := FALSE;
 end;
end;

function TControllerBlock.GetInfo: String;
begin
 case Info of
  STI_RIGHT:            Result := 'All right';
  STI_ERROR:            Result := 'Error: Unknown error';
  STI_ERROR_EXPECTED:   Result := 'Error: Expected ' + Param1;
  STI_ERROR_UNTYPE:     Result := 'Error: Unknown type';
  STI_ERROR_STRING_OUT: Result := 'Error: Out of string';
  STI_ERROR_INCOMPLETE: Result := 'Error: Out of text';
  else                  Result := 'Error: Unknown information';
 end;
end;

procedure TControllerBlock.Reset;
begin
 Info    := STI_RIGHT;
 isError := FALSE;
 Param1  := '';
 Param2  := '';
end;

procedure TControllerBlock.Msg(Message: stInfoType; SomeParam: String);
begin
 Info    := Message;
 isError := ErrorMessages;
 Param1  := SomeParam;
 Param2  := '';
end;

procedure TControllerBlock.Msg(Message: stInfoType);
begin
 Info    := Message;
 isError := ErrorMessages;
 Param1  := '';
 Param2  := '';
end;

procedure TControllerBlock.Bind(var SomeErr: PBoolean);
begin
 SomeErr := @isError;
end;

procedure TControllerBlock.Bind(var SomeInfo: stPInfoType);
begin
 SomeInfo := @Info;
end;

procedure TControllerBlock.Error;
begin
 isError := TRUE;
 Info    := STI_ERROR;
end;

(* TLexerST *)

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
   Control^.Error;
 end;
end;

procedure TLexerST.DETECT_STRING;
begin
 if CL = STL_QUOTATION_MARK then
 begin
  Exelent  := TRUE;
  PosText2 := Pos - 1;
  Exit;
 end;
 if C = #13 then Control^.Msg(STI_ERROR_STRING_OUT);
end;

procedure TLexerST.DETECT_CODE;
begin
 case stage of
  0: if CL in [STL_DIGIT, STL_LET16] then SetPar(1) else
    Control^.Msg(STI_ERROR_EXPECTED, 'hexadecimal number');
  1: if CL in [STL_DIGIT, STL_LET16] then SetPar(2) else
    Control^.Msg(STI_ERROR_EXPECTED, 'hexadecimal number');
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
   else Control^.Error;
  end;
  else Control^.Error;
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
    else Control^.Error;
   end;
  end;
  1: if CL in [STL_LET16, STL_LETTER] then SetPar(2) else
    Control^.Msg(STI_ERROR_EXPECTED, 'letter');
  2:
  case CL of
   STL_LETTER, STL_LET16, STL_DIGIT:;
   else
    begin
     Exelent := TRUE;
     PosText2 := Pos - 1;
     Back;
    end;
  end;
  else Control^.Error;
 end;
end;

procedure TLexerST.DETECT_COMMENT;
begin
 case stage of
  0: if C = '*' then SetPar(1);
  1: if C = '/' then
  begin
   PosText2 := Pos - 2;
   Exelent  := TRUE;
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
 Control := nil;
end;

procedure TLexerST.Clear;
begin
 Reset;
 PText := nil;
end;

procedure TLexerST.Reset;
begin
 Pos := 0;
 isFinished := FALSE;
 Cycle;
end;

procedure TLexerST.Bind(SomeText: PString);
begin
 PText := SomeText;
end;

procedure TLexerST.Bind(var SomeFinished: PBoolean);
begin
 SomeFinished := @isFinished;
end;

procedure TLexerST.Bind(var Pos1, Pos2: PInteger; var SomeLexeme: stPType);
begin
 Pos1  := @PosText1;
 Pos2  := @PosText2;
 SomeLexeme := @CurType;
 Reset;
end;

procedure TLexerST.BindController(SomeControl: PControllerBlock);
begin
 Control := SomeControl;
end;

procedure TLexerST.NextTerminal;
begin
 Cycle;
 repeat
  Step;
  case curType of
   ST_NONE:    DETECT_NONE;
   ST_STRING:  DETECT_STRING;
   ST_CODE:    DETECT_CODE;
   ST_ID:      DETECT_ID;
   ST_COMMENT: DETECT_COMMENT;
   else Control^.Msg(STI_ERROR_UNTYPE);
  end;
  if Pos = Length(PText^) then
  begin
   isFinished := TRUE;
   if not (curType in [ST_RECORD_END,ST_NONE]) then
     Control^.Msg(STI_ERROR_INCOMPLETE);
   Exit;
  end;
 until Control^.Err or Exelent;

end;

(* TParserST *)

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
 if Stack.IsEmpty then CurTerm := STT_NONE else
 begin
  CurTerm := Stack.Top;
  Stack.Pop;
 end;
end;

procedure TParserST.toSST;
begin
 case CurType of
  ST_ID: Push([STN_R, STT_OUT]);
  else Control^.Msg(STI_ERROR_EXPECTED, 'ID');
 end;
end;

procedure TParserST.toR;
begin
 case CurType of
  ST_ID: Push([STT_ID, STN_RR, STT_RECORD_END]);
  else Control^.Msg(STI_ERROR_EXPECTED, 'ID');
 end;
end;

procedure TParserST.toRR;
begin
 case CurType of
  ST_ID:     Push([STN_V, STN_RR]);
  ST_CODE:   Push([STN_V, STN_RR]);
  ST_STRING: Push([STN_V, STN_RR]);
  ST_RECORD_END: Exit;
  else Control^.Msg(STI_ERROR_EXPECTED, 'ID | CODE | STRING | ;');
 end;
end;

procedure TParserST.toV;
begin
 case CurType of
  ST_ID:     Push(STN_R);
  ST_CODE:   Push(STT_CODE);
  ST_STRING: Push(STT_STRING);
  else Control^.Msg(STI_ERROR_EXPECTED, 'ID | CODE | STRING');
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
 Control := nil;
end;

procedure TParserST.Clear;
begin
 while not Stack.IsEmpty do Stack.Pop;
end;

procedure TParserST.BindLexer(SomeLexer: PTLexerST);
begin
 Lex := SomeLexer;
end;

procedure TParserST.BindController(SomeControl: PControllerBlock);
begin
 Control := SomeControl;
end;

procedure TParserST.Reset;
begin
 Clear;
 Push(STN_SST);
end;

procedure TParserST.NextTerminal;
begin
 Lex^.NextTerminal;
 if (Control^.Err) or (CurType = ST_COMMENT) then Exit;
 repeat
  Pop;
  case CurTerm of
   STN_SST: toSST;
   STN_R:   toR;
   STN_RR:  toRR;
   STN_V:   toV;
   STT_STRING:
     if CurType = ST_STRING then Break
                            else Control^.Msg(STI_ERROR_EXPECTED, 'STRING');
   STT_CODE:
     if CurType = ST_CODE then Break
                          else Control^.Msg(STI_ERROR_EXPECTED, 'CODE');
   STT_ID:
     if CurType = ST_ID then Break
                        else Control^.Msg(STI_ERROR_EXPECTED, 'ID');
   STT_RECORD_END:
     if CurType = ST_RECORD_END then Break
                                else Control^.Msg(STI_ERROR_EXPECTED, ';');
   STT_OUT:
     if CurType = ST_RECORD_END then Control^.Msg(STI_ERROR_EXPECTED, ';')
                                else Control^.Error;
   else Control^.Error;
  end;
 until Control^.Err;
 if Lex^.Finished then
 begin
  Pop;
  if not (CurTerm = STT_OUT) then
    Control^.Msg(STI_ERROR_EXPECTED, 'syntax completion');
 end;
end;

initialization
  Lexer := TLexerST.Create;
  Parser := TParserST.Create;
  Controller := TControllerBlock.Create;
  Parser.BindLexer( @Lexer );
  Parser.BindController( @Controller );
  Lexer.BindController( @Controller );

finalization
  Controller.Destroy;
  Parser.Destroy;
  Lexer.Destroy;

end.
