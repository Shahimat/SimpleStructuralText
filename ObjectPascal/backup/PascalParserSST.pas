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
  stType = (ST_NONE, ST_RECORD, ST_ID, ST_CODE, ST_STRING);
  stPType = ^stType;

Procedure stBind(ValStructText: PString; var ValPos1, ValPos2: PInteger;
  var ValType: stPType; var ValError: PBoolean);
Procedure stReset;
Function  stNextToken: Boolean;
Function  stGetErrorInfo: String;


implementation

uses GStack;

type

 stErrorInfo = (STE_NONE);

 STlexeme = (
   STL_NONE, STL_DIGIT, STL_LET16, STL_LETTER, STL_ID_SYMBOLS, STL_CODE_MARK,
   STL_RECORD_BEGIN, STL_RECORD_END, STL_QUOTATION_MARK, STL_TAB
 );

 STterm = (
    (*Terminals*)
   STT_NONE, STT_STRING, STT_CODE, STT_ID, STT_COMMENT, STT_RECORD_END,
   STT_EPS, STT_OUT,
   (*Nonterminals (laws)*)
   STN_SST, STN_R, STN_V
 );

 { TLexerST }

 TLexerST = class
  private
   Stage: Integer;
   CurType: STterm;
   CurLexeme: STlexeme;
   Pos, PosBack: Integer;
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
   Procedure SetPar(SomeType: STterm); Overload;
   Procedure SetPar(SomeLevel: Integer); Overload;
   Procedure SetPar(SomeType: STterm; SomeLevel: Integer); Overload;
   Property  C: Char read GetChar;
  public
   Constructor Create;
   Destructor  Destroy;
   Procedure Clear;
   Procedure Reset;
   Procedure Bind(SomeText: PString; var Pos1, Pos2: PInteger);
   Function  NextToken: Boolean;
 end;

 TParserST = class
  private
   type stTermStack = specialize TStack<STterm>;
   var  Stack: stTermStack;
  public

 end;

{$MACRO ON}
//{$DEFINE Text :=PText^}

var
  ErrorInfo: stErrorInfo;
  isError: Boolean;
  CurType: stType;
  Lexer: TLexerST;

procedure stBind(ValStructText: PString; var ValPos1, ValPos2: PInteger;
  var ValType: stPType; var ValError: PBoolean);
begin

 ValType  := @CurType;
 ValError := @isError;
end;

procedure stReset;
begin
 //Pos1 := 0;
 //Pos2 := 0;
 CurType   := ST_NONE;
 ErrorInfo := STE_NONE;
 isError   := False;
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

{ TLexerST }

function TLexerST.GetChar: Char;
begin
 Result := PText^[Pos];
end;

procedure TLexerST.DETECT_NONE;
begin
 case CurLexeme of
  STL_CODE_MARK:      SetPar(STT_CODE);
  STL_RECORD_BEGIN:   SetPar(STT_ID);
  STL_RECORD_END:     SetPar(STT_RECORD_END);
  STL_QUOTATION_MARK: SetPar(STT_STRING);
  STL_TAB:;
  else
   Err := TRUE;
 end;
end;

procedure TLexerST.DETECT_STRING;
begin

end;

procedure TLexerST.DETECT_CODE;
begin

end;

procedure TLexerST.DETECT_ID;
begin

end;

procedure TLexerST.DETECT_COMMENT;
begin

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
 Stage     := 0;
 CurType   := STT_NONE;
 CurLexeme := STL_NONE;
 Err       := False;
 Exelent   := False;
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

procedure TLexerST.SetPar(SomeType: STterm);
begin
 CurType := SomeType;
end;

procedure TLexerST.SetPar(SomeLevel: Integer);
begin
 Stage := SomeLevel;
end;

procedure TLexerST.SetPar(SomeType: STterm; SomeLevel: Integer);
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
 Pos     := 0;
 PosBack := 0;
 Cycle;
end;

procedure TLexerST.Bind(SomeText: PString; var Pos1, Pos2: PInteger);
begin
 PText := SomeText;
 Pos1  := @PosBack;
 Pos2  := @Pos;
 Reset;
end;

function TLexerST.NextToken: Boolean;
begin
 if Err then Exit;
 Cycle;
 repeat
  Step;
  case curType of
   STT_NONE:    DETECT_NONE;
   STT_STRING:  DETECT_STRING;
   STT_CODE:    DETECT_CODE;
   STT_ID:      DETECT_ID;
   STT_COMMENT: DETECT_COMMENT;
   STT_RECORD_END: Exelent := TRUE;
   STT_EPS: Err := TRUE;
   STT_OUT: Err := TRUE;
  end;
 until Err or Exelent;
 if Err then Result := False
        else Result := True;
end;

initialization
  Lexer := TLexerST.Create;

finalization
  Lexer.Destroy;

end.
