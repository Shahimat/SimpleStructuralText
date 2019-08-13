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

//  Text :=  '/lexer'
//+ #13#10 + '    /*some define'+#39+'s*/'
//+ #13#10 + '    /define'
//+ #13#10 + '        /plus '+#39+'+'+#39+';'
//+ #13#10 + '        /minus '+#39+'-'+#39+';'
//+ #13#10 + '        /point '+#39+'.'+#39+';'
//+ #13#10 + '        /zero '+#39+'0'+#39+';'
//+ #13#10 + '        /digit19 '+#39+'1'+#39+' '+#39+'2'+#39+' '+#39+'3'+#39+' '+#39+'4'+#39+' '+#39+'5'+#39+' '+#39+'6'+#39+' '+#39+'7'+#39+' '+#39+'8'+#39+' '+#39+'9'+#39+';'
//+ #13#10 + '        /value_separator '+#39+','+#39+';'
//+ #13#10 + '        /name_separator '+#39+':'+#39+';'
//+ #13#10 + '        /quotation_mark '+#39+'"'+#39+';'
//+ #13#10 + '        /begin_object '+#39+'{'+#39+';'
//+ #13#10 + '        /end_object '+#39+'}'+#39+';'
//+ #13#10 + '        /begin_array '+#39+'['+#39+';'
//+ #13#10 + '        /end_array '+#39+']'+#39+';'
//+ #13#10 + '        /letter'
//+ #13#10 + '            '+#39+'A'+#39+' '+#39+'B'+#39+' '+#39+'C'+#39+' '+#39+'D'+#39+' '+#39+'E'+#39+' '+#39+'F'+#39+' '+#39+'G'+#39+''
//+ #13#10 + '            '+#39+'H'+#39+' '+#39+'I'+#39+' '+#39+'J'+#39+' '+#39+'K'+#39+' '+#39+'L'+#39+' '+#39+'M'+#39+' '+#39+'N'+#39+''
//+ #13#10 + '            '+#39+'O'+#39+' '+#39+'P'+#39+' '+#39+'Q'+#39+' '+#39+'R'+#39+' '+#39+'S'+#39+' '+#39+'T'+#39+' '+#39+'U'+#39+''
//+ #13#10 + '            '+#39+'V'+#39+' '+#39+'W'+#39+' '+#39+'X'+#39+' '+#39+'Y'+#39+' '+#39+'Z'+#39+''
//+ #13#10 + '            '+#39+'a'+#39+' '+#39+'b'+#39+' '+#39+'c'+#39+' '+#39+'d'+#39+' '+#39+'e'+#39+' '+#39+'f'+#39+' '+#39+'g'+#39+''
//+ #13#10 + '            '+#39+'h'+#39+' '+#39+'i'+#39+' '+#39+'j'+#39+' '+#39+'k'+#39+' '+#39+'l'+#39+' '+#39+'m'+#39+' '+#39+'n'+#39+''
//+ #13#10 + '            '+#39+'o'+#39+' '+#39+'p'+#39+' '+#39+'q'+#39+' '+#39+'r'+#39+' '+#39+'s'+#39+' '+#39+'t'+#39+' '+#39+'u'+#39+''
//+ #13#10 + '            '+#39+'v'+#39+' '+#39+'w'+#39+' '+#39+'x'+#39+' '+#39+'y'+#39+' '+#39+'z'+#39+''
//+ #13#10 + '        ;'
//+ #13#10 + '        /tab \09\0D\0A '+#39+' '+#39+';'
//+ #13#10 + '        /char /all; /without; '+#39+'"'+#39+';'
//+ #13#10 + '    ;'
//+ #13#10 + '    /*some law'+#39+'s*/'
//+ #13#10 + '    /laws'
//+ #13#10 + '        /string'
//+ #13#10 + '            /quotation_mark; '+#39+'*'+#39+' /char; /quotation_mark;'
//+ #13#10 + '        ;'
//+ #13#10 + '        /true '+#39+'true'+#39+';'
//+ #13#10 + '        /false '+#39+'false'+#39+';'
//+ #13#10 + '        /null '+#39+'null'+#39+';'
//+ #13#10 + '        /some_code \AA042F;'
//+ #13#10 + '    ;'
//+ #13#10 + ';';
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

