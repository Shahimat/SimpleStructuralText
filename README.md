# Simple structural text file format

## A few words about the format

Simple structural text file format (*.sst) was created as a tool for storing simple text structures for use in applications as analyzers. In the format, the syntax part is simplified as much as possible and the possibility of text compression is laid.

## EBNF notation

```ebnf
(* Lexical part *)
(* char -> all symbols without "'" *)
digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9';
let16 = 'A' | 'B' | 'C' | 'D' | 'E' | 'F';
code_mark      = '\';
record_begin   = '/';
record_end     = ';';
quotation_mark = "'";
id     = char, {char};
byte   = digit | let16, digit | let16;
code   = code_mark, byte, {byte}, code_mark;
string = quotation_mark, {char}, quotation_mark;

(*comment's part*)
comments = '/', '*', {char | quotation_mark}, '*', '/';

(* Syntax part *)
sst    = record;
record = record_begin, id, {value}, record_end;
value  = code | string | record;
```

## Example

This text format is based on two equivalent representations.

> First full version:

```
/lexer
    /*some define's*/
    /define
        /plus '+';
        /minus '-';
        /point '.';
        /zero '0';
        /digit19 '1' '2' '3' '4' '5' '6' '7' '8' '9';
        /value_separator ',';
        /name_separator ':';
        /quotation_mark '"';
        /begin_object '{';
        /end_object '}';
        /begin_array '[';
        /end_array ']';
        /letter 
            'A' 'B' 'C' 'D' 'E' 'F' 'G'
            'H' 'I' 'J' 'K' 'L' 'M' 'N'
            'O' 'P' 'Q' 'R' 'S' 'T' 'U'
            'V' 'W' 'X' 'Y' 'Z' 
            'a' 'b' 'c' 'd' 'e' 'f' 'g' 
            'h' 'i' 'j' 'k' 'l' 'm' 'n' 
            'o' 'p' 'q' 'r' 's' 't' 'u' 
            'v' 'w' 'x' 'y' 'z'
        ;
        /tab \09\ \0D\ \0A\ ' ';
        /char /all; /without; '"';
    ;
    /*some law's*/
    /laws
        /string
            /quotation_mark; '*' /char; /quotation_mark;
        ;
        /true 'true';
        /false 'false';
        /null 'null';
        /some_code \AA042F\;
    ;
;
```

> Second compressed version:

```

/lexer/define/plus'+';/minus'-';/point'.';/zero'0';/digit19'1''2''3''4''5''6''7''8''9';/value_separator',';/name_separator':';
/quotation_mark'"';/begin_object'{';/end_object'}';/begin_array'[';/end_array']';/letter'A''B''C''D''E''F''G''H''I''J''K''L''M
''N''O''P''Q''R''S''T''U''V''W''X''Y''Z''a''b''c''d''e''f''g''h''i''j''k''l''m''n''o''p''q''r''s''t''u''v''w''x''y''z';/tab\09
\\0D\\0A\'';/char/all;/without;'"';;/laws/string/quotation_mark;'*'/char;/quotation_mark;;/true'true';/false'false';/null'null
';/some_code\AA042F\;;;

```

## The MIT License

Text markup file format with MIT License (read `LICENSE` file).