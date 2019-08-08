(*===========================================================================*)
(*                                                                           *)
(*                         The MIT License (MIT)                             *)
(*                                                                           *)
(*       Copyright (c) 2019 Nakhapetyan Gevorg <ngs22071993@gmail.com>       *)
(*             ObjectPascal base structure of completed parser               *)
(*                                                                           *)
(*===========================================================================*)
unit PascalParser;

interface

uses GStack;

type

 generic TParser<T>=class
  private
   type TResStack = specialize TStack<T>;
   var FStack: TResStack;
  public
   //procedure Push(x:T);inline;
   //procedure Pop();inline;
   //function Top():T;inline;
   //function Size():longint;inline;
   //function IsEmpty():boolean;inline;
   //constructor Create;
   //destructor Destroy;override;
  end;

implementation

end.
