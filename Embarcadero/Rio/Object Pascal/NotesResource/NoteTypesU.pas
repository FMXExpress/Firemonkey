//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit NoteTypesU;

interface

uses System.JSON;

type
  TNote = record
  private
    FTitle: string;
    FContent: string;
    FID: string;
  public
    constructor Create(const ATitle, AContent: string);
    constructor CreateWithID(const ATitle, AContent: string; const AID: string);
    property Title: string read FTitle write FTitle;
    property Content: string read FContent write FContent;
    property ID: string read FID write FID;
  end;

  TNoteJSON = class
  public type
    TNames = record
    public const
      Title = 'title';
      Content = 'content';
      Id = 'id';
    end;
  public
    class function JSONToNote(const AJSON: TJSONValue): TNote; static;
    class function JSONToNotes(const AJSON: TJSONArray): TArray<TNote>; static;
    class procedure NotesToJSON(const ANotes: TArray<TNote>; const AJSON: TJSONArray); static;
    class function NoteToJSON(const ANote: TNote): TJSONObject; static;
  end;

implementation

uses System.Generics.Collections;

{ TNote }

constructor TNote.Create(const ATitle, AContent: string);
begin
  FContent := AContent;
  FTitle := ATitle;
end;

class function TNoteJSON.JSONToNote(const AJSON: TJSONValue): TNote;
begin
  Result := TNote.CreateWithID(
    AJSON.GetValue<string>(TNames.Title, ''),
    AJSON.GetValue<string>(TNames.Content, ''),
    AJSON.GetValue<string>(TNames.ID, ''));
end;

class function TNoteJSON.NoteToJSON(const ANote: TNote): TJSONObject; // Caller must free result
begin
  Result := TJSONObject.Create;
  Result.AddPair(TNames.Title, ANote.Title);
  Result.AddPair(TNames.Content, ANote.Content);
  Result.AddPair(TNames.ID, ANote.ID);
end;

class function TNoteJSON.JSONToNotes(const AJSON: TJSONArray): TArray<TNote>;
var
  LValue: TJSONValue;
  LList: TList<TNote>;
begin
  LList := TList<TNote>.Create;
  try
    for LValue in AJSON do
      LList.Add(TNoteJSON.JSONToNote(LValue));
    Result := LList.ToArray;
  finally
    LList.Free;
  end;
end;

class procedure TNoteJSON.NotesToJSON(const ANotes: TArray<TNote>;
  const AJSON: TJSONArray);
var
  LNote: TNote;
  LJSON: TJSONObject;
begin
  for LNote in ANotes do
  begin
    LJSON := NoteToJSON(LNote);
    AJSON.Add(LJSON);
  end;
end;

constructor TNote.CreateWithID(const ATitle, AContent, AID: string);
begin
  Create(ATitle, AContent);
  FID := AID;
end;

end.
