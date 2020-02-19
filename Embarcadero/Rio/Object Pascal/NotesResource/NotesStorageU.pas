//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit NotesStorageU;

interface

uses System.Classes, System.SysUtils,  System.IniFiles, System.Generics.Collections,
  NoteTypesU;

type

  TNotesStorage = class
  private
    FIniFile: TIniFile;
    FUserID: string;
    function DecodeMultilineText(const AText: string): string;
    function EncodeMultilineText(const AText: string): string;
    procedure ReadNote(const AID: string; out ANote: TNote);
    procedure ReadIDs(const AList: TList<string>); overload;
    function ReadIDs: TArray<string>; overload;
  public
    constructor Create(const ADirectory: string; const AUserID: string);
    destructor Destroy; override;
    function GetNotes: TArray<TNote>;
    function GetNote(const AID: string; out ANote: TNote): Boolean;
    procedure UpdateNote(const AID: string; const ANote: TNote);
    procedure AddNote(const ANote: TNote; out AID: string);
    function DeleteNote(const AID: string): Boolean;
    function NoteTitleExists(const ATitle: string): Boolean;
    function FindNote(const ATitle: string; out ANote: TNote): Boolean;
    function NoteIDExists(const AID: string): Boolean;
  end;

  ENoteError = class(Exception);
  ENoteNotFound = class(ENoteError);
  ENoteDuplicate = class(ENoteError);
  ENoteMissingTitle = class(ENoteError);

implementation

{ TNotesManager }

procedure TNotesStorage.AddNote(const ANote: TNote; out AID: string);

  function NextID: string;
  var
    LList: TList<string>;
    I: Integer;
  begin
    LList := TList<string>.Create;
    try
      ReadIDs(LList);
      I := LList.Count;
      while LList.Contains(IntToStr(I)) do
        Inc(I);
      Result := IntToStr(I);
    finally
      LList.Free;
    end;
  end;

begin
  if ANote.Title = '' then
    raise ENoteMissingTitle.Create('Note title required');
  if NoteTitleExists(ANote.Title) then
    raise ENoteDuplicate.CreateFmt('"%s" already exists', [ANote.Title]);
  AID := NextID;
  FIniFile.WriteString(FUserID, AID + '.title', ANote.Title);
  FIniFile.WriteString(FUserID, AID + '.text', EncodeMultilineText( ANote.Content));
end;

constructor TNotesStorage.Create(const ADirectory: string; const AUserID: string);
var
  LPath: string;
begin
  FUserID := AUserID;
  LPath := IncludeTrailingPathDelimiter(ExpandFileName(ADirectory)) + 'notes.ini';
  FIniFile := TIniFile.Create(LPath);
end;

function TNotesStorage.NoteIDExists(const AID: string): Boolean;
begin
  Result := FIniFile.ValueExists(FUserID, AID + '.title');
end;

function TNotesStorage.NoteTitleExists(const ATitle: string): Boolean;
var
  LNote: TNote;
begin
  Result := False;
  for LNote in GetNotes do
    if LNote.Title = ATitle then
    begin
      Result := True;
      break;
    end;
end;

function TNotesStorage.FindNote(const ATitle: string; out ANote: TNote): Boolean;
var
  LNote: TNote;
begin
  Result := False;
  for LNote in GetNotes do
    if LNote.Title = ATitle then
    begin
      Result := True;
      ReadNote(LNote.ID, ANote);
      break;
    end;
end;

function TNotesStorage.DeleteNote(const AID: string): Boolean;
begin
  Result := NoteIDExists(AID);
  if Result then
    FIniFile.DeleteKey(FUserID, AID + '.title');
end;

destructor TNotesStorage.Destroy;
begin
  FIniFile.Free;
  inherited;
end;

function TNotesStorage.EncodeMultilineText(const AText: string): string;
var
  LBuilder: TStringBuilder;
  S: Char;
begin
  LBuilder := TStringBuilder.Create;
  try
    for S in AText do
    begin
      case S of
      #10:
        LBuilder.Append('\n');
      #13:
        LBuilder.Append('\r');
      '\':
        LBuilder.Append('\\');
      else
        LBuilder.Append(S);
      end;
    end;
    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function TNotesStorage.DecodeMultilineText(const AText: string): string;
var
  LBuilder: TStringBuilder;
  I: Integer;
  S: Char;
begin
  LBuilder := TStringBuilder.Create;
  try
    I := 0;
    while I < AText.Length do
    begin
      S := AText.Chars[I];
      if (S = '\') and (I+1 < AText.Length) then
        case AText.Chars[I+1] of
          'n':
            begin
              Inc(I);
              S := #10;
            end;
          'r':
            begin
              Inc(I);
              S := #13;
            end;
          '\':
            begin
              Inc(I);
              S := '\';
            end
        end;
      LBuilder.Append(S);
      Inc(I);
    end;
    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function TNotesStorage.GetNote(const AID: string; out ANote: TNote): Boolean;
begin
  Result := NoteIDExists(AID);
  if Result then
    ReadNote(AID, ANote);
end;

procedure TNotesStorage.ReadNote(const AID: string; out ANote: TNote);
var
  LText: string;
  LTitle: string;
begin
  LTitle := FIniFile.ReadString(FUserID, AID + '.title', '');
  LText := DecodeMultilineText(FIniFile.ReadString(FUserID, AID + '.text', ''));
  ANote := TNote.CreateWithID(LTitle, LText, AID);
end;

function TNotesStorage.GetNotes: TArray<TNote>;
var
  LList: TList<TNote>;
  LNote: TNote;
  LSection: TStrings;
  LID: string;
begin
  LSection := nil;
  LList := nil;
  try
    LSection := TStringList.Create;
    LList := TList<TNote>.Create;
    for LID in ReadIDs do
    begin
      ReadNote(LID, LNote);
      LList.Add(LNote);
    end;
    Result := LList.ToArray;
  finally
    LList.Free;
    LSection.Free;
  end;
end;


function TNotesStorage.ReadIDs: TArray<string>;
var
  LList: TList<string>;
begin
  LList := TList<string>.Create;
  try
    ReadIDs(LList);
    Result := LList.ToArray;
  finally
    LList.Free;
  end;
end;

procedure TNotesStorage.ReadIDs(const AList: TList<string>);
var
  LSection: TStrings;
  I: Integer;
  LID: string;
  LKey: string;
begin
  LSection := nil;
  try
    LSection := TStringList.Create;
    FIniFile.ReadSection(FUserID, LSection);
    for I := 0 to LSection.Count - 1 do
    begin
      LKey := LSection[I];
      if LKey.EndsWith('.title') then
      begin
        LID := LKey.Substring(0, Length(LKey) - Length('.title'));
        Assert(not AList.Contains(LID));
        Assert(LID <> '');
        AList.Add(LID);
      end;
    end;
  finally
    LSection.Free;
  end;
end;

procedure TNotesStorage.UpdateNote(const AID: string; const ANote: TNote);
begin
  if not NoteIDExists(AID) then
    raise ENoteNotFound.CreateFmt('"%s" not found', [AID]);
  FIniFile.WriteString(FUserID, AID + '.title', EncodeMultilineText(ANote.Title));
  FIniFile.WriteString(FUserID, AID + '.text', EncodeMultilineText(ANote.Content));
end;


end.
