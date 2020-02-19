//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit NotesAdapterModuleU;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Classes, Data.Bind.ObjectScope,
  NoteTypesU;

type
  // Expose data binding properties
  TNoteWrapper = class
  private
    FNote: TNote;
    function GetContent: string;
    function GetID: string;
    function GetTitle: string;
    procedure SetContent(const Value: string);
    procedure SetTitle(const Value: string);
  public
    constructor Create(const ANote: TNote);
    function GetNote: TNote;
    procedure SetID(const AID: string);
    property Title: string read GetTitle write SetTitle;
    property Content: string read GetContent write SetContent;
    property ID: string read GetID;
  end;

  TNotesAdapterModule = class(TDataModule)
  private
    FBindSourceAdapter: TListBindSourceAdapter<TNoteWrapper>;
    procedure AfterPost(Sender: TBindSourceAdapter);
    procedure BeforeDelete(Sender: TBindSourceAdapter);
  public
    function GetBindSourceAdapter: TBindSourceAdapter;
    procedure UpdateAdapter(const ANotes: TArray<TNote>);
    { Private declarations }
  public
    { Public declarations }
    procedure RefreshAdapter; overload;
    property BindSourceAdapter: TBindSourceAdapter read GetBindSourceAdapter;
  end;

var
  NotesAdapterModule: TNotesAdapterModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses NotesClientModuleU;

{$R *.dfm}

procedure TNotesAdapterModule.RefreshAdapter;
var
  LNotes: TArray<TNote>;
begin
  LNotes := NotesClientModule.GetNotes;
  UpdateAdapter(LNotes);
end;

procedure TNotesAdapterModule.UpdateAdapter(const ANotes: TArray<TNote>);
var
  LList: TList<TNoteWrapper>;
  LNote: TNote;
begin
  if csDestroying in ComponentState then
    Exit;
  LList := TObjectList<TNoteWrapper>.Create;
  try
    for LNote in ANotes do
      LList.Add(TNoteWrapper.Create(LNote));
    if FBindSourceAdapter = nil then
      FBindSourceAdapter := TListBindSourceAdapter<TNoteWrapper>.Create(Self, LList, True)
    else
      FBindSourceAdapter.SetList(LList, True);
    FBindSourceAdapter.Active := True;
  except
    LList.Free;
    raise;
  end;
end;

function TNotesAdapterModule.GetBindSourceAdapter: TBindSourceAdapter;
begin
  if FBindSourceAdapter = nil then
  begin
     // Create empty adapter
     FBindSourceAdapter := TListBindSourceAdapter<TNoteWrapper>.Create(Self, TList<TNoteWrapper>.Create, True);
     FBindSourceAdapter.AfterPost := AfterPost;
     FBindSourceAdapter.BeforeDelete := BeforeDelete;
  end;
  Result := FBindSourceAdapter;
end;

procedure TNotesAdapterModule.BeforeDelete(Sender: TBindSourceAdapter);
var
  LWrapper: TNoteWrapper;
begin
  LWrapper := FBindSourceAdapter.List[FBindSourceAdapter.ItemIndex];
  if LWrapper.ID <> '' then
    NotesClientModule.DeleteNote(LWrapper.ID);
end;

procedure TNotesAdapterModule.AfterPost(Sender: TBindSourceAdapter);
var
  LWrapper: TNoteWrapper;
  LID: string;
begin
  try
    LWrapper := FBindSourceAdapter.List[FBindSourceAdapter.ItemIndex];
    if LWrapper.ID = '' then
    begin
      NotesClientModule.AddNote(LWrapper.GetNote, LID);
      LWrapper.SetID(LID);
    end
    else
      NotesClientModule.UpdateNote(LWrapper.GetNote);
  except
    FBindSourceAdapter.Edit;  // Return to edit mode
    raise;
  end;
end;

{ TRecipe }

constructor TNoteWrapper.Create(const ANote: TNote);
begin
  FNote := ANote;
end;

function TNoteWrapper.GetContent: string;
begin
  Result := FNote.Content;
end;

function TNoteWrapper.GetID: string;
begin
  Result := FNote.ID;
end;

function TNoteWrapper.GetNote: TNote;
begin
  Result := FNote;
end;

function TNoteWrapper.GetTitle: string;
begin
  Result := FNote.Title;

end;

procedure TNoteWrapper.SetContent(const Value: string);
begin
  FNote.Content := Value;
end;

procedure TNoteWrapper.SetID(const AID: string);
begin
  FNote.ID := AID;
end;

procedure TNoteWrapper.SetTitle(const Value: string);
begin
  FNote.Title := Value;
end;

end.
