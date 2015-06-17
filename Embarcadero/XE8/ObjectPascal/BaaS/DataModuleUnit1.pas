unit DataModuleUnit1;

interface

uses
  System.SysUtils, System.Classes, IPPeerClient, REST.Backend.ServiceTypes,
  REST.Backend.MetaTypes, System.JSON, REST.Backend.KinveyServices,
  REST.Backend.Providers, REST.Backend.ServiceComponents,
  REST.Backend.KinveyProvider, REST.Backend.ParseProvider, ToDoItemTypes,
  Data.Bind.ObjectScope, REST.OpenSSL;

type
  TDataModule1 = class(TDataModule)
    KinveyProvider1: TKinveyProvider;
    BackendStorage1: TBackendStorage;
  private
    FBackendList: TBackendObjectList<TToDo>;
    FToDoItemsAdapter: TListBindSourceAdapter<TToDo>;
    { Private declarations }
    function CreateToDoList(const AProviderID: string;
      const AStorage: TBackendStorageApi): TBackendObjectList<TToDo>;
    function GetToDoItemsAdapter: TBindSourceAdapter;
    procedure AfterPost(Sender: TBindSourceAdapter);
    procedure BeforeDelete(Sender: TBindSourceAdapter);
    procedure AddBackendItem(const AItem: TToDo);
    procedure UpdateBackendItem(const AItem: TToDo);
    procedure DeleteBackendItem(const AItem: TToDo);
  public
    { Public declarations }
    procedure RefreshAdapter;
    property ItemAdapter: TBindSourceAdapter read GetToDoItemsAdapter;
  end;

var
  DataModule1: TDataModule1;

implementation

uses System.Generics.Collections;

{$R *.dfm}

procedure TDataModule1.RefreshAdapter;
var
  LList: TList<TToDo>;
  LToDo: TToDo;
  LOldList: TBackendObjectList<TToDo>;
begin
  LOldList := FBackendList;
  try
    FBackendList := CreateToDoList(BackendStorage1.Provider.ProviderID, BackendStorage1.Storage);
    LList := TList<TToDo>.Create;
    try
      for LToDo in FBackendList do
        LList.Add(LToDo);
      if FToDoItemsAdapter = nil then
        FToDoItemsAdapter := TListBindSourceAdapter<TToDo>.Create(Self, LList, True);
      FToDoItemsAdapter.SetList(LList, True);
    except
      LList.Free;
      raise;
    end;
  finally
    LOldList.Free;
  end;
end;

function TDataModule1.CreateToDoList(const AProviderID: string; const AStorage: TBackendStorageApi): TBackendObjectList<TToDo>;
var
  LQuery: TArray<string>;
  LContactList: TBackendObjectList<TToDo>;
begin

  // Query for the objects, sort by name
  LContactList := TBackendObjectList<TToDo>.Create;
  try
    if SameText(AProviderID, TKinveyProvider.ProviderID) then
      LQuery := TArray<string>.Create(Format('sort=%s', [TToDoNames.TitleElement]))
    else if SameText(AProviderID, TParseProvider.ProviderID) then
      LQuery := TArray<string>.Create(Format('order=%s', [TToDoNames.TitleElement]))
    else
      raise Exception.Create('Unknown provider');

    AStorage.QueryObjects<TToDo>(TToDoNames.BackendClassname,
      LQuery, LContactList);
  except
    LContactList.Free;
    raise;
  end;
  Result := LContactList;
end;

function TDataModule1.GetToDoItemsAdapter: TBindSourceAdapter;
begin
  if FToDoItemsAdapter = nil then
  begin
     // Create empty adapter
     FToDoItemsAdapter := TListBindSourceAdapter<TToDo>.Create(Self, TList<TToDo>.Create, True);
     FToDoItemsAdapter.AfterPost := AfterPost;
     FToDoItemsAdapter.BeforeDelete := BeforeDelete;
  end;
  Result := FToDoItemsAdapter;
end;

procedure TDataModule1.AddBackendItem(const AItem: TToDo);
var
  LEntity: TBackendEntityValue;
begin
  BackendStorage1.Storage.CreateObject<TToDo>(
    TToDoNames.BackendClassname, AItem, LEntity);
  FBackendList.Add(AItem, LEntity);
end;

procedure TDataModule1.UpdateBackendItem(const AItem: TToDo);
var
  LEntity: TBackendEntityValue;
  LUpdate: TBackendEntityValue;
begin
  LEntity := FBackendList.EntityValues[AItem];
  BackendStorage1.Storage.UpdateObject<TToDo>(
    LEntity, AItem, LUpdate);
end;

procedure TDataModule1.DeleteBackendItem(const AItem: TToDo);
var
  LEntity: TBackendEntityValue;
begin
  LEntity := FBackendList.EntityValues[AItem];
  BackendStorage1.Storage.DeleteObject(
    LEntity);
end;

procedure TDataModule1.BeforeDelete(Sender: TBindSourceAdapter);
var
  LToDo: TToDo;
  I: Integer;
begin
  LToDo := FToDoItemsAdapter.List[FToDoItemsAdapter.ItemIndex];
  I := FBackendList.IndexOf(LToDo);
  if I >= 0 then
    DeleteBackendItem(LToDo);
end;

procedure TDataModule1.AfterPost(Sender: TBindSourceAdapter);
var
  LToDo: TToDo;
  I: Integer;
begin
  try
    LToDo := FToDoItemsAdapter.List[FToDoItemsAdapter.ItemIndex];
    I := FBackendList.IndexOf(LToDo);
    if I >= 0 then
      UpdateBackendItem(LToDo)
    else
      AddBackendItem(LToDo);
  except
    FToDoItemsAdapter.Edit;  // Return to edit mode
    raise;
  end;
end;


end.

