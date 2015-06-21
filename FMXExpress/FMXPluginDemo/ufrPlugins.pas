unit ufrPlugins;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListView.Types, FMX.Menus, FMX.StdActns, FMX.MediaLibrary.Actions,
  System.Actions, FMX.ActnList, FMX.ListView,
  uPlugin;

type
  TfrPlugins = class(TFrame)
    lvPlugins: TListView;
    actlst: TActionList;
    actLoadPlugins: TAction;
    actEditCurrent: TAction;
    actAddPlugin: TAction;
    actSavePlugins: TAction;
    TakePhotoFromLibraryAction: TTakePhotoFromLibraryAction;
    pmEdit: TPopupMenu;
    menuAdd: TMenuItem;
    menuEdit: TMenuItem;
    menuLoad: TMenuItem;
    menuSave: TMenuItem;
    dlgOpen: TOpenDialog;
    procedure actAddPluginExecute(Sender: TObject);
    procedure actEditCurrentExecute(Sender: TObject);
    procedure actLoadPluginsExecute(Sender: TObject);
    procedure actSavePluginsExecute(Sender: TObject);
    procedure lvPluginsChange(Sender: TObject);
    procedure pmEditPopup(Sender: TObject);
  private
    { Private declarations }
    procedure LaodListView;
    procedure ChangeCurrent(index : Integer);
    function GetCurrentPlugin: TPlugin;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CurrentPlugin : TPlugin read GetCurrentPlugin;
  end;

implementation
uses
    uUtils, OpenViewUrl,
    uPluginEdit;
const
    filePlugins : string = 'filePlugins.plg';
var
    FPlugins : TPlugins;
{$R *.fmx}

{ TFrame1 }
{------------------------------------------------------------------------------}
procedure TfrPlugins.actAddPluginExecute(Sender: TObject);
var
    fPluginEdit : TfPluginEdit;
begin
    fPluginEdit := TfPluginEdit.Create(self);
    try
        if fPluginEdit.ShowModal = mrOk then
        begin
            FPlugins.Add(fPluginEdit.FPlugin);
            LaodListView;
        end;
    finally
        FreeAndNil(fPluginEdit);
    end;
end;
{------------------------------------------------------------------------------}
procedure TfrPlugins.actEditCurrentExecute(Sender: TObject);
var
    fPluginEdit : TfPluginEdit;
begin
    fPluginEdit := TfPluginEdit.Create(self);
    try
        fPluginEdit.LoadPLugin(FPlugins.Current);
        if fPluginEdit.ShowModal = mrOk then
            LaodListView;
    finally
        FreeAndNil(fPluginEdit);
    end;
end;
{------------------------------------------------------------------------------}
procedure TfrPlugins.actLoadPluginsExecute(Sender: TObject);
begin
    if dlgOpen.Execute then
        FPlugins.FileName := dlgOpen.FileName;
end;
{------------------------------------------------------------------------------}
procedure TfrPlugins.actSavePluginsExecute(Sender: TObject);
begin
    FPlugins.SaveToFile;
end;
{------------------------------------------------------------------------------}
procedure TfrPlugins.ChangeCurrent(index: Integer);
begin
    lvPlugins.ItemIndex := index;
end;
{------------------------------------------------------------------------------}
constructor TfrPlugins.Create(AOwner: TComponent);
begin
  inherited;
    FPlugins := TPlugins.Create(uUtils.GetPath + filePlugins);
    FPlugins.OnChangeCurrent := ChangeCurrent;
    LaodListView;
end;
{------------------------------------------------------------------------------}
destructor TfrPlugins.Destroy;
begin
    FPlugins.SaveToFile;
    FreeAndNil(FPlugins);
  inherited;
end;
{------------------------------------------------------------------------------}
function TfrPlugins.GetCurrentPlugin: TPlugin;
begin
    Result := FPlugins.Current;
end;
{------------------------------------------------------------------------------}
procedure TfrPlugins.LaodListView;
var
    plugin : TPlugin;
    item : TListViewItem;
begin
    lvPlugins.BeginUpdate;
    lvPlugins.ClearItems;
    for plugin in FPlugins do
    begin
        item := lvPlugins.Items.Add;
        item.Text := plugin.FName;
        item.Bitmap := plugin.FIcon;
    end;
    lvPlugins.EndUpdate;
    if not FPlugins.IsEmpty then
        FPlugins.ItemIndex := 0;
end;
{------------------------------------------------------------------------------}
procedure TfrPlugins.lvPluginsChange(Sender: TObject);
begin
    FPlugins.ItemIndex := lvPlugins.ItemIndex;
end;
{------------------------------------------------------------------------------}
procedure TfrPlugins.pmEditPopup(Sender: TObject);
begin
    menuEdit.Visible := FPlugins.ItemIndex > -1;
end;
{------------------------------------------------------------------------------}
end.
