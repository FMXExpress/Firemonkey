// Copyright (c) 2017 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
unit StoreItems;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Actions,
  FMX.ActnList, FMX.TabControl, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Edit, FMX.Layouts, FMX.ListBox, IPPeerClient, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.DApt, REST.Backend.EMSServices,
  FireDAC.Comp.Client, REST.Backend.EMSFireDAC, Data.DB, FireDAC.Comp.DataSet,
  REST.Backend.EMSProvider, ClientDataModule, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.DBScope, FMX.ListView, FMX.Ani, FMX.Objects;

type
  TFormMain = class(TForm)
    tcStore: TTabControl;
    tiSelectStore: TTabItem;
    tiStoreItems: TTabItem;
    cbStores: TComboBox;
    edtStorePassword: TEdit;
    btnNext: TButton;
    ActionList1: TActionList;
    actNextTabToLogin: TNextTabAction;
    lvItems: TListView;
    BindingsList1: TBindingsList;
    LinkListControlToField1: TLinkListControlToField;
    BindSourceDB1: TBindSourceDB;
    BindSourceDB2: TBindSourceDB;
    LinkListControlToField2: TLinkListControlToField;
    ToolBar1: TToolBar;
    Label1: TLabel;
    ToolBar2: TToolBar;
    Label2: TLabel;
    btnAddItem: TSpeedButton;
    tiStoreEmployeeLogin: TTabItem;
    Button1: TButton;
    edtEmployeeLogin: TEdit;
    ToolBar3: TToolBar;
    Label3: TLabel;
    btnEditList: TSpeedButton;
    edtEmployeePassword: TEdit;
    actNextTabToItems: TNextTabAction;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    actBackTab: TPreviousTabAction;
    LabelInfo: TLabel;
    BindSourceDB3: TBindSourceDB;
    LinkPropertyToFieldText: TLinkPropertyToField;
    LinkPropertyToFieldVisible: TLinkPropertyToField;
    LinkPropertyToFieldVisible2: TLinkPropertyToField;
    LinkPropertyToFieldEditMode: TLinkPropertyToField;
    btnRefresh: TSpeedButton;
    EditHost: TEdit;
    EditPort: TEdit;
    procedure actNextTabToLoginUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tcStoreChange(Sender: TObject);
    procedure cbStoresChange(Sender: TObject);
    procedure actNextTabToItemsUpdate(Sender: TObject);
    procedure btnAddItemClick(Sender: TObject);
    procedure btnEditListClick(Sender: TObject);
    procedure lvItemsDeleteItem(Sender: TObject; AIndex: Integer);
    procedure btnRefreshClick(Sender: TObject);
    procedure lvItemsItemClick(const Sender: TObject;
      const AItem: TListViewItem);
  private
    { Private declarations }
    procedure PrepareLoginScreen;
    procedure PrepareItemsScreen;
    procedure Edit;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  DialogItem;

{$R *.fmx}

procedure TFormMain.cbStoresChange(Sender: TObject);
begin
  edtStorePassword.Text := string.Empty;
end;

procedure TFormMain.Edit;
begin
  if TDlgItem.ShowItemDialog = mrOk then
    ClientDM.Save
  else
    ClientDM.Cancel;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  tcStore.TabIndex := 0;
  ClientDM.Initialize(EditHost.Text, EditPort.Text);
end;

procedure TFormMain.lvItemsDeleteItem(Sender: TObject; AIndex: Integer);
begin
  ClientDM.DeleteCurrent;
end;

procedure TFormMain.lvItemsItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  if not btnEditList.Visible then
    Edit;
end;

procedure TFormMain.PrepareItemsScreen;
begin
  try
    ClientDM.SetUserCredentials(edtEmployeeLogin.Text, edtEmployeePassword.Text);
    ClientDM.LoginEmployee;
    ClientDM.LoadItems;
  except
    tcStore.ActiveTab := tiStoreEmployeeLogin;
    raise;
  end;
end;

procedure TFormMain.PrepareLoginScreen;
begin
  ClientDM.SetTenant(edtStorePassword.Text);
end;

procedure TFormMain.btnRefreshClick(Sender: TObject);
begin
  ClientDM.Initialize(EditHost.Text, EditPort.Text);

  EditPort.Visible := True;
  EditHost.Visible := True;
end;

procedure TFormMain.actNextTabToItemsUpdate(Sender: TObject);
begin
  actNextTabToItems.Enabled := not edtEmployeeLogin.Text.IsEmpty or not edtEmployeePassword.Text.IsEmpty;
end;

procedure TFormMain.actNextTabToLoginUpdate(Sender: TObject);
begin
  actNextTabToLogin.Enabled := not edtStorePassword.Text.IsEmpty and (cbStores.Items.Count > 0);
end;

procedure TFormMain.btnAddItemClick(Sender: TObject);
begin
  ClientDM.AddItem;
  if TDlgItem.ShowItemDialog = mrOk then
    ClientDM.Save
  else
    ClientDM.Cancel;
end;

procedure TFormMain.btnEditListClick(Sender: TObject);
begin
  Edit;
end;

procedure TFormMain.tcStoreChange(Sender: TObject);
begin
  if tcStore.ActiveTab = tiStoreEmployeeLogin then
  begin
    PrepareLoginScreen;
    if ClientDM.LoggedIn then
      ClientDM.Save
  end
  else
  if tcStore.ActiveTab = tiStoreItems then
    PrepareItemsScreen;
end;

end.
