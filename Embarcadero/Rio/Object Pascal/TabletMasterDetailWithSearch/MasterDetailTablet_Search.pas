//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit MasterDetailTablet_Search;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts,
  Data.Bind.GenData, Fmx.Bind.GenData, Data.Bind.Components,
  Data.Bind.ObjectScope, FMX.Edit, FMX.ListBox, FMX.StdCtrls, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  FMX.Objects, Fmx.Bind.Navigator, System.Actions, FMX.ActnList, FMX.Memo,
  FMX.ListView.Types, FMX.ListView, FMX.ScrollBox, FMX.Controls.Presentation,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base;

type
  TTabletSearchForm = class(TForm)
    LeftLayout: TLayout;
    RightLayout: TLayout;
    ToolBarList: TToolBar;
    ToolBarDetail: TToolBar;
    PrototypeBindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    imgContact: TImage;
    lblTitle: TLabel;
    LinkPropertyToFieldBitmap: TLinkPropertyToField;
    LinkPropertyToFieldText: TLinkPropertyToField;
    lblName: TLabel;
    LinkPropertyToFieldText2: TLinkPropertyToField;
    ListToolbarLabel: TLabel;
    lblDetail: TLabel;
    ActionList1: TActionList;
    LiveBindingsBindNavigatePrior1: TFMXBindNavigatePrior;
    LiveBindingsBindNavigateNext1: TFMXBindNavigateNext;
    Line1: TLine;
    DetailMemo: TMemo;
    LinkControlToField1: TLinkControlToField;
    btnUp: TSpeedButton;
    btnDown: TSpeedButton;
    ListView1: TListView;
    LinkFillControlToField1: TLinkFillControlToField;
    procedure ListView1ItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure LiveBindingsBindNavigatePrior1Execute(Sender: TObject);
    procedure LiveBindingsBindNavigateNext1Execute(Sender: TObject);
    procedure LiveBindingsBindNavigatePrior1Update(Sender: TObject);
    procedure LiveBindingsBindNavigateNext1Update(Sender: TObject);
  private
    function Filtered: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TabletSearchForm: TTabletSearchForm;

implementation

{$R *.fmx}
{$R *.iPad.fmx IOS}
{$R *.iPhone4in.fmx IOS}
{$R *.NmXhdpiPh.fmx ANDROID}
{$R *.LgXhdpiTb.fmx ANDROID}

uses
  System.Math,
  System.StrUtils;



function TTabletSearchForm.Filtered: Boolean;
begin
  Result := ListView1.Items.Filtered;
end;

procedure TTabletSearchForm.ListView1ItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  // Goto a row in the data source
  PrototypeBindSource1.Locate('ContactName1', AItem.Text);
end;

procedure TTabletSearchForm.LiveBindingsBindNavigateNext1Execute(Sender: TObject);
begin
  PrototypeBindSource1.Next;
  if not Filtered then
    ListView1.ItemIndex := PrototypeBindSource1.ItemIndex;
end;

procedure TTabletSearchForm.LiveBindingsBindNavigateNext1Update(Sender: TObject);
begin
  (Sender as TAction).Enabled := (not (PrototypeBindSource1 as IScopeNavigator).Eof) and not Filtered;
end;

procedure TTabletSearchForm.LiveBindingsBindNavigatePrior1Execute(Sender: TObject);
begin
  PrototypeBindSource1.Prior;
  if not Filtered then
    ListView1.ItemIndex := PrototypeBindSource1.ItemIndex;
end;

procedure TTabletSearchForm.LiveBindingsBindNavigatePrior1Update(Sender: TObject);
begin
  (Sender as TAction).Enabled :=  (not (PrototypeBindSource1 as IScopeNavigator).Bof) and  not Filtered;
end;

end.
