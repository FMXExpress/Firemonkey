unit PlatformListViewOptionsMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, Data.Bind.GenData, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components,
  Data.Bind.ObjectScope, FMX.ListView, IPPeerClient, REST.Backend.ServiceTypes,
  REST.Backend.MetaTypes, System.JSON, REST.Backend.KinveyServices,
  REST.Backend.BindSource, REST.Backend.ServiceComponents,
  REST.Backend.KinveyProvider, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.Bind.DBScope, REST.Response.Adapter, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Stan.StorageBin,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ListBox, FMX.Layouts;

type
  TForm24 = class(TForm)
    ListView1: TListView;
    ToolBar1: TToolBar;
    Label1: TLabel;
    ToolBar2: TToolBar;
    Layout1: TLayout;
    Grouped: TSpeedButton;
    Styled: TSpeedButton;
    Indexed: TSpeedButton;
    FDMemTable1: TFDMemTable;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkFillControlToField1: TLinkFillControlToField;
    procedure GroupedClick(Sender: TObject);
    procedure IndexedClick(Sender: TObject);
    procedure StyledClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form24: TForm24;

implementation

{$R *.fmx}

procedure TForm24.GroupedClick(Sender: TObject);
begin
 if TListViewNativeOption.Grouped in ListView1.NativeOptions then
    ListView1.NativeOptions := ListView1.NativeOptions - [TListViewNativeOption.Grouped]
  else
    ListView1.NativeOptions := ListView1.NativeOptions + [TListViewNativeOption.Grouped];
end;

procedure TForm24.IndexedClick(Sender: TObject);
begin
 if TListViewNativeOption.Indexed in ListView1.NativeOptions then
    ListView1.NativeOptions := ListView1.NativeOptions - [TListViewNativeOption.Indexed]
  else
    ListView1.NativeOptions := ListView1.NativeOptions + [TListViewNativeOption.Indexed];
end;

procedure TForm24.StyledClick(Sender: TObject);
begin
 if TListViewNativeOption.Styled in ListView1.NativeOptions then
    ListView1.NativeOptions := ListView1.NativeOptions - [TListViewNativeOption.Styled]
  else
    ListView1.NativeOptions := ListView1.NativeOptions + [TListViewNativeOption.Styled];
end;

end.
