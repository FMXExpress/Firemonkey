unit UDemo;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants, FMX.Forms,
  FMX.Types, FMX.Controls, FMX.TMSBaseControl, FMX.TMSGridCell,
  FMX.TMSGridOptions, FMX.TMSGridData, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  FMX.TMSGridDataBinding, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.DB, Datasnap.DBClient, FMX.Layouts, Fmx.Bind.Navigator,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope, FMX.TMSCustomGrid,
  FMX.TMSGrid, FMX.ListBox, FMX.Objects, FMX.Edit, FMX.StdCtrls,
  Data.Bind.Controls;

type
  TForm719 = class(TForm)
    Panel1: TPanel;
    BindingsList1: TBindingsList;
    TMSFMXGrid1: TTMSFMXGrid;
    BindSourceDB1: TBindSourceDB;
    LinkGridToDataSource1: TLinkGridToDataSource;
    NavigatorBindSourceDB1: TBindNavigator;
    ClientDataSet1: TClientDataSet;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    BindSourceDB2: TBindSourceDB;
    LinkControlToField1: TLinkControlToField;
    LinkControlToField2: TLinkControlToField;
    LinkControlToField3: TLinkControlToField;
    LinkControlToField4: TLinkControlToField;
    LinkPropertyToField1: TLinkPropertyToField;
    Panel2: TPanel;
    Label5: TLabel;
    procedure TMSFMXGrid1GetCellProperties(Sender: TObject; ACol, ARow: Integer;
      Cell: TFmxObject);
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXGrid1GetCellEditorType(Sender: TObject; ACol, ARow: Integer;
      var CellEditorType: TTMSFMXGridEditorType);
    procedure TMSFMXGrid1GetCellReadOnly(Sender: TObject; ACol, ARow: Integer;
      var AReadOnly: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form719: TForm719;

implementation

{$R *.fmx}

procedure TForm719.FormCreate(Sender: TObject);
begin
  TMSFMXGrid1.BeginUpdate;
  TMSFMXGrid1.Options.Bands.Enabled := True;
  TMSFMXGrid1.SelectionMode := smSingleRow;
  TMSFMXGrid1.DefaultRowHeight := 35;
  TMSFMXGrid1.DefaultColumnWidth := 75;
  ClientDataSet1.LoadFromFile('..\..\cars.xml');
  BindSourceDB1.DataSet := ClientDataSet1;
  TMSFMXGrid1.EndUpdate;

  TMSFMXGrid1.CellComboBox.Items.Add('4');
  TMSFMXGrid1.CellComboBox.Items.Add('6');
  TMSFMXGrid1.CellComboBox.Items.Add('8');
  TMSFMXGrid1.CellComboBox.Items.Add('12');
end;

procedure TForm719.TMSFMXGrid1GetCellEditorType(Sender: TObject; ACol,
  ARow: Integer; var CellEditorType: TTMSFMXGridEditorType);
begin
  if ACol = 6 then CellEditorType := etComboBox
end;

procedure TForm719.TMSFMXGrid1GetCellProperties(Sender: TObject; ACol,
  ARow: Integer; Cell: TFmxObject);
begin
  if (Cell is TTMSFMXBitmapGridCell) then
  begin
    {$if compilerversion > 26}
    (Cell as TTMSFMXBitmapGridCell).Bitmap.Align := TAlignLayout.Client;
    {$else}
    (Cell as TTMSFMXBitmapGridCell).Bitmap.Align := TAlignLayout.alClient;
    {$ifend}
    (Cell as TTMSFMXBitmapGridCell).Bitmap.HitTest := False;
  end;
end;

procedure TForm719.TMSFMXGrid1GetCellReadOnly(Sender: TObject; ACol,
  ARow: Integer; var AReadOnly: Boolean);
begin
  AReadOnly := (ACol = 1) or (ACol = 10);
end;

end.
