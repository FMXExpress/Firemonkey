unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, FMX.TMSGridDataBinding, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.DB, Datasnap.DBClient,
  Data.Bind.Components, Data.Bind.DBScope, Data.Bind.Grid, FMX.Layouts,
  Fmx.Bind.Navigator, FMX.TMSBaseControl, FMX.TMSGridCell, FMX.TMSGridOptions,
  FMX.TMSGridData, FMX.TMSCustomGrid, FMX.TMSGrid, FMX.StdCtrls, FMX.Objects,
  FMX.Edit, FMX.TMSXUtil, Data.Bind.Controls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    TMSFMXGrid1: TTMSFMXGrid;
    Panel2: TPanel;
    NavigatorBindSourceDB1: TBindNavigator;
    BindingsList1: TBindingsList;
    LinkGridToDataSource1: TLinkGridToDataSource;
    LinkControlToField1: TLinkControlToField;
    LinkControlToField2: TLinkControlToField;
    LinkControlToField3: TLinkControlToField;
    LinkControlToField4: TLinkControlToField;
    LinkPropertyToField1: TLinkPropertyToField;
    BindSourceDB1: TBindSourceDB;
    ClientDataSet1: TClientDataSet;
    BindSourceDB2: TBindSourceDB;
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXGrid1GetCellEditorType(Sender: TObject; ACol, ARow: Integer;
      var CellEditorType: TTMSFMXGridEditorType);
    procedure TMSFMXGrid1GetCellReadOnly(Sender: TObject; ACol, ARow: Integer;
      var AReadOnly: Boolean);
    procedure TMSFMXGrid1GetCellProperties(Sender: TObject; ACol, ARow: Integer;
      Cell: TFmxObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  TMSFMXGrid1.BeginUpdate;
  TMSFMXGrid1.Options.Bands.Enabled := True;
  TMSFMXGrid1.SelectionMode := smSingleRow;
  TMSFMXGrid1.EndUpdate;
  TMSFMXGrid1.DefaultRowHeight := 35;
  TMSFMXGrid1.DefaultColumnWidth := 75;
  XCopyFile(XGetRootDirectory+'CARS.xml', XGetDocumentsDirectory+'/CARS.xml');
  ClientDataSet1.LoadFromFile(XGetDocumentsDirectory + '/CARS.xml');
  BindSourceDB1.DataSet := ClientDataSet1;
  TMSFMXGrid1.EndUpdate;

  TMSFMXGrid1.CellComboBox.Items.Add('4');
  TMSFMXGrid1.CellComboBox.Items.Add('6');
  TMSFMXGrid1.CellComboBox.Items.Add('8');
  TMSFMXGrid1.CellComboBox.Items.Add('12');
end;

procedure TForm1.TMSFMXGrid1GetCellEditorType(Sender: TObject; ACol,
  ARow: Integer; var CellEditorType: TTMSFMXGridEditorType);
begin
  if ACol = 6 then CellEditorType := etComboBox
end;

procedure TForm1.TMSFMXGrid1GetCellProperties(Sender: TObject; ACol,
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

procedure TForm1.TMSFMXGrid1GetCellReadOnly(Sender: TObject; ACol,
  ARow: Integer; var AReadOnly: Boolean);
begin
  AReadOnly := (ACol = 1) or (ACol = 10);
end;

end.
