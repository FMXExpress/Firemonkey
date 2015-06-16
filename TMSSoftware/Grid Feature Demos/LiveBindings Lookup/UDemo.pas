unit UDemo;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants, FMX.Forms,
  FMX.Types, FMX.Controls, FMX.TMSBaseControl, FMX.TMSGridCell,
  FMX.TMSGridOptions, FMX.TMSGridData, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  FMX.TMSGridDataBinding, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.DB, Datasnap.DBClient, FMX.Layouts, Fmx.Bind.Navigator,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope, FMX.TMSCustomGrid,
  FMX.TMSGrid, FMX.ListBox, FMX.Objects, FMX.Edit, Data.Win.ADODB, FMX.StdCtrls;

type
  TForm719 = class(TForm)
    BindingsList1: TBindingsList;
    NavigatorBindSourceDB1: TBindNavigator;
    Panel2: TPanel;
    ADOTable1: TADOTable;
    ADOConnection2: TADOConnection;
    ADOTable2: TADOTable;
    ADOTable3: TADOTable;
    ADOTable1ID: TAutoIncField;
    ADOTable1ShortText: TWideStringField;
    ADOTable1Number: TIntegerField;
    ADOTable1Currency: TBCDField;
    ADOTable1DateTime: TDateTimeField;
    ADOTable1YesNo: TBooleanField;
    ADOTable1LongText1: TWideMemoField;
    ADOTable1Hyperlink: TWideMemoField;
    ADOTable1Country: TIntegerField;
    ADOTable1Field1: TIntegerField;
    ADOTable1CountryName: TStringField;
    ADOTable1CarName: TStringField;
    TMSFMXGrid1: TTMSFMXGrid;
    BindSourceDB2: TBindSourceDB;
    BindSourceDB3: TBindSourceDB;
    BindSourceDB1: TBindSourceDB;
    LinkGridToDataSource1: TLinkGridToDataSource;
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXGrid1GetCellEditorType(Sender: TObject; ACol, ARow: Integer;
      var CellEditorType: TTMSFMXGridEditorType);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form719: TForm719;
  LinkCountry, LinkCar: TLinkFillControlToField;

implementation

{$R *.fmx}

procedure TForm719.FormCreate(Sender: TObject);
begin
  ADOConnection2.ConnectionString := 'Provider=Microsoft.JET.OLEDB.4.0;User ID=Admin;Data Source=..\..\sample.mdb;';
  ADOTable1.Active := True;
  ADOTable2.Active := True;
  ADOTable3.Active := True;

  LinkCountry := TLinkFillControlToField.Create(Self);
  LinkCountry.DataSource := BindSourceDB1;
  LinkCountry.FieldName := 'Country';
  LinkCountry.FillDataSource := BindSourceDB2;
  LinkCountry.FillDisplayFieldName := 'Country';
  LinkCountry.FillValueFieldName := 'ID';
  LinkCountry.Track := False;

  LinkCar := TLinkFillControlToField.Create(Self);
  LinkCar.DataSource := BindSourceDB1;
  LinkCar.FieldName := 'Field1';
  LinkCar.FillDataSource := BindSourceDB3;
  LinkCar.FillDisplayFieldName := 'Field1';
  LinkCar.FillValueFieldName := 'ID';
  LinkCar.Track := False;
end;

procedure TForm719.TMSFMXGrid1GetCellEditorType(Sender: TObject; ACol,
  ARow: Integer; var CellEditorType: TTMSFMXGridEditorType);
begin
  if ACol = TMSFMXGrid1.ColumnCount - 3 then
  begin
    LinkCar.Control := nil;
    LinkCountry.Control := TMSFMXGrid1.CellComboBox;
    LinkCountry.Active := True;
    CellEditorType := etComboBox;
  end
  else if ACol = TMSFMXGrid1.ColumnCount - 1 then
  begin
    LinkCountry.Control := nil;
    LinkCar.Control := TMSFMXGrid1.CellComboBox;
    LinkCar.Active := True;
    CellEditorType := etComboBox;
  end;
end;

end.
