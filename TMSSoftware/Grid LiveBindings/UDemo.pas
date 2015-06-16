unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSBaseControl,
  FMX.Layouts, FMX.TreeView, FMX.Grid, FMX.Printer, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Data.Bind.Components,
  FMX.TMSGridCell, FMX.TMSGridOptions, FMX.TMSGridData, FMX.TMSGrid, Data.DB, Datasnap.DBClient, Data.Bind.DBScope,
  Fmx.Bind.Editors, FMX.TMSGridDataBinding, Fmx.Bind.Navigator, FMX.Edit,
  FMX.TMSCustomGrid, FMX.StdCtrls;

type
  TForm738 = class(TForm)
    EditWithHandler: TEdit;
    ImageWithHandler: TImageControl;
    BindNavigator1: TBindNavigator;
    CheckBoxActiveDataSet: TCheckBox;
    LabelPosition: TLabel;
    LabelFields: TLabel;
    EditWithHandler2: TEdit;
    BindingsList: TBindingsList;
    BindLinkEditHandler: TBindLink;
    BindLinkEditHandler2: TBindLink;
    BindLinkImageHandler: TBindLink;
    BindLinkPosition: TBindLink;
    BindLinkLabel: TBindLink;
    BindScopeDB1: TBindScopeDB;
    ClientDataSet1: TClientDataSet;
    CategoryField: TStringField;
    SpeciesNameField: TStringField;
    LengthCmField: TFloatField;
    LengthInField: TFloatField;
    CommonNameField: TStringField;
    NotesField: TMemoField;
    GraphicField: TBlobField;
    ClientDataSetDataSource1: TDataSource;
    TMSFMXGrid1: TTMSFMXGrid;
    TMSFMXBindDBGridLinkTMSFMXGridLiveBinding11: TTMSFMXBindDBGridLink;
    procedure CheckBoxActiveDataSetChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXGridLiveBinding1GetCellProperties(Sender: TObject; ACol,
      ARow: Integer; Cell: TFmxObject);
    procedure TMSFMXGrid1GetCellEditorType(Sender: TObject; ACol, ARow: Integer;
      var CellEditorType: TTMSFMXGridEditorType);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form738: TForm738;

implementation

{$R *.fmx}

procedure TForm738.CheckBoxActiveDataSetChange(Sender: TObject);
begin
  ClientDataSetDataSource1.Enabled := True;
  ClientDataSet1.Active := CheckBoxActiveDataSet.IsChecked;
end;

procedure TForm738.FormCreate(Sender: TObject);
begin
  ClientDataSetDataSource1.Enabled := False;
  ClientDataSet1.LoadFromFile('..\..\biolife.xml');
  ClientDataSet1.Active := False;
  TMSFMXGrid1.DefaultColumnWidth := 150;
  TMSFMXGrid1.DefaultRowHeight := 30;
  TMSFMXGrid1.ColumnWidths[0] := 30;
  TMSFMXGrid1.ColumnCount := 5;
  TMSFMXGrid1.Options.Bands.Enabled := True;
end;

procedure TForm738.TMSFMXGrid1GetCellEditorType(Sender: TObject; ACol,
  ARow: Integer; var CellEditorType: TTMSFMXGridEditorType);
var
  fn: string;
  f: TField;
begin
  if ACol > 0 then
  begin
    fn := TMSFMXBindDBGridLinkTMSFMXGridLiveBinding11.ColumnExpressions[ACol- TMSFMXGrid1.FixedColumns].SourceMemberName;
    if fn <> '' then
    begin
      f := ClientDataSet1.FieldByName(fn);
      case f.DataType of
        ftUnknown: ;
        ftString: ;
        ftSmallint: ;
        ftInteger: ;
        ftWord: ;
        ftBoolean: ;
        ftFloat: ;
        ftCurrency: ;
        ftBCD: ;
        ftDate: ;
        ftTime: ;
        ftDateTime: ;
        ftBytes: ;
        ftVarBytes: ;
        ftAutoInc: ;
        ftBlob: ;
        ftMemo: ;
        ftGraphic: ;
        ftFmtMemo: ;
        ftParadoxOle: ;
        ftDBaseOle: ;
        ftTypedBinary: ;
        ftCursor: ;
        ftFixedChar: ;
        ftWideString: ;
        ftLargeint: ;
        ftADT: ;
        ftArray: ;
        ftReference: ;
        ftDataSet: ;
        ftOraBlob: ;
        ftOraClob: ;
        ftVariant: ;
        ftInterface: ;
        ftIDispatch: ;
        ftGuid: ;
        ftTimeStamp: ;
        ftFMTBcd: ;
        ftFixedWideChar: ;
        ftWideMemo: ;
        ftOraTimeStamp: ;
        ftOraInterval: ;
        ftLongWord: ;
        ftShortint: ;
        ftByte: ;
        ftExtended: ;
        ftConnection: ;
        ftParams: ;
        ftStream: ;
        ftTimeStampOffset: ;
        ftObject: ;
        ftSingle: ;
      end;
    end;
  end;
end;

procedure TForm738.TMSFMXGridLiveBinding1GetCellProperties(Sender: TObject;
  ACol, ARow: Integer; Cell: TFmxObject);
begin
  if (Cell is TTMSFMXBitmapGridCell) then
    (Cell as TTMSFMXBitmapGridCell).Bitmap.Align := TAlignLayout.alClient;
end;

end.

