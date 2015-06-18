unit UReadingFiles;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.StdCtrls, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Grid,
  FMX.TabControl, FMX.Objects, System.Math, System.Rtti,
  FMX.FlexCel.Core, FlexCel.XlsAdapter, FMX.Edit;

type
  TFReadingFiles = class(TForm)
    ToolBar1: TToolBar;
    OpenDialog: TOpenDialog;
    btnOpen: TButton;
    SheetData: TGrid;
    Image1: TImage;
    btnFormatValues: TButton;
    Image2: TImage;
    btnInfo: TButton;
    Image4: TImage;
    Tabs: TTabControl;
    procedure btnInfoClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure SheetDataGetValue(Sender: TObject; const Col, Row: Integer;
      var Value: TValue);
    procedure btnFormatValuesClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Xls: TExcelFile;
    procedure ClearGrid;
    procedure SetupGrid;
    procedure ImportFile(const FileName: string);
    procedure FillTabs;
    procedure SheetChanged(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FReadingFiles: TFReadingFiles;

implementation

{$R *.fmx}

procedure TFReadingFiles.btnFormatValuesClick(Sender: TObject);
begin
  SheetData.Repaint; //when repainting, we will read the new value of this button.
end;

procedure TFReadingFiles.btnInfoClick(Sender: TObject);
begin
  ShowMessage('This demo shows how to read the contents of an xls file' + #10 +
      'The ''Open File'' button will load an Excel file into a dataset.'+ #10 +
      'The ''Format Values'' button will apply the format to the cells, or show the raw data.'+ #10 +
      'The ''Value in Current Cell'' button will show more information about the cell selected in the grid. Try it with formulas.');

end;

procedure TFReadingFiles.btnOpenClick(Sender: TObject);
begin
  if not OpenDialog.Execute then exit;
  ImportFile(OpenDialog.FileName);
end;

procedure TFReadingFiles.ImportFile(const FileName: string);
begin
   //Open the Excel file.
  if Xls = nil then Xls := TXlsFile.Create(false);
  xls.Open(FileName);

  FillTabs;
  SetupGrid;

  Caption := 'Reading Files: ' + ExtractFileName(FileName);
end;

procedure TFReadingFiles.SheetChanged(Sender: TObject);
begin
  Xls.ActiveSheet := (Sender as TComponent).Tag;
  SetupGrid;
end;

procedure TFReadingFiles.SheetDataGetValue(Sender: TObject; const Col,
  Row: Integer; var Value: TValue);
begin
  if Xls = nil then
  begin
    Value := '';
    exit;
  end;

  if btnFormatValues.IsPressed then
  begin
    value := Xls.GetStringFromCell(Row + 1, Col + 1).ToString;
  end
  else
  begin
    value := Xls.GetCellValue(Row + 1, Col + 1);
  end;
end;

procedure TFReadingFiles.FillTabs;
var
  s, i: integer;
  btn: TTabItem;
begin
  for i := Tabs.TabCount - 1 downto 0 do Tabs.Tabs[i].Free;

  for s := 1 to Xls.SheetCount do
  begin
    btn := TTabItem.Create(Tabs);
    btn.Text := Xls.GetSheetName(s);
    btn.Tag := s;

    btn.OnClick := SheetChanged;
    Tabs.AddObject(btn);
  end;
end;


procedure TFReadingFiles.FormDestroy(Sender: TObject);
begin
  Xls.Free;
end;

procedure TFReadingFiles.ClearGrid;
var
  i: integer;
begin
  SheetData.RowCount := 0;
  for i := SheetData.ColumnCount - 1 downto 0 do SheetData.Columns[i].Free;

end;

procedure TFReadingFiles.SetupGrid;
var
  ColCount: integer;
  Column: TColumn;
  c: Integer;
begin
  SheetData.BeginUpdate;
  try
    ClearGrid;

    SheetData.RowCount := Xls.RowCount;
    ColCount := Xls.ColCount; // NOTE THAT COLCOUNT IS SLOW. We use it here because we really need it. See the Performance.pdf doc.
    //Create the columns
    for c := 1 to ColCount do
    begin
      Column := TColumn.Create(SheetData);
      Column.Width := Xls.GetColWidth(c) / TExcelMetrics.ColMult(Xls);
      Column.Header := TCellAddress.EncodeColumn(c);
      Column.Parent := SheetData;
    end;
  finally
    SheetData.EndUpdate;
  end;

  SheetData.Repaint;
end;

end.
