unit UCalcWheel;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.FlexCel.Core, FlexCel.XlsAdapter,
  FMX.Edit, FMX.Objects, FMX.Platform, FMX.Layouts, FMX.ListBox, FMX.Ani;

type
  TWheelForm = class(TForm)
    ToolBar1: TToolBar;
    btnCalc: TSpeedButton;
    lblEntry: TLabel;
    edEntry: TEdit;
    lblResult: TLabel;
    edResult: TEdit;
    Wheel: TImage;
    lblCurrent: TLabel;
    WheelPop: TPopup;
    WheelListBox: TListBox;
    ColorKeyAnimation1: TColorKeyAnimation;
    procedure btnCalcClick(Sender: TObject);
    procedure WheelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Workbook: TXlsFile;

    procedure WheelListItemBoxClick(Sender: TObject);
    procedure LoadConfig;
    procedure CreateConfig;
    function GetCell(const sheet: string; const r, c: integer): string;
    function ConfigFile: string;
    procedure LoadPopupItems;
    function DocFolder: string;
    procedure Calc;
  public
    { Public declarations }
  end;

var
  WheelForm: TWheelForm;

implementation
uses IOUtils;

{$R *.fmx}

function TWheelForm.GetCell(const sheet: string; const r, c: integer): string;
var
  ResultIndex: integer;
begin
  Result := '*Error*';
  ResultIndex := Workbook.GetSheetIndex(sheet, false);
  if (ResultIndex < 1) then
  begin
    exit;
  end;
  Workbook.ActiveSheet := ResultIndex;
  Result := Workbook.GetStringFromCell(r, c);
end;

function TWheelForm.ConfigFile: string;
begin
  Result := TPath.GetLibraryPath + '/Preferences/config.txt';
end;

function TWheelForm.DocFolder: string;
begin
  Result := TPath.GetLibraryPath + '/';
end;

procedure TWheelForm.LoadConfig;
var
  sr: TStreamReader;
  fn: string;
begin
  try
    if (TFile.Exists(ConfigFile)) then
    begin
      sr := TStreamReader.Create(ConfigFile);
      fn := sr.ReadLine;
    end else
    begin
      fn := DocFolder + 'default.xls';
    end;
    if fn = '' then Workbook := TXlsFile.Create(1, true) else Workbook := TXlsFile.Create(fn, true);
  except on ex: Exception do
  begin
    Workbook := TXlsFile.Create(1, true);
  end;
  end;

  lblCurrent.Text := TPath.GetFileNameWithoutExtension(Workbook.ActiveFileName);
  lblEntry.Text := GetCell('Data', 1, 1);
  Calc;
end;

procedure TWheelForm.CreateConfig;
var
  sw: TStreamWriter;
begin
     sw := TStreamWriter.Create(ConfigFile);
     sw.WriteLine(Workbook.ActiveFileName);
end;

procedure TWheelForm.Calc;
var
  DataIndex, ResultIndex: integer;
begin
  DataIndex := Workbook.GetSheetIndex('Data', false);
  if (DataIndex < 1) then
  begin
    ShowMessage('Can''t find the sheet "Data"');
    exit;
  end;

  Workbook.ActiveSheet := DataIndex;
  Workbook.SetCellFromString(1, 2, edEntry.Text);

  ResultIndex := Workbook.GetSheetIndex('Result', false);
  if (ResultIndex < 1) then
  begin
    ShowMessage('Can''t find the sheet "Result"');
    exit;
  end;

  Workbook.Recalc;

  Workbook.ActiveSheet := ResultIndex;
  lblResult.Text := Workbook.GetStringFromCell(1,1);
  edResult.Text := Workbook.GetStringFromCell(1,2);
end;

procedure TWheelForm.btnCalcClick(Sender: TObject);
begin
  Calc;
end;

procedure TWheelForm.FormCreate(Sender: TObject);
begin
  LoadConfig;
end;

procedure TWheelForm.LoadPopupItems;
var
  fn: string;
  files: TStringDynArray;
  li: TListBoxItem;
begin
  WheelListBox.Items.Clear;
  files := TDirectory.GetFiles(DocFolder, '*.xls');
  for fn in files do
  begin
    li := TListBoxItem.Create(WheelListBox);
    li.Text := TPath.GetFileNameWithoutExtension(fn);
    li.OnClick := WheelListItemBoxClick;
    WheelListBox.AddObject(li);
  end;
end;

procedure TWheelForm.WheelClick(Sender: TObject);
begin
  LoadPopupItems;
  WheelPop.Popup;
end;

procedure TWheelForm.WheelListItemBoxClick(Sender: TObject);
begin
  WheelPop.Visible := false;

  try
    Workbook.Open(DocFolder + (Sender as TListBoxItem).Text + '.xls');
  except
    ShowMessage('Invalid file: ' + (Sender as TListBoxItem).Text);
    Workbook.NewFile(1);
  end;

  CreateConfig;
  LoadConfig;

end;

end.
