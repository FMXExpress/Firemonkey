unit UFlexCalc;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.FlexCel.Core, FlexCel.XlsAdapter,
  FMX.StdCtrls, FMX.Edit;

type
  TFFlexCalc = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    const
      RowHeight = 20;
      Margin = 10;
      HeadingW = 40;
      ResultsW = 200;
    var
    xls: TExcelFile;
    function ConfigFile: string;
    procedure EditChange(Sender: TObject);
    function GetCellOrFormula(const row: integer): string;
    procedure SetCellSize(Top: single; CellValue: TEdit);
  public
  end;

var
  FFlexCalc: TFFlexCalc;

implementation
uses IOUtils;

{$R *.fmx}

function TFFlexCalc.ConfigFile: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, 'config.xlsx');
end;

procedure TFFlexCalc.EditChange(Sender: TObject);
var
  z, k: integer;
begin
  z := (Sender as TEdit).Tag;
  xls.SetCellFromString(z + 1, 1, (Sender as TEdit).Text);
  xls.Recalc;
  xls.Save(ConfigFile);

  for k := 0 to ChildrenCount - 1 do
  begin
    if (Children[k] is TLabel) and (Children[k].Tag >= 0) then
    begin
      (Children[k] as TLabel).Text := xls.GetStringFromCell(Children[k].Tag + 1, 1);
    end;
  end;

end;

procedure TFFlexCalc.FormCreate(Sender: TObject);
const
  Predefined: array[0..10] of string =
            ('5', '=A1 * 3 + 7', '=Sum(A1, A2)*9', '=Sin(a1) + cos(a2)',
            '=Average(a1:a4)', '', '', '', '', '', '');

var
  Restoring: boolean;
  k: integer;
  ColHeading: TLabel;
  CellValue: TEdit;
  Results: TLabel;
begin
  Restoring := false;
  xls := TXlsFile.Create(true);
  if TFile.Exists(ConfigFile) then
  begin
    try
      xls.Open(ConfigFile);
      Restoring := true;
    except
      //if the file is corrupt, we'll just ignore it.
      //Restoring will be false, and we will create a new file.
    end;
  end;

  if (not Restoring) then
  begin
    xls.NewFile(1);

    for k := 0 to Length(Predefined) - 1 do
    begin
      xls.SetCellFromString(k + 1, 1, Predefined [k]); //Initialize the grid with something so users know what they have to do.
    end;
  end;

  xls.Recalc();

  for k := 0 to xls.RowCount - 1 do
  begin
    ColHeading := TLabel.Create(self);
    ColHeading.SetBounds(Margin, Margin + k * (RowHeight + 2 * Margin), HeadingW, RowHeight + Margin);
    ColHeading.Text := TCellAddress.Create(k + 1, 1).CellRef;
    ColHeading.Tag := -1;
    ColHeading.Parent := self;

    CellValue := TEdit.Create(self);
    SetCellSize(Margin + k * (RowHeight + 2 * Margin), CellValue);
    CellValue.Text := GetCellOrFormula(k + 1);
    CellValue.Tag := k;
    CellValue.OnChangeTracking := EditChange;
    CellValue.Parent := self;

    Results := TLabel.Create(self);
    Results.SetBounds(CellValue.Position.X + CellValue.Width + Margin, Margin + k * (RowHeight + 2 * Margin),
          ResultsW, RowHeight + Margin);
    Results.Text := xls.GetStringFromCell(k + 1, 1);
    Results.Tag := k;
    Results.Parent := self;
  end;
end;

procedure TFFlexCalc.FormDestroy(Sender: TObject);
begin
  if xls <> nil then xls.Save(ConfigFile);
end;

procedure TFFlexCalc.FormResize(Sender: TObject);
var
  i: integer;
begin
 for i := 0 to ChildrenCount - 1 do
 begin
   if Children[i] is TEdit then
   begin
     SetCellSize((Children[i] as TEdit).Position.Y, Children[i] as TEdit);
   end
   else if (Children[i] is TLabel) and ((Children[i] as TLabel).Tag >= 0) then
   begin
     (Children[i] as TLabel).Position.X := ClientWidth - (Children[i] as TLabel).Width - Margin;
   end;

 end;
end;

function TFFlexCalc.GetCellOrFormula(const row: integer): string;
var
  cell: TCellValue;
begin
  cell := xls.GetCellValue(row, 1);
  if (cell.IsEmpty) then exit ('');
  if (cell.IsFormula) then exit (cell.AsFormula.Text);
  exit(cell.ToString);
end;

procedure TFFlexCalc.SetCellSize(Top: single; CellValue: TEdit);
begin
  CellValue.SetBounds(HeadingW + 2 * Margin, Top, ClientWidth - HeadingW - ResultsW - 4 * Margin, RowHeight);
end;
end.
