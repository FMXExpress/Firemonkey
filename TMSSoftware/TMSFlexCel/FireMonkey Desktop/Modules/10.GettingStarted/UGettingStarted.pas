unit UGettingStarted;

interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.StdCtrls, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.FlexCel.Core, FlexCel.XlsAdapter, FMX.FlexCel.DocExport;

type
  TFGettingStarted = class(TForm)
    BtnCreateFile: TButton;
    SaveDialog: TSaveDialog;
    DocExport: TFlexCelDocExport;
    procedure btnCreateFileClick(Sender: TObject);
  private
    procedure AddData(const Xls: TExcelFile);
    procedure CreateFile;
    procedure ShowOpenDialog(const Xls: TExcelFile);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FGettingStarted: TFGettingStarted;

implementation


{$R *.fmx}
procedure TFGettingStarted.CreateFile;
var
  Xls: TExcelFile;
begin
  Xls := TXlsFile.Create(true);
  try
    AddData(Xls);
    ShowOpenDialog(Xls);
  finally
    FreeAndNil(Xls);
  end;
end;

procedure TFGettingStarted.AddData(const Xls: TExcelFile);
var
  Img: TResourceStream;
  fmt: TFlxFormat;
  XF, XF2: integer;
begin
	//Create a new file. We could also open an existing file with Xls.Open
  Xls.NewFile(1, TExcelFileFormat.v2010);

  //Set some cell values.
  Xls.SetCellValue(1, 1, 'Hello to the world');
  Xls.SetCellValue(2, 1, 3);
  Xls.SetCellValue(3, 1, 2.1);
  Xls.SetCellValue(4, 1, TFormula.Create('=Sum(A2, A3)')); //Note that formulas always are in English. This means use "," to separate arguments, not ";".


  //Add a new image on cell F2
  Img := TResourceStream.Create(hinstance, 'FlexCelLogo', RT_RCDATA);
  try
  Xls.AddImage(Img,
    TImageProperties_Create(
    TClientAnchor.Create(TFlxAnchorType.MoveAndResize, 2, 0, 6, 0, 4, 0, 8, 0),
    '', 'My image'));
  finally
    Img.Free;
  end;

	//Add a comment on cell a2
	Xls.SetComment(2, 1, 'This is a comment');

	//Custom Format cells a2 and a3
  fmt := Xls.GetDefaultFormat;  //Always initialize the record with an existing format.
  fmt.Font.Name := 'Times New Roman';
  fmt.Font.Color := TColorRec.Red;
  fmt.FillPattern.Pattern := TFlxPatternStyle.LightDown;
  fmt.FillPattern.FgColor := TColorRec.Blue;
  fmt.FillPattern.BgColor := TColorRec.White;

	//You can call AddFormat as many times as you want, it will never add a format twice.
	//But if you know the format you are going to use, you can get some extra CPU cycles by
	//calling addformat once and saving the result into a variable.
	XF := Xls.AddFormat(fmt);

  Xls.SetCellFormat(2, 1, XF);
  Xls.SetCellFormat(3, 1, XF);

	fmt.Rotation := 45;
  fmt.Font.Size20 := 400;
  fmt.FillPattern.Pattern := TFlxPatternStyle.Solid;
  XF2 := Xls.AddFormat(fmt);

	//Apply a custom format to all the row.
	Xls.SetRowFormat(1, XF2);

  //Merge cells
	Xls.MergeCells(5, 1, 10, 6);
	//Note how this one merges with the previous range, creating a final range (5,1,15,6)
	Xls.MergeCells(10, 6, 15, 6);


	//Make the page print in landscape or portrait mode
	Xls.PrintLandscape := true;
end;

procedure TFGettingStarted.btnCreateFileClick(Sender: TObject);
begin
  CreateFile;
end;


procedure TFGettingStarted.ShowOpenDialog(const Xls: TExcelFile);
begin
  if not SaveDialog.Execute then exit;
  Xls.Save(SaveDialog.FileName); //No need to delete the file first, since AllowOverWriteFiles is true in XlsAdapter.

  if MessageDlg('Do you want to open the generated file?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
  begin
    DocExport.ExportFile(nil, SaveDialog.FileName);
  end;
end;

end.
