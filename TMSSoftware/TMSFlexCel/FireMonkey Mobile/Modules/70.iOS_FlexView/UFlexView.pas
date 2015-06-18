unit UFlexView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.FlexCel.Core,
  FlexCel.XlsAdapter, FlexCel.Render, FMX.Layouts, FMX.FlexCel.Preview,
  FMX.StdCtrls, FMX.TabControl, FMX.Edit,
  {$IFDEF IOS}
  FMX.Platform, FMX.Platform.iOS,
  {$ENDIF}
  FMX.Memo, FMX.FlexCel.DocExport, FMX.ListBox, IOUtils,
  FMX.Controls.Presentation;

type
  TFormFlexView = class(TForm)
    FlexCelPreviewer1: TFlexCelPreviewer;
    ToolBar1: TToolBar;
    edEdit: TSpeedButton;
    PanelEditor: TCalloutPanel;
    edCell: TMemo;
    edAddress: TEdit;
    edCancel: TSpeedButton;
    edOk: TSpeedButton;
    FlexCelDocExport1: TFlexCelDocExport;
    edShare: TSpeedButton;
    PopShare: TPopup;
    ListBox1: TListBox;
    edExcel: TListBoxItem;
    edPdf: TListBoxItem;
    edSheets: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure edEditClick(Sender: TObject);
    procedure edOkClick(Sender: TObject);
    procedure edCancelClick(Sender: TObject);
    procedure edAddressChange(Sender: TObject);
    procedure edShareClick(Sender: TObject);
    procedure edExcelClick(Sender: TObject);
    procedure edPdfClick(Sender: TObject);
    procedure edSheetsChange(Sender: TObject);
  private
    {$IFDEF IOS}
    function AppHandler(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
    function OpenFile(const aURL: string): boolean;
    procedure LoadSheets(const xls: TXlsFile);
    {$endif}
    procedure UpdateCellValue;
    function GetTmpPath: string;
  public
  end;

var
  FormFlexView: TFormFlexView;

implementation
{$IFDEF IOS}
uses
  Macapi.Helpers,
  iOSapi.Foundation;
{$ENDIF}

{$R *.fmx}

{$IFDEF IOS}
function GetPhysicalPath(const URL: string): string;
var
 FileURL: NSURL;
begin
  FileURL := TNSURL.Wrap(TNSURL.OCClass.URLWithString(StrToNSStr(URL)));
  Result := UTF8ToString(FileURL.path.UTF8String);
end;

function TFormFlexView.AppHandler(AAppEvent: TApplicationEvent;
  AContext: TObject): Boolean;
begin
  Result := true;

  case AAppEvent of
   TApplicationEvent.OpenURL:
   begin
     Result := OpenFile(GetPhysicalPath((AContext as TiOSOpenApplicationContext).URL));
   end;
  end;

end;
{$endif}

procedure TFormFlexView.UpdateCellValue;
var
  addr: TCellAddress;
begin
  if Trim(edAddress.Text) <> '' then
  begin
    try
      addr := TCellAddress.Create(edAddress.Text);
    except
      exit;
    end;
    edCell.Text := FlexCelPreviewer1.Document.Workbook.GetCellValue(addr.Row, addr.Col).ToString;
  end else
  begin
    edCell.Text := '';
  end;
end;

procedure TFormFlexView.edAddressChange(Sender: TObject);
begin
  UpdateCellValue;
end;

procedure TFormFlexView.edCancelClick(Sender: TObject);
begin
  UpdateCellValue;
  PanelEditor.Visible := false;
end;

procedure TFormFlexView.edEditClick(Sender: TObject);
begin
  PanelEditor.Visible := true;
end;

procedure TFormFlexView.edExcelClick(Sender: TObject);
begin
  PopShare.Visible := false;
  FlexCelPreviewer1.Document.Workbook.Save(GetTmpPath + '/tmpflexcel.xlsx');
  FlexCelDocExport1.ExportFile(edShare, FlexCelPreviewer1.Document.Workbook.ActiveFileName);
end;

procedure TFormFlexView.edPdfClick(Sender: TObject);
var
  pdf: TFlexCelPdfExport;
  tmppdf: string;
begin
  PopShare.Visible := false;
  pdf := TFlexCelPdfExport.Create(FlexCelPreviewer1.Document.Workbook, true);
  tmppdf := GetTmpPath + '/tmpflexcel.pdf';
  pdf.BeginExport(TFileStream.Create(tmppdf, fmCreate));
  pdf.ExportAllVisibleSheets(false, 'Sheets');
  pdf.EndExport;
  FlexCelDocExport1.ExportFile(edShare, tmppdf);

end;


procedure TFormFlexView.edOkClick(Sender: TObject);
var
  addr: TCellAddress;
begin
  if Trim(edAddress.Text) <> '' then
  begin
    try
      addr := TCellAddress.Create(edAddress.Text);
    except
      ShowMessage('Invalid Cell Address: ' + edAddress.Text);
      exit;
    end;
    FlexCelPreviewer1.Document.Workbook.SetCellFromString(addr.Row, addr.Col, edCell.Text);
    FlexCelPreviewer1.Document.Workbook.Recalc;
    FlexCelPreviewer1.InvalidatePreview;
  end;
  PanelEditor.Visible := false;
end;

procedure TFormFlexView.edShareClick(Sender: TObject);
begin
  PopShare.Parent := edShare;
  PopShare.Popup;
end;

procedure TFormFlexView.edSheetsChange(Sender: TObject);
begin
  if (edSheets.ItemIndex < 0) then exit;
  FlexCelPreviewer1.Document.Workbook.ActiveSheet := edSheets.ItemIndex + 1;
  FlexCelPreviewer1.InvalidatePreview;
end;

procedure TFormFlexView.FormCreate(Sender: TObject);
begin
{$IFDEF IOS}
  IFmxApplicationEventService(TPlatformServices.Current.GetPlatformService(IFmxApplicationEventService)).SetApplicationEventHandler(AppHandler);
{$ENDIF}
  //Create an empty document for if the user starts the application directly.
  FlexCelPreviewer1.Document := TFlexCelImgExport.Create(TXlsFile.Create(1, true), true);
  FlexCelPreviewer1.InvalidatePreview;
end;

function TFormFlexView.GetTmpPath: string;
begin
  Result := TPath.GetTempPath;
end;

{$IFDEF IOS}
function TFormFlexView.OpenFile(const aURL: string): boolean;
var
  xls: TXlsFile;
  ImgExport: TFlexCelImgExport;
begin
  Result := true;
  try
    try
      xls := TXlsFile.Create(aURL, true);
    finally
      TFile.Delete(aURL); //We've already read it. Now we need to delete it or it would stay forever in the inbox.
      //The file must be deleted even if it was invalid and FlexCel raised an Exception when opening it.

    end;
    ImgExport := TFlexCelImgExport.Create(xls, true);
    FlexCelPreviewer1.Document := ImgExport;
    LoadSheets(xls);
    FlexCelPreviewer1.InvalidatePreview;
  except
    Result := false;
  end;
end;

procedure TFormFlexView.LoadSheets(const xls: TXlsFile);
var
  i: Integer;
begin
  edSheets.Clear;
  for i := 1 to xls.SheetCount do
  begin
    edSheets.Items.Add(ExtractFileName(xls.ActiveFileName) + ' - Sheet: ' + xls.GetSheetName(i));
  end;
  edSheets.ItemIndex := xls.ActiveSheet - 1;
end;
{$ENDIF}

end.
