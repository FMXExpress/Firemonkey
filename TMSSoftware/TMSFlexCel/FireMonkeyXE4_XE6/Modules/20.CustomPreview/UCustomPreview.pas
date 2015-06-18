unit UCustomPreview;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.StdCtrls, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Ani,
  FMX.FlexCel.Core, FlexCel.XlsAdapter, FlexCel.Render, FMX.Layouts,
  FMX.FlexCel.Preview, FMX.ListBox, FMX.Edit, FMX.Menus,
{$if CompilerVersion >= 26.0}  
  FMX.Printer,
{$endif}
  UPasswordDialog, UPdfExporting, UPrinting;

type
  TFCustomPreview = class(TForm)
    ToolBar1: TToolBar;
    Image1: TImage;
    ActionOpen: TButton;
    StyleBook1: TStyleBook;
    ActionPrint: TButton;
    Image2: TImage;
    ActionPdf: TButton;
    Image3: TImage;
    ActionAutofit: TButton;
    Image4: TImage;
    Layout1: TLayout;
    Splitter1: TSplitter;
    Layout2: TLayout;
    MainPreview: TFlexCelPreviewer;
    Thumbs: TFlexCelPreviewer;
    ActionClose: TButton;
    Image5: TImage;
    OpenDialog: TOpenDialog;
    PanelPrinting: TPanel;
    Layout3: TLayout;
    btnPrintCancel: TButton;
    PanelPdf: TPanel;
    Layout4: TLayout;
    btnPdfCancel: TButton;
    PanelPdfError: TPanel;
    Layout5: TLayout;
    btnPdfErrorClose: TButton;
    PanelPrintingError: TPanel;
    Layout6: TLayout;
    btnPrintingErrorClose: TButton;
    lblPrintingError: TLabel;
    PanelPrintingOk: TPanel;
    Layout7: TLayout;
    btnPrintOkClose: TButton;
    Label3: TLabel;
    PanelPdfOk: TPanel;
    Layout8: TLayout;
    btnPdfOkClose: TButton;
    Label4: TLabel;
    btnOpenGeneratedFile: TButton;
    PanelSheets: TLayout;
    lbSheets: TListBox;
    Panel1: TPanel;
    Label5: TLabel;
    PanelSelectPage: TLayout;
    ActionZoom: TButton;
    Image6: TImage;
    Panel2: TPanel;
    cbAllSheets: TCheckBox;
    ActionGridlines: TButton;
    Image7: TImage;
    ActionHeadings: TButton;
    Image8: TImage;
    ActionRecalc: TButton;
    Image9: TImage;
    PdfSaveDialog: TSaveDialog;
    Label6: TLabel;
    Label7: TLabel;
    lblPdfPage: TLabel;
    PdfProgressBar: TProgressBar;
    PrintProgressBar: TProgressBar;
    lblPrintPage: TLabel;
    lblPdfError: TLabel;
    PrintDialog: TPrintDialog;
    Label1: TLabel;
    lblTotalPages: TLabel;
    edPage: TEdit;
    AutofitMenu: TPopupMenu;
    NoAutofit1: TMenuItem;
    FittoWidth1: TMenuItem;
    FittoHeight1: TMenuItem;
    FittoPage1: TMenuItem;
    PanelZoom: TPanel;
    TrackBarZoom: TTrackBar;
    btn25: TButton;
    btn50: TButton;
    btn75: TButton;
    btn100: TButton;
    btn150: TButton;
    MainBkg: TRectangle;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionCloseClick(Sender: TObject);
    procedure ActionOpenClick(Sender: TObject);
    procedure cbAllSheetsChange(Sender: TObject);
    procedure lbSheetsChange(Sender: TObject);
    procedure btnPdfErrorCloseClick(Sender: TObject);
    procedure btnPrintingErrorCloseClick(Sender: TObject);
    procedure btnPrintOkCloseClick(Sender: TObject);
    procedure btnPdfOkCloseClick(Sender: TObject);
    procedure ActionPdfClick(Sender: TObject);
    procedure ActionPrintClick(Sender: TObject);
    procedure btnOpenGeneratedFileClick(Sender: TObject);
    procedure btnPdfCancelClick(Sender: TObject);
    procedure btnPrintCancelClick(Sender: TObject);
    procedure MainPreviewStartPageChanged(Sender: TObject);
    procedure edPageExit(Sender: TObject);
    procedure edPageKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure MainPreviewZoomChanged(Sender: TObject);
    procedure NoAutofit1Click(Sender: TObject);
    procedure FittoWidth1Click(Sender: TObject);
    procedure FittoHeight1Click(Sender: TObject);
    procedure FittoPage1Click(Sender: TObject);
    procedure ActionAutofitClick(Sender: TObject);
    procedure ActionRecalcClick(Sender: TObject);
    procedure ActionGridlinesClick(Sender: TObject);
    procedure ActionHeadingsClick(Sender: TObject);
    procedure ActionZoomClick(Sender: TObject);
    procedure btn25Click(Sender: TObject);
    procedure btn50Click(Sender: TObject);
    procedure btn75Click(Sender: TObject);
    procedure btn100Click(Sender: TObject);
    procedure btn150Click(Sender: TObject);
    procedure TrackBarZoomChange(Sender: TObject);
    procedure PanelZoomMouseLeave(Sender: TObject);
    procedure PanelZoomExit(Sender: TObject);
  private
    Xls: TExcelFile;
    ImgExport: TFlexCelImgExport;
    PrintingThread: TPrintingThread;
    PdfThread: TPdfThread;
    DisabledCount: integer;
    ChangingZoom: boolean;
    procedure EnableCommonActions(const Enable: boolean);
    procedure LoadFile(const FileName: string);
    procedure GetPassword(const e: TOnPasswordEventArgs);
    procedure UpdateZoom;
    procedure UpdateAutofitText;
    procedure UpdatePages;
    procedure ChangePages;

  public
    { Public declarations }
  end;

var
  FCustomPreview: TFCustomPreview;

implementation
uses
{$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  Posix.Stdlib;
{$ENDIF POSIX}

{$R *.fmx}

procedure TFCustomPreview.FormCreate(Sender: TObject);
begin
  PanelPdfOk.Visible := false;
  PanelPdfError.Visible := false;
  PanelPdf.Visible := false;
  PanelPrintingOk.Visible := false;
  PanelPrintingError.Visible := false;
  PanelPrinting.Visible := false;

  Xls := TXlsFile.Create(1, false);
  Xls.Protection.OnPassword := GetPassword;
  ImgExport := TFlexCelImgExport.Create(Xls, false);
  ImgExport.AllVisibleSheets := false;
  MainPreview.Document := ImgExport;
  Thumbs.Document := ImgExport;

{$IFDEF MSWINDOWS}
  //this won't work in OSX, there we need a different approach.
  if ParamCount > 0 then LoadFile(ParamStr(1)); //allow the app to be called by clicking a file in the explorer.
{$ENDIF}
end;

procedure TFCustomPreview.FormDestroy(Sender: TObject);
begin
  FreeAndNil(PrintingThread);
  FreeAndNil(PdfThread);
  FreeAndNil(ImgExport);
  FreeAndNil(Xls); //after freeing the threads, so we don't free the xls object while they are working.
end;

procedure TFCustomPreview.ActionCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFCustomPreview.GetPassword(const e: TOnPasswordEventArgs);
var
  Pwd: TPasswordDialog;
begin
  Pwd := TPasswordDialog.Create(self);
  try
    Pwd.SetFileName(OpenDialog.FileName);
    if Pwd.ShowModal <> mrOk then exit;
    e.Password := Pwd.Password;
  finally
    FreeAndNil(Pwd);
  end;
end;


procedure TFCustomPreview.ActionOpenClick(Sender: TObject);
begin
  if not OpenDialog.Execute then exit;
  LoadFile(OpenDialog.FileName);
end;

procedure TFCustomPreview.LoadFile(const FileName: string);
var
  i: Integer;
begin
  PanelPdfOk.Visible := false;
  PanelPdfError.Visible := false;
  PanelPdf.Visible := false;
  PanelPrintingOk.Visible := false;
  PanelPrintingError.Visible := false;
  PanelPrinting.Visible := false;


  OpenDialog.FileName := FileName;
  lbSheets.Items.Clear;

  try
    Xls.Open(FileName);
  except on ex: Exception do
  begin
    EnableCommonActions(false);
    ActionPrint.Enabled := false;
    ActionPdf.Enabled := false;
    ActionZoom.Enabled := false;
    ActionAutofit.Enabled := false;
    ActionOpen.Enabled := true;
    PanelSelectPage.Visible := false;
    Xls.NewFile(1);
    Caption := 'Custom Preview';
    ShowMessage('Error opening file: ' + ex.Message);
    MainPreview.InvalidatePreview;
    exit;
  end;
  end;

  for i := 1 to Xls.SheetCount do
  begin
    lbSheets.Items.Add(Xls.GetSheetName(i));
  end;
  lbSheets.ItemIndex := Xls.ActiveSheet - 1;

  EnableCommonActions(true);
  ActionPrint.Enabled := true;
  ActionPdf.Enabled := true;
  ActionZoom.Enabled := true;
  ActionAutofit.Enabled := true;
  Caption := 'Custom Preview: ' + OpenDialog.FileName;
  PanelSelectPage.Visible := true;
  MainPreview.InvalidatePreview;
end;

procedure TFCustomPreview.cbAllSheetsChange(Sender: TObject);
begin
  PanelSheets.Visible := not cbAllSheets.IsChecked;
  ImgExport.AllVisibleSheets := cbAllSheets.IsChecked;
  MainPreview.InvalidatePreview();
end;

procedure TFCustomPreview.lbSheetsChange(Sender: TObject);
begin
  if (lbSheets.Items.Count > Xls.SheetCount) or (lbSheets.ItemIndex < 0) then exit;
  Xls.ActiveSheet := lbSheets.ItemIndex + 1;
  MainPreview.InvalidatePreview();
end;

procedure TFCustomPreview.EnableCommonActions(const Enable: boolean);
begin
  if Enable then Dec(DisabledCount) else Inc(DisabledCount);
  if DisabledCount < 0 then DisabledCount := 0;
  if Enable and (DisabledCount > 0) then exit; //we would be both printing and exporting to pdf, if one finishes, the buttons shouldn't be enabled util the other finishes too.


  ActionOpen.Enabled := Enable;
  ActionGridLines.Enabled := Enable;
  ActionHeadings.Enabled := Enable;
  ActionRecalc.Enabled := Enable;
end;

procedure TFCustomPreview.ActionPdfClick(Sender: TObject);
begin
  if not PdfSaveDialog.Execute then exit;

  PdfProgressBar.Value := 0;
  lblPdfPage.Text := 'Initializing';
  EnableCommonActions(false);
  ActionPdf.Enabled := false;
  btnPdfCancel.Enabled := true;
  btnPdfCancel.Text := 'Cancel';

  PanelPdfOk.Visible := false;
  PanelPdfError.Visible := false;
  PanelPdf.Visible := true;

  FreeAndNil(PdfThread);
  PdfThread := TPdfThread.Create(
    Xls,
    procedure(Progress: integer; Msg: string)
    begin
      PdfProgressBar.Value := Progress;
      lblPdfPage.Text := Msg;
    end,

    procedure(Ok: boolean; Msg: string)
    begin
      PanelPdf.Visible := false;
      if not Ok then
      begin
        PanelPdfError.Visible := true;
        lblPdfError.Text := 'Error exporting to PDF: ' + Msg;
      end
      else
      begin
        PanelPdfOk.Visible := true;
      end;
      EnableCommonActions(true);
      ActionPdf.Enabled := true;
    end,
    PdfSaveDialog.FileName,
    cbAllSheets.IsChecked);

  PdfThread.Start;
end;


procedure TFCustomPreview.ActionPrintClick(Sender: TObject);
begin
  //Printing in firemonkey isn't supported yet.
  if not PrintDialog.Execute then exit;

  PrintProgressBar.Value := 0;
  lblPrintPage.Text := 'Initializing';
  EnableCommonActions(false);
  ActionPrint.Enabled := false;

  btnPrintCancel.Enabled := true;
  btnPrintCancel.Text := 'Cancel';

  PanelPrintingOk.Visible := false;
  PanelPrintingError.Visible := false;
  PanelPrinting.Visible := true;

  FreeAndNil(PrintingThread);
  PrintingThread := TPrintingThread.Create(
    Xls,
    procedure(Progress: integer; Msg: string)
    begin
      PrintProgressBar.Value := Progress;
      lblPrintPage.Text := Msg;
    end,

    procedure(Ok: boolean; Msg: string)
    begin
      PanelPrinting.Visible := false;
      if not Ok then
      begin
        PanelPrintingError.Visible := true;
        lblPrintingError.Text := 'Error printing: ' + Msg;
      end
      else
      begin
        PanelPrintingOk.Visible := true;
      end;
      EnableCommonActions(true);
      ActionPrint.Enabled := true;
    end,
    '',
    cbAllSheets.IsChecked);

  PrintingThread.Start;

end;

procedure TFCustomPreview.btnOpenGeneratedFileClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(PdfSaveDialog.FileName), '', '', SW_SHOWNORMAL);
{$ENDIF}
{$IFDEF POSIX}
  _system(PAnsiChar('open ' + UTF8Encode(PdfSaveDialog.FileName)));
{$ENDIF POSIX}
end;

procedure TFCustomPreview.btnPdfCancelClick(Sender: TObject);
begin
  if PdfThread = nil then //it shouldn't really happen
  begin
    PanelPdf.Visible := false;
    exit;
  end;
  btnPdfCancel.Enabled := false;
  btnPdfCancel.Text := 'Canceling...';
  PdfThread.Terminate; //FlexCel will check that we set terminated, and exit as fast as it can.
end;

procedure TFCustomPreview.btnPrintCancelClick(Sender: TObject);
begin
  if PrintingThread = nil then //it shouldn't really happen
  begin
    PanelPrinting.Visible := false;
    exit;
  end;
  btnPrintCancel.Enabled := false;
  btnPrintCancel.Text := 'Canceling...';
  PrintingThread.Terminate; //FlexCel will check that we set terminated, and exit as fast as it can.
end;

procedure TFCustomPreview.btnPdfErrorCloseClick(Sender: TObject);
begin
  PanelPdfError.Visible := false;
end;

procedure TFCustomPreview.btnPrintingErrorCloseClick(Sender: TObject);
begin
  PanelPrintingError.Visible := false;
end;

procedure TFCustomPreview.btnPdfOkCloseClick(Sender: TObject);
begin
  PanelPdfOk.Visible := false;
end;

procedure TFCustomPreview.btnPrintOkCloseClick(Sender: TObject);
begin
  PanelPrintingOk.Visible := false;
end;

procedure TFCustomPreview.UpdatePages;
begin
  edPage.Text := IntToStr(MainPreview.StartPage);
  lblTotalPages.Text := 'of ' + IntToStr(MainPreview.TotalPages);
end;

procedure TFCustomPreview.ChangePages;
var
  pn: integer;
begin
  if TryStrToInt(Trim(edPage.Text), pn) then MainPreview.StartPage := pn;
end;

procedure TFCustomPreview.MainPreviewStartPageChanged(Sender: TObject);
begin
  UpdatePages;
end;

procedure TFCustomPreview.edPageExit(Sender: TObject);
begin
  ChangePages;
end;

procedure TFCustomPreview.edPageKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 13 then
  begin
    ChangePages;
    Key := 0;
  end
  else if Key = 27 then
  begin
    UpdatePages;
    Key := 0;
  end;
end;

procedure TFCustomPreview.UpdateZoom;
begin
  ActionZoom.Text := IntToStr(Round(MainPreview.Zoom * 100)) + '%';
  if MainPreview.AutofitPreview = TAutofitPreview.None then UpdateAutofitText;
  ChangingZoom := true;
  try
    TrackBarZoom.Value := Round(MainPreview.Zoom * 100);
  finally
    ChangingZoom := false;
  end;
end;

procedure TFCustomPreview.MainPreviewZoomChanged(Sender: TObject);
begin
  UpdateZoom;
end;

procedure TFCustomPreview.ActionZoomClick(Sender: TObject);
var
  p: TPointF;
begin
  p := TPointF.Create(ActionZoom.Position.Point.X, ActionZoom.Position.Point.Y);
  p.Y := p.Y + ActionZoom.Height;

  PanelZoom.Position.Point := p;
  PanelZoom.Visible := true;
  TrackBarZoom.SetFocus;
end;

procedure TFCustomPreview.btn25Click(Sender: TObject);
begin
  MainPreview.Zoom := 0.25;
  PanelZoom.Visible := false;
end;

procedure TFCustomPreview.btn50Click(Sender: TObject);
begin
  MainPreview.Zoom := 0.50;
  PanelZoom.Visible := false;
end;

procedure TFCustomPreview.btn75Click(Sender: TObject);
begin
  MainPreview.Zoom := 0.75;
  PanelZoom.Visible := false;
end;

procedure TFCustomPreview.btn100Click(Sender: TObject);
begin
  MainPreview.Zoom := 1.0;
  PanelZoom.Visible := false;
end;

procedure TFCustomPreview.btn150Click(Sender: TObject);
begin
  MainPreview.Zoom := 1.5;
  PanelZoom.Visible := false;
end;

procedure TFCustomPreview.TrackBarZoomChange(Sender: TObject);
begin
  if (ChangingZoom) then exit; //avoid recursive calls.
  MainPreview.Zoom := TrackBarZoom.Value / 100.0;
end;

procedure TFCustomPreview.PanelZoomExit(Sender: TObject);
begin
  PanelZoom.Visible := false;
end;

procedure TFCustomPreview.PanelZoomMouseLeave(Sender: TObject);
begin
  PanelZoom.Visible := false;
end;


procedure TFCustomPreview.UpdateAutofitText;
begin
  case MainPreview.AutofitPreview of
    TAutofitPreview.None: ActionAutofit.Text := 'No Autofit';
    TAutofitPreview.Width: ActionAutofit.Text := 'Fit to Width';
    TAutofitPreview.Height: ActionAutofit.Text := 'Fit to Height';
    TAutofitPreview.Full: ActionAutofit.Text := 'Fit to Page';
  end;
end;

procedure TFCustomPreview.ActionAutofitClick(Sender: TObject);
var
  PopPoint: TPointF;
begin
  if not (Sender is TControl) then exit;

  PopPoint.X := (Sender as TControl).Position.X;
  PopPoint.Y := (Sender as TControl).Position.Y + (Sender as TControl).Height;
  PopPoint := ClientToScreen(PopPoint);
  AutofitMenu.Popup(PopPoint.X, PopPoint.Y);
end;


procedure TFCustomPreview.NoAutofit1Click(Sender: TObject);
begin
  MainPreview.AutofitPreview := TAutofitPreview.None;
  UpdateAutofitText;
end;

procedure TFCustomPreview.FittoWidth1Click(Sender: TObject);
begin
  MainPreview.AutofitPreview := TAutofitPreview.Width;
  UpdateAutofitText;
end;

procedure TFCustomPreview.FittoHeight1Click(Sender: TObject);
begin
  MainPreview.AutofitPreview := TAutofitPreview.Height;
  UpdateAutofitText;
end;

procedure TFCustomPreview.FittoPage1Click(Sender: TObject);
begin
  MainPreview.AutofitPreview := TAutofitPreview.Full;
  UpdateAutofitText;
end;

procedure TFCustomPreview.ActionRecalcClick(Sender: TObject);
begin
  Xls.Recalc;
  MainPreview.InvalidatePreview;
end;

procedure TFCustomPreview.ActionGridlinesClick(Sender: TObject);
var
  i: Integer;
  SaveActiveSheet: integer;
begin
  if cbAllSheets.IsChecked then
  begin
    SaveActiveSheet := Xls.ActiveSheet;
    for i := 1 to Xls.SheetCount do
    begin
      Xls.ActiveSheet := i;
      Xls.PrintGridLines := ActionGridLines.IsPressed;
    end;
    Xls.ActiveSheet := SaveActiveSheet;

  end
  else
  begin
    Xls.PrintGridLines := ActionGridLines.IsPressed;
  end;
  MainPreview.InvalidatePreview;
end;


procedure TFCustomPreview.ActionHeadingsClick(Sender: TObject);
var
  i: Integer;
  SaveActiveSheet: integer;
begin
  if cbAllSheets.IsChecked then
  begin
    SaveActiveSheet := Xls.ActiveSheet;
    for i := 1 to Xls.SheetCount do
    begin
      Xls.ActiveSheet := i;
      Xls.PrintHeadings := ActionHeadings.IsPressed;
    end;
    Xls.ActiveSheet := SaveActiveSheet;

  end
  else
  begin
    Xls.PrintHeadings := ActionHeadings.IsPressed;
  end;
  MainPreview.InvalidatePreview;
end;

end.
