unit UPDFViewer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.TMSNativePDFThumbnailView, FMX.TMSNativeNSBaseControl,
  FMX.TMSNativePDFView, FMX.TMSNativeNSPopover, FMX.TMSNativeNSButton,
  FMX.TMSNativeNSView, FMX.TMSNativeNSTextField, FMX.TMSNativeNSLabel;

type
  TForm1087 = class(TForm)
    TMSFMXNativeNSButton1: TTMSFMXNativeNSButton;
    TMSFMXNativePDFThumbnailView1: TTMSFMXNativePDFThumbnailView;
    TMSFMXNativeNSPopover1: TTMSFMXNativeNSPopover;
    TMSFMXNativeNSView1: TTMSFMXNativeNSView;
    TMSFMXNativeNSButton2: TTMSFMXNativeNSButton;
    TMSFMXNativeNSButton3: TTMSFMXNativeNSButton;
    TMSFMXNativePDFView1: TTMSFMXNativePDFView;
    TMSFMXNativeNSLabel1: TTMSFMXNativeNSLabel;
    TMSFMXNativeNSLabel2: TTMSFMXNativeNSLabel;
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXNativeNSButton1Click(Sender: TObject);
    procedure TMSFMXNativeNSButton3Click(Sender: TObject);
    procedure TMSFMXNativeNSButton2Click(Sender: TObject);
    procedure TMSFMXNativePDFView1PageChanged(Sender: TObject;
      APageIndex: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateButtons;
    procedure UpdateLabel;
  end;

var
  Form1087: TForm1087;

implementation

{$R *.fmx}

procedure TForm1087.FormCreate(Sender: TObject);
begin
  TMSFMXNativePDFView1.Location := ExtractFilePath(Paramstr(0)) + 'Paris.pdf';
  TMSFMXNativePDFThumbnailView1.PDFView := TMSFMXNativePDFView1;
  UpdateButtons;
  UpdateLabel;
end;

procedure TForm1087.TMSFMXNativeNSButton1Click(Sender: TObject);
begin
  TMSFMXNativeNSPopover1.ShowFromControl(TMSFMXNativeNSButton1);
end;

procedure TForm1087.TMSFMXNativeNSButton2Click(Sender: TObject);
begin
  TMSFMXNativePDFView1.GoToPreviousPage;
  UpdateButtons;
end;

procedure TForm1087.TMSFMXNativeNSButton3Click(Sender: TObject);
begin
  TMSFMXNativePDFView1.GoToNextPage;
  UpdateButtons;
end;

procedure TForm1087.TMSFMXNativePDFView1PageChanged(Sender: TObject;
  APageIndex: Integer);
begin
  UpdateLabel;
  UpdateButtons;
end;

procedure TForm1087.UpdateButtons;
begin
  TMSFMXNativeNSButton2.Enabled := TMSFMXNativePDFView1.CanGoToPreviousPage;
  TMSFMXNativeNSButton3.Enabled := TMSFMXNativePDFView1.CanGoToNextPage;
end;

procedure TForm1087.UpdateLabel;
begin
  TMSFMXNativeNSLabel1.Text := 'Page ' + inttostr(TMSFMXNativePDFView1.PageIndex + 1) + ' / ' + inttostr(TMSFMXNativePDFView1.PageCount);
end;

end.
