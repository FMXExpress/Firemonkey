unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  DPF.Android.OS,
  DPF.Android.Common,

  DPF.Android.BaseControl,
  DPF.Android.JButton,
  DPF.Android.JAlertDialog,
  DPF.Android.JRelativeLayout,
  DPF.Android.JHTTP, DPF.Android.JProgressDialog, DPF.Android.JTextView,
  DPF.Android.JImageView;

type
  TFHTTPDownload = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJButton1: TDPFJButton;
    DPFJAlertDialog1: TDPFJAlertDialog;
    DPFJProgressDialog1: TDPFJProgressDialog;
    DPFJHTTP1: TDPFJHTTP;
    DPFJImageView1: TDPFJImageView;
    DPFJTextViewMsg: TDPFJTextView;
    procedure DPFJButton1Click( Sender: TObject );
    procedure DPFJHTTP1PregoressUpdate( sender: TObject; Progress: Int64; DownloadSize: Int64; Donloaded: Int64 );
    procedure DPFJProgressDialog1Cancel( sender: TObject );
    procedure DPFJHTTP1Finished( sender: TObject; ReturnCode: TDPFHTTPReturnCode; httpResponseCode: Integer; httpResponseMessage, error: string );
    procedure DPFJHTTP1Cancelled( sender: TObject );
    procedure DPFJHTTP1Started( sender: TObject );
  private
    { Private declarations }

    SaveFileName: string;
  public
    { Public declarations }
  end;

var
  FHTTPDownload: TFHTTPDownload;

implementation

{$R *.fmx}

procedure TFHTTPDownload.DPFJButton1Click( Sender: TObject );
begin
  SaveFileName := GetExternalStorageDirectory + 'test.jpg';
  DPFJHTTP1.StartDownload( 'http://www.free-pictures-photos.com/water/water-stones-q0w9.jpg', SaveFileName, 'Babak', 'Babak' );
  //DPFJHTTP1.StartDownload( 'http://freebigpictures.com/wp-content/uploads/AUTUMN.jpg', SaveFileName, 'Babak', 'Babak' );
  DPFJButton1.Enabled := false;
end;

procedure TFHTTPDownload.DPFJHTTP1Cancelled( sender: TObject );
begin
  DPFJProgressDialog1.Close;
  DPFJTextViewMsg.Text.Text := 'Download canceled by user!';
  DPFJButton1.Enabled       := true;
end;

procedure TFHTTPDownload.DPFJHTTP1Finished( sender: TObject; ReturnCode: TDPFHTTPReturnCode; httpResponseCode: Integer; httpResponseMessage, error: string );
begin
  DPFJProgressDialog1.Close;
  case ReturnCode of
    TDPFHTTPReturnCode.OP_FINISHED:
      begin
        DPFJTextViewMsg.Text.Text := 'Download Completed!';
        DPFJImageView1.FileName   := SaveFileName;
      end;
    TDPFHTTPReturnCode.OP_INTERNAL_ERROR:
      DPFJTextViewMsg.Text.Text := 'Download Error : ' + error;
    TDPFHTTPReturnCode.OP_HTTPERROR:
      DPFJTextViewMsg.Text.Text := 'Download HTTP Error Code : ' + httpResponseCode.ToString + #10#13 + ' Response: ' + httpResponseMessage;
  end;
  DPFJButton1.Enabled := true;
end;

procedure TFHTTPDownload.DPFJHTTP1PregoressUpdate( sender: TObject; Progress: Int64; DownloadSize: Int64; Donloaded: Int64 );
begin
  DPFJProgressDialog1.Progress          := Progress;
  DPFJProgressDialog1.SecondaryProgress := Progress;
end;

procedure TFHTTPDownload.DPFJHTTP1Started( sender: TObject );
begin
  DPFJProgressDialog1.Max      := 100;
  DPFJProgressDialog1.Progress := 0;
  DPFJProgressDialog1.Title    := 'File Downloading';
  DPFJProgressDialog1.Message  := 'Please wait...';
  DPFJProgressDialog1.Show( TDPFProgressDialogStyle.dsHORIZONTAL );
end;

procedure TFHTTPDownload.DPFJProgressDialog1Cancel( sender: TObject );
begin
  DPFJTextViewMsg.Text.Text := 'Canceling...';
  DPFJButton1.Enabled := false;
  DPFJHTTP1.CancelTask;
end;

end.
