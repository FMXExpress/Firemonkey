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
  TFHTTPUpload = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJButton1: TDPFJButton;
    DPFJAlertDialog1: TDPFJAlertDialog;
    DPFJProgressDialog1: TDPFJProgressDialog;
    DPFJHTTP1: TDPFJHTTP;
    procedure DPFJButton1Click( Sender: TObject );
    procedure DPFJHTTP1PregoressUpdate( sender: TObject; Progress: Int64; DownloadSize: Int64; Donloaded: Int64 );
    procedure DPFJProgressDialog1Cancel( sender: TObject );
    procedure DPFJHTTP1Finished( sender: TObject; ReturnCode: TDPFHTTPReturnCode; httpResponseCode: Integer; httpResponseMessage, error: string );
    procedure DPFJHTTP1Cancelled( sender: TObject );
    procedure DPFJHTTP1Started( sender: TObject );
  private
    { Private declarations }

  public
    { Public declarations }
  end;

var
  FHTTPUpload: TFHTTPUpload;

implementation

{$R *.fmx}

procedure TFHTTPUpload.DPFJButton1Click( Sender: TObject );
begin
  DPFJHTTP1.StartUpload( GetAppFolder + 'DPF3.png', 'http://www.cebitco.com/1/getUpload.php', [BuildHeaderRecord( 'a', 'b' )], [BuildFormFieldRecord( 'Field1', 'Value1' ), BuildFormFieldRecord( 'Field2', 'Value2' )] );
  //DPFJHTTP1.StartUpload( GetAppFolder + 'DPF3.png', 'https://mail.dpfaragir.com:4433/getUpload.php', [BuildHeaderRecord( 'a', 'b' )], [BuildFormFieldRecord( 'Field1', 'Value1' ), BuildFormFieldRecord( 'Field2', 'Value2' )] );
end;

procedure TFHTTPUpload.DPFJHTTP1Cancelled( sender: TObject );
begin
  DPFJProgressDialog1.Close;
  DPFJAlertDialog1.ShowMessageDialog( 'Information', 'Upload canceled by user!', ['Close'] );
end;

procedure TFHTTPUpload.DPFJHTTP1Finished( sender: TObject; ReturnCode: TDPFHTTPReturnCode; httpResponseCode: Integer; httpResponseMessage, error: string );
begin
  DPFJProgressDialog1.Close;
  case ReturnCode of
    TDPFHTTPReturnCode.OP_FINISHED:
      begin
        DPFJAlertDialog1.ShowMessageDialog( 'Information', 'Upload Completed!', ['Close'] );

      end;
    TDPFHTTPReturnCode.OP_INTERNAL_ERROR:
      DPFJAlertDialog1.ShowMessageDialog( 'Information', 'Upload Error : ' + error, ['Close'] );
    TDPFHTTPReturnCode.OP_HTTPERROR:
      DPFJAlertDialog1.ShowMessageDialog( 'Information', 'Upload HTTP Error Code : ' + httpResponseCode.ToString + #10#13 + ' Response: ' + httpResponseMessage, ['Close'] );
  end;
end;

procedure TFHTTPUpload.DPFJHTTP1PregoressUpdate( sender: TObject; Progress: Int64; DownloadSize: Int64; Donloaded: Int64 );
begin
  DPFJProgressDialog1.Progress          := Progress;
  DPFJProgressDialog1.SecondaryProgress := Progress;
end;

procedure TFHTTPUpload.DPFJHTTP1Started( sender: TObject );
begin
  DPFJProgressDialog1.Max      := 100;
  DPFJProgressDialog1.Progress := 0;
  DPFJProgressDialog1.Title    := 'File Uploading';
  DPFJProgressDialog1.Message  := 'Please wait...';
  DPFJProgressDialog1.Show( TDPFProgressDialogStyle.dsHORIZONTAL );
end;

procedure TFHTTPUpload.DPFJProgressDialog1Cancel( sender: TObject );
begin
  DPFJHTTP1.CancelTask;
end;

end.
