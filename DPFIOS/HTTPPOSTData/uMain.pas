unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  DPF.iOS.Common,
  DPF.iOS.HTTP,
  DPF.iOS.UIImageView,
  DPF.iOS.UIButton,
  DPF.iOS.BaseControl,
  DPF.iOS.UITextField, DPF.iOS.UIProgressView, DPF.iOS.UITextView;

type
  TFHTTPPostData = class( TForm )
    DPFHttp1: TDPFHttp;
    DPFButton1: TDPFButton;
    DPFImageView1: TDPFImageView;
    DPFProgress1: TDPFProgress;
    DPFTextView1: TDPFTextView;
    DPFButton2: TDPFButton;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFHttp1ReceiveError( Sender: TObject; Error: string; var isFree: Boolean );
    procedure DPFHttp1SendProgress( Sender: TObject; const SendSize, SendededSize: Int64 );
    procedure DPFHttp1ReceiveData( Sender: TObject; Data: string; var isFree: Boolean );
    procedure DPFButton2Click( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FHTTPPostData: TFHTTPPostData;

implementation

{$R *.fmx}

procedure TFHTTPPostData.DPFButton1Click( Sender: TObject );
begin
  DPFTextView1.Text := '';
  DPFHttp1.PostFiles( 'http://posttestserver.com/post.php', [BuildFormFilesRecord( 'userfile1', 'DPF1.png', 'DPF1.png', 'image/png' ), BuildFormFilesRecord( 'userfile2', 'DPF2.png', GetAppFolder + 'Images/DPF2.png', 'image/png' ), BuildFormFilesRecord( 'userfile3', 'DPF3.png', GetAppFolder + 'Images/DPF3.png', 'image/png' )],
    [BuildFormFieldRecord( 'Field1', 'Value1' ), BuildFormFieldRecord( 'Field2', 'Value2' )], true );
end;

procedure TFHTTPPostData.DPFButton2Click( Sender: TObject );
begin
  DPFHttp1.PostImages( 'http://posttestserver.com/post.php', [BuildFormImageRecord( 'userfile1', 'myFile.png', DPFImageView1.GetUIImage, 'image/jpg' )], [BuildFormFieldRecord( 'Field1', 'Value1' ), BuildFormFieldRecord( 'Field2', 'Value2' )], true );
end;

procedure TFHTTPPostData.DPFHttp1ReceiveData( Sender: TObject; Data: string; var isFree: Boolean );
begin
  DPFTextView1.Text := Data;
  isFree            := false;
end;

procedure TFHTTPPostData.DPFHttp1ReceiveError( Sender: TObject; Error: string; var isFree: Boolean );
begin
  DPFTextView1.Text := DPFTextView1.Text + Error;
  ShowMessage( Error );
end;

procedure TFHTTPPostData.DPFHttp1SendProgress( Sender: TObject; const SendSize, SendededSize: Int64 );
begin
  DPFProgress1.Progress := SendededSize / SendSize;
end;

procedure TFHTTPPostData.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
