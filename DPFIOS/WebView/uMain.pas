unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.UIView,
  DPF.iOS.BaseControl, DPF.iOS.UIWebView, DPF.iOS.UIButton, DPF.iOS.UITextField,
  DPF.iOS.Common,
  DPF.iOS.UIViewController, DPF.iOS.UITabBarController;

type
  TFWebView = class( TForm )
    DPFWeb1: TDPFWeb;
    DPFUIView1: TDPFUIView;
    DPFTextField1: TDPFTextField;
    DPFButton1: TDPFButton;
    DPFTabBarController1: TDPFTabBarController;
    DPFTabBarItem1: TDPFTabBarItem;
    DPFTabBarItem2: TDPFTabBarItem;
    DPFUIViewToolBar: TDPFUIView;
    DPFWebLoadFromString: TDPFWeb;
    DPFTabBarItem3: TDPFTabBarItem;
    DPFWebRTF: TDPFWeb;
    procedure DPFButton1Click( Sender: TObject );
    procedure FormShow( Sender: TObject );
    procedure DPFWebLoadFromStringRequest( sender: TObject; navigationType: TDPFUIWebViewNavigationType; RequestURL: string; var ShouldStart: Boolean );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FWebView: TFWebView;

implementation

{$R *.fmx}

procedure TFWebView.DPFButton1Click( Sender: TObject );
begin
  DPFWeb1.LoadFromURL( DPFTextField1.text );
end;

procedure TFWebView.DPFWebLoadFromStringRequest( sender: TObject; navigationType: TDPFUIWebViewNavigationType; RequestURL: string; var ShouldStart: Boolean );
begin
  ShouldStart := MessageDlg( 'Request on link. ' + #10#13 + RequestURL + #10#13 + 'Continue ?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0 ) = mrYes;

end;

procedure TFWebView.FormShow( Sender: TObject );
begin
  DPFWebLoadFromString.LoadFromString( ' <html><body><h2><center>TDPFWeb LoadFromString Example<center></h2><br/><hr/><br/><a href="http://www.dpfaragir.com"> D.P.F Delphi Native iOS Components</a></body></html>' );
  DPFWebRTF.LoadFromFile( GetAppFolder + '/Documents/document.rtf', 'application/rtf', 'utf-8' );
end;

procedure TFWebView.PaintRects( const UpdateRects: array of TRectF );
begin

  { }
end;

end.
