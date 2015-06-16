unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  DPF.iOS.Common,
  DPF.iOS.BaseControl,
  DPF.iOS.UIButton,
  DPF.iOS.UIAlertView,
  DPF.iOS.UIView,
  DPF.iOS.UIViewController,
  FMX.InAppPurchase,
  DPF.iOS.StoreKit;

type
  TFAlertView = class( TForm )
    DPFAlertView1: TDPFAlertView;
    DPFButton1: TDPFButton;
    DPFButton2: TDPFButton;
    DPFButton3: TDPFButton;
    DPFButton4: TDPFButton;
    DPFButton5: TDPFButton;
    DPFUIViewController1: TDPFUIViewController;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFButton2Click( Sender: TObject );
    procedure DPFButton3Click( Sender: TObject );
    procedure DPFButton4Click( Sender: TObject );
    procedure DPFButton5Click( Sender: TObject );
    procedure DPFAlertView1Show( Sender: TObject; var InoutFocusedIndex: Integer );
    procedure FormShow( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FAlertView: TFAlertView;

implementation

{$R *.fmx}

// ------------------------------------------------------------------------------
procedure TFAlertView.DPFAlertView1Show( Sender: TObject; var InoutFocusedIndex: Integer );
begin
  InoutFocusedIndex := 1;
end;

procedure TFAlertView.DPFButton1Click( Sender: TObject );
begin
  DPFAlertView1.ViewStyle := TDPFAlertViewStyle.avsDefault;
  ShowMessage( 'Clicked Index : ' + IntToStr( DPFAlertView1.ShowMessageDialog( 'This is custom Title', 'My Custom Message', ['Yes', 'No'], 1, [], [] ) ) );
end;

// ------------------------------------------------------------------------------
procedure TFAlertView.DPFButton2Click( Sender: TObject );
begin
  DPFAlertView1.ViewStyle := TDPFAlertViewStyle.avsSecureTextInput;
  ShowMessage( 'Clicked Index : ' + IntToStr( DPFAlertView1.ShowMessageDialog( 'Secure Text Input', 'My Custom Message', ['Cancel', 'Ok'], 1, [], [] ) ) );
end;

procedure TFAlertView.DPFButton3Click( Sender: TObject );
begin
  DPFAlertView1.ViewStyle := TDPFAlertViewStyle.avsPlainTextInput;
  ShowMessage( 'Clicked Index : ' + IntToStr( DPFAlertView1.ShowMessageDialog( 'Plain Text Input', 'My Custom Message', ['Cancel', 'Ok'], 1, [], ['Type something here !'] ) ) );
end;

procedure TFAlertView.DPFButton4Click( Sender: TObject );
var
  idx: Integer;
begin
  DPFAlertView1.ViewStyle := TDPFAlertViewStyle.avsLoginAndPasswordInput;
  idx                     := DPFAlertView1.ShowMessageDialog( 'Login And Password Input', 'My Custom Message', ['Login', 'Cancel'], 1, [], [] );
  ShowMessage( 'User: ' + DPFAlertView1.ResultTexts[0] + ', Password: ' + DPFAlertView1.ResultTexts[1] );
end;

procedure TFAlertView.DPFButton5Click( Sender: TObject );
begin
  DPFAlertView1.ViewStyle := TDPFAlertViewStyle.avsPlainTextInput;
  ShowMessage( 'Clicked Index : ' + IntToStr( DPFAlertView1.ShowMessageDialog( 'Plain Text Input', 'My Custom Message', ['Cancel', 'Ok'], 1, ['Hello, Pre-Defined Text !'], [] ) ) );
end;

procedure TFAlertView.FormShow( Sender: TObject );
begin
//  DPFUIViewController1.Loaded;
end;

procedure TFAlertView.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
