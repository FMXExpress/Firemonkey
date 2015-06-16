unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UIView, DPF.iOS.UIToolbar, DPF.iOS.SLComposeViewController,
  DPF.iOS.UILabel, DPF.iOS.UISwitch;

type
  TFSLComposeView = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFSLComposeViewController1: TDPFSLComposeViewController;
    DPFToolbar1: TDPFToolbar;
    DPFSwitchF: TDPFSwitch;
    DPFLabelFaceBook: TDPFLabel;
    DPFSwitchT: TDPFSwitch;
    DPFLabel1: TDPFLabel;
    DPFSwitchS: TDPFSwitch;
    DPFLabel2: TDPFLabel;
    procedure DPFToolbar1BarItems0Click( Sender: TObject );
    procedure DPFToolbar1BarItems2Click( Sender: TObject );
    procedure DPFToolbar1BarItems4Click( Sender: TObject );
    procedure FormShow( Sender: TObject );
    procedure DPFSLComposeViewController1ComposeEvent(Sender: TObject;
      Result: Integer);
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FSLComposeView: TFSLComposeView;

implementation

{$R *.fmx}

procedure TFSLComposeView.DPFSLComposeViewController1ComposeEvent(
  Sender: TObject; Result: Integer);
begin
{}
end;

procedure TFSLComposeView.DPFToolbar1BarItems0Click( Sender: TObject );
begin
  if not DPFSLComposeViewController1.FacebookPost( 'iOS 8 Social Framework test !', 'http://www.dpfaragir.com/', '/Documents/admin.png', false ) then
//    ShowMessage( 'Facebook not available !' + #10#13 + 'Please setup it' );
end;

procedure TFSLComposeView.DPFToolbar1BarItems2Click( Sender: TObject );
begin
  if not DPFSLComposeViewController1.TwitterPost( 'iOS 8 Social Framework test !', 'http://www.dpfaragir.com/', '/Documents/admin.png', false ) then
//    ShowMessage( 'Twitter not available !' + #10#13 + 'Please setup it' );
end;

procedure TFSLComposeView.DPFToolbar1BarItems4Click( Sender: TObject );
begin
  if not DPFSLComposeViewController1.SinaWeiboPost( 'iOS 8 Social Framework test !', 'http://www.dpfaragir.com/', '/Documents/admin.png', false ) then
//    ShowMessage( 'SinaWeibo not available !' + #10#13 + 'Please setup it' );
end;

procedure TFSLComposeView.FormShow( Sender: TObject );
begin
  DPFSwitchF.ISON := DPFSLComposeViewController1.FacebookAvailable;
  DPFSwitchT.ISON := DPFSLComposeViewController1.TwitterAvailable;
  DPFSwitchS.ISON := DPFSLComposeViewController1.SinaWeiboAvailable;

  DPFSwitchF.Enabled := false;
  DPFSwitchT.Enabled := false;
  DPFSwitchS.Enabled := false;
end;

procedure TFSLComposeView.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
