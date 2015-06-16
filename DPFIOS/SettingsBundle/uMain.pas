(* *************************************************************** *)
(* Deploy Root.plist to .\Settings.bundle\ *)
(* *************************************************************** *)

unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  DPF.iOS.Common,
  DPF.iOS.BaseControl,
  DPF.iOS.UIButton,
  DPF.iOS.UILabel,
  DPF.iOS.UIView,
  DPF.iOS.NSUserDefaults,
  DPF.iOS.UISwitch;

type
  TFSettingsBundle = class( TForm )
    DPFLabel1: TDPFLabel;
    te: TDPFUIView;
    DPFUserDefaults1: TDPFUserDefaults;
    DPFLabel2: TDPFLabel;
    DPFSwitch1: TDPFSwitch;
    DPFLabel3: TDPFLabel;
    procedure DPFUserDefaults1UserDefaultsChanged( sender: TObject );
    procedure FormShow( Sender: TObject );
    procedure DPFSwitch1Changed( Sender: TObject; ISON: Boolean );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FSettingsBundle: TFSettingsBundle;

implementation

{$R *.fmx}

procedure TFSettingsBundle.DPFSwitch1Changed( Sender: TObject; ISON: Boolean );
begin
  DPFUserDefaults1.SetBooleanValue( 'Enabled_preference', DPFSwitch1.ISON );
end;

procedure TFSettingsBundle.DPFUserDefaults1UserDefaultsChanged( sender: TObject );
begin
  DPFLabel2.Text  := 'User Defaults Changed !';
  DPFSwitch1.ISON := DPFUserDefaults1.GetBooleanValue( 'Enabled_preference' );
end;

procedure TFSettingsBundle.FormShow( Sender: TObject );
begin
  DPFSwitch1.ISON := DPFUserDefaults1.GetBooleanValue( 'Enabled_preference' );
  DPFUserDefaults1.SetStringValue( 'version_preference', GetAppVersion );
end;

procedure TFSettingsBundle.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
