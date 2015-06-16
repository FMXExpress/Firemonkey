unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  iOSapi.Foundation,
  iOSapi.CocoaTypes,
  Macapi.ObjectiveC,

  DPF.iOS.iCloud,
  DPF.iOS.Common,
  DPF.iOS.BaseControl,
  DPF.iOS.UIButton,
  DPF.iOS.UILabel,
  DPF.iOS.UIView,
  DPF.iOS.UITextField,
  DPF.iOS.Keyboard,
  DPF.iOS.SlideDialog;

type
  TFiCloud = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFButtonCheckiCloud: TDPFButton;
    DPFiCloud1: TDPFiCloud;
    DPFButtonSetValue: TDPFButton;
    DPFLabel2: TDPFLabel;
    DPFTextField1: TDPFTextField;
    DPFKeyboard1: TDPFKeyboard;
    DPFSlideDialog1: TDPFSlideDialog;
    DPFButtonDelete: TDPFButton;
    DPFButtonSync: TDPFButton;
    procedure DPFButtonCheckiCloudClick( Sender: TObject );
    procedure DPFButtonSetValueClick( Sender: TObject );
    procedure DPFiCloud1KeyChanged( sender: TObject );
    procedure FormShow( Sender: TObject );
    procedure DPFButtonDeleteClick( Sender: TObject );
    procedure DPFButtonSyncClick( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FiCloud: TFiCloud;

implementation

{$R *.fmx}

procedure TFiCloud.DPFButtonCheckiCloudClick( Sender: TObject );
begin
  if DPFiCloud1.iCloudAvailable then
    DPFSlideDialog1.Show( ' iCloud is Available ', 250, 100 )
  else
    DPFSlideDialog1.Show( ' iCloud is not Available ', 250, 100 );
end;

procedure TFiCloud.DPFButtonDeleteClick( Sender: TObject );
begin
  DPFiCloud1.RemoveKey( 'MyKey' );
  DPFLabel2.Text := 'MyKey  = ' + DPFiCloud1.GetValue( 'MyKey', '' );
end;

procedure TFiCloud.DPFButtonSetValueClick( Sender: TObject );
begin
  if not DPFiCloud1.SetValue( 'MyKey', DPFTextField1.Text ) then
    DPFSlideDialog1.Show( 'Can not sync iCloud key value', 200, 100 );

  DPFLabel2.Text := 'MyKey  = ' + DPFiCloud1.GetValue( 'MyKey', '' );
end;

procedure TFiCloud.DPFButtonSyncClick( Sender: TObject );
begin
  DPFiCloud1.Synchronize;
end;

procedure TFiCloud.DPFiCloud1KeyChanged( sender: TObject );
begin
  DPFLabel2.Text := 'MyKey  = ' + DPFiCloud1.GetValue( 'MyKey', '' );
  DPFSlideDialog1.Show( 'iCloud key-value store change detected!', 200, 100 );
end;

procedure TFiCloud.FormShow( Sender: TObject );
begin
  DPFLabel2.Text := 'MyKey  = ' + DPFiCloud1.GetValue( 'MyKey', '' );
end;

procedure TFiCloud.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
