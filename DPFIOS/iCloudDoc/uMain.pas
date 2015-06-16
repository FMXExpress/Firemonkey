unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  iOSapi.Foundation,
  iOSapi.CocoaTypes,
  Macapi.ObjectiveC,

  DPF.iOS.iCloudDoc,
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
    DPFKeyboard1: TDPFKeyboard;
    DPFSlideDialog1: TDPFSlideDialog;
    DPFButtonSync: TDPFButton;
    DPFiCloudDoc1: TDPFiCloudDoc;
    procedure DPFButtonCheckiCloudClick( Sender: TObject );
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
  if DPFiCloudDoc1.iCloudAvailable then
    DPFSlideDialog1.Show( ' iCloud is Available ', 250, 100 )
  else
    DPFSlideDialog1.Show( ' iCloud is not Available ', 250, 100 );
end;

procedure TFiCloud.DPFButtonSyncClick( Sender: TObject );
begin
  DPFiCloudDoc1.LoadDocument;
end;

procedure TFiCloud.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
