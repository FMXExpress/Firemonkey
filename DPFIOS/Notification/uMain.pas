unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  System.DateUtils,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,

  DPF.iOS.BaseControl,
  DPF.iOS.UIView,
  DPF.iOS.UILocalNotification,
  DPF.iOS.UIButton,
  DPF.iOS.ApplicationManager,
  DPF.iOS.UILabel;

type
  TFNotification = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFApplicationManager1: TDPFApplicationManager;
    DPFLabel1: TDPFLabel;
    procedure DPFApplicationManager1EnteredBackground( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FNotification: TFNotification;

implementation

{$R *.fmx}

procedure TFNotification.DPFApplicationManager1EnteredBackground( Sender: TObject );
begin
  MakeNotification( 'Hello! D.P.F iOS Native Component Notification Demo', IncSecond( Now, 5 ) );
end;

procedure TFNotification.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
