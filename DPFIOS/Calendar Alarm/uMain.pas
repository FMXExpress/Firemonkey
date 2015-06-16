unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  System.DateUtils,
  FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Platform, FMX.StdCtrls,
  FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UIButton, DPF.iOS.EKCalendar, DPF.iOS.UILabel, DPF.iOS.UIView;

type
  TForm3 = class( TForm )
    DPFButton1: TDPFButton;
    DPFEKCalendar1: TDPFEKCalendar;
    DPFLabel1: TDPFLabel;
    DPFLabel2: TDPFLabel;
    DPFUIView1: TDPFUIView;
    DPFButton2: TDPFButton;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFEKCalendar1RequestAccess( Sender: TObject; const Granted: Boolean );
    procedure FormCreate( Sender: TObject );
    procedure DPFButton2Click( Sender: TObject );
  protected
    procedure DoShow; override;
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  private
    { Private declarations }
    eventIdentifier: string;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.DoShow;
begin
  inherited;
  SendToBack;
end;

procedure TForm3.DPFButton1Click( Sender: TObject );
begin
  // You can set you timezone
  // if timezone = MAXINT then auto calculate timezone
  DPFLabel2.Text := 'Please wait...';
{$IFDEF IOS}
  DPFEKCalendar1.RequestAccess;
{$ENDIF}
end;

procedure TForm3.DPFButton2Click( Sender: TObject );
begin
  if eventIdentifier = '' then
    exit;
  if DPFEKCalendar1.RemoveCalendarEvent( eventIdentifier ) then
    DPFLabel2.Text := 'Event Removed'
  else
    DPFLabel2.Text := 'Event Not Removed';
end;

procedure TForm3.DPFEKCalendar1RequestAccess( Sender: TObject; const Granted: Boolean );
begin
  if not Granted then
  begin
    DPFLabel2.Text := 'Your App not granted ! ';
    Exit;
  end;

{$IFDEF IOS}
  eventIdentifier := DPFEKCalendar1.AddOrEditCalendarEvent(eventIdentifier, 'D.P.F Calendar Alarm', -10, IncSecond( now, 20 ), IncSecond( now, 60 ), MAXINT );
  if eventIdentifier <> '' then
  begin
    DPFLabel2.Text    := 'Calendar Saved: ' + eventIdentifier;
    DPFLabel1.Visible := True;
  end
  else
    DPFLabel2.Text := 'Calendar Not Saved !';
{$ENDIF}
end;

procedure TForm3.FormCreate( Sender: TObject );
begin
  eventIdentifier := '';
end;

procedure TForm3.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
