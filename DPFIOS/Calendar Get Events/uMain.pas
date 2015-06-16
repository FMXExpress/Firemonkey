unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Platform, FMX.StdCtrls,
  FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UIButton, DPF.iOS.EKCalendar, DPF.iOS.UITextView, DPF.iOS.UIView;

type
  TForm3 = class( TForm )
    DPFEKCalendar1: TDPFEKCalendar;
    DPFButton2: TDPFButton;
    DPFTextView1: TDPFTextView;
    DPFUIView1: TDPFUIView;
    procedure DPFButton2Click( Sender: TObject );
    procedure DPFEKCalendar1RequestAccess( Sender: TObject; const Granted: Boolean );
  private
    { Private declarations }

  protected
    procedure DoShow; override;
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

// ------------------------------------------------------------------------------
procedure TForm3.DoShow;
begin
  inherited;
  SendToBack;
end;

procedure TForm3.DPFButton2Click( Sender: TObject );
var
  Events: TArrayOfCalendarEvent;
  I     : Integer;
begin
  DPFTextView1.Text := '';
{$IFDEF IOS}
  DPFEKCalendar1.RequestAccess;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TForm3.DPFEKCalendar1RequestAccess( Sender: TObject; const Granted: Boolean );
var
  Events: TArrayOfCalendarEvent;
  I     : Integer;
begin
  if not Granted then
  begin
    ShowMessage( 'Go to Setting->Privacy->Calendars and switch on this application' );
    Exit;
  end;

{$IFDEF IOS}
  Events := DPFEKCalendar1.GetAllEvents( Now - 1000, Now + 3600 );
  for I  := 0 to high( Events ) do
  begin
    DPFTextView1.Text := DPFTextView1.Text + Events[I].Title + ' Start: ' + DateToStr( Events[I].StartDate ) + ' End: ' + DateToStr( Events[I].EndDate ) + #13 + #10;
  end;
{$ENDIF}
end;

procedure TForm3.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

// ------------------------------------------------------------------------------
end.
