
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit Flyouts;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Menus, FMX.Layouts,
  FMX.Objects, FMX.Grid, FMX.Gestures, FMX.Ani, FMX.Edit, FMX.ListBox,
  FMX.ExtCtrls, FMX.StdCtrls, FMX.DateTimeCtrls;

type
  TFlyoutDemoForm = class(TForm)
    ExitFlyout: TPopup;
    Panel1: TPanel;
    Layout1: TLayout;
    ReallyCloseButton: TButton;
    ExitFlyoutAnimation: TFloatAnimation;
    CheckCalendarButton: TButton;
    CalendarFlyout: TPopup;
    SampleFlyoutPanel: TPanel;
    CalendarCloseButton: TButton;
    Layout2: TLayout;
    Calendar1: TCalendar;
    CalendarFlyoutAnimation: TFloatAnimation;
    DummyLayout: TLayout;
    FooterLayout: TLayout;
    CloseAppButton: TButton;
    TitleLabel: TLabel;
    CalendarTitleLabel: TLabel;
    ExitFlyoutLabel: TLabel;
    StyleBook1: TStyleBook;
    procedure CloseAppButtonClick(Sender: TObject);
    procedure ReallyCloseButtonClick(Sender: TObject);
    procedure CalendarCloseButtonClick(Sender: TObject);
    procedure CheckCalendarButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure ShowExitFlyout(AShow: Boolean);
    procedure ShowCalendarFlyout(AShow: Boolean);
  public
    { Public declarations }
  end;

var
  FlyoutDemoForm: TFlyoutDemoForm;

implementation

{$R *.fmx}

procedure TFlyoutDemoForm.CloseAppButtonClick(Sender: TObject);
begin
  ShowExitFlyout(True);
end;

procedure TFlyoutDemoForm.CheckCalendarButtonClick(Sender: TObject);
begin
  ShowCalendarFlyout(True);
end;

procedure TFlyoutDemoForm.CalendarCloseButtonClick(Sender: TObject);
begin
  ShowCalendarFlyout(False);
end;

procedure TFlyoutDemoForm.ReallyCloseButtonClick(Sender: TObject);
begin
  Application.Terminate;
end;


procedure TFlyoutDemoForm.ShowExitFlyout(AShow: Boolean);
begin
  ExitFlyoutAnimation.StartValue := ClientWidth;
  ExitFlyoutAnimation.StopValue :=
    Width/2 - ExitFlyout.Width/2;
  ExitFlyout.IsOpen := AShow;
end;

procedure TFlyoutDemoForm.ShowCalendarFlyout(AShow: Boolean);
begin
  CalendarFlyoutAnimation.StartValue := ClientWidth;
  CalendarFlyoutAnimation.StopValue :=
    Width/2 - CalendarFlyout.Width/2;
  CalendarFlyout.IsOpen := AShow;
end;


end.
