// ------------------------------------------------------------------------
//
// Thanks to Diego Molina for this Demo
//
// ------------------------------------------------------------------------
unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.UIView,
  DPF.iOS.BaseControl, Unit1, Unit2;

type
  TfMain = class( TForm )
    MainView: TDPFUIView;
    Frame1: TDPFUIView;
    Frame2: TDPFUIView;
    procedure ShowFormWithAnimation( MainFormView: TDPFUIView; FormToShow: TForm; Animation: DPF.iOS.BaseControl.TDPFViewAnimationTransition = vatNone; Duration: double = 0.5 );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.fmx}

procedure TfMain.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

procedure TfMain.ShowFormWithAnimation( MainFormView: TDPFUIView; FormToShow: TForm; Animation: DPF.iOS.BaseControl.TDPFViewAnimationTransition = vatNone; Duration: double = 0.5 );
begin
  MainFormView.SetAnimationTransition( Animation, Duration );
  MainFormView.Form := nil;
  MainFormView.Form := FormToShow;
end;

end.
