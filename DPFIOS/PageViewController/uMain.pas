unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.UIActionSheet,
  DPF.iOS.UIButton, DPF.iOS.UIPageViewController, DPF.iOS.BaseControl,
  DPF.iOS.UIViewController, DPF.iOS.UIView, DPF.iOS.UILabel, FMX.StdCtrls;

type
  TFNavigationControllerForm = class( TForm )
    DPFUIPageViewController1: TDPFUIPageViewController;
    DPFUIViewController1: TDPFUIViewController;
    DPFUIViewController2: TDPFUIViewController;
    DPFButton1: TDPFButton;
    DPFButton2: TDPFButton;
    DPFUIViewController3: TDPFUIViewController;
    DPFUIViewController4: TDPFUIViewController;
    DPFButton3: TDPFButton;
    DPFLabel1: TDPFLabel;
    DPFButton4: TDPFButton;
    procedure DPFUIPageViewController1PageChanged( Sender: TObject; PrevPageIndex, NewPageIndex: Integer );
    procedure DPFButton4Click( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FNavigationControllerForm: TFNavigationControllerForm;

implementation

{$R *.fmx}
{ TFNavigationController }

procedure TFNavigationControllerForm.DPFButton4Click( Sender: TObject );
begin
  DPFUIPageViewController1.ActivePageIndex := 1;
end;

procedure TFNavigationControllerForm.DPFUIPageViewController1PageChanged( Sender: TObject; PrevPageIndex, NewPageIndex: Integer );
begin
  DPFLabel1.Text := 'Page Changed From: ' + IntToStr( PrevPageIndex ) + ' to ' + IntToStr( NewPageIndex );
end;

procedure TFNavigationControllerForm.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
