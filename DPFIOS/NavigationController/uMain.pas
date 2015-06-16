unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  System.Math,
  System.Sensors,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  DPF.iOS.BaseControl,
  DPF.iOS.UINavigationController,
  DPF.iOS.UIView,
  DPF.iOS.UIViewController,
  DPF.iOS.UIButton,
  DPF.iOS.UIActionSheet;

type
  TFNavigationControllerForm = class( TForm )
    DPFNavigationController1: TDPFNavigationController;
    DPFButtonPushNext: TDPFButton;
    DPFUIViewMain: TDPFUIView;
    DPFUIActionSheet1: TDPFUIActionSheet;
    DPFButton2: TDPFButton;
    DPFButton3: TDPFButton;
    DPFButton4: TDPFButton;
    DPFNavigationControllerPage0: TDPFNavigationControllerPage;
    DPFNavigationControllerPage1: TDPFNavigationControllerPage;
    DPFNavigationControllerPage2: TDPFNavigationControllerPage;
    DPFButton5: TDPFButton;
    procedure DPFButtonPushNextClick( Sender: TObject );
    procedure DPFButton2Click( Sender: TObject );
    procedure DPFButton3Click( Sender: TObject );
    procedure DPFButton4Click( Sender: TObject );

    procedure DPFNavigationController1BarItems0Click( Sender: TObject );
    procedure DPFNavigationController1BarItems1Click( Sender: TObject );
    procedure DPFNavigationController1BarItems2Click( Sender: TObject );
    procedure DPFNavigationController1BarItems3Click( Sender: TObject );
    procedure FormShow( Sender: TObject );
    procedure DPFNavigationControllerItem2BarButtons0Click( Sender: TObject );
    procedure DPFNavigationControllerItem2BarButtons1Click( Sender: TObject );
    procedure DPFNavigationControllerPage0ToolBarButtons0Click( Sender: TObject );
    procedure DPFNavigationControllerPage2ToolBarButtons1Click( Sender: TObject );
    procedure DPFNavigationControllerPage1ToolBarButtons1Click( Sender: TObject );
    procedure DPFButton5Click( Sender: TObject );
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

procedure TFNavigationControllerForm.DPFButtonPushNextClick( Sender: TObject );
begin
  // DPFNavigationController1.SetAnimationTransition( vatUp, 1 );
  DPFNavigationController1.PushViewController( DPFNavigationControllerPage1, false );
end;

procedure TFNavigationControllerForm.DPFButton2Click( Sender: TObject );
begin
  DPFNavigationController1.PopViewController( DPFNavigationControllerPage0, false );
  DPFNavigationController1.SetAnimationTransition( vatFlipFromLeft, 1 );
end;

procedure TFNavigationControllerForm.DPFButton3Click( Sender: TObject );
begin
  DPFNavigationController1.PresentViewController( DPFNavigationControllerPage1, mtsCrossDissolve, true );
end;

procedure TFNavigationControllerForm.DPFButton5Click( Sender: TObject );
begin
  DPFNavigationController1.PresentViewController( DPFNavigationControllerPage1, mtsPartialCurl, true );
end;

procedure TFNavigationControllerForm.DPFButton4Click( Sender: TObject );
begin
  DPFNavigationController1.DismissViewController( True );
end;

procedure TFNavigationControllerForm.DPFNavigationController1BarItems0Click( Sender: TObject );
begin
  DPFNavigationController1.PushViewController( DPFNavigationControllerPage1, true );
end;

procedure TFNavigationControllerForm.DPFNavigationController1BarItems1Click( Sender: TObject );
begin
  DPFUIActionSheet1.ShowMessage( 'Clicked !', ['Ok'] );
end;

procedure TFNavigationControllerForm.DPFNavigationController1BarItems2Click( Sender: TObject );
begin
  // TDPFNavigationBarButtonItem( Sender ).PageView.SetAnimationTransition( vatDown );
  DPFNavigationController1.BarColor                 := Randomrange( 1, TAlphaColors.Yellowgreen );
  TDPFNavigationBarButtonItem( Sender ).ButtonColor := DPFNavigationController1.BarColor;
  TDPFNavigationBarButtonItem( Sender ).Page.BackgroundColor := DPFNavigationController1.BarColor;
end;

procedure TFNavigationControllerForm.DPFNavigationController1BarItems3Click( Sender: TObject );
begin
  if TDPFNavigationBarButtonItem( Sender ).ButtonSystemItem = bbsiDone then
    TDPFNavigationBarButtonItem( Sender ).ButtonSystemItem := bbsiSave
  else
    TDPFNavigationBarButtonItem( Sender ).ButtonSystemItem := bbsiDone;
end;

procedure TFNavigationControllerForm.DPFNavigationControllerItem2BarButtons0Click( Sender: TObject );
begin
  DPFNavigationController1.PopViewController( DPFNavigationControllerPage0, true );
end;

procedure TFNavigationControllerForm.DPFNavigationControllerItem2BarButtons1Click( Sender: TObject );
begin
  DPFNavigationController1.PushViewController( DPFNavigationControllerPage2, true );
end;

procedure TFNavigationControllerForm.DPFNavigationControllerPage0ToolBarButtons0Click( Sender: TObject );
begin
  ShowMessage( 'Toolbar Button Clicked!' );
end;

procedure TFNavigationControllerForm.DPFNavigationControllerPage1ToolBarButtons1Click( Sender: TObject );
begin
  ShowMessage( 'Compose button' );
end;

procedure TFNavigationControllerForm.DPFNavigationControllerPage2ToolBarButtons1Click( Sender: TObject );
begin
  ShowMessage( 'Centered Clicked!' );
end;

procedure TFNavigationControllerForm.FormShow( Sender: TObject );
begin
  Randomize;
  DPFUIViewMain.Loaded;
end;

procedure TFNavigationControllerForm.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
