unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  DPF.iOS.BaseControl,
  DPF.iOS.UITextField,
  DPF.iOS.UIScrollView,
  DPF.iOS.UIView,
  DPF.iOS.Keyboard,
  DPF.iOS.UITextView;

type
  TForm1 = class( TForm )
    DPFUIScrollView1: TDPFUIScrollView;
    DPFTextField1: TDPFTextField;
    DPFTextField2: TDPFTextField;
    DPFTextField3: TDPFTextField;
    DPFKeyboard1: TDPFKeyboard;
    DPFTextView1: TDPFTextView;

    procedure DPFKeyboard1KeyBoardWillHide( Sender: TObject; const Bounds: TRectF );
    procedure DPFKeyboard1KeyBoardWillShow( Sender: TObject; const Bounds: TRectF );
    procedure DPFTextField1BeginEditing( Sender: TObject; isBeginEditing: Boolean );
    procedure FormShow( Sender: TObject );
  private
    { Private declarations }

    FKBBounds     : TRectF;
    FocusedControl: TControl;

  protected
    { Protected declarations }
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

procedure TForm1.DPFKeyboard1KeyBoardWillHide( Sender: TObject; const Bounds: TRectF );
begin
  FKBBounds.Create( 0, 0, 0, 0 );
  DPFUIScrollView1.RestoreFromFocused( FKBBounds );
end;

procedure TForm1.DPFKeyboard1KeyBoardWillShow( Sender: TObject; const Bounds: TRectF );
begin
  Application.ProcessMessages;
  FKBBounds             := TRectF.Create( Bounds );
  FKBBounds.TopLeft     := ScreenToClient( FKBBounds.TopLeft );
  FKBBounds.BottomRight := ScreenToClient( FKBBounds.BottomRight );

  DPFUIScrollView1.ScrollToFocused( FocusedControl, FKBBounds, 0 );
end;

procedure TForm1.DPFTextField1BeginEditing( Sender: TObject; isBeginEditing: Boolean );
begin
  FocusedControl := Sender as TControl;
  DPFUIScrollView1.ScrollToControl( Sender as TDPFiOSBaseControl );
end;

procedure TForm1.FormShow( Sender: TObject );
begin
  DPFUIScrollView1.Loaded;
end;

end.
