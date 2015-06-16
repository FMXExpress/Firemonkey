unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  DPF.iOS.BaseControl,
  DPF.iOS.UIView,
  DPF.iOS.UITextField,
  DPF.iOS.UITextView,
  DPF.iOS.UIScrollView,
  DPF.iOS.UIButton,
  DPF.iOS.Keyboard,
  DPF.iOS.UILabel;

type
  TFTextField = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFKeyboard1: TDPFKeyboard;
    DPFTextField1: TDPFTextField;
    DPFButton1: TDPFButton;
    DPFTextField2: TDPFTextField;
    DPFLabel1: TDPFLabel;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFKeyboard1KeyBoardWillChange( Sender: TObject );
    procedure DPFKeyboard1KeyBoardWillHide( Sender: TObject; const Bounds: TRectF );
    procedure DPFKeyboard1KeyBoardWillShow( Sender: TObject; const Bounds: TRectF );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FTextField: TFTextField;

implementation

{$R *.fmx}

procedure TFTextField.DPFButton1Click( Sender: TObject );
begin
  DPFKeyboard1.ShowVirtualKeyboard( DPFTextField2 );
end;

procedure TFTextField.DPFKeyboard1KeyBoardWillChange( Sender: TObject );
begin
  DPFLabel1.Text := 'KeyBoardWillChange Event!';
end;

procedure TFTextField.DPFKeyboard1KeyBoardWillHide( Sender: TObject; const Bounds: TRectF );
begin
  DPFLabel1.Text := 'KeyBoardWillHide Event!';
end;

procedure TFTextField.DPFKeyboard1KeyBoardWillShow( Sender: TObject; const Bounds: TRectF );
begin
  DPFLabel1.Text := 'KeyBoardWillShow Event!';
end;

procedure TFTextField.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
