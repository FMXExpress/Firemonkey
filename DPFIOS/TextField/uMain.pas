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
  DPF.iOS.Keyboard;

type
  TFTextField = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFTextField1: TDPFTextField;
    DPFTextField2: TDPFTextField;
    DPFTextField3: TDPFTextField;
    DPFTextField4: TDPFTextField;
    DPFTextView1: TDPFTextView;
    DPFUIScrollView1: TDPFUIScrollView;
    DPFButton1: TDPFButton;
    DPFButton2: TDPFButton;
    DPFKeyboard1: TDPFKeyboard;
    procedure DPFTextView1TextChanged( Sender: TObject );
    procedure FormShow( Sender: TObject );
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFButton2Click( Sender: TObject );
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
  DPFTextField3.SetFocus;
end;

procedure TFTextField.DPFButton2Click( Sender: TObject );
begin
  DPFTextField2.SetFocus;
  DPFTextField2.SelectAll;
end;

procedure TFTextField.DPFTextView1TextChanged( Sender: TObject );
begin
  // ShowMessage( 'Memo Changed!' );
end;

procedure TFTextField.FormShow( Sender: TObject );
begin
  DPFUIScrollView1.SetScrollPageContentSize( width, height + Height / 2 );
end;

procedure TFTextField.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
