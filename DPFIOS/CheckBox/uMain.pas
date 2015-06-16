unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  DPF.iOS.BaseControl, DPF.iOS.UIView, DPF.iOS.UIButton, DPF.iOS.CheckBox,
  DPF.iOS.UILabel, DPF.iOS.UIImageView;

type
  TFCheckBox = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFCheckBox2: TDPFCheckBox;
    DPFCheckBox1: TDPFCheckBox;
    DPFCheckBox3: TDPFCheckBox;
    DPFCheckBox4: TDPFCheckBox;
    DPFCheckBox5: TDPFCheckBox;
    DPFCheckBox6: TDPFCheckBox;
    DPFCheckBox7: TDPFCheckBox;
    DPFCheckBox8: TDPFCheckBox;
    DPFCheckBox9: TDPFCheckBox;
    DPFCheckBox10: TDPFCheckBox;
    procedure DPFCheckBox2Click( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure DoShow; override;
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FCheckBox: TFCheckBox;

implementation

{$R *.fmx}

procedure TFCheckBox.DoShow;
begin
  inherited;
  SendToBack;
end;

procedure TFCheckBox.DPFCheckBox2Click( Sender: TObject );
begin
  { }
end;

procedure TFCheckBox.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
