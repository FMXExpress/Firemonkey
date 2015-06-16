unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,

  FMX.Platform,
  FMX.Pickers,
  FMX.Pickers.iOS,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UIView, uEmbForm1, uEmbForm2, DPF.iOS.UIButton, DPF.iOS.UIToolbar;

type
  TFFormEmbed = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFUIView2: TDPFUIView;
    DPFButton2: TDPFButton;
    DPFToolbar1: TDPFToolbar;
    DPFUIViewMain: TDPFUIView;
    procedure DPFButton2Click( Sender: TObject );
    procedure DPFToolbar1BarItems0Click( Sender: TObject );
    procedure DPFToolbar1BarItems2Click( Sender: TObject );
    procedure DPFToolbar1BarItems4Click( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FFormEmbed: TFFormEmbed;

implementation

{$R *.fmx}

procedure TFFormEmbed.DPFButton2Click( Sender: TObject );
begin
  if FEmbForm2.DPFSegmentedControl1.SelectedIndex < FEmbForm2.DPFSegmentedControl1.Segments.Count - 1 then
    FEmbForm2.DPFSegmentedControl1.SelectedIndex := FEmbForm2.DPFSegmentedControl1.SelectedIndex + 1
  else
    FEmbForm2.DPFSegmentedControl1.SelectedIndex := 0;
end;

procedure TFFormEmbed.DPFToolbar1BarItems0Click( Sender: TObject );
begin
  FEmbForm1.DPFButton1InForm1.Text := 'Caption Changed';
end;

procedure TFFormEmbed.DPFToolbar1BarItems2Click( Sender: TObject );
begin
  DPFUIView1.Form := nil;
end;

procedure TFFormEmbed.DPFToolbar1BarItems4Click( Sender: TObject );
begin
  DPFUIView1.Form := nil;
  DPFUIView1.Form := FEmbForm1;
end;

procedure TFFormEmbed.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
