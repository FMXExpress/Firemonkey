unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.Common, DPF.iOS.UITableView, DPF.iOS.UIButton, DPF.iOS.UILabel,
  DPF.iOS.UIToolbar, DPF.iOS.UIView, DPF.iOS.UITableViewItems;

type
  TFTableViewIndexed = class( TForm )
    DPFUITableView1: TDPFUITableView;
    DPFUIView1: TDPFUIView;
    DPFToolbar1: TDPFToolbar;
    procedure FormShow( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FTableViewIndexed: TFTableViewIndexed;

implementation

{$R *.fmx}
{ TFTableView1 }

procedure TFTableViewIndexed.FormShow( Sender: TObject );
var
  I, J: Integer;
begin
  for I := Ord( 'A' ) to Ord( 'P' ) do
  begin
    DPFUITableView1.IndexList.Add( Chr( I ) );
    with DPFUITableView1.Sections.Add do
    begin
      Header.Kind := kCustom;
      Header.Text := Chr( I );
      for J       := 1 to 5 do
        with TableItems.Add do
        begin
          ItemText.Text := Chr( I ) + J.ToString;
        end;
    end;
  end;
  DPFUITableView1.IndexList.Add( 'Q' );
  DPFUITableView1.IndexList.Add( 'R' );
  DPFUITableView1.IndexList.Add( 'S' );
  DPFUITableView1.IndexList.Add( 'T' );
  DPFUITableView1.RefreshNeeded;
end;

procedure TFTableViewIndexed.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
