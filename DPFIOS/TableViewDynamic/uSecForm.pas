unit uSecForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UITableViewItems,
  DPF.iOS.UIView, DPF.iOS.UITableView, DPF.iOS.UIImageView, DPF.iOS.UIButton;

type
  TFSecForm = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFUITableView1: TDPFUITableView;
    DPFButton1: TDPFButton;
    procedure FormCreate( Sender: TObject );
    procedure DPFButton1Click( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
    procedure CreateTable( t: TDPFUITableView );
  end;

var
  FSecForm: TFSecForm;

implementation

{$R *.fmx}

// ------------------------------------------------------------------------------
procedure TFSecForm.CreateTable( t: TDPFUITableView );
var
  I: Integer;
begin

  T.ClearAll( true );
  with t.Sections.Add do
  begin
    Header.Text := 'Section 1';
    for I       := 0 to 50 do
    begin
      with TableItems.Add do
      begin
        //BackgroundColor := TAlphaColors.Red;
        // ItemText.BackgroundColor := TAlphaColors.Null;
        ItemText.Text := 'Item ' + IntToStr( i );
        ImageName     := '/Documents/admin.png';
        if I < 10 then
          EditingStyle := TTableViewCellEditingStyle.tvesInsert ;
      end;
    end;
  end;

  with t.Sections.Add do
  begin
    Header.Text := 'Section 2';
    for I       := 0 to 15 do
    begin
      with TableItems.Add do
      begin
        // BackgroundColor := TAlphaColors.Null;
        ItemText.Text := 'Item ' + IntToStr( i );
      end;
    end;
  end;
  T.RefreshNeeded;
end;

// ------------------------------------------------------------------------------
procedure TFSecForm.DPFButton1Click( Sender: TObject );
begin
  CreateTable( DPFUITableView1 );
end;

procedure TFSecForm.FormCreate( Sender: TObject );
begin
  CreateTable( DPFUITableView1 );
end;

// ------------------------------------------------------------------------------
procedure TFSecForm.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

// ------------------------------------------------------------------------------
end.
