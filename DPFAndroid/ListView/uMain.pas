unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  FMX.Helpers.Android,
  DPF.Android.Widget,
  DPF.ANDROID.BaseControl,
  DPF.Android.Common,
  DPF.Android.JImageView,
  DPF.Android.JTextView,
  DPF.Android.JListView,
  DPF.Android.JLinearLayout,
  DPF.Android.JRelativeLayout,
  DPF.Android.JToast;

type
  TFListView = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJListView1: TDPFJListView;
    DPFJToast1: TDPFJToast;
    procedure DPFJListView1RowCount( Sender: TObject; var RowCount: Integer );
    procedure DPFJListView1DrawCell( Sender: TObject; Position: Integer; var Objects: TArray<DPF.Android.BaseControl.TDPFANDBaseControl> );
    procedure DPFJListView1ItemClick( Sender: TObject; const Position: Integer; const ID: Int64 );
    procedure DPFJView1Key( Sender: TObject; KeyCode: Integer; var isDispatchKeyEvent: Boolean );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FListView: TFListView;

implementation

{$R *.fmx}

procedure TFListView.DPFJListView1DrawCell( Sender: TObject; Position: Integer; var Objects: TArray<DPF.Android.BaseControl.TDPFANDBaseControl> );
begin
  if Length( Objects ) = 0 then
  begin
    SetLength( Objects, 5 );
    TDPFJRelativeLayout( Objects[0] )                      := TDPFJRelativeLayout.Create( nil );
    TDPFJRelativeLayout( Objects[0] ).IsAutoResize         := true;
    TDPFJRelativeLayout( Objects[0] ).LayoutWidth          := lpMATCH_PARENT;
    TDPFJRelativeLayout( Objects[0] ).LayoutHeight         := lpNone;
    TDPFJRelativeLayout( Objects[0] ).Height               := 100;
    TDPFJRelativeLayout( Objects[0] ).Position.X           := 0;
    TDPFJRelativeLayout( Objects[0] ).Position.Y           := 0;
    TDPFJRelativeLayout( Objects[0] ).AddThisToSubView     := false;
    TDPFJRelativeLayout( Objects[0] ).FocusableInTouchMode := false;
    TDPFJRelativeLayout( Objects[0] ).BackgroundColor1     := TAlphaColors.Yellow;
    TDPFJRelativeLayout( Objects[0] ).DrawType             := dtCustom;
    TDPFJRelativeLayout( Objects[0] ).Clickable            := false;
    TDPFJRelativeLayout( Objects[0] ).Focusable            := false;
    TDPFJRelativeLayout( Objects[0] ).Loaded;

    TDPFJImageView( Objects[1] )                      := TDPFJImageView.Create( Objects[0] );
    TDPFJImageView( Objects[1] ).Parent               := Objects[0];
    TDPFJImageView( Objects[1] ).Position.X           := 0;
    TDPFJImageView( Objects[1] ).Position.Y           := 0;
    TDPFJImageView( Objects[1] ).Width                := 100;
    TDPFJImageView( Objects[1] ).Height               := 100;
    TDPFJImageView( Objects[1] ).FileName             := 'DPF3.png';
    TDPFJImageView( Objects[1] ).FocusableInTouchMode := false;
    TDPFJImageView( Objects[1] ).Clickable            := false;
    TDPFJImageView( Objects[1] ).Focusable            := false;
    TDPFJImageView( Objects[1] ).Loaded;

    TDPFJTextView( Objects[2] )                      := TDPFJTextView.Create( Objects[0] );
    TDPFJTextView( Objects[2] ).Parent               := Objects[0];
    TDPFJTextView( Objects[2] ).Position.X           := TDPFJImageView( Objects[1] ).Width + 1;
    TDPFJTextView( Objects[2] ).Position.Y           := 0;
    TDPFJTextView( Objects[2] ).Width                := 280;
    TDPFJTextView( Objects[2] ).Height               := 50;
    TDPFJTextView( Objects[2] ).TextColor            := TAlphaColors.Black;
    TDPFJTextView( Objects[2] ).TextSize             := 18;
    TDPFJTextView( Objects[2] ).FocusableInTouchMode := false;
    TDPFJTextView( Objects[2] ).Clickable            := false;
    TDPFJTextView( Objects[2] ).Focusable            := false;
    TDPFJTextView( Objects[2] ).Loaded;

    TDPFJTextView( Objects[3] )                      := TDPFJTextView.Create( Objects[0] );
    TDPFJTextView( Objects[3] ).Parent               := Objects[0];
    TDPFJTextView( Objects[3] ).Position.X           := TDPFJImageView( Objects[1] ).Width;
    TDPFJTextView( Objects[3] ).Position.Y           := 50;
    TDPFJTextView( Objects[3] ).Width                := 280;
    TDPFJTextView( Objects[3] ).Height               := 50;
    TDPFJTextView( Objects[3] ).TextColor            := TAlphaColors.Gray;
    TDPFJTextView( Objects[3] ).TextSize             := 14;
    TDPFJTextView( Objects[3] ).FocusableInTouchMode := false;
    TDPFJTextView( Objects[3] ).Clickable            := false;
    TDPFJTextView( Objects[3] ).Focusable            := false;
    TDPFJTextView( Objects[3] ).Loaded;

    TDPFJTextView( Objects[4] )                      := TDPFJTextView.Create( Objects[0] );
    TDPFJTextView( Objects[4] ).Parent               := Objects[0];
    TDPFJTextView( Objects[4] ).Position.X           := DPFJListView1.Width - 50;
    TDPFJTextView( Objects[4] ).Position.Y           := 0;
    TDPFJTextView( Objects[4] ).Width                := 100;
    TDPFJTextView( Objects[4] ).TextColor            := TAlphaColors.Blue;
    TDPFJTextView( Objects[4] ).TextSize             := 16;
    TDPFJTextView( Objects[4] ).FocusableInTouchMode := false;
    TDPFJTextView( Objects[4] ).Clickable            := false;
    TDPFJTextView( Objects[4] ).Focusable            := false;
    TDPFJTextView( Objects[4] ).Loaded;

  end;
  TDPFJTextView( Objects[2] ).Text.Text  := 'Custom Text View ' + Position.ToString;
  TDPFJTextView( Objects[3] ).Text.Text  := 'Description Text View ' + Position.ToString;;
  TDPFJTextView( Objects[4] ).Position.X := DPFJListView1.Width - 50;
  TDPFJTextView( Objects[4] ).Text.Text  := GetTime( 1 );

end;

procedure TFListView.DPFJListView1ItemClick( Sender: TObject; const Position: Integer; const ID: Int64 );
var
  cViews: TArray<TDPFANDBaseControl>;
begin
  cViews := DPFJListView1.GetRowCustomViews( Position, id );
  if Assigned( cViews ) then
    DPFJToast1.Show( TDPFJTextView( cViews[2] ).Text.Text + ' Clicked!' )
  else
    DPFJToast1.Show( 'ID: ' + ID.ToString + ' , Position: ' + Position.ToString );

end;

procedure TFListView.DPFJListView1RowCount( Sender: TObject; var RowCount: Integer );
begin
  RowCount := 1000;
end;

procedure TFListView.DPFJView1Key( Sender: TObject; KeyCode: Integer; var isDispatchKeyEvent: Boolean );
begin
  { }
end;

end.
