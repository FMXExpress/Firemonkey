unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  DPF.iOS.BaseControl,
  DPF.iOS.UILabel, DPF.iOS.UIView, DPF.iOS.UIButton,
  DPF.iOS.UIActionSheet, DPF.iOS.UIViewController;

type
  TFActionMessages = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFButton1: TDPFButton;
    DPFUIActionSheet1: TDPFUIActionSheet;
    DPFButton2: TDPFButton;
    DPFLabel1: TDPFLabel;
    DPFButton3: TDPFButton;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFButton2Click( Sender: TObject );
    procedure DPFUIActionSheet1Click( Sender: TObject; ButtonIndex: Integer );
    procedure DPFButton3Click( Sender: TObject );
    procedure FormShow( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FActionMessages: TFActionMessages;

implementation

{$R *.fmx}

procedure TFActionMessages.DPFButton1Click( Sender: TObject );
begin
  DPFUIActionSheet1.Buttons.Clear;
  DPFUIActionSheet1.TitleBackGroundColor := TAlphaColors.Null;
  DPFUIActionSheet1.TitleColor           := TAlphaColors.Black;
  DPFUIActionSheet1.Title                := 'Hello!' + #10#13 + 'This is a Action Message !' + #10#13;
  // DPFUIActionSheet1.ShowMessage;
  DPFUIActionSheet1.Execute( DPFLabel1 );
end;

procedure TFActionMessages.DPFButton2Click( Sender: TObject );
begin
  DPFUIActionSheet1.Buttons.Clear;
  DPFUIActionSheet1.TitleBackGroundColor := TAlphaColors.Null;
  DPFUIActionSheet1.TitleColor           := TAlphaColors.Black;
  DPFUIActionSheet1.Title                := 'Are you sure ?';
  DPFUIActionSheet1.Buttons.Add.Title    := 'Yes';
  DPFUIActionSheet1.Buttons.Add.Title    := 'No';
  DPFUIActionSheet1.ShowMessage;
end;

procedure TFActionMessages.DPFButton3Click( Sender: TObject );
begin
  DPFUIActionSheet1.Title                := 'Hello!' + #10#13 + 'This is a Custom Action Message !' + #10#13;
  DPFUIActionSheet1.TitleBackGroundColor := TAlphaColors.Black;
  DPFUIActionSheet1.TitleColor           := TAlphaColors.Yellow;

  DPFUIActionSheet1.Buttons.Clear;
  with DPFUIActionSheet1.Buttons.Add do
  begin
    Title                   := 'Yes';
    BackgroundColor         := TAlphaColors.Green;
    CornerRadius            := 15;
    BorderWidth             := 1;
    BorderColor             := TAlphaColors.Black;
    Font.FontSize           := 16;
    Font.BoldSystemFontSize := true;
    TitleColorNormal        := TAlphaColors.Yellow;
    ButtonKind              := bkDestructive;
  end;
  with DPFUIActionSheet1.Buttons.Add do
  begin
    Title                   := 'No';
    BackgroundColor         := TAlphaColors.Blue;
    CornerRadius            := 15;
    BorderWidth             := 2;
    BorderColor             := TAlphaColors.Yellow;
    Font.FontSize           := 18;
    Font.BoldSystemFontSize := true;
    ButtonKind              := bkCustom;
  end;

  with DPFUIActionSheet1.Buttons.Add do
  begin
    Title                   := 'Cancel';
    BackgroundColor         := TAlphaColors.red;
    CornerRadius            := 15;
    BorderWidth             := 1;
    BorderColor             := TAlphaColors.Black;
    Font.FontSize           := 20;
    Font.BoldSystemFontSize := true;
    ButtonKind              := bkCancel;
  end;
  DPFUIActionSheet1.ShowMessage;
end;

procedure TFActionMessages.DPFUIActionSheet1Click( Sender: TObject; ButtonIndex: Integer );
begin
  DPFLabel1.Text := 'You clicked button : ' + IntToStr( ButtonIndex );
end;

procedure TFActionMessages.FormShow( Sender: TObject );
begin
  Application.FormFactor.Orientations := [TScreenOrientation.Portrait];
end;

procedure TFActionMessages.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
