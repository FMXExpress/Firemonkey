
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Menus, FMX.Objects, FMX.StdCtrls,
  FMX.Edit, System.Actions, FMX.ActnList, FMX.Layouts, FMX.Memo;

type

  TToolTipPanel = class(TPanel)
  private
    FOnlyInputFields : Boolean ;
    FMousePoint : TPointF ;
    FCounter : Cardinal;
    FActiveControl : TFmxObject ;
    FLabel : TLabel;
    FTimer : TTimer;
    FBorderWidth : Single;
    function GetToolTipText: string;
    procedure SetToolTipText(const Value: string);
    procedure OnTimer(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowToolTip(AX, AY: Single);
    property Text : string read GetToolTipText write SetToolTipText;
    property BorderWidth : Single read FBorderWidth write FBorderWidth;
    property OnlyInputFields : Boolean read FOnlyInputFields write FOnlyInputFields;
  end;

  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    CheckBox1: TCheckBox;
    Panel1: TPanel;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    StyleBook1: TStyleBook;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    TT : TToolTipPanel;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TToolTipPanel }



{ TToolTipPanel }

constructor TToolTipPanel.Create(AOwner: TComponent);
begin
  inherited;
  Visible := False;
  StyleLookup := 'tooltippanel' ;
  FLabel := TLabel.Create(AOwner);
  FLabel.Parent := Self ;
  FLabel.StyleLookup := 'tooltiplabel' ;
  FLabel.Text := Self.ToString;
  if assigned(FLabel.Canvas) then
    Height := FLabel.Canvas.TextHeight(FLabel.Text);
  FLabel.Align := TAlignLayout.alClient ;
  FLabel.TextAlign := TTextAlign.taCenter ;
  FLabel.VertTextAlign := TTextAlign.taCenter ;
  FTimer := TTimer.Create(AOwner);
  FTimer.OnTimer := OnTimer;
  FTimer.Enabled := True ;
  FTimer.Interval := 500;
  FActiveControl := nil;
  FCounter := 1000;
  FBorderWidth := 10 ;
end;

destructor TToolTipPanel.Destroy;
begin
  inherited;
end;

function TToolTipPanel.GetToolTipText: string;
begin
  Result := FLabel.Text;
end;

procedure TToolTipPanel.OnTimer;
var
  LActiveControl : IControl;
  LControl : TControl;
  LMousePos : TPointF;
  LObject : IControl ;
begin
  if not FOnlyInputFields then
  begin
    if Screen.MousePos <> FMousePoint then
    begin
      FMousePoint := Screen.MousePos ;
      FCounter := 0;
      Visible := False;
    end ;
    Inc(FCounter);
    case FCounter of
      0..2: Visible := False ;
      3:
      begin
        Text := '';
        if Parent is TForm then
        begin
          LObject := (Parent as TForm).ObjectAtPoint(FMousePoint) ;
          if Assigned(LObject) then
            Text := LObject.GetObject.Name;
        end;
        if Text = '' then
          Text := 'ToolTip for mouse pos ' + PointToString(FMousePoint)
        else
          Text := 'ToolTip for component: ' + Text ;
        LMousePos := (Parent as TForm).ScreenToClient(FMousePoint);
        ShowToolTip(LMousePos.X, LMousePos.Y);
      end;
      4..10:;
    else
      FCounter := 1000;
      Visible := False ;
    end;
  end
  else
  begin
    if Parent is TForm then
      LActiveControl := (Parent as TForm).Focused;
    if Assigned(LActiveControl) and (LActiveControl.GetObject <> FActiveControl) then
    begin
      Visible := False ;
      FActiveControl := LActiveControl.GetObject;
      if (FActiveControl is TEdit) then
        FCounter := 0;
    end;
    Inc(FCounter);
    case FCounter of
      0..2: Visible := False ;
      3..10:
      begin
        if assigned(LActiveControl) then
        begin
          LControl := (LActiveControl as TControl);
          Text := 'ToolTip for ' + LControl.Name ;
          ShowToolTip(LControl.Position.X + 20, LControl.Position.Y + LControl.Height);
        end;
      end
    else
      FCounter := 1000;
      Visible := False ;
    end;
  end;
end;

procedure TToolTipPanel.SetToolTipText(const Value: string);
begin
  FLabel.Text := Value ;
end;

procedure TToolTipPanel.ShowToolTip(AX, AY: Single);
begin
  Position.Point := PointF(AX,AY);
  Height := FLabel.Canvas.TextHeight(FLabel.Text) + 2 * FBorderWidth;
  Width  := FLabel.Canvas.TextWidth(FLabel.Text) + 2 * FBorderWidth;
  Visible := True ;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  TT.OnlyInputFields := CheckBox1.IsChecked;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TT := TToolTipPanel.Create(Form1);
  TT.Parent := Self ;
end;

end.
