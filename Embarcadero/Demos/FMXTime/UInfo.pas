(*************************************************************************)
(*                                                                       *)
(* Fmx Time                                                              *)
(*                                                                       *)
(* Author  : Thierry Laborde                                             *)
(* Email   : thierry.laborde@Embarcadero.com                             *)
(* Date    : 26 Février 2014                                             *)
(* Version : v1.1                                                        *)
(*                                                                       *)
(*************************************************************************)
unit UInfo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.Layouts, FMX.StdCtrls, FMX.Ani;

type
  TFrmInfo = class(TForm)
    LayoutBackInfo: TLayout;
    RectBlack: TRectangle;
    LayoutInfo: TLayout;
    GrdPnlLayoutInfo: TGridPanelLayout;
    LblInfo1: TLabel;
    ImgDelphiLogo: TImage;
    LblInfo2: TLabel;
    FltAnimPositionY: TFloatAnimation;
    BtClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FltAnimPositionYFinish(Sender: TObject);
    procedure BtCloseClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FrmInfo: TFrmInfo;

implementation

{$R *.fmx}

procedure TFrmInfo.BtCloseClick(Sender: TObject);
begin
  FrmInfo.FltAnimPositionY.StartValue := Application.MainForm.Height;
  FltAnimPositionY.Start;
end;

procedure TFrmInfo.FltAnimPositionYFinish(Sender: TObject);
begin
  FltAnimPositionY.Inverse := not(FltAnimPositionY.Inverse);
  LayoutBackInfo.Visible   := FltAnimPositionY.Inverse;
end;

procedure TFrmInfo.FormCreate(Sender: TObject);
begin
  LblInfo1.Text := 'Application developed with' + LineFeed + 'Delphi XE5';
  LblInfo2.Text := 'Author : Thierry Laborde' + LineFeed + 'Email : thierry.laborde@embarcadero.com';
end;

end.
