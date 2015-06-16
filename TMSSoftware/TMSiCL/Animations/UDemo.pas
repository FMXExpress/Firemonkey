unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSNativeUIButton,
  FMX.TMSNativeUIBaseControl, FMX.TMSNativeUIView, iOSApi.UIKit,
  FMX.TMSNativeUIImageView;

type
  TForm1024 = class(TForm)
    TMSFMXNativeUIView1: TTMSFMXNativeUIView;
    TMSFMXNativeUIButton1: TTMSFMXNativeUIButton;
    TMSFMXNativeUIImageView1: TTMSFMXNativeUIImageView;
    TMSFMXNativeUIView2: TTMSFMXNativeUIView;
    TMSFMXNativeUIImageView2: TTMSFMXNativeUIImageView;
    TMSFMXNativeUIView3: TTMSFMXNativeUIView;
    procedure TMSFMXNativeUIButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1024: TForm1024;

implementation

{$R *.fmx}

procedure TForm1024.FormCreate(Sender: TObject);
begin
  TMSFMXNativeUIView2.Parent := nil;
end;

procedure TForm1024.TMSFMXNativeUIButton1Click(Sender: TObject);
var
  vwRemove, vwAdd: TTMSFMXNativeUIView;
begin
  vwRemove := nil;
  vwAdd := nil;
  if Assigned(TMSFMXNativeUIView1.Parent) then
  begin
    vwRemove := TMSFMXNativeUIView1;
    vwAdd := TMSFMXNativeUIView2;
  end
  else if Assigned(TMSFMXNativeUIView2.Parent) then
  begin
    vwRemove := TMSFMXNativeUIView2;
    vwAdd := TMSFMXNativeUIView1;
  end;

  if Assigned(vwRemove) and Assigned(vwAdd) then
  begin
    TUIView.OCClass.beginAnimations(nil, nil);
    TUIView.OCClass.setAnimationTransition(UIViewAnimationTransitionFlipFromLeft, TMSFMXNativeUIView3.View, False);
    TUIView.OCClass.setAnimationCurve(UIViewAnimationCurveEaseIn);
    TUIView.OCClass.setAnimationDuration(0.8);
    vwRemove.Parent := nil;
    vwAdd.Parent := TMSFMXNativeUIView3;
    TUIView.OCClass.commitAnimations
  end;
end;

end.
