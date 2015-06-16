unit UFormWrapper;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSNativeFMXWrapper,
  FMX.TMSNativeUIPopoverController, FMX.TMSNativeUIBaseControl,
  FMX.TMSNativeUIButton, UFormSample, UFormSample2, UFormSample3, FMX.TMSNativeUITableView,
  FMX.TMSNativeUIView, FMX.TMSNativeUIToolBar;

type
  TForm936 = class(TForm)
    TMSFMXNativeUIPopoverController1: TTMSFMXNativeUIPopoverController;
    TMSFMXNativeFMXWrapper1: TTMSFMXNativeFMXWrapper;
    TMSFMXNativeUITableView1: TTMSFMXNativeUITableView;
    TMSFMXNativeFMXWrapper2: TTMSFMXNativeFMXWrapper;
    TMSFMXNativeUIView1: TTMSFMXNativeUIView;
    TMSFMXNativeFMXWrapper3: TTMSFMXNativeFMXWrapper;
    TMSFMXNativeUIToolBar1: TTMSFMXNativeUIToolBar;
    procedure TMSFMXNativeUIToolBar1ItemClick(ASender: TObject;
      AItem: TTMSFMXNativeUIToolBarItem);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form936: TForm936;

implementation

{$R *.fmx}

procedure TForm936.TMSFMXNativeUIToolBar1ItemClick(ASender: TObject;
  AItem: TTMSFMXNativeUIToolBarItem);
begin
  TMSFMXNativeUIPopoverController1.ShowFromButton(AItem.Item);
end;

end.
