//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
unit LoggingFrameU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  EMSHosting.ExtensionsServices, System.JSON, IPPeerClient,
  REST.Backend.EMSProvider, REST.Backend.Providers, EMSHosting.EdgeService;

type
  TEMSEdgeLoggingFrame = class(TFrame)
    Memo1: TMemo;
    Layout1: TLayout;
    ButtonClear: TButton;
    CheckBoxLogging: TCheckBox;
    procedure ButtonClearClick(Sender: TObject);
  private
    procedure LogItem(const ACategory: string; const AJSON: TJSONObject);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

uses EMSHosting.LoggingService, System.StrUtils;

var
  FFrame: TEMSEdgeLoggingFrame;

procedure TEMSEdgeLoggingFrame.ButtonClearClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TEMSEdgeLoggingFrame.LogItem(const ACategory: string; const AJSON: TJSONObject);
var
  LJSON: TJSONObject;
  LLine: string;
begin
  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair(ACategory, AJSON); // Note AJSON is owned by LJSON so AJSON will be freed
    LLine := LJSON.ToString;
    // Remove line breaks
    LLine := System.StrUtils.ReplaceStr(LLine, #13#10, ', ');
    Memo1.Lines.Add(LLine);
  finally
    LJSON.Free;
  end;
end;


constructor TEMSEdgeLoggingFrame.Create(AOwner: TComponent);
begin
  inherited;
  if FFrame <> nil then
    raise Exception.Create('Only one logging frame allowed in an application');
  FFrame := Self;

  // Setup logging
  TEMSLoggingService.OnLog :=
    procedure(const ACategory: string; const AJSON: TJSONObject)
    var
      LJSON: TJSONObject;
    begin
      LJSON := AJSON.Clone as TJSONObject;
      TThread.Queue(nil,
        procedure
        begin
          if FFrame <> nil then
            FFrame.LogItem(ACategory, LJSON)
          else
            LJSON.Free;
        end);
    end;
  TEMSLoggingService.OnLoggingEnabled :=
    function: Boolean
    begin
      Result := (FFrame <> nil) and FFrame.CheckBoxLogging.IsChecked;
    end;

end;

destructor TEMSEdgeLoggingFrame.Destroy;
begin
  FFrame := nil;
  inherited;
end;

end.
