
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit CustomBitmapCodec;

interface

uses
  System.Classes, System.Types, System.SysUtils, System.Math,
  System.UITypes, FMX.Consts, FMX.Types, FMX.Surfaces, FMX.Graphics;

type

{ TMyBitmapCodec }

  // This sample codec for own file format. It is just a example. You can create codec for real file format like PSD, JP2000
  
  TMyBitmapCodec = class(TCustomBitmapCodec)
  public
    class function GetImageSize(const AFileName: string): TPointF; override;
    class function IsValid(const AStream: TStream): Boolean; override;
    function LoadFromFile(const AFileName: string; const Bitmap: TBitmapSurface;
      const MaxSizeLimit: Cardinal = 0): Boolean; override;
    function SaveToFile(const AFileName: string; const Bitmap: TBitmapSurface;
      const SaveParams: PBitmapCodecSaveParams = nil): Boolean; override;
    function LoadThumbnailFromFile(const AFileName: string;
      const AFitWidth, AFitHeight: Single; const UseEmbedded: Boolean;
      const Bitmap: TBitmapSurface): Boolean; override;
    function LoadFromStream(const AStream: TStream; const Bitmap: TBitmapSurface;
      const MaxSizeLimit: Cardinal = 0): Boolean; override;
    function SaveToStream(const AStream: TStream; const Bitmap: TBitmapSurface;
      const Extension: string; const SaveParams: PBitmapCodecSaveParams = nil): Boolean; override;
  end;

implementation

type
  TSign = array [0..3] of Char;

var
  DefaultSign: TSign = 'BM32';
  
{ TMyBitmapCodec }

class function TMyBitmapCodec.GetImageSize(const AFileName: string): TPointF;
var
  S: TStream;
  W, H: Integer;
  Sign: TSign;
begin
  S := TFileStream.Create(AFileName, fmOpenRead);
  try
    S.Read(Sign, SizeOf(Sign));
    S.Read(W, SizeOf(W));
    S.Read(H, SizeOf(H));
    Result := TPointF.Create(W, H);
  finally
    S.Free;
  end;
end;

class function TMyBitmapCodec.IsValid(const AStream: TStream): Boolean;
var
  Sign: TSign;
begin
  AStream.Read(Sign, SizeOf(Sign));
  Result := Sign = DefaultSign;
end;

function TMyBitmapCodec.LoadFromFile(const AFileName: string; const Bitmap: TBitmapSurface;
  const MaxSizeLimit: Cardinal = 0): Boolean;
var
  S: TStream;
begin
  S := TFileStream.Create(AFileName, fmOpenRead);
  try
    Result := LoadFromStream(S, Bitmap);
  finally
    S.Free;
    Result := True;
  end;
end;

function TMyBitmapCodec.LoadFromStream(const AStream: TStream; const Bitmap: TBitmapSurface;
  const MaxSizeLimit: Cardinal = 0): Boolean;
var
  W, H: Integer;
  Sign: TSign;
begin
  AStream.Read(Sign, SizeOf(Sign));
  AStream.Read(W, SizeOf(W));
  AStream.Read(H, SizeOf(H));
  Bitmap.SetSize(W, H);
  AStream.Read(Bitmap.Bits^, H * Bitmap.Pitch);
  Result := True;
end;

function TMyBitmapCodec.LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: Single; const UseEmbedded: Boolean;
  const Bitmap: TBitmapSurface): Boolean;
var
  S: TStream;
begin
  S := TFileStream.Create(AFileName, fmOpenRead);
  try
    Result := LoadFromStream(S, Bitmap);
  finally
    S.Free;
    Result := True;
  end;
end;

function TMyBitmapCodec.SaveToFile(const AFileName: string; const Bitmap: TBitmapSurface; const SaveParams: PBitmapCodecSaveParams = nil): Boolean;
var
  S: TStream;
begin
  S := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(S, Bitmap, ExtractFileExt(AFileName), SaveParams);
  finally
    S.Free;
    Result := True;
  end;
end;

function TMyBitmapCodec.SaveToStream(const AStream: TStream; const Bitmap: TBitmapSurface; const Extension: string;
  const SaveParams: PBitmapCodecSaveParams = nil): Boolean;
var
  W, H: Integer;
begin
  W := Bitmap.Width;
  H := Bitmap.Height;
  AStream.Write(DefaultSign, SizeOf(DefaultSign));
  AStream.Write(W, SizeOf(W));
  AStream.Write(H, SizeOf(H));
  AStream.Write(Bitmap.Bits^, H * Bitmap.Pitch);
  Result := True;
end;

initialization
  TBitmapCodecManager.RegisterBitmapCodecClass('.b32', 'My B32 Files', True, TMyBitmapCodec);
end.
