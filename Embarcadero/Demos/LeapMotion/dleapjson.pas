{ Convert Leap Websocket data (JSON) to Leap Data Structures

  Copyright (C) 2013 Michael Van Canneyt (michael@freepascal.org);

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit dleapjson;

interface

uses
  Classes, SysUtils, leapdata, superobject;

Type

  { TJSONFrameConverter }

  TJSONFrameConverter = Class(TComponent)
  private
    FNilOnError: Boolean;
    function AddGestures(AFrame: TFrame; const JSON: TSuperArray): Integer;
    procedure JSONToCircleGesture(G: TCircleGesture; const JSON: ISuperObject);
    procedure JSONToGesture(G: TGesture; const JSON: ISuperObject);
    procedure JSONToSwipeGesture(G: TSwipeGesture; const JSON: ISuperObject);
    procedure JSONToTapGesture(G: TTapGesture; const JSON: ISuperObject);
    function NewGesture(AFrame: TFrame; AID: TLeapID; AType: String): TGesture;
  Protected
    procedure DoError(Msg: String);
    function NewPointable(AFrame: TFrame; AID: TLeapID; ATool: Boolean): TPointable;virtual;
    Function NewFrame(AID : TLeapID) : TFrame; virtual;
    function NewHand(AFrame: TFrame; AID: TLeapID): THand; virtual;
    procedure JSONToHand(H: THand; const JSON: ISuperObject); virtual;
    procedure JSONToPointable(P: TPointable; const JSON: ISuperObject); virtual;

    function ArrayTo3DVector(A: TSuperArray): T3DVector;
    function GetVector(const JSON: ISuperObject; AName: String; const AError: String=''): T3DVector;
    function GetMatrix(const JSON: TSuperArray; const AError: String): T3DMatrix;
    function AddInteractionBox(AFrame: TFrame; const JSON: ISuperObject): TInteractionBox;
    function AddPointables(AFrame: TFrame; const JSON: TSuperArray): Integer;
    function AddHands(AFrame: TFrame; const JSON: TSuperArray): Integer;
  Public
    Function FrameFromStream(Const S : TStream) : TFrame;
    Function FrameFromString(Const S : String) : TFrame;
    Function FrameFromJSON(Const JSON : ISuperObject) : TFrame;
    Property NilOnError : Boolean Read FNilOnError Write FNilOnError;
  end;

Function ExtractVersion(S : TStream) : Integer;
Function WriteEnableGestures(Enable : Boolean) : String;

implementation

Function WriteEnableGestures(Enable : Boolean) : String;

Var
  O : ISuperObject;
  S : String;

begin
  O:=SO('{}');
(*
  if Enable then
    O:=SO('{"enableGestures" : true}')
  else
    O:=SO('{"enableGestures" : true}');
*)
  O.B['enableGestures']:=Enable;
  S:=O.AsJSon();
  Result:=S;
end;

Function ExtractVersion(S : TStream) : Integer;

Var
  D : ISuperObject;
  SA : AnsiString;

begin
  SetLength(SA,S.Size);
  S.Read(SA[1],S.Size);
  D:=SO(SA);
  Result:=SO.I['Version'];
end;

function TJSONFrameConverter.FrameFromStream(Const S: TStream): TFrame;

Var
  O : ISuperObject;
  E : AnsiString;
  F : TFileStream;

begin
  SetLength(E,S.Size);
  F:=TFileStream.Create('c:\temp\frame.json',fmcreate);
  try
    F.CopyFrom(S,0);
  finally
    F.Free
  end;
  S.Position:=0;
  S.Read(PAnsiChar(E)^,Length(E));
  O:=SO(E);
  Result:=FrameFromJSON(O)
end;

function TJSONFrameConverter.FrameFromString(Const S: String): TFrame;

Var
  SS : TStringStream;

begin
  SS:=TStringStream.Create(S);
  try
    Result:=FrameFromStream(SS);
  finally
    SS.Free;
  end;
end;

Procedure  TJSONFrameConverter.DoError(Msg : String);

begin
  Raise EConvertError.Create(Msg);
end;

function TJSONFrameConverter.ArrayTo3DVector(A : TSuperArray) : T3DVector;

Var
  I : Integer;

begin
  If (A.Length<>3) then
    DoError('Array is not a vector: wrong length');
  For I:=0 to 2 do
    If Not (A[I].DataType in [stDouble,stCurrency,stInt]) then
      DoError('Array is not a vector: element '+IntToStr(i)+' not a number');
  Result.X:=A[0].AsDouble;
  Result.Y:=A[1].AsDouble;
  Result.Z:=A[2].AsDouble;
end;

function TJSONFrameConverter.AddInteractionBox(AFrame : TFrame;Const JSON: ISuperObject): TInteractionBox;

Var
  A1,A2 : TSuperArray;
begin
  if (JSON=Nil) then
    DoError('Missing interactionBox');
  A1:=JSON.A['center'];
  A2:=JSON.A['size'];
  If (A1=Nil) then
    DoError('Missing interactionBox.center');
  If (A2=Nil) then
    DoError('Missing interactionBox.size');
  Result:=TInteractionBox.Create(ArrayTo3DVector(A1),ArrayTo3DVector(A2));
  AFrame.InteractionBox:=Result;
end;

function TJSONFrameConverter.GetVector(Const JSON: ISuperObject; AName : String; Const AError : String = '') : T3DVector;

Var
  A : TSuperArray;

begin
  A:=JSON.A[AName];
  If A=Nil then
    DoError(AError+': missing '+AName);
  Result:=ArrayTo3DVector(A);
end;


Function TJSONFrameConverter.GetMatrix(Const JSON: TSuperArray; Const AError : String) : T3DMatrix;

Var
  I,J : Integer;
  A : TSuperArray;
  S : String;
begin
  If JSON.Length<>3 then
    DoError(AError+' array element count is not 3');
  For I:=0 to 2 do
    if (Not JSON[i].IsType(starray)) then
      DoError(AError+' array element '+IntToStr(i)+' is not an array')
    else
      begin
      A:=JSON.O[i].AsArray;
      S:=AError+' array '+IntToStr(i)+' : ';
      If A.Length<>3 then
        DoError(S+' element count is not 3');
      For J:=0 to 2 do
        if (Not (A[j].DataType in [stDouble,stCurrency,stInt])) then
          DoError(S+' array element '+IntToStr(j)+' is not a number')
        else
          begin
          Result[i+1][j+1]:=A.D[J];
          end;
      end;
end;

Procedure TJSONFrameConverter.JSONToHand(H : THand; Const JSON: ISuperObject);

  Function Vec(El : String) : T3DVector;
  begin
    Result:=GetVector(JSON,El,'Hand '+IntToStr(H.ID));
  end;

begin
  H.Direction:=vec('direction');
  H.PalmNormal:=vec('palmNormal');
  H.palmPosition:=vec('palmPosition');
  H.palmVelocity:=vec('palmVelocity');
  H.Scalefactor:=JSON.D['s'];
  H.Translation:=vec('t');
  H.SphereCenter:=vec('sphereCenter');
  H.SphereRadius:=JSON.D['sphereRadius'];
  H.RotationMatrix:=GetMatrix(JSON.A['r'],'Hand '+IntToStr(H.ID));
end;

procedure TJSONFrameConverter.JSONToPointable(P: TPointable; const JSON: ISuperObject);

  Function Vec(El : String) : T3DVector;
  begin
    Result:=GetVector(JSON,El,'Pointable '+IntToStr(P.ID));
  end;

Var
  A : ISuperObject;

begin
  P.Direction:=vec('direction');
  P.HandID:=JSON.I['handId'];
  P.Length:=JSON.D['length'];
  P.StabilizedTipPosition:=vec('stabilizedTipPosition');
  P.TipPosition:=vec('tipPosition');
  P.TipVelocity:=vec('tipVelocity');
  P.TouchDist:=JSON.D['touchDist'];
  P.TouchZone:=JSON.S['touchZone'];
  if (P is TTool) then
    TTool(P).Width:=JSON.D['width'];
end;

function TJSONFrameConverter.NewHand(AFrame : TFrame; AID : TLeapID): THand;

begin
  Result:=THand.Create(AFrame,AID);
end;

function TJSONFrameConverter.AddHands(AFrame : TFrame; Const JSON: TSuperArray): Integer;

Var
  I : Integer;
  O : ISuperObject;

begin
  For I:=0 to JSON.Length-1 do
    if Not JSON[i].IsType(stObject) then
      DoError('Hand '+IntToStr(i)+' is not a JSON Object');
  For I:=0 to JSON.Length-1 do
    begin
    O:=JSON.O[I];
    JSONToHand(NewHand(AFrame,O.I['id']),O);
    end;
  Result:=JSON.Length-1
end;

function TJSONFrameConverter.NewPointable(AFrame : TFrame; AID : TLeapID; ATool : Boolean): TPointable;

begin
  if ATool then
    Result:=TTool.Create(AFrame,AID)
  else
    Result:=TFinger.Create(AFrame,AID);
end;

function TJSONFrameConverter.AddPointables(AFrame : TFrame; Const JSON: TSuperArray): Integer;

Var
  I : Integer;
  O : ISuperObject;

begin
  For I:=0 to JSON.Length-1 do
    if Not JSON[i].IsType(stObject) then
      DoError('Pointable '+IntToStr(i)+' is not a JSON Object');
  For I:=0 to JSON.Length-1 do
    begin
    O:=JSON.O[I];
    JSONToPointable(NewPointable(AFrame,O.I['id'],O.B['tool']),O);
    end;
  Result:=JSON.Length-1;
end;

function TJSONFrameConverter.NewGesture(AFrame : TFrame; AID : TLeapID; AType : String): TGesture;

Var
  GC : TGestureClass;

begin
  GC:=TGesture;
  if (AType='circle') then
    GC:=TCircleGesture
  else if (AType='swipe') then
    GC:=TSwipeGesture
  else if (AType='screenTap') then
    GC:=TScreenTapGesture
  else if (AType='keyTap') then
    GC:=TKeyTapGesture;
  Result:=GC.Create(AFrame,AID);
end;


procedure TJSONFrameConverter.JSONToSwipeGesture(G: TSwipeGesture; const JSON: ISuperObject);

begin
  G.Position:=GetVector(JSON,'position','swipe position');
  G.Direction:=GetVector(JSON,'direction','swipe position');
  G.StartPosition:=GetVector(JSON,'startPosition','swipe startposition');
  G.Speed:=JSON.D['speed'];
end;

procedure TJSONFrameConverter.JSONToCircleGesture(G: TCircleGesture; const JSON: ISuperObject);

begin
  G.Center:=GetVector(JSON,'center','circle center');
  G.Normal:=GetVector(JSON,'normal','circle normal');
  G.Progress:=JSON.D['progress'];
  G.Radius:=JSON.D['radius'];
end;

procedure TJSONFrameConverter.JSONToTapGesture(G: TTapGesture; const JSON: ISuperObject);

begin
  G.Direction:=GetVector(JSON,'direction','tap direction');
  G.Position:=GetVector(JSON,'position','tap position');
  G.Progress:=JSON.D['progress'];
end;

Var
  GC : Integer;

procedure TJSONFrameConverter.JSONToGesture(G: TGesture; const JSON: ISuperObject);

Type
  TINarr = Array of INteger;

  Function GetA(N : String): TINarr;

  Var
    A : TSuperArray;
    I : Integer;

  begin
    A:=JSON.A[N];
    SetLength(Result,A.Length);
    For I:=0 to A.Length-1 do
      Result[i]:=A.I[i];
  end;

Var
  T : TStringList;
  S : String;

begin
  if JSON=Nil then
    Exit;
  G.Duration:=JSON.I['duration'];
  S:=JSON.S['state'];
  if (s='update') then
    G.State:=gsUpdate
  else if (s='start') then
    G.State:=gsStart
  else if (s='stop') then
    G.State:=gsStop;
  G.SetHandIDs(GetA('handIds'));
  G.SetPointableIDs(GetA('pointableIds'));
  case G.GestureType of
    gtSwipe : JSONToSwipeGesture(G as TSwipeGesture,JSON);
    gtCircle : JSONToCircleGesture(G as TCircleGesture,JSON);
    gtScreenTap,
    gtKeytap : JSONToTapGesture(G as TTapGesture,JSON);
  end;
end;

function TJSONFrameConverter.AddGestures(AFrame : TFrame; Const JSON: TSuperArray): Integer;

Var
  I : Integer;
  O : ISuperObject;

begin
  if JSON=Nil then
    exit;
  For I:=0 to JSON.Length-1 do
    if Not JSON[i].IsType(stObject) then
      DoError('Gesture '+IntToStr(i)+' is not a JSON Object');
  For I:=0 to JSON.Length-1 do
    begin
    O:=JSON.O[I];
    JSONToGesture(NewGesture(AFrame,O.I['id'],O.S['type']),O);
    end;
  Result:=JSON.Length-1
end;

function TJSONFrameConverter.FrameFromJSON(Const JSON: ISuperObject): TFrame;

Var
  O : ISuperObject;

begin
  Result:=Nil;
  if JSON.S['Version']='' then
    begin
    Result:=NewFrame(JSON.I['id']);
    try
      AddInteractionBox(Result,JSON.O['interactionBox']);
      AddHands(Result,JSON.A['hands']);
      AddPointables(Result,JSON.A['pointables']);
      AddGestures(Result,JSON.A['gestures']);
      Result.Scalefactor:=JSON.D['s'];
      Result.Translation:=GetVector(JSON,'t','Translation vector');
      Result.RotationMatrix:=GetMatrix(JSON.A['r'],'Rotation matrix');
      Result.TimeStamp:=JSON.I['timestamp'];
    except
      If NilOnError then
        FreeAndNil(Result)
      else
        Raise;
    end;
    end
  else if not NilOnError then
    DoError('Not a frame');
end;

{ TJSONFrameConverter }

function TJSONFrameConverter.NewFrame(AID: TLeapID): TFrame;
begin
  Result:=TFrame.Create(AID);
end;


end.

