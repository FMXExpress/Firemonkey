unit uMySimulation;

interface

{$I ..\..\Physics2D\Physics2D.inc}

uses
  uSimulation,
  FMX.Types,
  FMX.Objects,
  FMX.Layouts,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Graphics,
  UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TMyb2ContactListener = class(Tb2ContactListener)
   public
      procedure BeginContact(var contact: Tb2Contact); override;
      procedure PostSolve(var contact: Tb2Contact; const impulse: Tb2ContactImpulse); override;
   end;

   TMySimulation = class(TSimulation)
   private
     WorldObject: TLayout;
     m_bodyCount: Integer;
     class var m_level: Integer;
   public
     b2ContactListener: TMyb2ContactListener;
     class constructor Create;
     constructor Create; override;
     procedure Keyboard(key: Byte); override;
     procedure LaunchBomb(velocity_factor: PhysicsFloat = 1.0; Obj: TFmxObject = nil); override;
     procedure SetGUIForm(const Value: IGUIForm); override;
     procedure SetGUIBomb(const Value: TCircle); override;
   end;

implementation

{ TMySimulation }

procedure TMyb2ContactListener.PostSolve(var contact: Tb2Contact; const impulse: Tb2ContactImpulse);
const MAX_IMPULSE:Integer = 100;
var
   bodyA, bodyB: Tb2Body;
begin
  if (impulse.normalImpulses[0]>MAX_IMPULSE) then
   begin
     bodyA := contact.m_fixtureA.GetBody;
     bodyB := contact.m_fixtureB.GetBody;

     if Assigned(bodyA) AND Assigned(BodyB) then
       if (bodyB.UserData<>nil) AND (bodyA.UserData<>nil) then
        begin
          if (TFmxObject(bodyB.UserData) is TCircle) AND (TFmxObject(bodyA.UserData) is TRectangle) then
           begin
                 if TRectangle(bodyA.UserData).Tag=2 then
                   TRectangle(bodyA.UserData).TagFloat := TRectangle(bodyA.UserData).TagFloat+5;
           end
          else if (TFmxObject(bodyB.UserData) is TRectangle) AND (TFmxObject(bodyA.UserData) is TRectangle) then
           begin
                 //if (TRectangle(bodyA.UserData).Tag=2) AND (TRectangle(bodyA.UserData).TagFloat>=5) then
                 if (TRectangle(bodyA.UserData).Tag=2) then
                   TRectangle(bodyA.UserData).TagFloat := TRectangle(bodyA.UserData).TagFloat + 1;
           end;
        end;
   end;
end;

procedure TMyb2ContactListener.BeginContact(var contact: Tb2Contact);
var
   bodyA, bodyB: Tb2Body;
begin
{   bodyA := contact.m_fixtureA.GetBody;
   bodyB := contact.m_fixtureB.GetBody;

   if Assigned(bodyA) AND Assigned(BodyB) then
     if (bodyB.UserData<>nil) AND (bodyA.UserData<>nil) then
      begin
        if (TFmxObject(bodyB.UserData) is TCircle) AND (TFmxObject(bodyA.UserData) is TRectangle) then
         begin
               if TRectangle(bodyA.UserData).Tag=2 then
                 TRectangle(bodyA.UserData).TagFloat := TRectangle(bodyA.UserData).TagFloat+5;
         end
        else if (TFmxObject(bodyB.UserData) is TRectangle) AND (TFmxObject(bodyA.UserData) is TRectangle) then
         begin
               if (TRectangle(bodyA.UserData).Tag=2) AND (TRectangle(bodyA.UserData).TagFloat>=5) then
                 TRectangle(bodyA.UserData).TagFloat := TRectangle(bodyA.UserData).TagFloat + 1;
         end;
      end;}
end;


class constructor TMySimulation.Create;
begin
  m_level := 25;
end;

constructor TMySimulation.Create;
const
   deltaX: TVector2 = (X: 0.5625; Y: 2.0);
   deltaY: TVector2 = (X: 1.125; Y: 0.0);
var
   i, j: Integer;
   ed: Tb2EdgeShape;
   sd: Tb2PolygonShape;
   bd: Tb2BodyDef;
   ground: Tb2Body;
   body: Tb2Body;
   x, y: TVector2;
begin
   inherited;


   WorldObject := TLayout.Create(nil);

   begin
     b2ContactListener := TMyb2ContactListener.Create;
     m_world.SetContactListener(b2ContactListener);
   end;

   begin
      sd := Tb2PolygonShape.Create;
      sd.SetAsBox(70.0, 10.0);

      bd := Tb2BodyDef.Create;
      SetValue(bd.position, 0.0, -10.0);
      ground := m_world.CreateBody(bd);
      ground.CreateFixture(sd, 0.0);

			//bd := Tb2BodyDef.Create;
		 //	ground := m_world.CreateBody(bd);
      ground.UserData := TRectangle.Create(nil);
      TRectangle(ground.UserData).Parent := WorldObject;
      TRectangle(ground.UserData).Position.X := ground.GetPosition.x;
      TRectangle(ground.UserData).Position.Y := ground.GetPosition.y;
      TRectangle(ground.UserData).Width := 1400;
      TRectangle(ground.UserData).Height := 200;
      TRectangle(ground.UserData).BringToFront;
      TRectangle(ground.UserData).Tag := 1;
      TRectangle(ground.UserData).Fill.Color := $FF6C9726;
      TRectangle(ground.UserData).Stroke.Color := $FF6C9726;

		 //	ed := Tb2EdgeShape.Create;
		 //	ed.SetVertices(MakeVector(-40.0, 0.0), MakeVector(40.0, 0.0));
		 //	ground.CreateFixture(ed, 0.0);
   end;

   begin
      sd := Tb2PolygonShape.Create;
      sd.SetAsBox(1.0, 1.0);
      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;

      SetValue(x, -10.0, 0.75);
      m_bodyCount := 0;
      for i := 0 to m_level do
      begin
         y := x;

         for j := i to m_level do
         begin
            bd.position := y;
            body := m_world.CreateBody(bd, False);
            body.CreateFixture(sd, 5.0, False);
            Inc(m_bodyCount);

            body.UserData := TRectangle.Create(nil);
            TRectangle(body.UserData).Parent := WorldObject;
            TRectangle(body.UserData).Position.X := body.GetPosition.x;
            TRectangle(body.UserData).Position.Y := body.GetPosition.y;
            TRectangle(body.UserData).Width := 22;
            TRectangle(body.UserData).Height := 22;
            TRectangle(body.UserData).BringToFront;
            TRectangle(body.UserData).Tag := 2;
            TRectangle(body.UserData).Fill.Color := $FF453F33;
            TRectangle(body.UserData).Stroke.Color := $FF1F1C17;




            {$IFDEF OP_OVERLOAD}
            y.AddBy(deltaY);
            {$ELSE}
            AddBy(y, deltaY);
            {$ENDIF}
         end;
         {$IFDEF OP_OVERLOAD}
         x.AddBy(deltaX);
         {$ELSE}
         AddBy(x, deltaX);
         {$ENDIF}
      end;
      sd.Free;
      bd.Free;
   end;
end;

procedure TMySimulation.SetGUIForm(const Value: IGUIForm);
begin
   inherited;

   WorldObject.Parent := TForm(Value);
   WorldObject.Align := TAlignLayout.alContents;


end;

procedure TMySimulation.Keyboard(key: Byte);
begin
   inherited;

   if key > 0 then
   begin
     case key of
        187, 43{+}:
           if m_level < 30 then
              Inc(m_level);
        189, 95{-}:
           if m_level > 1 then
              Dec(m_level);
     else
        Exit;
     end;

     if Assigned(FGUIForm) then
       FGUIForm.SimulationRecreateAndRun;
   end;
end;

procedure TMySimulation.LaunchBomb(velocity_factor: PhysicsFloat; Obj: TFmxObject);
begin
   inherited LaunchBomb(15.0, Obj);
   Obj.Parent := WorldObject;
   Obj.Tag := 3;
end;

procedure TMySimulation.SetGUIBomb(const Value: TCircle);
begin
   inherited SetGUIBomb(Value);

   Value.Parent := WorldObject;
   Value.Tag := 3;


end;


end.

