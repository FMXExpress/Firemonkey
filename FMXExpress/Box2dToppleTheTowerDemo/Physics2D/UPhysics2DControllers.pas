unit UPhysics2DControllers;

{ box2D 2.3.0 translation

  ###  This unit is written based on Box2D maintained by Erin Catto (http://www.box2d.org)
  All type names follow the Delphi custom Txxx and xxx means the corresponding
  type in cpp source. This rule doesn't apply to the following types b2Vec2,
  b2Vec3, b2Mat22 and b2Mat33. Because I think these types are so primary that you
  may want to reuse them in other projects.

  ###  box2D 2.1.0 version is so different from 2.0.1 version that Erin nearly
  overwrote all codes. For more information I recommend reading the following page
  http://box2dflash.boristhebrave.com/docs/2.1a/updating

  ###  I translated version 2.0.1 without considering much of class encapsulation so
  you can find many m_xxx class memebers left out in public field. In this version
  I fixed all things and you should use Set/Get procedures to access these members.
  But for some members owning both Set and Get procedures I changed them to
  properties. For example I simplify SetUserData and GetUserData to such code
     Property UserData: Pointer read m_userData write m_userData;
  Another thing is that I changed some reference declaration(delphi doesn't support)
  to pointer to avoid unnecessary memory copy. You can check Tb2Body.GetTransform
  and Tb2DynamicTree.GetFatAABB for more details.

  ###  Another difference from version 2.0.1 is that I changed Tb2Contact
  declaration from class to record for effciency. Because class is created on
  heap and Tb2Contact is so frequently created and destroyed.

  ###  Because versions before Delphi 2006 don't support operator overloading, so
  I write two versions of all math operations for vector and matrix, etc. But
  later I found that the version without operator overloading runs faster.
  So if you want a better performance, DEFINE BETTER_PERFORMANCE in Physics2D.inc
  which will UNDEFINE OP_OVERLOAD even if you are using Delphi 2010.

  ###  This library supports three kinds of floats, Single(32bit), Double(64bit) and
  Extended(80bit). Double precision is always recommended because I will test all
  codes and demos using double precision. Using other float systems may cause some
  unpredictable exceptions.
       flags        EXTENDED_PRECISION        DOUBLE_PRECISION
     Extended               ON                     whatever
      Double(default)       OFF                       ON
      Single                OFF                       OFF
  There is also a flag SINGLE_PRECISION in the include file but it doesn't affect
  Float type definition.

  ###  Controllers are added as enhancement and can be flagged by CONTROLLERS.
  If you don't need them, please unflag to reduce code size.

  ###  If you want to do benchmark or something else, please flag COMPUTE_PHYSICS_TIME.
  Time consumed by each step is updated and stored in Tb2World.GetPhysicsTime.

  ###  All assertions are ignored.

  Translator: Qianyuan Wang(ÍõÇ¬Ôª)
  Contact me: http://hi.baidu.com/wqyfavor
              wqyfavor@qq.com
              QQ: 466798985
              Tweet: t.qq.com/wqyfavor
}

interface
{$I Physics2D.inc}

{$IFDEF CONTROLLERS}
uses
   UPhysics2DTypes, UPhysics2D;

type
   Tb2BuoyancyController = class(Tb2Controller)
   public
      normal: TVector2; /// The outer surface normal
      offset: PhysicsFloat; /// The height of the fluid surface along the normal
      density: PhysicsFloat; /// The fluid density
      velocity: TVector2; /// Fluid velocity, for drag calculations

      /// Linear drag co-efficient
      linearDrag: PhysicsFloat;      /// Linear drag co-efficient
      angularDrag: PhysicsFloat;

      /// If false, bodies are assumed to be uniformly dense, otherwise use the shapes densities
      useDensity: Boolean; //False by default to prevent a gotcha

      useWorldGravity: Boolean; /// If true, gravity is taken from the world instead of the gravity parameter.
      gravity: TVector2; /// Gravity vector, if the world's gravity is not used

      constructor Create;
      procedure Step(const step: Tb2TimeStep); override;
      procedure Draw(debugDraw: Tb2Draw); override;
   end;

   // Apply constant acceleration to a body. You can change A at any time.
   Tb2ConstantAccelController = class(Tb2Controller)
   public
	    A: TVector2; /// The acceleration to apply
      procedure Step(const step: Tb2TimeStep); override;
   end;

   // Apply constant force to a body. You can change F at any time.
   Tb2ConstantForceController = class(Tb2Controller)
   public
	    F: TVector2; /// The force to apply
      procedure Step(const step: Tb2TimeStep); override;
   end;

   /// Applies simplified gravity between every pair of bodies.
   Tb2GravityController = class(Tb2Controller)
   public
	    G:PhysicsFloat; /// Specifies the strength of the gravitiation force
	    invSqr: Boolean; /// If true, gravity is proportional to r^-2, otherwise r^-1
      reject: Boolean; // If true, objects reject each other

      constructor Create;
      procedure Step(const step: Tb2TimeStep); override;
   end;

   /// Applies top down linear damping to the controlled bodies
   /// The damping is calculated by multiplying velocity by a matrix in local co-ordinates.
   Tb2TensorDampingController = class(Tb2Controller)
   public
      /// Tensor to use in damping model
      T: TMatrix22;
      {Some examples (matrixes in format (row1; row2) )
      (-a 0;0 -a)		Standard isotropic damping with strength a
      (0 a;-a 0)		Electron in fixed field - a force at right angles to velocity with proportional magnitude
      (-a 0;0 -b)		Differing x and y damping. Useful e.g. for top-down wheels.
      }
      //By the way, tensor in this case just means matrix, don't let the terminology get you down.

      /// Set this to a positive number to clamp the maximum amount of damping done.
      maxTimestep: PhysicsFloat;
      // Typically one wants maxTimestep to be 1/(max eigenvalue of T), so that damping will never cause something to reverse direction

      procedure Step(const step: Tb2TimeStep); override;
      procedure SetAxisAligned(xDamping, yDamping: PhysicsFloat);
   end;

   // Returns the wind vector. Contain information about both direction and magnitude
   Tb2WindForceCallback = function: TVector2;
   Tb2WindController = class(Tb2Controller)
   private
      function ComputePolygonShapeEffectiveForce(const windforce: TVector2;
         shape: Tb2PolygonShape; const xf: Tb2Transform): TVector2;
      function ComputeEdgeShapeEffectiveForce(const windforce: TVector2;
         shape: Tb2EdgeShape; const xf: Tb2Transform): TVector2;
   public
      Callback: Tb2WindForceCallback;
      procedure Step(const step: Tb2TimeStep); override;
   end;

   Tb2ExplosionController = class(Tb2Controller)
   private
      FExploded: Boolean;
      FTriggered: Boolean;
      FEnergySet: Boolean;
      FAutoFree: Boolean;
      FEnergy: PhysicsFloat; // All kinetic energy produced by explosion
      
      procedure FSetEnergy(value: PhysicsFloat);
      procedure RecalculateDefaultEnergy;
   public
      constructor Create;
      procedure Trigger;

      procedure AddBody(body: Tb2Body); override;
      procedure RemoveBody(body: Tb2Body); override;
      procedure Step(const step: Tb2TimeStep); override;
      procedure Reset;

      property Exploded: Boolean read FExploded; 
      property AutoFree: Boolean read FAutoFree write FAutoFree;
      property Energy: PhysicsFloat read FEnergy write FSetEnergy;
   end;

{$ENDIF}

implementation

{$IFDEF CONTROLLERS}
{ Tb2BuoyancyController }

constructor Tb2BuoyancyController.Create;
begin
   inherited;
   SetValue(normal, 0, 1);
   offset := 0;
   density := 1.0;
   velocity := b2Vec2_Zero;
   linearDrag := 0;
   angularDrag := 0;
   useDensity := True;
   useWorldGravity := True;
   gravity := b2Vec2_Zero;
end;

procedure Tb2BuoyancyController.Step(const step: Tb2TimeStep);
var
   i: Pb2ControllerEdge;
   body: Tb2Body;
   shape: Tb2Fixture;
   areac, massc, sc, localCentroid, buoyancyForce, dragForce: TVector2;
   area, mass, sarea, shapeDensity: PhysicsFloat;
begin
   //B2_NOT_USED(step);
   if not Assigned(m_bodyList) then
      Exit;
   if useWorldGravity then
      gravity := m_world.Gravity;

   i := m_bodyList;
   while Assigned(i) do
      with i^.body do
      begin
         if not IsAwake then
         begin
            //Buoyancy force is just a function of position,
            //so unlike most forces, it is safe to ignore sleeping bodies
            i := i^.nextBody;
            Continue;
         end;

         areac := b2Vec2_Zero;
         massc := b2Vec2_Zero;
         area := 0;
         mass := 0;
         shape := GetFixtureList;
         while Assigned(shape) do
         begin
            sarea := shape.GetShape.ComputeSubmergedArea(normal, offset, GetTransform^, sc);
            area := area + sarea;
            {$IFDEF OP_OVERLOAD}
            areac.AddBy(sc * sarea);
            {$ELSE}
            AddBy(areac, Multiply(sc, sarea));
            {$ENDIF}
            if useDensity then
            begin
               //TODO: Expose density publicly
               shapeDensity := shape.Density;
            end
            else
               shapeDensity := 1;
            mass := mass + sarea * shapeDensity;
            {$IFDEF OP_OVERLOAD}
            massc.AddBy((sarea * shapeDensity) * sc);
            {$ELSE}
            AddBy(massc, Multiply(sc, sarea * shapeDensity));
            {$ENDIF}
            shape := shape.GetNext;
         end;

         {$IFDEF OP_OVERLOAD}
         if area <> 0.0 then
          	areac.DivideBy(area);
         if mass <> 0.0 then
          	massc.DivideBy(mass);
         {$ELSE}
         if area <> 0.0 then
            DivideBy(areac, area);
         if mass <> 0.0 then
            DivideBy(massc, mass);
         {$ENDIF}
         localCentroid := b2MulT(GetTransform^, areac);
         if area < FLT_EPSILON then
         begin
            i := i^.nextBody;
            Continue;
         end;

         //Buoyancy
         {$IFDEF OP_OVERLOAD}
         buoyancyForce := -density * area * gravity;
         {$ELSE}
         buoyancyForce := Multiply(gravity, -density * area);
         {$ENDIF}
         ApplyForce(buoyancyForce, massc, True);

         //Linear drag
         {$IFDEF OP_OVERLOAD}
         dragForce := GetLinearVelocityFromWorldPoint(areac) - velocity;
         dragForce.MultiplyBy(-linearDrag * area);
         {$ELSE}
         dragForce := Subtract(GetLinearVelocityFromWorldPoint(areac), velocity);
         MultiplyBy(dragForce, -linearDrag * area);
         {$ENDIF}
         ApplyForce(dragForce, areac, True);
         //Angular drag
         //TODO: Something that makes more physical sense?
         ApplyTorque(-GetInertia / GetMass * area * GetAngularVelocity * angularDrag, True);
         i := i^.nextBody;
      end;
end;

procedure Tb2BuoyancyController.Draw(debugDraw: Tb2Draw);
const
   color: RGBA = (0, 0, 0.8, 1.0);
begin
   {$IFDEF OP_OVERLOAD}
   debugDraw.DrawSegment(offset * normal + b2Cross(normal, 1000),
      offset * normal - b2Cross(normal, 1000), color);
   {$ELSE}
   debugDraw.DrawSegment(Add(Multiply(normal, offset), b2Cross(normal, 1000)),
      Subtract(Multiply(normal, offset), b2Cross(normal, 1000)), color);
   {$ENDIF}
end;

{ Tb2ConstantAccelController }

procedure Tb2ConstantAccelController.Step(const step: Tb2TimeStep);
var
   i: Pb2ControllerEdge;
begin
   i := m_bodyList;
   while Assigned(i) do
      with i^.body do
      begin
         if IsAwake then
            {$IFDEF OP_OVERLOAD}
            SetLinearVelocity(GetLinearVelocity + step.dt * A);
            {$ELSE}
            SetLinearVelocity(Add(GetLinearVelocity, Multiply(A, step.dt)));
            {$ENDIF}
         i := i^.nextBody;
      end;
end;

{ Tb2ConstantForceController }

procedure Tb2ConstantForceController.Step(const step: Tb2TimeStep);
var
   i: Pb2ControllerEdge;
begin
   i := m_bodyList;
   while Assigned(i) do
      with i^.body do
      begin
         if IsAwake then
            ApplyForce(F, GetWorldCenter, True);
         i := i^.nextBody;
      end;
end;

{ Tb2GravityController }

constructor Tb2GravityController.Create;
begin
   inherited;
   G := 0.5;
   invSqr := True;
   reject := False;
end;

procedure Tb2GravityController.Step(const step: Tb2TimeStep);
var
   i, j: Pb2ControllerEdge;
   body1, body2: Tb2Body;
   d, f: TVector2;
   r2: PhysicsFloat;
begin
   //B2_NOT_USED(step);
   if invSqr then
   begin
      i := m_bodyList;
      while Assigned(i) do
      begin
         body1 := i^.body;
         j := m_bodyList;
         while j <> i do
         begin
            body2 := j^.body;
            {$IFDEF OP_OVERLOAD}
            d := body2.GetWorldCenter - body1.GetWorldCenter;
            r2 := d.SqrLength;
            {$ELSE}
            d := Subtract(body2.GetWorldCenter, body1.GetWorldCenter);
            r2 := SqrLength(d);
            {$ENDIF}
            if r2 < FLT_EPSILON then
            begin
               j := j^.nextBody;
               Continue;
            end;

            {$IFDEF OP_OVERLOAD}
            f := G / r2 / Sqrt(r2) * body1.GetMass * body2.GetMass * d;
            {$ELSE}
            f := Multiply(d, G / r2 / Sqrt(r2) * body1.GetMass * body2.GetMass);
            {$ENDIF}

            if reject then
               {$IFDEF OP_OVERLOAD}
               f := -f;
               {$ELSE}
               f := Negative(f);
               {$ENDIF}

            body1.ApplyForce(f, body1.GetWorldCenter, True);
            {$IFDEF OP_OVERLOAD}
            body2.ApplyForce(-1.0 * f, body2.GetWorldCenter, True);
            {$ELSE}
            body2.ApplyForce(Negative(f), body2.GetWorldCenter, True);
            {$ENDIF}
            j := j^.nextBody;
         end;
         i := i^.nextBody;
      end;
   end
   else
   begin
      i := m_bodyList;
      while Assigned(i) do
      begin
         body1 := i^.body;
         j := m_bodyList;
         while j <> i do
         begin
            body2 := j^.body;
            {$IFDEF OP_OVERLOAD}
            d := body2.GetWorldCenter - body1.GetWorldCenter;
            r2 := d.SqrLength;
            {$ELSE}
            d := Subtract(body2.GetWorldCenter, body1.GetWorldCenter);
            r2 := SqrLength(d);
            {$ENDIF}
            if r2 < FLT_EPSILON then
            begin
               j := j^.nextBody;
               Continue;
            end;

            {$IFDEF OP_OVERLOAD}
            f := G / r2 * body1.GetMass * body2.GetMass * d;
            {$ELSE}
            f := Multiply(d, G / r2 * body1.GetMass * body2.GetMass);
            {$ENDIF}

            if reject then
               {$IFDEF OP_OVERLOAD}
               f := -f;
               {$ELSE}
               f := Negative(f);
               {$ENDIF}

            body1.ApplyForce(f, body1.GetWorldCenter, True);
            {$IFDEF OP_OVERLOAD}
            body2.ApplyForce(-1.0 * f, body2.GetWorldCenter, True);
            {$ELSE}
            body2.ApplyForce(Negative(f), body2.GetWorldCenter, True);
            {$ENDIF}
            j := j^.nextBody;
         end;
         i := i^.nextBody;
      end;
   end;
end;

{ Tb2TensorDampingController }

procedure Tb2TensorDampingController.Step(const step: Tb2TimeStep);
var
   i: Pb2ControllerEdge;
   timestep: PhysicsFloat;
   damping: TVector2;
begin
   timestep := step.dt;
   if timestep <= FLT_EPSILON then
      Exit;
   if (timestep > maxTimestep) and (maxTimestep > 0) then
      timestep := maxTimestep;

   i := m_bodyList;
   while Assigned(i) do
      with i^.body do
      begin
         if IsAwake then
         begin
            damping := GetWorldVector(b2Mul(T, GetLocalVector(GetLinearVelocity)));
            {$IFDEF OP_OVERLOAD}
            SetLinearVelocity(GetLinearVelocity + timestep * damping);
            {$ELSE}
            SetLinearVelocity(Add(GetLinearVelocity, Multiply(damping, timestep)));
            {$ENDIF}
         end;
         i := i^.nextBody;
      end;
end;

procedure Tb2TensorDampingController.SetAxisAligned(xDamping, yDamping: PhysicsFloat);
begin
   T.ex.x := -xDamping;
   T.ex.y := 0;
   T.ey.x := 0;
   T.ey.y := -yDamping;
   if(xDamping > 0) or (yDamping > 0) then
      maxTimestep := 1 / b2Max(xDamping, yDamping)
   else
      maxTimestep := 0;
end;

{ Tb2WindController }

procedure Tb2WindController.Step(const step: Tb2TimeStep);
const
   DefaultWindForce: TVector2 = (X: 10; Y: 0); // From left to right
var
   i: Pb2ControllerEdge;
   shape: Tb2Fixture;
   windforce: TVector2;
   appliedforce: TVector2;
begin
   i := m_bodyList;
   if Assigned(Callback) then
      windforce := Callback()
   else
      windforce := DefaultWindForce;

   while Assigned(i) do
      with i^.body do
      begin
         if GetType <> b2_dynamicBody then
         begin
            i := i^.nextBody;
            Continue;
         end;

         appliedforce := b2Vec2_Zero;
         shape := GetFixtureList;
         while Assigned(shape) do
         begin
            if shape.GetType = e_circleShape then
               {$IFDEF OP_OVERLOAD}
               appliedforce.AddBy(windforce * Tb2CircleShape(shape.GetShape).m_radius * 2)
               {$ELSE}
               AddBy(appliedforce, Multiply(windforce, Tb2CircleShape(shape.GetShape).m_radius * 2))
               {$ENDIF}
            else if shape.GetType = e_polygonShape then
               {$IFDEF OP_OVERLOAD}
               appliedforce.AddBy(ComputePolygonShapeEffectiveForce(windforce,
                  Tb2PolygonShape(shape.GetShape), GetTransform^))
               {$ELSE}
               AddBy(appliedforce, ComputePolygonShapeEffectiveForce(windforce,
                  Tb2PolygonShape(shape.GetShape), GetTransform^))
               {$ENDIF}
            else if shape.GetType = e_edgeShape then
               {$IFDEF OP_OVERLOAD}
               appliedforce.AddBy(ComputeEdgeShapeEffectiveForce(windforce,
                  Tb2EdgeShape(shape.GetShape), GetTransform^));
               {$ELSE}
               AddBy(appliedforce, ComputeEdgeShapeEffectiveForce(windforce,
                  Tb2EdgeShape(shape.GetShape), GetTransform^));
               {$ENDIF}
            shape := shape.GetNext;
         end;

         ApplyForce(appliedforce, GetWorldCenter, True); // apply wind force
         i := i^.nextBody;
      end;
end;

procedure _ComputeSegmentEffectiveForce(const first, second: TVector2;
   normal: TVector2; const xf: Tb2Transform; const windforce: TVector2;
   var accumulated: TVector2);
var
   edge: TVector2;
begin
   normal := b2Mul(xf.q, normal); // to world coordinate
   {$IFDEF OP_OVERLOAD}
   edge := b2Mul(xf.q, second - first);
   {$ELSE}
   edge := b2Mul(xf.q, Subtract(second, first));
   {$ENDIF}
   if b2Dot(windforce, normal) < 0 then // this edge faces the wind
   begin
      if windforce.x > 0 then
         accumulated.x := accumulated.x + Abs(windforce.x * edge.y)
      else
         accumulated.x := accumulated.x - Abs(windforce.x * edge.y);

      if windforce.y > 0 then
         accumulated.y := accumulated.y + Abs(windforce.y * edge.x)
      else
         accumulated.y := accumulated.y - Abs(windforce.y * edge.x);
   end;
end;

function Tb2WindController.ComputePolygonShapeEffectiveForce(const windforce: TVector2;
   shape: Tb2PolygonShape; const xf: Tb2Transform): TVector2;
var
   i: Integer;
begin
   Result := b2Vec2_Zero;
   with shape do
   begin
      if m_count < 2 then
         Exit;
      for i := 1 to m_count - 1 do
         _ComputeSegmentEffectiveForce(m_vertices[i - 1], m_vertices[i],
            m_normals[i - 1], xf, windforce, Result);

      _ComputeSegmentEffectiveForce(m_vertices[m_count - 1],
         m_vertices[0], m_normals[m_count - 1], xf, windforce, Result);
   end;
end;

function Tb2WindController.ComputeEdgeShapeEffectiveForce(const windforce: TVector2;
   shape: Tb2EdgeShape; const xf: Tb2Transform): TVector2;
begin
   Result := b2Vec2_Zero;
   with shape do
   begin
      _ComputeSegmentEffectiveForce(m_vertex1, m_vertex2, m_normal1,
         xf, windforce, Result);

      _ComputeSegmentEffectiveForce(m_vertex2, m_vertex1, m_normal2,
         xf, windforce, Result);
   end;
end;

{ Tb2ExplosionController }

constructor Tb2ExplosionController.Create;
begin
   FExploded := False;
   FTriggered := False;
   FEnergySet := False;
   FAutoFree := False;
end;

procedure Tb2ExplosionController.FSetEnergy(value: PhysicsFloat);
begin
   FEnergy := value;
   FEnergySet := True;
end;

procedure Tb2ExplosionController.RecalculateDefaultEnergy;
var
   i: Pb2ControllerEdge;
begin
   i := m_bodyList;
   FEnergy := 0.0;
   Randomize;
   
   while Assigned(i) do
      with i^.body do
      begin
         FEnergy := FEnergy + GetMass * RandomFloat(100, 196);   
         i := i^.nextBody;
      end;
end;

procedure Tb2ExplosionController.Trigger;
begin
   FTriggered := True; // will explode in next step
end;

procedure Tb2ExplosionController.AddBody(body: Tb2Body);
begin
   if body.GetType <> b2_dynamicBody then // body must be dynamic
      Exit;
   inherited;
   if not FEnergySet then
      RecalculateDefaultEnergy;           
end;

procedure Tb2ExplosionController.RemoveBody(body: Tb2Body);
begin
   inherited;
   if not FEnergySet then
      RecalculateDefaultEnergy;
end;

procedure Tb2ExplosionController.Step(const step: Tb2TimeStep);
var
   i, j: Pb2ControllerEdge;
   body1, body2: Tb2Body;
   totalMass, nTotalMass, divEnergy, linEnergy, angEnergy, ang_impulse: PhysicsFloat;
   bodyCount: Int32;
   lin_impulse: TVector2;
begin
   if FExploded then
      Exit;
   if FTriggered then
   begin
      totalMass := 0;
      bodyCount := 0;

      i := m_bodyList;
      while Assigned(i) do
         with i^.body do
         begin
            Inc(bodyCount);
            totalMass := totalMass + GetMass;
            i := i^.nextBody;
         end;
      nTotalMass := (bodyCount - 1) * totalMass;
      
      i := m_bodyList;
      while Assigned(i) do
      begin
         body1 := i^.body;
         j := m_bodyList;
         while j <> i do
         begin
            body2 := j^.body;

            // Use random to add some chaos factor
            divEnergy := (body1.GetMass + body2.GetMass) / nTotalMass * FEnergy * RandomFloat(0.9, 1.1);
            angEnergy := divEnergy * RandomFloat(0.1, 0.2);
            linEnergy := divEnergy - angEnergy;

            {$IFDEF OP_OVERLOAD}
            lin_impulse := body1.GetWorldCenter - body2.GetWorldCenter;
            lin_impulse.SetLength(linEnergy);
            {$ELSE}
            lin_impulse := Subtract(body1.GetWorldCenter, body2.GetWorldCenter);
            SetLengthVec(lin_impulse, linEnergy);
            {$ENDIF}
            ang_impulse := angEnergy;
            if RandomFloat <= 0.5 then     
               ang_impulse := -ang_impulse;             

            // Use this method to keep principle of momentum conservation
            body1.ApplyLinearImpulse(lin_impulse, body1.GetWorldCenter, True);
            {$IFDEF OP_OVERLOAD}
            body2.ApplyLinearImpulse(-lin_impulse, body2.GetWorldCenter, True);
            {$ELSE}
            body2.ApplyLinearImpulse(Negative(lin_impulse), body2.GetWorldCenter, True);
            {$ENDIF}
            body1.ApplyAngularImpulse(ang_impulse, True);
            body2.ApplyAngularImpulse(-ang_impulse, True);
            j := j^.nextBody;
         end;
         i := i^.nextBody;
      end;      

      FExploded := True;
      if FAutoFree then
      begin
         m_world.RemoveController(Self); 
         Free;
      end;
   end;
end;

procedure Tb2ExplosionController.Reset;
begin
   FExploded := False;
   FTriggered := False;
   FEnergySet := False;            
   Clear;
end;

{$ENDIF}

end.
