unit UPhysics2D;

// April 2014
// -- removed Windows dependency
// -- added default value for "Wake" parameter in some Tb2Body methods for compatibility with older code
// -- minor fixes to compile OK with OP_OVERLOAD not defined

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
{$IFDEF D2009UP}
{$POINTERMATH ON}
{$DEFINE SUPPORT_POINTER_MATH}
{$ELSE}
{$UNDEF SUPPORT_POINTER_MATH}
{$ENDIF}

uses

{$IFDEF COMPUTE_PHYSICS_TIME}
  System.Diagnostics,
{$ENDIF}

{$IFDEF ENABLE_DUMP}
  System.SysUtils,
{$ENDIF}

  UPhysics2DTypes,
  System.Math,
  System.Classes;

type
   RGBA = array[0..3] of Single;
   TRGBA = packed record
      red, green, blue, alpha: Single;
   end;

   {$IFDEF ENABLE_DUMP}
   Tb2DumpMethod = procedure(Indent: Integer; const Format: string; const Args: array of const) of object;
   {$ENDIF}

   Tb2Exception = class(Exception)
   end;

   Tb2BodyDef = class;
   Pb2Body = ^Tb2Body;
   Tb2Body = class;
   Tb2JointDef = class;
   Pb2Joint = ^Tb2Joint;
   Tb2Joint = class;
   Pb2Contact = ^Tb2Contact;
   Pb2Fixture = ^Tb2Fixture;
   Tb2Fixture = class;
   Pb2Shape = ^Tb2Shape;
   Tb2Shape = class;
   Tb2BroadPhase = class;
   Tb2ContactFilter = class;
   Tb2ContactListener = class;
   Tb2ContactManager = class;
   Tb2QueryCallback = class;
   Tb2RayCastCallback = class;
   Tb2Island = class;
   {$IFDEF CONTROLLERS}
   Tb2Controller = class;
   {$ENDIF}

   /// Profiling data. Times are in milliseconds.
   Pb2Profile = ^Tb2Profile;
   Tb2Profile = record
      step: Float64;
      collide: Float64;
      solve: Float64;
      solveInit: Float64;
      solveVelocity: Float64;
      solvePosition: Float64;
      broadphase: Float64;
      solveTOI: Float64;
   end;

   /// The features that intersect to form the contact point
   /// This must be 4 bytes or less.
   Tb2ContactFeature = record
      indexA: UInt8; ///< Feature index on shapeA
      indexB: UInt8; ///< Feature index on shapeB
      typeA: UInt8;  ///< The feature type on shapeA
      typeB: UInt8;  ///< The feature type on shapeB
   end;

   Tb2ContactID = record
      /// The features that intersect to form the contact point
      case Integer of
         0: (cf: Tb2ContactFeature);
         1: (key: UInt32); ///< Used to quickly compare contact ids.
   end;

   /// A distance proxy is used by the GJK algorithm.
   /// It encapsulates any shape.
   Pb2DistanceProxy = ^Tb2DistanceProxy;
   Tb2DistanceProxy = record
      m_vertices: PVector2;
      m_count: Int32;
      m_radius: PhysicsFloat;
      m_buffer: array[0..1] of TVector2;

      {$IFDEF OP_OVERLOAD}
      /// Initialize the proxy using the given shape. The shape
      /// must remain in scope while the proxy is in use.
      procedure SetShape(shape: Tb2Shape; index: Int32);

      /// Get the supporting vertex index in the given direction.
      function GetSupport(const d: TVector2): Int32;

      /// Get the supporting vertex in the given direction.
      function GetSupportVertex(const d: TVector2): PVector2;
      {$ENDIF}
   end;

   /// Input for b2Distance.
   /// You have to option to use the shape radii in the computation. Even
   Tb2DistanceInput = record
      proxyA, proxyB: Tb2DistanceProxy;
      transformA, transformB: Tb2Transform;
      useRadii: Boolean;
   end;

   /// Output for b2Distance.
   Tb2DistanceOutput = record
      pointA,            ///< closest point on shapeA
      pointB: TVector2;  ///< closest point on shapeB
      distance: PhysicsFloat;
      iterations: Int32; ///< number of GJK iterations used
   end;

   /// Used to warm start b2Distance.
   /// Set count to zero on first call.
   Tb2SimplexCache = record
      metric: PhysicsFloat;		///< length or area
      count: UInt16;
      indexA,
      indexB: array[0..2] of UInt8; /// vertices on shape A an B
   end;

   /// Input parameters for b2TimeOfImpact
   Tb2TOIInput = record
      proxyA, proxyB: Tb2DistanceProxy;
      sweepA, sweepB: Tb2Sweep;
      tMax: PhysicsFloat; // defines sweep interval [0, tMax]
   end;

   // Output parameters for b2TimeOfImpact.
   Tb2TOIOutputState = (e_toi_unknown, e_toi_failed, e_toi_overlapped,
      e_toi_touching, e_toi_separated);
   Tb2TOIOutput = record
      state: Tb2TOIOutputState;
      t: PhysicsFloat;
   end;

   /// A manifold point is a contact point belonging to a contact
   /// manifold. It holds details related to the geometry and dynamics
   /// of the contact points.
   /// The local point usage depends on the manifold type:
   /// -e_circles: the local center of circleB
   /// -e_faceA: the local center of cirlceB or the clip point of polygonB
   /// -e_faceB: the clip point of polygonA
   /// This structure is stored across time steps, so we keep it small.
   /// Note: the impulses are used for internal caching and may not
   /// provide reliable contact forces, especially for high speed collisions.
   Pb2ManifoldPoint = ^Tb2ManifoldPoint;
   Tb2ManifoldPoint = record
      localPoint: TVector2;		///< usage depends on manifold type
      normalImpulse: PhysicsFloat; ///< the non-penetration impulse
      tangentImpulse: PhysicsFloat; ///< the friction impulse
      id: Tb2ContactID; ///< uniquely identifies a contact point between two shapes
   end;

   /// A manifold for two touching convex shapes.
   /// Box2D supports multiple types of contact:
   /// - clip point versus plane with radius
   /// - point versus point with radius (circles)
   /// The local point usage depends on the manifold type:
   /// -e_circles: the local center of circleA
   /// -e_faceA: the center of faceA
   /// -e_faceB: the center of faceB
   /// Similarly the local normal usage:
   /// -e_circles: not used
   /// -e_faceA: the normal on polygonA
   /// -e_faceB: the normal on polygonB
   /// We store contacts in this way so that position correction can
   /// account for movement, which is critical for continuous physics.
   /// All contact scenarios must be expressed in one of these types.
   /// This structure is stored across time steps, so we keep it small.
   Tb2ManifoldType = (e_manifold_circles, e_manifold_faceA, e_manifold_faceB);
   Pb2Manifold = ^Tb2Manifold;
   Tb2Manifold = record
      points: array[0..b2_maxManifoldPoints - 1] of Tb2ManifoldPoint; ///< the points of contact

      localNormal: TVector2;								///< not use for Type::e_points
      localPoint: TVector2;								///< usage depends on manifold type
      manifoldType: Tb2ManifoldType;
      pointCount: Int32;								///< the number of manifold points
   end;

   /// This is used to compute the current state of a contact manifold.
   Tb2WorldManifold = record
    	normal: TVector2;						///< world vector pointing from A to B
      points: array[0..b2_maxManifoldPoints - 1] of TVector2; ///< world contact point (point of intersection)
      separations: array[0..b2_maxManifoldPoints - 1] of PhysicsFloat; ///< a negative value indicates overlap, in meters

      {$IFDEF OP_OVERLOAD}
      /// Evaluate the manifold with supplied transforms. This assumes
      /// modest motion from the original state. This does not change the
      /// point count, impulses, etc. The radii must come from the shapes
      /// that generated the manifold.
      procedure Initialize(const manifold: Tb2Manifold; const xfA, xfB: Tb2Transform;
         radiusA, radiusB: PhysicsFloat);
      {$ENDIF}
   end;

   /// This is used for determining the state of contact points.
	 /// b2_nullState,		///< point does not exist
	 /// b2_addState,	   	///< point was added in the update
	 /// b2_persistState,	///< point persisted across the update
	 /// b2_removeState		///< point was removed in the update
   Tb2PointState = (b2_nullState, b2_addState, b2_persistState, b2_removeState);
   Tb2PointStateArray = array[0..b2_maxManifoldPoints - 1] of Tb2PointState;

   /// Ray-cast input data.
   Tb2RayCastInput = record
      p1, p2: TVector2;
      maxFraction: PhysicsFloat;
   end;

   /// Ray-cast output data.
   Tb2RayCastOutput = record
      normal: TVector2;
      fraction: PhysicsFloat;
   end;

   /// An axis aligned bounding box.
   Pb2AABB = ^Tb2AABB;
   Tb2AABB = record
      lowerBound, upperBound: TVector2; // The lower and upper vertices

      {$IFDEF OP_OVERLOAD}
    	function IsValid: Boolean; {$IFDEF INLINE_AVAIL}inline;{$ENDIF} /// Verify that the bounds are sorted.
      function GetCenter: TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF} /// Get the center of the AABB.
      function GetExtents: TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF} /// Get the extents of the AABB (half-widths).
    	function GetPerimeter: PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF} /// Get the perimeter length
      procedure Combine(const aabb: Tb2AABB); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF} /// Combine an AABB into this one.
      procedure Combine(const aabb1, aabb2: Tb2AABB); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF} /// Combine two AABBs into this one.
      function Contains(const aabb: Tb2AABB): Boolean; /// Does this aabb contain the provided AABB.
      function RayCast(var output: Tb2RayCastOutput; const input: Tb2RayCastInput): Boolean;
      {$ENDIF}
   end;

   Pb2PolyVertices = ^Tb2PolyVertices;
   Tb2PolyVertices = array[0..b2_maxPolygonVertices - 1] of TVector2;

   //////////////////////////////////////////////////////////////
   // World

   Tb2TimeStep = record
      dt: PhysicsFloat; // time step
      inv_dt: PhysicsFloat; // inverse time step (0 if dt == 0).
	    dtRatio: PhysicsFloat; // dt * inv_dt0
      velocityIterations, positionIterations: Int32;
      warmStarting: Boolean;
   end;

   /// Joints and fixtures are destroyed when their associated
   /// body is destroyed. Implement this listener so that you
   /// may nullify references to these joints and shapes.
   Tb2DestructionListener = class
   public
      /// Called when any fixtures is about to be destroyed due
      /// to the destruction of one of its attached bodies.
      procedure SayGoodbye(joint: Tb2Joint); overload; virtual; abstract;

      /// Called when any shape is about to be destroyed due
      /// to the destruction of its parent body.
      procedure SayGoodbye(fixture: Tb2Fixture); overload; virtual; abstract;
   end;

   Tb2DrawBits = (e_shapeBit, e_jointBit, e_aabbBit, e_pairBit,
      e_centerOfMassBit{$IFDEF CONTROLLERS}, e_controllerBit{$ENDIF});
   Tb2DrawBitsSet = set of Tb2DrawBits;

   Tb2Draw = class
   public
      m_drawFlags: Tb2DrawBitsSet;
      m_shapeColor_Inactive, m_shapeColor_Static, m_shapeColor_Kinematic,
      m_shapeColor_Sleeping, m_shapeColor_Normal,
      m_pairColor, m_aabbColor, m_world_aabbColor, m_coreColor, m_jointLineColor: RGBA;

      constructor Create;

      procedure DrawPolygon(const vertices: Tb2PolyVertices; vertexCount: Int32; const color: RGBA); virtual; abstract;
      procedure DrawPolygon4(const vertices: TVectorArray4; vertexCount: Int32; const color: RGBA); virtual; abstract;
      procedure DrawSolidPolygon(const vertices: Tb2PolyVertices; vertexCount: Int32; const color: RGBA); virtual; abstract;
      procedure DrawCircle(const center: TVector2; radius: PhysicsFloat; const color: RGBA); virtual; abstract;
      procedure DrawSolidCircle(const center, axis: TVector2; radius: PhysicsFloat; const color: RGBA); virtual; abstract;
      procedure DrawSegment(const p1, p2: TVector2; const color: RGBA); virtual; abstract;
      procedure DrawTransform(const xf: Tb2Transform); virtual; abstract;
   end;

   // Simulate c++ template feature
   Tb2GenericCallBackWrapper = class
   public
      function QueryCallback(proxyId: Int32): Boolean; virtual; abstract;
      function RayCastCallback(const input: Tb2RayCastInput; proxyId: Int32): PhysicsFloat; virtual; abstract;
   end;

   /// The world class manages all physics entities, dynamic simulation,
   /// and asynchronous queries. The world also contains efficient memory
   /// management facilities.
   Tb2World = class
   private
      m_flags: UInt16;

      m_contactManager: Tb2ContactManager;

      m_bodyList: Tb2Body;
      m_jointList: Tb2Joint;

      {$IFDEF CONTROLLERS}
      m_controllerList: Tb2Controller;
      m_controllerCount: Int32;
      {$ENDIF}

      m_bodyCount,
      m_jointCount: Int32;

      m_gravity: TVector2;
      m_allowSleep: Boolean;

      m_destructionListener: Tb2DestructionListener;
      m_debugDraw: Tb2Draw;

      m_inv_dt0: PhysicsFloat;  // This is used to compute the time step ratio to support a variable time step.
      m_warmStarting: Boolean;
      m_continuousPhysics: Boolean; // For debugging the solver

	    m_subStepping: Boolean;
	    m_stepComplete: Boolean;

      {$IFDEF COMPUTE_PHYSICS_TIME}
      m_profile: Tb2Profile;
      {$ENDIF}

      procedure Solve(const step: Tb2TimeStep);

      { Sequentially solve TOIs for each body. We bring each body to the time
        of contact and perform some position correction. Time is not conserved.}
      procedure SolveTOI(const step: Tb2TimeStep);

      procedure DrawShape(fixture: Tb2Fixture; const xf: Tb2Transform; const color: RGBA);
      procedure DrawJoint(joint: Tb2Joint);

      {$IFDEF COMPUTE_PHYSICS_TIME}
      function FGetProfile: Pb2Profile;
      {$ENDIF}

      procedure FSetAllowSleeping(value: Boolean);
   public
      /// Construct a world object.
      /// @param gravity the world gravity vector.
      constructor Create(const gravity: TVector2);

      /// Destruct the world. All physics entities are destroyed and all heap memory is released.
      destructor Destroy; override;

      {$IFDEF ENABLE_DUMP}
      /// Dump the world into the log file.
      /// @warning this should be called outside of a time step.
      procedure SetDumpMethod(method: Tb2DumpMethod);
      procedure Dump;
      {$ENDIF}

      /// Register a contact filter to provide specific control over collision.
      /// Otherwise the default filter is used (b2_defaultFilter). The listener is
      /// owned by you and must remain in scope.
      procedure SetContactFilter(filter: Tb2ContactFilter);

      /// Register a contact event listener. The listener is owned by you and must remain in scope.
     	procedure SetContactListener(listener: Tb2ContactListener);

      /// Create a rigid body given a definition. No reference to the definition is retained.
      /// @warning This function is locked during callbacks.
      function CreateBody(def: Tb2BodyDef; AutoFreeBodyDef: Boolean = True): Tb2Body;

      /// Destroy a rigid body given a definition. No reference to the definition
      /// is retained. This function is locked during callbacks.
      /// @warning This automatically deletes all associated shapes and joints.
      /// @warning This function is locked during callbacks.
      procedure DestroyBody(body: Tb2Body; DoFree: Boolean = True);

      /// Create a joint to constrain bodies together. No reference to the definition
      /// is retained. This may cause the connected bodies to cease colliding.
      /// @warning This function is locked during callbacks.
      function CreateJoint(def: Tb2JointDef; AutoFreeJointDef: Boolean = True): Tb2Joint;

      /// Destroy a joint. This may cause the connected bodies to begin colliding.
      /// @warning This function is locked during callbacks.
      procedure DestroyJoint(j: Tb2Joint);

      {$IFDEF CONTROLLERS}
      procedure AddController(c: Tb2Controller);
      procedure RemoveController(c: Tb2Controller);
      {$ENDIF}

      /// Take a time step. This performs collision detection, integration,
      /// and constraint solution.
      /// @param timeStep the amount of time to simulate, this should not vary.
      /// @param velocityIterations for the velocity constraint solver.
      /// @param positionIterations for the position constraint solver.
      procedure Step(timeStep: PhysicsFloat; velocityIterations,
         positionIterations: Int32; Draw: Boolean = False);

      /// Manually clear the force buffer on all bodies. By default, forces are cleared automatically
      /// after each call to Step. The default behavior is modified by calling SetAutoClearForces.
      /// The purpose of this function is to support sub-stepping. Sub-stepping is often used to maintain
      /// a fixed sized time step under a variable frame-rate.
      /// When you perform sub-stepping you will disable auto clearing of forces and instead call
      /// ClearForces after all sub-steps are complete in one pass of your game loop.
      /// @see SetAutoClearForces
      procedure ClearForces;

      /// Call this to draw shapes and other debug draw data.
      procedure DrawDebugData;

      /// Query the world for all fixtures that potentially overlap the
      /// provided AABB.
      /// @param callback a user implemented callback class.
      /// @param aabb the query box.
      procedure QueryAABB(callback: Tb2QueryCallback; const aabb: Tb2AABB);

      /// Ray-cast the world for all fixtures in the path of the ray. Your callback
      /// controls whether you get the closest point, any point, or n-points.
      /// The ray-cast ignores shapes that contain the starting point.
      /// @param callback a user implemented callback class.
      /// @param point1 the ray starting point
      /// @param point2 the ray ending point
      procedure RayCast(callback: Tb2RayCastCallback; const point1, point2: TVector2);

      /// Get the world contact list. With the returned contact, use b2Contact::GetNext to get
      /// the next contact in the world list. A NULL contact indicates the end of the list.
      /// @return the head of the world contact list.
      /// @warning contacts are created and destroyed in the middle of a time step.
      /// Use b2ContactListener to avoid missing contacts.
      function GetContactList: Pb2Contact; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      function GetContactCount: Int32; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Get the height of the dynamic tree.
	    function GetTreeHeight: Int32;

	    /// Get the balance of the dynamic tree.
	    function GetTreeBalance: Int32;

	    /// Get the quality metric of the dynamic tree. The smaller the better.
	    /// The minimum is 1.
    	function GetTreeQuality: PhysicsFloat;

      function GetProxyCount: Int32; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}/// Get the number of broad-phase proxies.

      /// Change the global gravity vector.
      procedure SetGravity(const gravity: TVector2); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure WakeAllSleepingBodies;

      /// Is the world locked (in the middle of a time step).
      function IsLocked: Boolean; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure SetAutoClearForces(flag: Boolean); /// Set flag to control automatic clearing of forces after each time step.

      /// Get the flag that controls automatic clearing of forces after each time step.
      function GetAutoClearForces: Boolean; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Shift the world origin. Useful for large worlds.
		 	/// The body shift formula is: position -= newOrigin
		 	/// @param newOrigin the new origin with respect to the old origin
		 	procedure ShiftOrigin(const newOrigin: TVector2);

      //////////////////////////////////////////////////////////////////////
      property DestructionListener: Tb2DestructionListener read m_destructionListener write m_destructionListener;
      property Draw: Tb2Draw read m_debugDraw write m_debugDraw;
      property GetContactManager: Tb2ContactManager read m_contactManager;

      property Gravity: TVector2 read m_gravity;
      property GetBodyList: Tb2Body read m_bodyList;
      property GetJointList: Tb2Joint read m_jointList;
      property GetBodyCount: Int32 read m_bodyCount;
      property GetJointCount: Int32 read m_jointCount;

      property AllowSleeping: Boolean read m_allowSleep write FSetAllowSleeping;
      property WarmStarting: Boolean read m_warmStarting write m_warmStarting;
      property ContinuousPhysics: Boolean read m_continuousPhysics write m_continuousPhysics;

      property SubStepping: Boolean read m_subStepping write m_subStepping;

      {$IFDEF COMPUTE_PHYSICS_TIME}
      property Profile: Pb2Profile read FGetProfile;
      {$ENDIF}
   end;

   ////////////////////////////////////////////////////
   // Contact

   /// A contact edge is used to connect bodies and contacts together
   /// in a contact graph where each body is a node and each contact
   /// is an edge. A contact edge belongs to a doubly linked list
   /// maintained in each attached body. Each contact has two contact
   /// nodes, one for each attached body.
   Pb2ContactEdge = ^Tb2ContactEdge;
   Tb2ContactEdge = record
      other: Tb2Body;	///< provides quick access to the other body attached.
      contact: Pb2Contact; ///< the contact
      prev, next: Pb2ContactEdge;
   end;

   Pb2VelocityConstraintPoint = ^Tb2VelocityConstraintPoint;
   Tb2VelocityConstraintPoint = record
      rA, rB: TVector2;
      normalImpulse, tangentImpulse, normalMass, tangentMass, velocityBias: PhysicsFloat;
   end;

   Pb2ContactVelocityConstraint = ^Tb2ContactVelocityConstraint;
   Tb2ContactVelocityConstraint = record
      points: array[0..b2_maxManifoldPoints - 1] of Tb2VelocityConstraintPoint;
      normal: TVector2;
      normalMass, K: TMatrix22;

      indexA, indexB: Int32;
      invMassA, invMassB: PhysicsFloat;
      invIA, invIB: PhysicsFloat;

      restitution: PhysicsFloat;
      friction: PhysicsFloat;
      tangentSpeed: PhysicsFloat;

      pointCount: Int32;
      contactIndex: Int32;
   end;

   Pb2ContactPositionConstraint = ^Tb2ContactPositionConstraint;
   Tb2ContactPositionConstraint = record
      localPoints: array[0..b2_maxManifoldPoints - 1] of TVector2;
      localNormal: TVector2;
      localPoint: TVector2;
      indexA, indexB: Int32;
      invMassA, invMassB: PhysicsFloat;
      localCenterA, localCenterB: TVector2;
      invIA, invIB: PhysicsFloat;
      manifoldType: Tb2ManifoldType;
      radiusA, radiusB: PhysicsFloat;
      pointCount: Int32;
   end;

   /// The class manages contact between two shapes. A contact exists for each overlapping
   /// AABB in the broad-phase (except if filtered). Therefore a contact object may exist
   /// that has no contact points.
   //Tb2ContactType = (b2_circleContact, b2_circlepolyContact, b2_polypolyContact);
   Tb2ContactEvaluateProc = procedure(contact: Pb2Contact; var manifold: Tb2Manifold;
      A, B: TObject; const xfA, xfB: Tb2Transform; ABfixture: Boolean);

   Tb2Contact = record
      //m_type: Tb2ContactType;
      m_flags: UInt16;

      m_prev, m_next: Pb2Contact; // World pool and list pointers.
      m_nodeA, m_nodeB: Tb2ContactEdge; // Nodes for connecting bodies.
      m_fixtureA, m_fixtureB: Tb2Fixture;
      m_indexA, m_indexB: Int32;

      m_manifold: Tb2Manifold;
      m_toiCount: Int32;
      m_toi: PhysicsFloat;

      m_friction: PhysicsFloat;
      m_restitution: PhysicsFloat;

      m_tangentSpeed: PhysicsFloat;

      m_evaluateProc: Tb2ContactEvaluateProc; // For different contacts

      {$IFDEF OP_OVERLOAD}
      procedure Update(listener: Tb2ContactListener);
      /// Get the contact manifold. Do not modify the manifold unless you understand the
      /// internals of Box2D.
      function GetManifold: Pb2Manifold; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Get the world manifold.
      procedure GetWorldManifold(var worldManifold: Tb2WorldManifold); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

    	/// Flag this contact for filtering. Filtering will occur the next time step.
     	procedure FlagForFiltering; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Is this contact touching?
      function IsTouching: Boolean; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Reset the friction mixture to the default value.
      procedure ResetFriction; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Reset the restitution to the default value.
      procedure ResetRestitution; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Enable/disable this contact. This can be used inside the pre-solve
      /// contact listener. The contact is only disabled for the current
      /// time step (or sub-step in continuous collisions).
      procedure SetEnabled(flag: Boolean);
      function IsEnabled: Boolean; {$IFDEF INLINE_AVAIL}inline;{$ENDIF} /// Has this contact been disabled?
      {$ENDIF}
   end;

   /// Contact impulses for reporting. Impulses are used instead of forces because
   /// sub-step forces may approach infinity for rigid body collisions. These
   /// match up one-to-one with the contact points in b2Manifold.
   Tb2ContactImpulse = record
      normalImpulses: array[0..b2_maxManifoldPoints - 1] of PhysicsFloat;
      tangentImpulses: array[0..b2_maxManifoldPoints - 1] of PhysicsFloat;
      count: Int32;
   end;

   /// Implement this class to provide collision filtering. In other words,
   /// you can implement this class if you want finer control over contact creation.
   Tb2ContactFilter = class
   public
      /// Return True if contact calculations should be performed between
      /// these two shapes. @warning for performance reasons this is only
      /// called when the AABBs begin to overlap.
      function ShouldCollide(fixtureA, fixtureB: Tb2Fixture): Boolean; virtual;
   end;

   /// Implement this class to get contact information. You can use these results for
   /// things like sounds and game logic. You can also get contact results by
   /// traversing the contact lists after the time step. However, you might miss
   /// some contacts because continuous physics leads to sub-stepping.
   /// Additionally you may receive multiple callbacks for the same contact in a
   /// single time step.
   /// You should strive to make your callbacks efficient because there may be
   /// many callbacks per time step.
   /// @warning You cannot create/destroy Box2D entities inside these callbacks.
   Tb2ContactListener = class(Tb2GenericCallBackWrapper)
   public
      /// Called when two fixtures begin to touch.
      procedure BeginContact(var contact: Tb2Contact); virtual;

      /// Called when two fixtures cease to touch.
      procedure EndContact(var contact: Tb2Contact); virtual;

      /// This is called after a contact is updated. This allows you to inspect a
      /// contact before it goes to the solver. If you are careful, you can modify the
      /// contact manifold (e.g. disable contact).
      /// A copy of the old manifold is provided so that you can detect changes.
      /// Note: this is called only for awake bodies.
      /// Note: this is called even when the number of contact points is zero.
      /// Note: this is not called for sensors.
      /// Note: if you set the number of contact points to zero, you will not
      /// get an EndContact callback. However, you may get a BeginContact callback
      /// the next step.
      procedure PreSolve(var contact: Tb2Contact; const oldManifold: Tb2Manifold); virtual;

      /// This lets you inspect a contact after the solver is finished. This is useful
      /// for inspecting impulses.
      /// Note: the contact manifold does not include time of impact impulses, which can be
      /// arbitrarily large if the sub-step is small. Hence the impulse is provided explicitly
      /// in a separate data structure.
      /// Note: this is only called for contacts that are touching, solid, and awake.
      procedure PostSolve(var contact: Tb2Contact; const impulse: Tb2ContactImpulse); virtual;
   end;

   /// Callback class for AABB queries.
   /// See b2World::Query
   Tb2QueryCallback = class
   public
      /// Called for each fixture found in the query AABB.
      /// @return false to terminate the query.
      function ReportFixture(fixture: Tb2Fixture): Boolean; virtual; abstract;
   end;

   /// Callback class for ray casts. See b2World::RayCast
   Tb2RayCastCallback = class
   public
      /// Called for each fixture found in the query. You control how the ray cast
      /// proceeds by returning a float:
      /// return -1: ignore this fixture and continue
      /// return 0: terminate the ray cast
      /// return fraction: clip the ray to this point
      /// return 1: don't clip the ray and continue
      /// @param fixture the fixture hit by the ray
      /// @param point the point of initial intersection
      /// @param normal the normal vector at the point of intersection
      /// @return -1 to filter, 0 to terminate, fraction to clip the ray for
      /// closest hit, 1 to continue
      function ReportFixture(fixture:	Tb2Fixture; const point, normal: TVector2;
         fraction: PhysicsFloat): PhysicsFloat; virtual; abstract;
   end;

   /// This is an internal structure.
   Tb2Position = record
      c: TVector2;
      a: PhysicsFloat;
   end;
   Tb2Positions = array of Tb2Position;

   /// This is an internal structure.
   Tb2Velocity = record
      v: TVector2;
      w: PhysicsFloat;
   end;
   Tb2Velocities = array of Tb2Velocity;

   Tb2ContactSolver = class
   public
      m_step: Tb2TimeStep;
      m_positions: Tb2Positions;
      m_velocities: Tb2Velocities;
      m_positionConstraints: Pb2ContactPositionConstraint;
      m_velocityConstraints: Pb2ContactVelocityConstraint;
      m_contacts: TList;
      m_count: Int32;

      destructor Destroy; override;

      procedure Initialize(const step: Tb2TimeStep;
         positions: Tb2Positions; velocities: Tb2Velocities;
         contacts: TList; count: Int32);

      procedure InitializeVelocityConstraints;
      procedure WarmStart;
      procedure SolveVelocityConstraints;
      procedure StoreImpulses;

      function SolvePositionConstraints: Boolean;
      function SolveTOIPositionConstraints(toiIndexA, toiIndexB: Int32): Boolean;
   end;

   // Delegate of b2World.
   Tb2ContactManager = class
   public
      m_broadPhase: Tb2BroadPhase;

      m_contactList: Pb2Contact;
      m_contactCount: Int32;

      m_contactFilter: Tb2ContactFilter;
      m_contactListener: Tb2ContactListener;

      constructor Create;
      destructor Destroy; overload; override;

      // Broad-phase callback.
      procedure AddPair(proxyUserDataA, proxyUserDataB: Pointer);
      procedure FindNewContacts; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure Destroy(pc: Pb2Contact); overload;
      procedure Collide;
   end;

   ////////////////////////////////////////////////////
   // Island

   /// Solver Data
   Tb2SolverData = record
      step: Tb2TimeStep;
      positions: Tb2Positions;
      velocities: Tb2Velocities;
   end;

   Tb2Island = class
   private
      procedure Reset(bodyCapacity, contactCapacity, jointCapacity: Int32;
         listener: Tb2ContactListener); // Reset island solver so that instance won't be recreated over and over again
   public
      m_listener: Tb2ContactListener;

      m_bodies: TList;
      m_contacts: TList;
      m_joints: TList;

      m_positions: Tb2Positions;
      m_velocities: Tb2Velocities;

      m_bodyCount: Int32;
      m_jointCount: Int32;
      m_contactCount: Int32;

      m_bodyCapacity, m_contactCapacity, m_jointCapacity: Int32;

      constructor Create;
      destructor Destroy; override;

      procedure Clear;
      procedure Solve(
          {$IFDEF COMPUTE_PHYSICS_TIME}var profile: Tb2Profile;{$ENDIF}
          const step: Tb2TimeStep;
          const gravity: TVector2; allowSleep: Boolean);
      procedure SolveTOI(const subStep: Tb2TimeStep; toiIndexA, toiIndexB: Int32);

      procedure Add(body: Tb2Body); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure Add(contact: Pb2Contact); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure Add(joint: Tb2Joint); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      procedure Report(constraints: Pb2ContactVelocityConstraint);
   end;

   Pb2TOIConstraint = ^Tb2TOIConstraint;
   Tb2TOIConstraint = record
      localPoints: array[0..b2_maxManifoldPoints - 1] of TVector2;
      localNormal, localPoint: TVector2;
      manifoldType: Tb2ManifoldType;
      radius: PhysicsFloat;
      pointCount: Int32;
      bodyA, bodyB: Tb2Body;
   end;

   ////////////////////////////////////////////////////
   Pb2Pair = ^Tb2Pair;
   Tb2Pair = record
      proxyIdA: Int32;
      proxyIdB: Int32;
   end;

{$DEFINE B2_USE_DYNAMIC_TREE}

{$IFDEF B2_USE_DYNAMIC_TREE}

   /// A node in the dynamic tree. The client does not interact with this directly.
   Pb2TreeNode = ^Tb2TreeNode;
   Tb2TreeNode = record
   {$IFDEF OP_OVERLOAD}
   public
      function IsLeaf: Boolean; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
   public
   {$ENDIF}
    	aabb: Tb2AABB; /// Enlarged AABB
      userData: Pointer;
      child1, child2: Int32;
      height: Int32; // leaf = 0, free node = -1
      case Byte of
         0: (parent: Int32);
         1: (next: Int32);
   end;

   /// A dynamic AABB tree broad-phase, inspired by Nathanael Presson's btDbvt.
   /// A dynamic tree arranges data in a binary tree to accelerate
   /// queries such as volume queries and ray casts. Leafs are proxies
   /// with an AABB. In the tree we expand the proxy AABB by b2_fatAABBFactor
   /// so that the proxy AABB is bigger than the client object. This allows the client
   /// object to move by small amounts without triggering a tree update.
   ///
   /// Nodes are pooled and relocatable, so we use node indices rather than pointers.
   Tb2DynamicTree = class
   private
      m_root: Int32;
      m_nodes: array of Tb2TreeNode;
      m_nodeCount, m_nodeCapacity: Int32;
      m_freeList: Int32;
      m_path: UInt32; /// This is used to incrementally traverse the tree for re-balancing.
      m_insertionCount: Int32;

      function AllocateNode: Int32;
      procedure FreeNode(nodeId: Int32);

      procedure InsertLeaf(leaf: Int32);
      procedure RemoveLeaf(leaf: Int32);

      function Balance(iA: Int32): Int32;

      function ComputeHeight: Int32; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      function ComputeHeight(nodeId: Int32): Int32; overload;

      procedure ValidateStructure(index: Int32);
	    procedure ValidateMetrics(index: Int32);

   public
      constructor Create;

      procedure SetCapacity(value: Int32);

      /// Create a proxy. Provide a tight fitting AABB and a userData pointer.
      function CreateProxy(const _aabb: Tb2AABB; _userData: Pointer): Int32;

      /// Destroy a proxy. This asserts if the id is invalid.
      procedure DestroyProxy(proxyId: Int32); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Move a proxy with a swepted AABB. If the proxy has moved outside of its fattened AABB,
      /// then the proxy is removed from the tree and re-inserted. Otherwise
      /// the function returns immediately.
      /// @return true if the proxy was re-inserted.
      function MoveProxy(proxyId: Int32; const aabb: Tb2AABB;
         const displacement: TVector2): Boolean;

      /// Get proxy user data.
      /// @return the proxy user data or 0 if the id is invalid.
      function GetUserData(proxyId: Int32): Pointer; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Get the fat AABB for a proxy.
      function GetFatAABB(proxyId: Int32): Pb2AABB; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Query an AABB for overlapping proxies. The callback class
      /// is called for each proxy that overlaps the supplied AABB.
      procedure Query(callback: Tb2GenericCallBackWrapper; const _aabb: Tb2AABB);

      /// Ray-cast against the proxies in the tree. This relies on the callback
      /// to perform a exact ray-cast in the case were the proxy contains a shape.
      /// The callback also performs the any collision filtering. This has performance
      /// roughly equal to k * log(n), where k is the number of collisions and n is the
      /// number of proxies in the tree.
      /// @param input the ray-cast input data. The ray extends from p1 to p1 + maxFraction * (p2 - p1).
      /// @param callback a callback class that is called for each proxy that is hit by the ray.
      procedure RayCast(callback: Tb2GenericCallBackWrapper; const input: Tb2RayCastInput);

      /// Validate this tree. For testing.
      procedure Validate; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

	    /// Compute the height of the binary tree in O(N) time. Should not be called often.
      function GetHeight: Int32; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Get the maximum balance of an node in the tree. The balance is the difference
	    /// in height of the two children of a node.
      function GetMaxBalance: Int32; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

	    /// Get the ratio of the sum of the node areas to the root area.
	    function GetAreaRatio: PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Build an optimal tree. Very expensive. For testing.
	    procedure RebuildBottomUp;

      /// Shift the world origin. Useful for large worlds.
		 	/// The shift formula is: position -= newOrigin
		 	/// @param newOrigin the new origin with respect to the old origin
		 	procedure ShiftOrigin(const newOrigin: TVector2);
   end;
{$ENDIF}

{$IFDEF B2_USE_BRUTE_FORCE}

   Tb2Proxy = record
     aabb: Tb2AABB;
     userData: Pointer;
     id: Int32;
   end;

   /// This implementation is not a tree at all. It is just a cache friendly array of AABBs.
   Tb2DynamicTree = class
   private
      // Map of ids to proxies indices. This may have holes (which contain a free list).
      m_proxyMap: array of Int32;

      // Contiguous array of proxies
      m_proxies: array of Tb2Proxy;
      m_proxyCount: Int32;
      m_proxyCapacity: Int32;

      m_freeId: Int32;

      procedure Initialize;
   public
      constructor Create;

      /// Create a proxy. Provide a tight fitting AABB and a userData pointer.
      function CreateProxy(const _aabb: Tb2AABB; _userData: Pointer): Int32;

      /// Destroy a proxy. This asserts if the id is invalid.
      procedure DestroyProxy(proxyId: Int32); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Move a proxy with a swepted AABB. If the proxy has moved outside of its fattened AABB,
      /// then the proxy is removed from the tree and re-inserted. Otherwise
      /// the function returns immediately.
      /// @return true if the proxy was re-inserted.
      function MoveProxy(proxyId: Int32; const aabb: Tb2AABB;
         const displacement: TVector2): Boolean;

      /// Perform some iterations to re-balance the tree.
      procedure Rebalance(iterations: Int32);

      /// Get proxy user data.
      /// @return the proxy user data or 0 if the id is invalid.
      function GetUserData(proxyId: Int32): Pointer; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Get the fat AABB for a proxy.
      function GetFatAABB(proxyId: Int32): Pb2AABB; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

    	/// Compute the height of the binary tree in O(N) time. Should not be called often.
      function ComputeHeight: Int32; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Query an AABB for overlapping proxies. The callback class
      /// is called for each proxy that overlaps the supplied AABB.
      procedure Query(callback: Tb2GenericCallBackWrapper; const _aabb: Tb2AABB);

      /// Ray-cast against the proxies in the tree. This relies on the callback
      /// to perform a exact ray-cast in the case were the proxy contains a shape.
      /// The callback also performs the any collision filtering. This has performance
      /// roughly equal to k * log(n), where k is the number of collisions and n is the
      /// number of proxies in the tree.
      /// @param input the ray-cast input data. The ray extends from p1 to p1 + maxFraction * (p2 - p1).
      /// @param callback a callback class that is called for each proxy that is hit by the ray.
      procedure RayCast(callback: Tb2GenericCallBackWrapper; const input: Tb2RayCastInput);

      procedure Validate; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
   end;
{$ENDIF}

   /// The broad-phase is used for computing pairs and performing volume queries and ray casts.
   /// This broad-phase does not persist pairs. Instead, this reports potentially new pairs.
   /// It is up to the client to consume the new pairs and to track subsequent overlap.
   Tb2BroadPhase = class(Tb2GenericCallBackWrapper)
   private
      m_tree: Tb2DynamicTree;

      m_proxyCount: Int32;
      m_moveBuffer: array of Int32;
      m_moveCapacity,
      m_moveCount: Int32;

      m_pairBuffer: array of Tb2Pair;
      m_pairCapacity,
      m_pairCount: Int32;

      m_queryProxyId: Int32;

      procedure QuickSortPairBuffer(L, R: Int32);
      procedure BufferMove(proxyId: Int32);
      procedure UnBufferMove(proxyId: Int32);

      function QueryCallback(proxyId: Int32): Boolean; override;
   public
      constructor Create;
      destructor Destroy; override;

      /// Create a proxy with an initial AABB. Pairs are not reported until
      /// UpdatePairs is called.
      function CreateProxy(const aabb: Tb2AABB; userData: Pointer): Int32; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Destroy a proxy. It is up to the client to remove any pairs.
      procedure DestroyProxy(proxyId: Int32); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Call MoveProxy as many times as you like, then when you are done
      /// call UpdatePairs to finalized the proxy pairs (for your time step).
      procedure MoveProxy(proxyId: Int32; const aabb: Tb2AABB; const displacement: TVector2); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

	    /// Call to trigger a re-processing of it's pairs on the next call to UpdatePairs.
      procedure TouchProxy(proxyId: Int32); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Get the fat AABB for a proxy.
      function GetFatAABB(proxyId: Int32): Pb2AABB; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Get user data from a proxy. Returns NULL if the id is invalid.
      function GetUserData(proxyId: Int32): Pointer; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Test overlap of fat AABBs.
      function TestOverlap(proxyIdA, proxyIdB: Int32): Boolean; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Update the pairs. This results in pair callbacks. This can only add pairs.
      procedure UpdatePairs(callback: Tb2ContactManager);

      /// Query an AABB for overlapping proxies. The callback class
      /// is called for each proxy that overlaps the supplied AABB.
      procedure Query(callback: Tb2GenericCallBackWrapper; const aabb: Tb2AABB); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Ray-cast against the proxies in the tree. This relies on the callback
      /// to perform a exact ray-cast in the case were the proxy contains a shape.
      /// The callback also performs the any collision filtering. This has performance
      /// roughly equal to k * log(n), where k is the number of collisions and n is the
      /// number of proxies in the tree.
      /// @param input the ray-cast input data. The ray extends from p1 to p1 + maxFraction * (p2 - p1).
      /// @param callback a callback class that is called for each proxy that is hit by the ray.
      procedure RayCast(callback: Tb2GenericCallBackWrapper; const input: Tb2RayCastInput); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      function GetTreeHeight: Int32; {$IFDEF INLINE_AVAIL}inline;{$ENDIF} /// Get the height of the embedded tree.
      function GetTreeBalance: Int32; {$IFDEF INLINE_AVAIL}inline;{$ENDIF} /// Get the balance of the embedded tree.
      function GetTreeQuality: PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF} /// Get the quality metric of the embedded tree.

      /// Shift the world origin. Useful for large worlds.
  	  /// The shift formula is: position -= newOrigin
  		/// @param newOrigin the new origin with respect to the old origin
  		procedure ShiftOrigin(const newOrigin: TVector2); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      property GetProxyCount: Int32 read m_proxyCount; /// Get the number of proxies.
   end;

   //////////////////////////////////////////////////////////////
   /// Fixture & Shapes

   /// This holds the mass data computed for a shape.
   Tb2MassData = record
	    mass: PhysicsFloat; /// The mass of the shape, usually in kilograms.
      I: PhysicsFloat; /// The rotational inertia of the shape about the local origin.
      center: TVector2; /// The position of the shape's centroid relative to the shape's origin.
   end;

   /// The various collision shape types supported by Box2D.
   Tb2ShapeType = (e_circleShape, e_edgeShape, e_polygonShape, e_chainShape);

   /// A shape is used for collision detection. You can create a shape however you like.
   /// Shapes used for simulation in b2World are created automatically when a b2Fixture
   /// is created. Shapes may encapsulate a one or more child shapes.
   Tb2Shape = class
   private
      m_type: Tb2ShapeType;
      m_fixture: Tb2Fixture;
      m_destroyed: Boolean;
      m_baseMass: Tb2MassData; // Density = 1
   public
      m_radius: PhysicsFloat;

      constructor Create;
      destructor Destroy; override;

      /// Clone the concrete shape using the provided allocator.
      function Clone: Tb2Shape; virtual; abstract;

     	/// Get the number of child primitives.
	    function GetChildCount: Int32; virtual; abstract;

      /// Test a point for containment in this shape. This only works for convex shapes.
      /// @param xf the shape world transform.
      /// @param p a point in world coordinates.
      function TestPoint(const xf: Tb2Transform; const p: TVector2): Boolean; virtual; abstract;

      /// Cast a ray against a child shape.
      /// @param output the ray-cast results.
      /// @param input the ray-cast input parameters.
      /// @param transform the transform to be applied to the shape.
      /// @param childIndex the child shape index
      function RayCast(var output: Tb2RayCastOutput; const input: Tb2RayCastInput;
         const transform: Tb2Transform; childIndex: Int32): Boolean; virtual; abstract;

      /// Given a transform, compute the associated axis aligned bounding box for a child shape.
      /// @param aabb returns the axis aligned box.
      /// @param xf the world transform of the shape.
      /// @param childIndex the child shape
      procedure ComputeAABB(var aabb: Tb2AABB; const xf: Tb2Transform; childIndex: Int32); virtual; abstract;

      /// Compute the mass properties of this shape using its dimensions and density.
      /// The inertia tensor is computed about the local origin.
      /// @param massData returns the mass data for this shape.
      /// @param density the density in kilograms per meter squared.
      procedure ComputeMass(var massData: Tb2MassData; density: PhysicsFloat); virtual; abstract;

      { * Compute the volume and centroid of this shape intersected with a half plane
        * @param normal the surface normal
        * @param offset the surface offset along normal
        * @param xf the shape transform
        * @param c returns the centroid
        * @return the total volume less than offset along normal }
      function ComputeSubmergedArea(const normal: TVector2; offset: PhysicsFloat;
         const xf: Tb2Transform; var c: TVector2): PhysicsFloat; virtual; abstract;
      property GetType: Tb2ShapeType read m_type;
   end;

   /// This holds contact filtering data.
   Pb2Filter = ^Tb2Filter;
   Tb2Filter = record
      /// The collision category bits. Normally you would just set one bit.
      categoryBits: UInt16;

      /// The collision mask bits. This states the categories that this
      /// shape would accept for collision.
      maskBits: UInt16;

      /// Collision groups allow a certain group of objects to never collide (negative)
      /// or always collide (positive). Zero means no collision group. Non-zero group
      /// filtering always wins against the mask bits.
      groupIndex: Int16;
   end;

   /// This proxy is used internally to connect fixtures to the broad-phase.
   Pb2FixtureProxy = ^Tb2FixtureProxy;
   Tb2FixtureProxy = record
      aabb: Tb2AABB;
      fixture: Tb2Fixture;
      childIndex: Int32;
      proxyId: Int32;
   end;

   /// A fixture definition is used to create a fixture. This class defines an
   /// abstract fixture definition. You can reuse fixture definitions safely.
   Tb2FixtureDef = class
   public
      /// The shape, this must be set. The shape will be cloned, so you
      /// can create the shape on the stack.
      shape: Tb2Shape;
      userData: Pointer; /// Use this to store application specific fixture data.
      friction: PhysicsFloat; /// The friction coefficient, usually in the range [0,1].
      restitution: PhysicsFloat; /// The restitution (elasticity) usually in the range [0,1].
      density: PhysicsFloat; /// The density, usually in kg/m^2.
      isSensor: Boolean; /// A sensor shape collects contact information but never generates a collision response.
      filter: Tb2Filter; /// Contact filtering data.

      constructor Create;
   end;

   Tb2Fixture = class
   private
      m_shape: Tb2Shape;
      destructor Destroy2;
   protected
      m_density: PhysicsFloat;

      m_next: Tb2Fixture;
      m_body: Tb2Body;

      m_friction,
      m_restitution: PhysicsFloat;

      m_proxies: array of Tb2FixtureProxy;
      m_proxyCount: Int32;

      m_filter: Tb2Filter;

      m_isSensor: Boolean;
      m_userData: Pointer;

      // These support body activation/deactivation.
      procedure CreateProxies(broadPhase: Tb2BroadPhase; const xf: Tb2Transform);
      procedure DestroyProxies(broadPhase: Tb2BroadPhase);
      procedure Synchronize(broadPhase: Tb2BroadPhase; const xf1, xf2: Tb2Transform);

      procedure FSetIsSensor(value: Boolean);
   public
      constructor Create(body: Tb2Body; def: Tb2FixtureDef; AutoFreeShape: Boolean = True);
      destructor Destroy; override;

      {$IFDEF ENABLE_DUMP}
      procedure Dump(bodyIndex: Int32);
      {$ENDIF}

      /// Get the type of the child shape. You can use this to down cast to the concrete shape.
	    function GetType: Tb2ShapeType; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Set the contact filtering data. This will not update contacts until the next time
      /// step when either parent body is active and awake.
      procedure SetFilterData(const filter: Tb2Filter);
      procedure Refilter;
      function GetFilterData: Pb2Filter; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Test a point for containment in this fixture.
      /// @param xf the shape world transform.
      /// @param p a point in world coordinates.
      function TestPoint(const p: TVector2): Boolean;

      /// Cast a ray against this shape.
      /// @param output the ray-cast results.
      /// @param input the ray-cast input parameters.
      function RayCast(var output: Tb2RayCastOutput; const input: Tb2RayCastInput; childIndex: Int32): Boolean;

      /// Get the mass data for this fixture. The mass data is based on the density and
      /// the shape. The rotational inertia is about the shape's origin. This operation
      /// may be expensive.
      procedure GetMassData(var massData: Tb2MassData);

      /// Get the fixture's AABB. This AABB may be enlarge and/or stale.
      /// If you need a more accurate AABB, compute it using the shape and
      /// the body transform.
      function GetAABB(childIndex: Int32): Pb2AABB; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      property GetShape: Tb2Shape read m_shape;
      property GetBody: Tb2Body read m_body;
      property GetNext: Tb2Fixture read m_next;
      property UserData: Pointer read m_userData write m_userData;
      property Density: PhysicsFloat read m_density write m_density;
      property IsSensor: Boolean read m_isSensor write FSetIsSensor;

      /// Set the coefficient of friction. This will _not_ change the friction of existing contacts.
      property Friction: PhysicsFloat read m_friction write m_friction;
      /// Set the coefficient of restitution. This will _not_ change the restitution of existing contacts.
      property Restitution: PhysicsFloat read m_restitution write m_restitution;
   end;

   //////////////////////////////////////////////////////////////
   // Joints
   Tb2JointType = (e_unknownJoint, e_revoluteJoint, e_prismaticJoint,
      e_distanceJoint, e_pulleyJoint, e_mouseJoint, e_gearJoint, e_wheelJoint,
      e_weldJoint, e_frictionJoint, e_ropeJoint, e_motorJoint);
   Tb2LimitState = (e_inactiveLimit, e_atLowerLimit, e_atUpperLimit, e_equalLimits);

   Tb2Jacobian = record
      linear: TVector2;
      angularA, angularB: PhysicsFloat;
   end;

   /// A joint edge is used to connect bodies and joints together
   /// in a joint graph where each body is a node and each joint
   /// is an edge. A joint edge belongs to a doubly linked list
   /// maintained in each attached body. Each joint has two joint
   /// nodes, one for each attached body.
   Pb2JointEdge = ^Tb2JointEdge;
   Tb2JointEdge = record
      other: Tb2Body; ///< provides quick access to the other body attached.
      joint: Tb2Joint; ///< the joint
      prev, next: Pb2JointEdge;
   end;

   /// Joint definitions are used to construct joints.
   Tb2JointDef = class
   public
      JointType: Tb2JointType; /// The joint type is set automatically for concrete joint types.
      userData: Pointer; /// Use this to attach application specific data to your joints.

      bodyA, bodyB: Tb2Body ; /// The attached bodies.
      collideConnected: Boolean; /// Set this flag to True if the attached bodies should collide.

      constructor Create;
   end;

   /// The base joint class. Joints are used to constraint two bodies together in
   /// various fashions. Some joints also feature limits and motors.
   Tb2Joint = class
   protected
      m_type: Tb2JointType;
      m_prev, m_next: Tb2Joint;
      m_edgeA, m_edgeB: Tb2JointEdge;
      m_bodyA, m_bodyB: Tb2Body;
      m_index: Int32;

      m_islandFlag, m_collideConnected: Boolean;
      m_userData: Pointer;

      procedure InitVelocityConstraints(const data: Tb2SolverData); virtual; abstract;
      procedure SolveVelocityConstraints(const data: Tb2SolverData); virtual; abstract;

      // This returns True if the position errors are within tolerance.
      function SolvePositionConstraints(const data: Tb2SolverData): Boolean; virtual; abstract;

   public
      constructor Create(def: Tb2JointDef);

      {$IFDEF ENABLE_DUMP}
      procedure Dump; virtual;
      {$ENDIF}

      /// Shift the origin for any points stored in world coordinates.
	    procedure ShiftOrigin(const newOrigin: TVector2); virtual;

      /// Get the anchor point on bodyA in world coordinates.
      function GetAnchorA: TVector2; virtual; abstract;

      /// Get the anchor point on bodyB in world coordinates.
      function GetAnchorB: TVector2; virtual; abstract;

      /// Get the reaction force on bodyB at the joint anchor in Newtons.
      function GetReactionForce(inv_dt: PhysicsFloat): TVector2; virtual; abstract;

      /// Get the reaction torque on bodyB in N*m.
      function GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat; virtual; abstract;

	    /// Short-cut function to determine if either body is inactive.
    	function IsActive: Boolean; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      property GetType: Tb2JointType read m_type;
      property GetBodyA: Tb2Body read m_bodyA;
      property GetBodyB: Tb2Body read m_bodyB;
      property GetNext: Tb2Joint read m_next;
      property UserData: Pointer read m_userData write m_userData;

      /// Get collide connected.
	    /// Note: modifying the collide connect flag won't work correctly because
    	/// the flag is only checked when fixture AABBs begin to overlap.
      property IsCollideConnected: Boolean read m_collideConnected;
   end;

   {$IFDEF CONTROLLERS}
   //////////////////////////////////////////////////////////////
   // Controllers

   /// A controller edge is used to connect bodies and controllers together
   /// in a bipartite graph.
   Pb2ControllerEdge = ^Tb2ControllerEdge;
   Tb2ControllerEdge = record
      controller: Tb2Controller;		///< provides quick access to other end of this edge.
      body: Tb2Body;					///< the body
      prevBody: Pb2ControllerEdge;		///< the previous controller edge in the controllers's joint list
      nextBody: Pb2ControllerEdge;		///< the next controller edge in the controllers's joint list
      prevController: Pb2ControllerEdge;		///< the previous controller edge in the body's joint list
      nextController: Pb2ControllerEdge;		///< the next controller edge in the body's joint list
   end;

   /// Base class for controllers. Controllers are a convience for encapsulating common
   /// per-step functionality.
   Tb2Controller = class
   private
      m_prev,
      m_next: Tb2Controller;
   protected
      m_world: Tb2World;

      m_bodyList: Pb2ControllerEdge;
      m_bodyCount: Int32;
   public
      destructor Destroy; override;

      /// Controllers override this to implement per-step functionality.
      procedure Step(const step: Tb2TimeStep); virtual; abstract;

      /// Controllers override this to provide debug drawing.
      procedure Draw(debugDraw: Tb2Draw); virtual;

      /// Adds a body to the controller list.
      procedure AddBody(body: Tb2Body); virtual;

      /// Removes a body from the controller list.
      procedure RemoveBody(body: Tb2Body); virtual;

      /// Removes all bodies from the controller list.
      procedure Clear;

      property GetNext: Tb2Controller read m_next; /// Get the next controller in the world's body list.
      property GetWorld: Tb2World read m_world;/// Get the parent world of this body.
      property GetBodyList: Pb2ControllerEdge read m_bodyList; /// Get the attached body list
   end;
   {$ENDIF}

   //////////////////////////////////////////////////////////////
   // Body

   /// The body type.
   /// static: zero mass, zero velocity, may be manually moved
   /// kinematic: zero mass, non-zero velocity set by user, moved by solver
   /// dynamic: positive mass, non-zero velocity determined by forces, moved by solver
   Tb2BodyType = (b2_staticBody, b2_kinematicBody, b2_dynamicBody);

   /// A body definition holds all the data needed to construct a rigid body.
   /// You can safely re-use body definitions.
   Tb2BodyDef = class
   public
      bodyType: Tb2BodyType; /// Note: if a dynamic body would have zero mass, the mass is set to one.
      userData: Pointer; /// Use this to store application specific body data.

      ignoreColliding: Boolean;
      /// The world position of the body. Avoid creating bodies at the origin
      /// since this can lead to many overlapping shapes.
      position: TVector2;

      angle: PhysicsFloat; // The world angle of the body in radians.

    	/// The linear velocity of the body's origin in world co-ordinates.
	    linearVelocity: TVector2;

    	angularVelocity: PhysicsFloat; /// The angular velocity of the body.

      /// Linear damping is use to reduce the linear velocity. The damping parameter
      /// can be larger than 1.0f but the damping effect becomes sensitive to the
      /// time step when the damping parameter is large.
      linearDamping: PhysicsFloat;

      /// Angular damping is use to reduce the angular velocity. The damping parameter
      /// can be larger than 1.0f but the damping effect becomes sensitive to the
      /// time step when the damping parameter is large.
      angularDamping: PhysicsFloat;

      /// Set this flag to false if this body should never fall asleep. Note that
      /// this increases CPU usage.
      allowSleep: Boolean;

      awake: Boolean; /// Is this body initially awake or sleeping?
      fixedRotation: Boolean; /// Should this body be prevented from rotating? Useful for characters.

      /// Is this a fast moving body that should be prevented from tunneling through
      /// other moving bodies? Note that all bodies are prevented from tunneling through
      /// kinematic and static bodies. This setting is only considered on dynamic bodies.
      /// @warning You should use this flag sparingly since it increases processing time.
      bullet: Boolean;

      /// Does this body start out active?
      active: Boolean;

      /// Scale the gravity applied to this body.
	    gravityScale: PhysicsFloat;

      constructor Create;
   end;

   Tb2Body = class
   private
      m_type: Tb2BodyType;

      m_flags: UInt16;
      m_islandIndex: Int32;

      m_world: Tb2World;
      m_prev, m_next: Tb2Body;

      m_fixtureList: Tb2Fixture;
      m_fixtureCount: Int32;

      m_jointList: Pb2JointEdge;
      m_contactList: Pb2ContactEdge;

      {$IFDEF CONTROLLERS}
	    m_controllerList: Pb2ControllerEdge;
	    m_controllerCount: Int32;
      {$ENDIF}

      m_mass, m_invMass: PhysicsFloat;
      m_I, m_invI: PhysicsFloat; // Rotational inertia about the center of mass.
      m_storedInertia: PhysicsFloat;

      m_linearDamping: PhysicsFloat;
      m_angularDamping: PhysicsFloat;
      m_gravityScale: PhysicsFloat;

      destructor Destroy2; // Only free heap
      procedure ComputeStoredInertia;

      procedure SynchronizeFixtures;
      procedure SynchronizeTransform;

	    // This is used to prevent connected bodies from colliding.
	    // It may lie, depending on the collideConnected flag.
	    function ShouldCollide(other: Tb2Body): Boolean;
      procedure Advance(alpha: PhysicsFloat);
   protected
      m_xf: Tb2Transform; // the body origin transform
      m_sweep: Tb2Sweep; // the swept motion for CCD

      m_linearVelocity: TVector2;
      m_angularVelocity: PhysicsFloat;

      m_force: TVector2;
      m_torque: PhysicsFloat;

      m_sleepTime: PhysicsFloat;
      m_userData: Pointer;
   public
      constructor Create(bd: Tb2BodyDef; world: Tb2World);
      destructor Destroy; override;

      {$IFDEF ENABLE_DUMP}
      procedure Dump;
      {$ENDIF}

      /// Creates a fixture and attach it to this body. Use this function if you need
      /// to set some fixture parameters, like friction. Otherwise you can create the
      /// fixture directly from a shape.
      /// If the density is non-zero, this function automatically updates the mass of the body.
      /// Contacts are not created until the next time step.
      /// @param def the fixture definition.
      /// @warning This function is locked during callbacks.
      function CreateFixture(def: Tb2FixtureDef; AutoFreeFixtureDef: Boolean = True;
         AutoFreeShape: Boolean = True; AutoResetMassData: Boolean = True): Tb2Fixture; overload;

      /// Creates a fixture from a shape and attach it to this body.
      /// This is a convenience function. Use b2FixtureDef if you need to set parameters
      /// like friction, restitution, user data, or filtering.
      /// If the density is non-zero, this function automatically updates the mass of the body.
      /// @param shape the shape to be cloned.
      /// @param density the shape density (set to zero for static bodies).
      /// @warning This function is locked during callbacks.
      function CreateFixture(shape: Tb2Shape; density: PhysicsFloat;
         AutoFreeShape: Boolean = True; AutoResetMassData: Boolean = True): Tb2Fixture; overload;

      /// Destroy a fixture. This removes the fixture from the broad-phase and
      /// destroys all contacts associated with this fixture. This will
      /// automatically adjust the mass of the body if the body is dynamic and the
      /// fixture has positive density.
      /// All fixtures attached to a body are implicitly destroyed when the body is destroyed.
      /// @param fixture the fixture to be removed.
      /// @warning This function is locked during callbacks.
      procedure DestroyFixture(fixture: Tb2Fixture; DoFree: Boolean = True);

      /// Destroy all fixtures. If ResetMass is False, Self becomes a virtual body
      /// which doesn't react to collisions but keep all physical features.
      procedure DestroyFixtures(ResetMass: Boolean);

      /// Set the position of the body's origin and rotation.
      /// Manipulating a body's transform may cause non-physical behavior.
      /// Note: contacts are updated on the next call to b2World::Step.
      /// @param position the world position of the body's local origin.
      /// @param angle the world rotation in radians.
      procedure SetTransform(const position: TVector2; angle: PhysicsFloat);
    	/// Get the body transform for the body's origin.
    	/// @return the world transform of the body's origin.
    	function GetTransform: Pb2Transform; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      function GetAngle: PhysicsFloat;

      /// Set the linear velocity of the center of mass.
      /// @param v the new linear velocity of the center of mass.
      procedure SetLinearVelocity(const v: TVector2); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Set the angular velocity.
      /// @param omega the new angular velocity in radians/second.
      procedure SetAngularVelocity(omega: PhysicsFloat); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Apply a force at a world point. If the force is not
      /// applied at the center of mass, it will generate a torque and
      /// affect the angular velocity. This wakes up the body.
      /// @param force the world force vector, usually in Newtons (N).
      /// @param point the world position of the point of application.
      ///  /// @param wake also wake up the body
      procedure ApplyForce(const force, point: TVector2; wake: Boolean = True);  // added default value for "wake"

      /// Apply a force to the center of mass. This wakes up the body.
      /// @param force the world force vector, usually in Newtons (N).
      /// @param wake also wake up the body
      procedure ApplyForceToCenter(const force: TVector2;  wake: Boolean = True);   // added default value for "wake"

      /// Apply a torque. This affects the angular velocity
      /// without affecting the linear velocity of the center of mass.
      /// This wakes up the body.
      /// @param torque about the z-axis (out of the screen), usually in N-m.
      /// @param wake also wake up the body
      procedure ApplyTorque(torque: PhysicsFloat;  wake: Boolean = True);   // added default value for "wake"

      /// Apply an impulse at a point. This immediately modifies the velocity.
      /// It also modifies the angular velocity if the point of application
      /// is not at the center of mass. This wakes up the body.
      /// @param impulse the world impulse vector, usually in N-seconds or kg-m/s.
      /// @param point the world position of the point of application.
      /// @param wake also wake up the body
      procedure ApplyLinearImpulse(const impulse, point: TVector2;  wake: Boolean = True);   // added default value for "wake"
      /// Apply an angular impulse.
      /// @param impulse the angular impulse in units of kg*m*m/s
      /// @param wake also wake up the body
      procedure ApplyAngularImpulse(impulse: PhysicsFloat;  wake: Boolean = True);   // added default value for "wake"

    	/// Get the mass data of the body.
	    /// @return a struct containing the mass, inertia and center of the body.
      procedure GetMassData(var data: Tb2MassData);

      /// Set the mass properties to override the mass properties of the fixtures.
      /// Note that this changes the center of mass position.
      /// Note that creating or destroying fixtures can also alter the mass.
      /// This function has no effect if the body isn't dynamic.
      /// @param massData the mass properties.
      procedure SetMassData(const data: Tb2MassData);

      /// This resets the mass properties to the sum of the mass properties of the fixtures.
      /// This normally does not need to be called unless you called SetMassData to override
      /// the mass and you later want to reset the mass.
      procedure ResetMassData;

      /// Get the world coordinates of a point given the local coordinates.
      /// @param localPoint a point on the body measured relative the the body's origin.
      /// @return the same point expressed in world coordinates.
      function GetWorldPoint(const localPoint: TVector2): TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Get the world coordinates of a vector given the local coordinates.
      /// @param localVector a vector fixed in the body.
      /// @return the same vector expressed in world coordinates.
      function GetWorldVector(const localVector: TVector2): TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Gets a local point relative to the body's origin given a world point.
      /// @param a point in world coordinates.
      /// @return the corresponding local point relative to the body's origin.
      function GetLocalPoint(const worldPoint: TVector2): TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Gets a local vector given a world vector.
      /// @param a vector in world coordinates.
      /// @return the corresponding local vector.
      function GetLocalVector(const worldVector: TVector2): TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Get the world linear velocity of a world point attached to this body.
      /// @param a point in world coordinates.
      /// @return the world velocity of a point.
      function GetLinearVelocityFromWorldPoint(const worldPoint: TVector2): TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Get the world velocity of a local point.
      /// @param a point in local coordinates.
      /// @return the world velocity of a point.
      function GetLinearVelocityFromLocalPoint(const localPoint: TVector2): TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Set the type of this body. This may alter the mass and velocity.
    	procedure SetType(atype: Tb2BodyType);

      /// Is this body treated like a bullet for continuous collision detection?
      function IsBullet: Boolean; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Should this body be treated like a bullet for continuous collision detection?
      procedure SetBullet(flag: Boolean);

      /// You can disable sleeping on this body. If you disable sleeping, the
      /// body will be woken.
      procedure SetSleepingAllowed(flag: Boolean);

      /// Is this body allowed to sleep
      function IsSleepingAllowed: Boolean; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Set the sleep state of the body. A sleeping body has very
      /// low CPU cost.
      /// @param flag set to True to wake the body to sleep, false to put it to sleep.
      procedure SetAwake(flag: Boolean);

      /// Get the sleeping state of this body.
      /// @return true if the body is awake.
      function IsAwake: Boolean; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Set the active state of the body. An inactive body is not
      /// simulated and cannot be collided with or woken up.
      /// If you pass a flag of True, all fixtures will be added to the
      /// broad-phase.
      /// If you pass a flag of false, all fixtures will be removed from
      /// the broad-phase and all contacts will be destroyed.
      /// Fixtures and joints are otherwise unaffected. You may continue
      /// to create/destroy fixtures and joints on inactive bodies.
      /// Fixtures on an inactive body are implicitly inactive and will
      /// not participate in collisions, ray-casts, or queries.
      /// Joints connected to an inactive body are implicitly inactive.
      /// An inactive body is still owned by a b2World object and remains
      /// in the body list.
      procedure SetActive(flag: Boolean);

      /// Get the active state of the body.
      function IsActive: Boolean; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      procedure SetIgnoreColliding(flag: Boolean); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      function IsCollidingIgnored: Boolean; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Set this body to have fixed rotation. This causes the mass
      /// to be reset.
      procedure SetFixedRotation(flag: Boolean);

      /// Does this body have fixed rotation?
      function IsFixedRotation: Boolean; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      ////////////////////////////////////
      property GetType: Tb2BodyType read m_type;
      property GetPosition: TVector2 read m_xf.p;
      property GetWorldCenter: TVector2 read m_sweep.c;
      property GetLocalCenter: TVector2 read m_sweep.localCenter;
      property GetLinearVelocity: TVector2 read m_linearVelocity;
      property GetAngularVelocity: PhysicsFloat read m_angularVelocity;
      property LinearDamping: PhysicsFloat read m_linearDamping write m_linearDamping;
      property AngularDamping: PhysicsFloat read m_angularDamping write m_angularDamping;
      property GravityScale: PhysicsFloat read m_gravityScale write m_gravityScale;

      property GetMass: PhysicsFloat read m_mass;
      /// Get the rotational inertia of the body about the local origin.
      /// @return the rotational inertia, usually in kg-m^2.
      property GetInertia: PhysicsFloat read m_storedInertia;

      property GetFixtureList: Tb2Fixture read m_fixtureList;
      property GetJointList: Pb2JointEdge read m_jointList;
      property GetContactList: Pb2ContactEdge read m_contactList;
      property GetNext: Tb2Body read m_next;
      property UserData: Pointer read m_userData write m_userData;
      property GetWorld: Tb2World read m_world;
      {$IFDEF CONTROLLERS}
      property GetControllerCount: Int32 read m_controllerCount;
      property GetControllerList: Pb2ControllerEdge read m_controllerList;
      {$ENDIF}
   end;

   ///////////////////////////////////////////////
   // Specific implementations

   Tb2CircleShape = class(Tb2Shape)
   public
      m_p: TVector2;

      constructor Create;

      function Clone: Tb2Shape; override;
      function GetChildCount: Int32; override;
      function TestPoint(const xf: Tb2Transform; const p: TVector2): Boolean; override;
      function RayCast(var output: Tb2RayCastOutput; const input: Tb2RayCastInput;
         const transform: Tb2Transform; childIndex: Int32): Boolean; override;
      procedure ComputeAABB(var aabb: Tb2AABB; const xf: Tb2Transform; childIndex: Int32); override;
      procedure ComputeMass(var massData: Tb2MassData; density: PhysicsFloat); override;
      function ComputeSubmergedArea(const normal: TVector2; offset: PhysicsFloat;
         const xf: Tb2Transform; var c: TVector2): PhysicsFloat; override;

      /// Get the supporting vertex index in the given direction.
      function GetSupport(const d: TVector2): Int32; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Get the supporting vertex in the given direction.
      function GetSupportVertex(const d: TVector2): TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Get the vertex count.
      function GetVertexCount: Int32; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Get a vertex by index. Used by b2Distance.
      function GetVertex(index: Int32): TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
   end;

   /// A convex polygon. It is assumed that the interior of the polygon is to
   /// the left of each edge.
   /// Polygons have a maximum number of vertices equal to b2_maxPolygonVertices.
   /// In most cases you should not need many vertices for a convex polygon.
   Tb2PolygonShape = class(Tb2Shape)
   public
      m_centroid: TVector2; // Local position of the polygon centroid.
      m_vertices: Tb2PolyVertices;
      m_normals: Tb2PolyVertices;
      m_count: Int32;

      constructor Create;

      function Clone: Tb2Shape; override;
      function GetChildCount: Int32; override;
      function TestPoint(const xf: Tb2Transform; const p: TVector2): Boolean; override;
      function RayCast(var output: Tb2RayCastOutput; const input: Tb2RayCastInput;
         const transform: Tb2Transform; childIndex: Int32): Boolean; override;
      procedure ComputeAABB(var aabb: Tb2AABB; const xf: Tb2Transform; childIndex: Int32); override;
      procedure ComputeMass(var massData: Tb2MassData; density: PhysicsFloat); override;
      function ComputeSubmergedArea(const normal: TVector2; offset: PhysicsFloat;
         const xf: Tb2Transform; var c: TVector2): PhysicsFloat; override;

      /// Create a convex hull from the given array of local points.
      /// The count must be in the range [3, b2_maxPolygonVertices].
      /// @warning the points may be re-ordered, even if they form a convex polygon
      /// @warning collinear points are handled but not removed. Collinear points
      /// may lead to poor stacking behavior.
      procedure SetVertices(vertices: PVector2; count: Int32);

      /// Build vertices to represent an axis-aligned box centered on the local origin.
      /// @param hx the half-width.
      /// @param hy the half-height.
      procedure SetAsBox(hx, hy: PhysicsFloat); overload;

      /// Build vertices to represent an oriented box.
      /// @param hx the half-width.
      /// @param hy the half-height.
      /// @param center the center of the box in local coordinates.
      /// @param angle the rotation of the box in local coordinates.
      procedure SetAsBox(hx, hy: PhysicsFloat; const center: TVector2; angle: PhysicsFloat); overload;

      /// Validate convexity. This is a very time consuming operation.
      function Validate: Boolean;

      property GetVertexCount: Int32 read m_count;
   end;

   /// A line segment (edge) shape. These can be connected in chains or loops
   /// to other edge shapes. The connectivity information is used to ensure
   /// correct contact normals.
   Tb2EdgeShape = class(Tb2Shape)
   public
      m_vertex1, m_vertex2: TVector2; /// These are the edge vertices
      m_normal1, m_normal2: TVector2;
	    /// Optional adjacent vertices. These are used for smooth collision.
    	m_vertex0, m_vertex3: TVector2;
	    m_hasVertex0, m_hasVertex3: Boolean;
      m_edgeShapeMassed: Boolean; // If true density is linear density and mass will not be zero.

      constructor Create;

      procedure SetVertices(const v1, v2: TVector2); // implements C++ "Set" function
      function Clone: Tb2Shape; override;
      function GetChildCount: Int32; override;
      function TestPoint(const xf: Tb2Transform; const p: TVector2): Boolean; override;
      function RayCast(var output: Tb2RayCastOutput; const input: Tb2RayCastInput;
         const transform: Tb2Transform; childIndex: Int32): Boolean; override;
      procedure ComputeAABB(var aabb: Tb2AABB; const xf: Tb2Transform; childIndex: Int32); override;
      procedure ComputeMass(var massData: Tb2MassData; density: PhysicsFloat); override;
      function ComputeSubmergedArea(const normal: TVector2; offset: PhysicsFloat;
         const xf: Tb2Transform; var c: TVector2): PhysicsFloat; override;
   end;

   /// A chain shape is a free form sequence of line segments.
   /// The chain has two-sided collision, so you can use inside and outside collision.
   /// Therefore, you may use any winding order.
   /// Since there may be many vertices, they are allocated using b2Alloc.
   /// Connectivity information is used to create smooth collisions.
   /// WARNING: The chain will not collide properly if there are self-intersections.
   Tb2ChainShape = class(Tb2Shape)
   private
      class function IsValidVertices(pv: PVector2; count: Int32; loop: Boolean): Boolean;
   public
      m_loop: Boolean;
      m_vertices: TVectorArray;
      m_count: Int32;

      m_prevVertex, m_nextVertex: TVector2;
      m_hasPrevVertex, m_hasNextVertex: Boolean;

      /// Create a loop. This automatically adjusts connectivity.
      /// @param vertices an array of vertices, these are copied
      /// @param count the vertex count
      constructor CreateLoop(pv: PVector2; count: Int32);

      /// Create a chain with isolated end vertices.
      /// @param vertices an array of vertices, these are copied
      /// @param count the vertex count
      constructor CreateChain(pv: PVector2; count: Int32);

      /// Establish connectivity to a vertex that precedes the first vertex.
      /// Don't call this for loops.
      procedure SetPrevVertex(const prevVertex: TVector2);

      /// Establish connectivity to a vertex that follows the last vertex.
      /// Don't call this for loops.
      procedure SetNextVertex(const nextVertex: TVector2);

      function Clone: Tb2Shape; override;
      function GetChildCount: Int32; override;
      procedure GetChildEdge(edge: Tb2EdgeShape; index: Int32);
      function TestPoint(const xf: Tb2Transform; const p: TVector2): Boolean; override;
      function RayCast(var output: Tb2RayCastOutput; const input: Tb2RayCastInput;
         const transform: Tb2Transform; childIndex: Int32): Boolean; override;
      procedure ComputeAABB(var aabb: Tb2AABB; const xf: Tb2Transform; childIndex: Int32); override;
      procedure ComputeMass(var massData: Tb2MassData; density: PhysicsFloat); override;
      function ComputeSubmergedArea(const normal: TVector2; offset: PhysicsFloat;
         const xf: Tb2Transform; var c: TVector2): PhysicsFloat; override;
   end;

   ////////////////////////////////////////////////////////////

   /// Distance joint definition. This requires defining an
   /// anchor point on both bodies and the non-zero length of the
   /// distance joint. The definition uses local anchor points
   /// so that the initial configuration can violate the constraint
   /// slightly. This helps when saving and loading a game.
   /// @warning Do not use a zero or short length.
   Tb2DistanceJointDef = class(Tb2JointDef)
   public
      localAnchorA: TVector2; /// The local anchor point relative to bodyA's origin.
      localAnchorB: TVector2; /// The local anchor point relative to bodyB's origin.

      length : PhysicsFloat; /// The natural length between the anchor points.
      frequencyHz: PhysicsFloat; /// The mass-spring-damper frequency in Hertz. A value of 0 disables softness.
      dampingRatio: PhysicsFloat; /// The damping ratio. 0 = no damping, 1 = critical damping.

      constructor Create;
      procedure Initialize(bodyA, bodyB: Tb2Body; const anchorA, anchorB: TVector2);
   end;

   /// A distance joint constrains two points on two bodies
   /// to remain at a fixed distance from each other. You can view
   /// this as a massless, rigid rod.
   Tb2DistanceJoint = class(Tb2Joint)
   protected
      m_frequencyHz, m_dampingRatio, m_bias: PhysicsFloat;

      // Solver shared
      m_localAnchorA, m_localAnchorB: TVector2;
      m_gamma, m_impulse, m_length: PhysicsFloat;

      // Solver temp
      m_indexA, m_indexB: Int32;
      m_u, m_rA, m_rB: TVector2;
      m_localCenterA, m_localCenterB: TVector2;
      m_invMassA, m_invMassB: PhysicsFloat;
      m_invIA, m_invIB: PhysicsFloat;
      m_mass: PhysicsFloat;

      procedure InitVelocityConstraints(const data: Tb2SolverData); override;
      procedure SolveVelocityConstraints(const data: Tb2SolverData); override;
      function SolvePositionConstraints(const data: Tb2SolverData): Boolean; override;
   public
      constructor Create(def: Tb2DistanceJointDef);

      {$IFDEF ENABLE_DUMP}
      procedure Dump; override;
      {$ENDIF}

      function GetAnchorA: TVector2; override;
      function GetAnchorB: TVector2; override;

      /// Get the reaction force given the inverse time step. Unit is N.
      function GetReactionForce(inv_dt: PhysicsFloat): TVector2; override;
    	/// Get the reaction torque given the inverse time step.
    	/// Unit is N*m. This is always zero for a distance joint.
      function GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat; override;

      /// Manipulating the length can lead to non-physical behavior when the frequency is zero.
      property Length: PhysicsFloat read m_length write m_length;
      property Frequency: PhysicsFloat read m_frequencyHz write m_frequencyHz;
      property DampingRatio: PhysicsFloat read m_dampingRatio write m_dampingRatio;
      property GetLocalAnchorA: TVector2 read m_localAnchorA; /// The local anchor point relative to bodyA's origin.
      property GetLocalAnchorB: TVector2 read m_localAnchorB; /// The local anchor point relative to bodyB's origin.
   end;

   /// Prismatic joint definition. This requires defining a line of
   /// motion using an axis and an anchor point. The definition uses local
   /// anchor points and a local axis so that the initial configuration
   /// can violate the constraint slightly. The joint translation is zero
   /// when the local anchor points coincide in world space. Using local
   /// anchors and a local axis helps when saving and loading a game.
   Tb2PrismaticJointDef = class(Tb2JointDef)
   public
      localAnchorA: TVector2; /// The local anchor point relative to bodyA's origin.
      localAnchorB: TVector2; /// The local anchor point relative to bodyB's origin.

      localAxisA: TVector2; /// The local translation unit axis in bodyA.
      referenceAngle: PhysicsFloat; /// The constrained angle between the bodies: bodyB_angle - bodyA_angle.

      enableLimit: Boolean; /// Enable/disable the joint limit.

      lowerTranslation: PhysicsFloat; /// The lower translation limit, usually in meters.
      upperTranslation: PhysicsFloat; /// The upper translation limit, usually in meters.

      enableMotor: Boolean; /// Enable/disable the joint motor.
      maxMotorForce: PhysicsFloat; /// The maximum motor torque, usually in N-m.
      motorSpeed: PhysicsFloat; /// The desired motor speed in radians per second.

      constructor Create;

      /// Initialize the bodies, anchors, axis, and reference angle using the world
      /// anchor and unit world axis.
      procedure Initialize(bodyA, bodyB: Tb2Body; const anchor, axis: TVector2); // world anchor and world axis
   end;

   /// A prismatic joint. This joint provides one degree of freedom: translation
   /// along an axis fixed in bodyA. Relative rotation is prevented. You can
   /// use a joint limit to restrict the range of motion and a joint motor to
   /// drive the motion or to model joint friction.
   Tb2PrismaticJoint = class(Tb2Joint)
   protected
      // Solver shared
      m_localAnchorA, m_localAnchorB: TVector2;
      m_localXAxisA, m_localYAxisA: TVector2;
      m_referenceAngle: PhysicsFloat;
      m_impulse: TVector3;
      m_motorImpulse: PhysicsFloat;
      m_lowerTranslation, m_upperTranslation: PhysicsFloat;
      m_maxMotorForce, m_motorSpeed: PhysicsFloat;
      m_enableLimit, m_enableMotor: Boolean;
      m_limitState: Tb2LimitState;

      // Solver temp
      m_indexA, m_indexB: Int32;
      m_localCenterA, m_localCenterB: TVector2;
      m_invMassA, m_invMassB: PhysicsFloat;
      m_invIA, m_invIB: PhysicsFloat;
      m_axis, m_perp: TVector2;
      m_s1, m_s2: PhysicsFloat;
      m_a1, m_a2: PhysicsFloat;
      m_K: TMatrix33;
      m_motorMass: PhysicsFloat;

      procedure InitVelocityConstraints(const data: Tb2SolverData); override;
      procedure SolveVelocityConstraints(const data: Tb2SolverData); override;
      function SolvePositionConstraints(const data: Tb2SolverData): Boolean; override;
   public
      constructor Create(def: Tb2PrismaticJointDef);

      {$IFDEF ENABLE_DUMP}
      procedure Dump; override;
      {$ENDIF}

      function GetAnchorA: TVector2; override;
      function GetAnchorB: TVector2; override;

      function GetReactionForce(inv_dt: PhysicsFloat): TVector2; override;
      function GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat; override;

      /// Get the current joint translation, usually in meters.
      function GetJointTranslation: PhysicsFloat;
      /// Get the current joint translation speed, usually in meters per second.
      function GetJointSpeed: PhysicsFloat;
      /// Get the current motor force given the inverse time step, usually in N.
      function GetMotorForce(inv_dt: PhysicsFloat): PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

	    procedure EnableLimit(flag: Boolean); /// Enable/disable the joint limit.
	    procedure EnableMotor(flag: Boolean); /// Enable/disable the joint motor.
      procedure SetLimits(lower, upper: PhysicsFloat); /// Set the joint limits, usually in meters.
      procedure SetMotorSpeed(speed: PhysicsFloat); /// Set the motor speed, usually in meters per second.
	    procedure SetMaxMotorForce(force: PhysicsFloat); /// Set the maximum motor force, usually in N.

      property GetMotorSpeed: PhysicsFloat read m_motorSpeed; // usually in meters per second.
      property IsLimitEnabled: Boolean read m_enableLimit;
      property GetLowerLimit: PhysicsFloat read m_lowerTranslation;
      property GetUpperLimit: PhysicsFloat read m_upperTranslation;
      property IsMotorEnabled: Boolean read m_enableMotor;
      property GetMaxMotorForce: PhysicsFloat read m_maxMotorForce;

      property GetLocalAnchorA: TVector2 read m_localAnchorA;
      property GetLocalAnchorB: TVector2 read m_localAnchorB;
      property GetLocalAxisA: TVector2 read m_localXAxisA; /// The local joint axis relative to bodyA.
      property GetReferenceAngle: PhysicsFloat read m_referenceAngle;
   end;

   /// Mouse joint definition. This requires a world target point, tuning parameters, and the time step.
   Tb2MouseJointDef = class(Tb2JointDef)
   public
      /// The initial world target point. This is assumed to coincide with the body anchor initially.
      target: TVector2;

      /// The maximum constraint force that can be exerted
      /// to move the candidate body. Usually you will express
      /// as some multiple of the weight (multiplier * mass * gravity).
      maxForce: PhysicsFloat;

      frequencyHz: PhysicsFloat; /// The response speed.
      dampingRatio: PhysicsFloat; /// The damping ratio. 0 = no damping, 1 = critical damping.

      constructor Create;
   end;

   /// A mouse joint is used to make a point on a body track a
   /// specified world point. This a soft constraint with a maximum
   /// force. This allows the constraint to stretch and without
   /// applying huge forces.
   /// NOTE: this joint is not documented in the manual because it was
   /// developed to be used in the testbed. If you want to learn how to
   /// use the mouse joint, look at the testbed.
   Tb2MouseJoint = class(Tb2Joint)
   protected
      m_localAnchorB, m_targetA: TVector2;
      m_frequencyHz, m_dampingRatio, m_beta: PhysicsFloat;

      // Solver shared
      m_impulse: TVector2;
      m_maxForce: PhysicsFloat;
      m_gamma: PhysicsFloat; // softness

      // Solver temp
      m_indexA, m_indexB: Int32;
      m_rB: TVector2;
      m_localCenterB: TVector2;
      m_invMassB: PhysicsFloat;
      m_invIB: PhysicsFloat;
      m_mass: TMatrix22; // effective mass for point-to-point constraint.
      m_C: TVector2; // position error

      procedure InitVelocityConstraints(const data: Tb2SolverData); override;
      procedure SolveVelocityConstraints(const data: Tb2SolverData); override;
      function SolvePositionConstraints(const data: Tb2SolverData): Boolean; override;

   public
      constructor Create(def: Tb2MouseJointDef);

      procedure ShiftOrigin(const newOrigin: TVector2); override;

      function GetAnchorA: TVector2; override;
      function GetAnchorB: TVector2; override;

      function GetReactionForce(inv_dt: PhysicsFloat): TVector2; override;
      function GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat; override;

	    procedure SetTarget(const target: TVector2); /// Use this to update the target point.
      function GetTarget: TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      property MaxForce: PhysicsFloat read m_maxForce write m_maxForce;
      property Frequency: PhysicsFloat read m_frequencyHz write m_frequencyHz;
      property DampingRatio: PhysicsFloat read m_dampingRatio write m_dampingRatio;
   end;

   /// Pulley joint definition. This requires two ground anchors,
   /// two dynamic body anchor points, and a pulley ratio.
   Tb2PulleyJointDef = class(Tb2JointDef)
   public
      groundAnchorA: TVector2; /// The first ground anchor in world coordinates. This point never moves.
      groundAnchorB: TVector2; /// The second ground anchor in world coordinates. This point never moves.
      localAnchorA: TVector2; /// The local anchor point relative to bodyA's origin.
      localAnchorB: TVector2; /// The local anchor point relative to bodyB's origin.

      lengthA: PhysicsFloat; /// The a reference length for the segment attached to bodyA.
      lengthB: PhysicsFloat; /// The a reference length for the segment attached to bodyB.

      ratio: PhysicsFloat; /// The pulley ratio, used to simulate a block-and-tackle.

      constructor Create;
      /// Initialize the bodies, anchors, lengths, max lengths, and ratio using the world anchors.
      procedure Initialize(bodyA, bodyB: Tb2Body; const groundAnchorA, groundAnchorB,
        anchorA, anchorB: TVector2; ratio: PhysicsFloat);
   end;

   /// The pulley joint is connected to two bodies and two fixed ground points.
   /// The pulley supports a ratio such that:
   /// length1 + ratio * length2 <= constant
   /// Yes, the force transmitted is scaled by the ratio.
   /// Warning: the pulley joint can get a bit squirrelly by itself. They often
   /// work better when combined with prismatic joints. You should also cover the
   /// the anchor points with static shapes to prevent one side from going to
   /// zero length.
   Tb2PulleyJoint = class(Tb2Joint)
   protected
      m_groundAnchorA, m_groundAnchorB: TVector2;
      m_lengthA, m_lengthB: PhysicsFloat;

      // Solver shared
      m_localAnchorA, m_localAnchorB: TVector2;
      m_constant, m_ratio, m_impulse: PhysicsFloat;

      // Solver temp
      m_indexA, m_indexB: Int32;
      m_uA, m_uB: TVector2;
      m_rA, m_rB: TVector2;
      m_localCenterA, m_localCenterB: TVector2;
      m_invMassA, m_invMassB: PhysicsFloat;
      m_invIA, m_invIB: PhysicsFloat;
      m_mass: PhysicsFloat;

      procedure InitVelocityConstraints(const data: Tb2SolverData); override;
      procedure SolveVelocityConstraints(const data: Tb2SolverData); override;
      function SolvePositionConstraints(const data: Tb2SolverData): Boolean; override;
   public
      constructor Create(def: Tb2PulleyJointDef);

      {$IFDEF ENABLE_DUMP}
      procedure Dump; override;
      {$ENDIF}

      procedure ShiftOrigin(const newOrigin: TVector2); override;

      function GetAnchorA: TVector2; override;
      function GetAnchorB: TVector2; override;

      function GetReactionForce(inv_dt: PhysicsFloat): TVector2; override;
      function GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat; override;

      function GetLengthA: PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      function GetLengthB: PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      function GetGroundAnchorA: TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      function GetGroundAnchorB: TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Get the current length of the segment attached to bodyA.
	    function GetCurrentLengthA: PhysicsFloat;
      /// Get the current length of the segment attached to bodyB.
      function GetCurrentLengthB: PhysicsFloat;

      property GetRatio: PhysicsFloat read m_ratio;
   end;

   /// Revolute joint definition. This requires defining an
   /// anchor point where the bodies are joined. The definition
   /// uses local anchor points so that the initial configuration
   /// can violate the constraint slightly. You also need to
   /// specify the initial relative angle for joint limits. This
   /// helps when saving and loading a game.
   /// The local anchor points are measured from the body's origin
   /// rather than the center of mass because:
   /// 1. you might not know where the center of mass will be.
   /// 2. if you add/remove shapes from a body and recompute the mass,
   ///    the joints will be broken.
   Tb2RevoluteJointDef = class(Tb2JointDef)
   public
      localAnchorA: TVector2; /// The local anchor point relative to bodyA's origin.
      localAnchorB: TVector2; /// The local anchor point relative to bodyB's origin.
      referenceAngle: PhysicsFloat; /// The bodyB angle minus bodyA angle in the reference state (radians).

      enableLimit: Boolean; /// A flag to enable joint limits.
      lowerAngle, upperAngle: PhysicsFloat; /// The lower(upper) angle for the joint limit (radians).

      enableMotor: Boolean; /// A flag to enable the joint motor.
      motorSpeed: PhysicsFloat; /// The desired motor speed. Usually in radians per second.
      maxMotorTorque: PhysicsFloat; /// The maximum motor torque used to achieve the desired motor speed. Usually in N-m.

      constructor Create;
    	/// Initialize the bodies, anchors, and reference angle using the world anchor.
    	procedure Initialize(bodyA, bodyB: Tb2Body; const anchor: TVector2);
   end;

   /// A revolute joint constrains two bodies to share a common point while they
   /// are free to rotate about the point. The relative rotation about the shared
   /// point is the joint angle. You can limit the relative rotation with
   /// a joint limit that specifies a lower and upper angle. You can use a motor
   /// to drive the relative rotation about the shared point. A maximum motor torque
   /// is provided so that infinite forces are not generated.
   Tb2RevoluteJoint = class(Tb2Joint)
   protected
      // Solver shared
      m_localAnchorA, m_localAnchorB: TVector2;
      m_impulse: TVector3;
      m_motorImpulse: PhysicsFloat;

      m_enableMotor: Boolean;
      m_maxMotorTorque: PhysicsFloat;
      m_motorSpeed: PhysicsFloat;

      m_enableLimit: Boolean;
      m_referenceAngle: PhysicsFloat;
      m_lowerAngle, m_upperAngle: PhysicsFloat;

      // Solver temp
      m_indexA, m_indexB: Int32;
      m_rA, m_rB: TVector2;
      m_localCenterA, m_localCenterB: TVector2;
      m_invMassA, m_invMassB: PhysicsFloat;
      m_invIA, m_invIB: PhysicsFloat;
      m_mass: TMatrix33; // effective mass for point-to-point constraint.
      m_motorMass: PhysicsFloat; // effective mass for motor/limit angular constraint.
      m_limitState: Tb2LimitState;

      procedure InitVelocityConstraints(const data: Tb2SolverData); override;
      procedure SolveVelocityConstraints(const data: Tb2SolverData); override;
      function SolvePositionConstraints(const data: Tb2SolverData): Boolean; override;

   public
      constructor Create(def: Tb2RevoluteJointDef);

      {$IFDEF ENABLE_DUMP}
      procedure Dump; override;
      {$ENDIF}

      function GetAnchorA: TVector2; override;
      function GetAnchorB: TVector2; override;

      /// Get the reaction force given the inverse time step. Unit is N.
      function GetReactionForce(inv_dt: PhysicsFloat): TVector2; override;

      /// Get the reaction torque due to the joint limit given the inverse time step. Unit is N*m.
      function GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat; override;
      function GetJointAngle: PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Get the current joint angle speed in radians per second.
      function GetJointSpeed: PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

 	    /// Get the current motor torque given the inverse time step. Unit is N*m.
      function GetMotorTorque(inv_dt: PhysicsFloat): PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

	    procedure EnableLimit(flag: Boolean); /// Enable/disable the joint limit.
	    procedure SetLimits(lower, upper: PhysicsFloat); /// Set the joint limits in radians.
	    procedure EnableMotor(flag: Boolean); /// Enable/disable the joint motor.
	    procedure SetMotorSpeed(speed: PhysicsFloat); /// Set the motor speed in radians per second.
	    procedure SetMaxMotorTorque(torque: PhysicsFloat);  /// Set the maximum motor torque, usually in N-m.

      property IsLimitEnabled: Boolean read m_enableLimit;
      property IsMotorEnabled: Boolean read m_enableMotor;
      property GetLowerLimit: PhysicsFloat read m_lowerAngle;
      property GetUpperLimit: PhysicsFloat read m_upperAngle;
      property GetMotorSpeed: PhysicsFloat read m_motorSpeed;
      property GetMaxMotorTorque: PhysicsFloat read m_maxMotorTorque;

      property GetLocalAnchorA: TVector2 read m_localAnchorA;
      property GetLocalAnchorB: TVector2 read m_localAnchorB;
      property GetReferenceAngle: PhysicsFloat read m_referenceAngle;
   end;

   /// Gear joint definition. This definition requires two existing
   /// revolute or prismatic joints (any combination will work).
   /// The provided joints must attach a dynamic body to a static body.
   Tb2GearJointDef = class(Tb2JointDef)
   public
      joint1: Tb2Joint; /// The first revolute/prismatic joint attached to the gear joint.
      joint2: Tb2Joint; /// The second revolute/prismatic joint attached to the gear joint.
      ratio: PhysicsFloat; /// The gear ratio.

      constructor Create;
   end;

   /// A gear joint is used to connect two joints together. Either joint
   /// can be a revolute or prismatic joint. You specify a gear ratio
   /// to bind the motions together:
   /// coordinate1 + ratio * coordinate2 = constant
   /// The ratio can be negative or positive. If one joint is a revolute joint
   /// and the other joint is a prismatic joint, then the ratio will have units
   /// of length or units of 1/length.
   /// @warning You have to manually destroy the gear joint if joint1 or joint2
   /// is destroyed.
   Tb2GearJoint = class(Tb2Joint)
   protected
      m_joint1, m_joint2: Tb2Joint;
      m_typeA, m_typeB: Tb2JointType;

      // Body A is connected to body C
      // Body B is connected to body D
      m_bodyC, m_bodyD: Tb2Body;

      // Solver shared
      m_localAnchorA, m_localAnchorB, m_localAnchorC, m_localAnchorD: TVector2;
      m_localAxisC, m_localAxisD: TVector2;
      m_referenceAngleA, m_referenceAngleB: PhysicsFloat;

      m_constant, m_ratio: PhysicsFloat;
      m_impulse: PhysicsFloat;

      // Solver temp
      m_indexA, m_indexB, m_indexC, m_indexD: Int32;
      m_lcA, m_lcB, m_lcC, m_lcD: TVector2;
      m_mA, m_mB, m_mC, m_mD: PhysicsFloat;
      m_iA, m_iB, m_iC, m_iD: PhysicsFloat;
      m_JvAC, m_JvBD: TVector2;
      m_JwA, m_JwB, m_JwC, m_JwD: PhysicsFloat;
      m_mass: PhysicsFloat;

      procedure InitVelocityConstraints(const data: Tb2SolverData); override;
      procedure SolveVelocityConstraints(const data: Tb2SolverData); override;
      function SolvePositionConstraints(const data: Tb2SolverData): Boolean; override;

   public
      constructor Create(def: Tb2GearJointDef);

      {$IFDEF ENABLE_DUMP}
      procedure Dump; override;
      {$ENDIF}

      function GetAnchorA: TVector2; override;
      function GetAnchorB: TVector2; override;

      function GetReactionForce(inv_dt: PhysicsFloat): TVector2; override;
      function GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat; override;

      property Ratio: PhysicsFloat read m_ratio write m_ratio;
      property GetJoint1: Tb2Joint read m_joint1;
      property GetJoint2: Tb2Joint read m_joint2;
   end;

   /// Friction joint definition.
   Tb2FrictionJointDef = class(Tb2JointDef)
   public
      localAnchorA: TVector2; /// The local anchor point relative to bodyA's origin.
      localAnchorB: TVector2; /// The local anchor point relative to bodyB's origin.
      maxForce: PhysicsFloat; /// The maximum friction force in N.
      maxTorque: PhysicsFloat; /// The maximum friction torque in N-m.

      constructor Create;
      procedure Initialize(bodyA, bodyB: Tb2Body; const anchor: TVector2); /// Initialize the bodies.
   end;

   /// Friction joint. This is used for top-down friction.
   /// It provides 2D translational friction and angular friction.
   Tb2FrictionJoint = class(Tb2Joint)
   protected
      m_localAnchorA,
      m_localAnchorB: TVector2;

      // Solver shared
      m_linearImpulse: TVector2;
      m_angularImpulse: PhysicsFloat;

      m_maxForce: PhysicsFloat;
      m_maxTorque: PhysicsFloat;

      // Solver temp
      m_indexA, m_indexB: Int32;
      m_rA, m_rB: TVector2;
      m_localCenterA, m_localCenterB: TVector2;
      m_invMassA, m_invMassB: PhysicsFloat;
      m_invIA, m_invIB: PhysicsFloat;
      m_linearMass: TMatrix22;
      m_angularMass: PhysicsFloat;

      procedure InitVelocityConstraints(const data: Tb2SolverData); override;
      procedure SolveVelocityConstraints(const data: Tb2SolverData); override;
      function SolvePositionConstraints(const data: Tb2SolverData): Boolean; override;
   public
      constructor Create(def: Tb2FrictionJointDef);

      {$IFDEF ENABLE_DUMP}
      procedure Dump; override;
      {$ENDIF}

      function GetAnchorA: TVector2; override;
      function GetAnchorB: TVector2; override;

      function GetReactionForce(inv_dt: PhysicsFloat): TVector2; override;
      function GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat; override;

      property MaxForce: PhysicsFloat read m_maxForce write m_maxForce;
      property MaxTorque: PhysicsFloat read m_maxTorque write m_maxTorque;
      property GetLocalAnchorA: TVector2 read m_localAnchorA; /// The local anchor point relative to bodyA's origin.
      property GetLocalAnchorB: TVector2 read m_localAnchorB; /// The local anchor point relative to bodyB's origin.
   end;

   /// Wheel joint definition. This requires defining a line of
   /// motion using an axis and an anchor point. The definition uses local
   /// anchor points and a local axis so that the initial configuration
   /// can violate the constraint slightly. The joint translation is zero
   /// when the local anchor points coincide in world space. Using local
   /// anchors and a local axis helps when saving and loading a game.
   Tb2WheelJointDef = class(Tb2JointDef)
   public
      localAnchorA: TVector2; /// The local anchor point relative to bodyA's origin.
      localAnchorB: TVector2; /// The local anchor point relative to bodyB's origin.
      localAxisA: TVector2; /// The local translation axis in bodyA.

      enableMotor: Boolean; /// Enable/disable the joint motor.
      maxMotorTorque: PhysicsFloat; /// The maximum motor torque, usually in N-m.
      motorSpeed: PhysicsFloat; /// The desired motor speed in radians per second.

	    frequencyHz: PhysicsFloat; /// Suspension frequency, zero indicates no suspension
	    dampingRatio: PhysicsFloat; /// Suspension damping ratio, one indicates critical damping

      constructor Create;
      /// Initialize the bodies, anchors, axis, and reference angle using the world
      /// anchor and world axis.
      procedure Initialize(bodyA, bodyB: Tb2Body; const anchor, axis: TVector2);
   end;

   /// A wheel joint. This joint provides two degrees of freedom: translation
   /// along an axis fixed in bodyA and rotation in the plane. You can use a
   /// joint limit to restrict the range of motion and a joint motor to drive
   /// the rotation or to model rotational friction.
   /// This joint is designed for vehicle suspensions.
   Tb2WheelJoint = class(Tb2Joint)
   protected
      m_frequencyHz: PhysicsFloat;
      m_dampingRatio: PhysicsFloat;

      // Solver shared
      m_localAnchorA, m_localAnchorB: TVector2;
      m_localXAxisA, m_localYAxisA : TVector2;

      m_impulse, m_motorImpulse, m_springImpulse: PhysicsFloat;

      m_maxMotorTorque, m_motorSpeed: PhysicsFloat;
      m_enableMotor: Boolean;

      // Solver temp
      m_indexA, m_indexB: Int32;
      m_localCenterA, m_localCenterB: TVector2;
      m_invMassA, m_invMassB: PhysicsFloat;
      m_invIA, m_invIB: PhysicsFloat;

      m_ax, m_ay: TVector2;
      m_sAx, m_sBx: PhysicsFloat;
      m_sAy, m_sBy: PhysicsFloat;

      m_mass, m_motorMass, m_springMass: PhysicsFloat;

      m_bias, m_gamma: PhysicsFloat;

      procedure InitVelocityConstraints(const data: Tb2SolverData); override;
      procedure SolveVelocityConstraints(const data: Tb2SolverData); override;
      function SolvePositionConstraints(const data: Tb2SolverData): Boolean; override;

   public
      constructor Create(def: Tb2WheelJointDef);

      {$IFDEF ENABLE_DUMP}
      procedure Dump; override;
      {$ENDIF}

      function GetAnchorA: TVector2; override;
      function GetAnchorB: TVector2; override;

      function GetReactionForce(inv_dt: PhysicsFloat): TVector2; override;
      function GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat; override;

      /// Get the current joint translation, usually in meters.
      function GetJointTranslation: PhysicsFloat;

      /// Get the current joint translation speed, usually in meters per second.
      function GetJointSpeed: PhysicsFloat;

      /// Get the current motor torque given the inverse time step, usually in N-m.
      function GetMotorTorque(inv_dt: PhysicsFloat): PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      procedure EnableMotor(flag: Boolean); /// Enable/disable the joint motor.
      procedure SetMotorSpeed(speed: PhysicsFloat); /// Set the motor speed, usually in radians per second.
      procedure SetMaxMotorTorque(torque: PhysicsFloat); /// Set/Get the maximum motor force, usually in N-m.

      property IsMotorEnabled: Boolean read m_enableMotor; /// Is the joint motor enabled?
      property GetMotorSpeed: PhysicsFloat read m_motorSpeed; /// Get the motor speed, usually in radians per second.
      property GetMaxMotorTorque: PhysicsFloat read m_maxMotorTorque;

      /// Get/Set the spring frequency in hertz. Setting the frequency to zero disables the spring.
      property SpringFrequencyHz: PhysicsFloat read m_frequencyHz write m_frequencyHz;

      /// Get/Set the spring damping ratio
      property SpringDampingRatio: PhysicsFloat read m_dampingRatio write m_dampingRatio;

      property GetLocalAnchorA: TVector2 read m_localAnchorA;
      property GetLocalAnchorB: TVector2 read m_localAnchorB;
      property GetLocalAxisA: TVector2 read m_localXAxisA;
   end;

   /// Weld joint definition. You need to specify local anchor points
   /// where they are attached and the relative body angle. The position
   /// of the anchor points is important for computing the reaction torque.
   Tb2WeldJointDef = class(Tb2JointDef)
   public
      /// The local anchor point relative to bodyA's origin.
      localAnchorA: TVector2;

      /// The local anchor point relative to bodyB's origin.
      localAnchorB: TVector2;

      /// The bodyB angle minus bodyA angle in the reference state (radians).
      referenceAngle: PhysicsFloat;

      /// The mass-spring-damper frequency in Hertz. Rotation only.
      /// Disable softness with a value of 0.
      frequencyHz: PhysicsFloat;

      /// The damping ratio. 0 = no damping, 1 = critical damping.
      dampingRatio: PhysicsFloat;

      constructor Create;

      /// Initialize the bodies, anchors, and reference angle using a world
      /// anchor point.
      procedure Initialize(bodyA, bodyB: Tb2Body; const anchor: TVector2);
   end;

   /// A weld joint essentially glues two bodies together. A weld joint may
   /// distort somewhat because the island constraint solver is approximate.
   Tb2WeldJoint = class(Tb2Joint)
   protected
      m_frequencyHz: PhysicsFloat;
      m_dampingRatio: PhysicsFloat;
      m_bias: PhysicsFloat;

      // Solver shared
      m_localAnchorA, m_localAnchorB: TVector2;
      m_referenceAngle: PhysicsFloat;
      m_gamma: PhysicsFloat;
      m_impulse: TVector3;

      // Solver temp
      m_indexA, m_indexB: Int32;
      m_rA, m_rB: TVector2;
      m_localCenterA, m_localCenterB: TVector2;
      m_invMassA, m_invMassB: PhysicsFloat;
      m_invIA, m_invIB: PhysicsFloat;
      m_mass: TMatrix33;

      procedure InitVelocityConstraints(const data: Tb2SolverData); override;
      procedure SolveVelocityConstraints(const data: Tb2SolverData); override;
      function SolvePositionConstraints(const data: Tb2SolverData): Boolean; override;
   public
      constructor Create(def: Tb2WeldJointDef);

      {$IFDEF ENABLE_DUMP}
      procedure Dump; override;
      {$ENDIF}

      function GetAnchorA: TVector2; override;
      function GetAnchorB: TVector2; override;

      function GetReactionForce(inv_dt: PhysicsFloat): TVector2; override;
      function GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat; override;

      property GetLocalAnchorA: TVector2 read m_localAnchorA;
      property GetLocalAnchorB: TVector2 read m_localAnchorB;
      property GetReferenceAngle: PhysicsFloat read m_referenceAngle;
      property Frequency: PhysicsFloat read m_frequencyHz write m_frequencyHz;
      property DampingRatio: PhysicsFloat read m_dampingRatio write m_dampingRatio;
   end;

   /// Rope joint definition. This requires two body anchor points and
   /// a maximum lengths.
   /// Note: by default the connected objects will not collide.
   /// see collideConnected in b2JointDef.
   Tb2RopeJointDef = class(Tb2JointDef)
   public
      /// The local anchor point relative to bodyA's origin.
      localAnchorA: TVector2;

      /// The local anchor point relative to bodyB's origin.
      localAnchorB: TVector2;

      /// The maximum length of the rope.
      /// Warning: this must be larger than b2_linearSlop or
      /// the joint will have no effect.
      maxLength: PhysicsFloat;

      constructor Create;
   end;

   /// A rope joint enforces a maximum distance between two points
   /// on two bodies. It has no other effect.
   /// Warning: if you attempt to change the maximum length during
   /// the simulation you will get some non-physical behavior.
   /// A model that would allow you to dynamically modify the length
   /// would have some sponginess, so I chose not to implement it
   /// that way. See b2DistanceJoint if you want to dynamically
   /// control length.
   Tb2RopeJoint = class(Tb2Joint)
   protected
      // Solver shared
      m_localAnchorA, m_localAnchorB: TVector2;
      m_maxLength, m_length: PhysicsFloat;
      m_impulse: PhysicsFloat;

      // Solver temp
      m_indexA, m_indexB: Int32;
      m_u: TVector2;
      m_rA, m_rB: TVector2;
      m_localCenterA, m_localCenterB: TVector2;
      m_invMassA, m_invMassB: PhysicsFloat;
      m_invIA, m_invIB: PhysicsFloat;
      m_mass: PhysicsFloat;
      m_state: Tb2LimitState;

      procedure InitVelocityConstraints(const data: Tb2SolverData); override;
      procedure SolveVelocityConstraints(const data: Tb2SolverData); override;
      function SolvePositionConstraints(const data: Tb2SolverData): Boolean; override;
   public
      constructor Create(def: Tb2RopeJointDef);

      {$IFDEF ENABLE_DUMP}
      procedure Dump; override;
      {$ENDIF}

      function GetAnchorA: TVector2; override;
      function GetAnchorB: TVector2; override;

      function GetReactionForce(inv_dt: PhysicsFloat): TVector2; override;
      function GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat; override;

      property MaxLength: PhysicsFloat read m_maxLength write m_maxLength; /// Get the maximum length of the rope.
      property LimitState: Tb2LimitState read m_state;

      property GetLocalAnchorA: TVector2 read m_localAnchorA;
      property GetLocalAnchorB: TVector2 read m_localAnchorB;
   end;

   /// Motor joint definition.
   Tb2MotorJointDef = class(Tb2JointDef)
   public
      /// Position of bodyB minus the position of bodyA, in bodyA's frame, in meters.
      linearOffset: TVector2;

      /// The bodyB angle minus bodyA angle in radians.
      angularOffset: PhysicsFloat;

      /// The maximum motor force in N.
      maxForce: PhysicsFloat;

      /// The maximum motor torque in N-m.
      maxTorque: PhysicsFloat;

      /// Position correction factor in the range [0,1].
      correctionFactor: PhysicsFloat;

      constructor Create;

      /// Initialize the bodies and offsets using the current transforms.
      procedure Initialize(bodyA, bodyB: Tb2Body);
   end;

   /// A motor joint is used to control the relative motion
   /// between two bodies. A typical usage is to control the movement
   /// of a dynamic body with respect to the ground.
   Tb2MotorJoint = class(Tb2Joint)
   protected
      // Solver shared
      m_linearOffset, m_linearImpulse: TVector2;
      m_angularOffset, m_angularImpulse: PhysicsFloat;
      m_maxForce, m_maxTorque: PhysicsFloat;
      m_correctionFactor: PhysicsFloat;

      // Solver temp
      m_indexA, m_indexB: Int32;
      m_rA, m_rB: TVector2;
      m_localCenterA, m_localCenterB: TVector2;
      m_linearError: TVector2;
      m_angularError: PhysicsFloat;
      m_invMassA, m_invMassB: PhysicsFloat;
      m_invIA, m_invIB: PhysicsFloat;
      m_linearMass: TMatrix22;
      m_angularMass: PhysicsFloat;

      procedure FSetCorrectionFactor(value: PhysicsFloat);

      procedure InitVelocityConstraints(const data: Tb2SolverData); override;
      procedure SolveVelocityConstraints(const data: Tb2SolverData); override;
      function SolvePositionConstraints(const data: Tb2SolverData): Boolean; override;
   public
      constructor Create(def: Tb2MotorJointDef);

      {$IFDEF ENABLE_DUMP}
      procedure Dump; override;
      {$ENDIF}

      function GetAnchorA: TVector2; override;
      function GetAnchorB: TVector2; override;

      function GetReactionForce(inv_dt: PhysicsFloat): TVector2; override;
      function GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat; override;

      /// Set/get the target linear offset, in frame A, in meters.
      procedure SetLinearOffset(const linearOffset: TVector2);
      procedure SetAngularOffset(angularOffset: PhysicsFloat);

      property MaxForce: PhysicsFloat read m_maxForce write m_maxForce;
      property MaxTorque: PhysicsFloat read m_maxTorque write m_maxTorque;
      property GetLinearOffset: TVector2 read m_linearOffset;
      property GetAngularOffset: PhysicsFloat read m_angularOffset;
      property CorrectionFactor: PhysicsFloat read m_correctionFactor write FSetCorrectionFactor; // in the range [0,1]
   end;

   Tb2RopeDef = class
   public
      vertices: TVectorArray;
      count: Int32;
      masses: TPhysicsFloatArray;
      gravity: TVector2;
      damping: PhysicsFloat;
      k2: PhysicsFloat; // Stretching stiffness
      k3: PhysicsFloat; // Bending stiffness. Values above 0.5 can make the simulation blow up.

      constructor Create;
   end;

   Tb2Rope = class
   private
      m_count: Int32;
      m_ps: TVectorArray;
      m_p0s: TVectorArray;
      m_vs: TVectorArray;

      m_ims: TPhysicsFloatArray;
      m_Ls: TPhysicsFloatArray;
      m_as: TPhysicsFloatArray;

      m_gravity: TVector2;
      m_damping: PhysicsFloat;

      m_k2: PhysicsFloat;
      m_k3: PhysicsFloat;

      procedure	SolveC2;
      procedure	SolveC3;

   public
      constructor Create(def: Tb2RopeDef; AutoFreeDef: Boolean = True);

      procedure Step(timeStep: PhysicsFloat; iterations: Int32);

      procedure Draw(draw: Tb2Draw);
      procedure SetAngle(angle: PhysicsFloat);

      property VertexCount: Int32 read m_count;
      property Vertices: TVectorArray read m_ps;
   end;

//////////////////////////////////////////////////////////////////

procedure b2GetPointStates(var state1, state2: Tb2PointStateArray;
   const manifold1, manifold2: Tb2Manifold);

// Evaluate functions for different contacts
procedure b2CollideCircles(contact: Pb2Contact; var manifold: Tb2Manifold; A, B: TObject;
   const xfA, xfB: Tb2Transform; ABfixture: Boolean);
procedure b2CollidePolygonAndCircle(contact: Pb2Contact; var manifold: Tb2Manifold; A, B: TObject;
   const xfA, xfB: Tb2Transform; ABfixture: Boolean);
procedure b2CollideEdgeAndCircle(contact: Pb2Contact; var manifold: Tb2Manifold;
   A, B: TObject; const xfA, xfB: Tb2Transform; ABfixture: Boolean);
procedure b2CollideEdgeAndPolygon(contact: Pb2Contact; var manifold: Tb2Manifold;
   A, B: TObject; const xfA, xfB: Tb2Transform; ABfixture: Boolean);
procedure b2CollideChainAndCircle(contact: Pb2Contact; var manifold: Tb2Manifold;
   A, B: TObject; const xfA, xfB: Tb2Transform; ABfixture: Boolean);
procedure b2CollideChainAndPolygon(contact: Pb2Contact; var manifold: Tb2Manifold;
   A, B: TObject; const xfA, xfB: Tb2Transform; ABfixture: Boolean);
procedure b2CollidePolygons(contact: Pb2Contact; var manifold: Tb2Manifold; A, B: TObject;
   const xfA, xfB: Tb2Transform; ABfixture: Boolean);

procedure b2Distance(var output: Tb2DistanceOutput;	var cache: Tb2SimplexCache;
   const input: Tb2DistanceInput);

function b2TestOverlap(const a, b: Tb2AABB): Boolean; overload;
function b2TestOverlap(shapeA, shapeB: Tb2Shape; indexA, indexB: Int32;
   const xfA, xfB: Tb2Transform): Boolean; overload;

function b2TimeOfImpact(var output: Tb2TOIOutput; const input: Tb2TOIInput): PhysicsFloat;

{$IFNDEF OP_OVERLOAD}
// Methods for records

/// Tb2Contact
function GetManifold(const contact: Tb2Contact): Pb2Manifold; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure GetWorldManifold(const contact: Tb2Contact; var worldManifold: Tb2WorldManifold); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure FlagForFiltering(var contact: Tb2Contact); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function IsTouching(const contact: Tb2Contact): Boolean; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure SetEnabled(var contact: Tb2Contact; flag: Boolean); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function IsEnabled(const contact: Tb2Contact): Boolean; {$IFDEF INLINE_AVAIL}inline;{$ENDIF} /// Has this contact been disabled?
procedure Update(var contact: Tb2Contact; listener: Tb2ContactListener);
procedure ResetFriction(var contact: Tb2Contact); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure ResetRestitution(var contact: Tb2Contact); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

/// Tb2DistanceProxy
procedure SetShape(var dp: Tb2DistanceProxy; shape: Tb2Shape; index: Int32);
function GetSupport(const dp: Tb2DistanceProxy; const d: TVector2): Int32;
function GetSupportVertex(const dp: Tb2DistanceProxy; const d: TVector2): PVector2;

/// Tb2WorldManifold
procedure Initialize(var worldManifold: Tb2WorldManifold;
   const manifold: Tb2Manifold; const xfA, xfB: Tb2Transform; radiusA, radiusB: PhysicsFloat);

/// Tb2AABB
function IsValid(const AABB: Tb2AABB): Boolean; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function GetCenter(const AABB: Tb2AABB): TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function GetExtents(const AABB: Tb2AABB): TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function GetPerimeter(const AABB: Tb2AABB): PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure Combine(var AABB: Tb2AABB; const _aabb: Tb2AABB); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure Combine(var AABB: Tb2AABB; const aabb1, aabb2: Tb2AABB); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function Contains(const AABB, _aabb: Tb2AABB): Boolean;
function RayCast(const AABB: Tb2AABB; var output: Tb2RayCastOutput; const input: Tb2RayCastInput): Boolean;

/// Tb2TreeNode
function IsLeaf(const node: Tb2TreeNode): Boolean; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
{$ENDIF}

{$IFDEF COMPUTE_PHYSICS_TIME}
function GetRawReferenceTime: Double;
{$ENDIF}

/////////////////// Color functions //////
function MakeColor(r, g, b: Single; a: Single = 1.0): RGBA;

var
   b2_gjkCalls, b2_gjkIters, b2_gjkMaxIters: Int32;
   b2_toiCalls, b2_toiIters, b2_toiMaxIters: Int32;
	 b2_toiTime, b2_toiMaxTime: Double;
   b2_toiRootIters, b2_toiMaxRootIters: Int32;

const
   b2_nullNode = -1;

implementation

var
   b2_defaultFilter: Tb2ContactFilter;
   b2_defaultListener: Tb2ContactListener;

   {$IFDEF COMPUTE_PHYSICS_TIME}
//   vCounterFrequency: Int64;
   AStopwatch: TStopwatch;
{$ENDIF}

const
   e_nullProxy = -1;

   // Tb2World
   e_world_newFixture	= 1;
   e_world_locked	= 2;
   e_world_clearForces = 4;

   // Tb2Contact.m_flags
   e_contact_islandFlag	 = $1; // Used when crawling contact graph when forming islands.
   e_contact_touchingFlag	= $2; // Set when the shapes are touching.
   e_contact_enabledFlag = $4; // This contact can be disabled (by user)
   e_contact_filterFlag	= $8; // This contact needs filtering because a fixture filter was changed.
	 e_contact_bulletHitFlag = $10; // This bullet contact had a TOI event
   e_contact_toiFlag = $20; // This contact has a valid TOI in m_toi

   // Tb2Body.m_flags
   e_body_islandFlag = $1;
   e_body_awakeFlag	= $2;
   e_body_autoSleepFlag	= $4;
   e_body_bulletFlag = $8;
   e_body_fixedRotationFlag	= $10;
   e_body_activeFlag	= $20;
   e_body_toiFlag = $40;
   e_body_ignoreCollideFlag = $80;

   // Tb2ContactFeature
   e_contact_feature_vertex = 0;
   e_contact_feature_face = 1;

function MakeColor(r, g, b: Single; a: Single = 1.0): RGBA;
begin
   Result[0] := r;
   Result[1] := g;
   Result[2] := b;
   Result[3] := a;
end;

{$IFDEF ENABLE_DUMP}

var
   b2DumpMethod: Tb2DumpMethod;

function b2FloatToStr(value: PhysicsFloat): string;
var
   Buffer: array[0..63] of Char;
{$IFNDEF EXTENDED_PRECISION}
   extValue: Extended;
{$ENDIF}
begin
   {$IFDEF EXTENDED_PRECISION}
   SetString(Result, Buffer, FloatToText(Buffer, value, fvExtended, ffGeneral, 15, 0));
   {$ELSE}
      extValue := value;
      {$IFDEF DOUBLE_PRECISION}
      SetString(Result, Buffer, FloatToText(Buffer, extValue, fvExtended, ffGeneral, 15, 0));
      {$ELSE}
      SetString(Result, Buffer, FloatToText(Buffer, extValue, fvExtended, ffGeneral, 7, 0));
      {$ENDIF}
   {$ENDIF}
end;

function b2BoolToStr(value: Integer): string; overload;
begin
   if value = 0 then
      Result := 'False'
   else
      Result := 'True';
end;

function b2BoolToStr(value: Boolean): string; overload;
begin
   if value then
      Result := 'True'
   else
      Result := 'False';
end;

{$ENDIF}

//////////// Implements <b2Contact.cpp> InitializeRegisters and AddType
type
   PContactCreateRecord = ^TContactCreateRecord;
   TContactCreateRecord = record
      //ContactType: Tb2ContactType;
      EvaluateProc: Tb2ContactEvaluateProc;
      Primary: Boolean;
   end;

const
   // e_circleShape, e_edgeShape, e_polygonShape, e_chainShape
   ContactCreateRecords: array[e_circleShape..e_chainShape,
      e_circleShape..e_chainShape] of TContactCreateRecord = (
      // e_circleShape with
      ((EvaluateProc: b2CollideCircles; Primary: True), // with e_circleShape
       (EvaluateProc: b2CollideEdgeAndCircle; Primary: False), // with e_edgeShape
       (EvaluateProc: b2CollidePolygonAndCircle; Primary: False), // with e_polygonShape
       (EvaluateProc: b2CollideChainAndCircle; Primary: False)), // with e_chainShape

      // e_edgeShape with
      ((EvaluateProc: b2CollideEdgeAndCircle; Primary: True), // with e_circleShape
       (EvaluateProc: nil; Primary: True), // with e_edgeShape
       (EvaluateProc: b2CollideEdgeAndPolygon; Primary: True), // with e_polygonShape
       (EvaluateProc: nil; Primary: True)), // with e_chainShape

      // e_polygonShape with
      ((EvaluateProc: b2CollidePolygonAndCircle; Primary: True), // with e_circleShape
       (EvaluateProc: b2CollideEdgeAndPolygon; Primary: False), // with e_edgeShape
       (EvaluateProc: b2CollidePolygons; Primary: True), // with e_polygonShape
       (EvaluateProc: b2CollideChainAndPolygon; Primary: False)), // with e_chainShape

      // e_chainShape with
      ((EvaluateProc: b2CollideChainAndCircle; Primary: True), // with e_circleShape
       (EvaluateProc: nil; Primary: False), // with e_edgeShape
       (EvaluateProc: b2CollideChainAndPolygon; Primary: True), // with e_polygonShape
       (EvaluateProc: nil; Primary: False)) // with e_chainShape
       );

/// Friction mixing law. Feel free to customize this.
function b2MixFriction(friction1, friction2: PhysicsFloat): PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
begin
	 Result := Sqrt(friction1 * friction2);
end;

/// Restitution mixing law. Feel free to customize this.
function b2MixRestitution(restitution1, restitution2: PhysicsFloat): PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
begin
   if restitution1 > restitution2 then
      Result := restitution1
   else
      Result := restitution2;
end;

function NewContact(fixtureA, fixtureB: Tb2Fixture; indexA, indexB: Int32): Pb2Contact;
var
   rec: PContactCreateRecord;
begin
   Result := nil;

   rec := @ContactCreateRecords[fixtureA.m_shape.m_type][fixtureB.m_shape.m_type];
   if @rec^.EvaluateProc = nil then
      Exit;

   New(Result);
   if not Assigned(Result) then
      Exit;

   FillChar(Result^, SizeOf(Tb2Contact), 0);
   with Result^ do
   begin
      m_flags := e_contact_enabledFlag;
      //m_type := ContactType;
      m_evaluateProc := rec^.EvaluateProc;
      if rec^.Primary then
      begin
         m_fixtureA := fixtureA;
         m_fixtureB := fixtureB;
         m_indexA := indexA;
         m_indexB := indexB;
      end
      else
      begin
         m_fixtureA := fixtureB;
         m_fixtureB := fixtureA;
         m_indexA := indexB;
         m_indexB := indexA;
      end;

      m_friction := b2MixFriction(m_fixtureA.m_friction, m_fixtureB.m_friction);
      m_restitution := b2MixRestitution(m_fixtureA.m_restitution, m_fixtureB.m_restitution);
   end;
end;

procedure FreeContact(pc: Pb2Contact);
begin
   with pc^ do
      if (m_manifold.pointCount > 0) and (not m_fixtureA.IsSensor) and (not m_fixtureB.IsSensor) then
      begin
         m_fixtureA.m_body.SetAwake(True);
         m_fixtureB.m_body.SetAwake(True);
      end;
   Dispose(pc);
end;

{$IFNDEF OP_OVERLOAD}
// Record methods

/// Tb2Contact
function GetManifold(const contact: Tb2Contact): Pb2Manifold;
begin
   Result := @contact.m_manifold;
end;

procedure GetWorldManifold(const contact: Tb2Contact; var worldManifold: Tb2WorldManifold);
begin
   with contact do
      {$IFDEF OP_OVERLOAD}
      worldManifold.Initialize(m_manifold, m_fixtureA.m_body.m_xf,
         m_fixtureB.m_body.m_xf, m_fixtureA.m_shape.m_radius, m_fixtureB.m_shape.m_radius);
      {$ELSE}
      Initialize(worldManifold, m_manifold, m_fixtureA.m_body.m_xf,
         m_fixtureB.m_body.m_xf, m_fixtureA.m_shape.m_radius, m_fixtureB.m_shape.m_radius);
      {$ENDIF}
end;

procedure FlagForFiltering(var contact: Tb2Contact);
begin
   with contact do
      m_flags := m_flags or e_contact_filterFlag;
end;

function IsTouching(const contact: Tb2Contact): Boolean;
begin
   Result := (contact.m_flags and e_contact_touchingFlag) = e_contact_touchingFlag;
end;

procedure SetEnabled(var contact: Tb2Contact; flag: Boolean);
begin
   with contact do
      if flag then
         m_flags := m_flags or e_contact_enabledFlag
      else
         m_flags := m_flags and (not e_contact_enabledFlag);
end;

function IsEnabled(const contact: Tb2Contact): Boolean;
begin
   Result := (contact.m_flags and e_contact_enabledFlag) = e_contact_enabledFlag;
end;

procedure Update(var contact: Tb2Contact; listener: Tb2ContactListener);
var
   i, j: Integer;
   oldManifold: Tb2Manifold;
   touching, wasTouching, sensor: Boolean;
   bodyA, bodyB: Tb2Body;
   mp1, mp2: Pb2ManifoldPoint;
   id2key: UInt32;
   found: Boolean;
begin
   with contact do
   begin
      oldManifold := m_manifold;
      m_flags := m_flags or e_contact_enabledFlag; // Re-enable this contact.

      //touching := False;
      wasTouching := (m_flags and e_contact_touchingFlag) = e_contact_touchingFlag;

      sensor := m_fixtureA.IsSensor or m_fixtureB.IsSensor;

      bodyA := m_fixtureA.m_body;
      bodyB := m_fixtureB.m_body;

      // Is this contact a sensor?
      if sensor then
      begin
         touching := b2TestOverlap(m_fixtureA.m_shape, m_fixtureB.m_shape,
            m_indexA, m_indexB, bodyA.m_xf, bodyB.m_xf);

         // Sensors don't generate manifolds.
         m_manifold.pointCount := 0;
      end
      else
      begin
         m_evaluateProc(@contact, m_manifold, m_fixtureA, m_fixtureB, bodyA.m_xf, bodyB.m_xf, True);
         touching := m_manifold.pointCount > 0;

         // Match old contact ids to new contact ids and copy the
         // stored impulses to warm start the solver.
         for i := 0 to m_manifold.pointCount - 1 do
         begin
            mp2 := @m_manifold.points[i];
            mp2^.normalImpulse := 0.0;
            mp2^.tangentImpulse := 0.0;
            id2key := mp2^.id.key;
            found := False;

            for j := 0 to oldManifold.pointCount - 1 do
            begin
               mp1 := @oldManifold.points[j];

               if mp1^.id.key = id2key then
               begin
                  mp2^.normalImpulse := mp1^.normalImpulse;
                  mp2^.tangentImpulse := mp1^.tangentImpulse;
                  found := True;
                  Break;
               end;
            end;

            if not found then
            begin
               mp2^.normalImpulse := 0.0;
               mp2^.tangentImpulse := 0.0;
            end;
         end;

         if touching xor wasTouching then
         begin
            bodyA.SetAwake(True);
            bodyB.SetAwake(True);
         end;
      end;

      if touching then
         m_flags := m_flags or e_contact_touchingFlag
      else
         m_flags := m_flags and (not e_contact_touchingFlag);

      if Assigned(listener) then
      begin
         if (not wasTouching) and touching then
            listener.BeginContact(contact);

         if wasTouching and (not touching) then
            listener.EndContact(contact);

         if (not sensor) and touching then
            listener.PreSolve(contact, oldManifold);
      end;
   end;
end;

procedure ResetFriction(var contact: Tb2Contact);
begin
   with contact do
      m_friction := b2MixFriction(m_fixtureA.m_friction, m_fixtureB.m_friction);
end;

procedure ResetRestitution(var contact: Tb2Contact);
begin
   with contact do
      m_restitution := b2MixRestitution(m_fixtureA.m_restitution, m_fixtureB.m_restitution);
end;

/// Tb2WorldManifold
procedure Initialize(var worldManifold: Tb2WorldManifold;
   const manifold: Tb2Manifold; const xfA, xfB: Tb2Transform; radiusA, radiusB: PhysicsFloat);
var
   i: Integer;
   pointA, pointB, cA, cB, planePoint, clipPoint: TVector2;
begin
   if manifold.pointCount = 0 then
      Exit;

   with worldManifold do
      case manifold.manifoldType of
         e_manifold_circles:
            begin
               normal.x := 1.0;
               normal.y := 0.0;

               pointA := b2Mul(xfA, manifold.localPoint);
               pointB := b2Mul(xfB, manifold.points[0].localPoint);
               if b2DistanceSquared(pointA, pointB) > FLT_EPSILON * FLT_EPSILON then
               begin
                  normal := Subtract(pointB, pointA);
                  Normalize(normal);
               end;

               cA := Add(pointA, Multiply(normal, radiusA));
               cB := Subtract(pointB, Multiply(normal, radiusB));
               points[0] := b2MiddlePoint(cA, cB);
               separations[0] := b2Dot(Subtract(cB, cA), normal);
            end;
         e_manifold_faceA:
            begin
               normal := b2Mul(xfA.q, manifold.localNormal);
               planePoint := b2Mul(xfA, manifold.localPoint);

               for i := 0 to manifold.pointCount - 1 do
               begin
                  clipPoint := b2Mul(xfB, manifold.points[i].localPoint{manifold.points});
                  cA := Add(clipPoint, Multiply(normal,
                     (radiusA - b2Dot(Subtract(clipPoint, planePoint), normal))));
                  cB := Subtract(clipPoint, Multiply(normal, radiusB));
                  points[i] := b2MiddlePoint(cA, cB);
                  separations[i] := b2Dot(Subtract(cB, cA), normal);
               end;
            end;
         e_manifold_faceB:
            begin
               normal := b2Mul(xfB.q, manifold.localNormal);
               planePoint := b2Mul(xfB, manifold.localPoint);

               for i := 0 to manifold.pointCount - 1 do
               begin
                  clipPoint := b2Mul(xfA, manifold.points[i].localPoint{manifold.points});
                  cB := Add(clipPoint, Multiply(normal,
                     (radiusB - b2Dot(Subtract(clipPoint, planePoint), normal))));
                  cA := Subtract(clipPoint, Multiply(normal, radiusA));
                  points[i] := b2MiddlePoint(cA, cB);
                  separations[i] := b2Dot(Subtract(cA, cB), normal);
               end;

               // Ensure normal points from A to B.
               normal := Negative(normal);
            end;
      end;
end;

/// Tb2AABB
function IsValid(const AABB: Tb2AABB): Boolean;
var
   d: TVector2;
begin
   with AABB do
   begin
      d := Subtract(upperBound, lowerBound);
      Result := (d.x >= 0.0) and (d.y >= 0.0) and
         UPhysics2DTypes.IsValid(upperBound) and UPhysics2DTypes.IsValid(lowerBound);
   end;
end;

function GetCenter(const AABB: Tb2AABB): TVector2;
begin
   with AABB do
      Result := b2MiddlePoint(lowerBound, upperBound);
end;

function GetExtents(const AABB: Tb2AABB): TVector2;
begin
   with AABB do
      Result := Multiply(Subtract(upperBound, lowerBound), 0.5);
end;

function GetPerimeter(const AABB: Tb2AABB): PhysicsFloat;
begin
   with AABB do
      Result := 2.0 * ((upperBound.x - lowerBound.x) + (upperBound.y - lowerBound.y));
end;

procedure Combine(var AABB: Tb2AABB; const _aabb: Tb2AABB);
begin
   with AABB do
   begin
   	  lowerBound := b2Min(lowerBound, _aabb.lowerBound);
   	  upperBound := b2Max(upperBound, _aabb.upperBound);
   end;
end;

procedure Combine(var AABB: Tb2AABB; const aabb1, aabb2: Tb2AABB);
begin
   with AABB do
   begin
      lowerBound := b2Min(aabb1.lowerBound, aabb2.lowerBound);
      upperBound := b2Max(aabb1.upperBound, aabb2.upperBound);
   end;
end;

function Contains(const AABB, _aabb: Tb2AABB): Boolean;
begin
   with AABB do
   begin
      Result := True;
      Result := Result and (lowerBound.x <= _aabb.lowerBound.x);
      Result := Result and (lowerBound.y <= _aabb.lowerBound.y);
      Result := Result and (_aabb.upperBound.x <= upperBound.x);
      Result := Result and (_aabb.upperBound.y <= upperBound.y);
   end;
end;

function RayCast(const AABB: Tb2AABB; var output: Tb2RayCastOutput;
   const input: Tb2RayCastInput): Boolean;
var
   i: Integer;
   tmin, tmax, inv_d, t1, t2, s: PhysicsFloat;
   p, d, absD, normal: TVector2;
begin
   with AABB do
   begin
      tmin := -FLT_MAX;
      tmax := FLT_MAX;

      p := input.p1;
      d := Subtract(input.p2, input.p1);
      absD := b2Abs(d);

      for i := 0 to 1 do
      begin
         if TVector2Arraied(absD)[i] < FLT_EPSILON then
         begin
            // Parallel.
            if (TVector2Arraied(p)[i] < TVector2Arraied(lowerBound)[i]) or
               (TVector2Arraied(upperBound)[i] < TVector2Arraied(p)[i]) then
            begin
               Result := False;
               Exit;
            end;
         end
         else
         begin
            inv_d := 1.0 / TVector2Arraied(d)[i];
            t1 := (TVector2Arraied(lowerBound)[i] - TVector2Arraied(p)[i]) * inv_d;
            t2 := (TVector2Arraied(upperBound)[i] - TVector2Arraied(p)[i]) * inv_d;

            // Sign of the normal vector.
            s := -1.0;

            if t1 > t2 then
            begin
               b2Swap(t1, t2);
               s := 1.0;
            end;

            // Push the min up
            if t1 > tmin then
            begin
               normal := b2Vec2_Zero;
               TVector2Arraied(normal)[i] := s;
               tmin := t1;
            end;

            // Pull the max down
            tmax := b2Min(tmax, t2);

            if tmin > tmax then
            begin
               Result := False;
               Exit;
            end;
         end;
      end;

      // Does the ray start inside the box?
      // Does the ray intersect beyond the max fraction?
      if (tmin < 0.0) or (input.maxFraction < tmin) then
      begin
         Result := False;
         Exit;
      end;

      // Intersection.
      output.fraction := tmin;
      output.normal := normal;
      Result := True;
   end;
end;

{$ENDIF}

{ b2PolygonShape.cpp}

function ComputeCentroid(const vs: Tb2PolyVertices; count: Int32): TVector2;
const
   inv3 = 1 / 3;
var
   i: Integer;
   pRef, p1, p2, p3, e1, e2: TVector2;
   area, triangleArea: PhysicsFloat;
begin
   //b2Assert(count >= 3);

   area := 0.0;

   // pRef is the reference point for forming triangles.
   // It's location doesn't change the result (except for rounding error).
   Result := b2Vec2_Zero;
   pRef := b2Vec2_Zero;

  (* // This code would put the reference point inside the polygon.
   for (int32 i = 0; i < count; ++i)
   {
     pRef += vs[i];
   }
   pRef *= 1.0f / count; *)

   for i := 0 to count - 1 do
   begin
      // Triangle vertices.
      p1 := pRef;
      p2 := vs[i];
      if i + 1 < count then
         p3 := vs[i + 1]
      else
         p3 := vs[0];

      {$IFDEF OP_OVERLOAD}
      e1 := p2 - p1;
      e2 := p3 - p1;
      {$ELSE}
      e1 := Subtract(p2, p1);
      e2 := Subtract(p3, p1);
      {$ENDIF}

      triangleArea := 0.5 * b2Cross(e1, e2);
      area := area + triangleArea;

      // Area weighted centroid

      {$IFDEF OP_OVERLOAD}
      Result.AddBy(triangleArea * inv3 * (p1 + p2 + p3));
      {$ELSE}
      AddBy(Result, Multiply(Add(p1, p2, p3), triangleArea * inv3));
      {$ENDIF}
   end;

   // Centroid
   //b2Assert(area > B2_FLT_EPSILON);
   {$IFDEF OP_OVERLOAD}
   Result := Result / area;
   {$ELSE}
   DivideBy(Result, area);
   {$ENDIF}
end;

{ b2Distance.cpp }
{$IFDEF OP_OVERLOAD}
{ Tb2DistanceProxy }
procedure Tb2DistanceProxy.SetShape(shape: Tb2Shape; index: Int32);
begin
   case shape.m_type of
      e_circleShape:
         with Tb2CircleShape(shape) do
         begin
            m_vertices := @m_p;
            m_count := 1;
            Self.m_radius := m_radius;
         end;
      e_polygonShape:
         with Tb2PolygonShape(shape) do
         begin
            Self.m_vertices := @m_vertices[0];
            Self.m_count := m_count;
            Self.m_radius := m_radius;
         end;
      e_edgeShape:
         with Tb2EdgeShape(shape) do
         begin
            Self.m_vertices := @m_vertex1;
            Self.m_count := 2;
            Self.m_radius := m_radius;
         end;
      e_chainShape:
         with Tb2ChainShape(shape) do
         begin
            //b2Assert(0 <= index && index < chain->m_count);
            m_buffer[0] := m_vertices[index];
            if index + 1 < m_count then
               m_buffer[1] := m_vertices[index + 1]
            else
               m_buffer[1] := m_vertices[0];

            Self.m_vertices := @m_buffer[0];
            Self.m_count := 2;
            Self.m_radius := m_radius;
         end;
   end;
end;

function Tb2DistanceProxy.GetSupport(const d: TVector2): Int32;
var
   i: Integer;
   bestValue, value: PhysicsFloat;
   {$IFNDEF SUPPORT_POINTER_MATH}p: PVector2;{$ENDIF}
begin
   Result := 0;
   {$IFDEF SUPPORT_POINTER_MATH}
   bestValue := b2Dot(m_vertices[0], d);
   for i := 1 to m_count - 1 do
   begin
      value := b2Dot(m_vertices[i], d);
      if value > bestValue then
      begin
         Result := i;
         bestValue := value;
      end;
   end;
   {$ELSE}
   bestValue := b2Dot(m_vertices^, d);
   for i := 1 to m_count - 1 do
   begin
      p := m_vertices;
      Inc(p, i);
      value := b2Dot(p^, d);
      if value > bestValue then
      begin
         Result := i;
         bestValue := value;
      end;
   end;
   {$ENDIF}
end;

function Tb2DistanceProxy.GetSupportVertex(const d: TVector2): PVector2;
var
   i, bestIndex: Integer;
   bestValue, value: PhysicsFloat;
   {$IFNDEF SUPPORT_POINTER_MATH}p: PVector2;{$ENDIF}
begin
   bestIndex := 0;
   {$IFDEF SUPPORT_POINTER_MATH}
   bestValue := b2Dot(m_vertices[0], d);
   for i := 1 to m_count - 1 do
   begin
      value := b2Dot(m_vertices[i], d);
      if value > bestValue then
      begin
         bestIndex := i;
         bestValue := value;
      end;
   end;
   Result := @m_vertices[bestIndex];
   {$ELSE}
   bestValue := b2Dot(m_vertices^, d);
   for i := 1 to m_count - 1 do
   begin
      p := m_vertices;
      Inc(p, i);
      value := b2Dot(p^, d);
      if value > bestValue then
      begin
         bestIndex := i;
         bestValue := value;
      end;
   end;
   Result := m_vertices;
   Inc(Result, bestIndex);
   {$ENDIF}
end;
{$ELSE}
procedure SetShape(var dp: Tb2DistanceProxy; shape: Tb2Shape; index: Int32);
begin
   with dp do
      case shape.m_type of
         e_circleShape:
            with Tb2CircleShape(shape) do
            begin
               dp.m_vertices := @m_p;
               dp.m_count := 1;
               dp.m_radius := m_radius;
            end;
         e_polygonShape:
            with Tb2PolygonShape(shape) do
            begin
               dp.m_vertices := @m_vertices[0];
               dp.m_count := m_count;
               dp.m_radius := m_radius;
            end;
         e_edgeShape:
            with Tb2EdgeShape(shape) do
            begin
               dp.m_vertices := @m_vertex1;
               dp.m_count := 2;
               dp.m_radius := m_radius;
            end;
         e_chainShape:
            with Tb2ChainShape(shape) do
            begin
               //b2Assert(0 <= index && index < chain->m_count);
               m_buffer[0] := m_vertices[index];
               if index + 1 < m_count then
                  m_buffer[1] := m_vertices[index + 1]
               else
                  m_buffer[1] := m_vertices[0];

               dp.m_vertices := @m_buffer[0];
               dp.m_count := 2;
               dp.m_radius := m_radius;
            end;
      end;
end;

function GetSupport(const dp: Tb2DistanceProxy; const d: TVector2): Int32;
var
   i: Integer;
   bestValue, value: PhysicsFloat;
   {$IFNDEF SUPPORT_POINTER_MATH}p: PVector2;{$ENDIF}
begin
   with dp do
   begin
      Result := 0;
      {$IFDEF SUPPORT_POINTER_MATH}
      bestValue := b2Dot(m_vertices[0], d);
      for i := 1 to m_count - 1 do
      begin
         value := b2Dot(m_vertices[i], d);
         if value > bestValue then
         begin
            Result := i;
            bestValue := value;
         end;
      end;
      {$ELSE}
      bestValue := b2Dot(m_vertices^, d);
      for i := 1 to m_count - 1 do
      begin
         p := m_vertices;
         Inc(p, i);
         value := b2Dot(p^, d);
         if value > bestValue then
         begin
            Result := i;
            bestValue := value;
         end;
      end;
      {$ENDIF}
   end;
end;

function GetSupportVertex(const dp: Tb2DistanceProxy; const d: TVector2): PVector2;
var
   i, bestIndex: Integer;
   bestValue, value: PhysicsFloat;
   {$IFNDEF SUPPORT_POINTER_MATH}p: PVector2;{$ENDIF}
begin
   with dp do
   begin
      bestIndex := 0;
      {$IFDEF SUPPORT_POINTER_MATH}
      bestValue := b2Dot(m_vertices[0], d);
      for i := 1 to m_count - 1 do
      begin
         value := b2Dot(m_vertices[i], d);
         if value > bestValue then
         begin
            bestIndex := i;
            bestValue := value;
         end;
      end;
      Result := @m_vertices[bestIndex];
      {$ELSE}
      bestValue := b2Dot(m_vertices^, d);
      for i := 1 to m_count - 1 do
      begin
         p := m_vertices;
         Inc(p, i);
         value := b2Dot(p^, d);
         if value > bestValue then
         begin
            bestIndex := i;
            bestValue := value;
         end;
      end;
      Result := m_vertices;
      Inc(Result, bestIndex);
      {$ENDIF}
   end;
end;
{$ENDIF}

{ b2TimeOfImpact.cpp }
type
   Tb2SeparationFunctionType = (e_separation_points, e_separation_faceA, e_separation_faceB);
   Tb2SeparationFunction = class
   public
      m_proxyA, m_proxyB: Pb2DistanceProxy;
      m_sweepA, m_sweepB: Tb2Sweep;
      m_type: Tb2SeparationFunctionType;
      m_localPoint,
      m_axis: TVector2;

      function Initialize(const cache: Tb2SimplexCache; const proxyA,
         proxyB: Tb2DistanceProxy; const sweepA, sweepB: Tb2Sweep; const t1: PhysicsFloat): PhysicsFloat;
      function FindMinSeparation(var indexA, indexB: Int32; t: PhysicsFloat): PhysicsFloat;
      function Evaluate(indexA, indexB: Int32; t: PhysicsFloat): PhysicsFloat;
   end;

{ Tb2SeparationFunction }

function Tb2SeparationFunction.Initialize(const cache: Tb2SimplexCache;
   const proxyA, proxyB: Tb2DistanceProxy; const sweepA, sweepB: Tb2Sweep;
   const t1: PhysicsFloat): PhysicsFloat;
var
   xfA, xfB: Tb2Transform;
   localPointB1, localPointB2, localPointA1, localPointA2, normal: TVector2;
   {$IFNDEF SUPPORT_POINTER_MATH}pA, pB: PVector2;{$ENDIF}
begin
   m_proxyA := @proxyA;
   m_proxyB := @proxyB;
   //b2Assert(0 < count && count < 3);

   m_sweepA := sweepA;
   m_sweepB := sweepB;

   {$IFDEF OP_OVERLOAD}
   m_sweepA.GetTransform(xfA, t1);
   m_sweepB.GetTransform(xfB, t1);
   {$ELSE}
   GetTransform(m_sweepA, xfA, t1);
   GetTransform(m_sweepB, xfB, t1);
   {$ENDIF}

   if cache.count = 1 then
   begin
      m_type := e_separation_points;
      {$IFNDEF SUPPORT_POINTER_MATH}
      pA := proxyA.m_vertices;
      pB := proxyB.m_vertices;
      Inc(pA, cache.indexA[0]);
      Inc(pB, cache.indexB[0]);
      {$ENDIF}

      {$IFDEF OP_OVERLOAD}
         {$IFDEF SUPPORT_POINTER_MATH}
         m_axis := b2Mul(xfB, proxyB.m_vertices[cache.indexB[0]]) -
            b2Mul(xfA, proxyA.m_vertices[cache.indexA[0]]);
         {$ELSE}
         m_axis := b2Mul(xfB, pB^) - b2Mul(xfA, pA^);
         {$ENDIF}
         Result := m_axis.Normalize;
      {$ELSE}
         {$IFDEF SUPPORT_POINTER_MATH}
         m_axis := Subtract(b2Mul(xfB, proxyB.m_vertices[cache.indexB[0]]),
            b2Mul(xfA, proxyA.m_vertices[cache.indexA[0]]));
         {$ELSE}
         m_axis := Subtract(b2Mul(xfB, pB^), b2Mul(xfA, pA^));
         {$ENDIF}
         Result := Normalize(m_axis);
      {$ENDIF}
   end
   else if cache.indexA[0] = cache.indexA[1] then
   begin
      // Two points on B and one on A.
      m_type := e_separation_faceB;
      {$IFDEF SUPPORT_POINTER_MATH}
      localPointB1 := proxyB.m_vertices[cache.indexB[0]];
      localPointB2 := proxyB.m_vertices[cache.indexB[1]];
      {$ELSE}
      pB := proxyB.m_vertices;
      Inc(pB, cache.indexB[0]);
      localPointB1 := pB^;

      pB := proxyB.m_vertices;
      Inc(pB, cache.indexB[1]);
      localPointB2 := pB^;
      {$ENDIF}

      {$IFDEF OP_OVERLOAD}
      m_axis := b2Cross(localPointB2 - localPointB1, 1.0);
      m_axis.Normalize;
      {$ELSE}
      m_axis := b2Cross(Subtract(localPointB2, localPointB1), 1.0);
      Normalize(m_axis);
      {$ENDIF}

      normal := b2Mul(xfB.q, m_axis);
      m_localPoint := b2MiddlePoint(localPointB1, localPointB2);

      {$IFNDEF SUPPORT_POINTER_MATH}
      pA := proxyA.m_vertices;
      Inc(pA, cache.indexA[0]);
      {$ENDIF}

      {$IFDEF OP_OVERLOAD}
         {$IFDEF SUPPORT_POINTER_MATH}
         Result := b2Dot(b2Mul(xfA, proxyA.m_vertices[cache.indexA[0]]) -
            b2Mul(xfB, m_localPoint), normal);
         {$ELSE}
         Result := b2Dot(b2Mul(xfA, pA^) - b2Mul(xfB, m_localPoint), normal);
         {$ENDIF}
      {$ELSE}
         {$IFDEF SUPPORT_POINTER_MATH}
         Result := b2Dot(Subtract(b2Mul(xfA, proxyA.m_vertices[cache.indexA[0]]),
            b2Mul(xfB, m_localPoint)), normal);
         {$ELSE}
         Result := b2Dot(Subtract(b2Mul(xfA, pA^), b2Mul(xfB, m_localPoint)), normal);
         {$ENDIF}
      {$ENDIF}

      if Result < 0.0 then
      begin
         {$IFDEF OP_OVERLOAD}
         m_axis := -m_axis;
         {$ELSE}
         m_axis := Negative(m_axis);
         {$ENDIF}
         Result := -Result;
      end;
   end
   else
   begin
      // Two points on A and one or two points on B.
      m_type := e_separation_faceA;

      {$IFDEF SUPPORT_POINTER_MATH}
      localPointA1 := proxyA.m_vertices[cache.indexA[0]];
      localPointA2 := proxyA.m_vertices[cache.indexA[1]];
      {$ELSE}
      pA := proxyA.m_vertices;
      Inc(pA, cache.indexA[0]);
      localPointA1 := pA^;

      pA := proxyA.m_vertices;
      Inc(pA, cache.indexA[1]);
      localPointA2 := pA^;
      {$ENDIF}

      {$IFDEF OP_OVERLOAD}
      m_axis := b2Cross(localPointA2 - localPointA1, 1.0);
      m_axis.Normalize;
      {$ELSE}
      m_axis := b2Cross(Subtract(localPointA2, localPointA1), 1.0);
      Normalize(m_axis);
      {$ENDIF}

      normal := b2Mul(xfA.q, m_axis);
      m_localPoint := b2MiddlePoint(localPointA1, localPointA2);

      {$IFNDEF SUPPORT_POINTER_MATH}
      pB := proxyB.m_vertices;
      Inc(pB, cache.indexB[0]);
      {$ENDIF}

      {$IFDEF OP_OVERLOAD}
         {$IFDEF SUPPORT_POINTER_MATH}
         Result := b2Dot(b2Mul(xfB, proxyB.m_vertices[cache.indexB[0]]) -
            b2Mul(xfA, m_localPoint), normal);
         {$ELSE}
         Result := b2Dot(b2Mul(xfB, pB^) - b2Mul(xfA, m_localPoint), normal);
         {$ENDIF}
      {$ELSE}
         {$IFDEF SUPPORT_POINTER_MATH}
         Result := b2Dot(Subtract(b2Mul(xfB, proxyB.m_vertices[cache.indexB[0]]),
            b2Mul(xfA, m_localPoint)), normal);
         {$ELSE}
         Result := b2Dot(Subtract(b2Mul(xfB, pB^), b2Mul(xfA, m_localPoint)), normal);
         {$ENDIF}
      {$ENDIF}
      if Result < 0.0 then
      begin
         {$IFDEF OP_OVERLOAD}
         m_axis := -m_axis;
         {$ELSE}
         m_axis := Negative(m_axis);
         {$ENDIF}
         Result := -Result;
      end;
   end;
end;

function Tb2SeparationFunction.FindMinSeparation(var indexA, indexB: Int32; t: PhysicsFloat): PhysicsFloat;
var
   xfA, xfB: Tb2Transform;
   normal: TVector2;
   {$IFNDEF SUPPORT_POINTER_MATH}pA, pB: PVector2;{$ENDIF}
begin
   {$IFDEF OP_OVERLOAD}
   m_sweepA.GetTransform(xfA, t);
   m_sweepB.GetTransform(xfB, t);
   {$ELSE}
   GetTransform(m_sweepA, xfA, t);
   GetTransform(m_sweepB, xfB, t);
   {$ENDIF}

   case m_type of
      e_separation_points:
         begin
            {$IFDEF OP_OVERLOAD}
            indexA := m_proxyA^.GetSupport(b2MulT(xfA.q,  m_axis));
            indexB := m_proxyB^.GetSupport(b2MulT(xfB.q, -m_axis));
            {$ELSE}
            indexA := GetSupport(m_proxyA^, b2MulT(xfA.q,  m_axis));
            indexB := GetSupport(m_proxyB^, b2MulT(xfB.q, Negative(m_axis)));
            {$ENDIF}

            {$IFNDEF SUPPORT_POINTER_MATH}
            pA := m_proxyA^.m_vertices;
            pB := m_proxyB^.m_vertices;
            Inc(pA, indexA);
            Inc(pB, indexB);
            {$ENDIF}

            {$IFDEF OP_OVERLOAD}
               {$IFDEF SUPPORT_POINTER_MATH}
               Result := b2Dot(b2Mul(xfB, m_proxyB^.m_vertices[indexB]) -
                  b2Mul(xfA, m_proxyA^.m_vertices[indexA]), m_axis);
               {$ELSE}
               Result := b2Dot(b2Mul(xfB, pB^) - b2Mul(xfA, pA^), m_axis);
               {$ENDIF}
            {$ELSE}
               {$IFDEF SUPPORT_POINTER_MATH}
               Result:= b2Dot(Subtract(b2Mul(xfB, m_proxyB^.m_vertices[indexB]),
                  b2Mul(xfA, m_proxyA^.m_vertices[indexA])), m_axis);
               {$ELSE}
               Result:= b2Dot(Subtract(b2Mul(xfB, pB^), b2Mul(xfA, pA^)), m_axis);
               {$ENDIF}
            {$ENDIF}
         end;
      e_separation_faceA:
         begin
            normal := b2Mul(xfA.q, m_axis);
            indexA := -1;
            {$IFDEF OP_OVERLOAD}
            indexB := m_proxyB^.GetSupport(b2MulT(xfB.q, -normal));
            {$ELSE}
            indexB := GetSupport(m_proxyB^, b2MulT(xfB.q, Negative(normal)));
            {$ENDIF}

            {$IFNDEF SUPPORT_POINTER_MATH}
            pB := m_proxyB^.m_vertices;
            Inc(pB, indexB);
            {$ENDIF}

            {$IFDEF OP_OVERLOAD}
               {$IFDEF SUPPORT_POINTER_MATH}
               Result := b2Dot(b2Mul(xfB, m_proxyB^.m_vertices[indexB]) -
                  b2Mul(xfA, m_localPoint), normal);
               {$ELSE}
               Result := b2Dot(b2Mul(xfB, pB^) - b2Mul(xfA, m_localPoint), normal);
               {$ENDIF}
            {$ELSE}
               {$IFDEF SUPPORT_POINTER_MATH}
               Result := b2Dot(Subtract(b2Mul(xfB, m_proxyB^.m_vertices[indexB]),
                  b2Mul(xfA, m_localPoint)), normal);
               {$ELSE}
               Result := b2Dot(Subtract(b2Mul(xfB, pB^), b2Mul(xfA, m_localPoint)), normal);
               {$ENDIF}
            {$ENDIF}
         end;
      e_separation_faceB:
         begin
            normal := b2Mul(xfB.q, m_axis);

            indexB := -1;
            {$IFDEF OP_OVERLOAD}
            indexA := m_proxyA.GetSupport(b2MulT(xfA.q, -normal));
            {$ELSE}
            indexA := GetSupport(m_proxyA^, b2MulT(xfA.q, Negative(normal)));
            {$ENDIF}

            {$IFNDEF SUPPORT_POINTER_MATH}
            pA := m_proxyA^.m_vertices;
            Inc(pA, indexA);
            {$ENDIF}

            {$IFDEF OP_OVERLOAD}
               {$IFDEF SUPPORT_POINTER_MATH}
               Result := b2Dot(b2Mul(xfA, m_proxyA.m_vertices[indexA]) -
                  b2Mul(xfB, m_localPoint), normal);
               {$ELSE}
               Result := b2Dot(b2Mul(xfA, pA^) - b2Mul(xfB, m_localPoint), normal);
               {$ENDIF}
            {$ELSE}
               {$IFDEF SUPPORT_POINTER_MATH}
               Result := b2Dot(Subtract(b2Mul(xfA, m_proxyA.m_vertices[indexA]),
                  b2Mul(xfB, m_localPoint)), normal);
               {$ELSE}
               Result := b2Dot(Subtract(b2Mul(xfA, pA^), b2Mul(xfB, m_localPoint)), normal);
               {$ENDIF}
            {$ENDIF}
         end;
   else
      //b2Assert(false);
      indexA := -1;
      indexB := -1;
      Result := 0.0;
   end;
end;

function Tb2SeparationFunction.Evaluate(indexA, indexB: Int32; t: PhysicsFloat): PhysicsFloat;
var
   xfA, xfB: Tb2Transform;
   {$IFNDEF SUPPORT_POINTER_MATH}pA, pB: PVector2;{$ENDIF}
begin
   {$IFDEF OP_OVERLOAD}
   m_sweepA.GetTransform(xfA, t);
   m_sweepB.GetTransform(xfB, t);
   {$ELSE}
   GetTransform(m_sweepA, xfA, t);
   GetTransform(m_sweepB, xfB, t);
   {$ENDIF}

   case m_type of
      e_separation_points:
         begin
            {$IFNDEF SUPPORT_POINTER_MATH}
            pA := m_proxyA^.m_vertices;
            pB := m_proxyB^.m_vertices;
            Inc(pA, indexA);
            Inc(pB, indexB);
            {$ENDIF}

            {$IFDEF OP_OVERLOAD}
               {$IFDEF SUPPORT_POINTER_MATH}
               Result := b2Dot(b2Mul(xfB, m_proxyB^.m_vertices[indexB]) -
                  b2Mul(xfA, m_proxyA^.m_vertices[indexA]), m_axis);
               {$ELSE}
               Result := b2Dot(b2Mul(xfB, pB^) - b2Mul(xfA, pA^), m_axis);
               {$ENDIF}
            {$ELSE}
               {$IFDEF SUPPORT_POINTER_MATH}
               Result := b2Dot(Subtract(b2Mul(xfB, m_proxyB^.m_vertices[indexB]),
                  b2Mul(xfA, m_proxyA.m_vertices[indexA])), m_axis);
               {$ELSE}
               Result := b2Dot(Subtract(b2Mul(xfB, pB^), b2Mul(xfA, pA^)), m_axis);
               {$ENDIF}
            {$ENDIF}
         end;
      e_separation_faceA:
         begin
            {$IFNDEF SUPPORT_POINTER_MATH}
            pB := m_proxyB^.m_vertices;
            Inc(pB, indexB);
            {$ENDIF}

            {$IFDEF OP_OVERLOAD}
               {$IFDEF SUPPORT_POINTER_MATH}
               Result := b2Dot(b2Mul(xfB, m_proxyB^.m_vertices[indexB]) -
                  b2Mul(xfA, m_localPoint), b2Mul(xfA.q, m_axis));
               {$ELSE}
               Result := b2Dot(b2Mul(xfB, pB^) - b2Mul(xfA, m_localPoint), b2Mul(xfA.q, m_axis));
               {$ENDIF}
            {$ELSE}
               {$IFDEF SUPPORT_POINTER_MATH}
               Result := b2Dot(Subtract(b2Mul(xfB, m_proxyB^.m_vertices[indexB]),
                  b2Mul(xfA, m_localPoint)), b2Mul(xfA.q, m_axis));
               {$ELSE}
               Result := b2Dot(Subtract(b2Mul(xfB, pB^),
                  b2Mul(xfA, m_localPoint)), b2Mul(xfA.q, m_axis));
               {$ENDIF}
            {$ENDIF}
         end;
      e_separation_faceB:
         begin
            {$IFNDEF SUPPORT_POINTER_MATH}
            pA := m_proxyA^.m_vertices;
            Inc(pA, indexA);
            {$ENDIF}

            {$IFDEF OP_OVERLOAD}
               {$IFDEF SUPPORT_POINTER_MATH}
               Result := b2Dot(b2Mul(xfA, m_proxyA^.m_vertices[indexA]) -
                  b2Mul(xfB, m_localPoint), b2Mul(xfB.q, m_axis));
               {$ELSE}
               Result := b2Dot(b2Mul(xfA, pA^) - b2Mul(xfB, m_localPoint), b2Mul(xfB.q, m_axis));
               {$ENDIF}
            {$ELSE}
               {$IFDEF SUPPORT_POINTER_MATH}
               Result := b2Dot(Subtract(b2Mul(xfA, m_proxyA^.m_vertices[indexA]),
                  b2Mul(xfB, m_localPoint)), b2Mul(xfB.q, m_axis));
               {$ELSE}
               Result := b2Dot(Subtract(b2Mul(xfA, pA^),
                  b2Mul(xfB, m_localPoint)), b2Mul(xfB.q, m_axis));
               {$ENDIF}
            {$ENDIF}
         end;
   else
      //b2Assert(false);
      Result := 0.0;
   end;
end;

var
   toi_separation_fcn: Tb2SeparationFunction;
/// Compute the upper bound on time before two shapes penetrate. Time is represented as
/// a fraction between [0,tMax]. This uses a swept separating axis and may miss some intermediate,
/// non-tunneling collision. If you change the time interval, you should call this function again.
/// Note: use b2Distance to compute the contact point and normal at the time of impact.
function b2TimeOfImpact(var output: Tb2TOIOutput; const input: Tb2TOIInput): PhysicsFloat;
const
   c_tolerance = 0.25 * b2_linearSlop;
   k_maxIterations = 20;	// TODO_ERIN b2Settings
var
   sweepA, sweepB: Tb2Sweep;
   t1, s1, t2, s2, a1, a2, t, s, totalRadius, target, tolerance: PhysicsFloat;
   iter, pushBackIter: Int32;
   cache: Tb2SimplexCache;
   distanceInput: Tb2DistanceInput;
   distanceOutput: Tb2DistanceOutput;
   xfA, xfB: Tb2Transform;
   done: Boolean;
   indexA, indexB, rootIterCount: Int32;
   {$IFDEF COMPUTE_PHYSICS_TIME}
   time0: Double;
	 {$ENDIF}
begin
   {$IFDEF COMPUTE_PHYSICS_TIME}
   time0 := GetRawReferenceTime();
	 {$ENDIF}
   Inc(b2_toiCalls);

   output.state := e_toi_unknown;
   output.t := input.tMax;

   sweepA := input.sweepA;
   sweepB := input.sweepB;

   // Large rotations can make the root finder fail, so we normalize the sweep angles.
   {$IFDEF OP_OVERLOAD}
   sweepA.Normalize;
   sweepB.Normalize;
   {$ELSE}
   Normalize(sweepA);
   Normalize(sweepB);
   {$ENDIF}

   totalRadius := input.proxyA.m_radius + input.proxyB.m_radius;
   target := b2Max(b2_linearSlop, totalRadius - 3.0 * b2_linearSlop);
   tolerance := 0.25 * b2_linearSlop;
   //b2Assert(target > tolerance);

   t1 := 0.0;
   iter := 0;

   // Prepare input for distance query.
   cache.count := 0;
   distanceInput.proxyA := input.proxyA;
   distanceInput.proxyB := input.proxyB;
   distanceInput.useRadii := False;

   // The outer loop progressively attempts to compute new separating axes.
   // This loop terminates when an axis is repeated (no progress is made).
   while True do
   begin
      {$IFDEF OP_OVERLOAD}
      sweepA.GetTransform(xfA, t1);
      sweepB.GetTransform(xfB, t1);
      {$ELSE}
      GetTransform(sweepA, xfA, t1);
      GetTransform(sweepB, xfB, t1);
      {$ENDIF}

      // Get the distance between shapes. We can also use the results
      // to get a separating axis.
      distanceInput.transformA := xfA;
      distanceInput.transformB := xfB;
      b2Distance(distanceOutput, cache, distanceInput);

      // If the shapes are overlapped, we give up on continuous collision.
      if distanceOutput.distance <= 0.0 then
      begin
         // Failure!
         output.state := e_toi_overlapped;
         output.t := 0.0;
         Break;
      end;

      if distanceOutput.distance < target + tolerance then
      begin
         // Victory!
         output.state := e_toi_touching;
         output.t := t1;
         Break;
      end;

      // Initialize the separating axis.
      toi_separation_fcn.Initialize(cache, input.proxyA, input.proxyB, sweepA, sweepB, t1);

      // Compute the TOI on the separating axis. We do this by successively
      // resolving the deepest point. This loop is bounded by the number of vertices.
      done := False;
      t2 := input.tMax;
      pushBackIter := 0;
      while True do
      begin
         // Find the deepest point at t2. Store the witness point indices.
         s2 := toi_separation_fcn.FindMinSeparation(indexA, indexB, t2);

         // Is the final configuration separated?
         if s2 > target + tolerance then
         begin
            // Victory!
            output.state := e_toi_separated;
            output.t := input.tMax;
            done := True;
            Break;
         end;

         // Has the separation reached tolerance?
         if s2 > target - tolerance then
         begin
            // Advance the sweeps
            t1 := t2;
            Break;
         end;

         // Compute the initial separation of the witness points.
         s1 := toi_separation_fcn.Evaluate(indexA, indexB, t1);

         // Check for initial overlap. This might happen if the root finder
         // runs out of iterations.
         if s1 < target - tolerance then
         begin
            output.state := e_toi_failed;
            output.t := t1;
            done := True;
            Break;
         end;

         // Check for touching
         if s1 <= target + tolerance then
         begin
            // Victory! t1 should hold the TOI (could be 0.0).
            output.state := e_toi_touching;
            output.t := t1;
            done := True;
            Break;
         end;

         // Compute 1D root of: f(x) - target := 0
         rootIterCount := 0;
         a1 := t1;
         a2 := t2;
         while True do
         begin
            // Use a mix of the secant rule and bisection.
            if rootIterCount and 1 <> 0 then
            begin
               // Secant rule to improve convergence.
               t := a1 + (target - s1) * (a2 - a1) / (s2 - s1);
            end
            else
            begin
               // Bisection to guarantee progress.
               t := 0.5 * (a1 + a2);
            end;

            Inc(rootIterCount);
            Inc(b2_toiRootIters);

            s := toi_separation_fcn.Evaluate(indexA, indexB, t);

            if Abs(s - target) < tolerance then
            begin
               // t2 holds a tentative value for t1
               t2 := t;
               Break;
            end;

            // Ensure we continue to bracket the root.
            if s > target then
            begin
               a1 := t;
               s1 := s;
            end
            else
            begin
               a2 := t;
               s2 := s;
            end;

            if rootIterCount = 50 then
               Break;
         end;

         b2_toiMaxRootIters := b2Max(b2_toiMaxRootIters, rootIterCount);
         Inc(pushBackIter);

         if pushBackIter = b2_maxPolygonVertices then
            Break;
      end;

      Inc(iter);
      Inc(b2_toiIters);

      if done then
         Break;

      if iter = k_maxIterations then
      begin
         // Root finder got stuck. Semi-victory.
         output.state := e_toi_failed;
         output.t := t1;
         Break;
      end;
   end;

   b2_toiMaxIters := b2Max(b2_toiMaxIters, iter);
   {$IFDEF COMPUTE_PHYSICS_TIME}
	 time0 := GetRawReferenceTime() - time0;
	 b2_toiMaxTime := Max(b2_toiMaxTime, time0);
	 b2_toiTime := b2_toiTime + time0;
	 {$ENDIF}
end;

{ b2Distance.cpp }

type
   Pb2SimplexVertex = ^Tb2SimplexVertex;
   Tb2SimplexVertex = record
      wA: TVector2;		// support point in proxyA
      wB: TVector2;		// support point in proxyB
      w: TVector2;		// wB - wA
      a: PhysicsFloat;		// barycentric coordinate for closest point
      indexA: Int32;	// wA index
      indexB: Int32;	// wB index
   end;

   Tb2Simplex = class
   public
      m_v1, m_v2, m_v3: Tb2SimplexVertex;
      m_count: Int32;

	    procedure ReadCache(const cache: Tb2SimplexCache; const proxyA,
         proxyB: Tb2DistanceProxy; const transformA, transformB: Tb2Transform);
      procedure WriteCache(var cache: Tb2SimplexCache);

      function GetSearchDirection: TVector2;
      function GetClosestPoint: TVector2;
      procedure GetWitnessPoints(var pA, pB: TVector2);
      function GetMetric: PhysicsFloat;

      procedure Solve2;
	    procedure Solve3;
   end;

{ Tb2Simplex }

procedure Tb2Simplex.ReadCache(const cache: Tb2SimplexCache; const proxyA,
   proxyB: Tb2DistanceProxy; const transformA, transformB: Tb2Transform);
var
   vertices, v: Pb2SimplexVertex;
   i: Integer;
   metric1, metric2: PhysicsFloat;
   {$IFNDEF SUPPORT_POINTER_MATH}pA, pB: PVector2;{$ENDIF}
begin
   //b2Assert(cache.count <= 3);

   // Copy data from cache.
   m_count := cache.count;
   vertices := @m_v1;
   for i := 0 to m_count - 1 do
   begin
      v := vertices;
      Inc(v, i);

      with v^ do
      begin
         indexA := cache.indexA[i];
         indexB := cache.indexB[i];

         {$IFDEF SUPPORT_POINTER_MATH}
         wA := b2Mul(transformA, proxyA.m_vertices[indexA]);
         wB := b2Mul(transformB, proxyB.m_vertices[indexB]);
         {$ELSE}
         pA := proxyA.m_vertices;
         pB := proxyB.m_vertices;
         Inc(pA, indexA);
         Inc(pB, indexB);
         wA := b2Mul(transformA, pA^);
         wB := b2Mul(transformB, pB^);
         {$ENDIF}

         {$IFDEF OP_OVERLOAD}
         w := wB - wA;
         {$ELSE}
         w := Subtract(wB, wA);
         {$ENDIF}
         a := 0.0;
      end;
   end;

   // Compute the new simplex metric, if it is substantially different than
   // old metric then flush the simplex.
   if m_count > 1 then
   begin
      metric1 := cache.metric;
      metric2 := GetMetric;
      if (metric2 < 0.5 * metric1) or (2.0 * metric1 < metric2) or (metric2 < FLT_epsilon) then
      begin
         // Reset the simplex.
         m_count := 0;
      end;
   end;

   // If the cache is empty or invalid ...
   if m_count = 0 then
      with m_v1 do
      begin
         indexA := 0;
         indexB := 0;
         {$IFDEF SUPPORT_POINTER_MATH}
         wA := b2Mul(transformA, proxyA.m_vertices[0]);
         wB := b2Mul(transformB, proxyB.m_vertices[0]);
         {$ELSE}
         wA := b2Mul(transformA, proxyA.m_vertices^);
         wB := b2Mul(transformB, proxyB.m_vertices^);
         {$ENDIF}

         {$IFDEF OP_OVERLOAD}
         w := wB - wA;
         {$ELSE}
         w := Subtract(wB, wA);
         {$ENDIF}
         a := 1.0;
         m_count := 1;
      end;
end;

procedure Tb2Simplex.WriteCache(var cache: Tb2SimplexCache);
var
   i: Integer;
   vertices, v: Pb2SimplexVertex;
begin
   cache.metric := GetMetric;
   cache.count := m_count;
   vertices := @m_v1;
   for i := 0 to m_count - 1 do
   begin
      v := vertices;
      Inc(v, i);
      cache.indexA[i] := v^.indexA;
      cache.indexB[i] := v^.indexB;
   end;
end;

function Tb2Simplex.GetSearchDirection: TVector2;
var
   e12: TVector2;
begin
   case m_count of
      1:
         {$IFDEF OP_OVERLOAD}
         Result := -m_v1.w;
         {$ELSE}
         Result := Negative(m_v1.w);
         {$ENDIF}
      2:
         begin
            {$IFDEF OP_OVERLOAD}
            e12 := m_v2.w - m_v1.w;
            {$ELSE}
            e12 := Subtract(m_v2.w, m_v1.w);
            {$ENDIF}
            if b2Cross(e12, {$IFDEF OP_OVERLOAD}-m_v1.w{$ELSE}Negative(m_v1.w){$ENDIF}) > 0.0 then
            begin
               // Origin is left of e12.
               Result := b2Cross(1.0, e12);
            end
            else
            begin
              // Origin is right of e12.
              Result := b2Cross(e12, 1.0);
            end;
         end;
   else
      //b2Assert(false);
      Result := b2Vec2_zero;
   end;
end;

function Tb2Simplex.GetClosestPoint: TVector2;
begin
   case m_count of
      1: Result := m_v1.w;
      2: Result :=
            {$IFDEF OP_OVERLOAD}
            m_v1.a * m_v1.w + m_v2.a * m_v2.w;
            {$ELSE}
            Add(Multiply(m_v1.w, m_v1.a), Multiply(m_v2.w, m_v2.a));
            {$ENDIF}
      3: Result := b2Vec2_zero;
   else
      //b2Assert(false);
      Result := b2Vec2_zero;
   end;
end;

procedure Tb2Simplex.GetWitnessPoints(var pA, pB: TVector2);
begin
   case m_count of
      1:
         begin
            pA := m_v1.wA;
            pB := m_v1.wB;
         end;
      2:
         begin
            {$IFDEF OP_OVERLOAD}
            pA := m_v1.a * m_v1.wA + m_v2.a * m_v2.wA;
            pB := m_v1.a * m_v1.wB + m_v2.a * m_v2.wB;
            {$ELSE}
            pA := Add(Multiply(m_v1.wA, m_v1.a), Multiply(m_v2.wA, m_v2.a));
            pB := Add(Multiply(m_v1.wB, m_v1.a), Multiply(m_v2.wB, m_v2.a));
            {$ENDIF}
         end;
      3:
         begin
            {$IFDEF OP_OVERLOAD}
            pA := m_v1.a * m_v1.wA + m_v2.a * m_v2.wA + m_v3.a * m_v3.wA;
            {$ELSE}
            pA := Add(Multiply(m_v1.wA, m_v1.a), Multiply(m_v2.wA, m_v2.a),
               Multiply(m_v3.wA, m_v3.a));
            {$ENDIF}
            pB := pA;
         end;
   else
      //b2Assert(false);
   end;
end;

function Tb2Simplex.GetMetric: PhysicsFloat;
begin
   case m_count of
      0:
         begin
            //b2Assert(false);
            Result := 0.0;
         end;
      1: Result := 0.0;
      2: Result := UPhysics2DTypes.b2Distance(m_v1.w, m_v2.w);
      3: Result :=
         {$IFDEF OP_OVERLOAD}
         b2Cross(m_v2.w - m_v1.w, m_v3.w - m_v1.w)
         {$ELSE}
         b2Cross(Subtract(m_v2.w, m_v1.w), Subtract(m_v3.w, m_v1.w))
         {$ENDIF}
   else
      //b2Assert(false);
      Result := 0.0;
   end;
end;

// Solve a line segment using barycentric coordinates.
//
// p := a1 * w1 + a2 * w2
// a1 + a2 := 1
//
// The vector from the origin to the closest point on the line is
// perpendicular to the line.
// e12 := w2 - w1
// dot(p, e) := 0
// a1 * dot(w1, e) + a2 * dot(w2, e) := 0
//
// 2-by-2 linear system
// [1      1     ][a1] := [1]
// [w1.e12 w2.e12][a2] := [0]
//
// Define
// d12_1 :=  dot(w2, e12)
// d12_2 := -dot(w1, e12)
// d12 := d12_1 + d12_2
//
// Solution
// a1 := d12_1 / d12
// a2 := d12_2 / d12
procedure Tb2Simplex.Solve2;
var
   e12: TVector2;
   d12_2, d12_1, inv_d12: PhysicsFloat;
begin
   {$IFDEF OP_OVERLOAD}
   e12 := m_v2.w - m_v1.w;
   {$ELSE}
   e12 := Subtract(m_v2.w, m_v1.w);
   {$ENDIF}

   // w1 region
   d12_2 := -b2Dot(m_v1.w, e12);
   if d12_2 <= 0.0 then
   begin
      // a2 <= 0, so we clamp it to 0
      m_v1.a := 1.0;
      m_count := 1;
      Exit;
   end;

   // w2 region
   d12_1 := b2Dot(m_v2.w, e12);
   if d12_1 <= 0.0 then
   begin
      // a1 <= 0, so we clamp it to 0
      m_v2.a := 1.0;
      m_count := 1;
      m_v1 := m_v2;
      Exit;
   end;

   // Must be in e12 region.
   inv_d12 := 1.0 / (d12_1 + d12_2);
   m_v1.a := d12_1 * inv_d12;
   m_v2.a := d12_2 * inv_d12;
   m_count := 2;
end;

// Possible regions:
// - points[2]
// - edge points[0]-points[2]
// - edge points[1]-points[2]
// - inside the triangle
procedure Tb2Simplex.Solve3;
var
   w1, w2, w3: TVector2;
   e12, e13, e23: TVector2;
   d12_1, d12_2, d13_1, d13_2, d23_1, d23_2,
   n123, d123_1, d123_2, d123_3,
   inv_d: PhysicsFloat;
begin
   w1 := m_v1.w;
   w2 := m_v2.w;
   w3 := m_v3.w;

   // Edge12
   // [1      1     ][a1] := [1]
   // [w1.e12 w2.e12][a2] := [0]
   // a3 := 0
   {$IFDEF OP_OVERLOAD}
   e12 := w2 - w1;
   {$ELSE}
   e12 := Subtract(w2, w1);
   {$ENDIF}
   d12_1 := b2Dot(w2, e12);
   d12_2 := -b2Dot(w1, e12);

   // Edge13
   // [1      1     ][a1] := [1]
   // [w1.e13 w3.e13][a3] := [0]
   // a2 := 0
   {$IFDEF OP_OVERLOAD}
   e13 := w3 - w1;
   {$ELSE}
   e13 := Subtract(w3, w1);
   {$ENDIF}
   d13_1 := b2Dot(w3, e13);
   d13_2 := -b2Dot(w1, e13);

   // Edge23
   // [1      1     ][a2] := [1]
   // [w2.e23 w3.e23][a3] := [0]
   // a1 := 0
   {$IFDEF OP_OVERLOAD}
   e23 := w3 - w2;
   {$ELSE}
   e23 := Subtract(w3, w2);
   {$ENDIF}
   d23_1 := b2Dot(w3, e23);
   d23_2 := -b2Dot(w2, e23);

   // Triangle123
   n123 := b2Cross(e12, e13);

   d123_1 := n123 * b2Cross(w2, w3);
   d123_2 := n123 * b2Cross(w3, w1);
   d123_3 := n123 * b2Cross(w1, w2);

   // w1 region
   if (d12_2 <= 0.0) and (d13_2 <= 0.0) then
   begin
      m_v1.a := 1.0;
      m_count := 1;
      Exit;
   end;

   // e12
   if (d12_1 > 0.0) and (d12_2 > 0.0) and (d123_3 <= 0.0) then
   begin
      inv_d := 1.0 / (d12_1 + d12_2);
      m_v1.a := d12_1 * inv_d;
      m_v2.a := d12_2 * inv_d;
      m_count := 2;
      Exit;
   end;

   // e13
   if (d13_1 > 0.0) and (d13_2 > 0.0) and (d123_2 <= 0.0) then
   begin
      inv_d := 1.0 / (d13_1 + d13_2);
      m_v1.a := d13_1 * inv_d;
      m_v3.a := d13_2 * inv_d;
      m_count := 2;
      m_v2 := m_v3;
      Exit;
   end;

   // w2 region
   if (d12_1 <= 0.0) and (d23_2 <= 0.0) then
   begin
      m_v2.a := 1.0;
      m_count := 1;
      m_v1 := m_v2;
      Exit;
   end;

   // w3 region
   if (d13_1 <= 0.0) and (d23_1 <= 0.0) then
   begin
      m_v3.a := 1.0;
      m_count := 1;
      m_v1 := m_v3;
      Exit;
   end;

   // e23
   if (d23_1 > 0.0) and (d23_2 > 0.0) and (d123_1 <= 0.0) then
   begin
      inv_d := 1.0 / (d23_1 + d23_2);
      m_v2.a := d23_1 * inv_d;
      m_v3.a := d23_2 * inv_d;
      m_count := 2;
      m_v1 := m_v3;
      Exit;
   end;

   // Must be in triangle123
   inv_d := 1.0 / (d123_1 + d123_2 + d123_3);
   m_v1.a := d123_1 * inv_d;
   m_v2.a := d123_2 * inv_d;
   m_v3.a := d123_3 * inv_d;
   m_count := 3;
end;

var
   distance_simplex: Tb2Simplex;
procedure b2Distance(var output: Tb2DistanceOutput;	var cache: Tb2SimplexCache;
   const input: Tb2DistanceInput);
const
   k_maxIters = 20;
var
   i: Integer;
   saveA, saveB: array[0..2] of Int32;
   saveCount, iter: Int32;
   vertices, vertex: Pb2SimplexVertex;
   distanceSqr1, distanceSqr2, rA, rB: PhysicsFloat;
   d, normal: TVector2;
   duplicate: Boolean;
   {$IFNDEF SUPPORT_POINTER_MATH}pA, pB: PVector2;{$ENDIF}
begin
   Inc(b2_gjkCalls);

   // Initialize the simplex.
   distance_simplex.ReadCache(cache, input.proxyA, input.proxyB,
      input.transformA, input.transformB);

   // Get simplex vertices as an array.
   vertices := @distance_simplex.m_v1;

   // These store the vertices of the last simplex so that we
   // can check for duplicates and prevent cycling.
   //saveCount := 0;

   distanceSqr1 := FLT_MAX;
	 distanceSqr2 := distanceSqr1;

   // Main iteration loop.
   iter := 0;
   while iter < k_maxIters do
   begin
      // Copy simplex so we can identify duplicates.
      saveCount := distance_simplex.m_count;
      for i := 0 to saveCount - 1 do
      begin
         vertex := vertices;
         Inc(vertex, i);
         with vertex^ do
         begin
            saveA[i] := indexA;
            saveB[i] := indexB;
         end;
      end;

      case distance_simplex.m_count of
         2: distance_simplex.Solve2;
         3: distance_simplex.Solve3;
      else
         //b2Assert(false);
      end;

      // If we have 3 points, then the origin is in the corresponding triangle.
      if distance_simplex.m_count = 3 then
         Break;

      // Compute closest point.
      {$IFDEF OP_OVERLOAD}
      distanceSqr2 := distance_simplex.GetClosestPoint.SqrLength;
      {$ELSE}
      distanceSqr2 := SqrLength(distance_simplex.GetClosestPoint);
      {$ENDIF}

      // Ensure progress
      if distanceSqr2 >= distanceSqr1 then
      begin
        //break;
      end;
      distanceSqr1 := distanceSqr2;

      // Get search direction.
      d := distance_simplex.GetSearchDirection;

      // Ensure the search direction is numerically fit.
      if {$IFDEF OP_OVERLOAD}d.SqrLength{$ELSE}SqrLength(d){$ENDIF} <
         FLT_EPSILON * FLT_EPSILON then
      begin
         // The origin is probably contained by a line segment
         // or triangle. Thus the shapes are overlapped.

         // We can't return zero here even though there may be overlap.
         // In case the simplex is a point, segment, or triangle it is difficult
         // to determine if the origin is contained in the CSO or very close to it.
         Break;
      end;

      // Compute a tentative new simplex vertex using support points.
      vertex := vertices;
      Inc(vertex, distance_simplex.m_count);
      with vertex^ do
      begin
         {$IFDEF OP_OVERLOAD}
         indexA := input.proxyA.GetSupport(b2MulT(input.transformA.q, -d));
         indexB := input.proxyB.GetSupport(b2MulT(input.transformB.q, d));
         {$ELSE}
         indexA := GetSupport(input.proxyA, b2MulT(input.transformA.q, Negative(d)));
         indexB := GetSupport(input.proxyB, b2MulT(input.transformB.q, d));
         {$ENDIF}

         {$IFDEF SUPPORT_POINTER_MATH}
         wA := b2Mul(input.transformA, input.proxyA.m_vertices[indexA]);
         wB := b2Mul(input.transformB, input.proxyB.m_vertices[indexB]);
         {$ELSE}
         pA := input.proxyA.m_vertices;
         pB := input.proxyB.m_vertices;
         Inc(pA, indexA);
         Inc(pB, indexB);
         wA := b2Mul(input.transformA, pA^);
         wB := b2Mul(input.transformB, pB^);
         {$ENDIF}

         {$IFDEF OP_OVERLOAD}
         w := wB - wA;
         {$ELSE}
         w := Subtract(wB, wA);
         {$ENDIF}
      end;

      // Iteration count is equated to the number of support point calls.
      Inc(iter);
      Inc(b2_gjkIters);

      // Check for duplicate support points. This is the main termination criteria.
      duplicate := False;
      for i := 0 to saveCount - 1 do
      begin
         if (vertex.indexA = saveA[i]) and (vertex.indexB = saveB[i]) then
         begin
            duplicate := True;
            Break;
         end;
      end;

      // If we found a duplicate support point we must exit to avoid cycling.
      if duplicate then
         Break;

      // New vertex is ok and needed.
      Inc(distance_simplex.m_count);
   end;

   b2_gjkMaxIters := b2Max(b2_gjkMaxIters, iter);

   // Prepare output.
   with output do
   begin
      distance_simplex.GetWitnessPoints(pointA, pointB);
      distance := UPhysics2DTypes.b2Distance(pointA, pointB);
      iterations := iter;
   end;

   // Cache the simplex.
   distance_simplex.WriteCache(cache);

   // Apply radii if requested.
   if input.useRadii then
   begin
      rA := input.proxyA.m_radius;
      rB := input.proxyB.m_radius;

      with output do
      begin
         if (distance > rA + rB) and (distance > FLT_EPSILON) then
         begin
            // Shapes are still no overlapped.
            // Move the witness points to the outer surface.
            distance := distance - (rA + rB);
            {$IFDEF OP_OVERLOAD}
            normal := pointB - pointA;
            normal.Normalize;
            pointA.AddBy(rA * normal);
            pointB.SubtractBy(rB * normal);
            {$ELSE}
            normal := Subtract(pointB, pointA);
            Normalize(normal);
            AddBy(pointA, Multiply(normal, rA));
            SubtractBy(pointB, Multiply(normal, rB));
            {$ENDIF}
         end
         else
         begin
            // Shapes are overlapped when radii are considered.
            // Move the witness points to the middle.
            pointA := b2MiddlePoint(pointA, pointB);
            pointB := pointA;
            distance := 0.0;
         end;
      end;
   end;
end;

{ b2Collision.cpp }
function b2TestOverlap(const a, b: Tb2AABB): Boolean; overload;
var
   d1, d2: TVector2;
begin
   {$IFDEF OP_OVERLOAD}
   d1 := b.lowerBound - a.upperBound;
   d2 := a.lowerBound - b.upperBound;
   {$ELSE}
   d1 := Subtract(b.lowerBound, a.upperBound);
   d2 := Subtract(a.lowerBound, b.upperBound);
   {$ENDIF}

   if (d1.x > 0.0) or (d1.y > 0.0) or (d2.x > 0.0) or (d2.y > 0.0) then
      Result := False
   else
      Result := True;
end;

/// Determine if two generic shapes overlap.
function b2TestOverlap(shapeA, shapeB: Tb2Shape; indexA, indexB: Int32;
   const xfA, xfB: Tb2Transform): Boolean; overload;
var
   input: Tb2DistanceInput;
   output: Tb2DistanceOutput;
   cache: Tb2SimplexCache;
begin
   with input do
   begin
      {$IFDEF OP_OVERLOAD}
      proxyA.SetShape(shapeA, indexA);
      proxyB.SetShape(shapeB, indexB);
      {$ELSE}
      SetShape(proxyA, shapeA, indexA);
      SetShape(proxyB, shapeB, indexB);
      {$ENDIF}
      transformA := xfA;
      transformB := xfB;
      useRadii := True;
   end;

   cache.count := 0;
   b2Distance(output, cache, input);
   Result := output.distance < 10.0 * FLT_EPSILON;
end;

/////////////////////////////////////////////////////////////////////////////
{$IFDEF OP_OVERLOAD}
procedure Tb2WorldManifold.Initialize(const manifold: Tb2Manifold;
   const xfA, xfB: Tb2Transform; radiusA, radiusB: PhysicsFloat);
var
   i: Integer;
   pointA, pointB, cA, cB, planePoint, clipPoint: TVector2;
begin
   with manifold do
   begin
      if pointCount = 0 then
         Exit;

      case manifoldType of
         e_manifold_circles:
            begin
               normal.x := 1.0;
               normal.y := 0.0;

               pointA := b2Mul(xfA, localPoint);
               pointB := b2Mul(xfB, points[0].localPoint{manifold.points});
               if b2DistanceSquared(pointA, pointB) > FLT_EPSILON * FLT_EPSILON then
               begin
                  normal := pointB - pointA;
                  normal.Normalize;
               end;

               cA := pointA + radiusA * normal;
               cB := pointB - radiusB * normal;
               Self.points[0] := 0.5 * (cA + cB);
               Self.separations[0] := b2Dot(cB - cA, normal);
            end;
         e_manifold_faceA:
            begin
               normal := b2Mul(xfA.q, localNormal);
               planePoint := b2Mul(xfA, localPoint);

               for i := 0 to pointCount - 1 do
               begin
                  clipPoint := b2Mul(xfB, points[i].localPoint{manifold.points});
                  cA := clipPoint + (radiusA - b2Dot(clipPoint - planePoint, normal)) * normal;
                  cB := clipPoint - radiusB * normal;
                  Self.points[i] := 0.5 * (cA + cB);
                  Self.separations[i] := b2Dot(cB - cA, normal);
               end;
            end;
         e_manifold_faceB:
            begin
               normal := b2Mul(xfB.q, localNormal);
               planePoint := b2Mul(xfB, localPoint);

               for i := 0 to pointCount - 1 do
               begin
                  clipPoint := b2Mul(xfA, points[i].localPoint{manifold.points});
                  cB := clipPoint + (radiusB - b2Dot(clipPoint - planePoint, normal)) * normal;
                  cA := clipPoint - radiusA * normal;
                  Self.points[i] := 0.5 * (cA + cB);
                  Self.separations[i] := b2Dot(cA - cB, normal);
               end;

               // Ensure normal points from A to B.
               normal := -normal;
            end;
      end;
   end;
end;
{$ENDIF}

{ Tb2AABB }
{$IFDEF OP_OVERLOAD}
function Tb2AABB.IsValid: Boolean;
var
   d: TVector2;
begin
   d := upperBound - lowerBound;
   Result := (d.x >= 0.0) and (d.y >= 0.0) and upperBound.IsValid and lowerBound.IsValid;
end;

function Tb2AABB.GetCenter: TVector2;
begin
   Result := b2MiddlePoint(lowerBound, upperBound);
end;

function Tb2AABB.GetExtents: TVector2;
begin
   Result := (upperBound - lowerBound) * 0.5;
end;

function Tb2AABB.GetPerimeter: PhysicsFloat;
begin
   Result := 2.0 * ((upperBound.x - lowerBound.x) + (upperBound.y - lowerBound.y));
end;

procedure Tb2AABB.Combine(const aabb: Tb2AABB);
begin
	 lowerBound := b2Min(lowerBound, aabb.lowerBound);
	 upperBound := b2Max(upperBound, aabb.upperBound);
end;

procedure Tb2AABB.Combine(const aabb1, aabb2: Tb2AABB);
begin
   lowerBound := b2Min(aabb1.lowerBound, aabb2.lowerBound);
   upperBound := b2Max(aabb1.upperBound, aabb2.upperBound);
end;

function Tb2AABB.Contains(const aabb: Tb2AABB): Boolean;
begin
   Result := True;
   Result := Result and (lowerBound.x <= aabb.lowerBound.x);
   Result := Result and (lowerBound.y <= aabb.lowerBound.y);
   Result := Result and (aabb.upperBound.x <= upperBound.x);
   Result := Result and (aabb.upperBound.y <= upperBound.y);
end;

function Tb2AABB.RayCast(var output: Tb2RayCastOutput;
   const input: Tb2RayCastInput): Boolean;
var
   i: Integer;
   tmin, tmax, inv_d, t1, t2, s: PhysicsFloat;
   p, d, absD, normal: TVector2;
begin
   tmin := -FLT_MAX;
   tmax := FLT_MAX;

   p := input.p1;
   d := input.p2 - input.p1;
   absD := b2Abs(d);

   for i := 0 to 1 do
   begin
      if TVector2Arraied(absD)[i] < FLT_EPSILON then
      begin
         // Parallel.
         if (TVector2Arraied(p)[i] < TVector2Arraied(lowerBound)[i]) or
            (TVector2Arraied(upperBound)[i] < TVector2Arraied(p)[i]) then
         begin
            Result := False;
            Exit;
         end;
      end
      else
      begin
         inv_d := 1.0 / TVector2Arraied(d)[i];
         t1 := (TVector2Arraied(lowerBound)[i] - TVector2Arraied(p)[i]) * inv_d;
         t2 := (TVector2Arraied(upperBound)[i] - TVector2Arraied(p)[i]) * inv_d;

         // Sign of the normal vector.
         s := -1.0;

         if t1 > t2 then
         begin
            b2Swap(t1, t2);
            s := 1.0;
         end;

         // Push the min up
         if t1 > tmin then
         begin
            normal := b2Vec2_Zero;
            TVector2Arraied(normal)[i] := s;
            tmin := t1;
         end;

         // Pull the max down
         tmax := b2Min(tmax, t2);

         if tmin > tmax then
         begin
            Result := False;
            Exit;
         end;
      end;
   end;

   // Does the ray start inside the box?
   // Does the ray intersect beyond the max fraction?
   if (tmin < 0.0) or (input.maxFraction < tmin) then
   begin
      Result := False;
      Exit;
   end;

   // Intersection.
   output.fraction := tmin;
   output.normal := normal;
   Result := True;
end;

{$ENDIF}

{ Tb2Draw }

constructor Tb2Draw.Create;
begin
   m_drawFlags := [];
   m_shapeColor_Inactive := MakeColor(0.5, 0.5, 0.3);
   m_shapeColor_Static := MakeColor(0.5, 0.9, 0.5);
   m_shapeColor_Kinematic := MakeColor(0.5, 0.5, 0.9);
   m_shapeColor_Sleeping := MakeColor(0.6, 0.6, 0.6);
   m_shapeColor_Normal := MakeColor(0.9, 0.7, 0.7);

   m_pairColor := MakeColor(0.3, 0.9, 0.9);
   m_aabbColor := MakeColor(0.9, 0.3, 0.9);
   m_world_aabbColor := MakeColor(0.3, 0.9, 0.9);
   m_coreColor := MakeColor(0.9, 0.6, 0.6);
   m_jointLineColor := MakeColor(0.5, 0.8, 0.8);
end;

//////////////////////////////////////////////////////////////
// World

constructor Tb2World.Create(const gravity: TVector2);
begin
   m_destructionListener := nil;
   m_debugDraw := nil;

   m_bodyList := nil;
   m_jointList := nil;

   m_bodyCount := 0;
   m_jointCount := 0;

   {$IFDEF CONTROLLERS}
   m_controllerList := nil;
   m_controllerCount := 0;
   {$ENDIF}

   m_warmStarting := True;
   m_continuousPhysics := True;

 	 m_subStepping := False;
	 m_stepComplete := True;

   m_allowSleep := True;
   m_gravity := gravity;

   m_flags := e_world_clearForces;
   m_inv_dt0 := 0.0;

   m_contactManager := Tb2ContactManager.Create;
end;

destructor Tb2World.Destroy;
var
   p: Tb2Body;
begin
   // Free all shapes
   while Assigned(m_bodyList) do
   begin
      p := m_bodyList.m_next;
      DestroyBody(m_bodyList);
      m_bodyList := p;
   end;

   m_contactManager.Free;
end;

{$IFDEF ENABLE_DUMP}
procedure Tb2World.SetDumpMethod(method: Tb2DumpMethod);
begin
   b2DumpMethod := method;
end;

procedure Tb2World.Dump;
var
   i: Integer;
   b: Tb2Body;
   j: Tb2Joint;
begin
   if not Assigned(b2DumpMethod) then
      Exit;

   if IsLocked then
      Exit;

   b2DumpMethod(0, 'var bodies: array of Tb2Body;', []);
   b2DumpMethod(0, 'var joints: array of Tb2Joint;', []);
   b2DumpMethod(0, 'var fd: Tb2FixtureDef;', []);
   b2DumpMethod(0, 'var bd: Tb2BodyDef;', []);
   b2DumpMethod(0, 'var circle_shape: Tb2CircleShape;', []);
   b2DumpMethod(0, 'var edge_shape: Tb2EdgeShape;', []);
   b2DumpMethod(0, 'var poly_shape: Tb2PolygonShape;', []);
   b2DumpMethod(0, 'var chain_shape: Tb2ChainShape;', []);
   b2DumpMethod(0, 'var vs: TVectorArray;', []);

   b2DumpMethod(0, 'var distance_jd: Tb2DistanceJointDef;', []);
   b2DumpMethod(0, 'var prismatic_jd: Tb2PrismaticJointDef;', []);
   b2DumpMethod(0, 'var pulley_jd: Tb2PulleyJointDef;', []);
   b2DumpMethod(0, 'var revolute_jd: Tb2RevoluteJointDef;', []);
   b2DumpMethod(0, 'var gear_jd: Tb2GearJointDef;', []);
   b2DumpMethod(0, 'var friction_jd: Tb2FrictionJointDef;', []);
   b2DumpMethod(0, 'var wheel_jd: Tb2WheelJointDef;', []);
   b2DumpMethod(0, 'var weld_jd: Tb2WeldJointDef;', []);
   b2DumpMethod(0, 'var rope_jd: Tb2RopeJointDef;', []);

   b2DumpMethod(0, 'begin', []);
   b2DumpMethod(1, 'm_world.SetGravity(MakeVector(%s, %s));', [b2FloatToStr(m_gravity.x), b2FloatToStr(m_gravity.y)]);
   b2DumpMethod(1, 'SetLength(bodies, %d);', [m_bodyCount]);
   b2DumpMethod(1, 'SetLength(joints, %d);', [m_jointCount]);
   b2DumpMethod(1, '', []);
   b2DumpMethod(1, '// Bodies', []);
   i := 0;
   b := m_bodyList;
   while Assigned(b) do
   begin
      b.m_islandIndex := i;
      b.Dump;
      Inc(i);
      b := b.m_next;
   end;

   i := 0;
   j := m_jointList;
   while Assigned(j) do
   begin
      j.m_index := i;
      Inc(i);
      j := j.m_next;
   end;

   b2DumpMethod(1, '', []);
   b2DumpMethod(1, '// Joints', []);
   // First pass on joints, skip gear joints.
   j := m_jointList;
   while Assigned(j) do
   begin
      if j.m_type = e_gearJoint then
      begin
         j := j.m_next;
         Continue;
      end;

      j.Dump;
      j := j.m_next;
   end;

   // Second pass on joints, only gear joints.
   j := m_jointList;
   while Assigned(j) do
   begin
      if j.m_type <> e_gearJoint then
      begin
         j := j.m_next;
         Continue;
      end;

      j.Dump;
      j := j.m_next;
   end;

   b2DumpMethod(0, 'end;', []);
end;
{$ENDIF}

procedure Tb2World.SetContactFilter(filter: Tb2ContactFilter);
begin
   m_contactManager.m_contactFilter := filter;
end;

procedure Tb2World.SetContactListener(listener: Tb2ContactListener);
begin
   m_contactManager.m_contactListener := listener;
end;

var
   world_solve_island: Tb2Island;
   world_solve_stack: TList;
procedure Tb2World.Solve(const step: Tb2TimeStep);
var
   i: Integer;
   b, seed, other: Tb2Body;
   contact: Pb2Contact;
   j: Tb2Joint;
   stackCount: Int32;
   ce: Pb2ContactEdge;
   je: Pb2JointEdge;
   {$IFDEF CONTROLLERS}
   ctrl: Tb2Controller;
   {$ENDIF}

   {$IFDEF COMPUTE_PHYSICS_TIME}
   time0: Double;
   tmpProfile: Tb2Profile;
   {$ENDIF}
begin
   {$IFDEF COMPUTE_PHYSICS_TIME}
   m_profile.solveInit := 0.0;
   m_profile.solveVelocity := 0.0;
   m_profile.solvePosition := 0.0;
   {$ENDIF}

   {$IFDEF CONTROLLERS}
   // Step all controllers
   ctrl := m_controllerList;
   while Assigned(ctrl) do
   begin
      ctrl.Step(step);
      ctrl := ctrl.m_next;
   end;
   {$ENDIF}

   // Size the island for the worst case.
   world_solve_island.Reset(m_bodyCount, m_contactManager.m_contactCount,
      m_jointCount, m_contactManager.m_contactListener);

   // Clear all the island flags.
   b := m_bodyList;
   while Assigned(b) do
   begin
      b.m_flags := b.m_flags and (not e_body_islandFlag);
      b := b.m_next;
   end;

   contact := m_contactManager.m_contactList;
   while Assigned(contact) do
   begin
      contact^.m_flags := contact^.m_flags and (not e_contact_islandFlag);
      contact := contact^.m_next;
   end;

   j := m_jointList;
   while Assigned(j) do
   begin
      j.m_islandFlag := False;
      j := j.m_next;
   end;

   // Build and simulate all awake islands.
   world_solve_stack.Count := m_bodyCount;
   seed := m_bodyList;
   while Assigned(seed) do
   begin
      if seed.m_flags and e_body_islandFlag <> 0 then
      begin
         seed := seed.m_next;
         Continue;
      end;

      if (not seed.IsAwake) or (not seed.IsActive)  then
      begin
         seed := seed.m_next;
         Continue;
      end;

      // The seed can be dynamic or kinematic.
      if seed.m_type = b2_staticBody then
      begin
         seed := seed.m_next;
         Continue;
      end;

      // Reset island and stack.
      world_solve_island.Clear;
      stackCount := 0;
      world_solve_stack[stackCount] := seed;
      Inc(stackCount);
      seed.m_flags := seed.m_flags or e_body_islandFlag;

      // Perform a depth first search (DFS) on the constraint graph.
      while (stackCount > 0) do
      begin
         // Grab the next body off the stack and add it to the island.
         Dec(stackCount);
         b := Tb2Body(world_solve_stack[stackCount]);
         //b2Assert(b->IsActive() == True);
         world_solve_island.Add(b);

         // Make sure the body is awake.
         b.SetAwake(True);

         // To keep islands as small as possible, we don't
         // propagate islands across static bodies.
         if b.m_type = b2_staticBody then
            Continue;

         // Search all contacts connected to this body.
         ce := b.m_contactList;
         while Assigned(ce) do
         begin
            contact := ce^.contact;

            // Has this contact already been added to an island?
            if contact^.m_flags and e_contact_islandFlag <> 0 then
            begin
               ce := ce^.next;
               Continue;
            end;

            // Is this contact solid and touching?
            {$IFDEF OP_OVERLOAD}
            if (not contact^.IsEnabled) or (not contact^.IsTouching) then
            {$ELSE}
            if (not IsEnabled(contact^)) or (not IsTouching(contact^)) then
            {$ENDIF}
            begin
               ce := ce^.next;
               Continue;
            end;

            // Skip sensors.
            if contact^.m_fixtureA.m_isSensor or contact^.m_fixtureB.m_isSensor then
            begin
               ce := ce^.next;
               Continue;
            end;

            world_solve_island.Add(contact);
            contact^.m_flags := contact^.m_flags or e_contact_islandFlag;

            other := ce^.other;
            // Was the other body already added to this island?
            if (other.m_flags and e_body_islandFlag) <> 0 then
            begin
               ce := ce^.next;
               Continue;
            end;

            //b2Assert(stackCount < stackSize);
            world_solve_stack[stackCount] := other;
            Inc(stackCount);

            other.m_flags := other.m_flags or e_body_islandFlag;
            ce := ce^.next;
         end;

         // Search all joints connect to this body.
         je := b.m_jointList;
         while Assigned(je) do
         begin
            if je^.joint.m_islandFlag then
            begin
               je := je^.next;
               Continue;
            end;

            other := je^.other;
            // Don't simulate joints connected to inactive bodies.
            if not other.IsActive then
            begin
               je := je^.next;
               Continue;
            end;

            world_solve_island.Add(je^.joint);
            je^.joint.m_islandFlag := True;

            if (other.m_flags and e_body_islandFlag) <> 0 then
            begin
               je := je^.next;
               Continue;
            end;

            //b2Assert(stackCount < stackSize);
            world_solve_stack[stackCount] := other;
            Inc(stackCount);
            other.m_flags := other.m_flags or e_body_islandFlag;
            je := je^.next;
         end;
      end;

      {$IFDEF COMPUTE_PHYSICS_TIME}
      world_solve_island.Solve(tmpProfile, step, m_gravity, m_allowSleep);
      m_profile.solveInit := m_profile.solveInit + tmpProfile.solveInit;
		  m_profile.solveVelocity := m_profile.solveVelocity + tmpProfile.solveVelocity;
		  m_profile.solvePosition := m_profile.solvePosition + tmpProfile.solvePosition;
      {$ELSE}
      world_solve_island.Solve(step, m_gravity, m_allowSleep);
      {$ENDIF}

      // Post solve cleanup.
      for i := 0 to world_solve_island.m_bodyCount - 1 do
      begin
         // Allow static bodies to participate in other islands.
         b := Tb2Body(world_solve_island.m_bodies[i]);
         if b.m_type = b2_staticBody then
            b.m_flags := b.m_flags and (not e_body_islandFlag);
      end;
      seed := seed.m_next;
   end;

   {$IFDEF COMPUTE_PHYSICS_TIME}
   time0 := GetRawReferenceTime();
   {$ENDIF}
   // Synchronize fixtures, check for out of range bodies.
   b := m_bodyList;
   while Assigned(b) do
   begin
      // If a body was not in an island then it did not move.
      if b.m_flags and e_body_islandFlag = 0 then
      begin
         b := b.m_next;
         Continue;
      end;

      if b.m_type = b2_staticBody then
      begin
         b := b.m_next;
         Continue;
      end;
      b.SynchronizeFixtures; // Update fixtures (for broad-phase).
      b := b.m_next;
   end;

	 // Look for new contacts.
	 m_contactManager.FindNewContacts;
   {$IFDEF COMPUTE_PHYSICS_TIME}
   m_profile.broadphase := GetRawReferenceTime() - time0;
   {$ENDIF}
end;

procedure Tb2World.SolveTOI(const step: Tb2TimeStep);
var
   i: Integer;
   b, bA, bB, body, other: Tb2Body;
   activeA, activeB, collideA, collideB: Boolean;
   c, minContact, contact: Pb2Contact;
   minAlpha, alpha, alpha0, beta: PhysicsFloat;
   input: Tb2TOIInput;
   output: Tb2TOIOutput;
   backup, backup1, backup2: Tb2Sweep;
   bodies: array[0..1] of Tb2Body;
   ce: Pb2ContactEdge;
   subStep: Tb2TimeStep;
begin
   world_solve_island.Reset(2 * b2_maxTOIContacts, b2_maxTOIContacts,
      0, m_contactManager.m_contactListener);

   if m_stepComplete then
   begin
      b := m_bodyList;
      while Assigned(b) do
      begin
         b.m_flags := b.m_flags and (not e_body_islandFlag);
         b.m_sweep.alpha0 := 0.0;
         b := b.m_next;
      end;

      c := m_contactManager.m_contactList;
      while Assigned(c) do
      begin
         // Invalidate TOI
		     c.m_toiCount := 0;
		     c.m_toi := 1.0;
         c.m_flags := c.m_flags and (not (e_contact_toiFlag or e_contact_islandFlag));
         c := c.m_next;
      end;
   end;

   // Find TOI events and solve them.
   while True do
   begin
      // Find the first TOI.
      minContact := nil;
      minAlpha := 1.0;

      c := m_contactManager.m_contactList;
      while Assigned(c) do
      begin
         // Is this contact disabled?
         {$IFDEF OP_OVERLOAD}
         if not c.IsEnabled then
         {$ELSE}
         if not IsEnabled(c^) then
         {$ENDIF}
         begin
            c := c.m_next;
            Continue;
         end;

         // Prevent excessive sub-stepping.
         if c.m_toiCount > b2_maxSubSteps then
         begin
            c := c.m_next;
            Continue;
         end;

         if (c.m_flags and e_contact_toiFlag) <> 0  then
            alpha := c.m_toi // This contact has a valid cached TOI.
         else
         begin
            // Is there a sensor?
            if c.m_fixtureA.IsSensor or c.m_fixtureB.IsSensor then
            begin
               c := c.m_next;
               Continue;
            end;

            bA := c.m_fixtureA.m_body;
            bB := c.m_fixtureB.m_body;

            //b2BodyType typeA := bA.GetType();
            //b2BodyType typeB := bB.GetType();
            //b2Assert(typeA == b2_dynamicBody || typeB == b2_dynamicBody);

            activeA := bA.IsAwake and (bA.m_type <> b2_staticBody);
            activeB := bB.IsAwake and (bB.m_type <> b2_staticBody);
            // Is at least one body active (awake and dynamic or kinematic)?
            if (not activeA) and (not activeB) then
            begin
               c := c.m_next;
               Continue;
            end;

            collideA := bA.IsBullet or (bA.m_type <> b2_dynamicBody);
            collideB := bB.IsBullet or (bB.m_type <> b2_dynamicBody);
            // Are these two non-bullet dynamic bodies?
            if (not collideA) and (not collideB) then
            begin
               c := c.m_next;
               Continue;
            end;

            // Compute the TOI for this contact.
            // Put the sweeps onto the same time interval.
            alpha0 := bA.m_sweep.alpha0;

            if bA.m_sweep.alpha0 < bB.m_sweep.alpha0 then
            begin
               alpha0 := bB.m_sweep.alpha0;
               {$IFDEF OP_OVERLOAD}
               bA.m_sweep.Advance(alpha0);
               {$ELSE}
               Advance(bA.m_sweep, alpha0);
               {$ENDIF}
            end
            else if bB.m_sweep.alpha0 < bA.m_sweep.alpha0 then
            begin
               alpha0 := bA.m_sweep.alpha0;
               {$IFDEF OP_OVERLOAD}
               bB.m_sweep.Advance(alpha0);
               {$ELSE}
               Advance(bB.m_sweep, alpha0);
               {$ENDIF}
            end;

            //b2Assert(alpha0 < 1.0f);
            // Compute the time of impact in interval [0, minTOI]
            {$IFDEF OP_OVERLOAD}
            input.proxyA.SetShape(c.m_fixtureA.m_shape, c.m_indexA);
            input.proxyB.SetShape(c.m_fixtureB.m_shape, c.m_indexB);
            {$ELSE}
            SetShape(input.proxyA, c.m_fixtureA.m_shape, c.m_indexA);
            SetShape(input.proxyB, c.m_fixtureB.m_shape, c.m_indexB);
            {$ENDIF}
            input.sweepA := bA.m_sweep;
            input.sweepB := bB.m_sweep;
            input.tMax := 1.0;

            b2TimeOfImpact(output, input);

            // Beta is the fraction of the remaining portion of the .
            beta := output.t;

            if output.state = e_toi_touching then
               alpha := b2Min(alpha0 + (1.0 - alpha0) * beta, 1.0)
            else
               alpha := 1.0;

            c.m_toi := alpha;
            c.m_flags := c.m_flags or e_contact_toiFlag;
         end;

         if alpha < minAlpha then
         begin
            // This is the minimum TOI found so far.
            minContact := c;
            minAlpha := alpha;
         end;

         c := c.m_next;
      end;

      if (not Assigned(minContact)) or ( 1.0 - 10.0 * FLT_EPSILON < minAlpha) then
      begin
         // No more TOI events. Done!
         m_stepComplete := True;
         Break;
      end;

      // Advance the bodies to the TOI.
      bA := minContact.m_fixtureA.m_body;
      bB := minContact.m_fixtureB.m_body;

      backup1 := bA.m_sweep;
      backup2 := bB.m_sweep;

      bA.Advance(minAlpha);
      bB.Advance(minAlpha);

      // The TOI contact likely has some new contact points.
      {$IFDEF OP_OVERLOAD}
      minContact.Update(m_contactManager.m_contactListener);
      {$ELSE}
      Update(minContact^, m_contactManager.m_contactListener);
      {$ENDIF}
      minContact.m_flags := minContact.m_flags and (not e_contact_toiFlag);
      Inc(minContact.m_toiCount);

      // Is the contact solid?
      {$IFDEF OP_OVERLOAD}
      if (not minContact.IsEnabled) or (not minContact.IsTouching) then
      {$ELSE}
      if (not IsEnabled(minContact^)) or (not IsTouching(minContact^)) then
      {$ENDIF}
      begin
         // Restore the sweeps.
         {$IFDEF OP_OVERLOAD}
         minContact.SetEnabled(False);
         {$ELSE}
         SetEnabled(minContact^, False);
         {$ENDIF}
         bA.m_sweep := backup1;
         bB.m_sweep := backup2;
         bA.SynchronizeTransform;
         bB.SynchronizeTransform;
         Continue;
      end;

      bA.SetAwake(True);
      bB.SetAwake(True);

      // Build the island
      world_solve_island.Clear;
      world_solve_island.Add(bA);
      world_solve_island.Add(bB);
      world_solve_island.Add(minContact);

      bA.m_flags := bA.m_flags or e_body_islandFlag;
      bB.m_flags := bB.m_flags or e_body_islandFlag;
      minContact.m_flags := minContact.m_flags or e_contact_islandFlag;

      // Get contacts on bodyA and bodyB.
      bodies[0] := bA;
      bodies[1] := bB;
      for i := 0 to 1 do
      begin
         body := bodies[i];
         if body.m_type = b2_dynamicBody then
         begin
            ce := body.m_contactList;
            while Assigned(ce) and (world_solve_island.m_bodyCount < b2_maxTOIContacts) do
            begin
               if (world_solve_island.m_bodyCount = world_solve_island.m_bodyCapacity) or
                (world_solve_island.m_contactCount = world_solve_island.m_contactCapacity) then
                  Break;

               contact := ce.contact;

               // Has this contact already been added to the island?
               if (contact.m_flags and e_contact_islandFlag) <> 0 then
               begin
                  ce := ce.next;
                  Continue;
               end;

               // Only add static, kinematic, or bullet bodies.
               other := ce.other;
               if (other.m_type = b2_dynamicBody) and (not body.IsBullet) and (not other.IsBullet) then
               begin
                  ce := ce.next;
                  Continue;
               end;

               // Skip sensors.
               if contact.m_fixtureA.m_isSensor or contact.m_fixtureB.m_isSensor then
               begin
                  ce := ce.next;
                  Continue;
               end;

               // Tentatively advance the body to the TOI.
               backup := other.m_sweep;
               if (other.m_flags and e_body_islandFlag) = 0 then
                  other.Advance(minAlpha);

               // Update the contact points
               {$IFDEF OP_OVERLOAD}
               contact.Update(m_contactManager.m_contactListener);
               {$ELSE}
               Update(contact^, m_contactManager.m_contactListener);
               {$ENDIF}

               // Was the contact disabled by the user?
               {$IFDEF OP_OVERLOAD}
               if not contact.IsEnabled then
               {$ELSE}
               if not IsEnabled(contact^) then
               {$ENDIF}
               begin
                  other.m_sweep := backup;
                  other.SynchronizeTransform;
                  ce := ce.next;
                  Continue;
               end;

               // Are there contact points?
               {$IFDEF OP_OVERLOAD}
               if not contact.IsTouching then
               {$ELSE}
               if not IsTouching(contact^) then
               {$ENDIF}
               begin
                  other.m_sweep := backup;
                  other.SynchronizeTransform;
                  ce := ce.next;
                  Continue;
               end;

               // Add the contact to the island
               contact.m_flags := contact.m_flags or e_contact_islandFlag;
               world_solve_island.Add(contact);

               // Has the other body already been added to the island?
               if (other.m_flags and e_body_islandFlag) <> 0 then
               begin
                  ce := ce.next;
                  Continue;
               end;

               // Add the other body to the island.
               other.m_flags := other.m_flags or e_body_islandFlag;

               if other.m_type <> b2_staticBody then
                  other.SetAwake(True);

               world_solve_island.Add(other);
               ce := ce.next;
            end;
         end;
      end;

      subStep.dt := (1.0 - minAlpha) * step.dt;
      subStep.inv_dt := 1.0 / subStep.dt;
      subStep.dtRatio := 1.0;
      subStep.positionIterations := 20;
      subStep.velocityIterations := step.velocityIterations;
      subStep.warmStarting := false;
      world_solve_island.SolveTOI(subStep, bA.m_islandIndex, bB.m_islandIndex);

      // Reset island flags and synchronize broad-phase proxies.
      for i := 0 to world_solve_island.m_bodyCount - 1 do
         with Tb2Body(world_solve_island.m_bodies[i]) do
         begin
            m_flags := m_flags and (not e_body_islandFlag);
            if m_type <> b2_dynamicBody then
               Continue;
            SynchronizeFixtures;

            // Invalidate all contact TOIs on this displaced body.
            ce := m_contactList;
            while Assigned(ce) do
            begin
               ce.contact.m_flags := ce.contact.m_flags and (not (e_contact_toiFlag or e_contact_islandFlag));
               ce := ce.next;
            end;
         end;

      // Commit fixture proxy movements to the broad-phase so that new contacts are created.
      // Also, some contacts can be destroyed.
      m_contactManager.FindNewContacts;

      if m_subStepping then
      begin
         m_stepComplete := False;
         Break;
      end;
   end;
end;

procedure Tb2World.ClearForces;
var
   body: Tb2Body;
begin
   body := m_bodyList;
   while Assigned(body) do
      with body do
      begin
         m_force := b2Vec2_Zero;
         m_torque := 0.0;
         body := m_next;
      end;
end;

procedure Tb2World.DrawDebugData;
var
   i: Integer;
   xf: Tb2Transform;
   b: Tb2Body;
   f: Tb2Fixture;
   j: Tb2Joint;
   c: Pb2Contact;
   aabb: Tb2AABB;
   vs: TVectorArray4;
   {$IFDEF CONTROLLERS}ctrl: Tb2Controller;{$ENDIF}
begin
   if not Assigned(m_debugDraw) then
      Exit;

   with m_debugDraw do
   begin
      if e_shapeBit in m_drawFlags then
      begin
         b := m_bodyList;
         while Assigned(b) do
         begin
            f := b.GetFixtureList;
            while Assigned(f) do
            begin
               if not b.IsActive then
                  DrawShape(f, b.m_xf, m_shapeColor_Inactive)
               else if b.m_type = b2_staticBody then
                  DrawShape(f, b.m_xf, m_shapeColor_Static)
               else if b.m_type = b2_kinematicBody then
                  DrawShape(f, b.m_xf, m_shapeColor_kinematic)
               else if not b.IsAwake then
                  DrawShape(f, b.m_xf, m_shapeColor_Sleeping)
               else
                  DrawShape(f, b.m_xf, m_shapeColor_Normal);

               f := f.m_next;
            end;
            b := b.m_next;
         end;
      end;

      if e_jointBit in m_drawFlags then
      begin
         j := m_jointList;
         while Assigned(j) do
         begin
            DrawJoint(j);
            j := j.m_next;
         end;
      end;

      {$IFDEF CONTROLLERS}
      if e_controllerBit in m_drawFlags then
      begin
         ctrl := m_controllerList;
         while Assigned(ctrl) do
         begin
            ctrl.Draw(m_debugDraw);
            ctrl := ctrl.m_next;
         end;
      end;
      {$ENDIF}

      if e_pairBit in m_drawFlags then
      begin
         c := m_contactManager.m_contactList;
         while Assigned(c) do
            with c^ do
            begin
               (*{$IFDEF OP_OVERLOAD}
               DrawSegment(m_fixtureA.m_aabb.GetCenter,
                  m_fixtureB.m_aabb.GetCenter, m_pairColor);
               {$ELSE}
               DrawSegment(GetCenter(m_fixtureA.m_aabb),
                  GetCenter(m_fixtureB.m_aabb), m_pairColor);
               {$ENDIF}*)
               c := m_next;
            end;
      end;

      if e_aabbBit in m_drawFlags then
      begin
         b := m_bodyList;
         while Assigned(b) do
         begin
            if not b.IsActive then
            begin
               b := b.m_next;
               Continue;
            end;

            f := b.GetFixtureList;
            while Assigned(f) do
            begin
               for i := 0 to f.m_proxyCount - 1 do
               begin
					        aabb := m_contactManager.m_broadPhase.GetFatAABB(f.m_proxies[i].proxyId)^;
                  {$IFDEF OP_OVERLOAD}
                  vs[0].SetValue(aabb.lowerBound.x, aabb.lowerBound.y);
                  vs[1].SetValue(aabb.upperBound.x, aabb.lowerBound.y);
                  vs[2].SetValue(aabb.upperBound.x, aabb.upperBound.y);
                  vs[3].SetValue(aabb.lowerBound.x, aabb.upperBound.y);
                  {$ELSE}
                  SetValue(vs[0], aabb.lowerBound.x, aabb.lowerBound.y);
                  SetValue(vs[1], aabb.upperBound.x, aabb.lowerBound.y);
                  SetValue(vs[2], aabb.upperBound.x, aabb.upperBound.y);
                  SetValue(vs[3], aabb.lowerBound.x, aabb.upperBound.y);
                  {$ENDIF}
                  DrawPolygon4(vs, 4, m_aabbColor);
               end;
               f := f.m_next;
            end;
            b := b.m_next;
         end;
      end;

      if e_centerOfMassBit in m_drawFlags then
      begin
         b := m_bodyList;
         while Assigned(b) do
         begin
            xf := b.m_xf;
            xf.p := b.GetWorldCenter;
            DrawTransform(xf);
            b := b.m_next;
         end;
      end;
   end;
end;

type
   Tb2WorldQueryWrapper = class(Tb2GenericCallBackWrapper)
   public
      broadPhase: Tb2BroadPhase;
      callback: Tb2QueryCallback;

      function QueryCallback(proxyId: Int32): Boolean; override;
   end;

   Tb2WorldRayCastWrapper = class(Tb2GenericCallBackWrapper)
   public
      broadPhase: Tb2BroadPhase;
	    callback: Tb2RayCastCallback;

      function RayCastCallback(const input: Tb2RayCastInput; proxyId: Int32): PhysicsFloat; override;
   end;

var
   world_query_wrapper: Tb2WorldQueryWrapper;
   world_raycast_wrapper: Tb2WorldRayCastWrapper;

{ Tb2WorldQueryWrapper }

function Tb2WorldQueryWrapper.QueryCallback(proxyId: Int32): Boolean;
var
   proxy: Pb2FixtureProxy;
begin
	 proxy := Pb2FixtureProxy(broadPhase.GetUserData(proxyId));
	 Result := callback.ReportFixture(proxy^.fixture);
end;

{ Tb2WorldRayCastWrapper }

function Tb2WorldRayCastWrapper.RayCastCallback(const input: Tb2RayCastInput;
   proxyId: Int32): PhysicsFloat;
var
   proxy: Pb2FixtureProxy;
   fixture: Tb2Fixture;
   output: Tb2RayCastOutput;
   fraction: PhysicsFloat;
   point: TVector2;
begin
   proxy := Pb2FixtureProxy(broadPhase.GetUserData(proxyId));
   fixture := proxy^.fixture;
   if fixture.RayCast(output, input, proxy^.childIndex) then
   begin
      fraction := output.fraction;
      {$IFDEF OP_OVERLOAD}
      point := (1.0 - fraction) * input.p1 + fraction * input.p2;
      {$ELSE}
      point := Add(Multiply(input.p1, (1.0 - fraction)), Multiply(input.p2, fraction));
      {$ENDIF}
      Result := callback.ReportFixture(fixture, point, output.normal, fraction);
   end
   else
      Result := input.maxFraction;
end;

procedure Tb2World.QueryAABB(callback: Tb2QueryCallback; const aabb: Tb2AABB);
begin
   world_query_wrapper.broadPhase := m_contactManager.m_broadPhase;
   world_query_wrapper.callback := callback;
   m_contactManager.m_broadPhase.Query(world_query_wrapper, aabb);
end;

procedure Tb2World.RayCast(callback: Tb2RayCastCallback; const point1, point2: TVector2);
var
   input: Tb2RayCastInput;
begin
   world_raycast_wrapper.broadPhase := m_contactManager.m_broadPhase;
   world_raycast_wrapper.callback := callback;
   with input do
   begin
      maxFraction := 1.0;
      p1 := point1;
      p2 := point2;
   end;
   m_contactManager.m_broadPhase.RayCast(world_raycast_wrapper, input);
end;

function Tb2World.GetContactList: Pb2Contact;
begin
   Result := m_contactManager.m_contactList;
end;

function Tb2World.GetContactCount: Int32;
begin
   Result := m_contactManager.m_contactCount;
end;

function Tb2World.GetTreeHeight: Int32;
begin
   Result := m_contactManager.m_broadPhase.GetTreeHeight;
end;

function Tb2World.GetTreeBalance: Int32;
begin
   Result := m_contactManager.m_broadPhase.GetTreeBalance;
end;

function Tb2World.GetTreeQuality: PhysicsFloat;
begin
   Result := m_contactManager.m_broadPhase.GetTreeQuality;
end;

procedure Tb2World.DrawShape(fixture: Tb2Fixture; const xf: Tb2Transform;
  const color: RGBA);
var
   i: Integer;
   center, v1, v2, axis: TVector2;
   vertices: Tb2PolyVertices;
begin
   with m_debugDraw do
      case fixture.GetType of
         e_circleShape:
            with Tb2CircleShape(fixture.m_shape) do
            begin
               center := b2Mul(xf, m_p);
               axis := b2Mul(xf.q, MakeVector(1.0, 0.0));
               m_debugDraw.DrawSolidCircle(center, axis, m_radius, color);
            end;
         e_edgeShape:
            with Tb2EdgeShape(fixture.m_shape) do
            begin
               v1 := b2Mul(xf, m_vertex1);
               v2 := b2Mul(xf, m_vertex2);
               m_debugDraw.DrawSegment(v1, v2, color);
            end;
         e_polygonShape:
            with Tb2PolygonShape(fixture.m_shape) do
            begin
               //b2Assert(vertexCount <= b2_maxPolygonVertices);
               for i := 0 to m_count - 1 do
                  vertices[i] := b2Mul(xf, m_vertices[i]);

               DrawSolidPolygon(vertices, m_count, color);
            end;
         e_chainShape:
            with Tb2ChainShape(fixture.m_shape) do
            begin
               if m_loop then
               begin
                  v1 := b2Mul(xf, m_vertices[m_count - 1]);
                  for i := 0 to m_count - 1 do
                  begin
                     v2 := b2Mul(xf, m_vertices[i]);
                     m_debugDraw.DrawSegment(v1, v2, color);
                     m_debugDraw.DrawCircle(v1, 0.05, color);
                     v1 := v2;
                  end;               
               end
               else
               begin
                  v1 := b2Mul(xf, m_vertices[0]);
                  for i := 1 to m_count - 1 do
                  begin
                     v2 := b2Mul(xf, m_vertices[i]);
                     m_debugDraw.DrawSegment(v1, v2, color);
                     m_debugDraw.DrawCircle(v1, 0.05, color);
                     v1 := v2;
                  end;
                  m_debugDraw.DrawCircle(v2, 0.05, color);
               end;
            end;
      end;
end;

procedure Tb2World.DrawJoint(joint: Tb2Joint);
var
   p1, p2, s1, s2: TVector2;
begin
   p1 := joint.GetAnchorA;
   p2 := joint.GetAnchorB;

   with m_debugDraw do
   begin
      case joint.m_type of
         e_distanceJoint: DrawSegment(p1, p2, m_jointLineColor);
         e_pulleyJoint:
            with Tb2PulleyJoint(joint) do
            begin
               s1 := GetGroundAnchorA;
               s2 := GetGroundAnchorB;
               DrawSegment(s1, p1, m_jointLineColor);
               DrawSegment(s2, p2, m_jointLineColor);
               DrawSegment(s1, s2, m_jointLineColor);
            end;
         e_mouseJoint: ;
      else
         DrawSegment(joint.m_bodyA.m_xf.p, p1, m_jointLineColor);
         DrawSegment(p1, p2, m_jointLineColor);
         DrawSegment(joint.m_bodyB.m_xf.p, p2, m_jointLineColor);
      end;
   end;
end;

{$IFDEF COMPUTE_PHYSICS_TIME}
function Tb2World.FGetProfile: Pb2Profile;
begin
   Result := @m_profile;
end;
{$ENDIF}

procedure Tb2World.FSetAllowSleeping(value: Boolean);
var
   b: Tb2Body;
begin
   if value = m_allowSleep then
      Exit;

   m_allowSleep := value;
   if not m_allowSleep then
   begin
      b := m_bodyList;
      while Assigned(b) do
      begin
         b.SetAwake(True);
         b := b.m_next;
      end;
   end;
end;

function Tb2World.CreateBody(def: Tb2BodyDef; AutoFreeBodyDef: Boolean = True): Tb2Body;
begin
   //b2Assert(IsLocked() == false);
   if IsLocked then
   begin
      Result := nil;
      Exit;
   end;

   Result := Tb2Body.Create(def, Self);

   // Add to world doubly linked list.
   Result.m_prev := nil;
   Result.m_next := m_bodyList;
   if Assigned(m_bodyList) then
      m_bodyList.m_prev := Result;
   m_bodyList := Result;
   Inc(m_bodyCount);

   if AutoFreeBodyDef then
      def.Free;
end;

procedure Tb2World.DestroyBody(body: Tb2Body; DoFree: Boolean = True);
var
   je, je0: Pb2JointEdge;
   ce, ce0: Pb2ContactEdge;
   f, f0: Tb2Fixture;
   {$IFDEF CONTROLLERS}
   coe, coe0: Pb2ControllerEdge;
   {$ENDIF}
begin
   //b2Assert(m_bodyCount > 0);
   //b2Assert(IsLocked() == false);
   if IsLocked then
      Exit;

   // Delete the attached joints.
   je := body.m_jointList;

   if Assigned(m_destructionListener) then
   begin
      while Assigned(je) do
      begin
         je0 := je;
         je := je^.next;
         m_destructionListener.SayGoodbye(je0^.joint);
         DestroyJoint(je0^.joint);
         body.m_jointList := je;
      end;
   end
   else
   begin
      while Assigned(je) do
      begin
         je0 := je;
         je := je^.next;
         DestroyJoint(je0^.joint);
         body.m_jointList := je;
      end;
   end;
   body.m_jointList := nil;

   {$IFDEF CONTROLLERS}
   // Detach controllers attached to this body
   coe := body.m_controllerList;
   while Assigned(coe) do
   begin
      coe0 := coe;
      coe := coe^.nextController;
      coe0^.controller.RemoveBody(body);
   end;
   {$ENDIF}

   // Delete the attached contacts.
   ce := body.m_contactList;
   while Assigned(ce) do
   begin
      ce0 := ce;
      ce := ce^.next;
      m_contactManager.Destroy(ce0^.contact);
   end;
   body.m_contactList := nil;

   // Delete the attached fixtures. This destroys broad-phase proxies.
   f := body.m_fixtureList;
   while Assigned(f) do
   begin
      f0 := f;
      f := f.m_next;

      if Assigned(m_destructionListener) then
         m_destructionListener.SayGoodbye(f0);

      f0.DestroyProxies(m_contactManager.m_broadPhase);
      f0.Free;
	   	body.m_fixtureList := f;
	   	Dec(body.m_fixtureCount);
   end;
   body.m_fixtureList := nil;
   body.m_fixtureCount := 0;

   // Remove world body list.
   if Assigned(body.m_prev) then
      body.m_prev.m_next := body.m_next;

   if Assigned(body.m_next) then
      body.m_next.m_prev := body.m_prev;

   if body = m_bodyList then
      m_bodyList := body.m_next;

   Dec(m_bodyCount);
   if DoFree then
    begin
     {$IFNDEF AUTOREFCOUNT}
      body.Destroy2;
     {$ELSE}
      body.Free;
     {$ENDIF}
    end;
end;

function Tb2World.CreateJoint(def: Tb2JointDef; AutoFreeJointDef: Boolean = True): Tb2Joint;
var
   j: Tb2Joint;
   edge: Pb2ContactEdge;
begin
   //b2Assert(IsLocked() == false);
   if IsLocked then
   begin
      Result := nil;
      Exit;
   end;

   Result := nil;
   case def.jointType of
      e_unknownJoint: Exit;
      e_revoluteJoint: j := Tb2RevoluteJoint.Create(Tb2RevoluteJointDef(def));
      e_prismaticJoint: j := Tb2PrismaticJoint.Create(Tb2PrismaticJointDef(def));
      e_distanceJoint: j := Tb2DistanceJoint.Create(Tb2DistanceJointDef(def));
      e_pulleyJoint: j := Tb2PulleyJoint.Create(Tb2PulleyJointDef(def));
      e_mouseJoint: j := Tb2MouseJoint.Create(Tb2MouseJointDef(def));
      e_gearJoint: j := Tb2GearJoint.Create(Tb2GearJointDef(def));
      e_wheelJoint: j := Tb2WheelJoint.Create(Tb2WheelJointDef(def));
      e_weldJoint: j := Tb2WeldJoint.Create(Tb2WeldJointDef(def));
      e_frictionJoint: j := Tb2FrictionJoint.Create(Tb2FrictionJointDef(def));
      e_ropeJoint: j := Tb2RopeJoint.Create(Tb2RopeJointDef(def));
      e_motorJoint: j := Tb2MotorJoint.Create(Tb2MotorJointDef(def));
   end;

   // Connect to the world list.
   j.m_prev := nil;
   j.m_next := m_jointList;
   if Assigned(m_jointList) then
      m_jointList.m_prev := j;

   m_jointList := j;
   Inc(m_jointCount);

   // Connect to the bodies' doubly linked lists.
   j.m_edgeA.joint := j;
   j.m_edgeA.other := j.m_bodyB;
   j.m_edgeA.prev := nil;
   j.m_edgeA.next := j.m_bodyA.m_jointList;
   if Assigned(j.m_bodyA.m_jointList) then
      j.m_bodyA.m_jointList.prev := @j.m_edgeA;
   j.m_bodyA.m_jointList := @j.m_edgeA;

   j.m_edgeB.joint := j;
   j.m_edgeB.other := j.m_bodyA;
   j.m_edgeB.prev := nil;
   j.m_edgeB.next := j.m_bodyB.m_jointList;
   if Assigned(j.m_bodyB.m_jointList) then
      j.m_bodyB.m_jointList.prev := @j.m_edgeB;
   j.m_bodyB.m_jointList := @j.m_edgeB;

   // If the joint prevents collisions, then flag any contacts for filtering.
   if not def.collideConnected then
   begin
      edge := def.bodyB.GetContactList;
      while Assigned(edge) do
      begin
         if edge^.other = def.bodyA then
         begin
            // Flag the contact for filtering at the next time step (where either body is awake).
            {$IFDEF OP_OVERLOAD}
            edge^.contact.FlagForFiltering;
            {$ELSE}
            FlagForFiltering(edge^.contact^);
            {$ENDIF}
         end;

         edge := edge^.next;
      end;
   end;

   if AutoFreeJointDef then
      def.Free;

   // Note: creating a joint doesn't wake the bodies.
   Result := j;
end;

procedure Tb2World.DestroyJoint(j: Tb2Joint);
var
   collideConnected: Boolean;
   edge: Pb2ContactEdge;
begin
   //b2Assert(IsLocked() == false);
   if IsLocked then
      Exit;

   collideConnected := j.m_collideConnected;

   // Remove from the doubly linked list.
   if Assigned(j.m_prev) then
      j.m_prev.m_next := j.m_next;

   if Assigned(j.m_next) then
      j.m_next.m_prev := j.m_prev;

   if j = m_jointList then
      m_jointList := j.m_next;

   // Disconnect from island graph.
   // Wake up connected bodies.
   j.m_bodyA.SetAwake(True);
   j.m_bodyB.SetAwake(True);

   // Remove from body 1.
   if Assigned(j.m_edgeA.prev) then
      j.m_edgeA.prev^.next := j.m_edgeA.next;

   if Assigned(j.m_edgeA.next) then
      j.m_edgeA.next^.prev := j.m_edgeA.prev;

   if (@j.m_edgeA) = j.m_bodyA.m_jointList then
      j.m_bodyA.m_jointList := j.m_edgeA.next;

   j.m_edgeA.prev := nil;
   j.m_edgeA.next := nil;

   // Remove from body 2
   if Assigned(j.m_edgeB.prev) then
      j.m_edgeB.prev^.next := j.m_edgeB.next;

   if Assigned(j.m_edgeB.next) then
      j.m_edgeB.next^.prev := j.m_edgeB.prev;

   if (@j.m_edgeB) = j.m_bodyB.m_jointList then
      j.m_bodyB.m_jointList := j.m_edgeB.next;

   j.m_edgeB.prev := nil;
   j.m_edgeB.next := nil;

   //b2Assert(m_jointCount > 0);
   Dec(m_jointCount);

   // If the joint prevents collisions, then flag any contacts for filtering.
   if not collideConnected then
   begin
      edge := j.m_bodyB.GetContactList;
      while Assigned(edge) do
      begin
         if edge^.other = j.m_bodyA then
         begin
            // Flag the contact for filtering at the next time step (where either
            // body is awake).
            {$IFDEF OP_OVERLOAD}
            edge^.contact.FlagForFiltering;
            {$ELSE}
            FlagForFiltering(edge^.contact^);
            {$ENDIF}
         end;
         edge := edge^.next;
      end;
   end;
   j.Free;
end;

{$IFDEF CONTROLLERS}
procedure Tb2World.AddController(c: Tb2Controller);
begin
   c.m_next := m_controllerList;
   c.m_prev := nil;
   m_controllerList := c;
   c.m_world := Self;
   Inc(m_controllerCount);
end;

procedure Tb2World.RemoveController(c: Tb2Controller);
begin
   //TODO: Remove bodies from controller
   if Assigned(c.m_prev) then
      c.m_prev.m_next := c.m_next;
   if Assigned(c.m_next) then
      c.m_next.m_prev := c.m_prev;
   if m_controllerList = c then
      m_controllerList := c.m_next;

   Dec(m_controllerCount);
end;
{$ENDIF}

{$IFDEF COMPUTE_PHYSICS_TIME}
function GetRawReferenceTime: Double;
var
   counter: Int64;
begin
//   QueryPerformanceCounter(counter);
//   Result := counter / vCounterFrequency;
   counter := TStopwatch.GetTimeStamp;
   Result := counter / TStopwatch.Frequency;
end;
{$ENDIF}

procedure Tb2World.Step(timeStep: PhysicsFloat; velocityIterations,
   positionIterations: Int32; Draw: Boolean = False);
var
   step: Tb2TimeStep;
   {$IFDEF COMPUTE_PHYSICS_TIME}startTime, tmpTime: Double;{$ENDIF}
begin
   {$IFDEF COMPUTE_PHYSICS_TIME}
   FillChar(m_profile, SizeOf(m_profile), 0);
   startTime := GetRawReferenceTime;
   {$ENDIF}

   // If new fixtures were added, we need to find the new contacts.
   if m_flags and e_world_newFixture <> 0 then
   begin
      m_contactManager.FindNewContacts;
      m_flags := m_flags and (not e_world_newFixture);
   end;

   m_flags := m_flags or e_world_locked;

   step.dt := timeStep;
   step.velocityIterations := velocityIterations;
   step.positionIterations := positionIterations;
   if timeStep > 0.0 then
      step.inv_dt := 1.0 / timeStep
   else
      step.inv_dt := 0.0;

   step.dtRatio := m_inv_dt0 * timeStep;
   step.warmStarting := m_warmStarting;

   // Update contacts. This is where some contacts are destroyed.
   {$IFDEF COMPUTE_PHYSICS_TIME}
   tmpTime := GetRawReferenceTime;
   m_contactManager.Collide;
   m_profile.collide := GetRawReferenceTime - tmpTime;
   {$ELSE}
   m_contactManager.Collide;
   {$ENDIF}

   // Integrate velocities, solve velocity constraints, and integrate positions.
   if timeStep > 0.0 then
   begin
      if m_stepComplete then
      begin
         {$IFDEF COMPUTE_PHYSICS_TIME}
         tmpTime := GetRawReferenceTime;
         Solve(step);
         m_profile.solve := GetRawReferenceTime - tmpTime;
         {$ELSE}
         Solve(step);
         {$ENDIF}
      end;

      // Handle TOI events.
      if m_continuousPhysics then
      begin
         {$IFDEF COMPUTE_PHYSICS_TIME}
         tmpTime := GetRawReferenceTime;
         SolveTOI(step);
         m_profile.solveTOI := GetRawReferenceTime - tmpTime;
         {$ELSE}
         SolveTOI(step);
         {$ENDIF}
      end;

      m_inv_dt0 := step.inv_dt;
   end;

   if m_flags and e_world_clearForces <> 0 then
      ClearForces;

   m_flags := m_flags and (not e_world_locked);

   {$IFDEF COMPUTE_PHYSICS_TIME}
   m_profile.step := GetRawReferenceTime - startTime;
   {$ENDIF}

   if Draw then
      DrawDebugData;
end;

function Tb2World.GetProxyCount: Int32;
begin
   Result := m_contactManager.m_broadPhase.GetProxyCount;
end;

procedure Tb2World.SetGravity(const gravity: TVector2);
begin
   m_gravity := gravity;
end;

procedure Tb2World.WakeAllSleepingBodies;
var
   b: Tb2Body;
begin
   b := m_bodyList;
   while Assigned(b) do
   begin
      if b.IsActive and (b.m_type = b2_dynamicBody) and (not b.IsAwake) then
         b.SetAwake(True);
      b := b.m_next;
   end;
end;

function Tb2World.IsLocked: Boolean;
begin
   Result := (m_flags and e_world_locked) = e_world_locked;
end;

procedure Tb2World.SetAutoClearForces(flag: Boolean);
begin
   if flag then
      m_flags := m_flags or e_world_clearForces
	 else
      m_flags := m_flags and (not e_world_clearForces);
end;

function Tb2World.GetAutoClearForces: Boolean;
begin
   Result := (m_flags and e_world_clearForces) = e_world_clearForces;
end;

procedure Tb2World.ShiftOrigin(const newOrigin: TVector2);
var
	b: Tb2Body;
	j: Tb2Joint;
begin
  //b2Assert((m_flags & e_locked) == 0);
  if (m_flags and e_world_locked) = e_world_locked then
  Exit;

  b := m_bodyList;
  while Assigned(b) do
  begin
    {$IFDEF OP_OVERLOAD}
    b.m_xf.p.SubtractBy(newOrigin);
    b.m_sweep.c0.SubtractBy(newOrigin);
    b.m_sweep.c.SubtractBy(newOrigin);
    {$ELSE}
    SubtractBy(b.m_xf.p, newOrigin);
    SubtractBy(b.m_sweep.c0, newOrigin);
    SubtractBy(b.m_sweep.c, newOrigin);
    {$ENDIF}
    b := b.m_next;
  end;

  j := m_jointList;
  while Assigned(j) do
  begin
    j.ShiftOrigin(newOrigin);
    j := j.m_next;
  end;

  m_contactManager.m_broadPhase.ShiftOrigin(newOrigin);
end;

////////////////////////////////////////////////////
// Contact

{ Tb2Contact }

{$IFDEF OP_OVERLOAD}
procedure Tb2Contact.Update(listener: Tb2ContactListener);
var
   i, j: Integer;
   oldManifold: Tb2Manifold;
   touching, wasTouching, sensor: Boolean;
   bodyA, bodyB: Tb2Body;
   mp1, mp2: Pb2ManifoldPoint;
   id2key: UInt32;
begin
   oldManifold := m_manifold;
   m_flags := m_flags or e_contact_enabledFlag; // Re-enable this contact.

   //touching := False;
   wasTouching := (m_flags and e_contact_touchingFlag) = e_contact_touchingFlag;

   sensor := m_fixtureA.IsSensor or m_fixtureB.IsSensor;

   bodyA := m_fixtureA.m_body;
   bodyB := m_fixtureB.m_body;

   // Is this contact a sensor?
   if sensor then
   begin
      touching := b2TestOverlap(m_fixtureA.m_shape, m_fixtureB.m_shape,
         m_indexA, m_indexB, bodyA.m_xf, bodyB.m_xf);

      // Sensors don't generate manifolds.
      m_manifold.pointCount := 0;
   end
   else
   begin
      m_evaluateProc(@Self, m_manifold, m_fixtureA, m_fixtureB, bodyA.m_xf, bodyB.m_xf, True);
      touching := m_manifold.pointCount > 0;

      // Match old contact ids to new contact ids and copy the
      // stored impulses to warm start the solver.
      for i := 0 to m_manifold.pointCount - 1 do
      begin
         mp2 := @m_manifold.points[i];
         mp2^.normalImpulse := 0.0;
         mp2^.tangentImpulse := 0.0;
         id2key := mp2^.id.key;

         for j := 0 to oldManifold.pointCount - 1 do
         begin
            mp1 := @oldManifold.points[j];

            if mp1^.id.key = id2key then
            begin
               mp2^.normalImpulse := mp1^.normalImpulse;
               mp2^.tangentImpulse := mp1^.tangentImpulse;
               Break;
            end;
         end;
      end;

      if touching xor wasTouching then
      begin
         bodyA.SetAwake(True);
         bodyB.SetAwake(True);
      end;
   end;

   if touching then
      m_flags := m_flags or e_contact_touchingFlag
   else
      m_flags := m_flags and (not e_contact_touchingFlag);

   if Assigned(listener) then
   begin
      if (not wasTouching) and touching then
         listener.BeginContact(Self);

      if wasTouching and (not touching) then
         listener.EndContact(Self);

      if (not sensor) and touching then
         listener.PreSolve(Self, oldManifold);
   end;
end;

function Tb2Contact.GetManifold: Pb2Manifold;
begin
   Result := @m_manifold;
end;

procedure Tb2Contact.GetWorldManifold(var worldManifold: Tb2WorldManifold);
begin
   {$IFDEF OP_OVERLOAD}
   worldManifold.Initialize(m_manifold, m_fixtureA.m_body.m_xf,
      m_fixtureB.m_body.m_xf, m_fixtureA.m_shape.m_radius, m_fixtureB.m_shape.m_radius);
   {$ELSE}
   Initialize(worldManifold, m_manifold, m_fixtureA.m_body.m_xf,
      m_fixtureB.m_body.m_xf, m_fixtureA.m_shape.m_radius, m_fixtureB.m_shape.m_radius);
   {$ENDIF}
end;

procedure Tb2Contact.FlagForFiltering;
begin
   m_flags := m_flags or e_contact_filterFlag;
end;

function Tb2Contact.IsTouching: Boolean;
begin
   Result := (m_flags and e_contact_touchingFlag) = e_contact_touchingFlag;
end;

procedure Tb2Contact.ResetFriction;
begin
   m_friction := b2MixFriction(m_fixtureA.m_friction, m_fixtureB.m_friction);
end;

procedure Tb2Contact.ResetRestitution;
begin
   m_restitution := b2MixRestitution(m_fixtureA.m_restitution, m_fixtureB.m_restitution);
end;

procedure Tb2Contact.SetEnabled(flag: Boolean);
begin
   if flag then
      m_flags := m_flags or e_contact_enabledFlag
   else
      m_flags := m_flags and (not e_contact_enabledFlag);
end;

function Tb2Contact.IsEnabled: Boolean;
begin
   Result := (m_flags and e_contact_enabledFlag) = e_contact_enabledFlag;
end;
{$ENDIF}

{ Tb2ContactFilter }

function Tb2ContactFilter.ShouldCollide(fixtureA, fixtureB: Tb2Fixture): Boolean;
begin
   with fixtureA.m_filter do
      if (groupIndex = fixtureB.m_filter.groupIndex) and (groupIndex <> 0) then
         Result := groupIndex > 0
      else
         Result := ((maskBits and fixtureB.m_filter.categoryBits) <> 0) and
            ((categoryBits and fixtureB.m_filter.maskBits) <> 0);
end;

{ Tb2ContactListener }

procedure Tb2ContactListener.BeginContact(var contact: Tb2Contact);
begin
end;

procedure Tb2ContactListener.EndContact(var contact: Tb2Contact);
begin
end;

procedure Tb2ContactListener.PreSolve(var contact: Tb2Contact; const oldManifold: Tb2Manifold);
begin
end;

procedure Tb2ContactListener.PostSolve(var contact: Tb2Contact; const impulse: Tb2ContactImpulse);
begin
end;

{ Tb2ContactSolver }

destructor Tb2ContactSolver.Destroy;
begin
   if Assigned(m_positionConstraints) then
      FreeMemory(m_positionConstraints);
   if Assigned(m_velocityConstraints) then
      FreeMemory(m_velocityConstraints);
end;

procedure Tb2ContactSolver.Initialize(const step: Tb2TimeStep;
  positions: Tb2Positions; velocities: Tb2Velocities;
  contacts: TList; count: Int32);
var
   i, j: Integer;
   vc: Pb2ContactVelocityConstraint;
   pc: Pb2ContactPositionConstraint;
   cp: Pb2ManifoldPoint;
begin
   m_step := step;
   m_count := count;

   if Assigned(m_positionConstraints) then
      FreeMemory(m_positionConstraints);
   if Assigned(m_velocityConstraints) then
      FreeMemory(m_velocityConstraints);
   m_velocityConstraints := Pb2ContactVelocityConstraint(GetMemory(m_count * SizeOf(Tb2ContactVelocityConstraint)));
   m_positionConstraints := Pb2ContactPositionConstraint(GetMemory(m_count * SizeOf(Tb2ContactPositionConstraint)));

   m_positions := positions;
   m_velocities := velocities;
   m_contacts := contacts;

   // Initialize position independent portions of the constraints.
   for i := 0 to m_count - 1 do
      with Pb2Contact(m_contacts[i])^ do
      begin
         vc := m_velocityConstraints;
         Inc(vc, i);
         with vc^ do
         begin
            friction := m_friction;
            restitution := m_restitution;
            tangentSpeed := m_tangentSpeed;
            indexA := m_fixtureA.m_body.m_islandIndex;
            indexB := m_fixtureB.m_body.m_islandIndex;
            invMassA := m_fixtureA.m_body.m_invMass;
            invMassB := m_fixtureB.m_body.m_invMass;
            invIA := m_fixtureA.m_body.m_invI;
            invIB := m_fixtureB.m_body.m_invI;
            contactIndex := i;
            pointCount := m_manifold.pointCount;
            {$IFDEF OP_OVERLOAD}
            K.SetZero;
            normalMass.SetZero;
            {$ELSE}
            SetZero(K);
            SetZero(normalMass);
            {$ENDIF}
         end;

         pc := m_positionConstraints;
         Inc(pc, i);
         with pc^ do
         begin
            indexA := m_fixtureA.m_body.m_islandIndex;
            indexB := m_fixtureB.m_body.m_islandIndex;
            invMassA := m_fixtureA.m_body.m_invMass;
            invMassB := m_fixtureB.m_body.m_invMass;
            localCenterA := m_fixtureA.m_body.m_sweep.localCenter;
            localCenterB := m_fixtureB.m_body.m_sweep.localCenter;
            invIA := m_fixtureA.m_body.m_invI;
            invIB := m_fixtureB.m_body.m_invI;
            localNormal := m_manifold.localNormal;
            localPoint := m_manifold.localPoint;
            pointCount := m_manifold.pointCount;
            radiusA := m_fixtureA.m_shape.m_radius;
            radiusB := m_fixtureB.m_shape.m_radius;
            manifoldType := m_manifold.manifoldType;
         end;

         for j := 0 to m_manifold.pointCount - 1 do
         begin
            cp := @m_manifold.points[j];
            FillChar(vc.points[j], SizeOf(Tb2VelocityConstraintPoint), 0);
            with vc.points[j] do
            begin
               if m_step.warmStarting then
               begin
                  normalImpulse := step.dtRatio * cp^.normalImpulse;
                  tangentImpulse := step.dtRatio * cp^.tangentImpulse;
               end;
            end;

            pc^.localPoints[j] := cp^.localPoint;
         end;
      end;
end;

// Initialize position dependent portions of the velocity constraints.
procedure Tb2ContactSolver.InitializeVelocityConstraints;
const
   k_maxConditionNumber = 1000.0;
var
   i, j: Integer;
   vc: Pb2ContactVelocityConstraint;
   pc: Pb2ContactPositionConstraint;
   cA, cB, vA, vB: TVector2;
   aA, aB, wA, wB: PhysicsFloat;
   xfA, xfB: Tb2Transform;
   worldManifold: Tb2WorldManifold;
   manifold: Pb2Manifold;
   vcp, vcp1, vcp2: Pb2VelocityConstraintPoint;
   invMassSum, rnA, rnB, kNormal, kTangent, rtA, rtB, vRel: PhysicsFloat;
   k11, k12, k22, rn1A, rn1B, rn2A, rn2B: PhysicsFloat;
   tangent: TVector2;
begin
   for i := 0 to m_count - 1 do
   begin
      vc := m_velocityConstraints;
      Inc(vc, i);
      pc := m_positionConstraints;
      Inc(pc, i);

      with vc^ do
      begin
         cA := m_positions[indexA].c;
         aA := m_positions[indexA].a;
         vA := m_velocities[indexA].v;
         wA := m_velocities[indexA].w;

         cB := m_positions[indexB].c;
         aB := m_positions[indexB].a;
         vB := m_velocities[indexB].v;
         wB := m_velocities[indexB].w;

         //b2Assert(manifold->pointCount > 0);
         {$IFDEF OP_OVERLOAD}
         manifold := Pb2Contact(m_contacts[contactIndex])^.GetManifold;
         {$ELSE}
         manifold := GetManifold(Pb2Contact(m_contacts[contactIndex])^);
         {$ENDIF}

         {$IFDEF OP_OVERLOAD}
         xfA.q.SetAngle(aA);
         xfB.q.SetAngle(aB);
         xfA.p := cA - b2Mul(xfA.q, pc^.localCenterA);
         xfB.p := cB - b2Mul(xfB.q, pc^.localCenterB);
         worldManifold.Initialize(manifold^, xfA, xfB, pc^.radiusA, pc^.radiusB);
         {$ELSE}
         SetAngle(xfA.q, aA);
         SetAngle(xfB.q, aB);
         xfA.p := Subtract(cA, b2Mul(xfA.q, pc^.localCenterA));
         xfB.p := Subtract(cB, b2Mul(xfB.q, pc^.localCenterB));
         UPhysics2D.Initialize(worldManifold, manifold^, xfA, xfB, pc^.radiusA, pc^.radiusB);
         {$ENDIF}

         invMassSum := invMassA + invMassB;
         normal := worldManifold.normal;

         for j := 0 to pointCount - 1 do
         begin
            vcp := @points[j];

            with vcp^ do
            begin
               {$IFDEF OP_OVERLOAD}
               rA := worldManifold.points[j] - cA;
               rB := worldManifold.points[j] - cB;
               {$ELSE}
               rA := Subtract(worldManifold.points[j], cA);
               rB := Subtract(worldManifold.points[j], cB);
               {$ENDIF}
            end;

            rnA := b2Cross(vcp^.rA, normal);
            rnB := b2Cross(vcp^.rB, normal);
            kNormal := invMassSum + invIA * rnA * rnA + invIB * rnB * rnB;
            if kNormal > 0.0 then
               vcp^.normalMass := 1.0 / kNormal
            else
               vcp^.normalMass := 0.0;

            tangent := b2Cross(normal, 1.0);
            rtA := b2Cross(vcp^.rA, tangent);
            rtB := b2Cross(vcp^.rB, tangent);
            kTangent := invMassSum + invIA * rtA * rtA + invIB * rtB * rtB;
            if kTangent > 0.0 then
               vcp^.tangentMass := 1.0 / kTangent
            else
               vcp^.tangentMass := 0.0;

            // Setup a velocity bias for restitution.
            vcp^.velocityBias := 0.0;
            {$IFDEF OP_OVERLOAD}
            vRel := b2Dot(normal, vB + b2Cross(wB, vcp^.rB) - vA - b2Cross(wA, vcp^.rA));
            {$ELSE}
            vRel := b2Dot(normal, Subtract(Add(vB, b2Cross(wB, vcp^.rB)), Add(vA, b2Cross(wA, vcp^.rA))));
            {$ENDIF}
            if vRel < -b2_velocityThreshold then
               vcp^.velocityBias := -restitution * vRel;
         end;

         // If we have two points, then prepare the block solver.
         if pointCount = 2 then
         begin
            with points[0] do
            begin
               rn1A := b2Cross(rA, normal);
               rn1B := b2Cross(rB, normal);
            end;
            with points[1] do
            begin
               rn2A := b2Cross(rA, normal);
               rn2B := b2Cross(rB, normal);
            end;

            k11 := invMassSum + invIA * rn1A * rn1A + invIB * rn1B * rn1B;
            k22 := invMassSum + invIA * rn2A * rn2A + invIB * rn2B * rn2B;
            k12 := invMassSum + invIA * rn1A * rn2A + invIB * rn1B * rn2B;

            // Ensure a reasonable condition number.
            if k11 * k11 < k_maxConditionNumber * (k11 * k22 - k12 * k12) then
            begin
               // K is safe to invert.
               {$IFDEF OP_OVERLOAD}
               K.ex.SetValue(k11, k12);
               K.ey.SetValue(k12, k22);
               normalMass := K.GetInverse;
               {$ELSE}
               SetValue(K.ex, k11, k12);
               SetValue(K.ey, k12, k22);
               normalMass := GetInverse(K);
               {$ENDIF}
            end
            else
            begin
               // The constraints are redundant, just use one.
               // TODO_ERIN use deepest?
               pointCount := 1;
            end;
         end;
      end;
   end;
end;

procedure Tb2ContactSolver.WarmStart;
var
   i, j: Integer;
   vc: Pb2ContactVelocityConstraint;
   vcp: Pb2VelocityConstraintPoint;
   vA, vB, P, tangent: TVector2;
   wA, wB: PhysicsFloat;
begin
   // Warm start.
   for i := 0 to m_count - 1 do
   begin
      vc := m_velocityConstraints;
      Inc(vc, i);
      with vc^ do
      begin
         vA := m_velocities[indexA].v;
         vB := m_velocities[indexB].v;
         wA := m_velocities[indexA].w;
         wB := m_velocities[indexB].w;
         tangent := b2Cross(normal, 1.0);

         for j := 0 to pointCount - 1 do
         begin
            vcp := @points[j];
            {$IFDEF OP_OVERLOAD}
            P := vcp^.normalImpulse * normal + vcp^.tangentImpulse * tangent;
            {$ELSE}
            P := Add(Multiply(normal, vcp^.normalImpulse), Multiply(tangent, vcp^.tangentImpulse));
            {$ENDIF}
            wA := wA - invIA * b2Cross(vcp^.rA, P);
            wB := wB + invIB * b2Cross(vcp^.rB, P);

            {$IFDEF OP_OVERLOAD}
            vA.SubtractBy(invMassA * P);
            vB.AddBy(invMassB * P);
            {$ELSE}
            SubtractBy(vA, Multiply(P, invMassA));
            AddBy(vB, Multiply(P, invMassB));
            {$ENDIF}
         end;

         m_velocities[indexA].v := vA;
         m_velocities[indexA].w := wA;
         m_velocities[indexB].v := vB;
         m_velocities[indexB].w := wB;
      end;
   end;
end;

procedure Tb2ContactSolver.SolveVelocityConstraints;
var
   i, j: Integer;
   vc: Pb2ContactVelocityConstraint;
   vA, vB, dv, P, tangent: TVector2;
   wA, wB: PhysicsFloat;
   lambda, maxFriction, newImpulse: PhysicsFloat;
   cp1, cp2: Pb2VelocityConstraintPoint;
   a, b, dv1, dv2, x, d, P1, P2: TVector2;
   vn1, vn2, vt: PhysicsFloat;
begin
   for i := 0 to m_count - 1 do
   begin
      vc := m_velocityConstraints;
      Inc(vc, i);

      with vc^ do
      begin
         vA := m_velocities[indexA].v;
         vB := m_velocities[indexB].v;
         wA := m_velocities[indexA].w;
         wB := m_velocities[indexB].w;

         tangent := b2Cross(normal, 1.0);
         //b2Assert(pointCount == 1 || pointCount == 2);

         // Solve tangent constraints first because non-penetration is more important than friction.
         for j := 0 to pointCount - 1 do
            with points[j] do
            begin
               // Relative velocity at contact
               {$IFDEF OP_OVERLOAD}
               dv := vB + b2Cross(wB, rB) - vA - b2Cross(wA, rA);
               {$ELSE}
               dv := Subtract(Add(vB, b2Cross(wB, rB)), Add(vA, b2Cross(wA, rA)));
               {$ENDIF}

               // Compute tangent force
               vt := b2Dot(dv, tangent) - tangentSpeed;
               lambda := tangentMass * (-vt);

               // b2Clamp the accumulated force
               maxFriction := friction * normalImpulse;
               newImpulse := b2Clamp(tangentImpulse + lambda, -maxFriction, maxFriction);
               lambda := newImpulse - tangentImpulse;
               tangentImpulse := newImpulse;

               // Apply contact impulse
               {$IFDEF OP_OVERLOAD}
               P := lambda * tangent;
               vA.SubtractBy(invMassA * P);
               vB.AddBy(invMassB * P);
               {$ELSE}
               P := Multiply(tangent, lambda);
               SubtractBy(vA, Multiply(P, invMassA));
               AddBy(vB, Multiply(P, invMassB));
               {$ENDIF}
               wA := wA - invIA * b2Cross(rA, P);
               wB := wB + invIB * b2Cross(rB, P);
            end;

         // Solve normal constraints
         if pointCount = 1 then
         begin
            with points[0] do
            begin
               // Relative velocity at contact
               {$IFDEF OP_OVERLOAD}
               dv := vB + b2Cross(wB, rB) - vA - b2Cross(wA, rA);
               {$ELSE}
               dv := Subtract(Add(vB, b2Cross(wB, rB)), Add(vA, b2Cross(wA, rA)));
               {$ENDIF}

               // Compute normal impulse
               lambda := -normalMass * (b2Dot(dv, normal) - velocityBias);

               // b2Clamp the accumulated impulse
               newImpulse := b2Max(normalImpulse + lambda, 0.0);
               lambda := newImpulse - normalImpulse;
               normalImpulse := newImpulse;

               // Apply contact impulse
               {$IFDEF OP_OVERLOAD}
               P := lambda * normal;
               vA.SubtractBy(invMassA * P);
               vB.AddBy(invMassB * P);
               {$ELSE}
               P := Multiply(normal, lambda);
               SubtractBy(vA, Multiply(P, invMassA));
               AddBy(vB, Multiply(P, invMassB));
               {$ENDIF}
               wA := wA - invIA * b2Cross(rA, P);
               wB := wB + invIB * b2Cross(rB, P);
            end;
         end
         else
         begin
            // Block solver developed in collaboration with Dirk Gregorius (back in 01/07 on Box2D_Lite).
            // Build the mini LCP for this contact patch
            //
            // vn = A * x + b, vn >= 0, , vn >= 0, x >= 0 and vn_i * x_i = 0 with i = 1..2
            //
            // A = J * W * JT and J = ( -n, -r1 x n, n, r2 x n )
            // b = vn0 - velocityBias
            //
            // The system is solved using the "Total enumeration method" (s. Murty). The complementary constraint vn_i * x_i
            // implies that we must have in any solution either vn_i = 0 or x_i = 0. So for the 2D contact problem the cases
            // vn1 = 0 and vn2 = 0, x1 = 0 and x2 = 0, x1 = 0 and vn2 = 0, x2 = 0 and vn1 = 0 need to be tested. The first valid
            // solution that satisfies the problem is chosen.
            //
            // In order to account of the accumulated impulse 'a' (because of the iterative nature of the solver which only requires
            // that the accumulated impulse is clamped and not the incremental impulse) we change the impulse variable (x_i).
            //
            // Substitute:
            //
            // x = a + d
            //
            // a := old total impulse
            // x := new total impulse
            // d := incremental impulse
            //
            // For the current iteration we extend the formula for the incremental impulse
            // to compute the new total impulse:
            //
            // vn = A * d + b
            //    = A * (x - a) + b
            //    = A * x + b - A * a
            //    = A * x + b'
            // b' = b - A * a;

            cp1 := @points[0];
            cp2 := @points[1];

            SetValue(a, cp1^.normalImpulse, cp2^.normalImpulse);
            //b2Assert(a.x >= 0.0f && a.y >= 0.0f);

            // Relative velocity at contact
            {$IFDEF OP_OVERLOAD}
            dv1 := vB + b2Cross(wB, cp1.rB) - vA - b2Cross(wA, cp1.rA);
            dv2 := vB + b2Cross(wB, cp2.rB) - vA - b2Cross(wA, cp2.rA);
            {$ELSE}
            dv1 := Subtract(Add(vB, b2Cross(wB, cp1.rB)), Add(vA, b2Cross(wA, cp1.rA)));
            dv2 := Subtract(Add(vB, b2Cross(wB, cp2.rB)), Add(vA, b2Cross(wA, cp2.rA)));
            {$ENDIF}

            // Compute normal velocity
            vn1 := b2Dot(dv1, normal);
            vn2 := b2Dot(dv2, normal);

            // Compute b'
            b.x := vn1 - cp1.velocityBias;
            b.y := vn2 - cp2.velocityBias;
            {$IFDEF OP_OVERLOAD}
            b.SubtractBy(b2Mul(K, a));
            {$ELSE}
            SubtractBy(b, b2Mul(K, a));
            {$ENDIF}
            while True do
            begin
               //
               // Case 1: vn = 0
               //
               // 0 = A * x + b'
               //
               // Solve for x:
               //
               // x = - inv(A) * b'
               //
               {$IFDEF OP_OVERLOAD}
               x := -b2Mul(normalMass, b);
               {$ELSE}
               x := Negative(b2Mul(normalMass, b));
               {$ENDIF}

               if (x.x >= 0.0) and (x.y >= 0.0) then
               begin
                  // Get the incremental impulse
                  {$IFDEF OP_OVERLOAD}
                  d := x - a;
                  // Apply incremental impulse
                  P1 := d.x * normal;
                  P2 := d.y * normal;
                  vA.SubtractBy(invMassA * (P1 + P2));
                  vB.AddBy(invMassB * (P1 + P2));
                  {$ELSE}
                  d := Subtract(x, a);
                  // Apply incremental impulse
                  P1 := Multiply(normal, d.x);
                  P2 := Multiply(normal, d.y);
                  SubtractBy(vA, Multiply(Add(P1, P2), invMassA));
                  AddBy(vB, Multiply(Add(P1, P2), invMassB));
                  {$ENDIF}

                  wA := wA - (invIA * (b2Cross(cp1^.rA, P1) + b2Cross(cp2^.rA, P2)));
                  wB := wB + invIB * (b2Cross(cp1^.rB, P1) + b2Cross(cp2^.rB, P2));

                  // Accumulate
                  cp1^.normalImpulse := x.x;
                  cp2^.normalImpulse := x.y;
                  Break;
               end;

               //
               // Case 2: vn1 = 0 and x2 = 0
               //
               //   0 = a11 * x1 + a12 * 0 + b1'
               // vn2 = a21 * x1 + a22 * 0 + b2'
               //
               x.x := -cp1.normalMass * b.x;
               x.y := 0.0;
               //vn1 := 0.0;
               vn2 := K.ex.y * x.x + b.y;

               if (x.x >= 0.0) and (vn2 >= 0.0) then
               begin
                  // Get the incremental impulse
                  {$IFDEF OP_OVERLOAD}
                  d := x - a;
                  // Apply incremental impulse
                  P1 := d.x * normal;
                  P2 := d.y * normal;
                  vA.SubtractBy(invMassA * (P1 + P2));
                  vB.AddBy(invMassB * (P1 + P2));
                  {$ELSE}
                  d := Subtract(x, a);
                  // Apply incremental impulse
                  P1 := Multiply(normal, d.x);
                  P2 := Multiply(normal, d.y);
                  SubtractBy(vA, Multiply(Add(P1, P2), invMassA));
                  AddBy(vB, Multiply(Add(P1, P2), invMassB));
                  {$ENDIF}

                  wA := wA - (invIA * (b2Cross(cp1^.rA, P1) + b2Cross(cp2^.rA, P2)));
                  wB := wB + invIB * (b2Cross(cp1^.rB, P1) + b2Cross(cp2^.rB, P2));

                  // Accumulate
                  cp1^.normalImpulse := x.x;
                  cp2^.normalImpulse := x.y;
                  Break;
               end;

               //
               // Case 3: vn2 = 0 and x1 = 0
               //
               // vn1 = a11 * 0 + a12 * x2 + b1'
               //   0 = a21 * 0 + a22 * x2 + b2'
               //
               x.x := 0.0;
               x.y := -cp2.normalMass * b.y;
               vn1 := K.ey.x * x.y + b.x;
               //vn2 := 0.0;

               if (x.y >= 0.0) and (vn1 >= 0.0) then
               begin
                  // Resubstitute for the incremental impulse
                  {$IFDEF OP_OVERLOAD}
                  d := x - a;
                  // Apply incremental impulse
                  P1 := d.x * normal;
                  P2 := d.y * normal;
                  vA.SubtractBy(invMassA * (P1 + P2));
                  vB.AddBy(invMassB * (P1 + P2));
                  {$ELSE}
                  d := Subtract(x, a);
                  // Apply incremental impulse
                  P1 := Multiply(normal, d.x);
                  P2 := Multiply(normal, d.y);
                  SubtractBy(vA, Multiply(Add(P1, P2), invMassA));
                  AddBy(vB, Multiply(Add(P1, P2), invMassB));
                  {$ENDIF}

                  wA := wA - (invIA * (b2Cross(cp1^.rA, P1) + b2Cross(cp2^.rA, P2)));
                  wB := wB + invIB * (b2Cross(cp1^.rB, P1) + b2Cross(cp2^.rB, P2));

                  // Accumulate
                  cp1^.normalImpulse := x.x;
                  cp2^.normalImpulse := x.y;
                  Break;
               end;

               //
               // Case 4: x1 := 0 and x2 := 0
               //
               // vn1 := b1
               // vn2 := b2;
               x.x := 0.0;
               x.y := 0.0;
               vn1 := b.x;
               vn2 := b.y;

               if (vn1 >= 0.0) and (vn2 >= 0.0) then
               begin
                  // Resubstitute for the incremental impulse
                  {$IFDEF OP_OVERLOAD}
                  d := x - a;
                  // Apply incremental impulse
                  P1 := d.x * normal;
                  P2 := d.y * normal;
                  vA.SubtractBy(invMassA * (P1 + P2));
                  vB.AddBy(invMassB * (P1 + P2));
                  {$ELSE}
                  d := Subtract(x, a);
                  // Apply incremental impulse
                  P1 := Multiply(normal, d.x);
                  P2 := Multiply(normal, d.y);
                  SubtractBy(vA, Multiply(Add(P1, P2), invMassA));
                  AddBy(vB, Multiply(Add(P1, P2), invMassB));
                  {$ENDIF}

                  wA := wA - (invIA * (b2Cross(cp1^.rA, P1) + b2Cross(cp2^.rA, P2)));
                  wB := wB + invIB * (b2Cross(cp1^.rB, P1) + b2Cross(cp2^.rB, P2));

                  // Accumulate
                  cp1^.normalImpulse := x.x;
                  cp2^.normalImpulse := x.y;
                  Break;
               end;
               Break; // No solution, give up. This is hit sometimes, but it doesn't seem to matter.
            end;
         end;

         m_velocities[indexA].v := vA;
         m_velocities[indexA].w := wA;
         m_velocities[indexB].v := vB;
         m_velocities[indexB].w := wB;
      end;
   end;
end;

procedure Tb2ContactSolver.StoreImpulses;
var
   i, j: Integer;
   m: Pb2Manifold;
   vc: Pb2ContactVelocityConstraint;
begin
   for i := 0 to m_count - 1 do
   begin
      vc := m_velocityConstraints;
      Inc(vc, i);

      {$IFDEF OP_OVERLOAD}
      m := Pb2Contact(m_contacts[vc^.contactIndex])^.GetManifold;
      {$ELSE}
      m := GetManifold(Pb2Contact(m_contacts[vc^.contactIndex])^);
      {$ENDIF}
      for j := 0 to vc^.pointCount - 1 do
      begin
         m^.points[j].normalImpulse := vc^.points[j].normalImpulse;
         m^.points[j].tangentImpulse := vc^.points[j].tangentImpulse;
      end;
   end;
end;

type
   Tb2PositionSolverManifold = class
   public
      normal, point: TVector2;
      separation: PhysicsFloat;

      procedure Initialize(const pc: Tb2ContactPositionConstraint; const xfA, xfB: Tb2Transform; index: Int32);
   end;

{ Tb2PositionSolverManifold }

procedure Tb2PositionSolverManifold.Initialize(const pc: Tb2ContactPositionConstraint;
   const xfA, xfB: Tb2Transform; index: Int32);
var
   pointA, pointB, planePoint, clipPoint: TVector2;
begin
   //b2Assert(pc.pointCount > 0);

   case pc.manifoldType of
      e_manifold_circles:
         begin
            pointA := b2Mul(xfA, pc.localPoint);
            pointB := b2Mul(xfB, pc.localPoints[0]);
            {$IFDEF OP_OVERLOAD}
            normal := pointB - pointA;
            normal.Normalize;
            {$ELSE}
            normal := Subtract(pointB, pointA);
            Normalize(normal);
            {$ENDIF}

            point := b2MiddlePoint(pointA, pointB);
            {$IFDEF OP_OVERLOAD}
            separation := b2Dot(pointB - pointA, normal) - pc.radiusA - pc.radiusB;
            {$ELSE}
            separation := b2Dot(Subtract(pointB, pointA), normal) - pc.radiusA - pc.radiusB;
            {$ENDIF}
         end;
      e_manifold_faceA:
         begin
            normal := b2Mul(xfA.q, pc.localNormal);
            planePoint := b2Mul(xfA, pc.localPoint);
            clipPoint := b2Mul(xfB, pc.localPoints[index]);
            {$IFDEF OP_OVERLOAD}
            separation := b2Dot(clipPoint - planePoint, normal) - pc.radiusA - pc.radiusB;
            {$ELSE}
            separation := b2Dot(Subtract(clipPoint, planePoint), normal) - pc.radiusA - pc.radiusB;
            {$ENDIF}
            point := clipPoint;
         end;
      e_manifold_faceB:
         begin
            normal := b2Mul(xfB.q, pc.localNormal);
            planePoint := b2Mul(xfB, pc.localPoint);
            clipPoint := b2Mul(xfA, pc.localPoints[index]);
            {$IFDEF OP_OVERLOAD}
            separation := b2Dot(clipPoint - planePoint, normal) - pc.radiusA - pc.radiusB;
            normal := -normal; // Ensure normal points from A to B
            {$ELSE}
            separation := b2Dot(Subtract(clipPoint, planePoint), normal) - pc.radiusA - pc.radiusB;
            normal := Negative(normal); // Ensure normal points from A to B
            {$ENDIF}
            point := clipPoint;
         end;
   end;
end;

var
   position_solver_manifold: Tb2PositionSolverManifold;
function Tb2ContactSolver.SolvePositionConstraints: Boolean;
var
   i, j: Integer;
   minSeparation: PhysicsFloat;
   pc: Pb2ContactPositionConstraint;
   cA, cB: TVector2;
   aA, aB: PhysicsFloat;
   xfA, xfB: Tb2Transform;
   _normal, _point, P: TVector2;
   _separation, _c, rnA, rnB, _K, impulse: PhysicsFloat;
   rA, rB: TVector2;
begin
   minSeparation := 0.0;
   for i := 0 to m_count - 1 do
   begin
      pc := m_positionConstraints;
      Inc(pc, i);
      with pc^ do
      begin
         cA := m_positions[indexA].c;
         aA := m_positions[indexA].a;
         cB := m_positions[indexB].c;
         aB := m_positions[indexB].a;

         // Solve normal constraints
         for j := 0 to pointCount - 1 do
         begin
            with position_solver_manifold do
            begin
               {$IFDEF OP_OVERLOAD}
               xfA.q.SetAngle(aA);
               xfB.q.SetAngle(aB);
               xfA.p := cA - b2Mul(xfA.q, localCenterA);
               xfB.p := cB - b2Mul(xfB.q, localCenterB);
               {$ELSE}
               SetAngle(xfA.q, aA);
               SetAngle(xfB.q, aB);
               xfA.p := Subtract(cA, b2Mul(xfA.q, localCenterA));
               xfB.p := Subtract(cB, b2Mul(xfB.q, localCenterB));
               {$ENDIF}

               Initialize(pc^, xfA, xfB, j);
               _normal := normal;
               _point := point;
               _separation := separation;
            end;

            {$IFDEF OP_OVERLOAD}
            rA := _point - cA;
            rB := _point - cB;
            {$ELSE}
            rA := Subtract(_point, cA);
            rB := Subtract(_point, cB);
            {$ENDIF}

            // Track max constraint error.
            minSeparation := b2Min(minSeparation, _separation);

            // Prevent large corrections and allow slop.
            _c := b2Clamp(b2_baumgarte * (_separation + b2_linearSlop), -b2_maxLinearCorrection, 0.0);

            // Compute the effective mass.
            rnA := b2Cross(rA, _normal);
            rnB := b2Cross(rB, _normal);
            _K := invMassA + invMassB + invIA * rnA * rnA + invIB * rnB * rnB;

            // Compute normal impulse
            if _K > 0.0 then
               impulse := -_c / _K
            else
               impulse := 0.0;

            {$IFDEF OP_OVERLOAD}
            P := impulse * _normal;
            cA.SubtractBy(invMassA * P);
            cB.AddBy(invMassB * P);
            {$ELSE}
            P := Multiply(_normal, impulse);
            SubtractBy(cA, Multiply(P, invMassA));
            AddBy(cB, Multiply(P, invMassB));
            {$ENDIF}

            aA := aA - invIA * b2Cross(rA, P);
            aB := aB + invIB * b2Cross(rB, P);
         end;

         m_positions[indexA].c := cA;
         m_positions[indexA].a := aA;
         m_positions[indexB].c := cB;
         m_positions[indexB].a := aB;
      end;
   end;

   // We can't expect minSpeparation >= -b2_linearSlop because we don't
   // push the separation above -b2_linearSlop.
   Result := minSeparation >= -3.0 * b2_linearSlop;
end;

// Sequential position solver for position constraints.
function Tb2ContactSolver.SolveTOIPositionConstraints(toiIndexA, toiIndexB: Int32): Boolean;
var
   minSeparation: PhysicsFloat;
   i, j: Integer;
   pc: Pb2ContactPositionConstraint;
   cA, cB: TVector2;
   aA, aB: PhysicsFloat;
   mA, mB, iA, iB, _C, _K, separation, rnA, rnB, impulse: PhysicsFloat;
   xfA, xfB: Tb2Transform;
   normal, point, rA, rB, P: TVector2;
begin
   minSeparation := 0.0;
   for i := 0 to m_count - 1 do
   begin
      pc := m_positionConstraints;
      Inc(pc, i);
      with pc^ do
      begin
         mA := 0.0;
         iA := 0.0;
         if (indexA = toiIndexA) or (indexA = toiIndexB) then
         begin
            mA := invMassA;
            iA := invIA;
         end;

         mB := 0.0;
         iB := 0.0;
         if (indexB = toiIndexA) or (indexB = toiIndexB) then
         begin
            mB := invMassB;
            iB := invIB;
         end;

         cA := m_positions[indexA].c;
         aA := m_positions[indexA].a;
         cB := m_positions[indexB].c;
         aB := m_positions[indexB].a;

         // Solve normal constraints
         for j := 0 to pointCount - 1 do
         begin
            {$IFDEF OP_OVERLOAD}
            xfA.q.SetAngle(aA);
            xfB.q.SetAngle(aB);
            xfA.p := cA - b2Mul(xfA.q, localCenterA);
            xfB.p := cB - b2Mul(xfB.q, localCenterB);
            {$ELSE}
            SetAngle(xfA.q, aA);
            SetAngle(xfB.q, aB);
            xfA.p := Subtract(cA, b2Mul(xfA.q, localCenterA));
            xfB.p := Subtract(cB, b2Mul(xfB.q, localCenterB));
            {$ENDIF}

            position_solver_manifold.Initialize(pc^, xfA, xfB, j);
            normal := position_solver_manifold.normal;
            point := position_solver_manifold.point;
            separation := position_solver_manifold.separation;

            {$IFDEF OP_OVERLOAD}
            rA := point - cA;
            rB := point - cB;
            {$ELSE}
            rA := Subtract(point, cA);
            rB := Subtract(point, cB);
            {$ENDIF}

            // Track max constraint error.
            minSeparation := b2Min(minSeparation, separation);

            // Prevent large corrections and allow slop.
            _C := b2Clamp(b2_toiBaugarte * (separation + b2_linearSlop), -b2_maxLinearCorrection, 0.0);

            // Compute the effective mass.
            rnA := b2Cross(rA, normal);
            rnB := b2Cross(rB, normal);
            _K := mA + mB + iA * rnA * rnA + iB * rnB * rnB;

            // Compute normal impulse
            if _K > 0.0 then
               impulse := - _C / _K
            else
               impulse := 0.0;

            {$IFDEF OP_OVERLOAD}
            P := impulse * normal;
            cA.SubtractBy(mA * P);
            cB.AddBy(mB * P);
            {$ELSE}
            P := Multiply(normal, impulse);
            SubtractBy(cA, Multiply(P, mA));
            AddBy(cB, Multiply(P, mB));
            {$ENDIF}

            aA := aA - iA * b2Cross(rA, P);
            aB := aB + iB * b2Cross(rB, P);
         end;

         m_positions[indexA].c := cA;
         m_positions[indexA].a := aA;
         m_positions[indexB].c := cB;
         m_positions[indexB].a := aB;
      end;
   end;

   // We can't expect minSpeparation >= -b2_linearSlop because we don't
   // push the separation above -b2_linearSlop.
   Result := minSeparation >= -1.5 * b2_linearSlop;
end;

{ Tb2ContactManager }

constructor Tb2ContactManager.Create;
begin
   m_broadPhase := Tb2BroadPhase.Create;
   m_contactList := nil;
   m_contactCount := 0;
   m_contactFilter := b2_defaultFilter;
   m_contactListener := b2_defaultListener;
end;

destructor Tb2ContactManager.Destroy;
begin
   m_broadPhase.Free;
end;

procedure Tb2ContactManager.AddPair(proxyUserDataA, proxyUserDataB: Pointer);
var
   proxyA, proxyB: Pb2FixtureProxy;
   indexA, indexB: Int32;
   fixtureA, fixtureB: Tb2Fixture;
   bodyA, bodyB: Tb2Body;
   edge: Pb2ContactEdge;
   c: Pb2Contact;
begin
	 proxyA := Pb2FixtureProxy(proxyUserDataA);
	 proxyB := Pb2FixtureProxy(proxyUserDataB);

	 fixtureA := proxyA^.fixture;
	 fixtureB := proxyB^.fixture;

	 indexA := proxyA^.childIndex;
	 indexB := proxyB^.childIndex;

   bodyA := fixtureA.m_body;
   bodyB := fixtureB.m_body;

   if bodyA = bodyB then // Are the fixtures on the same body?
      Exit;

   // TODO_ERIN use a hash table to remove a potential bottleneck when both
   // bodies have a lot of contacts.
   // Does a contact already exist?
   edge := bodyB.GetContactList;
   while Assigned(edge) do
   begin
      if edge.other = bodyA then
      begin
         with edge^.contact^ do
         begin
            if ((m_fixtureA = fixtureA) and (m_fixtureB = fixtureB) and
               (m_indexA = indexA) and (m_indexB = indexB)) or
               ((m_fixtureA = fixtureB) and (m_fixtureB = fixtureA) and
               (m_indexA = indexB) and (m_indexB = indexA)) then // A contact already exists.
               Exit;
         end;
      end;

      edge := edge^.next;
   end;

   // Does a joint override collision? Is at least one body dynamic?
   if not bodyB.ShouldCollide(bodyA) then
      Exit;

   // Check user filtering.
   if Assigned(m_contactFilter) and (not m_contactFilter.ShouldCollide(fixtureA, fixtureB)) then
      Exit;

   // Call the factory.
   c := NewContact(fixtureA, fixtureB, indexA, indexB);
   if not Assigned(c) then
      Exit;

   with c^ do
   begin
      // Contact creation may swap fixtures.
      //indexA := m_indexA;
      //indexB := m_indexB;
      bodyA := m_fixtureA.m_body;
      bodyB := m_fixtureB.m_body;

      // Insert into the world.
      m_prev := nil;
      m_next := m_contactList;
      if Assigned(m_contactList) then
         m_contactList.m_prev := c;
      m_contactList := c;

      // Connect to island graph.
      // Connect to body A
      with m_nodeA do
      begin
         contact := c;
         other := bodyB;
         prev := nil;
         next := bodyA.m_contactList;
         if Assigned(bodyA.m_contactList) then
            bodyA.m_contactList.prev := @m_nodeA;
         bodyA.m_contactList := @m_nodeA;
      end;

      // Connect to body B
      with m_nodeB do
      begin
         contact := c;
         other := bodyA;
         prev := nil;
         next := bodyB.m_contactList;
         if Assigned(bodyB.m_contactList) then
            bodyB.m_contactList.prev := @m_nodeB;
         bodyB.m_contactList := @m_nodeB;
      end;
   end;

   // Wake up the bodies
   if (not fixtureA.IsSensor) and (not fixtureB.IsSensor) then
   begin
      bodyA.SetAwake(True);
      bodyB.SetAwake(True);
   end;

   Inc(m_contactCount);
end;

procedure Tb2ContactManager.FindNewContacts;
begin
   m_broadPhase.UpdatePairs(Self);
end;

procedure Tb2ContactManager.Destroy(pc: Pb2Contact);
var
   bodyA, bodyB: Tb2Body;
begin
   with pc^ do
   begin
      bodyA := m_fixtureA.m_body;
      bodyB := m_fixtureB.m_body;

      {$IFDEF OP_OVERLOAD}
      if Assigned(m_contactListener) and IsTouching then
      {$ELSE}
      if Assigned(m_contactListener) and IsTouching(pc^) then
      {$ENDIF}
         m_contactListener.EndContact(pc^);

      // Remove from the world.
      if Assigned(m_prev) then
         m_prev.m_next := m_next;

      if Assigned(m_next) then
         m_next.m_prev := m_prev;

      if pc = m_contactList then
         m_contactList := m_next;

      // Remove from body 1
      if Assigned(m_nodeA.prev) then
         m_nodeA.prev^.next := m_nodeA.next;

      if Assigned(m_nodeA.next) then
         m_nodeA.next^.prev := m_nodeA.prev;

      if (@m_nodeA = bodyA.m_contactList) then
         bodyA.m_contactList := m_nodeA.next;

      // Remove from body 2
      if Assigned(m_nodeB.prev) then
         m_nodeB.prev^.next := m_nodeB.next;

      if Assigned(m_nodeB.next) then
         m_nodeB.next^.prev := m_nodeB.prev;

      if (@m_nodeB = bodyB.m_contactList) then
         bodyB.m_contactList := m_nodeB.next;

      // Call the factory.
      if m_manifold.pointCount > 0 then
      begin
         m_fixtureA.m_body.SetAwake(True);
         m_fixtureB.m_body.SetAwake(True);
      end;
   end;
   FreeContact(pc);
   Dec(m_contactCount);
end;

procedure Tb2ContactManager.Collide;
var
   c, cNuke: Pb2Contact;
   bodyA, bodyB: Tb2Body;
   proxyIdA, proxyIdB: Int32;
   fixtureA, fixtureB: Tb2Fixture;
   overlap: Boolean;
   activeA, activeB: Boolean;
begin
   // Update awake contacts.
   c := m_contactList;
   while Assigned(c) do
   begin
      fixtureA := c^.m_fixtureA;
      fixtureB := c^.m_fixtureB;
      bodyA := fixtureA.m_body;
      bodyB := fixtureB.m_body;

      // Is this contact flagged for filtering?
      if (c^.m_flags and e_contact_filterFlag) <> 0 then
      begin
         // Should these bodies collide?
         if not bodyB.ShouldCollide(bodyA) then
         begin
            cNuke := c;
            c := cNuke^.m_next;
            Destroy(cNuke);
            Continue;
         end;

         // Check user filtering.
         if Assigned(m_contactFilter) and (not
            m_contactFilter.ShouldCollide(fixtureA, fixtureB)) then
         begin
            cNuke := c;
            c := cNuke^.m_next;
            Destroy(cNuke);
            Continue;
         end;

         // Clear the filtering flag.
         c^.m_flags := c^.m_flags and (not e_contact_filterFlag);
      end;

      activeA := bodyA.IsAwake and (bodyA.m_type <> b2_staticBody);
      activeB := bodyB.IsAwake and (bodyB.m_type <> b2_staticBody);

      // At least one body must be awake and it must be dynamic or kinematic.
      if (not activeA) and (not activeB) then
      begin
         c := c^.m_next;
         Continue;
      end;

		  proxyIdA := fixtureA.m_proxies[c^.m_indexA].proxyId;
		  proxyIdB := fixtureB.m_proxies[c^.m_indexB].proxyId;
      overlap := m_broadPhase.TestOverlap(proxyIdA, proxyIdB);

      // Here we destroy contacts that cease to overlap in the broad-phase.
      if not overlap then
      begin
         cNuke := c;
         c := cNuke^.m_next;
         Destroy(cNuke);
         Continue;
      end;

      // The contact persists.
      {$IFDEF OP_OVERLOAD}
      c^.Update(m_contactListener);
      {$ELSE}
      Update(c^, m_contactListener);
      {$ENDIF}
      c := c^.m_next;
   end;
end;

////////////////////////////////////////////////////
// Island
{ Tb2Island }

(*
Position Correction Notes
=========================
I tried the several algorithms for position correction of the 2D revolute joint.
I looked at these systems:
- simple pendulum (1m diameter sphere on massless 5m stick) with initial angular velocity of 100 rad/s.
- suspension bridge with 30 1m long planks of length 1m.
- multi-link chain with 30 1m long links.

Here are the algorithms:

Baumgarte - A fraction of the position error is added to the velocity error. There is no
separate position solver.

Pseudo Velocities - After the velocity solver and position integration,
the position error, Jacobian, and effective mass are recomputed. Then
the velocity constraints are solved with pseudo velocities and a fraction
of the position error is added to the pseudo velocity error. The pseudo
velocities are initialized to zero and there is no warm-starting. After
the position solver, the pseudo velocities are added to the positions.
This is also called the First Order World method or the Position LCP method.

Modified Nonlinear Gauss-Seidel (NGS) - Like Pseudo Velocities except the
position error is re-computed for each constraint and the positions are updated
after the constraint is solved. The radius vectors (aka Jacobians) are
re-computed too (otherwise the algorithm has horrible instability). The pseudo
velocity states are not needed because they are effectively zero at the beginning
of each iteration. Since we have the current position error, we allow the
iterations to terminate early if the error becomes smaller than b2_linearSlop.

Full NGS or just NGS - Like Modified NGS except the effective mass are re-computed
each time a constraint is solved.

Here are the results:
Baumgarte - this is the cheapest algorithm but it has some stability problems,
especially with the bridge. The chain links separate easily close to the root
and they jitter as they struggle to pull together. This is one of the most common
methods in the field. The big drawback is that the position correction artificially
affects the momentum, thus leading to instabilities and False bounce. I used a
bias factor of 0.2. A larger bias factor makes the bridge less stable, a smaller
factor makes joints and contacts more spongy.

Pseudo Velocities - the is more stable than the Baumgarte method. The bridge is
stable. However, joints still separate with large angular velocities. Drag the
simple pendulum in a circle quickly and the joint will separate. The chain separates
easily and does not recover. I used a bias factor of 0.2. A larger value lead to
the bridge collapsing when a heavy cube drops on it.

Modified NGS - this algorithm is better in some ways than Baumgarte and Pseudo
Velocities, but in other ways it is worse. The bridge and chain are much more
stable, but the simple pendulum goes unstable at high angular velocities.

Full NGS - stable in all tests. The joints display good stiffness. The bridge
still sags, but this is better than infinite forces.

Recommendations
Pseudo Velocities are not really worthwhile because the bridge and chain cannot
recover from joint separation. In other cases the benefit over Baumgarte is small.

Modified NGS is not a robust method for the revolute joint due to the violent
instability seen in the simple pendulum. Perhaps it is viable with other constraint
types, especially scalar constraints where the effectivprocedure Tb2Island.Solve(const step: Tb2TimeStep; const gravity: TVector2;
  correctPositions, allowSleep: Boolean);
begin

end;

procedure Tb2Island.SolveTOI(const subStep: Tb2TimeStep);
begin

end;
e mass is a scalar.

This leaves Baumgarte and Full NGS. Baumgarte has small, but manageable instabilities
and is very fast. I don't think we can escape Baumgarte, especially in highly
demanding cases where high constraint fidelity is not needed.

Full NGS is robust and easy on the eyes. I recommend this as an option for
higher fidelity simulation and certainly for suspension bridges and long chains.
Full NGS might be a good choice for ragdolls, especially motorized ragdolls where
joint separation can be problematic. The number of NGS iterations can be reduced
for better performance without harming robustness much.

Each joint in a can be handled differently in the position solver. So I recommend
a system where the user can select the algorithm on a per joint basis. I would
probably default to the slower Full NGS and let the user select the faster
Baumgarte method in performance critical scenarios.
*)

(*Cache Performance

The Box2D solvers are dominated by cache misses. Data structures are designed
to increase the number of cache hits. Much of misses are due to random access
to body data. The constraint structures are iterated over linearly, which leads
to few cache misses.

The bodies are not accessed during iteration. Instead read only data, such as
the mass values are stored with the constraints. The mutable data are the constraint
impulses and the bodies velocities/positions. The impulses are held inside the
constraint structures. The body velocities/positions are held in compact, temporary
arrays to increase the number of cache hits. Linear and angular velocity are
stored in a single array since multiple arrays lead to multiple misses.
*/

/*
2D Rotation

R = [cos(theta) -sin(theta)]
    [sin(theta) cos(theta) ]

thetaDot = omega

Let q1 = cos(theta), q2 = sin(theta).
R = [q1 -q2]
    [q2  q1]

q1Dot = -thetaDot * q2
q2Dot = thetaDot * q1

q1_new = q1_old - dt * w * q2
q2_new = q2_old + dt * w * q1
then normalize.

This might be faster than computing sin+cos.
However, we can compute sin+cos of the same angle fast.
*)

constructor Tb2Island.Create;
begin
   m_bodies := TList.Create;
   m_contacts := TList.Create;
   m_joints := TList.Create;
end;

destructor Tb2Island.Destroy;
begin
   // Warning: the order should reverse the constructor order.
   m_joints.Free;
   m_contacts.Free;
   m_bodies.Free;
end;

procedure Tb2Island.Reset(bodyCapacity, contactCapacity, jointCapacity: Int32;
   listener: Tb2ContactListener);
begin
   m_bodyCapacity := bodyCapacity;
   m_contactCapacity := contactCapacity;
   m_jointCapacity := jointCapacity;
   m_bodyCount := 0;
   m_contactCount := 0;
   m_jointCount := 0;

   m_listener := listener;

   m_bodies.Count := bodyCapacity;
   m_contacts.Count := contactCapacity;
   m_joints.Count := jointCapacity;

   SetLength(m_velocities, m_bodyCapacity);
   SetLength(m_positions, m_bodyCapacity);
end;

procedure Tb2Island.Clear;
begin
   m_bodyCount := 0;
   m_contactCount := 0;
   m_jointCount := 0;
end;

var
   island_solve_contact_solver: Tb2ContactSolver;
procedure Tb2Island.Solve(
    {$IFDEF COMPUTE_PHYSICS_TIME}var profile: Tb2Profile;{$ENDIF}
    const step: Tb2TimeStep;
    const gravity: TVector2; allowSleep: Boolean);
const
   linTolSqr = b2_linearSleepTolerance * b2_linearSleepTolerance;
   angTolSqr = b2_angularSleepTolerance * b2_angularSleepTolerance;
var
   i, j: Integer;
   h: PhysicsFloat;
   c, v: TVector2;
   a, w: PhysicsFloat;
   solverData: Tb2SolverData;
   translation: TVector2;
   rotation, minSleepTime: PhysicsFloat;
   positionSolved, contactsOkay, jointsOKay: Boolean;
   {$IFDEF COMPUTE_PHYSICS_TIME}time0: Double;{$ENDIF}
begin
   {$IFDEF COMPUTE_PHYSICS_TIME}
   time0 := GetRawReferenceTime();
   {$ENDIF}

   h := step.dt;

   // Integrate velocities and apply damping. Initialize the body state.
   for i := 0 to m_bodyCount - 1 do
      with Tb2Body(m_bodies[i]) do
      begin
         c := m_sweep.c;
         a := m_sweep.a;
         v := m_linearVelocity;
         w := m_angularVelocity;

         // Store positions for continuous collision.
         m_sweep.c0 := m_sweep.c;
         m_sweep.a0 := m_sweep.a;

         if m_type = b2_dynamicBody then
         begin
            // Integrate velocities.
            {$IFDEF OP_OVERLOAD}
            v.AddBy(h * (m_gravityScale * gravity + m_invMass * m_force));
            {$ELSE}
            AddBy(v, Multiply(UPhysics2DTypes.Add(Multiply(gravity, m_gravityScale), Multiply(m_force, m_invMass)), h));
            {$ENDIF}
            w := w + h * m_invI * m_torque;

            // Apply damping.
            // ODE: dv/dt + c * v = 0
            // Solution: v(t) = v0 * exp(-c * t)
            // Time step: v(t + dt) = v0 * exp(-c * (t + dt)) = v0 * exp(-c * t) * exp(-c * dt) = v * exp(-c * dt)
            // v2 = exp(-c * dt) * v1
            // Pade approximation:
            // v2 = v1 * 1 / (1 + c * dt)
            {$IFDEF OP_OVERLOAD}
            v.DivideBy(1.0 + h * m_linearDamping);
            {$ELSE}
            DivideBy(v, 1.0 + h * m_linearDamping);
            {$ENDIF}
            w := w / (1.0 + h * m_angularDamping);
         end;

         m_positions[i].c := c;
         m_positions[i].a := a;
         m_velocities[i].v := v;
         m_velocities[i].w := w;
      end;

   // Solver data
   solverData.step := step;
   solverData.positions := m_positions;
   solverData.velocities := m_velocities;

   // Initialize velocity constraints.
   island_solve_contact_solver.Initialize(step, m_positions, m_velocities, m_contacts, m_contactCount);
   island_solve_contact_solver.InitializeVelocityConstraints();

   if step.warmStarting then
      island_solve_contact_solver.WarmStart();

   for i := 0 to m_jointCount - 1 do
      Tb2Joint(m_joints[i]).InitVelocityConstraints(solverData);

   {$IFDEF COMPUTE_PHYSICS_TIME}
   profile.solveInit := GetRawReferenceTime() - time0;
   time0 := GetRawReferenceTime();
   {$ENDIF}

   // Solve velocity constraints.
   if (island_solve_contact_solver.m_count > 0) or (m_jointCount > 0) then
      for i := 0 to step.velocityIterations - 1 do
      begin
         for j := 0 to m_jointCount - 1 do
            Tb2Joint(m_joints[j]).SolveVelocityConstraints(solverData);
         island_solve_contact_solver.SolveVelocityConstraints;
      end;

   // Store impulses for warm starting
   island_solve_contact_solver.StoreImpulses();
   {$IFDEF COMPUTE_PHYSICS_TIME}
   profile.solveVelocity := GetRawReferenceTime() - time0;
   {$ENDIF}

   // Integrate positions
   for i := 0 to m_bodyCount - 1 do
   begin
      c := m_positions[i].c;
      a := m_positions[i].a;
      v := m_velocities[i].v;
      w := m_velocities[i].w;

      // Check for large velocities
      {$IFDEF OP_OVERLOAD}
      translation := h * v;
      {$ELSE}
      translation := Multiply(v, h);
      {$ENDIF}

      if b2Dot(translation, translation) > b2_maxTranslationSquared then
         {$IFDEF OP_OVERLOAD}
         v.MultiplyBy(b2_maxTranslation / translation.Length());
         {$ELSE}
         MultiplyBy(v, b2_maxTranslation / LengthVec(translation));
         {$ENDIF}

      rotation := h * w;
      if (rotation * rotation > b2_maxRotationSquared) then
         w := w * b2_maxRotation / Abs(rotation);

      // Integrate
      {$IFDEF OP_OVERLOAD}
      c.AddBy(h * v);
      {$ELSE}
      AddBy(c, Multiply(v, h));
      {$ENDIF}
      a := a + h * w;

      m_positions[i].c := c;
      m_positions[i].a := a;
      m_velocities[i].v := v;
      m_velocities[i].w := w;
   end;

   // Solve position constraints
   {$IFDEF COMPUTE_PHYSICS_TIME}
   time0 := GetRawReferenceTime();
   {$ENDIF}
   positionSolved := False;
   for i := 0 to step.positionIterations - 1 do
   begin
      contactsOkay := island_solve_contact_solver.SolvePositionConstraints();
      jointsOkay := True;

      for j := 0 to m_jointCount - 1 do
         jointsOkay := Tb2Joint(m_joints[j]).SolvePositionConstraints(solverData) and jointsOkay;

      if contactsOkay and jointsOkay then // Exit early if the position errors are small.
      begin
         positionSolved := True;
         Break;
      end;
   end;

   // Copy state buffers back to the bodies
   for i := 0 to m_bodyCount - 1 do
      with Tb2Body(m_bodies[i]) do
      begin
         m_sweep.c := m_positions[i].c;
         m_sweep.a := m_positions[i].a;
         m_linearVelocity := m_velocities[i].v;
         m_angularVelocity := m_velocities[i].w;
         SynchronizeTransform();
      end;

   {$IFDEF COMPUTE_PHYSICS_TIME}
   profile.solvePosition := GetRawReferenceTime() - time0;
   {$ENDIF}

   Report(island_solve_contact_solver.m_velocityConstraints);

   if allowSleep then
   begin
      minSleepTime := FLT_MAX;

      for i := 0 to m_bodyCount - 1 do
         with Tb2Body(m_bodies[i]) do
         begin
            if m_type = b2_staticBody then
               Continue;

            if ((m_flags and e_body_autoSleepFlag) = 0) or
               (m_angularVelocity * m_angularVelocity > angTolSqr) or
               (b2Dot(m_linearVelocity, m_linearVelocity) > linTolSqr) then
            begin
               m_sleepTime := 0.0;
               minSleepTime := 0.0;
            end
            else
            begin
               m_sleepTime := m_sleepTime + h;
               minSleepTime := b2Min(minSleepTime, m_sleepTime);
            end;
         end;

      if (minSleepTime >= b2_timeToSleep) and positionSolved then
         for i := 0 to m_bodyCount - 1 do
            Tb2Body(m_bodies[i]).SetAwake(False);
   end;
end;

procedure Tb2Island.SolveTOI(const subStep: Tb2TimeStep; toiIndexA, toiIndexB: Int32);
var
   i: Integer;
   h: PhysicsFloat;
   c, v: TVector2;
   a, w: PhysicsFloat;
   translation: TVector2;
   rotation: PhysicsFloat;
begin
	 //b2Assert(toiIndexA < m_bodyCount);
	 //b2Assert(toiIndexB < m_bodyCount);

   // Initialize the body state.
   for i := 0 to m_bodyCount - 1 do
      with Tb2Body(m_bodies[i]) do
      begin
         m_positions[i].c := m_sweep.c;
         m_positions[i].a := m_sweep.a;
         m_velocities[i].v := m_linearVelocity;
         m_velocities[i].w := m_angularVelocity;
      end;

   island_solve_contact_solver.Initialize(subStep, m_positions, m_velocities, m_contacts, m_contactCount);

   // Solve position constraints.
   for i := 0 to subStep.positionIterations - 1 do
   begin
      if island_solve_contact_solver.SolveTOIPositionConstraints(toiIndexA, toiIndexB) then
         Break;
   end;

   // Leap of faith to new safe state.
   Tb2Body(m_bodies[toiIndexA]).m_sweep.c0 := m_positions[toiIndexA].c;
   Tb2Body(m_bodies[toiIndexA]).m_sweep.a0 := m_positions[toiIndexA].a;
   Tb2Body(m_bodies[toiIndexB]).m_sweep.c0 := m_positions[toiIndexB].c;
   Tb2Body(m_bodies[toiIndexB]).m_sweep.a0 := m_positions[toiIndexB].a;

   // No warm starting is needed for TOI events because warm
   // starting impulses were applied in the discrete solver.
   island_solve_contact_solver.InitializeVelocityConstraints();

   // Solve velocity constraints.
   for i := 0 to subStep.velocityIterations - 1 do
      island_solve_contact_solver.SolveVelocityConstraints();

   // Don't store the TOI contact forces for warm starting
   // because they can be quite large.
   h := subStep.dt;

   // Integrate positions.
   for i := 0 to m_bodyCount - 1 do
   begin
      c := m_positions[i].c;
      a := m_positions[i].a;
      v := m_velocities[i].v;
      w := m_velocities[i].w;

      // Check for large velocities
      {$IFDEF OP_OVERLOAD}
      translation := h * v;
      {$ELSE}
      translation := Multiply(v, h);
      {$ENDIF}
      if b2Dot(translation, translation) > b2_maxTranslationSquared then
         {$IFDEF OP_OVERLOAD}
         v.MultiplyBy(b2_maxTranslation / translation.Length());
         {$ELSE}
         MultiplyBy(v, b2_maxTranslation / LengthVec(translation));
         {$ENDIF}

      rotation := h * w;
      if (rotation * rotation > b2_maxRotationSquared) then
         w := w * b2_maxRotation / Abs(rotation);

      // Integrate
      {$IFDEF OP_OVERLOAD}
      c.AddBy(h * v);
      {$ELSE}
      AddBy(c, Multiply(v, h));
      {$ENDIF}
      a := a + h * w;

      m_positions[i].c := c;
      m_positions[i].a := a;
      m_velocities[i].v := v;
      m_velocities[i].w := w;

      // Sync bodies
      with Tb2Body(m_bodies[i]) do
      begin
         m_sweep.c := c;
         m_sweep.a := a;
         m_linearVelocity := v;
         m_angularVelocity := w;
         SynchronizeTransform();
      end;
   end;

   Report(island_solve_contact_solver.m_velocityConstraints);
end;

procedure Tb2Island.Add(body: Tb2Body);
begin
	 //b2Assert(m_bodyCount < m_bodyCapacity);
   body.m_islandIndex := m_bodyCount;
   m_bodies[m_bodyCount] := body;
   Inc(m_bodyCount);
end;

procedure Tb2Island.Add(contact: Pb2Contact);
begin
	 //b2Assert(m_contactCount < m_contactCapacity);
   m_contacts[m_contactCount] := contact;
   Inc(m_contactCount);
end;

procedure Tb2Island.Add(joint: Tb2Joint);
begin
	 //b2Assert(m_jointCount < m_jointCapacity);
   m_joints[m_jointCount] := joint;
   Inc(m_jointCount);
end;

procedure Tb2Island.Report(constraints: Pb2ContactVelocityConstraint);
var
   i, j: Integer;
   c: Pb2Contact;
   cc: Pb2ContactVelocityConstraint;
   impulse: Tb2ContactImpulse;
begin
   if not Assigned(m_listener) then
      Exit;

   for i := 0 to m_contactCount - 1 do
   begin
      c := Pb2Contact(m_contacts[i]);
      cc := constraints;
      Inc(cc, i);

      impulse.count := cc^.pointCount;
      for j := 0 to cc^.pointCount - 1 do
      begin
         impulse.normalImpulses[j] := cc^.points[j].normalImpulse;
         impulse.tangentImpulses[j] := cc^.points[j].tangentImpulse;
      end;

      m_listener.PostSolve(c^, impulse);
   end;
end;

{ Tb2GrowableStack }

type
   /// This is a growable LIFO stack with an initial capacity of N.
   /// If the stack size exceeds the initial capacity, the heap is used
   /// to increase the size of the stack.
   Tb2GrowableStack = class
   private
      m_stack: PInt32;
      m_count, m_capacity: Int32;
   public
      constructor Create;
      destructor Destroy; override;

      procedure Reset;
      procedure Push(const element: Int32);
      function Pop: Int32;

      property GetCount: Int32 read m_count write m_count;
   end;

const
   _defaultStackCapacity = 256;
var
   _StackArray: array[0.._defaultStackCapacity - 1] of Int32;
   growable_stack: Tb2GrowableStack;

constructor Tb2GrowableStack.Create;
begin
   m_stack := @_StackArray[0];
	 m_count := 0;
	 m_capacity := _defaultStackCapacity;
end;

destructor Tb2GrowableStack.Destroy;
begin
   Reset;
   inherited;
end;

procedure Tb2GrowableStack.Reset;
begin
   if m_stack <> @_StackArray[0] then
      FreeMemory(m_stack);
   m_stack := @_StackArray[0];
	 m_count := 0;
	 m_capacity := _defaultStackCapacity;
end;

procedure Tb2GrowableStack.Push(const element: Int32);
var
   old: PInt32;
begin
   if m_count = m_capacity then
   begin
      old := m_stack;
      m_capacity := m_capacity * 2;
      m_stack := PInt32(GetMemory(m_capacity * sizeof(Int32)));
      Move(old^, m_stack^, m_count * sizeof(Int32));
      if old <> @_StackArray[0] then
         FreeMemory(old);
   end;

   PInt32(Integer(m_stack) + SizeOf(Int32) * m_count)^ := element;
   Inc(m_count);
end;

function Tb2GrowableStack.Pop: Int32;
begin
   //b2Assert(m_count > 0);
	 Dec(m_count);
	 Result := PInt32(Integer(m_stack) + SizeOf(Int32) * m_count)^;
end;

{ Tb2TreeNode }
{$IFDEF B2_USE_DYNAMIC_TREE}
{$IFDEF OP_OVERLOAD}
function Tb2TreeNode.IsLeaf: Boolean;
begin
   Result := child1 = b2_nullNode;
end;
{$ELSE}
function IsLeaf(const node: Tb2TreeNode): Boolean; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
begin
   Result := node.child1 = b2_nullNode;
end;
{$ENDIF}
{$ENDIF}

{ Tb2DynamicTree }

{$IFDEF B2_USE_DYNAMIC_TREE}

constructor Tb2DynamicTree.Create;
begin
   m_root := b2_nullNode;

   m_nodeCount := 0;
   SetCapacity(16);
   m_freeList := 0;

   m_path := 0;
   m_insertionCount := 0;
end;

procedure Tb2DynamicTree.SetCapacity(value: Int32);
var
   i: Integer;
begin
   SetLength(m_nodes, value);

   // Build a linked list for the free list.
   for i := m_nodeCount to value - 2 do
   begin
      m_nodes[i].next := i + 1;
      m_nodes[i].height := -1;
   end;

   m_nodes[value - 1].next := b2_nullNode;
   m_nodes[value - 1].height := -1;
   m_nodeCapacity := value;
end;

function Tb2DynamicTree.AllocateNode: Int32;
var
   nodeId: Int32;
begin
   // Expand the node pool as needed.
   if m_freeList = b2_nullNode then
   begin
      SetCapacity(m_nodeCapacity * 2);
      m_freeList := m_nodeCount;
   end;

   // Peel a node off the free list.
   nodeId := m_freeList;
   m_freeList := m_nodes[nodeId].next;
   m_nodes[nodeId].parent := b2_nullNode;
   m_nodes[nodeId].child1 := b2_nullNode;
   m_nodes[nodeId].child2 := b2_nullNode;
   m_nodes[nodeId].height := 0;
   m_nodes[nodeId].userData := nil;
   Inc(m_nodeCount);
   Result := nodeId;
end;

procedure Tb2DynamicTree.FreeNode(nodeId: Int32);
begin
   //b2Assert(0 <= nodeId && nodeId < m_nodeCapacity);
   //b2Assert(0 < m_nodeCount);
   m_nodes[nodeId].next := m_freeList;
   m_nodes[nodeId].height := -1;
   m_freeList := nodeId;
   Dec(m_nodeCount);
end;

procedure Tb2DynamicTree.InsertLeaf(leaf: Int32);
var
   combinedAABB, leafAABB, parentAABB, aabb: Tb2AABB;
   area, combinedArea, cost, inheritanceCost, cost1, cost2: PhysicsFloat;
   index, sibling, _child1, _child2, oldParent, newParent: Int32;
begin
   Inc(m_insertionCount);

   if m_root = b2_nullNode then
   begin
      m_root := leaf;
      m_nodes[m_root].parent := b2_nullNode;
      Exit;
   end;

   // Find the best sibling for this node
   leafAABB := m_nodes[leaf].aabb;
   index := m_root;
   {$IFDEF OP_OVERLOAD}
   while not m_nodes[index].IsLeaf do
   {$ELSE}
   while not IsLeaf(m_nodes[index]) do
   {$ENDIF}
   begin
      // Expand the node's AABB.
      with m_nodes[index] do
      begin
         {$IFDEF OP_OVERLOAD}
         area := aabb.GetPerimeter;
         {$ELSE}
         area := GetPerimeter(aabb);
         {$ENDIF}

         {$IFDEF OP_OVERLOAD}
         combinedAABB.Combine(aabb, leafAABB);
         combinedArea := combinedAABB.GetPerimeter;
         {$ELSE}
         Combine(combinedAABB, aabb, leafAABB);
         combinedArea := GetPerimeter(combinedAABB);
         {$ENDIF}

         _child1 := child1;
         _child2 := child2
      end;

      // Cost of creating a new parent for this node and the new leaf
      cost := 2.0 * combinedArea;

      // Minimum cost of pushing the leaf further down the tree
      inheritanceCost := 2.0 * (combinedArea - area);

      // Cost of descending into child1
      {$IFDEF OP_OVERLOAD}
      if m_nodes[_child1].IsLeaf then
      begin
         aabb.Combine(leafAABB, m_nodes[_child1].aabb);
         cost1 := aabb.GetPerimeter + inheritanceCost;
      end
      else
      begin
         aabb.Combine(leafAABB, m_nodes[_child1].aabb);
         cost1 := aabb.GetPerimeter - m_nodes[_child1].aabb.GetPerimeter + inheritanceCost;
      end;

      // Cost of descending into child2
      if m_nodes[_child2].IsLeaf then
      begin
         aabb.Combine(leafAABB, m_nodes[_child2].aabb);
         cost2 := aabb.GetPerimeter + inheritanceCost;
      end
      else
      begin
         aabb.Combine(leafAABB, m_nodes[_child2].aabb);
         cost2 := aabb.GetPerimeter - m_nodes[_child2].aabb.GetPerimeter + inheritanceCost;
      end;
      {$ELSE}
      if IsLeaf(m_nodes[_child1]) then
      begin
         Combine(aabb, leafAABB, m_nodes[_child1].aabb);
         cost1 := GetPerimeter(aabb) + inheritanceCost;
      end
      else
      begin
         Combine(aabb, leafAABB, m_nodes[_child1].aabb);
         cost1 := GetPerimeter(aabb) - GetPerimeter(m_nodes[_child1].aabb) + inheritanceCost;
      end;
      if IsLeaf(m_nodes[_child2]) then
      begin
         Combine(aabb, leafAABB, m_nodes[_child2].aabb);
         cost2 := GetPerimeter(aabb) + inheritanceCost;
      end
      else
      begin
         Combine(aabb, leafAABB, m_nodes[_child2].aabb);
         cost2 := GetPerimeter(aabb) - GetPerimeter(m_nodes[_child2].aabb) + inheritanceCost;
      end;
      {$ENDIF}

      // Descend according to the minimum cost.
      if (cost < cost1) and (cost < cost2) then
         Break;

      // Descend
      if cost1 < cost2 then
         index := _child1
      else
         index := _child2;
   end;

   // Create a new parent for the siblings.
   sibling := index;
   oldParent := m_nodes[sibling].parent;
   newParent := AllocateNode;
   with m_nodes[newParent] do
   begin
      parent := oldParent;
      userData := nil;
      {$IFDEF OP_OVERLOAD}
      aabb.Combine(leafAABB, m_nodes[sibling].aabb);
      {$ELSE}
      Combine(aabb, leafAABB, m_nodes[sibling].aabb);
      {$ENDIF}
      height := m_nodes[sibling].height + 1;
   end;

   if oldParent <> b2_nullNode then
   begin
      // The sibling was not the root.
      if m_nodes[oldParent].child1 = sibling then
         m_nodes[oldParent].child1 := newParent
      else
         m_nodes[oldParent].child2 := newParent;

      m_nodes[newParent].child1 := sibling;
      m_nodes[newParent].child2 := leaf;
      m_nodes[sibling].parent := newParent;
      m_nodes[leaf].parent := newParent;
   end
   else
   begin
      // The sibling was the root.
      m_nodes[newParent].child1 := sibling;
      m_nodes[newParent].child2 := leaf;
      m_nodes[sibling].parent := newParent;
      m_nodes[leaf].parent := newParent;
      m_root := newParent;
   end;

   // Walk back up the tree fixing heights and AABBs
   index := m_nodes[leaf].parent;
   while index <> b2_nullNode do
   begin
      index := Balance(index);
      with m_nodes[index] do
      begin
         //b2Assert(child1 != b2_nullNode);
         //b2Assert(child2 != b2_nullNode);

         height := 1 + b2Max(m_nodes[child1].height, m_nodes[child2].height);

         {$IFDEF OP_OVERLOAD}
         aabb.Combine(m_nodes[child1].aabb, m_nodes[child2].aabb);
         {$ELSE}
         Combine(aabb, m_nodes[child1].aabb, m_nodes[child2].aabb);
         {$ENDIF}

         index := parent;
      end;
   end;

   //Validate();
end;

procedure Tb2DynamicTree.RemoveLeaf(leaf: Int32);
var
   grandParent, _parent, index, sibling: Int32;
   oldAABB: Tb2AABB;
begin
   if leaf = m_root then
   begin
      m_root := b2_nullNode;
      Exit;
   end;

   _parent := m_nodes[leaf].parent;
   with m_nodes[_parent] do
   begin
      grandParent := parent;
      if child1 = leaf then
         sibling := child2
      else
         sibling := child1;
   end;

   if grandParent <> b2_nullNode then
   begin
      // Destroy parent and connect sibling to grandParent.
      with m_nodes[grandParent] do
         if child1 = _parent then
            child1 := sibling
         else
            child2 := sibling;
      m_nodes[sibling].parent := grandParent;
      FreeNode(_parent);

      // Adjust ancestor bounds.
      index := grandParent;
      while index <> b2_nullNode do
      begin
         index := Balance(index);
         with m_nodes[index] do
         begin
            oldAABB := aabb;
            {$IFDEF OP_OVERLOAD}
            aabb.Combine(m_nodes[child1].aabb, m_nodes[child2].aabb);
            {$ELSE}
            Combine(aabb, m_nodes[child1].aabb, m_nodes[child2].aabb);
            {$ENDIF}

            height := 1 + b2Max(m_nodes[child1].height, m_nodes[child2].height);
            index := parent;
         end;
      end;
   end
   else
   begin
      m_root := sibling;
      m_nodes[sibling].parent := b2_nullNode;
      FreeNode(_parent);
   end;

   //Validate();
end;

// Perform a left or right rotation if node A is imbalanced.
// Returns the new root index.
function Tb2DynamicTree.Balance(iA: Int32): Int32;
var
   A, B, C, D, E, F, G: Pb2TreeNode;
   iB, iC, _iF, iG, iD, iE, balance: Int32;
begin
   //b2Assert(iA != b2_nullNode);

   A := @m_nodes[iA];
   {$IFDEF OP_OVERLOAD}
   if A^.IsLeaf or (A^.height < 2) then
   {$ELSE}
   if IsLeaf(A^) or (A^.height < 2) then
   {$ENDIF}
   begin
      Result := iA;
      Exit;
   end;

   iB := A^.child1;
   iC := A^.child2;
   //b2Assert(0 <= iB && iB < m_nodeCapacity);
   //b2Assert(0 <= iC && iC < m_nodeCapacity);

   B := @m_nodes[iB];
   C := @m_nodes[iC];

   balance := C^.height - B^.height;

   // Rotate C up
   if balance > 1 then
   begin
      _iF := C^.child1;
      iG := C^.child2;
      F := @m_nodes[_iF];
      G := @m_nodes[iG];
      //b2Assert(0 <= iF && iF < m_nodeCapacity);
      //b2Assert(0 <= iG && iG < m_nodeCapacity);

      // Swap A and C
      C^.child1 := iA;
      C^.parent := A^.parent;
      A^.parent := iC;

      // A's old parent should point to C
      if C^.parent <> b2_nullNode then
      begin
         if m_nodes[C^.parent].child1 = iA then
            m_nodes[C^.parent].child1 := iC
         else
         begin
            //b2Assert(m_nodes[C->parent].child2 == iA);
            m_nodes[C^.parent].child2 := iC;
         end;
      end
      else
         m_root := iC;

      // Rotate
      if F^.height > G^.height then
      begin
         C^.child2 := _iF;
         A^.child2 := iG;
         G^.parent := iA;
         {$IFDEF OP_OVERLOAD}
         A^.aabb.Combine(B^.aabb, G^.aabb);
         C^.aabb.Combine(A^.aabb, F^.aabb);
         {$ELSE}
         Combine(A^.aabb, B^.aabb, G^.aabb);
         Combine(C^.aabb, A^.aabb, F^.aabb);
         {$ENDIF}

         A^.height := 1 + b2Max(B^.height, G^.height);
         C^.height := 1 + b2Max(A^.height, F^.height);
      end
      else
      begin
         C^.child2 := iG;
         A^.child2 := _iF;
         F^.parent := iA;
         {$IFDEF OP_OVERLOAD}
         A^.aabb.Combine(B^.aabb, F^.aabb);
         C^.aabb.Combine(A^.aabb, G^.aabb);
         {$ELSE}
         Combine(A^.aabb, B^.aabb, F^.aabb);
         Combine(C^.aabb, A^.aabb, G^.aabb);
         {$ENDIF}

         A^.height := 1 + b2Max(B^.height, F^.height);
         C^.height := 1 + b2Max(A^.height, G^.height);
      end;

      Result := iC;
      Exit;
   end;

   // Rotate B up
   if balance < -1 then
   begin
      iD := B^.child1;
      iE := B^.child2;
      D := @m_nodes[iD];
      E := @m_nodes[iE];
      //b2Assert(0 <= iD && iD < m_nodeCapacity);
      //b2Assert(0 <= iE && iE < m_nodeCapacity);

      // Swap A and B
      B^.child1 := iA;
      B^.parent := A^.parent;
      A^.parent := iB;

      // A's old parent should point to B
      if B^.parent <> b2_nullNode then
      begin
         if m_nodes[B^.parent].child1 = iA then
            m_nodes[B^.parent].child1 := iB
         else
         begin
            //b2Assert(m_nodes[B->parent].child2 == iA);
            m_nodes[B^.parent].child2 := iB;
         end;
      end
      else
         m_root := iB;

      // Rotate
      if D^.height > E^.height then
      begin
         B^.child2 := iD;
         A^.child1 := iE;
         E^.parent := iA;

         {$IFDEF OP_OVERLOAD}
         A^.aabb.Combine(C^.aabb, E^.aabb);
         B^.aabb.Combine(A^.aabb, D^.aabb);
         {$ELSE}
         Combine(A^.aabb, C^.aabb, E^.aabb);
         Combine(B^.aabb, A^.aabb, D^.aabb);
         {$ENDIF}

         A^.height := 1 + b2Max(C^.height, E^.height);
         B^.height := 1 + b2Max(A^.height, D^.height);
      end
      else
      begin
         B^.child2 := iE;
         A^.child1 := iD;
         D^.parent := iA;

         {$IFDEF OP_OVERLOAD}
         A^.aabb.Combine(C^.aabb, D^.aabb);
         B^.aabb.Combine(A^.aabb, E^.aabb);
         {$ELSE}
         Combine(A^.aabb, C^.aabb, D^.aabb);
         Combine(B^.aabb, A^.aabb, E^.aabb);
         {$ENDIF}

         A^.height := 1 + b2Max(C^.height, D^.height);
         B^.height := 1 + b2Max(A^.height, E^.height);
      end;

      Result := iB;
      Exit;
   end;

   Result := iA;
end;

function Tb2DynamicTree.ComputeHeight(nodeId: Int32): Int32;
var
   height1, height2: Int32;
begin
	 //b2Assert(0 <= nodeId && nodeId < m_nodeCapacity);
   with m_nodes[nodeId] do
   begin
      {$IFDEF OP_OVERLOAD}
      if IsLeaf then
      {$ELSE}
      if IsLeaf(m_nodes[nodeId]) then
      {$ENDIF}
      begin
         Result := 0;
         Exit;
      end;

      height1 := ComputeHeight(child1);
      height2 := ComputeHeight(child2);
   end;
   Result := 1 + b2Max(height1, height2);
end;

procedure Tb2DynamicTree.ValidateStructure(index: Int32);
begin
   if index = b2_nullNode then
      Exit;

   //if index = m_root then
      //b2Assert(m_nodes[index].parent == b2_nullNode);

   {$IFDEF OP_OVERLOAD}
   if m_nodes[index].IsLeaf then
   {$ELSE}
   if IsLeaf(m_nodes[index]) then
   {$ENDIF}
   begin
      //b2Assert(child1 == b2_nullNode);
      //b2Assert(child2 == b2_nullNode);
      //b2Assert(node->height == 0);
      Exit;
   end;

   //b2Assert(0 <= child1 && child1 < m_nodeCapacity);
   //b2Assert(0 <= child2 && child2 < m_nodeCapacity);

   //b2Assert(m_nodes[child1].parent == index);
   //b2Assert(m_nodes[child2].parent == index);

   ValidateStructure(m_nodes[index].child1);
   ValidateStructure(m_nodes[index].child2);
end;

procedure Tb2DynamicTree.ValidateMetrics(index: Int32);
var
   node: Pb2TreeNode;
   child1, child2, height1, height2, height: Int32;
   aabb: Tb2AABB;
begin
   if index = b2_nullNode then
      Exit;

   node := @m_nodes[index];

   child1 := node^.child1;
   child2 := node^.child2;

   {$IFDEF OP_OVERLOAD}
   if node^.IsLeaf then
   {$ELSE}
   if IsLeaf(node^) then
   {$ENDIF}
   begin
      //b2Assert(child1 == b2_nullNode);
      //b2Assert(child2 == b2_nullNode);
      //b2Assert(node->height == 0);
      Exit;
   end;

   //b2Assert(0 <= child1 && child1 < m_nodeCapacity);
   //b2Assert(0 <= child2 && child2 < m_nodeCapacity);

   height1 := m_nodes[child1].height;
   height2 := m_nodes[child2].height;
   height := 1 + b2Max(height1, height2);
   //b2Assert(node->height == height);

   {$IFDEF OP_OVERLOAD}
   aabb.Combine(m_nodes[child1].aabb, m_nodes[child2].aabb);
   {$ELSE}
   Combine(aabb, m_nodes[child1].aabb, m_nodes[child2].aabb);
   {$ENDIF}

   //b2Assert(aabb.lowerBound == node->aabb.lowerBound);
   //b2Assert(aabb.upperBound == node->aabb.upperBound);

   ValidateMetrics(child1);
   ValidateMetrics(child2);
end;

function Tb2DynamicTree.ComputeHeight: Int32;
begin
   Result := ComputeHeight(m_root);
end;

function Tb2DynamicTree.CreateProxy(const _aabb: Tb2AABB;
   _userData: Pointer): Int32;
const
   _r: TVector2 = (X: b2_aabbExtension; Y: b2_aabbExtension);
var
   proxyId: Int32;
begin
   proxyId := AllocateNode;

   // Fatten the aabb.
   with m_nodes[proxyId], aabb do
   begin
      {$IFDEF OP_OVERLOAD}
      lowerBound := _aabb.lowerBound - _r;
      upperBound := _aabb.upperBound + _r;
      {$ELSE}
      lowerBound := Subtract(_aabb.lowerBound, _r);
      upperBound := Add(_aabb.upperBound, _r);
      {$ENDIF}
      userData := _userData;
      height := 0;
   end;

   InsertLeaf(proxyId);
   Result := proxyId;
end;

procedure Tb2DynamicTree.DestroyProxy(proxyId: Int32);
begin
   //b2Assert(0 <= proxyId && proxyId < m_nodeCapacity);
   //b2Assert(m_nodes[proxyId].IsLeaf());

   RemoveLeaf(proxyId);
   FreeNode(proxyId);
end;

function Tb2DynamicTree.MoveProxy(proxyId: Int32; const aabb: Tb2AABB;
   const displacement: TVector2): Boolean;
const
   _r: TVector2 = (X: b2_aabbExtension; Y: b2_aabbExtension);
var
   b: Tb2AABB;
   d: TVector2;
begin
   //b2Assert(0 <= proxyId && proxyId < m_nodeCapacity);
   //b2Assert(m_nodes[proxyId].IsLeaf());

   if {$IFDEF OP_OVERLOAD}m_nodes[proxyId].aabb.Contains(aabb)
      {$ELSE}Contains(m_nodes[proxyId].aabb, aabb){$ENDIF} then
   begin
      Result := False;
      Exit;
   end;

   RemoveLeaf(proxyId);

   // Extend AABB.
   b := aabb;
   {$IFDEF OP_OVERLOAD}
   b.lowerBound.SubtractBy(_r);
   b.upperBound.AddBy(_r);
   {$ELSE}
   SubtractBy(b.lowerBound, _r);
   AddBy(b.upperBound, _r);
   {$ENDIF}

   // Predict AABB displacement.
   {$IFDEF OP_OVERLOAD}
   d := b2_aabbMultiplier * displacement;
   {$ELSE}
   d := Multiply(displacement, b2_aabbMultiplier);
   {$ENDIF}

   with b do
   begin
      if d.x < 0.0 then
         lowerBound.x := lowerBound.x + d.x
      else
         upperBound.x := upperBound.x + d.x;

      if d.y < 0.0 then
         lowerBound.y := lowerBound.y + d.y
      else
         upperBound.y := upperBound.y + d.y;
   end;

   m_nodes[proxyId].aabb := b;

   InsertLeaf(proxyId);
   Result := True;
end;

function Tb2DynamicTree.GetUserData(proxyId: Int32): Pointer;
begin
   Result := m_nodes[proxyId].userData;
end;

function Tb2DynamicTree.GetFatAABB(proxyId: Int32): Pb2AABB;
begin
   Result := @m_nodes[proxyId].aabb;
end;

procedure Tb2DynamicTree.Query(callback: Tb2GenericCallBackWrapper; const _aabb: Tb2AABB);
var
   nodeId: Int32;
begin
   growable_stack.Reset;
   growable_stack.Push(m_root);

   while growable_stack.GetCount > 0 do
   begin
      nodeId := growable_stack.Pop;
      if nodeId = b2_nullNode then
         Continue;

      with m_nodes[nodeId] do
         if b2TestOverlap(aabb, _aabb) then
         begin
            {$IFDEF OP_OVERLOAD}
            if IsLeaf then
            {$ELSE}
            if IsLeaf(m_nodes[nodeId]) then
            {$ENDIF}
            begin
               if not callback.QueryCallback(nodeId) then
                  Exit;
            end
            else
            begin
				       growable_stack.Push(child1);
				       growable_stack.Push(child2);
            end;
         end;
   end;
end;

procedure Tb2DynamicTree.RayCast(callback: Tb2GenericCallBackWrapper;
   const input: Tb2RayCastInput);
var
   maxFraction, value: PhysicsFloat;
   r, t, v, abs_v, c, h: TVector2;
   segmentAABB: Tb2AABB;
   nodeId: Int32;
   subInput: Tb2RayCastInput;
begin
   {$IFDEF OP_OVERLOAD}
   r := input.p2 - input.p1;
   //b2Assert(r.LengthSquared() > 0.0f);
   r.Normalize;
   {$ELSE}
   r := Subtract(input.p2, input.p1);
   //b2Assert(r.LengthSquared() > 0.0f);
   Normalize(r);
   {$ENDIF}

   // v is perpendicular to the segment.
   v := b2Cross(1.0, r);
   abs_v := b2Abs(v);

   // Separating axis for segment (Gino, p80).
   // |dot(v, p1 - c)| > dot(|v|, h)

   maxFraction := input.maxFraction;

   // Build a bounding box for the segment.
   {$IFDEF OP_OVERLOAD}
   t := input.p1 + maxFraction * (input.p2 - input.p1);
   {$ELSE}
   t := Add(input.p1, Multiply(Subtract(input.p2, input.p1), maxFraction));
   {$ENDIF}
   segmentAABB.lowerBound := b2Min(input.p1, t);
   segmentAABB.upperBound := b2Max(input.p1, t);

   growable_stack.Reset;
   growable_stack.Push(m_root);

   while growable_stack.GetCount > 0 do
   begin
      nodeId := growable_stack.Pop;
      if nodeId = b2_nullNode then
         Continue;

      with m_nodes[nodeId] do
      begin
         if not b2TestOverlap(aabb, segmentAABB) then
            Continue;

         // Separating axis for segment (Gino, p80).
         // |dot(v, p1 - c)| > dot(|v|, h)
         {$IFDEF OP_OVERLOAD}
         c := aabb.GetCenter;
         h := aabb.GetExtents;
         if Abs(b2Dot(v, input.p1 - c)) - b2Dot(abs_v, h) > 0.0 then
            Continue;
         {$ELSE}
         c := GetCenter(aabb);
         h := GetExtents(aabb);
         if Abs(b2Dot(v, Subtract(input.p1, c))) - b2Dot(abs_v, h) > 0.0 then
            Continue;
         {$ENDIF}

         {$IFDEF OP_OVERLOAD}
         if IsLeaf then
         {$ELSE}
         if IsLeaf(m_nodes[nodeId]) then
         {$ENDIF}
         begin
            subInput.p1 := input.p1;
            subInput.p2 := input.p2;
            subInput.maxFraction := maxFraction;

            value := callback.RayCastCallback(subInput, nodeId);

            if value = 0.0 then
               Exit; // The client has terminated the ray cast.

            if value > 0.0 then
            begin
               maxFraction := value;
               // Update segment bounding box.
               {$IFDEF OP_OVERLOAD}
               t := input.p1 + maxFraction * (input.p2 - input.p1);
               {$ELSE}
               t := Add(input.p1, Multiply(Subtract(input.p2, input.p1), maxFraction));
               {$ENDIF}
               segmentAABB.lowerBound := b2Min(input.p1, t);
               segmentAABB.upperBound := b2Max(input.p1, t);
            end;
         end
         else
         begin
 			      growable_stack.Push(child1);
			      growable_stack.Push(child2);
         end;
      end;
   end;
end;

procedure Tb2DynamicTree.Validate;
var
   freeCount, freeIndex: Int32;
begin
   ValidateStructure(m_root);
   ValidateMetrics(m_root);

   freeCount := 0;
   freeIndex := m_freeList;
   while freeIndex <> b2_nullNode do
   begin
      //b2Assert(0 <= freeIndex && freeIndex < m_nodeCapacity);
      freeIndex := m_nodes[freeIndex].next;
      Inc(freeCount);
   end;

   //b2Assert(GetHeight() == ComputeHeight());
   //b2Assert(m_nodeCount + freeCount == m_nodeCapacity);
end;

function Tb2DynamicTree.GetHeight: Int32;
begin
   if m_root = b2_nullNode then
      Result := 0
   else
      Result := m_nodes[m_root].height;
end;

function Tb2DynamicTree.GetMaxBalance: Int32;
var
   i: Integer;
   child1, child2, balance: Int32;
   node: Pb2TreeNode;
begin
   Result := 0;
   for i := 0 to m_nodeCapacity - 1 do
   begin
      node := @m_nodes[i];
      if node^.height <= 1 then
         Continue;

      //b2Assert(node->IsLeaf() == false);

      child1 := node^.child1;
      child2 := node^.child2;
      balance := Abs(m_nodes[child2].height - m_nodes[child1].height);
      Result := b2Max(Result, balance);
   end;
end;

function Tb2DynamicTree.GetAreaRatio: PhysicsFloat;
var
   i: Integer;
   root, node: Pb2TreeNode;
   rootArea, totalArea: PhysicsFloat;
begin
   if m_root = b2_nullNode then
      Result := 0.0
   else
   begin
      root := @m_nodes[m_root];
      {$IFDEF OP_OVERLOAD}
      rootArea := root^.aabb.GetPerimeter();
      {$ELSE}
      rootArea := GetPerimeter(root^.aabb);
      {$ENDIF}

      totalArea := 0.0;
      for i := 0 to m_nodeCapacity - 1 do
      begin
         node := @m_nodes[i];
         if node^.height < 0 then
            Continue; // Free node in pool

         {$IFDEF OP_OVERLOAD}
         totalArea := totalArea + node^.aabb.GetPerimeter();
         {$ELSE}
         totalArea := totalArea + GetPerimeter(node^.aabb);
         {$ENDIF}
      end;

      Result := totalArea / rootArea;
   end;
end;

procedure Tb2DynamicTree.RebuildBottomUp;
var
   i, j: Integer;
   count, iMin, jMin, index1, index2, parentIndex: Int32;
   minCost, cost: PhysicsFloat;
   nodes: array of Int32;
   aabbi, aabbj, b: Tb2AABB;
   child1, child2, parent: Pb2TreeNode;
begin
   SetLength(nodes, m_nodeCount);
   count := 0;

   // Build array of leaves. Free the rest.
   for i := 0 to m_nodeCapacity - 1 do
   begin
      if m_nodes[i].height < 0 then
         Continue; // free node in pool

      {$IFDEF OP_OVERLOAD}
      if m_nodes[i].IsLeaf then
      {$ELSE}
      if IsLeaf(m_nodes[i]) then
      {$ENDIF}
      begin
         m_nodes[i].parent := b2_nullNode;
         nodes[count] := i;
         Inc(count);
      end
      else
         FreeNode(i);
   end;

   while count > 1 do
   begin
      minCost := FLT_MAX;
      iMin := -1;
      jMin := -1;

      for i := 0 to count - 1 do
      begin
         aabbi := m_nodes[nodes[i]].aabb;

         for j := i + 1 to count - 1 do
         begin
            aabbj := m_nodes[nodes[j]].aabb;
            {$IFDEF OP_OVERLOAD}
            b.Combine(aabbi, aabbj);
            cost := b.GetPerimeter();
            {$ELSE}
            Combine(b, aabbi, aabbj);
            cost := GetPerimeter(b);
            {$ENDIF}
            if cost < minCost then
            begin
               iMin := i;
               jMin := j;
               minCost := cost;
            end;
         end;
      end;

      index1 := nodes[iMin];
      index2 := nodes[jMin];
      child1 := @m_nodes[index1];
      child2 := @m_nodes[index2];

      parentIndex := AllocateNode();
      parent := @m_nodes[parentIndex];
      parent^.child1 := index1;
      parent^.child2 := index2;
      parent^.height := 1 + b2Max(child1^.height, child2^.height);
      {$IFDEF OP_OVERLOAD}
      parent^.aabb.Combine(child1^.aabb, child2^.aabb);
      {$ELSE}
      Combine(parent^.aabb, child1^.aabb, child2^.aabb);
      {$ENDIF}
      parent^.parent := b2_nullNode;

      child1^.parent := parentIndex;
      child2^.parent := parentIndex;

      nodes[jMin] := nodes[count - 1];
      nodes[iMin] := parentIndex;

      Dec(count);
   end;

   m_root := nodes[0];
end;

procedure Tb2DynamicTree.ShiftOrigin(const newOrigin: TVector2);
var
	i: Integer;
begin
 	// Build array of leaves. Free the rest.
 	for i := 0 to m_nodeCapacity - 1 do
 	begin
    {$IFDEF OP_OVERLOAD}
    m_nodes[i].aabb.lowerBound.SubtractBy(newOrigin);
    m_nodes[i].aabb.upperBound.SubtractBy(newOrigin);
    {$ELSE}
    SubtractBy(m_nodes[i].aabb.lowerBound, newOrigin);
    SubtractBy(m_nodes[i].aabb.upperBound, newOrigin);
    {$ENDIF}
 	end;
end;

{$ENDIF}

{$IFDEF B2_USE_BRUTE_FORCE}
constructor Tb2DynamicTree.Create;
begin
   m_proxyCapacity := 128;
   m_proxyCount := 0;

   SetLength(m_proxyMap, m_proxyCapacity);
   SetLength(m_proxies, m_proxyCapacity);

   Initialize;
end;

procedure Tb2DynamicTree.Initialize;
var
   i: Integer;
begin
   // Build the free list
   m_freeId := m_proxyCount;
   for i := m_freeId to m_proxyCapacity - 2 do
      m_proxyMap[i] := i + 1;
   m_proxyMap[m_proxyCapacity - 1] := b2_nullNode;
end;

function Tb2DynamicTree.ComputeHeight: Int32;
begin
   Result := 0;
end;

function Tb2DynamicTree.CreateProxy(const _aabb: Tb2AABB;
   _userData: Pointer): Int32;
var
   id: Int32;
begin
   if m_proxyCount = m_proxyCapacity then
   begin
      m_proxyCapacity := m_proxyCapacity * 2;
      SetLength(m_proxyMap, m_proxyCapacity);
      SetLength(m_proxies, m_proxyCapacity);

      Initialize;
   end;

   //b2Assert(0 <= m_freeId && m_freeId < m_proxyCapacity);
   Result := m_freeId;
   m_freeId := m_proxyMap[Result];

   m_proxies[m_proxyCount].aabb := _aabb;
   m_proxies[m_proxyCount].userData := _userData;
   m_proxies[m_proxyCount].id := Result;
   m_proxyMap[Result] := m_proxyCount;
   Inc(m_proxyCount);
end;

procedure Tb2DynamicTree.DestroyProxy(proxyId: Int32);
var
   index: Int32;
begin
   // b2Assert(0 < m_proxyCount && 0 <= proxyId && proxyId < m_proxyCapacity);
   index := m_proxyMap[proxyId];

   // Add to free list
   m_proxyMap[proxyId] := m_freeId;
   m_freeId := proxyId;

   // Keep proxy array contiguous
   if index < m_proxyCount - 1 then
   begin
      m_proxies[index] := m_proxies[m_proxyCount - 1];
      m_proxyMap[m_proxies[index].id] := index;
   end;

   Dec(m_proxyCount);
   Validate;
end;

function Tb2DynamicTree.MoveProxy(proxyId: Int32; const aabb: Tb2AABB;
   const displacement: TVector2): Boolean;
const
   _r: TVector2 = (X: b2_aabbExtension; Y: b2_aabbExtension);
var
   b: Tb2AABB;
   d: TVector2;
   index: Int32;
begin
   // b2Assert(0 < m_proxyCount && 0 <= proxyId && proxyId < m_proxyCapacity);
	 // B2_NOT_USED(displacement);

   index := m_proxyMap[proxyId];

   if {$IFDEF OP_OVERLOAD}m_proxies[index].aabb.Contains(aabb)
      {$ELSE}Contains(m_proxies[index].aabb, aabb){$ENDIF} then
   begin
      Result := False;
      Exit;
   end;

   // Extend AABB.
   b := aabb;
   {$IFDEF OP_OVERLOAD}
   b.lowerBound.SubtractBy(_r);
   b.upperBound.AddBy(_r);
   {$ELSE}
   SubtractBy(b.lowerBound, _r);
   AddBy(b.upperBound, _r);
   {$ENDIF}

   // Predict AABB displacement.
   {$IFDEF OP_OVERLOAD}
   d := b2_aabbMultiplier * displacement;
   {$ELSE}
   d := Multiply(displacement, b2_aabbMultiplier);
   {$ENDIF}

   with b do
   begin
      if d.x < 0.0 then
         lowerBound.x := lowerBound.x + d.x
      else
         upperBound.x := upperBound.x + d.x;

      if d.y < 0.0 then
         lowerBound.y := lowerBound.y + d.y
      else
         upperBound.y := upperBound.y + d.y;
   end;

   m_proxies[index].aabb := b;
   Result := True;
end;

procedure Tb2DynamicTree.Rebalance(iterations: Int32);
begin
   //B2_NOT_USED(iterations);
end;

function Tb2DynamicTree.GetUserData(proxyId: Int32): Pointer;
begin
   Result := m_proxies[m_proxyMap[proxyId]].userData;
end;

function Tb2DynamicTree.GetFatAABB(proxyId: Int32): Pb2AABB;
begin
   Result := @m_proxies[m_proxyMap[proxyId]].aabb;
end;

procedure Tb2DynamicTree.Query(callback: Tb2GenericCallBackWrapper; const _aabb: Tb2AABB);
var
   i: Integer;
begin
   for i := 0 to m_proxyCount - 1 do
      if b2TestOverlap(m_proxies[i].aabb, _aabb) then
         if not callback.QueryCallback(m_proxies[i].id) then
            Exit;
end;

procedure Tb2DynamicTree.RayCast(callback: Tb2GenericCallBackWrapper;
   const input: Tb2RayCastInput);
var
   i: Integer;
   maxFraction, value: PhysicsFloat;
   r, t, v, abs_v, c, h: TVector2;
   segmentAABB: Tb2AABB;
   subInput: Tb2RayCastInput;
begin
   {$IFDEF OP_OVERLOAD}
   r := input.p2 - input.p1;
   //b2Assert(r.LengthSquared() > 0.0f);
   r.Normalize;
   {$ELSE}
   r := Subtract(input.p2, input.p1);
   //b2Assert(r.LengthSquared() > 0.0f);
   Normalize(r);
   {$ENDIF}

   // v is perpendicular to the segment.
   v := b2Cross(1.0, r);
   abs_v := b2Abs(v);

   // Separating axis for segment (Gino, p80).
   // |dot(v, p1 - c)| > dot(|v|, h)

   maxFraction := input.maxFraction;

   // Build a bounding box for the segment.
   {$IFDEF OP_OVERLOAD}
   t := input.p1 + maxFraction * (input.p2 - input.p1);
   {$ELSE}
   t := Add(input.p1, Multiply(Subtract(input.p2, input.p1), maxFraction));
   {$ENDIF}
   segmentAABB.lowerBound := b2Min(input.p1, t);
   segmentAABB.upperBound := b2Max(input.p1, t);

   for i := 0 to m_proxyCount - 1 do
   begin
      with m_proxies[i] do
      begin
         if not b2TestOverlap(aabb, segmentAABB) then
            Continue;

         // Separating axis for segment (Gino, p80).
         // |dot(v, p1 - c)| > dot(|v|, h)
         {$IFDEF OP_OVERLOAD}
         c := aabb.GetCenter;
         h := aabb.GetExtents;
         if Abs(b2Dot(v, input.p1 - c)) - b2Dot(abs_v, h) > 0.0 then
            Continue;
         {$ELSE}
         c := GetCenter(aabb);
         h := GetExtents(aabb);
         if Abs(b2Dot(v, Subtract(input.p1, c))) - b2Dot(abs_v, h) > 0.0 then
            Continue;
         {$ENDIF}

         subInput.p1 := input.p1;
         subInput.p2 := input.p2;
         subInput.maxFraction := maxFraction;

         value := callback.RayCastCallback(subInput, id);

         if value = 0.0 then
            Exit; // The client has terminated the ray cast.

         if value > 0.0 then
         begin
            maxFraction := value;
            // Update segment bounding box.
            {$IFDEF OP_OVERLOAD}
            t := input.p1 + maxFraction * (input.p2 - input.p1);
            {$ELSE}
            t := Add(input.p1, Multiply(Subtract(input.p2, input.p1), maxFraction));
            {$ENDIF}
            segmentAABB.lowerBound := b2Min(input.p1, t);
            segmentAABB.upperBound := b2Max(input.p1, t);
         end;
      end;
   end;
end;

procedure Tb2DynamicTree.Validate;
var
   //i: Integer;
   id, freeCount: Int32;
begin
   //b2Assert(m_proxyCount > 0 || m_freeId == b2_nullNode);
   //b2Assert(m_freeId == b2_nullNode || m_freeId < m_proxyCapacity);

   id := m_freeId;
   //freeCount := 0;
   while id <> b2_nullNode do
   begin
      //Inc(freeCount);
      //b2Assert(freeCount <= m_proxyCapacity);
      id := m_proxyMap[id];
   end;

   //b2Assert(freeCount + m_proxyCount == m_proxyCapacity);
   //b2Assert(m_proxyCount <= m_proxyCapacity);

   //for i := 0 to m_proxyCount - 1 do
      //b2Assert(m_proxyMap[m_proxies[i].id] == i);
end;

{$ENDIF}

{ Tb2BroadPhase }

/// This is used to sort pairs.
function b2PairLessThan(const pair1, pair2: Tb2Pair): Int32; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
begin
   Result := pair1.proxyIdA - pair2.proxyIdA;
   if Result = 0 then
      Result := pair1.proxyIdB - pair2.proxyIdB;
end;

constructor Tb2BroadPhase.Create;
begin
   m_tree := Tb2DynamicTree.Create;

   m_proxyCount := 0;
   m_pairCapacity := 16;
   m_pairCount := 0;
   SetLength(m_pairBuffer, m_pairCapacity);

   m_moveCapacity := 16;
   m_moveCount := 0;
   SetLength(m_moveBuffer, m_moveCapacity);
end;

destructor Tb2BroadPhase.Destroy;
begin
   m_tree.Free;
end;

procedure Tb2BroadPhase.QuickSortPairBuffer(L, R: Int32);
var
  I, J: Integer;
  P: Pb2Pair;
  T: Tb2Pair;
begin
   repeat
      I := L;
      J := R;
      P := @m_pairBuffer[(L + R) shr 1];
      repeat
         while b2PairLessThan(m_pairBuffer[I], P^) < 0 do
            Inc(I);
         while b2PairLessThan(m_pairBuffer[J], P^) > 0 do
            Dec(J);
         if I <= J then
         begin
            if I <> J then
            begin
               T := m_pairBuffer[I];
               m_pairBuffer[I] := m_pairBuffer[J];
               m_pairBuffer[J] := T;
            end;
            Inc(I);
            Dec(J);
         end;
      until I > J;
      if L < J then
         QuickSortPairBuffer(L, J);
      L := I;
   until I >= R;
end;

procedure Tb2BroadPhase.BufferMove(proxyId: Int32);
begin
   if m_moveCount = m_moveCapacity then
   begin
      m_moveCapacity := m_moveCapacity * 2;
      SetLength(m_moveBuffer, m_moveCapacity);
   end;

   m_moveBuffer[m_moveCount] := proxyId;
   Inc(m_moveCount);
end;

procedure Tb2BroadPhase.UnBufferMove(proxyId: Int32);
var
   i: Integer;
begin
   for i := 0 to m_moveCount - 1 do
      if m_moveBuffer[i] = proxyId then
         m_moveBuffer[i] := e_nullProxy;
end;

function Tb2BroadPhase.QueryCallback(proxyId: Int32): Boolean;
begin
   // A proxy cannot form a pair with itself.
   if proxyId = m_queryProxyId then
   begin
      Result := True;
      Exit;
   end;

   // Grow the pair buffer as needed.
   if m_pairCount = m_pairCapacity then
   begin
      m_pairCapacity := m_pairCapacity * 2;
      SetLength(m_pairBuffer, m_pairCapacity);
   end;

   m_pairBuffer[m_pairCount].proxyIdA := b2Min(proxyId, m_queryProxyId);
   m_pairBuffer[m_pairCount].proxyIdB := b2Max(proxyId, m_queryProxyId);
   Inc(m_pairCount);

   Result := True;
end;

function Tb2BroadPhase.CreateProxy(const aabb: Tb2AABB; userData: Pointer): Int32;
begin
   Result := m_tree.CreateProxy(aabb, userData);
   Inc(m_proxyCount);
   BufferMove(Result);
end;

procedure Tb2BroadPhase.DestroyProxy(proxyId: Int32);
begin
   UnBufferMove(proxyId);
   Dec(m_proxyCount);
   m_tree.DestroyProxy(proxyId);
end;

procedure Tb2BroadPhase.MoveProxy(proxyId: Int32; const aabb: Tb2AABB;
   const displacement: TVector2);
begin
   if m_tree.MoveProxy(proxyId, aabb, displacement) then
      BufferMove(proxyId);
end;

procedure Tb2BroadPhase.TouchProxy(proxyId: Int32);
begin
   BufferMove(proxyId);
end;

function Tb2BroadPhase.GetFatAABB(proxyId: Int32): Pb2AABB;
begin
   Result := m_tree.GetFatAABB(proxyId);
end;

function Tb2BroadPhase.GetUserData(proxyId: Int32): Pointer;
begin
   Result := m_tree.GetUserData(proxyId);
end;

function Tb2BroadPhase.TestOverlap(proxyIdA, proxyIdB: Int32): Boolean;
begin
   Result := b2TestOverlap(m_tree.GetFatAABB(proxyIdA)^, m_tree.GetFatAABB(proxyIdB)^);
end;

procedure Tb2BroadPhase.UpdatePairs(callback: Tb2ContactManager);
var
   i: Integer;
   fatAABB: Pb2AABB;
   primaryPair, pair: Pb2Pair;
begin
   // Reset pair buffer
   m_pairCount := 0;

   // Perform tree queries for all moving proxies.
   for i := 0 to m_moveCount - 1 do
   begin
      m_queryProxyId := m_moveBuffer[i];
      if m_queryProxyId = e_nullProxy then
         Continue;

      // We have to query the tree with the fat AABB so that
      // we don't fail to create a pair that may touch later.
      fatAABB := m_tree.GetFatAABB(m_queryProxyId);

      // Query tree, create pairs and add them pair buffer.
      m_tree.Query(Self, fatAABB^);
   end;

   // Reset move buffer
   m_moveCount := 0;

   // Sort the pair buffer to expose duplicates.
   if m_pairCount >= 2 then
      QuickSortPairBuffer(0, m_pairCount - 1);

   // Send the pairs back to the client.
   i := 0;
   while i < m_pairCount do
   begin
      primaryPair := @m_pairBuffer[i];
      callback.AddPair(m_tree.GetUserData(primaryPair^.proxyIdA),
         m_tree.GetUserData(primaryPair^.proxyIdB));
      Inc(i);

      // Skip any duplicate pairs.
      while i < m_pairCount do
      begin
         pair := @m_pairBuffer[i];
         if (pair^.proxyIdA <> primaryPair^.proxyIdA) or (pair^.proxyIdB <> primaryPair^.proxyIdB) then
            Break;
         Inc(i);
      end;
   end;

   // Try to keep the tree balanced.
   //m_tree.Rebalance(4);
end;

procedure Tb2BroadPhase.Query(callback: Tb2GenericCallBackWrapper; const aabb: Tb2AABB);
begin
   m_tree.Query(callback, aabb);
end;

procedure Tb2BroadPhase.RayCast(callback: Tb2GenericCallBackWrapper;
   const input: Tb2RayCastInput);
begin
   m_tree.RayCast(callback, input);
end;

function Tb2BroadPhase.GetTreeHeight: Int32;
begin
   Result := m_tree.GetHeight;
end;

function Tb2BroadPhase.GetTreeBalance: Int32;
begin
   Result := m_tree.GetMaxBalance;
end;

function Tb2BroadPhase.GetTreeQuality: PhysicsFloat;
begin
   Result := m_tree.GetAreaRatio;
end;

procedure Tb2BroadPhase.ShiftOrigin(const newOrigin: TVector2);
begin
 	m_tree.ShiftOrigin(newOrigin);
end;

//////////////////////////////////////////////////////////////
/// Fixture & Shapes

{ Tb2Shape }

constructor Tb2Shape.Create;
begin
   m_destroyed := False;
end;

destructor Tb2Shape.Destroy;
begin
   m_destroyed := True;
   if Assigned(m_fixture) then
      if Assigned(m_fixture.m_body) then
         m_fixture.m_body.DestroyFixture(m_fixture);
end;

{ Tb2FixtureDef }

constructor Tb2FixtureDef.Create;
begin
   shape := nil;
   userData := nil;
   friction := 0.2;
   restitution := 0.0;
   density := 0.0;
   filter.categoryBits := 1;
   filter.maskBits := $FFFF;
   filter.groupIndex := 0;
   isSensor := False;
end;

{ Tb2Fixture }

constructor Tb2Fixture.Create(body: Tb2Body; def: Tb2FixtureDef;
   AutoFreeShape: Boolean = True);
var
   i: Integer;
   childCount: Int32;
begin
   m_userData := def.userData;
   m_friction := def.friction;
   m_restitution := def.restitution;

   m_body := body;
   m_next := nil;

   m_filter := def.filter;
   m_isSensor := def.isSensor;
   m_density := def.density;

   m_shape := def.shape.Clone;
   if AutoFreeShape then
      def.shape.Free;
   m_shape.m_fixture := Self;

	 // Reserve proxy space
	 childCount := m_shape.GetChildCount;
   SetLength(m_proxies, childCount);
   for i := 0 to childCount - 1 do
   begin
	  	m_proxies[i].fixture := nil;
	  	m_proxies[i].proxyId := e_nullProxy;
   end;
   m_proxyCount := 0;
end;

destructor Tb2Fixture.Destroy;
begin
   // The proxies must be destroyed before calling this.
	 //b2Assert(m_proxyCount == 0);

   if Assigned(m_body) then
      m_body.DestroyFixture(Self, False);

   // Free the child shape.
   m_shape.m_fixture := nil;
   m_shape.Free;
end;

destructor Tb2Fixture.Destroy2;
begin
   if not m_shape.m_destroyed then
   begin
      m_shape.m_fixture := nil;
      m_shape.Free;
   end;
end;

{$IFDEF ENABLE_DUMP}
procedure Tb2Fixture.Dump(bodyIndex: Int32);
var
   i: Integer;
   cs: Tb2CircleShape;
   es: Tb2EdgeShape;
   ps: Tb2PolygonShape;
   chs: Tb2ChainShape;
begin
   b2DumpMethod(2, 'begin', []);
   b2DumpMethod(3, 'fd := Tb2FixtureDef.Create;', []);
   b2DumpMethod(3, 'fd.friction := %s;', [b2FloatToStr(m_friction)]);
   b2DumpMethod(3, 'fd.restitution := %s;', [b2FloatToStr(m_restitution)]);
   b2DumpMethod(3, 'fd.density := %s;', [b2FloatToStr(m_density)]);
   b2DumpMethod(3, 'fd.isSensor := %s;', [b2BoolToStr(m_isSensor)]);
   b2DumpMethod(3, 'fd.filter.categoryBits := %d;', [m_filter.categoryBits]);
   b2DumpMethod(3, 'fd.filter.maskBits := %d;', [m_filter.maskBits]);
   b2DumpMethod(3, 'fd.filter.groupIndex := %d;', [m_filter.groupIndex]);

   case m_shape.m_type of
      e_circleShape: 
         begin
            cs := Tb2CircleShape(m_shape);
            b2DumpMethod(3, 'circle_shape := Tb2CircleShape.Create;', []);
            b2DumpMethod(3, 'circle_shape.m_radius := %s;', [b2FloatToStr(cs.m_radius)]);
            b2DumpMethod(3, 'circle_shape.m_p := MakeVector(%s, %s);', [b2FloatToStr(cs.m_p.x), b2FloatToStr(cs.m_p.y)]);
         end;
      e_edgeShape:
         begin
            es := Tb2EdgeShape(m_shape);
            b2DumpMethod(3, 'edge_shape := Tb2EdgeShape.Create;', []);
            b2DumpMethod(3, 'edge_shape.m_radius := %s;', [b2FloatToStr(es.m_radius)]);
            b2DumpMethod(3, 'edge_shape.m_vertex0 := MakeVector(%s, %s);', [b2FloatToStr(es.m_vertex0.x), b2FloatToStr(es.m_vertex0.y)]);
            b2DumpMethod(3, 'edge_shape.m_vertex1 := MakeVector(%s, %s);', [b2FloatToStr(es.m_vertex1.x), b2FloatToStr(es.m_vertex1.y)]);
            b2DumpMethod(3, 'edge_shape.m_vertex2 := MakeVector(%s, %s);', [b2FloatToStr(es.m_vertex2.x), b2FloatToStr(es.m_vertex2.y)]);
            b2DumpMethod(3, 'edge_shape.m_vertex3 := MakeVector(%s, %s);', [b2FloatToStr(es.m_vertex3.x), b2FloatToStr(es.m_vertex3.y)]);
            b2DumpMethod(3, 'edge_shape.m_hasVertex0 := %s;', [b2BoolToStr(es.m_hasVertex0)]);
            b2DumpMethod(3, 'edge_shape.m_hasVertex3 := %s;', [b2BoolToStr(es.m_hasVertex3)]);
         end;  
      e_polygonShape:
         begin
            ps := Tb2PolygonShape(m_shape);
            b2DumpMethod(3, 'poly_shape := Tb2PolygonShape.Create;', []);
            b2DumpMethod(3, 'SetLength(vs, %d);', [b2_maxPolygonVertices]);
            for i := 0 to ps.m_count - 1 do
               b2DumpMethod(3, 'vs[%d] := MakeVector(%s, %s);', [i, b2FloatToStr(ps.m_vertices[i].x), b2FloatToStr(ps.m_vertices[i].y)]);
            b2DumpMethod(3, 'poly_shape.SetVertices(@vs[0], %d);', [ps.m_count]);
         end;
      e_chainShape:
         begin
            chs := Tb2ChainShape(m_shape);
            b2DumpMethod(3, 'chain_shape := Tb2ChainShape.Create;', []);
            b2DumpMethod(3, 'SetLength(vs, %d);', [chs.m_count]);
            for i := 0 to chs.m_count - 1 do
               b2DumpMethod(3, 'vs[%d] := MakeVector(%s, %s);', [i, b2FloatToStr(chs.m_vertices[i].x), b2FloatToStr(chs.m_vertices[i].y)]);
            if chs.m_loop then
            begin
               b2DumpMethod(3, 'chain_shape.CreateLoop(@vs[0], %d);', [chs.m_count]);
               b2DumpMethod(3, 'chain_shape.m_prevVertex := MakeVector(%s, %s);', [b2FloatToStr(chs.m_prevVertex.x), b2FloatToStr(chs.m_prevVertex.y)]);
               b2DumpMethod(3, 'chain_shape.m_nextVertex := MakeVector(%s, %s);', [b2FloatToStr(chs.m_nextVertex.x), b2FloatToStr(chs.m_nextVertex.y)]);
               b2DumpMethod(3, 'chain_shape.m_hasPrevVertex := %s;', [b2BoolToStr(chs.m_hasPrevVertex)]);
               b2DumpMethod(3, 'chain_shape.m_hasNextVertex := %s;', [b2BoolToStr(chs.m_hasNextVertex)]);
            end
            else
               b2DumpMethod(3, 'chain_shape.CreateChain(@vs[0], %d);', [chs.m_count]);
         end;
   end;

   b2DumpMethod(3, 'fd.shape := shape;', []);
   b2DumpMethod(3, 'bodies[%d].CreateFixture(fd);', [bodyIndex]);
   b2DumpMethod(2, 'end;', []);
end;
{$ENDIF}

procedure Tb2Fixture.CreateProxies(broadPhase: Tb2BroadPhase; const xf: Tb2Transform);
var
   i: Integer;
begin
   //b2Assert(m_proxyCount == 0);

   // Create proxies in the broad-phase.
   m_proxyCount := m_shape.GetChildCount;

   // Create proxies in the broad-phase.
   for i := 0 to m_proxyCount - 1 do
      with m_proxies[i] do
      begin
         m_shape.ComputeAABB(aabb, xf, i);
         proxyId := broadPhase.CreateProxy(aabb, @m_proxies[i]);
         fixture := Self;
         childIndex := i;
      end;
end;

procedure Tb2Fixture.DestroyProxies(broadPhase: Tb2BroadPhase);
var
   i: Integer;
begin
   // Destroy proxies in the broad-phase.
   for i := 0 to m_proxyCount - 1 do
      with m_proxies[i] do
      begin
         broadPhase.DestroyProxy(proxyId);
         proxyId := e_nullProxy;
      end;

   m_proxyCount := 0;
end;

procedure Tb2Fixture.Synchronize(broadPhase: Tb2BroadPhase; const xf1, xf2: Tb2Transform);
var
   i: Integer;
   aabb1, aabb2: Tb2AABB;
   displacement: TVector2;
begin
   if m_proxyCount = 0 then
      Exit;

   for i := 0 to m_proxyCount - 1 do
      with m_proxies[i] do
      begin
         // Compute an AABB that covers the swept shape (may miss some rotation effect).
         m_shape.ComputeAABB(aabb1, xf1, childIndex);
         m_shape.ComputeAABB(aabb2, xf2, childIndex);

         {$IFDEF OP_OVERLOAD}
         aabb.Combine(aabb1, aabb2);
         displacement := xf2.p - xf1.p;
         {$ELSE}
         Combine(aabb, aabb1, aabb2);
         displacement := Subtract(xf2.p, xf1.p);
         {$ENDIF}
         broadPhase.MoveProxy(proxyId, aabb, displacement);
      end;
end;

function Tb2Fixture.GetType: Tb2ShapeType;
begin
   Result := m_shape.m_type;
end;

procedure Tb2Fixture.SetFilterData(const filter: Tb2Filter);
begin
   m_filter := filter;
   Refilter;
end;

procedure Tb2Fixture.Refilter;
var
   edge: Pb2ContactEdge;
   broadPhase: Tb2BroadPhase;
   i: Integer;
begin
   if not Assigned(m_body) then
      Exit;

   // Flag associated contacts for filtering.
   edge := m_body.GetContactList;
   while Assigned(edge) do
      with edge^, contact^ do
      begin
         if (m_fixtureA = Self) or (m_fixtureB = Self) then
            {$IFDEF OP_OVERLOAD}
            FlagForFiltering;
            {$ELSE}
            FlagForFiltering(contact^);
            {$ENDIF}
         edge := next;
      end;

   if m_body.m_world = nil then
      Exit;

   // Touch each proxy so that new pairs may be created
   broadPhase := m_body.m_world.m_contactManager.m_broadPhase;
   for i := 0 to m_proxyCount - 1 do
      broadPhase.TouchProxy(m_proxies[i].proxyId);
end;

procedure Tb2Fixture.FSetIsSensor(value: Boolean);
begin
   if value <> m_isSensor then
   begin
      m_body.SetAwake(True);
      m_isSensor := value;
   end;
end;

function Tb2Fixture.GetFilterData: Pb2Filter;
begin
   Result := @m_filter;
end;

function Tb2Fixture.TestPoint(const p: TVector2): Boolean;
begin
   Result := m_shape.TestPoint(m_body.m_xf, p);
end;

function Tb2Fixture.RayCast(var output: Tb2RayCastOutput;
   const input: Tb2RayCastInput; childIndex: Int32): Boolean;
begin
   Result := m_shape.RayCast(output, input, m_body.m_xf, childIndex);
end;

procedure Tb2Fixture.GetMassData(var massData: Tb2MassData);
begin
   m_shape.ComputeMass(massData, m_density);
end;

function Tb2Fixture.GetAABB(childIndex: Int32): Pb2AABB;
begin
   //b2Assert(0 <= childIndex && childIndex < m_proxyCount);
	 Result := @m_proxies[childIndex].aabb;
end;

//////////////////////////////////////////////////////////////
// Joints

{ Tb2JointDef }

constructor Tb2JointDef.Create;
begin
		jointType := e_unknownJoint;
		userData := nil;
		bodyA := nil;
		bodyB := nil;
		collideConnected := False;
end;

{ Tb2Joint }

constructor Tb2Joint.Create(def: Tb2JointDef);
begin
   //b2Assert(def->bodyA != def->bodyB);
   m_type := def.jointType;
   m_prev := nil;
   m_next := nil;
   m_bodyA := def.bodyA;
   m_bodyB := def.bodyB;
   m_index := 0;
   m_collideConnected := def.collideConnected;
   m_islandFlag := False;
   m_userData := def.userData;

   m_edgeA.joint := nil;
   m_edgeA.other := nil;
   m_edgeA.prev := nil;
   m_edgeA.next := nil;
   m_edgeB.joint := nil;
   m_edgeB.other := nil;
   m_edgeB.prev := nil;
   m_edgeB.next := nil;
end;

{$IFDEF ENABLE_DUMP}
procedure Tb2Joint.Dump;
begin
   b2DumpMethod(1, 'begin', []);
   b2DumpMethod(2, '// Dump is not supported for %s.', [Self.ClassName]);
   b2DumpMethod(1, 'end;', []);
end;
{$ENDIF}


procedure Tb2Joint.ShiftOrigin(const newOrigin: TVector2);
begin
	//B2_NOT_USED(newOrigin);
end;

function Tb2Joint.IsActive: Boolean;
begin
   Result := m_bodyA.IsActive and m_bodyB.IsActive;
end;

{$IFDEF CONTROLLERS}
//////////////////////////////////////////////////////////////
// Controllers

{ Tb2Controller }

destructor Tb2Controller.Destroy;
begin
   //Remove attached bodies
   Clear;
   inherited;
end;

procedure Tb2Controller.Draw(debugDraw: Tb2Draw);
begin
end;

procedure Tb2Controller.Clear;
var
   edge: Pb2ControllerEdge;
begin
   while Assigned(m_bodyList) do
   begin
      edge := m_bodyList;

      //Remove edge from controller list
      m_bodyList := edge^.nextBody;

      //Remove edge from body list
      if Assigned(edge^.prevController) then
         edge^.prevController^.nextController := edge^.nextController;
      if Assigned(edge^.nextController) then
         edge^.nextController^.prevController := edge^.prevController;
      if edge = edge^.body.m_controllerList then
         edge^.body.m_controllerList := edge^.nextController;

      Dispose(edge)//Free the edge
   end;
   m_bodyCount := 0;
end;

procedure Tb2Controller.AddBody(body: Tb2Body);
var
   edge: Pb2ControllerEdge;
begin
   New(edge);
   edge^.body := body;
   edge^.controller := Self;

   //Add edge to controller list
   edge^.nextBody := m_bodyList;
   edge^.prevBody := nil;
   if Assigned(m_bodyList) then
      m_bodyList^.prevBody := edge;
   m_bodyList := edge;
   Inc(m_bodyCount);

   //Add edge to body list
   edge^.nextController := body.m_controllerList;
   edge^.prevController := nil;
   if Assigned(body.m_controllerList) then
      body.m_controllerList^.prevController := edge;
   body.m_controllerList := edge;
end;

procedure Tb2Controller.RemoveBody(body: Tb2Body);
var
   edge: Pb2ControllerEdge;
begin
   //Assert that the controller is not empty
   //b2Assert(m_bodyCount>0);

   //Find the corresponding edge
   edge := m_bodyList;
   while Assigned(edge) and (edge^.body <> body) do
      edge := edge^.nextBody;

   //Assert that we are removing a body that is currently attached to the controller
   //b2Assert(edge!=NULL);

   //Remove edge from controller list
   if Assigned(edge^.prevBody) then
      edge^.prevBody^.nextBody := edge^.nextBody;
   if Assigned(edge^.nextBody) then
      edge^.nextBody^.prevBody := edge^.prevBody;
   if edge = m_bodyList then
      m_bodyList := edge^.nextBody;
   Dec(m_bodyCount);

   //Remove edge from body list
   if Assigned(edge^.prevController) then
      edge^.prevController^.nextController := edge^.nextController;
   if Assigned(edge^.nextController) then
      edge^.nextController^.prevController := edge^.prevController;
   if edge = body.m_controllerList then
      body.m_controllerList := edge^.nextController;

   Dispose(edge); //Free the edge
end;
{$ENDIF}

//////////////////////////////////////////////////////////////
// Body

{ Tb2BodyDef }

constructor Tb2BodyDef.Create;
begin
   userData := nil;
   ignoreColliding := False;
   position := b2Vec2_Zero;
   angle := 0.0;
   linearVelocity := b2Vec2_Zero;
   angularVelocity := 0.0;
   linearDamping := 0.0;
   angularDamping := 0.0;
   allowSleep := True;
   awake := True;
   fixedRotation := False;
   bullet := False;
   bodyType := b2_staticBody;
   active := True;
   gravityScale := 1.0;
end;

{ Tb2Body }

constructor Tb2Body.Create(bd: Tb2BodyDef; world: Tb2World);
begin
   //b2Assert(bd->position.IsValid());
   //b2Assert(bd->linearVelocity.IsValid());
   //b2Assert(b2IsValid(bd->angle));
   //b2Assert(b2IsValid(bd->angularVelocity));
   //b2Assert(b2IsValid(bd->gravityScale) && bd->gravityScale >= 0.0f);
   //b2Assert(b2IsValid(bd->angularDamping) && bd->angularDamping >= 0.0f);
   //b2Assert(b2IsValid(bd->linearDamping) && bd->linearDamping >= 0.0f);

   m_flags := 0;

   if bd.bullet then
      m_flags := m_flags or e_body_bulletFlag;
   if bd.fixedRotation then
      m_flags := m_flags or e_body_fixedRotationFlag;
   if bd.allowSleep then
      m_flags := m_flags or e_body_autoSleepFlag;
   if bd.awake then
      m_flags := m_flags or e_body_awakeFlag;
   if bd.active then
      m_flags := m_flags or e_body_activeFlag;
   if bd.ignoreColliding then
      m_flags := m_flags or e_body_ignoreCollideFlag;

   m_world := world;

   m_xf.p := bd.position;
   {$IFDEF OP_OVERLOAD}
   m_xf.q.SetAngle(bd.angle);
   {$ELSE}
   SetAngle(m_xf.q, bd.angle);
   {$ENDIF}

   m_sweep.localCenter := b2Vec2_Zero;
	 m_sweep.c0 := m_xf.p;
	 m_sweep.c := m_xf.p;
	 m_sweep.a0 := bd.angle;
	 m_sweep.a := bd.angle;
	 m_sweep.alpha0 := 0.0;

   m_jointList := nil;
   m_contactList := nil;
   m_prev := nil;
   m_next := nil;

	 m_linearVelocity := bd.linearVelocity;
   m_angularVelocity := bd.angularVelocity;
   m_linearDamping := bd.linearDamping;
   m_angularDamping := bd.angularDamping;

   m_gravityScale := 1.0;

   m_force := b2Vec2_Zero;
   m_torque := 0.0;
   m_sleepTime := 0.0;

   m_type := bd.bodyType;
   if m_type = b2_dynamicBody then
   begin
      m_mass := 1.0;
      m_invMass := 1.0;
   end
   else
   begin
      m_mass := 0.0;
      m_invMass := 0.0;
   end;

   m_I := 0.0;
   m_invI := 0.0;

   m_userData := bd.userData;

   m_fixtureList := nil;
   m_fixtureCount := 0;

   {$IFDEF CONTROLLERS}
   m_controllerList := nil;
   m_controllerCount := 0;
   {$ENDIF}

   ComputeStoredInertia;
end;

destructor Tb2Body.Destroy;
begin
   if Assigned(m_world) then
      m_world.DestroyBody(Self, False);
end;

destructor Tb2Body.Destroy2;
begin
end;

{$IFDEF ENABLE_DUMP}
procedure Tb2Body.Dump;
const 
   bodyTypeDesc: array[Tb2BodyType] of string = ('b2_staticBody', 'b2_kinematicBody', 'b2_dynamicBody');
var
   bodyIndex: Int32;
   f: Tb2Fixture;
begin
   bodyIndex := m_islandIndex;

   b2DumpMethod(1, 'begin', []);
   b2DumpMethod(2, 'bd := Tb2BodyDef.Create;', []);
   b2DumpMethod(2, 'bd.bodyType := %s;', [bodyTypeDesc[m_type]]);
   b2DumpMethod(2, 'bd.position := MakeVector(%s, %s);', [b2FloatToStr(m_xf.p.x), b2FloatToStr(m_xf.p.y)]);
   b2DumpMethod(2, 'bd.angle := %s;', [b2FloatToStr(m_sweep.a)]);
   b2DumpMethod(2, 'bd.linearVelocity := MakeVector(%s, %s);', [b2FloatToStr(m_linearVelocity.x), b2FloatToStr(m_linearVelocity.y)]);
   b2DumpMethod(2, 'bd.angularVelocity := %s;', [b2FloatToStr(m_angularVelocity)]);
   b2DumpMethod(2, 'bd.linearDamping := %s;', [b2FloatToStr(m_linearDamping)]);
   b2DumpMethod(2, 'bd.angularDamping := %s;', [b2FloatToStr(m_angularDamping)]);
   b2DumpMethod(2, 'bd.allowSleep := %s;', [b2BoolToStr(m_flags and e_body_autoSleepFlag)]);
   b2DumpMethod(2, 'bd.awake := %s;', [b2BoolToStr(m_flags and e_body_awakeFlag)]);
   b2DumpMethod(2, 'bd.fixedRotation := %s;', [b2BoolToStr(m_flags and e_body_fixedRotationFlag)]);
   b2DumpMethod(2, 'bd.bullet := %s;', [b2BoolToStr(m_flags and e_body_bulletFlag)]);
   b2DumpMethod(2, 'bd.active := %s;', [b2BoolToStr(m_flags and e_body_activeFlag)]);
   b2DumpMethod(2, 'bd.gravityScale := %s;', [b2FloatToStr(m_gravityScale)]);
   b2DumpMethod(2, 'bodies[%d] := m_world.CreateBody(bd);', [m_islandIndex]);

   if Assigned(m_fixtureList) then
      b2DumpMethod(2, '', []);

   f := m_fixtureList;
   while Assigned(f) do
   begin
      f.Dump(bodyIndex);
      f := f.m_next;
   end;
   b2DumpMethod(1, 'end;', []);
end;
{$ENDIF}

procedure Tb2Body.ComputeStoredInertia;
begin
   m_storedInertia := m_I + m_mass * b2Dot(m_sweep.localCenter, m_sweep.localCenter);
end;

function Tb2Body.CreateFixture(def: Tb2FixtureDef;
   AutoFreeFixtureDef: Boolean = True; AutoFreeShape: Boolean = True;
   AutoResetMassData: Boolean = True): Tb2Fixture;
begin
   Result := nil;
   //b2Assert(m_world->IsLocked() == false);
   if m_world.IsLocked then
   begin
      if AutoFreeFixtureDef then
         def.Free;
      Exit;
   end;

   Result := Tb2Fixture.Create(Self, def, AutoFreeShape);
   if m_flags and e_body_activeFlag <> 0 then
      Result.CreateProxies(m_world.m_contactManager.m_broadPhase, m_xf);

   Result.m_body := Self;
   Result.m_next := m_fixtureList;
   m_fixtureList := Result;
   Inc(m_fixtureCount);

   // Adjust mass properties if needed.
   if (Result.m_density > 0.0) and AutoResetMassData then
      ResetMassData;

   // Let the world know we have a new fixture. This will cause new contacts
   // to be created at the beginning of the next time step.
   m_world.m_flags := m_world.m_flags or e_world_newFixture;

   if AutoFreeFixtureDef then
      def.Free;
end;

function Tb2Body.CreateFixture(shape: Tb2Shape; density: PhysicsFloat;
   AutoFreeShape: Boolean = True; AutoResetMassData: Boolean = True): Tb2Fixture;
var
   def: Tb2FixtureDef;
begin
	 def := Tb2FixtureDef.Create;
	 def.shape := shape;
	 def.density := density;
	 Result := CreateFixture(def, True, AutoFreeShape, AutoResetMassData);
end;

procedure Tb2Body.DestroyFixture(fixture: Tb2Fixture; DoFree: Boolean = True);
var
   node: ^Tb2Fixture;
   found: Boolean;
   edge: Pb2ContactEdge;
   c: Pb2Contact;
begin
   //b2Assert(m_world->IsLocked() == false);
   if m_world.IsLocked then
      Exit;

   //b2Assert(fixture->m_body == this);

   // Remove the fixture from this body's singly linked list.
   //b2Assert(m_fixtureCount > 0);
   node := @m_fixtureList;
   found := False;
   while Assigned(node^) do
   begin
      if node^ = fixture then
      begin
         node^ := fixture.m_next;
         found := True;
         Break;
      end;
      node := @(node^.m_next);
   end;

   // You tried to remove a shape that is not attached to this body.
   if not found then
      Exit;

   // Destroy any contacts associated with the fixture.
   edge := m_contactList;
   while Assigned(edge) do
   begin
      c := edge^.contact;
      edge := edge^.next;

      if (fixture = c^.m_fixtureA) or (fixture = c^.m_fixtureB) then
      begin
         // This destroys the contact and removes it from
         // this body's contact list.
         m_world.m_contactManager.Destroy(c);
      end;
   end;

   if m_flags and e_body_activeFlag <> 0 then
   begin
      //b2Assert(fixture->m_proxyId != b2BroadPhase::e_nullProxy);
      fixture.DestroyProxies(m_world.m_contactManager.m_broadPhase);
   end;

   if DoFree then
    begin
     {$IFNDEF AUTOREFCOUNT}
      fixture.Destroy2; // Call a destructor without side effects.
     {$ELSE}
      fixture.DisposeOf;
     {$ENDIF}
    end;
   Dec(m_fixtureCount);
   if m_fixtureCount = 0 then
      m_fixtureList := nil;
   // Reset the mass data.
   ResetMassData;
end;

procedure Tb2Body.DestroyFixtures(ResetMass: Boolean);
var
   node, tmpnode: Tb2Fixture;
   edge: Pb2ContactEdge;
   c: Pb2Contact;
begin
   if m_world.IsLocked then
      Exit;

   // Destroy all contacts
   edge := m_contactList;
   while Assigned(edge) do
   begin
      c := edge^.contact;
      m_world.m_contactManager.Destroy(c);
      edge := edge^.next;
   end;

   if m_flags and e_body_activeFlag <> 0 then
   begin
      node := m_fixtureList;
      while Assigned(node) do
      begin
         node.DestroyProxies(m_world.m_contactManager.m_broadPhase);
         node := node.m_next;
      end;
   end;

   // Free all fixtures
   node := m_fixtureList;
   while Assigned(node) do
   begin
      tmpnode := node.m_next;
      node.Destroy2;
      node := tmpnode;
   end;

   m_fixtureList := nil;
   m_fixtureCount := 0;

   if ResetMass then
      ResetMassData;
   // If not ResetMass, this body becomes a shape-less massed object
end;

procedure Tb2Body.SynchronizeFixtures;
var
   xf1: Tb2Transform;
   f: Tb2Fixture;
begin
   {$IFDEF OP_OVERLOAD}
   xf1.q.SetAngle(m_sweep.a0);
   xf1.p := m_sweep.c0 - b2Mul(xf1.q, m_sweep.localCenter);
   {$ELSE}
   SetAngle(xf1.q, m_sweep.a0);
   xf1.p := Subtract(m_sweep.c0, b2Mul(xf1.q, m_sweep.localCenter));
   {$ENDIF}

   f := m_fixtureList;
   while Assigned(f) do
   begin
      f.Synchronize(m_world.m_contactManager.m_broadPhase, xf1, m_xf);
      f := f.m_next;
   end;
end;

procedure Tb2Body.SynchronizeTransform;
begin
   {$IFDEF OP_OVERLOAD}
   m_xf.q.SetAngle(m_sweep.a);
   m_xf.p := m_sweep.c - b2Mul(m_xf.q, m_sweep.localCenter);
   {$ELSE}
   SetAngle(m_xf.q, m_sweep.a);
   m_xf.p := Subtract(m_sweep.c, b2Mul(m_xf.q, m_sweep.localCenter));
   {$ENDIF}
end;

function Tb2Body.ShouldCollide(other: Tb2Body): Boolean;
var
   jn: Pb2JointEdge;
begin
   if IsCollidingIgnored then
   begin
      Result := False;
      Exit;
   end;

   // At least one body should be dynamic.
   if (m_type <> b2_dynamicBody) and (other.m_type <> b2_dynamicBody) then
   begin
      Result := False;
      Exit;
   end;

   // Does a joint prevent collision?
   jn := m_jointList;
   while Assigned(jn) do
   begin
      if jn^.other = other then
         if not jn^.joint.m_collideConnected then
         begin
            Result := False;
            Exit;
         end;
      jn := jn^.next;
   end;
   Result := True;
end;

procedure Tb2Body.Advance(alpha: PhysicsFloat);
begin
   // Advance to the new safe time. This doesn't sync the broad-phase.
   {$IFDEF OP_OVERLOAD}
   m_sweep.Advance(alpha);
   m_sweep.c := m_sweep.c0;
   m_sweep.a := m_sweep.a0;
   m_xf.q.SetAngle(m_sweep.a);
   m_xf.p := m_sweep.c - b2Mul(m_xf.q, m_sweep.localCenter);
   {$ELSE}
   UPhysics2DTypes.Advance(m_sweep, alpha);
   m_sweep.c := m_sweep.c0;
   m_sweep.a := m_sweep.a0;
   SetAngle(m_xf.q, m_sweep.a);
   m_xf.p := Subtract(m_sweep.c, b2Mul(m_xf.q, m_sweep.localCenter));
   {$ENDIF}
end;

procedure Tb2Body.SetTransform(const position: TVector2; angle: PhysicsFloat);
var
   f: Tb2Fixture;
begin
   //b2Assert(m_world->m_lock == False);
   if m_world.IsLocked then
      Exit;

   {$IFDEF OP_OVERLOAD}
   m_xf.q.SetAngle(angle);
   {$ELSE}
   SetAngle(m_xf.q, angle);
   {$ENDIF}
   m_xf.p := position;

	 m_sweep.c := b2Mul(m_xf, m_sweep.localCenter);
	 m_sweep.a := angle;
	 m_sweep.c0 := m_sweep.c;
	 m_sweep.a0 := angle;

   f := m_fixtureList;
   while Assigned(f) do
   begin
      f.Synchronize(m_world.m_contactManager.m_broadPhase, m_xf, m_xf);
      f := f.m_next;
   end;
end;

function Tb2Body.GetTransform: Pb2Transform;
begin
   Result := @m_xf;
end;

function Tb2Body.GetAngle: PhysicsFloat;
begin
   Result := m_sweep.a;
end;

procedure Tb2Body.SetLinearVelocity(const v: TVector2);
begin
   if m_type = b2_staticBody then
      Exit;

   if b2Dot(v, v) > 0.0 then
      SetAwake(True);

   m_linearVelocity := v;
end;

procedure Tb2Body.SetAngularVelocity(omega: PhysicsFloat);
begin
   if m_type = b2_staticBody then
      Exit;

   if omega * omega > 0.0 then
      SetAwake(True);

   m_angularVelocity := omega;
end;

procedure Tb2Body.ApplyForce(const force, point: TVector2; wake: Boolean);
begin
   if m_type <> b2_dynamicBody then
      Exit;
   if wake and (not IsAwake) then
      SetAwake(True);

   // Don't accumulate a force if the body is sleeping.
   if IsAwake then
   begin
      {$IFDEF OP_OVERLOAD}
      m_force.AddBy(force);
      m_torque := m_torque + b2Cross(point - m_sweep.c, force);
      {$ELSE}
      AddBy(m_force, force);
      m_torque := m_torque + b2Cross(Subtract(point, m_sweep.c), force);
      {$ENDIF}
   end;
end;

procedure Tb2Body.ApplyForceToCenter(const force: TVector2; wake: Boolean);
begin
   if m_type <> b2_dynamicBody then
      Exit;

   if wake and (not IsAwake) then
      SetAwake(True);

   // Don't accumulate a force if the body is sleeping.
   if IsAwake then
   begin
      {$IFDEF OP_OVERLOAD}
      m_force.AddBy(force);
      {$ELSE}
      AddBy(m_force, force);
      {$ENDIF}
   end;
end;

procedure Tb2Body.ApplyTorque(torque: PhysicsFloat; wake: Boolean);
begin
   if m_type <> b2_dynamicBody then
      Exit;

   if wake and (not IsAwake) then
      SetAwake(True);

   // Don't accumulate a force if the body is sleeping
   if IsAwake then
      m_torque := m_torque + torque;
end;

procedure Tb2Body.ApplyLinearImpulse(const impulse, point: TVector2; wake: Boolean);
begin
   if m_type <> b2_dynamicBody then
      Exit;

   if wake and (not IsAwake) then
      SetAwake(True);

   // Don't accumulate velocity if the body is sleeping
   if IsAwake then
   begin
      {$IFDEF OP_OVERLOAD}
      m_linearVelocity.AddBy(m_invMass * impulse);
      m_angularVelocity := m_angularVelocity + m_invI * b2Cross(point -
         m_sweep.c, impulse);
      {$ELSE}
      AddBy(m_linearVelocity, Multiply(impulse, m_invMass));
      m_angularVelocity := m_angularVelocity + m_invI * b2Cross(Subtract(point,
         m_sweep.c), impulse);
      {$ENDIF}
   end;
end;

procedure Tb2Body.ApplyAngularImpulse(impulse: PhysicsFloat; wake: Boolean);
begin
   if m_type <> b2_dynamicBody then
      Exit;

   if wake and (not IsAwake) then
      SetAwake(True);

   // Don't accumulate velocity if the body is sleeping
   if IsAwake then
      m_angularVelocity := m_angularVelocity + m_invI * impulse;
end;

procedure Tb2Body.GetMassData(var data: Tb2MassData);
begin
   data.mass := m_mass;
   data.I := m_storedInertia;
   data.center := m_sweep.localCenter;
end;

procedure Tb2Body.SetMassData(const data: Tb2MassData);
var
   oldCenter: TVector2;
begin
   //b2Assert(m_world.IsLocked() == false);
   if m_world.IsLocked then
      Exit;

   if m_type <> b2_dynamicBody then
      Exit;

   m_invMass := 0.0;
   m_I := 0.0;
   m_invI := 0.0;

   m_mass := data.mass;
   if m_mass <= 0.0 then
      m_mass := 1.0;
   m_invMass := 1.0 / m_mass;

   if (data.I > 0.0) and ((m_flags and e_body_fixedRotationFlag) = 0) then
   begin
      m_I := data.I - m_mass * b2Dot(data.center, data.center);
      //b2Assert(m_I > 0.0f);
      m_invI := 1.0 / m_I;
   end;

   // Move center of mass.
   oldCenter := m_sweep.c;
   m_sweep.localCenter := data.center;
   m_sweep.c0 := b2Mul(m_xf, m_sweep.localCenter);
   m_sweep.c := m_sweep.c0;

   // Update center of mass velocity.
   {$IFDEF OP_OVERLOAD}
	 m_linearVelocity.AddBy(b2Cross(m_angularVelocity, m_sweep.c - oldCenter));
   {$ELSE}
   AddBy(m_linearVelocity, b2Cross(m_angularVelocity, Subtract(m_sweep.c, oldCenter)));
   {$ENDIF}

   ComputeStoredInertia;
end;

procedure Tb2Body.ResetMassData;
var
   f: Tb2Fixture;
   locatCenter, oldCenter: TVector2;
   massData: Tb2MassData;
begin
   // Compute mass data from shapes. Each shape has its own density.
   m_mass := 0.0;
   m_invMass := 0.0;
   m_I := 0.0;
   m_invI := 0.0;
   m_sweep.localCenter := b2Vec2_Zero;

   // Static and kinematic bodies have zero mass.
   if (m_type = b2_staticBody) or (m_type = b2_kinematicBody) then
   begin
		  m_sweep.c0 := m_xf.p;
		  m_sweep.c := m_xf.p;
		  m_sweep.a0 := m_sweep.a;
      Exit;
   end;

   //b2Assert(m_type == b2_dynamicBody);

   // Accumulate mass over all fixtures.
   locatCenter := b2Vec2_zero;
   f := m_fixtureList;
   while Assigned(f) do
   begin
      if IsZero(f.m_density) then
      begin
         f := f.m_next;
         Continue;
      end;

      f.GetMassData(massData);
      m_mass := m_mass + massData.mass;
      {$IFDEF OP_OVERLOAD}
      locatCenter.AddBy(massData.mass * massData.center);
      {$ELSE}
      AddBy(locatCenter, Multiply(massData.center, massData.mass));
      {$ENDIF}
      m_I := m_I + massData.I;
      f := f.m_next;
   end;

   // Compute center of mass.
   if m_mass > 0.0 then
   begin
      m_invMass := 1.0 / m_mass;
      {$IFDEF OP_OVERLOAD}
      locatCenter.MultiplyBy(m_invMass);
      {$ELSE}
      MultiplyBy(locatCenter, m_invMass);
      {$ENDIF}
   end
   else
   begin
		  // Force all dynamic bodies to have a positive mass.
		  m_mass := 1.0;
		  m_invMass := 1.0;
   end;

   if (m_I > 0.0) and ((m_flags and e_body_fixedRotationFlag) = 0) then
   begin
      // Center the inertia about the center of mass.
      m_I := m_I - m_mass * b2Dot(locatCenter, locatCenter);
      //b2Assert(m_I > 0.0f);
      m_invI := 1.0 / m_I;
   end
   else
   begin
      m_I := 0.0;
      m_invI := 0.0;
   end;

   // Move center of mass.
   oldCenter := m_sweep.c;
   m_sweep.localCenter := locatCenter;
   m_sweep.c := b2Mul(m_xf, m_sweep.localCenter);
   m_sweep.c0 := m_sweep.c;

	 // Update center of mass velocity.
   {$IFDEF OP_OVERLOAD}
	 m_linearVelocity.AddBy(b2Cross(m_angularVelocity, m_sweep.c - oldCenter));
   {$ELSE}
   AddBy(m_linearVelocity, b2Cross(m_angularVelocity, Subtract(m_sweep.c, oldCenter)));
   {$ENDIF}

   ComputeStoredInertia;
end;

procedure Tb2Body.SetType(atype: Tb2BodyType);
var
   i: Integer;
   f: Tb2Fixture;
   proxyCount: Int32;
   ce, ce0: Pb2ContactEdge;
   broadPhase: Tb2BroadPhase;
begin
	 //b2Assert(m_world->IsLocked() == false);
	 if m_world.IsLocked then
      Exit;

   if m_type = atype then
      Exit;

   m_type := atype;
   ResetMassData;

   if m_type = b2_staticBody then
   begin
      m_linearVelocity := b2Vec2_Zero;
      m_angularVelocity := 0.0;
      m_sweep.a0 := m_sweep.a;
      m_sweep.c0 := m_sweep.c;
      SynchronizeFixtures;
   end;

   SetAwake(True);
   m_force := b2Vec2_Zero;
   m_torque := 0.0;

   // Delete the attached contacts.
   ce := m_contactList;
   while Assigned(ce) do
   begin
      ce0 := ce;
      ce := ce^.next;
      m_world.m_contactManager.Destroy(ce0.contact);
   end;
   m_contactList := nil;

   // Touch the proxies so that new contacts will be created (when appropriate)
   broadPhase := m_world.m_contactManager.m_broadPhase;
   f := m_fixtureList;
   while Assigned(f) do
   begin
      proxyCount := f.m_proxyCount;
      for i := 0 to proxyCount - 1 do
         broadPhase.TouchProxy(f.m_proxies[i].proxyId);
      f := f.m_next;
   end;
end;

function Tb2Body.GetWorldPoint(const localPoint: TVector2): TVector2;
begin
   Result := b2Mul(m_xf, localPoint);
end;

function Tb2Body.GetWorldVector(const localVector: TVector2): TVector2;
begin
   Result := b2Mul(m_xf.q, localVector);
end;

function Tb2Body.GetLocalPoint(const worldPoint: TVector2): TVector2;
begin
   Result := b2MulT(m_xf, worldPoint);
end;

function Tb2Body.GetLocalVector(const worldVector: TVector2): TVector2;
begin
   Result := b2MulT(m_xf.q, worldVector);
end;

function Tb2Body.GetLinearVelocityFromWorldPoint(
   const worldPoint: TVector2): TVector2;
begin
   {$IFDEF OP_OVERLOAD}
   Result := m_linearVelocity + b2Cross(m_angularVelocity, worldPoint - m_sweep.c);
   {$ELSE}
   Result := Add(m_linearVelocity, b2Cross(m_angularVelocity,
      Subtract(worldPoint, m_sweep.c)));
   {$ENDIF}
end;

function Tb2Body.GetLinearVelocityFromLocalPoint(
   const localPoint: TVector2): TVector2;
begin
   Result := GetLinearVelocityFromWorldPoint(GetWorldPoint(localPoint));
end;

function Tb2Body.IsBullet: Boolean;
begin
   Result := (m_flags and e_body_bulletFlag) = e_body_bulletFlag;
end;

procedure Tb2Body.SetBullet(flag: Boolean);
begin
   if flag then
      m_flags := m_flags or e_body_bulletFlag
   else
      m_flags := m_flags and (not e_body_bulletFlag);
end;

procedure Tb2Body.SetSleepingAllowed(flag: Boolean);
begin
   if flag then
      m_flags := m_flags or e_body_autoSleepFlag
   else
   begin
      m_flags := m_flags and (not e_body_autoSleepFlag);
		  SetAwake(True);
   end;
end;

function Tb2Body.IsSleepingAllowed: Boolean;
begin
   Result := (m_flags and e_body_autoSleepFlag) = e_body_autoSleepFlag;
end;

procedure Tb2Body.SetAwake(flag: Boolean);
begin
   if flag then
   begin
      if m_flags and e_body_awakeFlag = 0 then
      begin
         m_flags := m_flags or e_body_awakeFlag;
         m_sleepTime := 0.0;
      end;
   end
   else
   begin
      m_flags := m_flags and (not e_body_awakeFlag);
      m_sleepTime := 0.0;
      m_linearVelocity := b2Vec2_Zero;
      m_angularVelocity := 0.0;
      m_force := b2Vec2_Zero;
      m_torque := 0.0;
   end;
end;

function Tb2Body.IsAwake: Boolean;
begin
   Result := (m_flags and e_body_awakeFlag) = e_body_awakeFlag;
end;

procedure Tb2Body.SetActive(flag: Boolean);
var
   f: Tb2Fixture;
   ce, ce0: Pb2ContactEdge;
begin
   //b2Assert(m_world->IsLocked() == false);
   if flag = IsActive then
      Exit;

   if flag then
   begin
      m_flags := m_flags or e_body_activeFlag;

      // Create all proxies.
      f := m_fixtureList;
      while Assigned(f) do
      begin
         f.CreateProxies(m_world.m_contactManager.m_broadPhase, m_xf);
         f := f.m_next;
      end;
      // Contacts are created the next time step.
   end
   else
   begin
      m_flags := m_flags and (not e_body_activeFlag);

      // Destroy all proxies.
      f := m_fixtureList;
      while Assigned(f) do
      begin
         f.DestroyProxies(m_world.m_contactManager.m_broadPhase);
         f := f.m_next;
      end;

      // Destroy the attached contacts.
      ce := m_contactList;
      while Assigned(ce) do
      begin
         ce0 := ce;
         ce := ce^.next;
         m_world.m_contactManager.Destroy(ce0^.contact);
      end;
      m_contactList := nil;
   end;
end;

function Tb2Body.IsActive: Boolean;
begin
   Result := (m_flags and e_body_activeFlag) = e_body_activeFlag;
end;

procedure Tb2Body.SetIgnoreColliding(flag: Boolean);
begin
   if flag then
      m_flags := m_flags or e_body_ignoreCollideFlag
   else
      m_flags := m_flags and (not e_body_ignoreCollideFlag);
end;

function Tb2Body.IsCollidingIgnored: Boolean;
begin
   Result := (m_flags and e_body_ignoreCollideFlag) = e_body_ignoreCollideFlag;
end;

procedure Tb2Body.SetFixedRotation(flag: Boolean);
var
  status: Boolean;
begin
   status := (m_flags and e_body_fixedRotationFlag) = e_body_fixedRotationFlag;
     if status = flag then
   Exit;

   if flag then
      m_flags := m_flags or e_body_fixedRotationFlag
   else
      m_flags := m_flags and (not e_body_fixedRotationFlag);

m_angularVelocity := 0.0;

   ResetMassData;
end;

function Tb2Body.IsFixedRotation: Boolean;
begin
   Result := (m_flags and e_body_fixedRotationFlag) = e_body_fixedRotationFlag;
end;

///////////////////////////////////////////////
// Specific implementations

/// Compute the point states given two manifolds. The states pertain to the transition from manifold1
/// to manifold2. So state1 is either persist or remove while state2 is either add or persist.
procedure b2GetPointStates(var state1, state2: Tb2PointStateArray;
   const manifold1, manifold2: Tb2Manifold);
var
   i, j: Integer;
   key: UInt32;
begin
   for i := 0 to High(state1) do
   begin
      state1[i] := b2_nullState;
      state2[i] := b2_nullState;
   end;

   // Detect persists and removes.
   for i := 0 to manifold1.pointCount - 1 do
   begin
      key := manifold1.points[i].id.key;
      state1[i] := b2_removeState;
      for j := 0 to manifold2.pointCount - 1 do
         if manifold2.points[j].id.key = key then
         begin
            state1[i] := b2_persistState;
            Break;
         end;
   end;

   // Detect persists and adds.
   for i := 0 to manifold2.pointCount - 1 do
   begin
      key := manifold2.points[i].id.key;
      state2[i] := b2_addState;
      for j := 0 to manifold1.pointCount - 1 do
         if manifold1.points[j].id.key = key then
         begin
            state2[i] := b2_persistState;
            Break;
         end;
   end;
end;

procedure b2CollideCircles(contact: Pb2Contact; var manifold: Tb2Manifold;
   A, B: TObject; const xfA, xfB: Tb2Transform; ABfixture: Boolean);
var
   pA, pB, d: TVector2;
   circleA, circleB: Tb2CircleShape;
begin
   if ABfixture then
   begin
      circleA := Tb2CircleShape(Tb2Fixture(A).m_shape);
      circleB := Tb2CircleShape(Tb2Fixture(B).m_shape);
   end
   else
   begin
      circleA := Tb2CircleShape(A);
      circleB := Tb2CircleShape(B);
   end;

   manifold.pointCount := 0;

   pA := b2Mul(xfA, circleA.m_p);
   pB := b2Mul(xfB, circleB.m_p);

   {$IFDEF OP_OVERLOAD}
   d := pB - pA;
   {$ELSE}
   d := Subtract(pB, pA);
   {$ENDIF}
   if b2Dot(d, d) > Sqr(circleA.m_radius + circleB.m_radius) then
      Exit;

   with manifold do
   begin
      manifoldType := e_manifold_circles;
      localPoint := circleA.m_p;
      localNormal := b2Vec2_Zero;
      pointCount := 1;

      points[0].localPoint := circleB.m_p;
      points[0].id.key := 0;
   end;
end;

procedure b2CollidePolygonAndCircle(contact: Pb2Contact; var manifold: Tb2Manifold;
   A, B: TObject; const xfA, xfB: Tb2Transform; ABfixture: Boolean);
var
   circle: Tb2CircleShape;
   polygon: Tb2PolygonShape;
   i: Integer;
   c, cLocal, v1, v2, faceCenter: TVector2;
   normalIndex: Int32;
   _separation, radius: PhysicsFloat;
   s, u1, u2: PhysicsFloat;
begin
   if ABfixture then
   begin
      polygon := Tb2PolygonShape(Tb2Fixture(A).m_shape);
      circle := Tb2CircleShape(Tb2Fixture(B).m_shape);
   end
   else
   begin
      polygon := Tb2PolygonShape(A);
      circle := Tb2CircleShape(B);
   end;

   manifold.pointCount := 0;
   // Compute circle position in the frame of the polygon.
   c := b2Mul(xfB, circle.m_p);
   cLocal := b2MulT(xfA, c);

   // Find the min separating edge.
   normalIndex := 0;
   _separation := -FLT_MAX;
   radius := circle.m_radius + polygon.m_radius;

   for i := 0 to polygon.m_count - 1 do
   begin
      {$IFDEF OP_OVERLOAD}
      s := b2Dot(polygon.m_normals[i], cLocal - polygon.m_vertices[i]);
      {$ELSE}
      s := b2Dot(polygon.m_normals[i], Subtract(cLocal, polygon.m_vertices[i]));
      {$ENDIF}
      if s > radius then
         Exit;

      if s > _separation then
      begin
         _separation := s;
         normalIndex := i;
      end;
   end;

   // Vertices that subtend the incident face.
   v1 := polygon.m_vertices[normalIndex];
   if normalIndex + 1 < polygon.m_count then
      v2 := polygon.m_vertices[normalIndex + 1]
   else
      v2 := polygon.m_vertices[0];

   // If the center is inside the polygon ...
   if _separation < FLT_EPSILON then
      with manifold do
      begin
         pointCount := 1;
         manifoldType := e_manifold_faceA;
         localNormal := polygon.m_normals[normalIndex];
         localPoint := b2MiddlePoint(v1, v2);
         points[0].localPoint := circle.m_p;
         points[0].id.key := 0;
         Exit;
      end;

   // Compute barycentric coordinates
   {$IFDEF OP_OVERLOAD}
   u1 := b2Dot(cLocal - v1, v2 - v1);
   u2 := b2Dot(cLocal - v2, v1 - v2);
   {$ELSE}
   u1 := b2Dot(Subtract(cLocal, v1), Subtract(v2, v1));
   u2 := b2Dot(Subtract(cLocal, v2), Subtract(v1, v2));
   {$ENDIF}
   if u1 <= 0.0 then
   begin
      if b2DistanceSquared(cLocal, v1) > radius * radius then
         Exit;

      with manifold do
      begin
         pointCount := 1;
         manifoldType := e_manifold_faceA;
         {$IFDEF OP_OVERLOAD}
         localNormal := cLocal - v1;
         localNormal.Normalize;
         {$ELSE}
         localNormal := Subtract(cLocal, v1);
         Normalize(localNormal);
         {$ENDIF}
         localPoint := v1;
         points[0].localPoint := circle.m_p;
         points[0].id.key := 0;
      end;
   end
   else if u2 <= 0.0 then
   begin
      if b2DistanceSquared(cLocal, v2) > radius * radius then
         Exit;

      with manifold do
      begin
         pointCount := 1;
         manifoldType := e_manifold_faceA;
         {$IFDEF OP_OVERLOAD}
         localNormal := cLocal - v2;
         localNormal.Normalize;
         {$ELSE}
         localNormal := Subtract(cLocal, v2);
         Normalize(localNormal);
         {$ENDIF}
         localPoint := v2;
         points[0].localPoint := circle.m_p;
         points[0].id.key := 0;
      end;
   end
   else
   begin
      faceCenter := b2MiddlePoint(v1, v2);
      {$IFDEF OP_OVERLOAD}
      _separation := b2Dot(cLocal - faceCenter, polygon.m_normals[normalIndex]);
      {$ELSE}
      _separation := b2Dot(Subtract(cLocal, faceCenter), polygon.m_normals[normalIndex]);
      {$ENDIF}
      if _separation > radius then
         Exit;

      with manifold do
      begin
         pointCount := 1;
         manifoldType := e_manifold_faceA;
         localNormal := polygon.m_normals[normalIndex];
         localPoint := faceCenter;
         points[0].localPoint := circle.m_p;
         points[0].id.key := 0;
      end;
   end;
end;

type
   Tb2EdgeType = (b2_isolated, b2_concave, b2_flat, b2_convex);

/// Compute the collision manifold between an edge and a circle.
procedure b2CollideEdgeAndCircle(contact: Pb2Contact; var manifold: Tb2Manifold;
   A, B: TObject; const xfA, xfB: Tb2Transform; ABfixture: Boolean);
var
   edgeA: Tb2EdgeShape;
   circleB: Tb2CircleShape;
   Q, pA, pB, e, P, d, n: TVector2;
   u, v, radius, dd, den: PhysicsFloat;
   cf: Tb2ContactFeature;
begin
   if ABfixture then
   begin
      edgeA := Tb2EdgeShape(Tb2Fixture(A).m_shape);
      circleB := Tb2CircleShape(Tb2Fixture(B).m_shape);
   end
   else
   begin
      edgeA := Tb2EdgeShape(A);
      circleB := Tb2CircleShape(B);
   end;

   manifold.pointCount := 0;

   // Compute circle in frame of edge
   Q := b2MulT(xfA, b2Mul(xfB, circleB.m_p));

   pA := edgeA.m_vertex1;
   pB := edgeA.m_vertex2;
   {$IFDEF OP_OVERLOAD}
   e := pB - pA;

   // Barycentric coordinates
   u := b2Dot(e, pB - Q);
   v := b2Dot(e, Q - pA);
   {$ELSE}
   e := Subtract(pB, pA);

   // Barycentric coordinates
   u := b2Dot(e, Subtract(pB, Q));
   v := b2Dot(e, Subtract(Q, pA));
   {$ENDIF}

   radius := edgeA.m_radius + circleB.m_radius;

   cf.indexB := 0;
   cf.typeB := e_contact_feature_vertex;

   // Region pA
   if v <= 0.0 then
   begin
      {$IFDEF OP_OVERLOAD}
      d := Q - pA;
      {$ELSE}
      d := Subtract(Q, pA);
      {$ENDIF}
      dd := b2Dot(d, d);
      if dd > radius * radius then
         Exit;

      // Is there an edge connected to pA?
      if edgeA.m_hasVertex0 then
         // Is the circle in Region AB of the previous edge?
         {$IFDEF OP_OVERLOAD}
         if b2Dot(pA - edgeA.m_vertex0, pA - Q) > 0.0 then
         {$ELSE}
         if b2Dot(Subtract(pA, edgeA.m_vertex0), Subtract(pA, Q)) > 0.0 then
         {$ENDIF}
            Exit;

      cf.indexA := 0;
      cf.typeA := e_contact_feature_vertex;
      with manifold do
      begin
         pointCount := 1;
         manifoldType := e_manifold_circles;
         localNormal := b2Vec2_Zero;
         localPoint := pA;
         points[0].id.key := 0;
         points[0].id.cf := cf;
         points[0].localPoint := circleB.m_p;
      end;
      Exit;
   end;

   // Region pB
   if u <= 0.0 then
   begin
      {$IFDEF OP_OVERLOAD}
      d := Q - pB;
      {$ELSE}
      d := Subtract(Q, pB);
      {$ENDIF}
      dd := b2Dot(d, d);
      if dd > radius * radius then
         Exit;

      // Is there an edge connected to pB?
      if edgeA.m_hasVertex3 then
         // Is the circle in Region AB of the next edge?
         {$IFDEF OP_OVERLOAD}
         if b2Dot(edgeA.m_vertex3 - pB, Q - pB) > 0.0 then
         {$ELSE}
         if b2Dot(Subtract(edgeA.m_vertex3, pB), Subtract(Q, pB)) > 0.0 then
         {$ENDIF}
            Exit;

      cf.indexA := 1;
      cf.typeA := e_contact_feature_vertex;
      with manifold do
      begin
         pointCount := 1;
         manifoldType := e_manifold_circles;
         localNormal := b2Vec2_Zero;
         localPoint := pB;
         points[0].id.key := 0;
         points[0].id.cf := cf;
         points[0].localPoint := circleB.m_p;
      end;
      Exit;
   end;

   // Region AB
   den := b2Dot(e, e);
   //b2Assert(den > 0.0f);
   {$IFDEF OP_OVERLOAD}
   P := (1.0 / den) * (u * pA + v * pB);
   d := Q - P;
   {$ELSE}
   P := Multiply(Add(Multiply(pA, u), Multiply(pB, v)), 1.0 / den);
   d := Subtract(Q, P);
   {$ENDIF}
   dd := b2Dot(d, d);
   if dd > radius * radius then
      Exit;

   n.x := -e.y;
   n.y := e.x;
   {$IFDEF OP_OVERLOAD}
   if b2Dot(n, Q - pA) < 0.0 then
   {$ELSE}
   if b2Dot(n, Subtract(Q, pA)) < 0.0 then
   {$ENDIF}
     SetValue(n, -n.x, -n.y);
   {$IFDEF OP_OVERLOAD}
   n.Normalize;
   {$ELSE}
   Normalize(n);
   {$ENDIF}

   cf.indexA := 0;
   cf.typeA := e_contact_feature_face;
   with manifold do
   begin
      pointCount := 1;
      manifoldType := e_manifold_faceA;
      localNormal := n;
      localPoint := pA;
      points[0].id.key := 0;
      points[0].id.cf := cf;
      points[0].localPoint := circleB.m_p;
   end;
end;

const
   e_ep_unknown = 0;
   e_ep_edgeA = 1;
   e_ep_edgeB = 2;

type
   // This structure is used to keep track of the best separating axis.
   Tb2EPAxis = record
      AxisType: Byte;
      index: Int32;
      separation: PhysicsFloat;
   end;

   // This holds polygon B expressed in frame A.
   Tb2TempPolygon = record
      vertices: Tb2PolyVertices;
      normals: Tb2PolyVertices;
      count: Int32;
   end;

   // Reference face used for clipping
   Tb2ReferenceFace = record
      i1, i2: Int32;
      v1, v2: TVector2;
      normal: TVector2;
      sideNormal1: TVector2;
      sideOffset1: PhysicsFloat;
      sideNormal2: TVector2;
      sideOffset2: PhysicsFloat;
   end;

   /// Used for computing contact manifolds.
   Pb2ClipVertex = ^Tb2ClipVertex;
   Tb2ClipVertex = record
      v: TVector2;
      id: Tb2ContactID;
   end;

   Pb2ClipVertices = ^Tb2ClipVertices;
   Tb2ClipVertices = array[0..1] of Tb2ClipVertex;

   // This class collides and edge and a polygon, taking into account edge adjacency.
const
   e_vt_isolated = 0;
   e_vt_concave = 1;
   e_vt_convex = 2;

type
   Tb2EPCollider = class
   public
      m_polygonB: Tb2TempPolygon;
      m_xf: Tb2Transform;
      m_centroidB: TVector2;
      m_v0, m_v1, m_v2, m_v3: TVector2;
      m_normal0, m_normal1, m_normal2: TVector2;
      m_normal: TVector2;
      m_type1, m_type2: Int32;
      m_lowerLimit, m_upperLimit: TVector2;
      m_radius: PhysicsFloat;
      m_front: Boolean;

      procedure Collide(var manifold: Tb2Manifold; edgeA: Tb2EdgeShape;
         polygonB: Tb2PolygonShape; const xfA, xfB: Tb2Transform);
      function ComputeEdgeSeparation: Tb2EPAxis;
      function ComputePolygonSeparation: Tb2EPAxis;
   end;

procedure b2FindIncidentEdge(var c: Tb2ClipVertices; poly1, poly2: Tb2PolygonShape;
   edge1: Int32); overload;
var
   i: Integer;
   index, i1, i2: Int32;
   normal1: TVector2;
   minDot, dot: PhysicsFloat;
begin
   //b2Assert(0 <= edge1 && edge1 < poly1.m_vertexCount);

   // Get the normal of the reference edge in poly2's frame.
   normal1 := poly1.m_normals[edge1];

   // Find the incident edge on poly2.
   index := 0;
   minDot := FLT_MAX;
   for i := 0 to poly2.m_count - 1 do
   begin
      dot := b2Dot(normal1, poly2.m_normals[i]);
      if dot < minDot then
      begin
         minDot := dot;
         index := i;
      end;
   end;

   // Build the clip vertices for the incident edge.
   i1 := index;
   if i1 + 1 < poly2.m_count then
      i2 := i1 + 1
   else
      i2 := 0;

   with c[0], id.cf do
   begin
      v := poly2.m_vertices[i1];
      indexA := edge1;
      indexB := i1;
      typeA := e_contact_feature_face;
      typeB := e_contact_feature_vertex;
   end;

   with c[1], id.cf do
   begin
      v := poly2.m_vertices[i2];
      indexA := edge1;
      indexB := i2;
      typeA := e_contact_feature_face;
      typeB := e_contact_feature_vertex;
   end;
end;

/// Clipping for contact manifolds.
function b2ClipSegmentToLine(var vOut: Tb2ClipVertices; const vIn: Tb2ClipVertices;
   const normal: TVector2; offset: PhysicsFloat; vertexIndexA: Int32): Int32;
var
   distance0, distance1, interp: PhysicsFloat;
begin
   Result := 0; // Start with no output points

   // Calculate the distance of end points to the line
   distance0 := b2Dot(normal, vIn[0].v) - offset;
   distance1 := b2Dot(normal, vIn[1].v) - offset;

   // If the points are behind the plane
   if distance0 <= 0.0 then
   begin
      vOut[Result] := vIn[0];
      Inc(Result);
   end;
   if distance1 <= 0.0 then
   begin
      vOut[Result] := vIn[1];
      Inc(Result);
   end;

   // If the points are on different sides of the plane
   if distance0 * distance1 < 0.0 then
   begin
      // Find intersection point of edge and plane
      interp := distance0 / (distance0 - distance1);
      {$IFDEF OP_OVERLOAD}
      vOut[Result].v := vIn[0].v + interp * (vIn[1].v - vIn[0].v);
      {$ELSE}
      vOut[Result].v := Add(vIn[0].v, Multiply(Subtract(vIn[1].v, vIn[0].v), interp));
      {$ENDIF}

      // VertexA is hitting edgeB.
      with vOut[Result].id.cf do
      begin
         indexA := vertexIndexA;
         indexB := vIn[0].id.cf.indexB;
         typeA := e_contact_feature_vertex;
         typeB := e_contact_feature_face;
      end;
      Inc(Result);
   end;
end;

{ Tb2EPCollider }

// Algorithm:
// 1. Classify v1 and v2
// 2. Classify polygon centroid as front or back
// 3. Flip normal if necessary
// 4. Initialize normal range to [-pi, pi] about face normal
// 5. Adjust normal range according to adjacent edges
// 6. Visit each separating axes, only accept axes within the range
// 7. Return if _any_ axis indicates separation
// 8. Clip
procedure Tb2EPCollider.Collide(var manifold: Tb2Manifold; edgeA: Tb2EdgeShape;
   polygonB: Tb2PolygonShape; const xfA, xfB: Tb2Transform);
const
   // Use hysteresis for jitter reduction.
   k_relativeTol = 0.98;
   k_absoluteTol = 0.001;
var
   i: Integer;
   hasVertex0, hasVertex3, convex1, convex2: Boolean;
   edge0, edge1, edge2: TVector2;
   offset0, offset1, offset2: PhysicsFloat;
   edgeAxis, polygonAxis, primaryAxis: Tb2EPAxis;
   ie: Tb2ClipVertices;
   rf: Tb2ReferenceFace;
   bestIndex: Int32;
   bestValue, value: PhysicsFloat;
   i1, i2: Int32;
   clipPoints1, clipPoints2: Tb2ClipVertices;
   np, pointCount: Int32;
   separation: PhysicsFloat;
   cp: Pb2ManifoldPoint;
begin
   m_xf := b2MulT(xfA, xfB);

   m_centroidB := b2Mul(m_xf, polygonB.m_centroid);

   m_v0 := edgeA.m_vertex0;
   m_v1 := edgeA.m_vertex1;
   m_v2 := edgeA.m_vertex2;
   m_v3 := edgeA.m_vertex3;

   hasVertex0 := edgeA.m_hasVertex0;
   hasVertex3 := edgeA.m_hasVertex3;

   {$IFDEF OP_OVERLOAD}
   edge1 := m_v2 - m_v1;
   edge1.Normalize;
   SetValue(m_normal1, edge1.y, -edge1.x);
   offset1 := b2Dot(m_normal1, m_centroidB - m_v1);
   {$ELSE}
   edge1 := Subtract(m_v2, m_v1);
   Normalize(edge1);
   SetValue(m_normal1, edge1.y, -edge1.x);
   offset1 := b2Dot(m_normal1, Subtract(m_centroidB, m_v1));
   {$ENDIF}
   offset0 := 0.0;
   offset2 := 0.0;
   convex1 := False;
   convex2 := False;

   // Is there a preceding edge?
   if hasVertex0 then
   begin
      {$IFDEF OP_OVERLOAD}
      edge0 := m_v1 - m_v0;
      edge0.Normalize;
      SetValue(m_normal0, edge0.y, -edge0.x);
      convex1 := b2Cross(edge0, edge1) >= 0.0;
      offset0 := b2Dot(m_normal0, m_centroidB - m_v0);
      {$ELSE}
      edge0 := Subtract(m_v1, m_v0);
      Normalize(edge0);
      SetValue(m_normal0, edge0.y, -edge0.x);
      convex1 := b2Cross(edge0, edge1) >= 0.0;
      offset0 := b2Dot(m_normal0, Subtract(m_centroidB, m_v0));
      {$ENDIF}
   end;

   // Is there a following edge?
   if hasVertex3 then
   begin
      {$IFDEF OP_OVERLOAD}
      edge2 := m_v3 - m_v2;
      edge2.Normalize;
      SetValue(m_normal2, edge2.y, -edge2.x);
      convex2 := b2Cross(edge1, edge2) > 0.0;
      offset2 := b2Dot(m_normal2, m_centroidB - m_v2);
      {$ELSE}
      edge2 := Subtract(m_v3, m_v2);
      Normalize(edge2);
      SetValue(m_normal2, edge2.y, -edge2.x);
      convex2 := b2Cross(edge1, edge2) > 0.0;
      offset2 := b2Dot(m_normal2, Subtract(m_centroidB, m_v2));
      {$ENDIF}
   end;

   // Determine front or back collision. Determine collision normal limits.
   if hasVertex0 and hasVertex3 then
   begin
      if convex1 and convex2 then
      begin
         m_front := (offset0 >= 0.0) or (offset1 >= 0.0) or (offset2 >= 0.0);
         if m_front then
         begin
            m_normal := m_normal1;
            m_lowerLimit := m_normal0;
            m_upperLimit := m_normal2;
         end
         else
         begin
            {$IFDEF OP_OVERLOAD}
            m_normal := -m_normal1;
            m_lowerLimit := -m_normal1;
            m_upperLimit := -m_normal1;
            {$ELSE}
            m_normal := Negative(m_normal1);
            m_lowerLimit := Negative(m_normal1);
            m_upperLimit := Negative(m_normal1);
            {$ENDIF}
         end;
      end
      else if convex1 then
      begin
         m_front := (offset0 >= 0.0) or ((offset1 >= 0.0) and (offset2 >= 0.0));
         if m_front then
         begin
            m_normal := m_normal1;
            m_lowerLimit := m_normal0;
            m_upperLimit := m_normal1;
         end
         else
         begin
            {$IFDEF OP_OVERLOAD}
            m_normal := -m_normal1;
            m_lowerLimit := -m_normal2;
            m_upperLimit := -m_normal1;
            {$ELSE}
            m_normal := Negative(m_normal1);
            m_lowerLimit := Negative(m_normal2);
            m_upperLimit := Negative(m_normal1);
            {$ENDIF}
         end;
      end
      else if convex2 then
      begin
         m_front := (offset2 >= 0.0) or ((offset0 >= 0.0) and (offset1 >= 0.0));
         if m_front then
         begin
            m_normal := m_normal1;
            m_lowerLimit := m_normal1;
            m_upperLimit := m_normal2;
         end
         else
         begin
            {$IFDEF OP_OVERLOAD}
            m_normal := -m_normal1;
            m_lowerLimit := -m_normal1;
            m_upperLimit := -m_normal0;
            {$ELSE}
            m_normal := Negative(m_normal1);
            m_lowerLimit := Negative(m_normal1);
            m_upperLimit := Negative(m_normal0);
            {$ENDIF}
         end;
      end
      else
      begin
         m_front := (offset0 >= 0.0) and (offset1 >= 0.0) and (offset2 >= 0.0);
         if m_front then
         begin
            m_normal := m_normal1;
            m_lowerLimit := m_normal1;
            m_upperLimit := m_normal1;
         end
         else
         begin
            {$IFDEF OP_OVERLOAD}
            m_normal := -m_normal1;
            m_lowerLimit := -m_normal2;
            m_upperLimit := -m_normal0;
            {$ELSE}
            m_normal := Negative(m_normal1);
            m_lowerLimit := Negative(m_normal2);
            m_upperLimit := Negative(m_normal0);
            {$ENDIF}
         end;
      end;
   end
   else if hasVertex0 then
   begin
      if convex1 then
      begin
         m_front := (offset0 >= 0.0) or (offset1 >= 0.0);
         if m_front then
         begin
            m_normal := m_normal1;
            m_lowerLimit := m_normal0;
            {$IFDEF OP_OVERLOAD}
            m_upperLimit := -m_normal1;
            {$ELSE}
            m_upperLimit := Negative(m_normal1);
            {$ENDIF}
         end
         else
         begin
            {$IFDEF OP_OVERLOAD}
            m_normal := -m_normal1;
            m_lowerLimit := m_normal1;
            m_upperLimit := -m_normal1;
            {$ELSE}
            m_normal := Negative(m_normal1);
            m_lowerLimit := m_normal1;
            m_upperLimit := Negative(m_normal1);
            {$ENDIF}
         end;
      end
      else
      begin
         m_front := (offset0 >= 0.0) and (offset1 >= 0.0);
         if m_front then
         begin
            m_normal := m_normal1;
            m_lowerLimit := m_normal1;
            {$IFDEF OP_OVERLOAD}
            m_upperLimit := -m_normal1;
            {$ELSE}
            m_upperLimit := Negative(m_normal1);
            {$ENDIF}
         end
         else
         begin
            {$IFDEF OP_OVERLOAD}
            m_normal := -m_normal1;
            m_lowerLimit := m_normal1;
            m_upperLimit := -m_normal0;
            {$ELSE}
            m_normal := Negative(m_normal1);
            m_lowerLimit := m_normal1;
            m_upperLimit := Negative(m_normal0);
            {$ENDIF}
         end;
      end;
   end
   else if hasVertex3 then
   begin
      if convex2 then
      begin
         m_front := (offset1 >= 0.0) or (offset2 >= 0.0);
         if m_front then
         begin
            m_normal := m_normal1;
            {$IFDEF OP_OVERLOAD}
            m_lowerLimit := -m_normal1;
            {$ELSE}
            m_lowerLimit := Negative(m_normal1);
            {$ENDIF}
            m_upperLimit := m_normal2;
         end
         else
         begin
            {$IFDEF OP_OVERLOAD}
            m_normal := -m_normal1;
            {$ELSE}
            m_normal := Negative(m_normal1);
            {$ENDIF}
            m_lowerLimit := m_normal;
            m_upperLimit := m_normal1;
         end;
      end
      else
      begin
         m_front := (offset1 >= 0.0) and (offset2 >= 0.0);
         if m_front then
         begin
            m_normal := m_normal1;
            {$IFDEF OP_OVERLOAD}
            m_lowerLimit := -m_normal1;
            {$ELSE}
            m_lowerLimit := Negative(m_normal1);
            {$ENDIF}
            m_upperLimit := m_normal1;
         end
         else
         begin
            {$IFDEF OP_OVERLOAD}
            m_normal := -m_normal1;
            m_lowerLimit := -m_normal2;
            {$ELSE}
            m_normal := Negative(m_normal1);
            m_lowerLimit := Negative(m_normal2);
            {$ENDIF}
            m_upperLimit := m_normal1;
         end;
      end;
   end
   else
   begin
      m_front := offset1 >= 0.0;
      if m_front then
      begin
         m_normal := m_normal1;
         {$IFDEF OP_OVERLOAD}
         m_lowerLimit := -m_normal1;
         {$ELSE}
         m_lowerLimit := Negative(m_normal1);
         {$ENDIF}
         m_upperLimit := m_lowerLimit;
      end
      else
      begin
         {$IFDEF OP_OVERLOAD}
         m_normal := -m_normal1;
         {$ELSE}
         m_normal := Negative(m_normal1);
         {$ENDIF}
         m_lowerLimit := m_normal1;
         m_upperLimit := m_normal1;
      end;
   end;

   // Get polygonB in frameA
   m_polygonB.count := polygonB.m_count;
   for i := 0 to polygonB.m_count - 1 do
   begin
      m_polygonB.vertices[i] := b2Mul(m_xf, polygonB.m_vertices[i]);
      m_polygonB.normals[i] := b2Mul(m_xf.q, polygonB.m_normals[i]);
   end;

   m_radius := 2.0 * b2_polygonRadius;
   manifold.pointCount := 0;

   edgeAxis := ComputeEdgeSeparation();

   // If no valid normal can be found than this edge should not collide.
   if edgeAxis.AxisType = e_ep_unknown then
      Exit;

   if edgeAxis.separation > m_radius then
      Exit;

   polygonAxis := ComputePolygonSeparation();
   if (polygonAxis.AxisType <> e_ep_unknown) and (polygonAxis.separation > m_radius) then
      Exit;

   if polygonAxis.AxisType = e_ep_unknown then
      primaryAxis := edgeAxis
   else if (polygonAxis.separation > k_relativeTol * edgeAxis.separation + k_absoluteTol) then
      primaryAxis := polygonAxis
   else
      primaryAxis := edgeAxis;

   if primaryAxis.AxisType = e_ep_edgeA then
   begin
      manifold.manifoldType := e_manifold_faceA;

      // Search for the polygon normal that is most anti-parallel to the edge normal.
      bestIndex := 0;
      bestValue := b2Dot(m_normal, m_polygonB.normals[0]);
      for i := 0 to m_polygonB.count - 1 do
      begin
         value := b2Dot(m_normal, m_polygonB.normals[i]);
         if value < bestValue then
         begin
            bestValue := value;
            bestIndex := i;
         end;
      end;

      i1 := bestIndex;
      if i1 + 1 < m_polygonB.count then
         i2 := i1 + 1
      else
         i2 := 0;

      ie[0].v := m_polygonB.vertices[i1];
      ie[0].id.cf.indexA := 0;
      ie[0].id.cf.indexB := i1;
      ie[0].id.cf.typeA := e_contact_feature_face;
      ie[0].id.cf.typeB := e_contact_feature_vertex;

      ie[1].v := m_polygonB.vertices[i2];
      ie[1].id.cf.indexA := 0;
      ie[1].id.cf.indexB := i2;
      ie[1].id.cf.typeA := e_contact_feature_face;
      ie[1].id.cf.typeB := e_contact_feature_vertex;

      if m_front then
      begin
         rf.i1 := 0;
         rf.i2 := 1;
         rf.v1 := m_v1;
         rf.v2 := m_v2;
         rf.normal := m_normal1;
      end
      else
      begin
         rf.i1 := 1;
         rf.i2 := 0;
         rf.v1 := m_v2;
         rf.v2 := m_v1;
         {$IFDEF OP_OVERLOAD}
         rf.normal := -m_normal1;
         {$ELSE}
         rf.normal := Negative(m_normal1);
         {$ENDIF}
      end;
   end
   else
   begin
      manifold.manifoldType := e_manifold_faceB;

      ie[0].v := m_v1;
      ie[0].id.cf.indexA := 0;
      ie[0].id.cf.indexB := primaryAxis.index;
      ie[0].id.cf.typeA := e_contact_feature_vertex;
      ie[0].id.cf.typeB := e_contact_feature_face;

      ie[1].v := m_v2;
      ie[1].id.cf.indexA := 0;
      ie[1].id.cf.indexB := primaryAxis.index;
      ie[1].id.cf.typeA := e_contact_feature_vertex;
      ie[1].id.cf.typeB := e_contact_feature_face;

      rf.i1 := primaryAxis.index;
      if rf.i1 + 1 < m_polygonB.count then
         rf.i2 := rf.i1 + 1
      else
         rf.i2 := 0;
      rf.v1 := m_polygonB.vertices[rf.i1];
      rf.v2 := m_polygonB.vertices[rf.i2];
      rf.normal := m_polygonB.normals[rf.i1];
   end;

   SetValue(rf.sideNormal1, rf.normal.y, -rf.normal.x);
   {$IFDEF OP_OVERLOAD}
   rf.sideNormal2 := -rf.sideNormal1;
   {$ELSE}
   rf.sideNormal2 := Negative(rf.sideNormal1);
   {$ENDIF}
   rf.sideOffset1 := b2Dot(rf.sideNormal1, rf.v1);
   rf.sideOffset2 := b2Dot(rf.sideNormal2, rf.v2);

   // Clip incident edge against extruded edge1 side edges.
   // Clip to box side 1
   np := b2ClipSegmentToLine(clipPoints1, ie, rf.sideNormal1, rf.sideOffset1, rf.i1);

   if np < b2_maxManifoldPoints then
      Exit;

   // Clip to negative box side 1
   np := b2ClipSegmentToLine(clipPoints2, clipPoints1, rf.sideNormal2, rf.sideOffset2, rf.i2);

   if np < b2_maxManifoldPoints then
      Exit;

   // Now clipPoints2 contains the clipped points.
   if primaryAxis.AxisType = e_ep_edgeA then
   begin
      manifold.localNormal := rf.normal;
      manifold.localPoint := rf.v1;
   end
   else
   begin
      manifold.localNormal := polygonB.m_normals[rf.i1];
      manifold.localPoint := polygonB.m_vertices[rf.i1];
   end;

   pointCount := 0;
   for i := 0 to b2_maxManifoldPoints - 1 do
   begin
      {$IFDEF OP_OVERLOAD}
      separation := b2Dot(rf.normal, clipPoints2[i].v - rf.v1);
      {$ELSE}
      separation := b2Dot(rf.normal, Subtract(clipPoints2[i].v, rf.v1));
      {$ENDIF}

      if separation <= m_radius then
      begin
         cp := @manifold.points[pointCount];
         if primaryAxis.AxisType = e_ep_edgeA then
         begin
            cp^.localPoint := b2MulT(m_xf, clipPoints2[i].v);
            cp^.id := clipPoints2[i].id;
         end
         else
         begin
            cp^.localPoint := clipPoints2[i].v;
            cp^.id.cf.typeA := clipPoints2[i].id.cf.typeB;
            cp^.id.cf.typeB := clipPoints2[i].id.cf.typeA;
            cp^.id.cf.indexA := clipPoints2[i].id.cf.indexB;
            cp^.id.cf.indexB := clipPoints2[i].id.cf.indexA;
         end;

         Inc(pointCount);
      end;
   end;

   manifold.pointCount := pointCount;
end;

function Tb2EPCollider.ComputeEdgeSeparation: Tb2EPAxis;
var
   i: Integer;
   s: PhysicsFloat;
begin
   Result.AxisType := e_ep_edgeA;
   if m_front then
      Result.index := 0
   else
      Result.index := 1;
   Result.separation := FLT_MAX;

   for i := 0 to m_polygonB.count - 1 do
   begin
      {$IFDEF OP_OVERLOAD}
      s := b2Dot(m_normal, m_polygonB.vertices[i] - m_v1);
      {$ELSE}
      s := b2Dot(m_normal, Subtract(m_polygonB.vertices[i], m_v1));
      {$ENDIF}
      if s < Result.separation then
         Result.separation := s;
   end;
end;

function Tb2EPCollider.ComputePolygonSeparation: Tb2EPAxis;
var
   i: Integer;
   perp, n: TVector2;
   s1, s2, s: PhysicsFloat;
begin
   Result.AxisType := e_ep_unknown;
   Result.index := -1;
   Result.separation := -FLT_MAX;

   perp.x := -m_normal.y;
   perp.y := m_normal.x;

   for i := 0 to m_polygonB.count - 1 do
   begin
      {$IFDEF OP_OVERLOAD}
      n := -m_polygonB.normals[i];
      s1 := b2Dot(n, m_polygonB.vertices[i] - m_v1);
      s2 := b2Dot(n, m_polygonB.vertices[i] - m_v2);
      {$ELSE}
      n := Negative(m_polygonB.normals[i]);
      s1 := b2Dot(n, Subtract(m_polygonB.vertices[i], m_v1));
      s2 := b2Dot(n, Subtract(m_polygonB.vertices[i], m_v2));
      {$ENDIF}
      s := b2Min(s1, s2);

      if s > m_radius then
      begin
         // No collision
         Result.AxisType := e_ep_edgeB;
         Result.index := i;
         Result.separation := s;
         Exit;
      end;

      // Adjacency
      if b2Dot(n, perp) >= 0.0 then
      begin
         {$IFDEF OP_OVERLOAD}
         if b2Dot(n - m_upperLimit, m_normal) < -b2_angularSlop then
         {$ELSE}
         if b2Dot(Subtract(n, m_upperLimit), m_normal) < -b2_angularSlop then
         {$ENDIF}
            Continue;
      end
      else
      begin
         {$IFDEF OP_OVERLOAD}
         if b2Dot(n - m_lowerLimit, m_normal) < -b2_angularSlop then
         {$ELSE}
         if b2Dot(Subtract(n, m_lowerLimit), m_normal) < -b2_angularSlop then
         {$ENDIF}
            Continue;
      end;

      if s > Result.separation then
      begin
         Result.AxisType := e_ep_edgeB;
         Result.index := i;
         Result.separation := s;
      end;
   end;
end;

var
  ep_collieder: Tb2EPCollider;
procedure b2CollideEdgeAndPolygon(contact: Pb2Contact; var manifold: Tb2Manifold;
   A, B: TObject; const xfA, xfB: Tb2Transform; ABfixture: Boolean);
var
   edgeA: Tb2EdgeShape;
   polygonB: Tb2PolygonShape;
begin
   if ABfixture then
   begin
      edgeA := Tb2EdgeShape(Tb2Fixture(A).m_shape);
      polygonB := Tb2PolygonShape(Tb2Fixture(B).m_shape);
   end
   else
   begin
      edgeA := Tb2EdgeShape(A);
      polygonB := Tb2PolygonShape(B);
   end;

   ep_collieder.Collide(manifold, edgeA, polygonB, xfA, xfB);
end;

///////////////////////////////////////////////////////////////////

var
   static_edge_shape: Tb2EdgeShape;
procedure b2CollideChainAndCircle(contact: Pb2Contact; var manifold: Tb2Manifold;
   A, B: TObject; const xfA, xfB: Tb2Transform; ABfixture: Boolean);
var
   chain: Tb2ChainShape;
   circle: Tb2CircleShape;
begin
   if ABfixture then
   begin
      chain := Tb2ChainShape(Tb2Fixture(A).m_shape);
      circle := Tb2CircleShape(Tb2Fixture(B).m_shape);
   end
   else
   begin
      chain := Tb2ChainShape(A);
      circle := Tb2CircleShape(B);
   end;

   chain.GetChildEdge(static_edge_shape, contact^.m_indexA);
	 b2CollideEdgeAndCircle(contact,	manifold, static_edge_shape, circle, xfA, xfB, False);
end;

procedure b2CollideChainAndPolygon(contact: Pb2Contact; var manifold: Tb2Manifold;
   A, B: TObject; const xfA, xfB: Tb2Transform; ABfixture: Boolean);
var
   chain: Tb2ChainShape;
   poly: Tb2PolygonShape;
begin
   if ABfixture then
   begin
      chain := Tb2ChainShape(Tb2Fixture(A).m_shape);
      poly := Tb2PolygonShape(Tb2Fixture(B).m_shape);
   end
   else
   begin
      chain := Tb2ChainShape(A);
      poly := Tb2PolygonShape(B);
   end;

   chain.GetChildEdge(static_edge_shape, contact^.m_indexA);
	 b2CollideEdgeAndPolygon(contact,	manifold, static_edge_shape, poly, xfA, xfB, False);
end;

// Find the max separation between poly1 and poly2 using edge normals from poly1.
function b2FindMaxSeparation(var edgeIndex: Int32;
   poly1, poly2: Tb2PolygonShape; const xf1, xf2: Tb2Transform): PhysicsFloat;
var
   i, j: Integer;
   xf: Tb2Transform;
   bestIndex: Int32;
   n, v1: TVector2;
   maxSeparation, si, sij: PhysicsFloat;
begin
   xf := b2MulT(xf2, xf1);

   bestIndex := 0;
   maxSeparation := -FLT_MAX;
   for i := 0 to poly1.m_count - 1 do
   begin
      // Get poly1 normal in frame2.
      n := b2Mul(xf.q, poly1.m_normals[i]);
      v1 := b2Mul(xf, poly1.m_vertices[i]);

      // Find deepest point for normal i.
      si := FLT_MAX;
      for j := 0 to poly2.m_count - 1 do
      begin
         {$IFDEF OP_OVERLOAD}
         sij := b2Dot(n, poly2.m_vertices[j] - v1);
         {$ELSE}
         sij := b2Dot(n, Subtract(poly2.m_vertices[j],v1));
         {$ENDIF}
         if sij < si then
            si := sij;
      end;

      if si > maxSeparation then
      begin
         maxSeparation := si;
         bestIndex := i;
      end;
   end;

   edgeIndex := bestIndex;
   Result := maxSeparation;
end;

procedure b2FindIncidentEdge(var c: Tb2ClipVertices; poly1, poly2: Tb2PolygonShape;
   const xf1, xf2: Tb2Transform; edge1: Int32); overload;
var
   i: Integer;
   index, i1, i2: Int32;
   normal1: TVector2;
   minDot, dot: PhysicsFloat;
begin
   //b2Assert(0 <= edge1 && edge1 < poly1.m_vertexCount);

   // Get the normal of the reference edge in poly2's frame.
   normal1 := b2MulT(xf2.q, b2Mul(xf1.q, poly1.m_normals[edge1]));

   // Find the incident edge on poly2.
   index := 0;
   minDot := FLT_MAX;
   for i := 0 to poly2.m_count - 1 do
   begin
      dot := b2Dot(normal1, poly2.m_normals[i]);
      if dot < minDot then
      begin
         minDot := dot;
         index := i;
      end;
   end;

   // Build the clip vertices for the incident edge.
   i1 := index;
   if i1 + 1 < poly2.m_count then
      i2 := i1 + 1
   else
      i2 := 0;

   with c[0], id.cf do
   begin
      v := b2Mul(xf2, poly2.m_vertices[i1]);
      indexA := edge1;
      indexB := i1;
      typeA := e_contact_feature_face;
      typeB := e_contact_feature_vertex;
   end;

   with c[1], id.cf do
   begin
      v := b2Mul(xf2, poly2.m_vertices[i2]);
      indexA := edge1;
      indexB := i2;
      typeA := e_contact_feature_face;
      typeB := e_contact_feature_vertex;
   end;
end;

// Find edge normal of max separation on A - return if separating axis is found
// Find edge normal of max separation on B - return if separation axis is found
// Choose reference edge as min(minA, minB)
// Find incident edge
// Clip the normal points from 1 to 2
procedure b2CollidePolygons(contact: Pb2Contact; var manifold: Tb2Manifold;
   A, B: TObject; const xfA, xfB: Tb2Transform; ABfixture: Boolean);
var
   polyA, polyB: Tb2PolygonShape;
   i: Integer;
   edgeA, edgeB: Int32;
   edge1: Int32; // reference edge
   iv1, iv2: Int32;
   flip: UInt8;
   totalRadius, separationA, separationB: PhysicsFloat;
   poly1, poly2: Tb2PolygonShape; // reference poly and incident poly
   xf1, xf2: Tb2Transform;
   k_tol: PhysicsFloat;
   incidentEdge, clipPoints1, clipPoints2: Tb2ClipVertices;
   v11, v12, localTangent, localNormal, planePoint, tangent, normal: TVector2;
   frontOffset, sideOffset1, sideOffset2: PhysicsFloat;
   np: Int32; // Clip incident edge against extruded edge1 side edges.
   pointCount: Int32;
   cfSwap: Tb2ContactFeature;
begin
   if ABfixture then
   begin
      polyA := Tb2PolygonShape(Tb2Fixture(A).m_shape);
      polyB := Tb2PolygonShape(Tb2Fixture(B).m_shape);
   end
   else
   begin
      polyA := Tb2PolygonShape(A);
      polyB := Tb2PolygonShape(B);
   end;

   manifold.pointCount := 0;
   totalRadius := polyA.m_radius + polyB.m_radius;

   separationA := b2FindMaxSeparation(edgeA, polyA, polyB, xfA, xfB);
   if separationA > totalRadius then
      Exit;

   edgeB := 0;
   separationB := b2FindMaxSeparation(edgeB, polyB, polyA, xfB, xfA);
   if separationB > totalRadius then
      Exit;

   k_tol := 0.1 * b2_linearSlop;
	 if separationB > separationA + k_tol then
   begin
      poly1 := polyB;
      poly2 := polyA;
      xf1 := xfB;
      xf2 := xfA;
      edge1 := edgeB;
      manifold.manifoldType := e_manifold_faceB;
      flip := 1;
   end
   else
   begin
      poly1 := polyA;
      poly2 := polyB;
      xf1 := xfA;
      xf2 := xfB;
      edge1 := edgeA;
      manifold.manifoldType := e_manifold_faceA;
      flip := 0;
   end;

   b2FindIncidentEdge(incidentEdge, poly1, poly2, xf1, xf2, edge1);

   iv1 := edge1;
   if edge1 + 1 < poly1.m_count then
      iv2 := edge1 + 1
   else
      iv2 := 0;

   v11 := poly1.m_vertices[iv1];
   v12 := poly1.m_vertices[iv2];

   {$IFDEF OP_OVERLOAD}
   localTangent := v12 - v11;
   localTangent.Normalize;
   {$ELSE}
   localTangent := Subtract(v12, v11);
   Normalize(localTangent);
   {$ENDIF}

   localNormal := b2Cross(localTangent, 1.0);
   planePoint := b2MiddlePoint(v11, v12);

   tangent := b2Mul(xf1.q, localTangent);
   normal := b2Cross(tangent, 1.0);

   v11 := b2Mul(xf1, v11);
   v12 := b2Mul(xf1, v12);

   // Face offset.
   frontOffset := b2Dot(normal, v11);

   // Side offsets, extended by polytope skin thickness.
   sideOffset1 := -b2Dot(tangent, v11) + totalRadius;
   sideOffset2 := b2Dot(tangent, v12) + totalRadius;

   // Clip to box side 1
   {$IFDEF OP_OVERLOAD}
   np := b2ClipSegmentToLine(clipPoints1, incidentEdge, -tangent, sideOffset1, iv1);
   {$ELSE}
   np := b2ClipSegmentToLine(clipPoints1, incidentEdge, Negative(tangent), sideOffset1, iv1);
   {$ENDIF}
   if np < 2 then
      Exit;

   // Clip to negative box side 1
   np := b2ClipSegmentToLine(clipPoints2, clipPoints1, tangent, sideOffset2, iv2);
   if np < 2 then
      Exit;

   // Now clipPoints2 contains the clipped points.
   manifold.localNormal := localNormal;
   manifold.localPoint := planePoint;

   pointCount := 0;
   for i := 0 to b2_maxManifoldPoints - 1 do
   begin
      if b2Dot(normal, clipPoints2[i].v) - frontOffset <= totalRadius then
         with manifold.points[pointCount] do
         begin
            localPoint := b2MulT(xf2, clipPoints2[i].v);
            id := clipPoints2[i].id;
            if flip <> 0 then
               with id.cf do
               begin
                  // Swap features
                  cfSwap := id.cf;
                  indexA := cfSwap.indexB;
                  indexB := cfSwap.indexA;
                  typeA := cfSwap.typeB;
                  typeB := cfSwap.typeA;
               end;

            Inc(pointCount);
         end;
   end;

   manifold.pointCount := pointCount;
end;

{ Tb2CircleShape }

constructor Tb2CircleShape.Create;
begin
	 m_type := e_circleShape;
   m_radius := 0.0;
   m_p := b2Vec2_Zero;
end;

function Tb2CircleShape.Clone: Tb2Shape;
begin
   Result := Tb2CircleShape.Create;
   Result.m_type := m_type;
   Result.m_radius := m_radius;
   Tb2CircleShape(Result).m_p := m_p;
end;

function Tb2CircleShape.GetChildCount: Int32;
begin
   Result := 1;
end;

function Tb2CircleShape.TestPoint(const xf: Tb2Transform; const p: TVector2): Boolean;
var
   center, d: TVector2;
begin
   {$IFDEF OP_OVERLOAD}
   center := xf.p + b2Mul(xf.q, m_p);
   d := p - center;
   {$ELSE}
   center := Add(xf.p, b2Mul(xf.q, m_p));
   d := Subtract(p, center);
   {$ENDIF}
	 Result := b2Dot(d, d) <= m_radius * m_radius;
end;

// Collision Detection in Interactive 3D Environments by Gino van den Bergen
// From Section 3.1.2
// x = s + a * r
// norm(x) = radius
function Tb2CircleShape.RayCast(var output: Tb2RayCastOutput;
   const input: Tb2RayCastInput; const transform: Tb2Transform;
   childIndex: Int32): Boolean;
var
   s, r: TVector2;
   b, c, rr, sigma, a: PhysicsFloat;
begin
   //B2_NOT_USED(childIndex);
   {$IFDEF OP_OVERLOAD}
   s := input.p1 - (transform.p + b2Mul(transform.q, m_p));
   {$ELSE}
   s := Subtract(input.p1, Add(transform.p, b2Mul(transform.q, m_p)));
   {$ENDIF}
   b := b2Dot(s, s) - m_radius * m_radius;

   // Solve quadratic equation.
   {$IFDEF OP_OVERLOAD}
   r := input.p2 - input.p1;
   {$ELSE}
   r := Subtract(input.p2, input.p1);
   {$ENDIF}
   c :=  b2Dot(s, r);
   rr := b2Dot(r, r);
   sigma := c * c - rr * b;

   // Check for negative discriminant and short segment.
   if (sigma < 0.0) or (rr < FLT_EPSILON) then
   begin
      Result := False;
      Exit;
   end;

   // Find the point of intersection of the line with the circle.
   a := -(c + Sqrt(sigma));

   // Is the intersection point on the segment?
   if (0.0 <= a) and (a <= input.maxFraction * rr) then
   begin
      a := a / rr;
      with output do
      begin
         output.fraction := a;
         {$IFDEF OP_OVERLOAD}
         output.normal := s + a * r;
         output.normal.Normalize;
         {$ELSE}
         output.normal := Add(s, Multiply(r, a));
         Normalize(output.normal);
         {$ENDIF}
      end;
      Result := True;
      Exit;
   end;

   Result := False;
end;

procedure Tb2CircleShape.ComputeAABB(var aabb: Tb2AABB;
   const xf: Tb2Transform; childIndex: Int32);
var
   p: TVector2;
begin
   //B2_NOT_USED(childIndex);
   {$IFDEF OP_OVERLOAD}
   p := xf.p + b2Mul(xf.q, m_p);
   {$ELSE}
   p := Add(xf.p, b2Mul(xf.q, m_p));
   {$ENDIF}
   SetValue(aabb.lowerBound, p.x - m_radius, p.y - m_radius);
   SetValue(aabb.upperBound, p.x + m_radius, p.y + m_radius);
end;

procedure Tb2CircleShape.ComputeMass(var massData: Tb2MassData; density: PhysicsFloat);
begin
   m_baseMass.mass := Pi * m_radius * m_radius;
   m_baseMass.center := m_p;
   m_baseMass.I := m_baseMass.mass * (0.5 * m_radius * m_radius + b2Dot(m_p, m_p));

   massData.mass := density * m_baseMass.mass;
   massData.center := m_p;
   massData.I := density * m_baseMass.I;
end;

function Tb2CircleShape.ComputeSubmergedArea(const normal: TVector2; offset: PhysicsFloat;
   const xf: Tb2Transform; var c: TVector2): PhysicsFloat;
var
   p: TVector2;
   l, r2, l2, com: PhysicsFloat;
begin
   p := b2Mul(xf, m_p);
   l := -(b2Dot(normal, p) - offset);

   if l < -m_radius + FLT_EPSILON then
   begin
      Result := 0.0; // Untouch
      Exit;
   end;

   r2 := m_radius * m_radius;
   if l > m_radius then
   begin
      //Completely wet
      c := p;
      Result := Pi * r2;
      Exit;
   end;

   //Magic
   l2 := l * l;
   Result := r2 *(ArcSin(l / m_radius) + Pi / 2) + l * Sqrt(r2 - l2);
   com := -2 / 3 * Power(r2 - l2, 1.5) / Result;

   c.x := p.x + normal.x * com;
   c.y := p.y + normal.y * com;
end;

function Tb2CircleShape.GetSupport(const d: TVector2): Int32;
begin
   Result := 0;
end;

function Tb2CircleShape.GetSupportVertex(const d: TVector2): TVector2;
begin
   Result := m_p;
end;

function Tb2CircleShape.GetVertexCount: Int32;
begin
   Result := 1;
end;

function Tb2CircleShape.GetVertex(index: Int32): TVector2;
begin
   //b2Assert(index == 0);
   Result := m_p;
end;

{ Tb2PolygonShape }

constructor Tb2PolygonShape.Create;
begin
   m_type := e_polygonShape;
   m_radius := b2_polygonRadius;
   m_count := 0;
   m_centroid := b2Vec2_Zero;
end;

function Tb2PolygonShape.Clone: Tb2Shape;
begin
   Result := Tb2PolygonShape.Create;
   Result.m_type := m_type;
   Result.m_radius := m_radius;

   with Tb2PolygonShape(Result) do
   begin
      m_centroid := Self.m_centroid;
      m_vertices := Self.m_vertices;
      m_normals := Self.m_normals;
      m_count := Self.m_count;
   end;
end;

function Tb2PolygonShape.GetChildCount: Int32;
begin
   Result := 1;
end;

function Tb2PolygonShape.TestPoint(const xf: Tb2Transform; const p: TVector2): Boolean;
var
   i: Integer;
   pLocal: TVector2;
begin
   {$IFDEF OP_OVERLOAD}
   pLocal := b2MulT(xf.q, p - xf.p);
   {$ELSE}
   pLocal := b2MulT(xf.q, Subtract(p, xf.p));
   {$ENDIF}

   for i := 0 to m_count - 1 do
      {$IFDEF OP_OVERLOAD}
      if b2Dot(m_normals[i], pLocal - m_vertices[i]) > 0.0 then
      {$ELSE}
      if b2Dot(m_normals[i], Subtract(pLocal, m_vertices[i])) > 0.0 then
      {$ENDIF}
      begin
         Result := False;
         Exit;
      end;

   Result := True;
end;

function Tb2PolygonShape.RayCast(var output: Tb2RayCastOutput;
   const input: Tb2RayCastInput; const transform: Tb2Transform;
   childIndex: Int32): Boolean;
var
   i: Integer;
   index: Int32;
   p1, p2, d, r: TVector2;
   numerator, denominator, t, rr, s, lower, upper: PhysicsFloat;
begin
   //B2_NOT_USED(childIndex);
   // Put the ray into the polygon's frame of reference.
   {$IFDEF OP_OVERLOAD}
   p1 := b2MulT(transform.q, input.p1 - transform.p);
   p2 := b2MulT(transform.q, input.p2 - transform.p);
   d := p2 - p1;
   {$ELSE}
   p1 := b2MulT(transform.q, Subtract(input.p1, transform.p));
   p2 := b2MulT(transform.q, Subtract(input.p2, transform.p));
   d := Subtract(p2, p1);
   {$ENDIF}

   begin
      lower := 0.0;
      upper := input.maxFraction;

      index := -1;

      for i := 0 to m_count - 1 do
      begin
         // p := p1 + a * d
         // dot(normal, p - v) := 0
         // dot(normal, p1 - v) + a * dot(normal, d) := 0
         {$IFDEF OP_OVERLOAD}
         numerator := b2Dot(m_normals[i], m_vertices[i] - p1);
         {$ELSE}
         numerator := b2Dot(m_normals[i], Subtract(m_vertices[i], p1));
         {$ENDIF}
         denominator := b2Dot(m_normals[i], d);

         if denominator = 0.0 then
         begin
            if numerator < 0.0 then
            begin
               Result := False;
               Exit;
            end;
         end
         else
         begin
            // Note: we want this predicate without division:
            // lower < numerator / denominator, where denominator < 0
            // Since denominator < 0, we have to flip the inequality:
            // lower < numerator / denominator <==> denominator * lower > numerator.
            if (denominator < 0.0) and (numerator < lower * denominator) then
            begin
               // Increase lower.
               // The segment enters this half-space.
               lower := numerator / denominator;
               index := i;
            end
            else if (denominator > 0.0) and (numerator < upper * denominator) then
            begin
               // Decrease upper.
               // The segment exits this half-space.
               upper := numerator / denominator;
            end;
         end;

         // The use of epsilon here causes the assert on lower to trip
         // in some cases. Apparently the use of epsilon was to make edge
         // shapes work, but now those are handled separately.
         //if (upper < lower - b2_epsilon)
         if upper < lower then
         begin
            Result := False;
            Exit;
         end;
      end;

      //b2Assert(0.0f <= lower && lower <= input.maxFraction);

      if index >= 0 then
      begin
         output.fraction := lower;
         output.normal := b2Mul(transform.q, m_normals[index]);
         Result := True;
         Exit;
      end;
   end;

   Result := False;
end;

procedure Tb2PolygonShape.ComputeAABB(var aabb: Tb2AABB;
   const xf: Tb2Transform; childIndex: Int32);
var
   i: Integer;
   lower, upper, v, r: TVector2;
begin
   //B2_NOT_USED(childIndex);
   lower := b2Mul(xf, m_vertices[0]);
   upper := lower;

   for i := 1 to m_count - 1 do
   begin
      v := b2Mul(xf, m_vertices[i]);
      lower := b2Min(lower, v);
      upper := b2Max(upper, v);
   end;

   SetValue(r, m_radius, m_radius);
   {$IFDEF OP_OVERLOAD}
   aabb.lowerBound := lower - r;
   aabb.upperBound := upper + r;
   {$ELSE}
   aabb.lowerBound := Subtract(lower, r);
   aabb.upperBound := Add(upper, r);
   {$ENDIF}
end;

procedure Tb2PolygonShape.ComputeMass(var massData: Tb2MassData; density: PhysicsFloat);
const
   k_inv3 = 1.0 / 3.0;
var
   i: Integer;
   center, s, e1, e2: TVector2;
   area, inertia: PhysicsFloat;
   D, triangleArea: PhysicsFloat;
   ex1, ey1, ex2, ey2: PhysicsFloat;
   intx2, inty2: PhysicsFloat;
begin
   // Polygon mass, centroid, and inertia.
   // Let rho be the polygon density in mass per unit area.
   // Then:
   // mass = rho * int(dA)
   // centroid.x = (1/mass) * rho * int(x * dA)
   // centroid.y = (1/mass) * rho * int(y * dA)
   // I = rho * int((x*x + y*y) * dA)
   //
   // We can compute these integrals by summing all the integrals
   // for each triangle of the polygon. To evaluate the integral
   // for a single triangle, we make a change of variables to
   // the (u,v) coordinates of the triangle:
   // x = x0 + e1x * u + e2x * v
   // y = y0 + e1y * u + e2y * v
   // where 0 <= u && 0 <= v && u + v <= 1.
   //
   // We integrate u from [0,1-v] and then v from [0,1].
   // We also need to use the Jacobian of the transformation:
   // D = cross(e1, e2)
   //
   // Simplification: triangle centroid = (1/3) * (p1 + p2 + p3)
   //
   // The rest of the derivation is handled by computer algebra.

   //b2Assert(m_count >= 3);

   center := b2Vec2_Zero;
   area := 0.0;
   inertia := 0.0;

   // s is the reference point for forming triangles.
   // It's location doesn't change the result (except for rounding error).
   s := b2Vec2_Zero;
   // This code would put the reference point inside the polygon.
   {$IFDEF OP_OVERLOAD}
 	 for i := 0 to m_count - 1 do
      s.AddBy(m_vertices[i]);
   s.DivideBy(m_count);
   {$ELSE}
 	 for i := 0 to m_count - 1 do
      AddBy(s, m_vertices[i]);
   DivideBy(s, m_count);
   {$ENDIF}

   for i := 0 to m_count - 1 do
   begin
      // Triangle vertices.
      {$IFDEF OP_OVERLOAD}
      e1 := m_vertices[i] - s;
      if i + 1 < m_count then
         e2 := m_vertices[i + 1] - s
      else
         e2 := m_vertices[0] - s;
      {$ELSE}
      e1 := Subtract(m_vertices[i], s);
      if i + 1 < m_count then
         e2 := Subtract(m_vertices[i + 1], s)
      else
         e2 := Subtract(m_vertices[0], s);
      {$ENDIF}

      D := b2Cross(e1, e2);

      triangleArea := 0.5 * D;
      area := area + triangleArea;

      // Area weighted centroid
      {$IFDEF OP_OVERLOAD}
      center.AddBy(triangleArea * k_inv3 * (e1 + e2));
      {$ELSE}
      AddBy(center, Multiply(Add(e1, e2), triangleArea * k_inv3));
      {$ENDIF}

      ex1 := e1.x;
      ey1 := e1.y;
      ex2 := e2.x;
      ey2 := e2.y;

      intx2 := ex1 * ex1 + ex2 * ex1 + ex2 * ex2;
      inty2 := ey1 * ey1 + ey2 * ey1 + ey2 * ey2;

      inertia := inertia + (0.25 * k_inv3 * D) * (intx2 + inty2);
   end;

   m_baseMass.mass := area;
   m_baseMass.I := inertia + area * b2Dot(s, s);

   // Total mass
   massData.mass := density * area;

   // Center of mass
   //b2Assert(area > B2_FLT_EPSILON);
   {$IFDEF OP_OVERLOAD}
   center.DivideBy(area);
   massData.center := center + s;
   {$ELSE}
   DivideBy(center, area);
   massData.center := Add(center, s);
   {$ENDIF}
   m_baseMass.center := massData.center;

   // Inertia tensor relative to the local origin (point s).
   massData.I := m_baseMass.I * density;

   // Shift to center of mass then to original body origin.
   massData.I := massData.I + massData.mass * (b2Dot(massData.center, massData.center) - b2Dot(center, center));
end;

function Tb2PolygonShape.ComputeSubmergedArea(const normal: TVector2; offset: PhysicsFloat;
   const xf: Tb2Transform; var c: TVector2): PhysicsFloat;
var
   i: Integer;
   normalL, intoVec, outoVec, center, p2, p3: TVector2;
   offsetL, intoLamdda, outoLamdda, triangleArea: PhysicsFloat;
   diveCount, intoIndex, outoIndex, intoIndex2, outoIndex2: Int32;
   lastSubmerged, isSubmerged: Boolean;
   depths: array[0..b2_maxPolygonVertices - 1] of PhysicsFloat;
begin
   // Transform plane into shape co-ordinates
   normalL := b2MulT(xf.q, normal);
   offsetL := offset - b2Dot(normal, xf.p);

   diveCount := 0;
   intoIndex := -1;
   outoIndex := -1;
   lastSubmerged := False;

   for i := 0 to m_count - 1 do
   begin
      depths[i] := b2Dot(normalL, m_vertices[i]) - offsetL;
      isSubmerged := depths[i] < -FLT_EPSILON;
      if i > 0 then
      begin
         if isSubmerged then
         begin
            if not lastSubmerged then
            begin
               intoIndex := i - 1;
               Inc(diveCount);
            end;
         end
         else
         begin
            if lastSubmerged then
            begin
               outoIndex := i - 1;
               Inc(diveCount);
            end;
         end;
      end;
      lastSubmerged := isSubmerged;
   end;

   case diveCount of
      0:
         begin
            if lastSubmerged then
            begin
               // Completely submerged
               c := b2Mul(xf, m_baseMass.center);
               Result := m_baseMass.mass;
            end
            else
               Result := 0; //Completely dry
            Exit;
         end;
      1:
         if intoIndex = -1 then
            intoIndex := m_count - 1
         else
            outoIndex := m_count - 1;
   end;

   intoIndex2 := (intoIndex + 1) mod m_count;
   outoIndex2 := (outoIndex + 1) mod m_count;
   intoLamdda := -depths[intoIndex] / (depths[intoIndex2] - depths[intoIndex]);
   outoLamdda := -depths[outoIndex] / (depths[outoIndex2] - depths[outoIndex]);

   intoVec.x := m_vertices[intoIndex].x * (1 - intoLamdda) + m_vertices[intoIndex2].x * intoLamdda;
   intoVec.y := m_vertices[intoIndex].y * (1 - intoLamdda) + m_vertices[intoIndex2].y * intoLamdda;
   outoVec.x := m_vertices[outoIndex].x * (1 - outoLamdda) + m_vertices[outoIndex2].x * outoLamdda;
   outoVec.y := m_vertices[outoIndex].y * (1 - outoLamdda) + m_vertices[outoIndex2].y * outoLamdda;

   // Initialize accumulator
   Result := 0;
   center := b2Vec2_Zero;
   p2 := m_vertices[intoIndex2];

   // An awkward loop from intoIndex2+1 to outIndex2
   i := intoIndex2;
   while i <> outoIndex2 do
   begin
      i := (i + 1) mod m_count;
      if i = outoIndex2 then
         p3 := outoVec
      else
         p3 := m_vertices[i];

      triangleArea := 0.5 * ((p2.x - intoVec.x) * (p3.y - intoVec.y) -
         (p2.y - intoVec.y) * (p3.x - intoVec.x));
      Result := Result + triangleArea;
      // Area weighted centroid
      center.x := center.x + triangleArea * (intoVec.x + p2.x + p3.x) / 3;
      center.y := center.y + triangleArea * (intoVec.y + p2.y + p3.y) / 3;

      p2 := p3;
   end;

   //Normalize and transform centroid
   {$IFDEF OP_OVERLOAD}
   center.DivideBy(Result);
   {$ELSE}
   DivideBy(center, Result);
   {$ENDIF}
   c := b2Mul(xf, center);
end;

procedure Tb2PolygonShape.SetVertices(vertices: PVector2; count: Int32);
var
   i, j: Integer;
   {$IFNDEF SUPPORT_POINTER_MATH}pv: PVector2;{$ENDIF}
   tempCount, n, m, i0, ih, ie: Int32;
	 unique: Boolean;
   x0, x, c: PhysicsFloat;
   ps: Tb2PolyVertices;
   hull: array[0..b2_maxPolygonVertices - 1] of Int32;
   r, v, edge: TVector2;
   i1, i2: Int32;
begin
   //b2Assert(3 <= count && count <= b2_maxPolygonVertices);
   if count < 3 then
   begin
      SetAsBox(1.0, 1.0);
      Exit;
   end;

   n := b2Min(count, b2_maxPolygonVertices);

   // Perform welding and copy vertices into local buffer.
   tempCount := 0;
   for i := 0 to n - 1 do
   begin
      {$IFNDEF SUPPORT_POINTER_MATH}
      pv := vertices;
      Inc(pv, i);
      v := pv^;
      {$ELSE}
      v := vertices[i];
      {$ENDIF}
      unique := True;
      for j := 0 to tempCount - 1 do
      begin
         if b2DistanceSquared(v, ps[j]) <  0.5 * b2_linearSlop then
         begin
            unique := False;
            Break;
         end;
      end;

      if unique then
      begin
         ps[tempCount] := v;
         Inc(tempCount);
      end;
   end;

   n := tempCount;
	 if n < 3 then
   begin
      // Polygon is degenerate.
      SetAsBox(1.0, 1.0);
		  Exit;
   end;

   // Create the convex hull using the Gift wrapping algorithm
   // http://en.wikipedia.org/wiki/Gift_wrapping_algorithm

   // Find the right most point on the hull
   i0 := 0;
   x0 := ps[0].x;
   for i := 1 to n - 1 do
   begin
      x := ps[i].x;
      if (x > x0) or ((x = x0) and (ps[i].y < ps[i0].y)) then
      begin
         i0 := i;
         x0 := x;
      end;
   end;

   m := 0;
   ih := i0;

   while True do
   begin
      hull[m] := ih;

      ie := 0;
      for j := 1 to n - 1 do
      begin
         if ie = ih then
         begin
            ie := j;
            Continue;
         end;

         {$IFDEF OP_OVERLOAD}
         r := ps[ie] - ps[hull[m]];
         v := ps[j] - ps[hull[m]];
         {$ELSE}
         r := Subtract(ps[ie], ps[hull[m]]);
         v := Subtract(ps[j], ps[hull[m]]);
         {$ENDIF}
         c := b2Cross(r, v);
         if c < 0.0 then
            ie := j;

         // Collinearity check
         {$IFDEF OP_OVERLOAD}
         if (c = 0.0) and (v.SqrLength > r.SqrLength) then
         {$ELSE}
         if (c = 0.0) and (SqrLength(v) > SqrLength(r)) then
         {$ENDIF}
            ie := j;
      end;

      Inc(m);
      ih := ie;

      if ie = i0 then
         Break;
   end;

   m_count := m;

   // Copy vertices.
   for i := 0 to m - 1 do
      m_vertices[i] := ps[hull[i]];

   // Compute normals. Ensure the edges have non-zero length.
   for i := 0 to m - 1 do
   begin
      i1 := i;
      if i + 1 < m then
         i2 := i + 1
      else
         i2 := 0;
      {$IFDEF OP_OVERLOAD}
      edge := m_vertices[i2] - m_vertices[i1];
      {$ELSE}
      edge := Subtract(m_vertices[i2], m_vertices[i1]);
      {$ENDIF}
      //b2Assert(edge.LengthSquared() > b2_epsilon * b2_epsilon);
      m_normals[i] := b2Cross(edge, 1.0);
      {$IFDEF OP_OVERLOAD}
      m_normals[i].Normalize;
      {$ELSE}
      Normalize(m_normals[i]);
      {$ENDIF}
   end;

   // Compute the polygon centroid.
   m_centroid := ComputeCentroid(m_vertices, m);
end;

procedure Tb2PolygonShape.SetAsBox(hx, hy: PhysicsFloat);
begin
   m_count := 4;
   SetValue(m_vertices[0], -hx, -hy);
   SetValue(m_vertices[1], hx, -hy);
   SetValue(m_vertices[2], hx, hy);
   SetValue(m_vertices[3], -hx, hy);
   SetValue(m_normals[0], 0.0, -1.0);
   SetValue(m_normals[1], 1.0, 0.0);
   SetValue(m_normals[2], 0.0, 1.0);
   SetValue(m_normals[3], -1.0, 0.0);
   m_centroid := b2Vec2_Zero;
end;

procedure Tb2PolygonShape.SetAsBox(hx, hy: PhysicsFloat; const center: TVector2; angle: PhysicsFloat);
var
   i: Integer;
   xf: Tb2Transform;
begin
   SetAsBox(hx, hy);
   m_centroid := center;

   xf.p := center;
   {$IFDEF OP_OVERLOAD}
   xf.q.SetAngle(angle);
   {$ELSE}
   SetAngle(xf.q, angle);
   {$ENDIF}

   // Transform vertices and normals.
   for i := 0 to m_count - 1 do
   begin
      m_vertices[i] := b2Mul(xf, m_vertices[i]);
      m_normals[i] := b2Mul(xf.q, m_normals[i]);
   end;
end;

function Tb2PolygonShape.Validate: Boolean;
var
   i, j: Integer;
   i1, i2: Int32;
   p, e, v: TVector2;
   c: PhysicsFloat;
begin
   for i := 0 to m_count - 1 do
   begin
      i1 := i;
      if i < m_count - 1 then
         i2 := i1 + 1
      else
         i2 := 0;

      p := m_vertices[i1];
      {$IFDEF OP_OVERLOAD}
      e := m_vertices[i2] - p;
      {$ELSE}
      e := Subtract(m_vertices[i2], p);
      {$ENDIF}

      for j := 0 to m_count - 1 do
      begin
         if (j = i1) or (j = i2) then
            Continue;

         {$IFDEF OP_OVERLOAD}
         v := m_vertices[j] - p;
         {$ELSE}
         v := Subtract(m_vertices[j], p);
         {$ENDIF}
         c := b2Cross(e, v);
         if c < 0.0  then
         begin
            Result := False;
            Exit;
         end;
      end;
   end;

   Result := True;
end;

{ Tb2EdgeShape }

constructor Tb2EdgeShape.Create;
begin
   m_type := e_edgeShape;
   m_radius := b2_polygonRadius;
	 m_hasVertex0 := False;
   m_hasVertex3 := False;
   m_edgeShapeMassed := False;
end;

procedure Tb2EdgeShape.SetVertices(const v1, v2: TVector2);
begin
   m_vertex1 := v1;
   m_vertex2 := v2;
   m_hasVertex0 := False;
   m_hasVertex3 := False;

   {$IFDEF OP_OVERLOAD}
   m_normal1 := b2Cross(v2 - v1, 1.0);
   m_normal1.Normalize;
   m_normal2 := -m_normal1;
   {$ELSE}
   m_normal1 := b2Cross(Subtract(v2, v1), 1.0);
   Normalize(m_normal1);
   m_normal2 := Negative(m_normal1);
   {$ENDIF}
end;

function Tb2EdgeShape.Clone: Tb2Shape;
begin
   Result := Tb2EdgeShape.Create;
   Result.m_type := m_type;
   Result.m_radius := m_radius;

   with Tb2EdgeShape(Result) do
   begin
      m_vertex1 := Self.m_vertex1;
      m_vertex2 := Self.m_vertex2;
      m_vertex3 := Self.m_vertex3;
      m_vertex0 := Self.m_vertex0;
      m_normal1 := Self.m_normal1;
      m_normal2 := Self.m_normal2;
      m_hasVertex0 := Self.m_hasVertex0;
      m_hasVertex3 := Self.m_hasVertex3;
      m_edgeShapeMassed := Self.m_edgeShapeMassed;
   end;
end;

function Tb2EdgeShape.GetChildCount: Int32;
begin
   Result := 1;
end;

function Tb2EdgeShape.TestPoint(const xf: Tb2Transform; const p: TVector2): Boolean;
begin
   //B2_NOT_USED(xf);
   //B2_NOT_USED(p);
   Result := False;
end;

// p = p1 + t * d
// v = v1 + s * e
// p1 + t * d = v1 + s * e
// s * e - t * d = p1 - v1
function Tb2EdgeShape.RayCast(var output: Tb2RayCastOutput;
   const input: Tb2RayCastInput; const transform: Tb2Transform;
   childIndex: Int32): Boolean;
var
   p1, p2, d, e, normal, q: TVector2;
   numerator, denominator, t, rr, s: PhysicsFloat;
begin
   //B2_NOT_USED(childIndex);
   {$IFDEF OP_OVERLOAD}
   // Put the ray into the edge's frame of reference.
   p1 := b2MulT(transform.q, input.p1 - transform.p);
   p2 := b2MulT(transform.q, input.p2 - transform.p);
   d := p2 - p1;

   e := m_vertex2 - m_vertex1;
   normal.x := e.y;
   normal.y := -e.x;
   normal.Normalize;

   // q := p1 + t * d
   // dot(normal, q - m_vertex1) := 0
   // dot(normal, p1 - m_vertex1) + t * dot(normal, d) := 0
   numerator := b2Dot(normal, m_vertex1 - p1);
   {$ELSE}
   // Put the ray into the edge's frame of reference.
   p1 := b2MulT(transform.q, Subtract(input.p1, transform.p));
   p2 := b2MulT(transform.q, Subtract(input.p2, transform.p));
   d := Subtract(p2, p1);

   e := Subtract(m_vertex2, m_vertex1);
   normal.x := e.y;
   normal.y := -e.x;
   Normalize(normal);

   // q := p1 + t * d
   // dot(normal, q - m_vertex1) := 0
   // dot(normal, p1 - m_vertex1) + t * dot(normal, d) := 0
   numerator := b2Dot(normal, Subtract(m_vertex1, p1));
   {$ENDIF}

   denominator := b2Dot(normal, d);

   if denominator = 0.0 then
   begin
      Result := False;
      Exit;
   end;

   t := numerator / denominator;
   if (t < 0.0) or (input.maxFraction < t) then
   begin
      Result := False;
      Exit;
   end;

   {$IFDEF OP_OVERLOAD}
   q := p1 + t * d;
   {$ELSE}
   q := Add(p1, Multiply(d, t));
   {$ENDIF}

   // q := m_vertex1 + s * r
   // s := dot(q - m_vertex1, r) / dot(r, r)
   rr := b2Dot(e, e);
   if rr = 0.0 then
   begin
      Result := False;
      Exit;
   end;

   {$IFDEF OP_OVERLOAD}
   s := b2Dot(q - m_vertex1, e) / rr;
   {$ELSE}
   s := b2Dot(Subtract(q, m_vertex1), e) / rr;
   {$ENDIF}
   if (s < 0.0) or (1.0 < s) then
   begin
      Result := False;
      Exit;
   end;

   output.fraction := t;
   if numerator > 0.0 then
      {$IFDEF OP_OVERLOAD}
      output.normal := -b2Mul(transform.q, normal)
      {$ELSE}
      output.normal := Negative(b2Mul(transform.q, normal))
      {$ENDIF}
   else
      output.normal := b2Mul(transform.q, normal);

   Result := True;
end;

procedure Tb2EdgeShape.ComputeAABB(var aabb: Tb2AABB;
   const xf: Tb2Transform; childIndex: Int32);
var
   v1, v2, lower, upper, r: TVector2;
begin
   //B2_NOT_USED(childIndex);
   v1 := b2Mul(xf, m_vertex1);
   v2 := b2Mul(xf, m_vertex2);

   lower := b2Min(v1, v2);
   upper := b2Max(v1, v2);

   SetValue(r, m_radius, m_radius);
   {$IFDEF OP_OVERLOAD}
   aabb.lowerBound := lower - r;
   aabb.upperBound := upper + r;
   {$ELSE}
   aabb.lowerBound := Subtract(lower, r);
   aabb.upperBound := Add(upper, r);
   {$ENDIF}
end;

procedure Tb2EdgeShape.ComputeMass(var massData: Tb2MassData; density: PhysicsFloat);
var
   area: PhysicsFloat;
begin
   massData.center := b2MiddlePoint(m_vertex1, m_vertex2);

   if m_edgeShapeMassed then
   begin
      {$IFDEF OP_OVERLOAD}
      area := (m_vertex1 - m_vertex2).Length;
      {$ELSE}
      area := LengthVec(Subtract(m_vertex1, m_vertex2));
      {$ENDIF}
      massData.mass := density * area;
      massData.I := massData.mass * Sqr(area) / 12;
   end
   else
   begin
      massData.mass := 0.0;
      massData.I := 0.0;
   end;
end;

function Tb2EdgeShape.ComputeSubmergedArea(const normal: TVector2;
   offset: PhysicsFloat; const xf: Tb2Transform; var c: TVector2): PhysicsFloat;
begin
   Result := 0.0;
end;

{ Tb2ChainShape }

class function Tb2ChainShape.IsValidVertices(pv: PVector2; count: Int32; loop: Boolean): Boolean;
var
   i: Integer;
   {$IFDEF SUPPORT_POINTER_MATH}
   v1, v2: TVector2;
   {$ELSE}
   pv1, pv2: PVector2;
   {$ENDIF}
   e: TVector2;
   sqrSlop: PhysicsFloat;
begin
   if (not Assigned(pv)) or (loop and (count < 3)) or ((not loop) and (count < 2)) then
   begin
      Result := False;
      Exit;
   end;

   sqrSlop := b2_linearSlop * b2_linearSlop;
   for i := 1 to count - 1 do
   begin
      {$IFDEF SUPPORT_POINTER_MATH}
      v1 := pv[i - 1];
      v2 := pv[i];
      {$IFDEF OP_OVERLOAD}
      e := v2 - v1;
      {$ELSE}
      e := Subtract(v2, v1);
      {$ENDIF}
      {$ELSE}
      pv1 := pv; Inc(pv1, i - 1);
      pv2 := pv; Inc(pv2, i);
      {$IFDEF OP_OVERLOAD}
      e := pv2^ - pv1^;
      {$ELSE}
      e := Subtract(pv2^, pv1^);
      {$ENDIF}
      {$ENDIF}

      // If the code crashes here, it means your vertices are too close together.
      {$IFDEF OP_OVERLOAD}
      if e.SqrLength <= sqrSlop then
      {$ELSE}
      if SqrLength(e) <= sqrSlop then
      {$ENDIF}
      begin
         Result := False;
         Exit;
      end;
   end;

   Result := True;
end;

constructor Tb2ChainShape.CreateLoop(pv: PVector2; count: Int32);
begin
   if not IsValidVertices(pv, count, True) then
     	raise Tb2Exception.Create('Invalid loop shape.');

   m_loop := True;
   m_type := e_chainShape;
   m_radius := b2_polygonRadius;
   m_count := count + 1;
   SetLength(m_vertices, m_count);
   Move(pv^, m_vertices[0], count * SizeOf(TVector2));
   m_vertices[count] := m_vertices[0];
   m_prevVertex := m_vertices[m_count - 2];
   m_nextVertex := m_vertices[1];
   m_hasPrevVertex := True;
   m_hasNextVertex := True;
end;

constructor Tb2ChainShape.CreateChain(pv: PVector2; count: Int32);
begin
   if not IsValidVertices(pv, count, False) then
      raise Tb2Exception.Create('Invalid chain shape.');

   m_loop := False;
   m_type := e_chainShape;
   m_radius := b2_polygonRadius;

   //b2Assert(m_vertices == NULL && m_count == 0);
   //b2Assert(count >= 2);
   m_count := count;
   SetLength(m_vertices, m_count);
   Move(pv^, m_vertices[0], m_count * SizeOf(TVector2));
   m_hasPrevVertex := False;
   m_hasNextVertex := False;
   SetZero(m_prevVertex);
   SetZero(m_nextVertex);
end;

procedure Tb2ChainShape.SetPrevVertex(const prevVertex: TVector2);
begin
	 m_prevVertex := prevVertex;
	 m_hasPrevVertex := True;
end;

procedure Tb2ChainShape.SetNextVertex(const nextVertex: TVector2);
begin
	 m_nextVertex := nextVertex;
	 m_hasNextVertex := True;
end;

function Tb2ChainShape.Clone: Tb2Shape;
begin
   Result := Tb2ChainShape.CreateChain(@Self.m_vertices[0], Self.m_count);
   Tb2ChainShape(Result).m_loop := m_loop;
   Result.m_type := m_type;
   Result.m_radius := m_radius;
   Tb2ChainShape(Result).m_prevVertex := Self.m_prevVertex;
   Tb2ChainShape(Result).m_nextVertex := Self.m_nextVertex;
   Tb2ChainShape(Result).m_hasPrevVertex := Self.m_hasPrevVertex;
   Tb2ChainShape(Result).m_hasNextVertex := Self.m_hasNextVertex;
end;

function Tb2ChainShape.GetChildCount: Int32;
begin
	 // edge count = vertex count - 1
	 Result := m_count - 1;
end;

procedure Tb2ChainShape.GetChildEdge(edge: Tb2EdgeShape; index: Int32);
begin
   //b2Assert(0 <= index && index < m_count - 1);
   edge.m_type := e_edgeShape;
   edge.m_radius := m_radius;

   edge.m_vertex1 := m_vertices[index + 0];
   edge.m_vertex2 := m_vertices[index + 1];

   if index > 0 then
   begin
      edge.m_vertex0 := m_vertices[index - 1];
      edge.m_hasVertex0 := True;
   end
   else
   begin
      edge.m_vertex0 := m_prevVertex;
      edge.m_hasVertex0 := m_hasPrevVertex;
   end;

   if index < m_count - 2 then
   begin
      edge.m_vertex3 := m_vertices[index + 2];
      edge.m_hasVertex3 := True;
   end
   else
   begin
      edge.m_vertex3 := m_nextVertex;
      edge.m_hasVertex3 := m_hasNextVertex;
   end;
end;

function Tb2ChainShape.TestPoint(const xf: Tb2Transform;
   const p: TVector2): Boolean;
begin
	 //B2_NOT_USED(xf);
	 //B2_NOT_USED(p);
   Result := False;
end;

function Tb2ChainShape.RayCast(var output: Tb2RayCastOutput;
   const input: Tb2RayCastInput; const transform: Tb2Transform;
   childIndex: Int32): Boolean;
var
   i2: Int32;
begin
   //b2Assert(childIndex < m_count);
   i2 := childIndex + 1;
   if i2 = m_count then
      i2 := 0;

   static_edge_shape.m_vertex1 := m_vertices[childIndex];
   static_edge_shape.m_vertex2 := m_vertices[i2];

   Result := static_edge_shape.RayCast(output, input, transform, 0);
end;

procedure Tb2ChainShape.ComputeAABB(var aabb: Tb2AABB; const xf: Tb2Transform;
   childIndex: Int32);
var
   i2: Int32;
   v1, v2: TVector2;
begin
   //b2Assert(childIndex < m_count);

   i2 := childIndex + 1;
   if i2 = m_count then
      i2 := 0;

   v1 := b2Mul(xf, m_vertices[childIndex]);
   v2 := b2Mul(xf, m_vertices[i2]);

   aabb.lowerBound := b2Min(v1, v2);
   aabb.upperBound := b2Max(v1, v2);
end;

procedure Tb2ChainShape.ComputeMass(var massData: Tb2MassData; density: PhysicsFloat);
begin
   //B2_NOT_USED(density);
   massData.mass := 0.0;
   massData.center := b2Vec2_Zero;
   massData.I := 0.0;
end;

function Tb2ChainShape.ComputeSubmergedArea(const normal: TVector2;
   offset: PhysicsFloat; const xf: Tb2Transform; var c: TVector2): PhysicsFloat;
begin
   Result := 0.0;
end;

{ Tb2DistanceJointDef }

// 1-D constrained system
// m (v2 - v1) = lambda
// v2 + (beta/h) * x1 + gamma * lambda = 0, gamma has units of inverse mass.
// x2 = x1 + h * v2

// 1-D mass-damper-spring system
// m (v2 - v1) + h * d * v2 + h * k *

// C = norm(p2 - p1) - L
// u = (p2 - p1) / norm(p2 - p1)
// Cdot = dot(u, v2 + cross(w2, r2) - v1 - cross(w1, r1))
// J = [-u -cross(r1, u) u cross(r2, u)]
// K = J * invM * JT
//   = invMass1 + invI1 * cross(r1, u)^2 + invMass2 + invI2 * cross(r2, u)^2

constructor Tb2DistanceJointDef.Create;
begin
   inherited;
   jointType := e_distanceJoint;
   localAnchorA := b2Vec2_Zero;
   localAnchorB := b2Vec2_Zero;
   length := 1.0;
   frequencyHz := 0.0;
   dampingRatio := 0.0;
end;

procedure Tb2DistanceJointDef.Initialize(bodyA, bodyB: Tb2Body; const anchorA,
   anchorB: TVector2);
begin
   Self.bodyA := bodyA;
   Self.bodyB := bodyB;
   localAnchorA := bodyA.GetLocalPoint(anchorA);
   localAnchorB := bodyB.GetLocalPoint(anchorB);
   {$IFDEF OP_OVERLOAD}
   length := (anchorB - anchorA).Length;
   {$ELSE}
   length := UPhysics2DTypes.LengthVec(Subtract(anchorB, anchorA));
   {$ENDIF}
end;

{ Tb2DistanceJoint }

constructor Tb2DistanceJoint.Create(def: Tb2DistanceJointDef);
begin
   inherited Create(def);
   m_localAnchorA := def.localAnchorA;
   m_localAnchorB := def.localAnchorB;
   m_length := def.length;
   m_frequencyHz := def.frequencyHz;
   m_dampingRatio := def.dampingRatio;
   m_impulse := 0.0;
   m_gamma := 0.0;
   m_bias := 0.0;
end;

{$IFDEF ENABLE_DUMP}
procedure Tb2DistanceJoint.Dump;
var
   indexA, indexB: Int32;
begin
   indexA := m_bodyA.m_islandIndex;
   indexB := m_bodyB.m_islandIndex;

   b2DumpMethod(1, 'begin', []);
   b2DumpMethod(2, 'distance_jd := Tb2DistanceJointDef.Create;', []);
   b2DumpMethod(2, 'distance_jd.bodyA := bodies[%d];', [indexA]);
   b2DumpMethod(2, 'distance_jd.bodyB := bodies[%d];', [indexB]);
   b2DumpMethod(2, 'distance_jd.collideConnected := %s;', [b2BoolToStr(m_collideConnected)]);
   b2DumpMethod(2, 'distance_jd.localAnchorA := MakeVector(%s, %s);', [b2FloatToStr(m_localAnchorA.x), b2FloatToStr(m_localAnchorA.y)]);
   b2DumpMethod(2, 'distance_jd.localAnchorB := MakeVector(%s, %s);', [b2FloatToStr(m_localAnchorB.x), b2FloatToStr(m_localAnchorB.y)]);
   b2DumpMethod(2, 'distance_jd.length := %s;', [b2FloatToStr(m_length)]);
   b2DumpMethod(2, 'distance_jd.frequencyHz := %s;', [b2FloatToStr(m_frequencyHz)]);
   b2DumpMethod(2, 'distance_jd.dampingRatio := %s;', [b2FloatToStr(m_dampingRatio)]);
   b2DumpMethod(2, 'joints[%d] := m_world.CreateJoint(distance_jd);', [m_index]);
   b2DumpMethod(1, 'end;', []);
end;
{$ENDIF}

function Tb2DistanceJoint.GetAnchorA: TVector2;
begin
   Result := m_bodyA.GetWorldPoint(m_localAnchorA);
end;

function Tb2DistanceJoint.GetAnchorB: TVector2;
begin
   Result := m_bodyB.GetWorldPoint(m_localAnchorB);
end;

function Tb2DistanceJoint.GetReactionForce(inv_dt: PhysicsFloat): TVector2;
begin
   {$IFDEF OP_OVERLOAD}
   Result := (inv_dt * m_impulse) * m_u;
   {$ELSE}
   Result := Multiply(m_u, inv_dt * m_impulse);
   {$ENDIF}
end;

function Tb2DistanceJoint.GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat;
begin
   Result := 0.0;
end;

procedure Tb2DistanceJoint.InitVelocityConstraints(const data: Tb2SolverData);
var
   cA, vA, cB, vB: TVector2;
   aA, wA, aB, wB: PhysicsFloat;
   qA, qB: Tb2Rot;
   crAu, crBu: PhysicsFloat;
   h, C, omega, d, k: PhysicsFloat;
   P: TVector2;
   length, invMass: PhysicsFloat;
begin
   m_indexA := m_bodyA.m_islandIndex;
   m_indexB := m_bodyB.m_islandIndex;
   m_localCenterA := m_bodyA.m_sweep.localCenter;
   m_localCenterB := m_bodyB.m_sweep.localCenter;
   m_invMassA := m_bodyA.m_invMass;
   m_invMassB := m_bodyB.m_invMass;
   m_invIA := m_bodyA.m_invI;
   m_invIB := m_bodyB.m_invI;

   cA := data.positions[m_indexA].c;
   aA := data.positions[m_indexA].a;
   vA := data.velocities[m_indexA].v;
   wA := data.velocities[m_indexA].w;

   cB := data.positions[m_indexB].c;
   aB := data.positions[m_indexB].a;
   vB := data.velocities[m_indexB].v;
   wB := data.velocities[m_indexB].w;

   {$IFDEF OP_OVERLOAD}
   qA.SetAngle(aA);
   qB.SetAngle(aB);
   m_rA := b2Mul(qA, m_localAnchorA - m_localCenterA);
   m_rB := b2Mul(qB, m_localAnchorB - m_localCenterB);
   m_u := cB + m_rB - cA - m_rA;
   {$ELSE}
   SetAngle(qA, aA);
   SetAngle(qB, aB);
   m_rA := b2Mul(qA, Subtract(m_localAnchorA, m_localCenterA));
   m_rB := b2Mul(qB, Subtract(m_localAnchorB, m_localCenterB));
   m_u := Subtract(Add(cB, m_rB), Add(cA, m_rA));
   {$ENDIF}

   // Handle singularity.
   {$IFDEF OP_OVERLOAD}
   length := m_u.Length;
   if length > b2_linearSlop then
      m_u := m_u / length
   else
      m_u := b2Vec2_Zero;
   {$ELSE}
   length := UPhysics2DTypes.LengthVec(m_u);
   if length > b2_linearSlop then
      DivideBy(m_u, length)
   else
      m_u := b2Vec2_Zero;
   {$ENDIF}

   crAu := b2Cross(m_rA, m_u);
   crBu := b2Cross(m_rB, m_u);
   invMass := m_invMassA + m_invIA * crAu * crAu + m_invMassB + m_invIB * crBu * crBu;

	 // Compute the effective mass matrix.
   if invMass <> 0.0 then
      m_mass := 1.0 / invMass
   else
      m_mass := 0.0;

   if m_frequencyHz > 0.0 then
   begin
      C := length - m_length;
      omega := 2.0 * Pi * m_frequencyHz; // Frequency
      d := 2.0 * m_mass * m_dampingRatio * omega; // Damping coefficient
      k := m_mass * omega * omega; // Spring stiffness

      // magic formulas
      h := data.step.dt;
      m_gamma := h * (d + h * k);
      if m_gamma <> 0.0 then
         m_gamma := 1.0 / m_gamma
      else
         m_gamma := 0.0;
      m_bias := C * h * k * m_gamma;
      invMass := invMass + m_gamma;
      if invMass <> 0.0 then
         m_mass := 1.0 / invMass
      else
         m_mass := 0.0;
   end
   else
   begin
   		m_gamma := 0.0;
      m_bias := 0.0;
   end;

   if data.step.warmStarting then
   begin
      // Scale the impulse to support a variable time step.
      m_impulse := m_impulse * data.step.dtRatio;
      {$IFDEF OP_OVERLOAD}
      P := m_impulse * m_u;
      vA.SubtractBy(m_invMassA * P);
      vB.AddBy(m_invMassB * P);
      {$ELSE}
      P := Multiply(m_u, m_impulse);
      SubtractBy(vA, Multiply(P, m_invMassA));
      AddBy(vB, Multiply(P, m_invMassB));
      {$ENDIF}
      wA := wA - m_invIA * b2Cross(m_rA, P);
      wB := wB + m_invIB * b2Cross(m_rB, P);
   end
   else
      m_impulse := 0.0;

   data.velocities[m_indexA].v := vA;
   data.velocities[m_indexA].w := wA;
   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
end;

procedure Tb2DistanceJoint.SolveVelocityConstraints(const data: Tb2SolverData);
var
   vA, vB: TVector2;
   wA, wB: PhysicsFloat;
   vpA, vpB, P: TVector2;
   Cdot, impulse: PhysicsFloat;
begin
   vA := data.velocities[m_indexA].v;
	 wA := data.velocities[m_indexA].w;
	 vB := data.velocities[m_indexB].v;
	 wB := data.velocities[m_indexB].w;

   // Cdot = dot(u, v + cross(w, r))
   {$IFDEF OP_OVERLOAD}
   vpA := vA + b2Cross(wA, m_rA);
   vpB := vB + b2Cross(wB, m_rB);
   Cdot := b2Dot(m_u, vpB - vpA);
   {$ELSE}
   vpA := Add(vA, b2Cross(wA, m_rA));
   vpB := Add(vB, b2Cross(wB, m_rB));
   Cdot := b2Dot(m_u, Subtract(vpB, vpA));
   {$ENDIF}

   impulse := -m_mass * (Cdot + m_bias + m_gamma * m_impulse);
   m_impulse := m_impulse + impulse;

   {$IFDEF OP_OVERLOAD}
   P := impulse * m_u;
   vA.SubtractBy(m_invMassA * P);
   vB.AddBy(m_invMassB * P);
   {$ELSE}
   P := Multiply(m_u, impulse);
   SubtractBy(vA, Multiply(P, m_invMassA));
   AddBy(vB, Multiply(P, m_invMassB));
   {$ENDIF}
   wA := wA - m_invIA * b2Cross(m_rA, P);
   wB := wB + m_invIB * b2Cross(m_rB, P);

   data.velocities[m_indexA].v := vA;
   data.velocities[m_indexA].w := wA;
   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
end;

function Tb2DistanceJoint.SolvePositionConstraints(const data: Tb2SolverData): Boolean;
var
   cA, cB: TVector2;
   aA, aB: PhysicsFloat;
   qA, qB: Tb2Rot;
   rA, rB, u, P: TVector2;
   C, impulse: PhysicsFloat;
begin
   if m_frequencyHz > 0.0 then
   begin
      // There is no position correction for soft distance constraints.
      Result := True;
      Exit;
   end;

   cA := data.positions[m_indexA].c;
   aA := data.positions[m_indexA].a;
   cB := data.positions[m_indexB].c;
   aB := data.positions[m_indexB].a;

   {$IFDEF OP_OVERLOAD}
   qA.SetAngle(aA);
   qB.SetAngle(aB);
   {$ELSE}
   SetAngle(qA, aA);
   SetAngle(qB, aB);
   {$ENDIF}

   {$IFDEF OP_OVERLOAD}
   rA := b2Mul(qA, m_localAnchorA - m_localCenterA);
   rB := b2Mul(qB, m_localAnchorB - m_localCenterB);
   u := cB + rB - cA - rA;
   C := u.Normalize - m_length;
   {$ELSE}
   rA := b2Mul(qA, Subtract(m_localAnchorA, m_localCenterA));
   rB := b2Mul(qB, Subtract(m_localAnchorB, m_localCenterB));
   u := Subtract(Add(cB, rB), Add(cA, rA));
   C := Normalize(u) - m_length;
   {$ENDIF}
   C := b2Clamp(C, -b2_maxLinearCorrection, b2_maxLinearCorrection);

   impulse := -m_mass * C;
   {$IFDEF OP_OVERLOAD}
   P := impulse * m_u;
   cA.SubtractBy(m_invMassA * P);
   cB.AddBy(m_invMassB * P);
   {$ELSE}
   P := Multiply(m_u, impulse);
   SubtractBy(cA, Multiply(P, m_invMassA));
   AddBy(cB, Multiply(P, m_invMassB));
   {$ENDIF}
   aA := aA - m_invIA * b2Cross(rA, P);
   aB := aB + m_invIB * b2Cross(rB, P);

   data.positions[m_indexA].c := cA;
   data.positions[m_indexA].a := aA;
   data.positions[m_indexB].c := cB;
   data.positions[m_indexB].a := aB;

   Result := Abs(C) < b2_linearSlop;
end;

{ Tb2PrismaticJointDef }

// Linear constraint (point-to-line)
// d = p2 - p1 = x2 + r2 - x1 - r1
// C = dot(perp, d)
// Cdot = dot(d, cross(w1, perp)) + dot(perp, v2 + cross(w2, r2) - v1 - cross(w1, r1))
//      = -dot(perp, v1) - dot(cross(d + r1, perp), w1) + dot(perp, v2) + dot(cross(r2, perp), v2)
// J = [-perp, -cross(d + r1, perp), perp, cross(r2,perp)]
//
// Angular constraint
// C = a2 - a1 + a_initial
// Cdot = w2 - w1
// J = [0 0 -1 0 0 1]
//
// K = J * invM * JT
//
// J = [-a -s1 a s2]
//     [0  -1  0  1]
// a = perp
// s1 = cross(d + r1, a) = cross(p2 - x1, a)
// s2 = cross(r2, a) = cross(p2 - x2, a)


// Motor/Limit linear constraint
// C = dot(ax1, d)
// Cdot = = -dot(ax1, v1) - dot(cross(d + r1, ax1), w1) + dot(ax1, v2) + dot(cross(r2, ax1), v2)
// J = [-ax1 -cross(d+r1,ax1) ax1 cross(r2,ax1)]

// Block Solver
// We develop a block solver that includes the joint limit. This makes the limit stiff (inelastic) even
// when the mass has poor distribution (leading to large torques about the joint anchor points).
//
// The Jacobian has 3 rows:
// J = [-uT -s1 uT s2] // linear
//     [0   -1   0  1] // angular
//     [-vT -a1 vT a2] // limit
//
// u = perp
// v = axis
// s1 = cross(d + r1, u), s2 = cross(r2, u)
// a1 = cross(d + r1, v), a2 = cross(r2, v)

// M * (v2 - v1) = JT * df
// J * v2 = bias
//
// v2 = v1 + invM * JT * df
// J * (v1 + invM * JT * df) = bias
// K * df = bias - J * v1 = -Cdot
// K = J * invM * JT
// Cdot = J * v1 - bias
//
// Now solve for f2.
// df = f2 - f1
// K * (f2 - f1) = -Cdot
// f2 = invK * (-Cdot) + f1
//
// Clamp accumulated limit impulse.
// lower: f2(3) = max(f2(3), 0)
// upper: f2(3) = min(f2(3), 0)
//
// Solve for correct f2(1:2)
// K(1:2, 1:2) * f2(1:2) = -Cdot(1:2) - K(1:2,3) * f2(3) + K(1:2,1:3) * f1
//                       = -Cdot(1:2) - K(1:2,3) * f2(3) + K(1:2,1:2) * f1(1:2) + K(1:2,3) * f1(3)
// K(1:2, 1:2) * f2(1:2) = -Cdot(1:2) - K(1:2,3) * (f2(3) - f1(3)) + K(1:2,1:2) * f1(1:2)
// f2(1:2) = invK(1:2,1:2) * (-Cdot(1:2) - K(1:2,3) * (f2(3) - f1(3))) + f1(1:2)
//
// Now compute impulse to be applied:
// df = f2 - f1

constructor Tb2PrismaticJointDef.Create;
begin
   inherited;
   jointType := e_prismaticJoint;
   localAnchorA := b2Vec2_Zero;
   localAnchorB := b2Vec2_Zero;
   SetValue(localAxisA, 1.0, 0.0);
   referenceAngle := 0.0;
   enableLimit := False;
   lowerTranslation := 0.0;
   upperTranslation := 0.0;
   enableMotor := False;
   maxMotorForce := 0.0;
   motorSpeed := 0.0;
end;

procedure Tb2PrismaticJointDef.Initialize(bodyA, bodyB: Tb2Body; const anchor,
   axis: TVector2);
begin
   Self.bodyA := bodyA;
   Self.bodyB := bodyB;
   localAnchorA := bodyA.GetLocalPoint(anchor);
   localAnchorB := bodyB.GetLocalPoint(anchor);
   localAxisA := bodyA.GetLocalVector(axis);
   referenceAngle := bodyB.GetAngle - bodyA.GetAngle;
end;

{ Tb2PrismaticJoint }

constructor Tb2PrismaticJoint.Create(def: Tb2PrismaticJointDef);
begin
   inherited Create(def);
   m_localAnchorA := def.localAnchorA;
   m_localAnchorB := def.localAnchorB;
   m_localXAxisA := def.localAxisA;
   {$IFDEF OP_OVERLOAD}
   m_localXAxisA.Normalize;
   {$ELSE}
   Normalize(m_localXAxisA);
   {$ENDIF}
   m_localYAxisA := b2Cross(1.0, m_localXAxisA);
   m_referenceAngle := def.referenceAngle;

   m_impulse := b2Vec3_Zero;
   m_motorMass := 0.0;
   m_motorImpulse := 0.0;

   m_lowerTranslation := def.lowerTranslation;
   m_upperTranslation := def.upperTranslation;
   m_maxMotorForce := def.maxMotorForce;
   m_motorSpeed := def.motorSpeed;
   m_enableLimit := def.enableLimit;
   m_enableMotor := def.enableMotor;
   m_limitState := e_inactiveLimit;

   m_axis := b2Vec2_Zero;
   m_perp := b2Vec2_Zero;
end;

{$IFDEF ENABLE_DUMP}
procedure Tb2PrismaticJoint.Dump;
var
   indexA, indexB: Int32;
begin
   indexA := m_bodyA.m_islandIndex;
   indexB := m_bodyB.m_islandIndex;

   b2DumpMethod(1, 'begin', []);
   b2DumpMethod(2, 'prismatic_jd := Tb2PrismaticJointDef.Create;', []);
   b2DumpMethod(2, 'prismatic_jd.bodyA := bodies[%d];', [indexA]);
   b2DumpMethod(2, 'prismatic_jd.bodyB := bodies[%d];', [indexB]);
   b2DumpMethod(2, 'prismatic_jd.collideConnected := %s;', [b2BoolToStr(m_collideConnected)]);
   b2DumpMethod(2, 'prismatic_jd.localAnchorA := MakeVector(%s, %s);', [b2FloatToStr(m_localAnchorA.x), b2FloatToStr(m_localAnchorA.y)]);
   b2DumpMethod(2, 'prismatic_jd.localAnchorB := MakeVector(%s, %s);', [b2FloatToStr(m_localAnchorB.x), b2FloatToStr(m_localAnchorB.y)]);
   b2DumpMethod(2, 'prismatic_jd.localAxisA := MakeVector(%s, %s);', [b2FloatToStr(m_localXAxisA.x), b2FloatToStr(m_localXAxisA.y)]);
   b2DumpMethod(2, 'prismatic_jd.referenceAngle := %s;', [b2FloatToStr(m_referenceAngle)]);
   b2DumpMethod(2, 'prismatic_jd.enableLimit := %s;', [b2BoolToStr(m_enableLimit)]);
   b2DumpMethod(2, 'prismatic_jd.lowerTranslation := %s;', [b2FloatToStr(m_lowerTranslation)]);
   b2DumpMethod(2, 'prismatic_jd.upperTranslation := %s;', [b2FloatToStr(m_upperTranslation)]);
   b2DumpMethod(2, 'prismatic_jd.enableMotor := %s;', [b2BoolToStr(m_enableMotor)]);
   b2DumpMethod(2, 'prismatic_jd.motorSpeed := %s;', [b2FloatToStr(m_motorSpeed)]);
   b2DumpMethod(2, 'prismatic_jd.maxMotorForce := %s;', [b2FloatToStr(m_maxMotorForce)]);
   b2DumpMethod(2, 'joints[%d] := m_world.CreateJoint(prismatic_jd);', [m_index]);
   b2DumpMethod(1, 'end;', []);
end;
{$ENDIF}

function Tb2PrismaticJoint.GetAnchorA: TVector2;
begin
   Result := m_bodyA.GetWorldPoint(m_localAnchorA);
end;

function Tb2PrismaticJoint.GetAnchorB: TVector2;
begin
   Result := m_bodyB.GetWorldPoint(m_localAnchorB);
end;

function Tb2PrismaticJoint.GetReactionForce(inv_dt: PhysicsFloat): TVector2;
begin
   {$IFDEF OP_OVERLOAD}
   Result := inv_dt * (m_impulse.x * m_perp + (m_motorImpulse + m_impulse.z) * m_axis);
   {$ELSE}
   Result := Multiply(Add(Multiply(m_perp, m_impulse.x), Multiply(m_axis,
      m_motorImpulse + m_impulse.z)), inv_dt);
   {$ENDIF}
end;

function Tb2PrismaticJoint.GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat;
begin
   Result := inv_dt * m_impulse.y;
end;

procedure Tb2PrismaticJoint.InitVelocityConstraints(const data: Tb2SolverData);
var
   cA, cB: TVector2;
   vA, vB: TVector2;
   aA, aB: PhysicsFloat;
   wA, wB: PhysicsFloat;
   qA, qB: Tb2Rot;
   rA, rB, d: TVector2;
   mA, iA, mB, iB: PhysicsFloat;
   k11, k12, k13, k22, k23, k33: PhysicsFloat;
   jointTranslation: PhysicsFloat;
   P: TVector2;
   LA, LB: PhysicsFloat;
begin
   m_indexA := m_bodyA.m_islandIndex;
   m_indexB := m_bodyB.m_islandIndex;
   m_localCenterA := m_bodyA.m_sweep.localCenter;
   m_localCenterB := m_bodyB.m_sweep.localCenter;
   m_invMassA := m_bodyA.m_invMass;
   m_invMassB := m_bodyB.m_invMass;
   m_invIA := m_bodyA.m_invI;
   m_invIB := m_bodyB.m_invI;

   cA := data.positions[m_indexA].c;
   aA := data.positions[m_indexA].a;
   vA := data.velocities[m_indexA].v;
   wA := data.velocities[m_indexA].w;

   cB := data.positions[m_indexB].c;
   aB := data.positions[m_indexB].a;
   vB := data.velocities[m_indexB].v;
   wB := data.velocities[m_indexB].w;

   {$IFDEF OP_OVERLOAD}
   qA.SetAngle(aA);
   qB.SetAngle(aB);

   // Compute the effective masses.
   rA := b2Mul(qA, m_localAnchorA - m_localCenterA);
   rB := b2Mul(qB, m_localAnchorB - m_localCenterB);
   d := (cB - cA) + rB - rA;
   {$ELSE}
   SetAngle(qA, aA);
   SetAngle(qB, aB);

   // Compute the effective masses.
   rA := b2Mul(qA, Subtract(m_localAnchorA, m_localCenterA));
   rB := b2Mul(qB, Subtract(m_localAnchorB, m_localCenterB));
   d := Add(Subtract(cB, cA), Subtract(rB, rA));
   {$ENDIF}

   mA := m_invMassA;
   mB := m_invMassB;
   iA := m_invIA;
   iB := m_invIB;

   // Compute motor Jacobian and effective mass.
   m_axis := b2Mul(qA, m_localXAxisA);
   {$IFDEF OP_OVERLOAD}
   m_a1 := b2Cross(d + rA, m_axis);
   {$ELSE}
   m_a1 := b2Cross(Add(d, rA), m_axis);
   {$ENDIF}
   m_a2 := b2Cross(rB, m_axis);

   m_motorMass := mA + mB + iA * m_a1 * m_a1 + iB * m_a2 * m_a2;
   if m_motorMass > 0.0 then
      m_motorMass := 1.0 / m_motorMass;

   // Prismatic constraint.
   m_perp := b2Mul(qA, m_localYAxisA);

   {$IFDEF OP_OVERLOAD}
   m_s1 := b2Cross(d + rA, m_perp);
   {$ELSE}
   m_s1 := b2Cross(Add(d, rA), m_perp);
   {$ENDIF}
   m_s2 := b2Cross(rB, m_perp);

   k11 := mA + mB + iA * m_s1 * m_s1 + iB * m_s2 * m_s2;
   k12 := iA * m_s1 + iB * m_s2;
   k13 := iA * m_s1 * m_a1 + iB * m_s2 * m_a2;
   k22 := iA + iB;
   if k22 = 0.0 then // For bodies with fixed rotation.
      k22 := 1.0;

   k23 := iA * m_a1 + iB * m_a2;
   k33 := mA + mB + iA * m_a1 * m_a1 + iB * m_a2 * m_a2;

   m_K.ex := MakeVector(k11, k12, k13);
   m_K.ey := MakeVector(k12, k22, k23);
   m_K.ez := MakeVector(k13, k23, k33);

   // Compute motor and limit terms.
   if m_enableLimit then
   begin
      jointTranslation := b2Dot(m_axis, d);
      if Abs(m_upperTranslation - m_lowerTranslation) < 2.0 * b2_linearSlop then
         m_limitState := e_equalLimits
      else if jointTranslation <= m_lowerTranslation then
      begin
         if m_limitState <> e_atLowerLimit then
         begin
            m_limitState := e_atLowerLimit;
            m_impulse.z := 0.0;
         end;
      end
      else if jointTranslation >= m_upperTranslation then
      begin
         if m_limitState <> e_atUpperLimit then
         begin
            m_limitState := e_atUpperLimit;
            m_impulse.z := 0.0;
         end;
      end
      else
      begin
         m_limitState := e_inactiveLimit;
         m_impulse.z := 0.0;
      end;
   end
   else
   begin
      m_limitState := e_inactiveLimit;
      m_impulse.z := 0.0;
   end;

   if not m_enableMotor then
      m_motorImpulse := 0.0;

   if data.step.warmStarting then
   begin
      // Account for variable time step.
      {$IFDEF OP_OVERLOAD}
      m_impulse.MultiplyBy(data.step.dtRatio);
      {$ELSE}
      MultiplyBy(m_impulse, data.step.dtRatio);
      {$ENDIF}
      m_motorImpulse := m_motorImpulse * data.step.dtRatio;

      {$IFDEF OP_OVERLOAD}
      P := m_impulse.x * m_perp + (m_motorImpulse + m_impulse.z) * m_axis;
      {$ELSE}
      P := Add(Multiply(m_perp, m_impulse.x), Multiply(m_axis, m_motorImpulse + m_impulse.z));
      {$ENDIF}
      LA := m_impulse.x * m_s1 + m_impulse.y + (m_motorImpulse + m_impulse.z) * m_a1;
      LB := m_impulse.x * m_s2 + m_impulse.y + (m_motorImpulse + m_impulse.z) * m_a2;

      {$IFDEF OP_OVERLOAD}
      vA.SubtractBy(mA * P);
      vB.AddBy(mB * P);
      {$ELSE}
      SubtractBy(vA, Multiply(P, mA));
      AddBy(vB, Multiply(P, mB));
      {$ENDIF}
      wA := wA - iA * LA;
      wB := wB + iB * LB;
   end
   else
   begin
      m_impulse := b2Vec3_Zero;
      m_motorImpulse := 0.0;
   end;

   data.velocities[m_indexA].v := vA;
   data.velocities[m_indexA].w := wA;
   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
end;

procedure Tb2PrismaticJoint.SolveVelocityConstraints(const data: Tb2SolverData);
var
   vA, vB: TVector2;
   wA, wB: PhysicsFloat;
   mA, iA, mB, iB: PhysicsFloat;
   Cdot, impulse, oldImpulse, maxImpulse: PhysicsFloat;
   P, Cdot1: TVector2;
   LA, LB: PhysicsFloat;
   Cdot2: PhysicsFloat;
   f1, df, Cdot3: TVector3;
   b, f2r: TVector2;
   df2: TVector2;
begin
   vA := data.velocities[m_indexA].v;
   wA := data.velocities[m_indexA].w;
   vB := data.velocities[m_indexB].v;
   wB := data.velocities[m_indexB].w;

   mA := m_invMassA;
   mB := m_invMassB;
   iA := m_invIA;
   iB := m_invIB;

   // Solve linear motor constraint.
   if m_enableMotor and (m_limitState <> e_equalLimits) then
   begin
      {$IFDEF OP_OVERLOAD}
      Cdot := b2Dot(m_axis, vB - vA) + m_a2 * wB - m_a1 * wA;
      {$ELSE}
      Cdot := b2Dot(m_axis, Subtract(vB, vA)) + m_a2 * wB - m_a1 * wA;
      {$ENDIF}
      impulse := m_motorMass * (m_motorSpeed - Cdot);
      oldImpulse := m_motorImpulse;
      maxImpulse := data.step.dt * m_maxMotorForce;
      m_motorImpulse := b2Clamp(m_motorImpulse + impulse, -maxImpulse, maxImpulse);
      impulse := m_motorImpulse - oldImpulse;

      {$IFDEF OP_OVERLOAD}
      P := impulse * m_axis;
      {$ELSE}
      P := Multiply(m_axis, impulse);
      {$ENDIF}
      LA := impulse * m_a1;
      LB := impulse * m_a2;

      {$IFDEF OP_OVERLOAD}
      vA.SubtractBy(mA * P);
      vB.AddBy(mB * P);
      {$ELSE}
      SubtractBy(vA, Multiply(P, mA));
      AddBy(vB, Multiply(P, mB));
      {$ENDIF}

      wA := wA - iA * LA;
      wB := wB + iB * LB;
   end;

   {$IFDEF OP_OVERLOAD}
   Cdot1.x := b2Dot(m_perp, vB - vA) + m_s2 * wB - m_s1 * wA;
   {$ELSE}
   Cdot1.x := b2Dot(m_perp, Subtract(vB, vA)) + m_s2 * wB - m_s1 * wA;
   {$ENDIF}
   Cdot1.y := wB - wA;

   if m_enableLimit and (m_limitState <> e_inactiveLimit) then
   begin
      // Solve prismatic and limit constraint in block form.
      {$IFDEF OP_OVERLOAD}
      Cdot2 := b2Dot(m_axis, vB - vA) + m_a2 * wB - m_a1 * wA;
      {$ELSE}
      Cdot2 := b2Dot(m_axis, Subtract(vB, vA)) + m_a2 * wB - m_a1 * wA;
      {$ENDIF}

      Cdot3.x := Cdot1.x;
      Cdot3.y := Cdot1.y;
      Cdot3.z := Cdot2;
      f1 := m_impulse;
      {$IFDEF OP_OVERLOAD}
      df := m_K.Solve33(-Cdot3);
      m_impulse.AddBy(df);
      {$ELSE}
      df := Solve33(m_K, Negative(Cdot3));
      AddBy(m_impulse, df);
      {$ENDIF}

      if m_limitState = e_atLowerLimit then
         m_impulse.z := b2Max(m_impulse.z, 0.0)
      else if m_limitState = e_atUpperLimit then
         m_impulse.z := b2Min(m_impulse.z, 0.0);

      // f2(1:2) := invK(1:2,1:2) * (-Cdot(1:2) - K(1:2,3) * (f2(3) - f1(3))) + f1(1:2)
      {$IFDEF OP_OVERLOAD}
      b := -Cdot1 - (m_impulse.z - f1.z) * MakeVector(m_K.ez.x, m_K.ez.y);
      f2r := m_K.Solve22(b) + MakeVector(f1.x, f1.y);
      {$ELSE}
      b := Subtract(Negative(Cdot1), Multiply(MakeVector(m_K.ez.x, m_K.ez.y), m_impulse.z - f1.z));
      f2r := Add(Solve22(m_K, b), MakeVector(f1.x, f1.y));
      {$ENDIF}
      m_impulse.x := f2r.x;
      m_impulse.y := f2r.y;

      {$IFDEF OP_OVERLOAD}
      df := m_impulse - f1;
      P := df.x * m_perp + df.z * m_axis;
      {$ELSE}
      df := Subtract(m_impulse, f1);
      P := Add(Multiply(m_perp, df.x), Multiply(m_axis, df.z));
      {$ENDIF}
      LA := df.x * m_s1 + df.y + df.z * m_a1;
      LB := df.x * m_s2 + df.y + df.z * m_a2;
   end
   else
   begin
      // Limit is inactive, just solve the prismatic constraint in block form.
      {$IFDEF OP_OVERLOAD}
      df2 := m_K.Solve22(-Cdot1);
      {$ELSE}
      df2 := Solve22(m_K, Negative(Cdot1));
      {$ENDIF}
      m_impulse.x := m_impulse.x + df2.x;
      m_impulse.y := m_impulse.y + df2.y;

      {$IFDEF OP_OVERLOAD}
      P := df2.x * m_perp;
      {$ELSE}
      P := Multiply(m_perp, df2.x);
      {$ENDIF}
      LA := df2.x * m_s1 + df2.y;
      LB := df2.x * m_s2 + df2.y;
   end;

   {$IFDEF OP_OVERLOAD}
   vA.SubtractBy(mA * P);
   vB.AddBy(mB * P);
   {$ELSE}
   SubtractBy(vA, Multiply(P, mA));
   AddBy(vB, Multiply(P, mB));
   {$ENDIF}
   wA := wA - iA * LA;
   wB := wB + iB * LB;

   data.velocities[m_indexA].v := vA;
   data.velocities[m_indexA].w := wA;
   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
end;

function Tb2PrismaticJoint.SolvePositionConstraints(const data: Tb2SolverData): Boolean;
var
   cA, cB: TVector2;
   aA, aB: PhysicsFloat;
   qA, qB: Tb2Rot;
   mA, iA, mB, iB: PhysicsFloat;
   rA, rB, d, axis, perp: TVector2;
   a1, a2, s1, s2: PhysicsFloat;
   impulse: TVector3;
   C1: TVector2;
   C2: PhysicsFloat;
   linearError, angularError: PhysicsFloat;
   active: Boolean;
   translation: PhysicsFloat;
   k11, k12, k13, k22, k23, k33: PhysicsFloat;
   C: TVector3;
   K: TMatrix33;
   K2: TMatrix22;
   impulse1, P: TVector2;
   LA, LB: PhysicsFloat;
begin
   cA := data.positions[m_indexA].c;
   aA := data.positions[m_indexA].a;
   cB := data.positions[m_indexB].c;
   aB := data.positions[m_indexB].a;

   {$IFDEF OP_OVERLOAD}
   qA.SetAngle(aA);
   qB.SetAngle(aB);
   {$ELSE}
   SetAngle(qA, aA);
   SetAngle(qB, aB);
   {$ENDIF}

   mA := m_invMassA;
   mB := m_invMassB;
   iA := m_invIA;
   iB := m_invIB;

   // Compute fresh Jacobians
   {$IFDEF OP_OVERLOAD}
   rA := b2Mul(qA, m_localAnchorA - m_localCenterA);
   rB := b2Mul(qB, m_localAnchorB - m_localCenterB);
   d := cB + rB - cA - rA;

   axis := b2Mul(qA, m_localXAxisA);
   a1 := b2Cross(d + rA, axis);
   a2 := b2Cross(rB, axis);
   perp := b2Mul(qA, m_localYAxisA);

   s1 := b2Cross(d + rA, perp);
   s2 := b2Cross(rB, perp);
   {$ELSE}
   rA := b2Mul(qA, Subtract(m_localAnchorA, m_localCenterA));
   rB := b2Mul(qB, Subtract(m_localAnchorB, m_localCenterB));
   d := Subtract(Add(cB, rB), Add(cA, rA));

   axis := b2Mul(qA, m_localXAxisA);
   a1 := b2Cross(Add(d, rA), axis);
   a2 := b2Cross(rB, axis);
   perp := b2Mul(qA, m_localYAxisA);

   s1 := b2Cross(Add(d, rA), perp);
   s2 := b2Cross(rB, perp);
   {$ENDIF}

   C1.x := b2Dot(perp, d);
   C1.y := aB - aA - m_referenceAngle;

   linearError := Abs(C1.x);
   angularError := Abs(C1.y);

   active := False;
   C2 := 0.0;
   if m_enableLimit then
   begin
      translation := b2Dot(axis, d);
      if Abs(m_upperTranslation - m_lowerTranslation) < 2.0 * b2_linearSlop then
      begin
         // Prevent large angular corrections
         C2 := b2Clamp(translation, -b2_maxLinearCorrection, b2_maxLinearCorrection);
         linearError := b2Max(linearError, Abs(translation));
         active := True;
      end
      else if translation <= m_lowerTranslation then
      begin
         // Prevent large linear corrections and allow some slop.
         C2 := b2Clamp(translation - m_lowerTranslation + b2_linearSlop, -b2_maxLinearCorrection, 0.0);
         linearError := b2Max(linearError, m_lowerTranslation - translation);
         active := True;
      end
      else if translation >= m_upperTranslation then
      begin
         // Prevent large linear corrections and allow some slop.
         C2 := b2Clamp(translation - m_upperTranslation - b2_linearSlop, 0.0, b2_maxLinearCorrection);
         linearError := b2Max(linearError, translation - m_upperTranslation);
         active := True;
      end
   end;

   if active then
   begin
      k11 := mA + mB + iA * s1 * s1 + iB * s2 * s2;
      k12 := iA * s1 + iB * s2;
      k13 := iA * s1 * a1 + iB * s2 * a2;
      k22 := iA + iB;
      if k22 = 0.0 then // For fixed rotation
         k22 := 1.0;
      k23 := iA * a1 + iB * a2;
      k33 := mA + mB + iA * a1 * a1 + iB * a2 * a2;

      SetValue(K.ex, k11, k12, k13);
      SetValue(K.ey, k12, k22, k23);
      SetValue(K.ez, k13, k23, k33);

      C := MakeVector(C1.x, C1.y, C2);
      {$IFDEF OP_OVERLOAD}
      impulse := K.Solve33(-C);
      {$ELSE}
      impulse := Solve33(K, Negative(C));
      {$ENDIF}
   end
   else
   begin
      k11 := mA + mB + iA * s1 * s1 + iB * s2 * s2;
      k12 := iA * s1 + iB * s2;
      k22 := iA + iB;
      if k22 = 0.0 then
         k22 := 1.0;

      SetValue(K2.ex, k11, k12);
      SetValue(K2.ey, k12, k22);

      {$IFDEF OP_OVERLOAD}
      impulse1 := K2.Solve(-C1);
      {$ELSE}
      impulse1 := Solve(K2, Negative(C1));
      {$ENDIF}
      impulse.x := impulse1.x;
      impulse.y := impulse1.y;
      impulse.z := 0.0;
   end;

   {$IFDEF OP_OVERLOAD}
   P := impulse.x * perp + impulse.z * axis;
   {$ELSE}
   P := Add(Multiply(perp, impulse.x), Multiply(axis, impulse.z));
   {$ENDIF}
   LA := impulse.x * s1 + impulse.y + impulse.z * a1;
   LB := impulse.x * s2 + impulse.y + impulse.z * a2;

   {$IFDEF OP_OVERLOAD}
   cA.SubtractBy(mA * P);
   cB.AddBy(mB * P);
   {$ELSE}
   SubtractBy(cA, Multiply(P, mA));
   AddBy(cB, Multiply(P, mB));
   {$ENDIF}

   aA := aA - iA * LA;
   aB := aB + iB * LB;

   data.positions[m_indexA].c := cA;
   data.positions[m_indexA].a := aA;
   data.positions[m_indexB].c := cB;
   data.positions[m_indexB].a := aB;

   Result := (linearError <= b2_linearSlop) and (angularError <= b2_angularSlop);
end;

function Tb2PrismaticJoint.GetJointTranslation: PhysicsFloat;
var
   pA, pB, d, axis: TVector2;
begin
	 pA := m_bodyA.GetWorldPoint(m_localAnchorA);
	 pB := m_bodyB.GetWorldPoint(m_localAnchorB);
   {$IFDEF OP_OVERLOAD}
   d := pB - pA;
   {$ELSE}
   d := Subtract(pB, pA);
   {$ENDIF}
	 axis := m_bodyA.GetWorldVector(m_localXAxisA);

   Result := b2Dot(d, axis);
end;

procedure Tb2PrismaticJoint.EnableLimit(flag: Boolean);
begin
   if flag <> m_enableLimit then
   begin
      m_bodyA.SetAwake(True);
      m_bodyB.SetAwake(True);
      m_enableLimit := flag;
      m_impulse.z := 0.0;
   end;
end;

procedure Tb2PrismaticJoint.EnableMotor(flag: Boolean);
begin
	 m_bodyA.SetAwake(True);
	 m_bodyB.SetAwake(True);
	 m_enableMotor := flag;
end;

function Tb2PrismaticJoint.GetJointSpeed: PhysicsFloat;
var
   rA, rB: TVector2;
   p1, p2, d, axis: TVector2;
   vA, vB: TVector2;
   wA, wB: PhysicsFloat;
begin
   {$IFDEF OP_OVERLOAD}
	 rA := b2Mul(m_bodyA.m_xf.q, m_localAnchorA - m_bodyA.m_sweep.localCenter);
	 rB := b2Mul(m_bodyB.m_xf.q, m_localAnchorB - m_bodyB.m_sweep.localCenter);
	 p1 := m_bodyA.m_sweep.c + rA;
	 p2 := m_bodyB.m_sweep.c + rB;
	 d := p2 - p1;
   {$ELSE}
	 rA := b2Mul(m_bodyA.m_xf.q, Subtract(m_localAnchorA, m_bodyA.m_sweep.localCenter));
	 rB := b2Mul(m_bodyB.m_xf.q, Subtract(m_localAnchorB, m_bodyB.m_sweep.localCenter));
	 p1 := Add(m_bodyA.m_sweep.c, rA);
	 p2 := Add(m_bodyB.m_sweep.c, rB);
	 d := Subtract(p2, p1);
   {$ENDIF}
	 axis := b2Mul(m_bodyA.m_xf.q, m_localXAxisA);

	 vA := m_bodyA.m_linearVelocity;
	 vB := m_bodyB.m_linearVelocity;
	 wA := m_bodyA.m_angularVelocity;
	 wB := m_bodyB.m_angularVelocity;

   {$IFDEF OP_OVERLOAD}
   Result := b2Dot(d, b2Cross(wA, axis)) + b2Dot(axis, vB + b2Cross(wB, rB) - vA - b2Cross(wA, rA));
   {$ELSE}
   Result := b2Dot(d, b2Cross(wA, axis)) + b2Dot(axis, Subtract(Add(vB, b2Cross(wB, rB)), Add(vA, b2Cross(wA, rA))));
   {$ENDIF}
end;

function Tb2PrismaticJoint.GetMotorForce(inv_dt: PhysicsFloat): PhysicsFloat;
begin
   Result := inv_dt * m_motorImpulse;
end;

procedure Tb2PrismaticJoint.SetLimits(lower, upper: PhysicsFloat);
begin
   //b2Assert(lower <= upper);
   if (lower <> m_lowerTranslation) and (upper <> m_upperTranslation) then
   begin
      m_bodyA.SetAwake(True);
      m_bodyB.SetAwake(True);
      m_lowerTranslation := lower;
      m_upperTranslation := upper;
      m_impulse.z := 0.0;
   end;
end;

procedure Tb2PrismaticJoint.SetMotorSpeed(speed: PhysicsFloat);
begin
   m_bodyA.SetAwake(True);
   m_bodyB.SetAwake(True);
	 m_motorSpeed := speed;
end;

procedure Tb2PrismaticJoint.SetMaxMotorForce(force: PhysicsFloat);
begin
   m_bodyA.SetAwake(True);
   m_bodyB.SetAwake(True);
   m_maxMotorForce := force;
end;

{ Tb2MouseJointDef }

// p = attached point, m = mouse point
// C = p - m
// Cdot = v
//      = v + cross(w, r)
// J = [I r_skew]
// Identity used:
// w k % (rx i + ry j) = w * (-ry i + rx j)

constructor Tb2MouseJointDef.Create;
begin
    inherited;
		jointType := e_mouseJoint;
    target := b2Vec2_Zero;
		maxForce := 0.0;
		frequencyHz := 5.0;
		dampingRatio := 0.7;
end;

{ Tb2MouseJoint }

constructor Tb2MouseJoint.Create(def: Tb2MouseJointDef);
begin
   //b2Assert(def->target.IsValid());
   //b2Assert(b2IsValid(def->maxForce) && def->maxForce >= 0.0f);
   //b2Assert(b2IsValid(def->frequencyHz) && def->frequencyHz >= 0.0f);
   //b2Assert(b2IsValid(def->dampingRatio) && def->dampingRatio >= 0.0f);

   inherited Create(def);
   m_targetA := def.target;
   m_localAnchorB := b2MulT(m_bodyB.m_xf, m_targetA);

   m_maxForce := def.maxForce;
   m_impulse := b2Vec2_Zero;

   m_frequencyHz := def.frequencyHz;
   m_dampingRatio := def.dampingRatio;

   m_beta := 0.0;
   m_gamma := 0.0;
end;

procedure Tb2MouseJoint.ShiftOrigin(const newOrigin: TVector2);
begin
  {$IFDEF OP_OVERLOAD}
  m_targetA.SubtractBy(newOrigin);
  {$ELSE}
  SubtractBy(m_targetA, newOrigin);
  {$ENDIF}
end;

function Tb2MouseJoint.GetAnchorA: TVector2;
begin
   Result := m_targetA;
end;

function Tb2MouseJoint.GetAnchorB: TVector2;
begin
   Result := m_bodyB.GetWorldPoint(m_localAnchorB);
end;

function Tb2MouseJoint.GetReactionForce(inv_dt: PhysicsFloat): TVector2;
begin
   {$IFDEF OP_OVERLOAD}
   Result := inv_dt * m_impulse;
   {$ELSE}
   Result := Multiply(m_impulse, inv_dt);
   {$ENDIF}
end;

function Tb2MouseJoint.GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat;
begin
   Result := 0.0;
end;

procedure Tb2MouseJoint.InitVelocityConstraints(const data: Tb2SolverData);
var
   cB, vB: TVector2;
   aB, wB: PhysicsFloat;
   qB: Tb2Rot;
   mass: PhysicsFloat;
   h: PhysicsFloat;
   omega, d, _k: PhysicsFloat;
   K: TMatrix22;
begin
   m_indexB := m_bodyB.m_islandIndex;
   m_localCenterB := m_bodyB.m_sweep.localCenter;
   m_invMassB := m_bodyB.m_invMass;
   m_invIB := m_bodyB.m_invI;

   cB := data.positions[m_indexB].c;
   aB := data.positions[m_indexB].a;
   vB := data.velocities[m_indexB].v;
   wB := data.velocities[m_indexB].w;

   {$IFDEF OP_OVERLOAD}
   qB.SetAngle(aB);
   {$ELSE}
   SetAngle(qB, aB);
   {$ENDIF}

   mass := m_bodyB.m_mass;

   // Frequency
   omega := 2.0 * Pi * m_frequencyHz;

   // Damping coefficient
   d := 2.0 * mass * m_dampingRatio * omega;

   // Spring stiffness
   _k := mass * (omega * omega);

   // magic formulas
   // gamma has units of inverse mass.
   // beta has units of inverse time.
   h := data.step.dt;
   // b2Assert(d + h * k > b2_epsilon);
   m_gamma := h * (d + h * _k);
   if m_gamma <> 0.0 then
      m_gamma := 1.0 / m_gamma;
   m_beta := h * _k * m_gamma;

   // Compute the effective mass matrix.
   {$IFDEF OP_OVERLOAD}
   m_rB := b2Mul(qB, m_localAnchorB - m_localCenterB);
   {$ELSE}
   m_rB := b2Mul(qB, Subtract(m_localAnchorB, m_localCenterB));
   {$ENDIF}

   // K    := [(1/m1 + 1/m2) * eye(2) - skew(r1) * invI1 * skew(r1) - skew(r2) * invI2 * skew(r2)]
   //      := [1/m1+1/m2     0    ] + invI1 * [r1.y*r1.y -r1.x*r1.y] + invI2 * [r1.y*r1.y -r1.x*r1.y]
   //        [    0     1/m1+1/m2]           [-r1.x*r1.y r1.x*r1.x]           [-r1.x*r1.y r1.x*r1.x]
   K.ex.x := m_invMassB + m_invIB * m_rB.y * m_rB.y + m_gamma;
   K.ex.y := -m_invIB * m_rB.x * m_rB.y;
   K.ey.x := K.ex.y;
   K.ey.y := m_invMassB + m_invIB * m_rB.x * m_rB.x + m_gamma;

   {$IFDEF OP_OVERLOAD}
   m_mass := K.Invert;
   m_C := cB + m_rB - m_targetA;
   m_C.MultiplyBy(m_beta);
   {$ELSE}
   m_mass := Invert(K);
   m_C := Add(cB, m_rB);
   SubtractBy(m_C, m_targetA);
   MultiplyBy(m_C, m_beta);
   {$ENDIF}

   // Cheat with some damping
   wB := wB * 0.98;

   // Warm starting.
   if data.step.warmStarting then
   begin
      {$IFDEF OP_OVERLOAD}
      m_impulse.MultiplyBy(data.step.dtRatio);
      vB.AddBy(m_invMassB * m_impulse);
      {$ELSE}
      MultiplyBy(m_impulse, data.step.dtRatio);
      AddBy(vB, Multiply(m_impulse, m_invMassB));
      {$ENDIF}
      wB := wB + m_invIB * b2Cross(m_rB, m_impulse);
   end
   else
      m_impulse := b2Vec2_Zero;

   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
end;

procedure Tb2MouseJoint.SolveVelocityConstraints(const data: Tb2SolverData);
var
   vB: TVector2;
   wB: PhysicsFloat;
   Cdot, impulse, oldImpulse: TVector2;
   maxImpulse: PhysicsFloat;
begin
   vB := data.velocities[m_indexB].v;
   wB := data.velocities[m_indexB].w;

   oldImpulse := m_impulse;
   // Cdot := v + cross(w, r)
   {$IFDEF OP_OVERLOAD}
   Cdot := vB + b2Cross(wB, m_rB);
   impulse := b2Mul(m_mass, -(Cdot + m_C + m_gamma * m_impulse));
   m_impulse.AddBy(impulse);
   maxImpulse := data.step.dt * m_maxForce;
   if m_impulse.SqrLength > maxImpulse * maxImpulse then
      m_impulse.MultiplyBy(maxImpulse / m_impulse.Length);
   impulse := m_impulse - oldImpulse;

 	 vB.AddBy(m_invMassB * impulse);
   {$ELSE}
   Cdot := Add(vB, b2Cross(wB, m_rB));
   impulse := b2Mul(m_mass, Negative(Add(Cdot, m_C, Multiply(m_impulse, m_gamma))));
   AddBy(m_impulse, impulse);
   maxImpulse := data.step.dt * m_maxForce;
   if SqrLength(m_impulse) > maxImpulse * maxImpulse then
      MultiplyBy(m_impulse, maxImpulse / LengthVec(m_impulse));
   impulse := Subtract(m_impulse, oldImpulse);

   AddBy(vB, Multiply(impulse, m_invMassB));
   {$ENDIF}
   wB := wB + m_invIB * b2Cross(m_rB, impulse);

   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
end;

function Tb2MouseJoint.SolvePositionConstraints(const data: Tb2SolverData): Boolean;
begin
   // B2_NOT_USED(data)
   Result := True;
end;

procedure Tb2MouseJoint.SetTarget(const target: TVector2);
begin
   if not m_bodyB.IsAwake then
      m_bodyB.SetAwake(True);
   m_targetA := target;
end;

function Tb2MouseJoint.GetTarget: TVector2;
begin
   Result := m_targetA;
end;

{ Tb2PulleyJointDef }

// Pulley:
// length1 = norm(p1 - s1)
// length2 = norm(p2 - s2)
// C0 = (length1 + ratio * length2)_initial
// C = C0 - (length1 + ratio * length2)
// u1 = (p1 - s1) / norm(p1 - s1)
// u2 = (p2 - s2) / norm(p2 - s2)
// Cdot = -dot(u1, v1 + cross(w1, r1)) - ratio * dot(u2, v2 + cross(w2, r2))
// J = -[u1 cross(r1, u1) ratio * u2  ratio * cross(r2, u2)]
// K = J * invM * JT

const
   b2_minPulleyLength = 2.0;

constructor Tb2PulleyJointDef.Create;
begin
   inherited;
   jointType := e_pulleyJoint;
   SetValue(groundAnchorA, -1.0, 1.0);
   SetValue(groundAnchorB, 1.0, 1.0);
   SetValue(localAnchorA, -1.0, 0.0);
   SetValue(localAnchorB, 1.0, 0.0);
   lengthA := 0.0;
   lengthB := 0.0;
   ratio := 1.0;
   collideConnected := True;
end;

procedure Tb2PulleyJointDef.Initialize(bodyA, bodyB: Tb2Body; const groundAnchorA,
   groundAnchorB, anchorA, anchorB: TVector2; ratio: PhysicsFloat);
begin
   Self.bodyA := bodyA;
   Self.bodyB := bodyB;
   Self.groundAnchorA := groundAnchorA;
   Self.groundAnchorB := groundAnchorB;
   Self.localAnchorA := bodyA.GetLocalPoint(anchorA);
   Self.localAnchorB := bodyB.GetLocalPoint(anchorB);
   {$IFDEF OP_OVERLOAD}
   lengthA := (anchorA - groundAnchorA).Length;
   lengthB := (anchorB - groundAnchorB).Length;
   {$ELSE}
   lengthA := LengthVec(Subtract(anchorA, groundAnchorA));
   lengthB := LengthVec(Subtract(anchorB, groundAnchorB));
   {$ENDIF}
   Self.ratio := ratio;
   //b2Assert(ratio > b2_epsilon);
end;

{ Tb2PulleyJoint }

constructor Tb2PulleyJoint.Create(def: Tb2PulleyJointDef);
begin
   inherited Create(def);
   m_groundAnchorA := def.groundAnchorA;
   m_groundAnchorB := def.groundAnchorB;
   m_localAnchorA := def.localAnchorA;
   m_localAnchorB := def.localAnchorB;

   m_lengthA := def.lengthA;
   m_lengthB := def.lengthB;

   //b2Assert(def.ratio != 0.0);
   m_ratio := def.ratio;
   m_constant := def.lengthA + m_ratio * def.lengthB;

   m_impulse := 0.0;
end;

{$IFDEF ENABLE_DUMP}
procedure Tb2PulleyJoint.Dump;
var
   indexA, indexB: Int32;
begin
   indexA := m_bodyA.m_islandIndex;
   indexB := m_bodyB.m_islandIndex;

   b2DumpMethod(1, 'begin', []);
   b2DumpMethod(2, 'pulley_jd := Tb2PulleyJointDef.Create;', []);
   b2DumpMethod(2, 'pulley_jd.bodyA := bodies[%d];', [indexA]);
   b2DumpMethod(2, 'pulley_jd.bodyB := bodies[%d];', [indexB]);
   b2DumpMethod(2, 'pulley_jd.collideConnected := %s;', [b2BoolToStr(m_collideConnected)]);
   b2DumpMethod(2, 'pulley_jd.groundAnchorA := MakeVector(%s, %s);', [b2FloatToStr(m_groundAnchorA.x), b2FloatToStr(m_groundAnchorA.y)]);
   b2DumpMethod(2, 'pulley_jd.groundAnchorB := MakeVector(%s, %s);', [b2FloatToStr(m_groundAnchorB.x), b2FloatToStr(m_groundAnchorB.y)]);
   b2DumpMethod(2, 'pulley_jd.localAnchorA := MakeVector(%s, %s);', [b2FloatToStr(m_localAnchorA.x), b2FloatToStr(m_localAnchorA.y)]);
   b2DumpMethod(2, 'pulley_jd.localAnchorB := MakeVector(%s, %s);', [b2FloatToStr(m_localAnchorB.x), b2FloatToStr(m_localAnchorB.y)]);
   b2DumpMethod(2, 'pulley_jd.lengthA := %s;', [b2FloatToStr(m_lengthA)]);
   b2DumpMethod(2, 'pulley_jd.lengthB := %s;', [b2FloatToStr(m_lengthB)]);
   b2DumpMethod(2, 'pulley_jd.ratio := %s;', [b2FloatToStr(m_ratio)]);
   b2DumpMethod(2, 'joints[%d] := m_world.CreateJoint(pulley_jd);', [m_index]);
   b2DumpMethod(1, 'end;', []);
end;
{$ENDIF}

procedure Tb2PulleyJoint.ShiftOrigin(const newOrigin: TVector2);
begin
 	{$IFDEF OP_OVERLOAD}
 	m_groundAnchorA.SubtractBy(newOrigin);
 	m_groundAnchorB.SubtractBy(newOrigin);
 	{$ELSE}
 	SubtractBy(m_groundAnchorA, newOrigin);
 	SubtractBy(m_groundAnchorB, newOrigin);
 	{$ENDIF}
end;

function Tb2PulleyJoint.GetAnchorA: TVector2;
begin
   Result := m_bodyA.GetWorldPoint(m_localAnchorA);
end;

function Tb2PulleyJoint.GetAnchorB: TVector2;
begin
   Result := m_bodyB.GetWorldPoint(m_localAnchorB);
end;

function Tb2PulleyJoint.GetReactionForce(inv_dt: PhysicsFloat): TVector2;
begin
   {$IFDEF OP_OVERLOAD}
   Result := (m_impulse * inv_dt) * m_uB;
   {$ELSE}
   Result := Multiply(m_uB, m_impulse * inv_dt);
   {$ENDIF}
end;

function Tb2PulleyJoint.GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat;
begin
   Result := 0.0;
end;

procedure Tb2PulleyJoint.InitVelocityConstraints(const data: Tb2SolverData);
var
   cA, cB, vA, vB: TVector2;
   aA, aB, wA, wB: PhysicsFloat;
   qA, qB: Tb2Rot;
   lengthA, lengthB: PhysicsFloat;
   ruA, ruB: PhysicsFloat;
   mA, mB: PhysicsFloat;
   PA, PB: TVector2;
begin
   m_indexA := m_bodyA.m_islandIndex;
   m_indexB := m_bodyB.m_islandIndex;
   m_localCenterA := m_bodyA.m_sweep.localCenter;
   m_localCenterB := m_bodyB.m_sweep.localCenter;
   m_invMassA := m_bodyA.m_invMass;
   m_invMassB := m_bodyB.m_invMass;
   m_invIA := m_bodyA.m_invI;
   m_invIB := m_bodyB.m_invI;

   cA := data.positions[m_indexA].c;
   aA := data.positions[m_indexA].a;
   vA := data.velocities[m_indexA].v;
   wA := data.velocities[m_indexA].w;

   cB := data.positions[m_indexB].c;
   aB := data.positions[m_indexB].a;
   vB := data.velocities[m_indexB].v;
   wB := data.velocities[m_indexB].w;

   {$IFDEF OP_OVERLOAD}
   qA.SetAngle(aA);
   qB.SetAngle(aB);

   m_rA := b2Mul(qA, m_localAnchorA - m_localCenterA);
   m_rB := b2Mul(qB, m_localAnchorB - m_localCenterB);

   // Get the pulley axes.
   m_uA := cA + m_rA - m_groundAnchorA;
   m_uB := cB + m_rB - m_groundAnchorB;

   lengthA := m_uA.Length;
   lengthB := m_uB.Length;
   {$ELSE}
   SetAngle(qA, aA);
   SetAngle(qB, aB);

   m_rA := b2Mul(qA, Subtract(m_localAnchorA, m_localCenterA));
   m_rB := b2Mul(qB, Subtract(m_localAnchorB, m_localCenterB));

   // Get the pulley axes.
   m_uA := Subtract(Add(cA, m_rA), m_groundAnchorA);
   m_uB := Subtract(Add(cB, m_rB), m_groundAnchorB);

   lengthA := LengthVec(m_uA);
   lengthB := LengthVec(m_uB);
   {$ENDIF}

   if lengthA > 10.0 * b2_linearSlop then
      {$IFDEF OP_OVERLOAD}
      m_uA.MultiplyBy(1.0 / lengthA)
      {$ELSE}
      MultiplyBy(m_uA, 1.0 / lengthA)
      {$ENDIF}
   else
      m_uA := b2Vec2_Zero;

   if lengthB > 10.0 * b2_linearSlop then
      {$IFDEF OP_OVERLOAD}
      m_uB.MultiplyBy(1.0 / lengthB)
      {$ELSE}
      MultiplyBy(m_uB, 1.0 / lengthB)
      {$ENDIF}
   else
      m_uB := b2Vec2_Zero;

   // Compute effective mass.
   ruA := b2Cross(m_rA, m_uA);
   ruB := b2Cross(m_rB, m_uB);

   mA := m_invMassA + m_invIA * ruA * ruA;
   mB := m_invMassB + m_invIB * ruB * ruB;

   m_mass := mA + m_ratio * m_ratio * mB;

   if m_mass > 0.0 then
      m_mass := 1.0 / m_mass;

   if data.step.warmStarting then
   begin
     // Scale impulses to support variable time steps.
     m_impulse := m_impulse * data.step.dtRatio;

     // Warm starting.
     {$IFDEF OP_OVERLOAD}
     PA := -(m_impulse) * m_uA;
     PB := (-m_ratio * m_impulse) * m_uB;
     vA.AddBy(m_invMassA * PA);
     vB.AddBy(m_invMassB * PB);
     {$ELSE}
     PA := Multiply(m_uA, -m_impulse);
     PB := Multiply(m_uB, -m_ratio * m_impulse);
     AddBy(vA, Multiply(PA, m_invMassA));
     AddBy(vB, Multiply(PB, m_invMassB));
     {$ENDIF}
     wA := wA + m_invIA * b2Cross(m_rA, PA);
     wB := wB + m_invIB * b2Cross(m_rB, PB);
   end
   else
      m_impulse := 0.0;

   data.velocities[m_indexA].v := vA;
   data.velocities[m_indexA].w := wA;
   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
end;

procedure Tb2PulleyJoint.SolveVelocityConstraints(const data: Tb2SolverData);
var
   vA, vB: TVector2;
   vpA, vpB: TVector2;
   wA, wB: PhysicsFloat;
   Cdot, impulse: PhysicsFloat;
   PA, PB: TVector2;
begin
   vA := data.velocities[m_indexA].v;
   wA := data.velocities[m_indexA].w;
   vB := data.velocities[m_indexB].v;
   wB := data.velocities[m_indexB].w;

   {$IFDEF OP_OVERLOAD}
   vpA := vA + b2Cross(wA, m_rA);
   vpB := vB + b2Cross(wB, m_rB);
   {$ELSE}
   vpA := Add(vA, b2Cross(wA, m_rA));
   vpB := Add(vB, b2Cross(wB, m_rB));
   {$ENDIF}

   Cdot := -b2Dot(m_uA, vpA) - m_ratio * b2Dot(m_uB, vpB);
   impulse := -m_mass * Cdot;
   m_impulse := m_impulse + impulse;

   {$IFDEF OP_OVERLOAD}
   PA := -impulse * m_uA;
   PB := -m_ratio * impulse * m_uB;
   vA.AddBy(m_invMassA * PA);
   vB.AddBy(m_invMassB * PB);
   {$ELSE}
   PA := Multiply(m_uA, -impulse);
   PB := Multiply(m_uB, -m_ratio * impulse);
   AddBy(vA, Multiply(PA, m_invMassA));
   AddBy(vB, Multiply(PB, m_invMassB));
   {$ENDIF}
   wA := wA + m_invIA * b2Cross(m_rA, PA);
   wB := wB + m_invIB * b2Cross(m_rB, PB);

   data.velocities[m_indexA].v := vA;
   data.velocities[m_indexA].w := wA;
   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
end;

function Tb2PulleyJoint.SolvePositionConstraints(const data: Tb2SolverData): Boolean;
var
   cA, cB: TVector2;
   aA, aB: PhysicsFloat;
   qA, qB: Tb2Rot;
   rA, rB, uA, uB: TVector2;
   lengthA, lengthB: PhysicsFloat;
   ruA, ruB: PhysicsFloat;
   mA, mB, mass: PhysicsFloat;
   C, linearError, impulse: PhysicsFloat;
   PA, PB: TVector2;
begin
   cA := data.positions[m_indexA].c;
   aA := data.positions[m_indexA].a;
   cB := data.positions[m_indexB].c;
   aB := data.positions[m_indexB].a;

   {$IFDEF OP_OVERLOAD}
   qA.SetAngle(aA);
   qB.SetAngle(aB);

   rA := b2Mul(qA, m_localAnchorA - m_localCenterA);
   rB := b2Mul(qB, m_localAnchorB - m_localCenterB);

   // Get the pulley axes.
   uA := cA + rA - m_groundAnchorA;
   uB := cB + rB - m_groundAnchorB;

   lengthA := uA.Length;
   lengthB := uB.Length;
   {$ELSE}
   SetAngle(qA, aA);
   SetAngle(qB, aB);

   rA := b2Mul(qA, Subtract(m_localAnchorA, m_localCenterA));
   rB := b2Mul(qB, Subtract(m_localAnchorB, m_localCenterB));

   // Get the pulley axes.
   uA := Subtract(Add(cA, rA), m_groundAnchorA);
   uB := Subtract(Add(cB, rB), m_groundAnchorB);

   lengthA := LengthVec(uA);
   lengthB := LengthVec(uB);
   {$ENDIF}

   if lengthA > 10.0 * b2_linearSlop then
      {$IFDEF OP_OVERLOAD}
      uA.MultiplyBy(1.0 / lengthA)
      {$ELSE}
      MultiplyBy(uA, 1.0 / lengthA)
      {$ENDIF}
   else
      uA := b2Vec2_Zero;

   if lengthB > 10.0 * b2_linearSlop then
      {$IFDEF OP_OVERLOAD}
      uB.MultiplyBy(1.0 / lengthB)
      {$ELSE}
      MultiplyBy(uB, 1.0 / lengthB)
      {$ENDIF}
   else
      uB := b2Vec2_Zero;

   // Compute effective mass.
   ruA := b2Cross(rA, uA);
   ruB := b2Cross(rB, uB);

   mA := m_invMassA + m_invIA * ruA * ruA;
   mB := m_invMassB + m_invIB * ruB * ruB;
   mass := mA + m_ratio * m_ratio * mB;

   if mass > 0.0 then
      mass := 1.0 / mass;

   C := m_constant - lengthA - m_ratio * lengthB;
   linearError := Abs(C);

   impulse := -mass * C;

   {$IFDEF OP_OVERLOAD}
   PA := -impulse * uA;
   PB := -m_ratio * impulse * uB;
   cA.AddBy(m_invMassA * PA);
   cB.AddBy(m_invMassB * PB);
   {$ELSE}
   PA := Multiply(uA, -impulse);
   PB := Multiply(uB, -m_ratio * impulse);
   AddBy(cA, Multiply(PA, m_invMassA));
   AddBy(cB, Multiply(PB, m_invMassB));
   {$ENDIF}
   aA := aA + m_invIA * b2Cross(rA, PA);
   aB := aB + m_invIB * b2Cross(rB, PB);

   data.positions[m_indexA].c := cA;
   data.positions[m_indexA].a := aA;
   data.positions[m_indexB].c := cB;
   data.positions[m_indexB].a := aB;

   Result := linearError < b2_linearSlop;
end;

function Tb2PulleyJoint.GetLengthA: PhysicsFloat;
begin
   {$IFDEF OP_OVERLOAD}
   Result := (m_bodyA.GetWorldPoint(m_localAnchorA) - m_groundAnchorA).Length;
   {$ELSE}
   Result := LengthVec(Subtract(m_bodyA.GetWorldPoint(m_localAnchorA), m_groundAnchorA));
   {$ENDIF}
end;

function Tb2PulleyJoint.GetLengthB: PhysicsFloat;
begin
   {$IFDEF OP_OVERLOAD}
   Result := (m_bodyB.GetWorldPoint(m_localAnchorB) - m_groundAnchorB).Length;
   {$ELSE}
   Result := LengthVec(Subtract(m_bodyB.GetWorldPoint(m_localAnchorB), m_groundAnchorB));
   {$ENDIF}
end;

function Tb2PulleyJoint.GetGroundAnchorA: TVector2;
begin
   Result := m_groundAnchorA;
end;

function Tb2PulleyJoint.GetGroundAnchorB: TVector2;
begin
   Result := m_groundAnchorB;
end;

function Tb2PulleyJoint.GetCurrentLengthA: PhysicsFloat;
var
   d: TVector2;
begin
   {$IFDEF OP_OVERLOAD}
   d := m_bodyA.GetWorldPoint(m_localAnchorA) - m_groundAnchorA;
   Result := d.Length;
   {$ELSE}
   d := Subtract(m_bodyA.GetWorldPoint(m_localAnchorA), m_groundAnchorA);
   Result := LengthVec(d);
   {$ENDIF}
end;

function Tb2PulleyJoint.GetCurrentLengthB: PhysicsFloat;
var
   d: TVector2;
begin
   {$IFDEF OP_OVERLOAD}
   d := m_bodyB.GetWorldPoint(m_localAnchorB) - m_groundAnchorB;
   Result := d.Length;
   {$ELSE}
   d := Subtract(m_bodyB.GetWorldPoint(m_localAnchorB), m_groundAnchorB);
   Result := LengthVec(d);
   {$ENDIF}
end;

{ Tb2RevoluteJointDef }

// Point-to-point constraint
// C = p2 - p1
// Cdot = v2 - v1
//      = v2 + cross(w2, r2) - v1 - cross(w1, r1)
// J = [-I -r1_skew I r2_skew ]
// Identity used:
// w k % (rx i + ry j) = w * (-ry i + rx j)

// Motor constraint
// Cdot = w2 - w1
// J = [0 0 -1 0 0 1]
// K = invI1 + invI2

constructor Tb2RevoluteJointDef.Create;
begin
   inherited;
   jointType := e_revoluteJoint;
   localAnchorA := b2Vec2_Zero;
   localAnchorB := b2Vec2_Zero;
   referenceAngle := 0.0;
   lowerAngle := 0.0;
   upperAngle := 0.0;
   maxMotorTorque := 0.0;
   motorSpeed := 0.0;
   enableLimit := False;
   enableMotor := False;
end;

procedure Tb2RevoluteJointDef.Initialize(bodyA, bodyB: Tb2Body; const anchor: TVector2);
begin
   Self.bodyA := bodyA;
	 Self.bodyB := bodyB;
	 localAnchorA := bodyA.GetLocalPoint(anchor);
	 localAnchorB := bodyB.GetLocalPoint(anchor);
	 referenceAngle := bodyB.GetAngle - bodyA.GetAngle;
end;

{ Tb2RevoluteJoint }

constructor Tb2RevoluteJoint.Create(def: Tb2RevoluteJointDef);
begin
   inherited Create(def);
   m_localAnchorA := def.localAnchorA;
   m_localAnchorB := def.localAnchorB;
   m_referenceAngle := def.referenceAngle;

   m_impulse := b2Vec3_Zero;
	 m_motorImpulse := 0.0;

   m_lowerAngle := def.lowerAngle;
   m_upperAngle := def.upperAngle;
   m_maxMotorTorque := def.maxMotorTorque;
   m_motorSpeed := def.motorSpeed;
   m_enableLimit := def.enableLimit;
   m_enableMotor := def.enableMotor;
   m_limitState := e_inactiveLimit;
end;

{$IFDEF ENABLE_DUMP}
procedure Tb2RevoluteJoint.Dump;
var
   indexA, indexB: Int32;
begin
   indexA := m_bodyA.m_islandIndex;
   indexB := m_bodyB.m_islandIndex;

   b2DumpMethod(1, 'begin', []);
   b2DumpMethod(2, 'revolute_jd := Tb2RevoluteJointDef.Create;', []);
   b2DumpMethod(2, 'revolute_jd.bodyA := bodies[%d];', [indexA]);
   b2DumpMethod(2, 'revolute_jd.bodyB := bodies[%d];', [indexB]);
   b2DumpMethod(2, 'revolute_jd.collideConnected := %s;', [b2BoolToStr(m_collideConnected)]);
   b2DumpMethod(2, 'revolute_jd.localAnchorA := MakeVector(%s, %s);', [b2FloatToStr(m_localAnchorA.x), b2FloatToStr(m_localAnchorA.y)]);
   b2DumpMethod(2, 'revolute_jd.localAnchorB := MakeVector(%s, %s);', [b2FloatToStr(m_localAnchorB.x), b2FloatToStr(m_localAnchorB.y)]);
   b2DumpMethod(2, 'revolute_jd.referenceAngle := %s;', [b2FloatToStr(m_referenceAngle)]);
   b2DumpMethod(2, 'revolute_jd.enableLimit := %s;', [b2BoolToStr(m_enableLimit)]);
   b2DumpMethod(2, 'revolute_jd.lowerAngle := %s;', [b2FloatToStr(m_lowerAngle)]);
   b2DumpMethod(2, 'revolute_jd.upperAngle := %s;', [b2FloatToStr(m_upperAngle)]);
   b2DumpMethod(2, 'revolute_jd.enableMotor := %s;', [b2BoolToStr(m_enableMotor)]);
   b2DumpMethod(2, 'revolute_jd.motorSpeed := %s;', [b2FloatToStr(m_motorSpeed)]);
   b2DumpMethod(2, 'revolute_jd.maxMotorTorque := %s;', [b2FloatToStr(m_maxMotorTorque)]);
   b2DumpMethod(2, 'joints[%d] := m_world.CreateJoint(revolute_jd);', [m_index]);
   b2DumpMethod(1, 'end;', []);
end;
{$ENDIF}

function Tb2RevoluteJoint.GetAnchorA: TVector2;
begin
   Result := m_bodyA.GetWorldPoint(m_localAnchorA);
end;

function Tb2RevoluteJoint.GetAnchorB: TVector2;
begin
   Result := m_bodyB.GetWorldPoint(m_localAnchorB);
end;

function Tb2RevoluteJoint.GetReactionForce(inv_dt: PhysicsFloat): TVector2;
begin
   Result.x := m_impulse.x * inv_dt;
   Result.y := m_impulse.y * inv_dt;
end;

function Tb2RevoluteJoint.GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat;
begin
   Result := inv_dt * m_impulse.z;
end;

procedure Tb2RevoluteJoint.InitVelocityConstraints(const data: Tb2SolverData);
var
   vA, vB: TVector2;
   aA, aB: PhysicsFloat;
   wA, wB: PhysicsFloat;
   qA, qB: Tb2Rot;
   mA, mB, iA, iB: PhysicsFloat;
   fixedRotation: Boolean;
   jointAngle: PhysicsFloat;
   P: TVector2;
begin
   m_indexA := m_bodyA.m_islandIndex;
   m_indexB := m_bodyB.m_islandIndex;
   m_localCenterA := m_bodyA.m_sweep.localCenter;
   m_localCenterB := m_bodyB.m_sweep.localCenter;
   m_invMassA := m_bodyA.m_invMass;
   m_invMassB := m_bodyB.m_invMass;
   m_invIA := m_bodyA.m_invI;
   m_invIB := m_bodyB.m_invI;

   aA := data.positions[m_indexA].a;
   vA := data.velocities[m_indexA].v;
   wA := data.velocities[m_indexA].w;

   aB := data.positions[m_indexB].a;
   vB := data.velocities[m_indexB].v;
   wB := data.velocities[m_indexB].w;

   {$IFDEF OP_OVERLOAD}
   qA.SetAngle(aA);
   qB.SetAngle(aB);

   m_rA := b2Mul(qA, m_localAnchorA - m_localCenterA);
   m_rB := b2Mul(qB, m_localAnchorB - m_localCenterB);
   {$ELSE}
   SetAngle(qA, aA);
   SetAngle(qB, aB);

   m_rA := b2Mul(qA, Subtract(m_localAnchorA, m_localCenterA));
   m_rB := b2Mul(qB, Subtract(m_localAnchorB, m_localCenterB));
   {$ENDIF}

   // J := [-I -r1_skew I r2_skew]
   //     [ 0       -1 0       1]
   // r_skew := [-ry; rx]

   // Matlab
   // K := [ mA+r1y^2*iA+mB+r2y^2*iB,  -r1y*iA*r1x-r2y*iB*r2x,          -r1y*iA-r2y*iB]
   //     [  -r1y*iA*r1x-r2y*iB*r2x, mA+r1x^2*iA+mB+r2x^2*iB,           r1x*iA+r2x*iB]
   //     [          -r1y*iA-r2y*iB,           r1x*iA+r2x*iB,                   iA+iB]

   mA := m_invMassA;
   mB := m_invMassB;
   iA := m_invIA;
   iB := m_invIB;

   fixedRotation := iA + iB = 0.0;

   m_mass.ex.x := mA + mB + m_rA.y * m_rA.y * iA + m_rB.y * m_rB.y * iB;
   m_mass.ey.x := -m_rA.y * m_rA.x * iA - m_rB.y * m_rB.x * iB;
   m_mass.ez.x := -m_rA.y * iA - m_rB.y * iB;
   m_mass.ex.y := m_mass.ey.x;
   m_mass.ey.y := mA + mB + m_rA.x * m_rA.x * iA + m_rB.x * m_rB.x * iB;
   m_mass.ez.y := m_rA.x * iA + m_rB.x * iB;
   m_mass.ex.z := m_mass.ez.x;
   m_mass.ey.z := m_mass.ez.y;
   m_mass.ez.z := iA + iB;

   m_motorMass := iA + iB;
   if m_motorMass > 0.0 then
      m_motorMass := 1.0 / m_motorMass;

   if (not m_enableMotor) or fixedRotation then
      m_motorImpulse := 0.0;

   if m_enableLimit and (not fixedRotation) then
   begin
      jointAngle := aB - aA - m_referenceAngle;
      if Abs(m_upperAngle - m_lowerAngle) < 2.0 * b2_angularSlop then
         m_limitState := e_equalLimits
      else if jointAngle <= m_lowerAngle then
      begin
         if m_limitState <> e_atLowerLimit then
            m_impulse.z := 0.0;
         m_limitState := e_atLowerLimit;
      end
      else if jointAngle >= m_upperAngle then
      begin
         if m_limitState <> e_atUpperLimit then
            m_impulse.z := 0.0;
         m_limitState := e_atUpperLimit;
      end
      else
      begin
         m_limitState := e_inactiveLimit;
         m_impulse.z := 0.0;
      end;
   end
   else
      m_limitState := e_inactiveLimit;

   if data.step.warmStarting then
   begin
      // Scale impulses to support a variable time step.
      {$IFDEF OP_OVERLOAD}
      m_impulse.MultiplyBy(data.step.dtRatio);
      {$ELSE}
      MultiplyBy(m_impulse, data.step.dtRatio);
      {$ENDIF}
      m_motorImpulse := m_motorImpulse * data.step.dtRatio;

      P.x := m_impulse.x;
      P.y := m_impulse.y;

      {$IFDEF OP_OVERLOAD}
      vA.SubtractBy(mA * P);
      vB.AddBy(mB * P);
      {$ELSE}
      SubtractBy(vA, Multiply(P, mA));
      AddBy(vB, Multiply(P, mB));
      {$ENDIF}

      wA := wA - iA * (b2Cross(m_rA, P) + m_motorImpulse + m_impulse.z);
      wB := wB + iB * (b2Cross(m_rB, P) + m_motorImpulse + m_impulse.z);
   end
   else
   begin
      m_impulse := b2Vec3_Zero;
      m_motorImpulse := 0.0;
   end;

   data.velocities[m_indexA].v := vA;
   data.velocities[m_indexA].w := wA;
   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
end;

procedure Tb2RevoluteJoint.SolveVelocityConstraints(const data: Tb2SolverData);
var
   vA, vB: TVector2;
   wA, wB: PhysicsFloat;
   mA, iA, mB, iB: PhysicsFloat;
   fixedRotation: Boolean;
   Cdotf: PhysicsFloat;
   impulsef, oldImpulse, maxImpulse: PhysicsFloat;
   Cdot1: TVector2;
   Cdot2, newImpulse: PhysicsFloat;
   Cdot, impulse: TVector3;
   rhs, reduced, P: TVector2;
   impulse2: TVector2;
begin
   vA := data.velocities[m_indexA].v;
   wA := data.velocities[m_indexA].w;
   vB := data.velocities[m_indexB].v;
   wB := data.velocities[m_indexB].w;

   mA := m_invMassA;
   mB := m_invMassB;
   iA := m_invIA;
   iB := m_invIB;

   fixedRotation := iA + iB = 0.0;

   // Solve motor constraint.
   if m_enableMotor and (m_limitState <> e_equalLimits) and (not fixedRotation) then
   begin
      Cdotf := wB - wA - m_motorSpeed;
      impulsef := -m_motorMass * Cdotf;
      oldImpulse := m_motorImpulse;
      maxImpulse := data.step.dt * m_maxMotorTorque;
      m_motorImpulse := b2Clamp(m_motorImpulse + impulsef, -maxImpulse, maxImpulse);
      impulsef := m_motorImpulse - oldImpulse;

      wA := wA - iA * impulsef;
      wB := wB + iB * impulsef;
   end;

   // Solve limit constraint.
   if m_enableLimit and (m_limitState <> e_inactiveLimit) and (not fixedRotation) then
   begin
      {$IFDEF OP_OVERLOAD}
      Cdot1 := vB + b2Cross(wB, m_rB) - vA - b2Cross(wA, m_rA);
      {$ELSE}
      Cdot1 := Subtract(Add(vB, b2Cross(wB, m_rB)), Add(vA, b2Cross(wA, m_rA)));
      {$ENDIF}
      Cdot2 := wB - wA;

      Cdot.x := Cdot1.x;
      Cdot.y := Cdot1.y;
      Cdot.z := Cdot2;
      {$IFDEF OP_OVERLOAD}
      impulse := -m_mass.Solve33(Cdot);
      {$ELSE}
      impulse := Negative(Solve33(m_mass, Cdot));
      {$ENDIF}

      if m_limitState = e_equalLimits then
         {$IFDEF OP_OVERLOAD}
         m_impulse.AddBy(impulse)
         {$ELSE}
         AddBy(m_impulse, impulse)
         {$ENDIF}
      else if m_limitState = e_atLowerLimit then
      begin
         newImpulse := m_impulse.z + impulse.z;
         if newImpulse < 0.0 then
         begin
            {$IFDEF OP_OVERLOAD}
            rhs := m_impulse.z * MakeVector(m_mass.ez.x, m_mass.ez.y) - Cdot1;
            reduced := m_mass.Solve22(rhs);
            {$ELSE}
            rhs := Subtract(Multiply(MakeVector(m_mass.ez.x, m_mass.ez.y), m_impulse.z), Cdot1);
            reduced := Solve22(m_mass, rhs);
            {$ENDIF}

            impulse.x := reduced.x;
            impulse.y := reduced.y;
            impulse.z := -m_impulse.z;
            m_impulse.x := m_impulse.x + reduced.x;
            m_impulse.y := m_impulse.y + reduced.y;
            m_impulse.z := 0.0;
         end
         else
            {$IFDEF OP_OVERLOAD}
            m_impulse.AddBy(impulse);
            {$ELSE}
            AddBy(m_impulse, impulse);
            {$ENDIF}
      end
      else if m_limitState = e_atUpperLimit then
      begin
         newImpulse := m_impulse.z + impulse.z;
         if newImpulse > 0.0 then
         begin
            {$IFDEF OP_OVERLOAD}
            rhs := m_impulse.z * MakeVector(m_mass.ez.x, m_mass.ez.y) - Cdot1;
            reduced := m_mass.Solve22(rhs);
            {$ELSE}
            rhs := Subtract(Multiply(MakeVector(m_mass.ez.x, m_mass.ez.y), m_impulse.z), Cdot1);
            reduced := Solve22(m_mass, rhs);
            {$ENDIF}

            impulse.x := reduced.x;
            impulse.y := reduced.y;
            impulse.z := -m_impulse.z;
            m_impulse.x := m_impulse.x + reduced.x;
            m_impulse.y := m_impulse.y + reduced.y;
            m_impulse.z := 0.0;
         end
         else
            {$IFDEF OP_OVERLOAD}
            m_impulse.AddBy(impulse);
            {$ELSE}
            AddBy(m_impulse, impulse);
            {$ENDIF}
      end;

      P.x := impulse.x;
      P.y := impulse.y;

      {$IFDEF OP_OVERLOAD}
      vA.SubtractBy(mA * P);
      vB.AddBy(mB * P);
      {$ELSE}
      SubtractBy(vA, Multiply(P, mA));
      AddBy(vB, Multiply(P, mB));
      {$ENDIF}

      wA := wA - iA * (b2Cross(m_rA, P) + impulse.z);
      wB := wB + iB * (b2Cross(m_rB, P) + impulse.z);
   end
   else
   begin
     // Solve point-to-point constraint
     {$IFDEF OP_OVERLOAD}
     Cdot1 := vB + b2Cross(wB, m_rB) - vA - b2Cross(wA, m_rA);
     impulse2 := m_mass.Solve22(-Cdot1);
     {$ELSE}
     Cdot1 := Subtract(Add(vB, b2Cross(wB, m_rB)), Add(vA, b2Cross(wA, m_rA)));
     impulse2 := Solve22(m_mass, Negative(Cdot1));
     {$ENDIF}

     m_impulse.x := m_impulse.x + impulse2.x;
     m_impulse.y := m_impulse.y + impulse2.y;

     {$IFDEF OP_OVERLOAD}
     vA.SubtractBy(mA * impulse2);
     vB.AddBy(mB * impulse2);
     {$ELSE}
     SubtractBy(vA, Multiply(impulse2, mA));
     AddBy(vB, Multiply(impulse2, mB));
     {$ENDIF}

     wA := wA - iA * b2Cross(m_rA, impulse2);
     wB := wB + iB * b2Cross(m_rB, impulse2);
   end;

   data.velocities[m_indexA].v := vA;
   data.velocities[m_indexA].w := wA;
   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
end;

function Tb2RevoluteJoint.SolvePositionConstraints(const data: Tb2SolverData): Boolean;
var
   cA, cB: TVector2;
   aA, aB: PhysicsFloat;
   qA, qB: Tb2Rot;
   angularError, positionError: PhysicsFloat;
   fixedRotation: Boolean;
   angle, limitImpulse, C: PhysicsFloat;
   rA, rB, Cv: TVector2;
   mA, mB, iA, iB: PhysicsFloat;
   K: TMatrix22;
   impulse: TVector2;
begin
   cA := data.positions[m_indexA].c;
   aA := data.positions[m_indexA].a;
   cB := data.positions[m_indexB].c;
   aB := data.positions[m_indexB].a;

   angularError := 0.0;
   positionError := 0.0;

   fixedRotation := m_invIA + m_invIB = 0.0;

   // Solve angular limit constraint.
   if m_enableLimit and (m_limitState <> e_inactiveLimit) and (not fixedRotation) then
   begin
      angle := aB - aA - m_referenceAngle;
      limitImpulse := 0.0;

      if m_limitState = e_equalLimits then
      begin
         // Prevent large angular corrections
         C := b2Clamp(angle - m_lowerAngle, -b2_maxAngularCorrection, b2_maxAngularCorrection);
         limitImpulse := -m_motorMass * C;
         angularError := Abs(C);
      end
      else if m_limitState = e_atLowerLimit then
      begin
         C := angle - m_lowerAngle;
         angularError := -C;

         // Prevent large angular corrections and allow some slop.
         C := b2Clamp(C + b2_angularSlop, -b2_maxAngularCorrection, 0.0);
         limitImpulse := -m_motorMass * C;
      end
      else if m_limitState = e_atUpperLimit then
      begin
         C := angle - m_upperAngle;
         angularError := C;

         // Prevent large angular corrections and allow some slop.
         C := b2Clamp(C - b2_angularSlop, 0.0, b2_maxAngularCorrection);
         limitImpulse := -m_motorMass * C;
      end;

      aA := aA - m_invIA * limitImpulse;
      aB := aB + m_invIB * limitImpulse;
   end;

   // Solve point-to-point constraint.
   {$IFDEF OP_OVERLOAD}
   qA.SetAngle(aA);
   qB.SetAngle(aB);
   rA := b2Mul(qA, m_localAnchorA - m_localCenterA);
   rB := b2Mul(qB, m_localAnchorB - m_localCenterB);

   Cv := cB + rB - cA - rA;
   positionError := Cv.Length;
   {$ELSE}
   SetAngle(qA, aA);
   SetAngle(qB, aB);
   rA := b2Mul(qA, Subtract(m_localAnchorA, m_localCenterA));
   rB := b2Mul(qB, Subtract(m_localAnchorB, m_localCenterB));

   Cv := Subtract(Add(cB, rB), Add(cA, rA));
   positionError := LengthVec(Cv);
   {$ENDIF}

   mA := m_invMassA;
   mB := m_invMassB;
   iA := m_invIA;
   iB := m_invIB;

   K.ex.x := mA + mB + iA * rA.y * rA.y + iB * rB.y * rB.y;
   K.ex.y := -iA * rA.x * rA.y - iB * rB.x * rB.y;
   K.ey.x := K.ex.y;
   K.ey.y := mA + mB + iA * rA.x * rA.x + iB * rB.x * rB.x;

   {$IFDEF OP_OVERLOAD}
   impulse := -K.Solve(Cv);
   {$ELSE}
   impulse := Negative(Solve(K, Cv));
   {$ENDIF}

   {$IFDEF OP_OVERLOAD}
   cA.SubtractBy(mA * impulse);
   cB.AddBy(mB * impulse);
   {$ELSE}
   SubtractBy(cA, Multiply(impulse, mA));
   AddBy(cB, Multiply(impulse, mB));
   {$ENDIF}
   aA := aA - iA * b2Cross(rA, impulse);
   aB := aB + iB * b2Cross(rB, impulse);

   data.positions[m_indexA].c := cA;
   data.positions[m_indexA].a := aA;
   data.positions[m_indexB].c := cB;
   data.positions[m_indexB].a := aB;

   Result := (positionError <= b2_linearSlop) and (angularError <= b2_angularSlop);
end;

function Tb2RevoluteJoint.GetJointAngle: PhysicsFloat;
begin
	 Result := m_bodyB.m_sweep.a - m_bodyA.m_sweep.a - m_referenceAngle;
end;

function Tb2RevoluteJoint.GetJointSpeed: PhysicsFloat;
begin
	 Result := m_bodyB.m_angularVelocity - m_bodyA.m_angularVelocity;
end;

function Tb2RevoluteJoint.GetMotorTorque(inv_dt: PhysicsFloat): PhysicsFloat;
begin
   Result := inv_dt * m_motorImpulse;
end;

procedure Tb2RevoluteJoint.EnableLimit(flag: Boolean);
begin
   if flag <> m_enableLimit then
   begin
 	     m_bodyA.SetAwake(True);
	     m_bodyB.SetAwake(True);
	     m_enableLimit := flag;
       m_impulse.z := 0.0;
   end;
end;

procedure Tb2RevoluteJoint.SetLimits(lower, upper: PhysicsFloat);
begin
   //b2Assert(lower <= upper);
   if (lower <> m_lowerAngle) and (upper <> m_upperAngle) then
   begin
   	  m_bodyA.SetAwake(True);
      m_bodyB.SetAwake(True);
      m_lowerAngle := lower;
      m_upperAngle := upper;
      m_impulse.z := 0.0;
   end;
end;

procedure Tb2RevoluteJoint.EnableMotor(flag: Boolean);
begin
	 m_bodyA.SetAwake(True);
	 m_bodyB.SetAwake(True);
	 m_enableMotor := flag;
end;

procedure Tb2RevoluteJoint.SetMotorSpeed(speed: PhysicsFloat);
begin
	 m_bodyA.SetAwake(True);
	 m_bodyB.SetAwake(True);
	 m_motorSpeed := speed;
end;

procedure Tb2RevoluteJoint.SetMaxMotorTorque(torque: PhysicsFloat);
begin
	 m_bodyA.SetAwake(True);
	 m_bodyB.SetAwake(True);
   m_maxMotorTorque := torque;
end;

{ Tb2GearJointDef }

// Gear Joint:
// C0 = (coordinate1 + ratio * coordinate2)_initial
// C = (coordinate1 + ratio * coordinate2) - C0 = 0
// J = [J1 ratio * J2]
// K = J * invM * JT
//   = J1 * invM1 * J1T + ratio * ratio * J2 * invM2 * J2T
//
// Revolute:
// coordinate = rotation
// Cdot = angularVelocity
// J = [0 0 1]
// K = J * invM * JT = invI
//
// Prismatic:
// coordinate = dot(p - pg, ug)
// Cdot = dot(v + cross(w, r), ug)
// J = [ug cross(r, ug)]
// K = J * invM * JT = invMass + invI * cross(r, ug)^2

constructor Tb2GearJointDef.Create;
begin
   inherited;
	 jointType := e_gearJoint;
	 joint1 := nil;
	 joint2 := nil;
	 ratio := 1.0;
end;

{ Tb2GearJoint }

constructor Tb2GearJoint.Create(def: Tb2GearJointDef);
var
   coordinateA, coordinateB: PhysicsFloat;
   xfA, xfB, xfC, xfD: Tb2Transform;
   aA, aB, aC, aD: PhysicsFloat;
   revolute: Tb2RevoluteJoint;
   prismatic: Tb2PrismaticJoint;
   pA, pB, pC, pD: TVector2;
begin
   inherited Create(def);

   m_joint1 := def.joint1;
   m_joint2 := def.joint2;

   m_typeA := m_joint1.m_type;
   m_typeB := m_joint2.m_type;

   //b2Assert(m_typeA == e_revoluteJoint || m_typeA == e_prismaticJoint);
   //b2Assert(m_typeB == e_revoluteJoint || m_typeB == e_prismaticJoint);

   // TODO_ERIN there might be some problem with the joint edges in b2Joint.

   m_bodyC := m_joint1.m_bodyA;
   m_bodyA := m_joint1.m_bodyB;

   // Get geometry of joint1
   xfA := m_bodyA.m_xf;
   aA := m_bodyA.m_sweep.a;
   xfC := m_bodyC.m_xf;
   aC := m_bodyC.m_sweep.a;

   if m_typeA = e_revoluteJoint then
   begin
      revolute := Tb2RevoluteJoint(def.joint1);
      m_localAnchorC := revolute.m_localAnchorA;
      m_localAnchorA := revolute.m_localAnchorB;
      m_referenceAngleA := revolute.m_referenceAngle;
      m_localAxisC := b2Vec2_Zero;
      coordinateA := aA - aC - m_referenceAngleA;
   end
   else
   begin
      prismatic := Tb2PrismaticJoint(def.joint1);
      m_localAnchorC := prismatic.m_localAnchorA;
      m_localAnchorA := prismatic.m_localAnchorB;
      m_referenceAngleA := prismatic.m_referenceAngle;
      m_localAxisC := prismatic.m_localXAxisA;

      pC := m_localAnchorC;
      {$IFDEF OP_OVERLOAD}
      pA := b2MulT(xfC.q, b2Mul(xfA.q, m_localAnchorA) + (xfA.p - xfC.p));
      coordinateA := b2Dot(pA - pC, m_localAxisC);
      {$ELSE}
      pA := b2MulT(xfC.q, Add(b2Mul(xfA.q, m_localAnchorA), Subtract(xfA.p, xfC.p)));
      coordinateA := b2Dot(Subtract(pA, pC), m_localAxisC);
      {$ENDIF}
   end;

   m_bodyD := m_joint2.m_bodyA;
   m_bodyB := m_joint2.m_bodyB;

   // Get geometry of joint2
   xfB := m_bodyB.m_xf;
   aB := m_bodyB.m_sweep.a;
   xfD := m_bodyD.m_xf;
   aD := m_bodyD.m_sweep.a;

   if m_typeB = e_revoluteJoint then
   begin
      revolute := Tb2RevoluteJoint(def.joint2);
      m_localAnchorD := revolute.m_localAnchorA;
      m_localAnchorB := revolute.m_localAnchorB;
      m_referenceAngleB := revolute.m_referenceAngle;
      m_localAxisD := b2Vec2_Zero;
      coordinateB := aB - aD - m_referenceAngleB;
   end
   else
   begin
      prismatic := Tb2PrismaticJoint(def.joint2);
      m_localAnchorD := prismatic.m_localAnchorA;
      m_localAnchorB := prismatic.m_localAnchorB;
      m_referenceAngleB := prismatic.m_referenceAngle;
      m_localAxisD := prismatic.m_localXAxisA;

      pD := m_localAnchorD;
      {$IFDEF OP_OVERLOAD}
      pB := b2MulT(xfD.q, b2Mul(xfB.q, m_localAnchorB) + (xfB.p - xfD.p));
      coordinateB := b2Dot(pB - pD, m_localAxisD);
      {$ELSE}
      pB := b2MulT(xfD.q, Add(b2Mul(xfB.q, m_localAnchorB), Subtract(xfB.p, xfD.p)));
      coordinateB := b2Dot(Subtract(pB, pD), m_localAxisD);
      {$ENDIF}
   end;

   m_ratio := def.ratio;
   m_constant := coordinateA + m_ratio * coordinateB;
   m_impulse := 0.0;
end;

{$IFDEF ENABLE_DUMP}
procedure Tb2GearJoint.Dump;
var
   indexA, indexB: Int32;
   index1, index2: Int32;
begin
   indexA := m_bodyA.m_islandIndex;
   indexB := m_bodyB.m_islandIndex;

   index1 := m_joint1.m_index;
   index2 := m_joint2.m_index;

   b2DumpMethod(1, 'begin', []);
   b2DumpMethod(2, 'gear_jd := Tb2GearJointDef.Create;', []);
   b2DumpMethod(2, 'gear_jd.bodyA := bodies[%d];', [indexA]);
   b2DumpMethod(2, 'gear_jd.bodyB := bodies[%d];', [indexB]);
   b2DumpMethod(2, 'gear_jd.collideConnected := %s;', [b2BoolToStr(m_collideConnected)]);
   b2DumpMethod(2, 'gear_jd.joint1 := joints[%d];', [index1]);
   b2DumpMethod(2, 'gear_jd.joint2 := joints[%d];', [index2]);
   b2DumpMethod(2, 'gear_jd.ratio := %s;', [b2FloatToStr(m_ratio)]);
   b2DumpMethod(2, 'joints[%d] := m_world->CreateJoint(gear_jd);', [m_index]);
   b2DumpMethod(1, 'end;', []);
end;
{$ENDIF}

function Tb2GearJoint.GetAnchorA: TVector2;
begin
   Result := m_bodyA.GetWorldPoint(m_localAnchorA);
end;

function Tb2GearJoint.GetAnchorB: TVector2;
begin
   Result := m_bodyB.GetWorldPoint(m_localAnchorB);
end;

function Tb2GearJoint.GetReactionForce(inv_dt: PhysicsFloat): TVector2;
begin
   {$IFDEF OP_OVERLOAD}
   Result := (inv_dt * m_impulse) * m_JvAC;
   {$ELSE}
   Result := Multiply(m_JvAC, inv_dt * m_impulse);
   {$ENDIF}
end;

function Tb2GearJoint.GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat;
begin
   Result := (inv_dt * m_impulse) * m_JwA;
end;

procedure Tb2GearJoint.InitVelocityConstraints(const data: Tb2SolverData);
var
   vA, vB, vC, vD: TVector2;
   aA, wA, aB, wB, aC, wC, aD, wD: PhysicsFloat;
   qA, qB, qC, qD: Tb2Rot;
   u, rC, rA, rD, rB: TVector2;
begin
   m_indexA := m_bodyA.m_islandIndex;
   m_indexB := m_bodyB.m_islandIndex;
   m_indexC := m_bodyC.m_islandIndex;
   m_indexD := m_bodyD.m_islandIndex;
   m_lcA := m_bodyA.m_sweep.localCenter;
   m_lcB := m_bodyB.m_sweep.localCenter;
   m_lcC := m_bodyC.m_sweep.localCenter;
   m_lcD := m_bodyD.m_sweep.localCenter;
   m_mA := m_bodyA.m_invMass;
   m_mB := m_bodyB.m_invMass;
   m_mC := m_bodyC.m_invMass;
   m_mD := m_bodyD.m_invMass;
   m_iA := m_bodyA.m_invI;
   m_iB := m_bodyB.m_invI;
   m_iC := m_bodyC.m_invI;
   m_iD := m_bodyD.m_invI;

   aA := data.positions[m_indexA].a;
   vA := data.velocities[m_indexA].v;
   wA := data.velocities[m_indexA].w;

   aB := data.positions[m_indexB].a;
   vB := data.velocities[m_indexB].v;
   wB := data.velocities[m_indexB].w;

   aC := data.positions[m_indexC].a;
   vC := data.velocities[m_indexC].v;
   wC := data.velocities[m_indexC].w;

   aD := data.positions[m_indexD].a;
   vD := data.velocities[m_indexD].v;
   wD := data.velocities[m_indexD].w;

   {$IFDEF OP_OVERLOAD}
   qA.SetAngle(aA);
   qB.SetAngle(aB);
   qC.SetAngle(aC);
   qD.SetAngle(aD);
   {$ELSE}
   SetAngle(qA, aA);
   SetAngle(qB, aB);
   SetAngle(qC, aC);
   SetAngle(qD, aD);
   {$ENDIF}

   m_mass := 0.0;

   if m_typeA = e_revoluteJoint then
   begin
      m_JvAC := b2Vec2_Zero;
      m_JwA := 1.0;
      m_JwC := 1.0;
      m_mass := m_mass + m_iA + m_iC;
   end
   else
   begin
      u := b2Mul(qC, m_localAxisC);
      {$IFDEF OP_OVERLOAD}
      rC := b2Mul(qC, m_localAnchorC - m_lcC);
      rA := b2Mul(qA, m_localAnchorA - m_lcA);
      {$ELSE}
      rC := b2Mul(qC, Subtract(m_localAnchorC, m_lcC));
      rA := b2Mul(qA, Subtract(m_localAnchorA, m_lcA));
      {$ENDIF}
      m_JvAC := u;
      m_JwC := b2Cross(rC, u);
      m_JwA := b2Cross(rA, u);
      m_mass := m_mass + m_mC + m_mA + m_iC * m_JwC * m_JwC + m_iA * m_JwA * m_JwA;
   end;

   if m_typeB = e_revoluteJoint then
   begin
      m_JvBD := b2Vec2_Zero;
      m_JwB := m_ratio;
      m_JwD := m_ratio;
      m_mass := m_mass + m_ratio * m_ratio * (m_iB + m_iD);
   end
   else
   begin
      u := b2Mul(qD, m_localAxisD);
      {$IFDEF OP_OVERLOAD}
      rD := b2Mul(qD, m_localAnchorD - m_lcD);
      rB := b2Mul(qB, m_localAnchorB - m_lcB);
      m_JvBD := m_ratio * u;
      {$ELSE}
      rD := b2Mul(qD, Subtract(m_localAnchorD, m_lcD));
      rB := b2Mul(qB, Subtract(m_localAnchorB, m_lcB));
      m_JvBD := Multiply(u, m_ratio);
      {$ENDIF}
      m_JwD := m_ratio * b2Cross(rD, u);
      m_JwB := m_ratio * b2Cross(rB, u);
      m_mass := m_mass + m_ratio * m_ratio * (m_mD + m_mB) + m_iD * m_JwD * m_JwD + m_iB * m_JwB * m_JwB;
   end;

   // Compute effective mass.
   if m_mass > 0.0 then
      m_mass := 1.0 / m_mass
   else
      m_mass := 0.0;

   if data.step.warmStarting then
   begin
      {$IFDEF OP_OVERLOAD}
      vA.AddBy((m_mA * m_impulse) * m_JvAC);
      vB.AddBy((m_mB * m_impulse) * m_JvBD);
      vC.SubtractBy((m_mC * m_impulse) * m_JvAC);
      vD.SubtractBy((m_mD * m_impulse) * m_JvBD);
      {$ELSE}
      AddBy(vA, Multiply(m_JvAC, m_mA * m_impulse));
      AddBy(vB, Multiply(m_JvBD, m_mB * m_impulse));
      SubtractBy(vC, Multiply(m_JvAC, m_mC * m_impulse));
      SubtractBy(vD, Multiply(m_JvBD, m_mD * m_impulse));
      {$ENDIF}

      wA := wA + m_iA * m_impulse * m_JwA;
      wB := wB + m_iB * m_impulse * m_JwB;
      wC := wC - m_iC * m_impulse * m_JwC;
      wD := wD - m_iD * m_impulse * m_JwD;
   end
   else
      m_impulse := 0.0;

   data.velocities[m_indexA].v := vA;
   data.velocities[m_indexA].w := wA;
   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
   data.velocities[m_indexC].v := vC;
   data.velocities[m_indexC].w := wC;
   data.velocities[m_indexD].v := vD;
   data.velocities[m_indexD].w := wD;
end;

procedure Tb2GearJoint.SolveVelocityConstraints(const data: Tb2SolverData);
var
   vA, vB, vC, vD: TVector2;
   wA, wB, wC, wD: PhysicsFloat;
   Cdot: PhysicsFloat;
   impulse: PhysicsFloat;
begin
   vA := data.velocities[m_indexA].v;
   wA := data.velocities[m_indexA].w;
   vB := data.velocities[m_indexB].v;
   wB := data.velocities[m_indexB].w;
   vC := data.velocities[m_indexC].v;
   wC := data.velocities[m_indexC].w;
   vD := data.velocities[m_indexD].v;
   wD := data.velocities[m_indexD].w;

   {$IFDEF OP_OVERLOAD}
   Cdot := b2Dot(m_JvAC, vA - vC) + b2Dot(m_JvBD, vB - vD);
   {$ELSE}
   Cdot := b2Dot(m_JvAC, Subtract(vA, vC)) + b2Dot(m_JvBD, Subtract(vB, vD));
   {$ENDIF}
   Cdot := Cdot + (m_JwA * wA - m_JwC * wC) + (m_JwB * wB - m_JwD * wD);

   impulse := -m_mass * Cdot;
   m_impulse := m_impulse + impulse;

   {$IFDEF OP_OVERLOAD}
   vA.AddBy((m_mA * impulse) * m_JvAC);
   vB.AddBy((m_mB * impulse) * m_JvBD);
   vC.SubtractBy((m_mC * impulse) * m_JvAC);
   vD.SubtractBy((m_mD * impulse) * m_JvBD);
   {$ELSE}
   AddBy(vA, Multiply(m_JvAC, m_mA * impulse));
   AddBy(vB, Multiply(m_JvBD, m_mB * impulse));
   SubtractBy(vC, Multiply(m_JvAC, m_mC * impulse));
   SubtractBy(vD, Multiply(m_JvBD, m_mD * impulse));
   {$ENDIF}

   wA := wA + m_iA * impulse * m_JwA;
   wB := wB + m_iB * impulse * m_JwB;
   wC := wC - m_iC * impulse * m_JwC;
   wD := wD - m_iD * impulse * m_JwD;

   data.velocities[m_indexA].v := vA;
   data.velocities[m_indexA].w := wA;
   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
   data.velocities[m_indexC].v := vC;
   data.velocities[m_indexC].w := wC;
   data.velocities[m_indexD].v := vD;
   data.velocities[m_indexD].w := wD;
end;

function Tb2GearJoint.SolvePositionConstraints(const data: Tb2SolverData): Boolean;
var
   cA, cB, cC, cD: TVector2;
   aA, aB, aC, aD: PhysicsFloat;
   qA, qB, qC, qD: Tb2Rot;
   linearError, coordinateA, coordinateB: PhysicsFloat;
   JvAC, JvBD: TVector2;
   JwA, JwB, JwC, JwD: PhysicsFloat;
   u, rA, rB, rC, rD, pA, pB, pC, pD: TVector2;
   mass, C, impulse: PhysicsFloat;
begin
   cA := data.positions[m_indexA].c;
   aA := data.positions[m_indexA].a;
   cB := data.positions[m_indexB].c;
   aB := data.positions[m_indexB].a;
   cC := data.positions[m_indexC].c;
   aC := data.positions[m_indexC].a;
   cD := data.positions[m_indexD].c;
   aD := data.positions[m_indexD].a;

   {$IFDEF OP_OVERLOAD}
   qA.SetAngle(aA);
   qB.SetAngle(aB);
   qC.SetAngle(aC);
   qD.SetAngle(aD);
   {$ELSE}
   SetAngle(qA, aA);
   SetAngle(qB, aB);
   SetAngle(qC, aC);
   SetAngle(qD, aD);
   {$ENDIF}

   linearError := 0.0;
   mass := 0.0;

   if m_typeA = e_revoluteJoint then
   begin
      JvAC := b2Vec2_Zero;
      JwA := 1.0;
      JwC := 1.0;
      mass := mass + m_iA + m_iC;
      coordinateA := aA - aC - m_referenceAngleA;
   end
   else
   begin
      u := b2Mul(qC, m_localAxisC);
      {$IFDEF OP_OVERLOAD}
      rC := b2Mul(qC, m_localAnchorC - m_lcC);
      rA := b2Mul(qA, m_localAnchorA - m_lcA);
      {$ELSE}
      rC := b2Mul(qC, Subtract(m_localAnchorC, m_lcC));
      rA := b2Mul(qA, Subtract(m_localAnchorA, m_lcA));
      {$ENDIF}
      JvAC := u;
      JwC := b2Cross(rC, u);
      JwA := b2Cross(rA, u);
      mass := mass + m_mC + m_mA + m_iC * JwC * JwC + m_iA * JwA * JwA;

      {$IFDEF OP_OVERLOAD}
      pC := m_localAnchorC - m_lcC;
      pA := b2MulT(qC, rA + (cA - cC));
      coordinateA := b2Dot(pA - pC, m_localAxisC);
      {$ELSE}
      pC := Subtract(m_localAnchorC, m_lcC);
      pA := b2MulT(qC, Add(rA, Subtract(cA, cC)));
      coordinateA := b2Dot(Subtract(pA, pC), m_localAxisC);
      {$ENDIF}
   end;

   if m_typeB = e_revoluteJoint then
   begin
      JvBD := b2Vec2_Zero;
      JwB := m_ratio;
      JwD := m_ratio;
      mass := mass + m_ratio * m_ratio * (m_iB + m_iD);
      coordinateB := aB - aD - m_referenceAngleB;
   end
   else
   begin
      u := b2Mul(qD, m_localAxisD);
      {$IFDEF OP_OVERLOAD}
      rD := b2Mul(qD, m_localAnchorD - m_lcD);
      rB := b2Mul(qB, m_localAnchorB - m_lcB);
      JvBD := m_ratio * u;
      {$ELSE}
      rD := b2Mul(qD, Subtract(m_localAnchorD, m_lcD));
      rB := b2Mul(qB, Subtract(m_localAnchorB, m_lcB));
      JvBD := Multiply(u, m_ratio);
      {$ENDIF}
      JwD := m_ratio * b2Cross(rD, u);
      JwB := m_ratio * b2Cross(rB, u);
      mass := mass + m_ratio * m_ratio * (m_mD + m_mB) + m_iD * JwD * JwD + m_iB * JwB * JwB;

      {$IFDEF OP_OVERLOAD}
      pD := m_localAnchorD - m_lcD;
      pB := b2MulT(qD, rB + (cB - cD));
      coordinateB := b2Dot(pB - pD, m_localAxisD);
      {$ELSE}
      pD := Subtract(m_localAnchorD, m_lcD);
      pB := b2MulT(qD, Add(rB, Subtract(cB, cD)));
      coordinateB := b2Dot(Subtract(pB, pD), m_localAxisD);
      {$ENDIF}
   end;

   C := (coordinateA + m_ratio * coordinateB) - m_constant;

   impulse := 0.0;
   if mass > 0.0 then
      impulse := -C / mass;

   {$IFDEF OP_OVERLOAD}
   cA.AddBy(m_mA * impulse * JvAC);
   cB.AddBy(m_mB * impulse * JvBD);
   cC.SubtractBy(m_mC * impulse * JvAC);
   cD.SubtractBy(m_mD * impulse * JvBD);
   {$ELSE}
   AddBy(cA, Multiply(JvAC, m_mA * impulse));
   AddBy(cB, Multiply(JvBD, m_mB * impulse));
   SubtractBy(cC, Multiply(JvAC, m_mC * impulse));
   SubtractBy(cD, Multiply(JvBD, m_mD * impulse));
   {$ENDIF}

   aA := aA + m_iA * impulse * JwA;
   aB := aB + m_iB * impulse * JwB;
   aC := aC - m_iC * impulse * JwC;
   aD := aD - m_iD * impulse * JwD;

   data.positions[m_indexA].c := cA;
   data.positions[m_indexA].a := aA;
   data.positions[m_indexB].c := cB;
   data.positions[m_indexB].a := aB;
   data.positions[m_indexC].c := cC;
   data.positions[m_indexC].a := aC;
   data.positions[m_indexD].c := cD;
   data.positions[m_indexD].a := aD;

   // TODO_ERIN not implemented
   Result := linearError < b2_linearSlop;
end;

{ Tb2FrictionJointDef }

// Point-to-point constraint
// Cdot = v2 - v1
//      = v2 + cross(w2, r2) - v1 - cross(w1, r1)
// J = [-I -r1_skew I r2_skew ]
// Identity used:
// w k % (rx i + ry j) = w * (-ry i + rx j)

// Angle constraint
// Cdot = w2 - w1
// J = [0 0 -1 0 0 1]
// K = invI1 + invI2

constructor Tb2FrictionJointDef.Create;
begin
   inherited;
   jointType := e_frictionJoint;
   localAnchorA := b2Vec2_Zero;
   localAnchorB := b2Vec2_Zero;
   maxForce := 0.0;
   maxTorque := 0.0;
end;

procedure Tb2FrictionJointDef.Initialize(bodyA, bodyB: Tb2Body; const anchor: TVector2);
begin
   Self.bodyA := bodyA;
   Self.bodyB := bodyB;
   localAnchorA := bodyA.GetLocalPoint(anchor);
   localAnchorB := bodyB.GetLocalPoint(anchor);
end;

{ Tb2FrictionJoint }

constructor Tb2FrictionJoint.Create(def: Tb2FrictionJointDef);
begin
   inherited Create(def);
   m_localAnchorA := def.localAnchorA;
   m_localAnchorB := def.localAnchorB;

   m_linearImpulse := b2Vec2_Zero;
   m_angularImpulse := 0.0;

   m_maxForce := def.maxForce;
   m_maxTorque := def.maxTorque;
end;

{$IFDEF ENABLE_DUMP}
procedure Tb2FrictionJoint.Dump;
var
   indexA: Int32;
   indexB: Int32;
begin
   indexA := m_bodyA.m_islandIndex;
   indexB := m_bodyB.m_islandIndex;

   b2DumpMethod(1, 'begin', []);
   b2DumpMethod(2, 'friction_jd := Tb2FrictionJointDef.Create;', []);
   b2DumpMethod(2, 'friction_jd.bodyA := bodies[%d];', [indexA]);
   b2DumpMethod(2, 'friction_jd.bodyB := bodies[%d];', [indexB]);
   b2DumpMethod(2, 'friction_jd.collideConnected := %s;', [b2BoolToStr(m_collideConnected)]);
   b2DumpMethod(2, 'friction_jd.localAnchorA := MakeVector(%s, %s);', [b2FloatToStr(m_localAnchorA.x), b2FloatToStr(m_localAnchorA.y)]);
   b2DumpMethod(2, 'friction_jd.localAnchorB := MakeVector(%s, %s);', [b2FloatToStr(m_localAnchorB.x), b2FloatToStr(m_localAnchorB.y)]);
   b2DumpMethod(2, 'friction_jd.maxForce := %s;', [b2FloatToStr(m_maxForce)]);
   b2DumpMethod(2, 'friction_jd.maxTorque := %s;', [b2FloatToStr(m_maxTorque)]);
   b2DumpMethod(2, 'joints[%d] := m_world.CreateJoint(friction_jd);', [m_index]);
   b2DumpMethod(1, 'end;', []);
end;
{$ENDIF}

function Tb2FrictionJoint.GetAnchorA: TVector2;
begin
   Result := m_bodyA.GetWorldPoint(m_localAnchorA);
end;

function Tb2FrictionJoint.GetAnchorB: TVector2;
begin
   Result  := m_bodyB.GetWorldPoint(m_localAnchorB);
end;

function Tb2FrictionJoint.GetReactionForce(inv_dt: PhysicsFloat): TVector2;
begin
   {$IFDEF OP_OVERLOAD}
   Result := inv_dt * m_linearImpulse;
   {$ELSE}
   Result := Multiply(m_linearImpulse, inv_dt);
   {$ENDIF}
end;

function Tb2FrictionJoint.GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat;
begin
   Result := inv_dt * m_angularImpulse;
end;

procedure Tb2FrictionJoint.InitVelocityConstraints(const data: Tb2SolverData);
var
   vA, vB: TVector2;
   aA, wA, aB, wB: PhysicsFloat;
   qA, qB: Tb2Rot;
   mA, mB, iA, iB: PhysicsFloat;
   K: TMatrix22;
begin
   m_indexA := m_bodyA.m_islandIndex;
   m_indexB := m_bodyB.m_islandIndex;
   m_localCenterA := m_bodyA.m_sweep.localCenter;
   m_localCenterB := m_bodyB.m_sweep.localCenter;
   m_invMassA := m_bodyA.m_invMass;
   m_invMassB := m_bodyB.m_invMass;
   m_invIA := m_bodyA.m_invI;
   m_invIB := m_bodyB.m_invI;

   aA := data.positions[m_indexA].a;
   vA := data.velocities[m_indexA].v;
   wA := data.velocities[m_indexA].w;

   aB := data.positions[m_indexB].a;
   vB := data.velocities[m_indexB].v;
   wB := data.velocities[m_indexB].w;

   // Compute the effective mass matrix.
   {$IFDEF OP_OVERLOAD}
   qA.SetAngle(aA);
   qB.SetAngle(aB);
	 m_rA := b2Mul(qA, m_localAnchorA - m_localCenterA);
	 m_rB := b2Mul(qB, m_localAnchorB - m_localCenterB);
   {$ELSE}
   SetAngle(qA, aA);
   SetAngle(qB, aB);
   m_rA := b2Mul(qA, Subtract(m_localAnchorA, m_localCenterA));
   m_rB := b2Mul(qB, Subtract(m_localAnchorB, m_localCenterB));
   {$ENDIF}

   // J := [-I -r1_skew I r2_skew]
   //     [ 0       -1 0       1]
   // r_skew := [-ry; rx]

   // Matlab
   // K := [ mA+r1y^2*iA+mB+r2y^2*iB,  -r1y*iA*r1x-r2y*iB*r2x,          -r1y*iA-r2y*iB]
   //     [  -r1y*iA*r1x-r2y*iB*r2x, mA+r1x^2*iA+mB+r2x^2*iB,           r1x*iA+r2x*iB]
   //     [          -r1y*iA-r2y*iB,           r1x*iA+r2x*iB,                   iA+iB]

   mA := m_invMassA;
   mB := m_invMassB;
   iA := m_invIA;
   iB := m_invIB;

   K.ex.x := mA + mB + iA * m_rA.y * m_rA.y + iB * m_rB.y * m_rB.y;
   K.ex.y := -iA * m_rA.x * m_rA.y - iB * m_rB.x * m_rB.y;
   K.ey.x := K.ex.y;
   K.ey.y := mA + mB + iA * m_rA.x * m_rA.x + iB * m_rB.x * m_rB.x;

   {$IFDEF OP_OVERLOAD}
   m_linearMass := K.GetInverse;
   {$ELSE}
   m_linearMass := GetInverse(K);
   {$ENDIF}

   m_angularMass := iA + iB;
   if m_angularMass > 0.0 then
      m_angularMass := 1.0 / m_angularMass;

   if data.step.warmStarting then
   begin
      // Scale impulses to support a variable time step.
      {$IFDEF OP_OVERLOAD}
      m_linearImpulse.MultiplyBy(data.step.dtRatio);
      {$ELSE}
      MultiplyBy(m_linearImpulse, data.step.dtRatio);
      {$ENDIF}
      m_angularImpulse := m_angularImpulse * data.step.dtRatio;

      {$IFDEF OP_OVERLOAD}
      vA.SubtractBy(mA * m_linearImpulse);
      vB.AddBy(mB * m_linearImpulse);
      {$ELSE}
      SubtractBy(vA, Multiply(m_linearImpulse, mA));
      AddBy(vB, Multiply(m_linearImpulse, mB));
      {$ENDIF}

      wA := wA - iA * (b2Cross(m_rA, m_linearImpulse) + m_angularImpulse);
      wB := wB + iB * (b2Cross(m_rB, m_linearImpulse) + m_angularImpulse);
   end
   else
   begin
      m_linearImpulse := b2Vec2_Zero;
      m_angularImpulse := 0.0;
   end;

   data.velocities[m_indexA].v := vA;
   data.velocities[m_indexA].w := wA;
   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
end;

procedure Tb2FrictionJoint.SolveVelocityConstraints(const data: Tb2SolverData);
var
   vA, vB, Cdot, impulse, oldImpulse: TVector2;
   mA, mB, iA, iB, wA, wB, h, fCdot, impulsef, oldImpulsef, maxImpulse: PhysicsFloat;
begin
   vA := data.velocities[m_indexA].v;
   wA := data.velocities[m_indexA].w;
   vB := data.velocities[m_indexB].v;
   wB := data.velocities[m_indexB].w;

   mA := m_invMassA;
   mB := m_invMassB;
   iA := m_invIA;
   iB := m_invIB;

   h := data.step.dt;

   // Solve angular friction
   begin
      fCdot := wB - wA;
      impulsef := -m_angularMass * fCdot;

      oldImpulsef := m_angularImpulse;
      maxImpulse := h * m_maxTorque;
      m_angularImpulse := b2Clamp(m_angularImpulse + impulsef, -maxImpulse, maxImpulse);
      impulsef := m_angularImpulse - oldImpulsef;

      wA := wA - iA * impulsef;
      wB := wB + iB * impulsef;
   end;

   // Solve linear friction
   begin
      {$IFDEF OP_OVERLOAD}
      Cdot := vB + b2Cross(wB, m_rB) - vA - b2Cross(wA, m_rA);
      impulse := -b2Mul(m_linearMass, Cdot);
      oldImpulse := m_linearImpulse;
      m_linearImpulse.AddBy(impulse);
      maxImpulse := h * m_maxForce;

      if m_linearImpulse.SqrLength > maxImpulse * maxImpulse then
      begin
         m_linearImpulse.Normalize;
         m_linearImpulse.MultiplyBy(maxImpulse);
      end;

      impulse := m_linearImpulse - oldImpulse;
      vA.SubtractBy(mA * impulse);
      vB.AddBy(mB * impulse);
      {$ELSE}
      Cdot := Subtract(Add(vB, b2Cross(wB, m_rB)), Add(vA, b2Cross(wA, m_rA)));
      impulse := Negative(b2Mul(m_linearMass, Cdot));
      oldImpulse := m_linearImpulse;
      AddBy(m_linearImpulse, impulse);
      maxImpulse := h * m_maxForce;

      if SqrLength(m_linearImpulse) > maxImpulse * maxImpulse then
      begin
         Normalize(m_linearImpulse);
         MultiplyBy(m_linearImpulse, maxImpulse);
      end;

      impulse := Subtract(m_linearImpulse, oldImpulse);
      SubtractBy(vA, Multiply(impulse, mA));
      AddBy(vB, Multiply(impulse, mB));
      {$ENDIF}

      wA := wA - iA * b2Cross(m_rA, impulse);
      wB := wB + iB * b2Cross(m_rB, impulse);
   end;

   data.velocities[m_indexA].v := vA;
   data.velocities[m_indexA].w := wA;
   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
end;

function Tb2FrictionJoint.SolvePositionConstraints(const data: Tb2SolverData): Boolean;
begin
	 //B2_NOT_USED(data);
   Result := True;
end;

{ Tb2WheelJointDef }

// Linear constraint (point-to-line)
// d = pB - pA = xB + rB - xA - rA
// C = dot(ay, d)
// Cdot = dot(d, cross(wA, ay)) + dot(ay, vB + cross(wB, rB) - vA - cross(wA, rA))
//      = -dot(ay, vA) - dot(cross(d + rA, ay), wA) + dot(ay, vB) + dot(cross(rB, ay), vB)
// J = [-ay, -cross(d + rA, ay), ay, cross(rB, ay)]

// Spring linear constraint
// C = dot(ax, d)
// Cdot = = -dot(ax, vA) - dot(cross(d + rA, ax), wA) + dot(ax, vB) + dot(cross(rB, ax), vB)
// J = [-ax -cross(d+rA, ax) ax cross(rB, ax)]

// Motor rotational constraint
// Cdot = wB - wA
// J = [0 0 -1 0 0 1]

constructor Tb2WheelJointDef.Create;
begin
   inherited;
   jointType := e_wheelJoint;
   localAnchorA := b2Vec2_Zero;
   localAnchorB := b2Vec2_Zero;
   SetValue(localAxisA, 1.0, 0.0);

   enableMotor := False;
   maxMotorTorque := 0.0;
   motorSpeed := 0.0;
   frequencyHz := 2.0;
   dampingRatio := 0.7;
end;

procedure Tb2WheelJointDef.Initialize(bodyA, bodyB: Tb2Body; const anchor, axis: TVector2);
begin
   Self.bodyA := bodyA;
   Self.bodyB := bodyB;
   localAnchorA := bodyA.GetLocalPoint(anchor);
   localAnchorB := bodyB.GetLocalPoint(anchor);
   localAxisA := bodyA.GetLocalVector(axis);
end;

{ Tb2WheelJoint }

constructor Tb2WheelJoint.Create(def: Tb2WheelJointDef);
begin
   inherited Create(def);
   m_localAnchorA := def.localAnchorA;
   m_localAnchorB := def.localAnchorB;
   m_localXAxisA := def.localAxisA;
   m_localYAxisA := b2Cross(1.0, m_localXAxisA);

   m_mass := 0.0;
	 m_impulse := 0.0;
   m_motorMass := 0.0;
   m_motorImpulse := 0.0;
   m_springMass := 0.0;
   m_springImpulse := 0.0;

   m_maxMotorTorque := def.maxMotorTorque;
   m_motorSpeed := def.motorSpeed;
   m_enableMotor := def.enableMotor;

 	 m_frequencyHz := def.frequencyHz;
	 m_dampingRatio := def.dampingRatio;
	 m_bias := 0.0;
	 m_gamma := 0.0;

   m_ax := b2Vec2_Zero;
   m_ay := b2Vec2_Zero;
end;

{$IFDEF ENABLE_DUMP}
procedure Tb2WheelJoint.Dump;
var
   indexA, indexB: Int32;
begin
   indexA := m_bodyA.m_islandIndex;
   indexB := m_bodyB.m_islandIndex;

   b2DumpMethod(1, 'begin', []);
   b2DumpMethod(2, 'wheel_jd := Tb2WheelJointDef.Create;', []);
   b2DumpMethod(2, 'wheel_jd.bodyA := bodies[%d];', [indexA]);
   b2DumpMethod(2, 'wheel_jd.bodyB := bodies[%d];', [indexB]);
   b2DumpMethod(2, 'wheel_jd.collideConnected := %s;', [b2BoolToStr(m_collideConnected)]);
   b2DumpMethod(2, 'wheel_jd.localAnchorA := MakeVector(%s, %s);', [b2FloatToStr(m_localAnchorA.x), b2FloatToStr(m_localAnchorA.y)]);
   b2DumpMethod(2, 'wheel_jd.localAnchorB := MakeVector(%s, %s);', [b2FloatToStr(m_localAnchorB.x), b2FloatToStr(m_localAnchorB.y)]);
   b2DumpMethod(2, 'wheel_jd.localAxisA := MakeVector(%s, %s);', [b2FloatToStr(m_localXAxisA.x), b2FloatToStr(m_localXAxisA.y)]);
   b2DumpMethod(2, 'wheel_jd.enableMotor := %s;', [b2BoolToStr(m_enableMotor)]);
   b2DumpMethod(2, 'wheel_jd.motorSpeed := %s;', [b2FloatToStr(m_motorSpeed)]);
   b2DumpMethod(2, 'wheel_jd.maxMotorTorque := %s;', [b2FloatToStr(m_maxMotorTorque)]);
   b2DumpMethod(2, 'wheel_jd.frequencyHz := %s;', [b2FloatToStr(m_frequencyHz)]);
   b2DumpMethod(2, 'wheel_jd.dampingRatio := %s;', [b2FloatToStr(m_dampingRatio)]);
   b2DumpMethod(2, 'joints[%d] := m_world.CreateJoint(wheel_jd);', [m_index]);
   b2DumpMethod(1, 'end;', []);
end;
{$ENDIF}

procedure Tb2WheelJoint.InitVelocityConstraints(const data: Tb2SolverData);
var
   mA, mB: PhysicsFloat;
   iA, iB: PhysicsFloat;
   cA, cB: TVector2;
   vA, vB: TVector2;
   aA, aB: PhysicsFloat;
   wA, wB: PhysicsFloat;
   qA, qB: Tb2Rot;
   rA, rB, P, d: TVector2;
   invMass, C, omega, k, LA, LB, dampCof, h: PhysicsFloat;
begin
   m_indexA := m_bodyA.m_islandIndex;
   m_indexB := m_bodyB.m_islandIndex;
   m_localCenterA := m_bodyA.m_sweep.localCenter;
   m_localCenterB := m_bodyB.m_sweep.localCenter;
   m_invMassA := m_bodyA.m_invMass;
   m_invMassB := m_bodyB.m_invMass;
   m_invIA := m_bodyA.m_invI;
   m_invIB := m_bodyB.m_invI;

   mA := m_invMassA;
   mB := m_invMassB;
   iA := m_invIA;
   iB := m_invIB;

   cA := data.positions[m_indexA].c;
   aA := data.positions[m_indexA].a;
   vA := data.velocities[m_indexA].v;
   wA := data.velocities[m_indexA].w;

   cB := data.positions[m_indexB].c;
   aB := data.positions[m_indexB].a;
   vB := data.velocities[m_indexB].v;
   wB := data.velocities[m_indexB].w;

   {$IFDEF OP_OVERLOAD}
   qA.SetAngle(aA);
   qB.SetAngle(aB);

   // Compute the effective masses.
   rA := b2Mul(qA, m_localAnchorA - m_localCenterA);
   rB := b2Mul(qB, m_localAnchorB - m_localCenterB);
   d := cB + rB - cA - rA;
   {$ELSE}
   SetAngle(qA, aA);
   SetAngle(qB, aB);

   // Compute the effective masses.
   rA := b2Mul(qA, Subtract(m_localAnchorA, m_localCenterA));
   rB := b2Mul(qB, Subtract(m_localAnchorB, m_localCenterB));
   d := Subtract(Add(cB, rB), Add(cA, rA));
   {$ENDIF}

   // Point to line constraint
   begin
      m_ay := b2Mul(qA, m_localYAxisA);
      {$IFDEF OP_OVERLOAD}
      m_sAy := b2Cross(d + rA, m_ay);
      {$ELSE}
      m_sAy := b2Cross(Add(d, rA), m_ay);
      {$ENDIF}
      m_sBy := b2Cross(rB, m_ay);

      m_mass := mA + mB + iA * m_sAy * m_sAy + iB * m_sBy * m_sBy;
      if m_mass > 0.0 then
         m_mass := 1.0 / m_mass;
   end;

	 // Spring constraint
	 m_springMass := 0.0;
   m_bias := 0.0;
   m_gamma := 0.0;
   if m_frequencyHz > 0.0 then
   begin
      m_ax := b2Mul(qA, m_localXAxisA);
      {$IFDEF OP_OVERLOAD}
      m_sAx := b2Cross(d + rA, m_ax);
      {$ELSE}
      m_sAx := b2Cross(Add(d, rA), m_ax);
      {$ENDIF}
      m_sBx := b2Cross(rB, m_ax);

      invMass := mA + mB + iA * m_sAx * m_sAx + iB * m_sBx * m_sBx;

      if invMass > 0.0 then
      begin
         m_springMass := 1.0 / invMass;

         C := b2Dot(d, m_ax);

         // Frequency
         omega := 2.0 * Pi * m_frequencyHz;

         // Damping coefficient
         dampCof := 2.0 * m_springMass * m_dampingRatio * omega;

         // Spring stiffness
         k := m_springMass * omega * omega;

         // magic formulas
         h := data.step.dt;
         m_gamma := h * (dampCof + h * k);
         if m_gamma > 0.0 then
            m_gamma := 1.0 / m_gamma;

         m_bias := C * h * k * m_gamma;

         m_springMass := invMass + m_gamma;
         if m_springMass > 0.0 then
            m_springMass := 1.0 / m_springMass;
      end;
   end
   else
		  m_springImpulse := 0.0;

   // Rotational motor
   if m_enableMotor then
   begin
      m_motorMass := iA + iB;
      if m_motorMass > 0.0 then
         m_motorMass := 1.0 / m_motorMass;
   end
   else
   begin
      m_motorMass := 0.0;
      m_motorImpulse := 0.0;
   end;

   if data.step.warmStarting then
   begin
      // Account for variable time step.
      m_impulse := m_impulse * data.step.dtRatio;
      m_springImpulse := m_springImpulse * data.step.dtRatio;
      m_motorImpulse := m_motorImpulse * data.step.dtRatio;

      {$IFDEF OP_OVERLOAD}
      P := m_impulse * m_ay + m_springImpulse * m_ax;
      {$ELSE}
      P := Add(Multiply(m_ay, m_impulse), Multiply(m_ax, m_springImpulse));
      {$ENDIF}
      LA := m_impulse * m_sAy + m_springImpulse * m_sAx + m_motorImpulse;
      LB := m_impulse * m_sBy + m_springImpulse * m_sBx + m_motorImpulse;

      {$IFDEF OP_OVERLOAD}
      vA.SubtractBy(m_invMassA * P);
      vB.AddBy(m_invMassB * P);
      {$ELSE}
      SubtractBy(vA, Multiply(P, m_invMassA));
      AddBy(vB, Multiply(P, m_invMassB));
      {$ENDIF}

      wA := wA - m_invIA * LA;
      wB := wB + m_invIB * LB;
   end
   else
   begin
      m_impulse := 0.0;
      m_springImpulse := 0.0;
      m_motorImpulse := 0.0;
   end;

   data.velocities[m_indexA].v := vA;
   data.velocities[m_indexA].w := wA;
   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
end;

procedure Tb2WheelJoint.SolveVelocityConstraints(const data: Tb2SolverData);
var
   mA, mB: PhysicsFloat;
   iA, iB: PhysicsFloat;
   vA, vB: TVector2;
   wA, wB: PhysicsFloat;
   Cdot, impulse: PhysicsFloat;
   P: TVector2;
   LA, LB: PhysicsFloat;
   Cdotf, oldImpulse, maxImpulse: PhysicsFloat;
begin
   mA := m_invMassA;
   mB := m_invMassB;
   iA := m_invIA;
   iB := m_invIB;

   vA := data.velocities[m_indexA].v;
   wA := data.velocities[m_indexA].w;
   vB := data.velocities[m_indexB].v;
   wB := data.velocities[m_indexB].w;

   // Solve spring constraint
   {$IFDEF OP_OVERLOAD}
   Cdot := b2Dot(m_ax, vB - vA) + m_sBx * wB - m_sAx * wA;
   {$ELSE}
   Cdot := b2Dot(m_ax, Subtract(vB, vA)) + m_sBx * wB - m_sAx * wA;
   {$ENDIF}
   impulse := -m_springMass * (Cdot + m_bias + m_gamma * m_springImpulse);
   m_springImpulse := m_springImpulse + impulse;

   {$IFDEF OP_OVERLOAD}
   P := impulse * m_ax;
   {$ELSE}
   P := Multiply(m_ax, impulse);
   {$ENDIF}
   LA := impulse * m_sAx;
   LB := impulse * m_sBx;

   {$IFDEF OP_OVERLOAD}
   vA.SubtractBy(mA * P);
   vB.AddBy(mB * P);
   {$ELSE}
   SubtractBy(vA, Multiply(P, mA));
   AddBy(vB, Multiply(P, mB));
   {$ENDIF}
   wA := wA - iA * LA;
   wB := wB + iB * LB;

   // Solve rotational motor constraint
   Cdotf := wB - wA - m_motorSpeed;
   impulse := -m_motorMass * Cdotf;

   oldImpulse := m_motorImpulse;
   maxImpulse := data.step.dt * m_maxMotorTorque;
   m_motorImpulse := b2Clamp(m_motorImpulse + impulse, -maxImpulse, maxImpulse);
   impulse := m_motorImpulse - oldImpulse;

   wA := wA - iA * impulse;
   wB := wB + iB * impulse;

   // Solve point to line constraint
   {$IFDEF OP_OVERLOAD}
   Cdotf := b2Dot(m_ay, vB - vA) + m_sBy * wB - m_sAy * wA;
   {$ELSE}
   Cdotf := b2Dot(m_ay, Subtract(vB, vA)) + m_sBy * wB - m_sAy * wA;
   {$ENDIF}
   impulse := -m_mass * Cdotf;
   m_impulse := m_impulse + impulse;

   {$IFDEF OP_OVERLOAD}
   P := impulse * m_ay;
   {$ELSE}
   P := Multiply(m_ay, impulse);
   {$ENDIF}
   LA := impulse * m_sAy;
   LB := impulse * m_sBy;

   {$IFDEF OP_OVERLOAD}
   vA.SubtractBy(mA * P);
   vB.AddBy(mB * P);
   {$ELSE}
   SubtractBy(vA, Multiply(P, mA));
   AddBy(vB, Multiply(P, mB));
   {$ENDIF}
   wA := wA - iA * LA;
   wB := wB + iB * LB;

   data.velocities[m_indexA].v := vA;
   data.velocities[m_indexA].w := wA;
   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
end;

function Tb2WheelJoint.SolvePositionConstraints(const data: Tb2SolverData): Boolean;
var
   cA, cB: TVector2;
   aA, aB: PhysicsFloat;
   qA, qB: Tb2Rot;
   rA, rB, d, ay: TVector2;
   sAy, sBy: PhysicsFloat;
   C, k, impulse: PhysicsFloat;
   P: TVector2;
   LA, LB: PhysicsFloat;
begin
   cA := data.positions[m_indexA].c;
   aA := data.positions[m_indexA].a;
   cB := data.positions[m_indexB].c;
   aB := data.positions[m_indexB].a;

   {$IFDEF OP_OVERLOAD}
   qA.SetAngle(aA);
   qB.SetAngle(aB);

   rA := b2Mul(qA, m_localAnchorA - m_localCenterA);
   rB := b2Mul(qB, m_localAnchorB - m_localCenterB);
   d := (cB - cA) + rB - rA;
   {$ELSE}
   SetAngle(qA, aA);
   SetAngle(qB, aB);

   rA := b2Mul(qA, Subtract(m_localAnchorA, m_localCenterA));
   rB := b2Mul(qB, Subtract(m_localAnchorB, m_localCenterB));
   d := Add(Subtract(cB, cA), Subtract(rB, rA));
   {$ENDIF}

   ay := b2Mul(qA, m_localYAxisA);

   {$IFDEF OP_OVERLOAD}
   sAy := b2Cross(d + rA, ay);
   {$ELSE}
   sAy := b2Cross(Add(d, rA), ay);
   {$ENDIF}
   sBy := b2Cross(rB, ay);

   C := b2Dot(d, ay);

   k := m_invMassA + m_invMassB + m_invIA * m_sAy * m_sAy + m_invIB * m_sBy * m_sBy;

   if k <> 0.0 then
      impulse := - C / k
   else
      impulse := 0.0;

   {$IFDEF OP_OVERLOAD}
   P := impulse * ay;
   {$ELSE}
   P := Multiply(ay, impulse);
   {$ENDIF}
   LA := impulse * sAy;
   LB := impulse * sBy;

   {$IFDEF OP_OVERLOAD}
   cA.SubtractBy(m_invMassA * P);
   cB.AddBy(m_invMassB * P);
   {$ELSE}
   SubtractBy(cA, Multiply(P, m_invMassA));
   AddBy(cB, Multiply(P, m_invMassB));
   {$ENDIF}
   aA := aA - m_invIA * LA;
   aB := aB + m_invIB * LB;

   data.positions[m_indexA].c := cA;
   data.positions[m_indexA].a := aA;
   data.positions[m_indexB].c := cB;
   data.positions[m_indexB].a := aB;

   Result := Abs(C) <= b2_linearSlop;
end;

function Tb2WheelJoint.GetAnchorA: TVector2;
begin
   Result := m_bodyA.GetWorldPoint(m_localAnchorA);
end;

function Tb2WheelJoint.GetAnchorB: TVector2;
begin
   Result := m_bodyB.GetWorldPoint(m_localAnchorB);
end;

function Tb2WheelJoint.GetReactionForce(inv_dt: PhysicsFloat): TVector2;
begin
   {$IFDEF OP_OVERLOAD}
   Result := inv_dt * (m_impulse * m_ay + m_springImpulse * m_ax);
   {$ELSE}
   Result := Multiply(Add(Multiply(m_ay, m_impulse), Multiply(m_ax, m_springImpulse)), inv_dt);
   {$ENDIF}
end;

function Tb2WheelJoint.GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat;
begin
	 Result := inv_dt * m_motorImpulse;
end;

function Tb2WheelJoint.GetJointSpeed: PhysicsFloat;
begin
   Result := m_bodyB.m_angularVelocity - m_bodyA.m_angularVelocity;
end;

function Tb2WheelJoint.GetJointTranslation: PhysicsFloat;
begin
   {$IFDEF OP_OVERLOAD}
   Result := b2Dot(m_bodyB.GetWorldPoint(m_localAnchorB) -
      m_bodyA.GetWorldPoint(m_localAnchorA), m_bodyA.GetWorldVector(m_localXAxisA));
   {$ELSE}
   Result := b2Dot(Subtract(m_bodyB.GetWorldPoint(m_localAnchorB),
      m_bodyA.GetWorldPoint(m_localAnchorA)), m_bodyA.GetWorldVector(m_localXAxisA));
   {$ENDIF}
end;

function Tb2WheelJoint.GetMotorTorque(inv_dt: PhysicsFloat): PhysicsFloat;
begin
   Result := inv_dt * m_motorImpulse;
end;

procedure Tb2WheelJoint.EnableMotor(flag: Boolean);
begin
   m_bodyA.SetAwake(True);
   m_bodyB.SetAwake(True);
   m_enableMotor := flag;
end;

procedure Tb2WheelJoint.SetMotorSpeed(speed: PhysicsFloat);
begin
   m_bodyA.SetAwake(True);
   m_bodyB.SetAwake(True);
   m_motorSpeed := speed;
end;

procedure Tb2WheelJoint.SetMaxMotorTorque(torque: PhysicsFloat);
begin
   m_bodyA.SetAwake(True);
   m_bodyB.SetAwake(True);
   m_maxMotorTorque := torque;
end;

{ Tb2WeldJointDef }

// Point-to-point constraint
// C = p2 - p1
// Cdot = v2 - v1
//      = v2 + cross(w2, r2) - v1 - cross(w1, r1)
// J = [-I -r1_skew I r2_skew ]
// Identity used:
// w k % (rx i + ry j) = w * (-ry i + rx j)

// Angle constraint
// C = angle2 - angle1 - referenceAngle
// Cdot = w2 - w1
// J = [0 0 -1 0 0 1]
// K = invI1 + invI2

constructor Tb2WeldJointDef.Create;
begin
   inherited;
   jointType := e_weldJoint;
   localAnchorA := b2Vec2_Zero;
   localAnchorB := b2Vec2_Zero;
   referenceAngle := 0.0;
   frequencyHz := 0.0;
   dampingRatio := 0.0;
end;

procedure Tb2WeldJointDef.Initialize(bodyA, bodyB: Tb2Body; const anchor: TVector2);
begin
   Self.bodyA := bodyA;
   Self.bodyB := bodyB;
   localAnchorA := bodyA.GetLocalPoint(anchor);
   localAnchorB := bodyB.GetLocalPoint(anchor);
   referenceAngle := bodyB.GetAngle - bodyA.GetAngle;
end;

{ Tb2WeldJoint }

constructor Tb2WeldJoint.Create(def: Tb2WeldJointDef);
begin
   inherited Create(def);
   m_localAnchorA := def.localAnchorA;
   m_localAnchorB := def.localAnchorB;
   m_referenceAngle := def.referenceAngle;
   m_frequencyHz := def.frequencyHz;
   m_dampingRatio := def.dampingRatio;
   m_impulse := b2Vec3_Zero;
end;

{$IFDEF ENABLE_DUMP}
procedure Tb2WeldJoint.Dump;
var
   indexA, indexB: Int32;
begin
   indexA := m_bodyA.m_islandIndex;
   indexB := m_bodyB.m_islandIndex;

   b2DumpMethod(1, 'begin', []);
   b2DumpMethod(2, 'weld_jd := Tb2WeldJointDef.Create;', []);
   b2DumpMethod(2, 'weld_jd.bodyA := bodies[%d];', [indexA]);
   b2DumpMethod(2, 'weld_jd.bodyB := bodies[%d];', [indexB]);
   b2DumpMethod(2, 'weld_jd.collideConnected := %s;', [b2BoolToStr(m_collideConnected)]);
   b2DumpMethod(2, 'weld_jd.localAnchorA := MakeVector(%s, %s);', [b2FloatToStr(m_localAnchorA.x), b2FloatToStr(m_localAnchorA.y)]);
   b2DumpMethod(2, 'weld_jd.localAnchorB := MakeVector(%s, %s);', [b2FloatToStr(m_localAnchorB.x), b2FloatToStr(m_localAnchorB.y)]);
   b2DumpMethod(2, 'weld_jd.referenceAngle := %s;', [b2FloatToStr(m_referenceAngle)]);
   b2DumpMethod(2, 'weld_jd.frequencyHz := %s;', [b2FloatToStr(m_frequencyHz)]);
   b2DumpMethod(2, 'weld_jd.dampingRatio := %s;', [b2FloatToStr(m_dampingRatio)]);
   b2DumpMethod(2, 'joints[%d] := m_world.CreateJoint(weld_jd);', [m_index]);
   b2DumpMethod(1, 'end;', []);
end;
{$ENDIF}

function Tb2WeldJoint.GetAnchorA: TVector2;
begin
   Result := m_bodyA.GetWorldPoint(m_localAnchorA);
end;

function Tb2WeldJoint.GetAnchorB: TVector2;
begin
   Result := m_bodyB.GetWorldPoint(m_localAnchorB);
end;

function Tb2WeldJoint.GetReactionForce(inv_dt: PhysicsFloat): TVector2;
begin
   Result.x := inv_dt * m_impulse.x;
   Result.y := inv_dt * m_impulse.y;
end;

function Tb2WeldJoint.GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat;
begin
   Result := inv_dt * m_impulse.z;
end;

procedure Tb2WeldJoint.InitVelocityConstraints(const data: Tb2SolverData);
var
   vA, vB: TVector2;
   aA, aB: PhysicsFloat;
   wA, wB: PhysicsFloat;
   qA, qB: Tb2Rot;
   mA, mB: PhysicsFloat;
   iA, iB: PhysicsFloat;
   P: TVector2;
   K: TMatrix33;
   invM, m, C, omega, d, kk, h: PhysicsFloat;
begin
   m_indexA := m_bodyA.m_islandIndex;
   m_indexB := m_bodyB.m_islandIndex;
   m_localCenterA := m_bodyA.m_sweep.localCenter;
   m_localCenterB := m_bodyB.m_sweep.localCenter;
   m_invMassA := m_bodyA.m_invMass;
   m_invMassB := m_bodyB.m_invMass;
   m_invIA := m_bodyA.m_invI;
   m_invIB := m_bodyB.m_invI;

   aA := data.positions[m_indexA].a;
   vA := data.velocities[m_indexA].v;
   wA := data.velocities[m_indexA].w;

   aB := data.positions[m_indexB].a;
   vB := data.velocities[m_indexB].v;
   wB := data.velocities[m_indexB].w;

   {$IFDEF OP_OVERLOAD}
   qA.SetAngle(aA);
   qB.SetAngle(aB);

   m_rA := b2Mul(qA, m_localAnchorA - m_localCenterA);
   m_rB := b2Mul(qB, m_localAnchorB - m_localCenterB);
   {$ELSE}
   SetAngle(qA, aA);
   SetAngle(qB, aB);

   m_rA := b2Mul(qA, Subtract(m_localAnchorA, m_localCenterA));
   m_rB := b2Mul(qB, Subtract(m_localAnchorB, m_localCenterB));
   {$ENDIF}

   // J := [-I -r1_skew I r2_skew]
   //     [ 0       -1 0       1]
   // r_skew := [-ry; rx]

   // Matlab
   // K := [ mA+r1y^2*iA+mB+r2y^2*iB,  -r1y*iA*r1x-r2y*iB*r2x,          -r1y*iA-r2y*iB]
   //     [  -r1y*iA*r1x-r2y*iB*r2x, mA+r1x^2*iA+mB+r2x^2*iB,           r1x*iA+r2x*iB]
   //     [          -r1y*iA-r2y*iB,           r1x*iA+r2x*iB,                   iA+iB]

   mA := m_invMassA;
   mB := m_invMassB;
   iA := m_invIA;
   iB := m_invIB;

   K.ex.x := mA + mB + m_rA.y * m_rA.y * iA + m_rB.y * m_rB.y * iB;
   K.ey.x := -m_rA.y * m_rA.x * iA - m_rB.y * m_rB.x * iB;
   K.ez.x := -m_rA.y * iA - m_rB.y * iB;
   K.ex.y := K.ey.x;
   K.ey.y := mA + mB + m_rA.x * m_rA.x * iA + m_rB.x * m_rB.x * iB;
   K.ez.y := m_rA.x * iA + m_rB.x * iB;
   K.ex.z := K.ez.x;
   K.ey.z := K.ez.y;
   K.ez.z := iA + iB;

   if m_frequencyHz > 0.0 then
   begin
      {$IFDEF OP_OVERLOAD}
      K.GetInverse22(m_mass);
      {$ELSE}
      GetInverse22(K, m_mass);
      {$ENDIF}

      invM := iA + iB;
      if invM > 0.0 then
         m := 1.0 / invM
      else
         m := 0.0;

      C := aB - aA - m_referenceAngle;

      // Frequency
      omega := 2.0 * Pi * m_frequencyHz;

      // Damping coefficient
      d := 2.0 * m * m_dampingRatio * omega;

      // Spring stiffness
      kk := m * omega * omega;

      // magic formulas
      h := data.step.dt;
      m_gamma := h * (d + h * kk);
      if m_gamma <> 0.0 then
         m_gamma := 1.0 / m_gamma
      else
         m_gamma := 0.0;
      m_bias := C * h * kk * m_gamma;

      invM := invM + m_gamma;
      if invM <> 0.0 then
         m_mass.ez.z := 1.0 / invM
      else
         m_mass.ez.z := 0.0;
   end
   else
   begin
      {$IFDEF OP_OVERLOAD}
      K.GetSymInverse33(m_mass);
      {$ELSE}
      GetSymInverse33(K, m_mass);
      {$ENDIF}
      m_gamma := 0.0;
      m_bias := 0.0;
   end;

   if data.step.warmStarting then
   begin
      // Scale impulses to support a variable time step.
      {$IFDEF OP_OVERLOAD}
      m_impulse.MultiplyBy(data.step.dtRatio);
      {$ELSE}
      MultiplyBy(m_impulse, data.step.dtRatio);
      {$ENDIF}

      P.x := m_impulse.x;
      P.y := m_impulse.y;

      {$IFDEF OP_OVERLOAD}
      vA.SubtractBy(mA * P);
      vB.AddBy(mB * P);
      {$ELSE}
      SubtractBy(vA, Multiply(P, mA));
      AddBy(vB, Multiply(P, mB));
      {$ENDIF}
      wA := wA - iA * (b2Cross(m_rA, P) + m_impulse.z);
      wB := wB + iB * (b2Cross(m_rB, P) + m_impulse.z);
   end
   else
      m_impulse := b2Vec3_Zero;

   data.velocities[m_indexA].v := vA;
   data.velocities[m_indexA].w := wA;
   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
end;

procedure Tb2WeldJoint.SolveVelocityConstraints(const data: Tb2SolverData);
var
   vA, vB: TVector2;
   wA, wB: PhysicsFloat;
   mA, mB: PhysicsFloat;
   iA, iB: PhysicsFloat;
   Cdot1: TVector2;
   Cdot2: PhysicsFloat;
   Cdot, impulse: TVector3;
   impulse1: TVector2;
   impulse2: PhysicsFloat;
   P: TVector2;
begin
   vA := data.velocities[m_indexA].v;
   wA := data.velocities[m_indexA].w;
   vB := data.velocities[m_indexB].v;
   wB := data.velocities[m_indexB].w;

   mA := m_invMassA;
   mB := m_invMassB;
   iA := m_invIA;
   iB := m_invIB;

   if m_frequencyHz > 0.0 then
   begin
      Cdot2 := wB - wA;

      impulse2 := -m_mass.ez.z * (Cdot2 + m_bias + m_gamma * m_impulse.z);
      m_impulse.z := m_impulse.z + impulse2;

      wA := wA - iA * impulse2;
      wB := wB + iB * impulse2;

      {$IFDEF OP_OVERLOAD}
      Cdot1 := vB + b2Cross(wB, m_rB) - vA - b2Cross(wA, m_rA);
      impulse1 := -b2Mul22(m_mass, Cdot1);
      {$ELSE}
      Cdot1 := Subtract(Add(vB, b2Cross(wB, m_rB)), Add(vA, b2Cross(wA, m_rA)));
      impulse1 := Negative(b2Mul22(m_mass, Cdot1));
      {$ENDIF}
      m_impulse.x := m_impulse.x + impulse1.x;
      m_impulse.y := m_impulse.y + impulse1.y;

      P := impulse1;

      {$IFDEF OP_OVERLOAD}
      vA.SubtractBy(mA * P);
      vB.AddBy(mB * P);
      {$ELSE}
      SubtractBy(vA, Multiply(P, mA));
      AddBy(vB, Multiply(P, mB));
      {$ENDIF}
      wA := wA - iA * b2Cross(m_rA, P);
      wB := wB + iB * b2Cross(m_rB, P);
   end
   else
   begin
      {$IFDEF OP_OVERLOAD}
      Cdot1 := vB + b2Cross(wB, m_rB) - vA - b2Cross(wA, m_rA);
      {$ELSE}
      Cdot1 := Subtract(Add(vB, b2Cross(wB, m_rB)), Add(vA, b2Cross(wA, m_rA)));
      {$ENDIF}
      Cdot2 := wB - wA;

      Cdot.x := Cdot1.x;
      Cdot.y := Cdot1.y;
      Cdot.z := Cdot2;

      {$IFDEF OP_OVERLOAD}
      impulse := -b2Mul(m_mass, Cdot);
      m_impulse.AddBy(impulse);
      {$ELSE}
      impulse := Negative(b2Mul(m_mass, Cdot));
      AddBy(m_impulse, impulse);
      {$ENDIF}

      P.x := impulse.x;
      P.y := impulse.y;

      {$IFDEF OP_OVERLOAD}
      vA.SubtractBy(mA * P);
      vB.AddBy(mB * P);
      {$ELSE}
      SubtractBy(vA, Multiply(P, mA));
      AddBy(vB, Multiply(P, mB));
      {$ENDIF}
      wA := wA - iA * (b2Cross(m_rA, P) + impulse.z);
      wB := wB + iB * (b2Cross(m_rB, P) + impulse.z);
   end;

   data.velocities[m_indexA].v := vA;
   data.velocities[m_indexA].w := wA;
   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
end;

function Tb2WeldJoint.SolvePositionConstraints(const data: Tb2SolverData): Boolean;
var
   cA, cB: TVector2;
   aA, aB: PhysicsFloat;
   qA, qB: Tb2Rot;
   mA, mB: PhysicsFloat;
   iA, iB: PhysicsFloat;
   rA, rB, C1: TVector2;
   C2: PhysicsFloat;
   positionError, angularError: PhysicsFloat;
   C, impulse: TVector3;
   P: TVector2;
   K: TMatrix33;
begin
   cA := data.positions[m_indexA].c;
   aA := data.positions[m_indexA].a;
   cB := data.positions[m_indexB].c;
   aB := data.positions[m_indexB].a;

   {$IFDEF OP_OVERLOAD}
   qA.SetAngle(aA);
   qB.SetAngle(aB);
   {$ELSE}
   SetAngle(qA, aA);
   SetAngle(qB, aB);
   {$ENDIF}

   mA := m_invMassA;
   mB := m_invMassB;
   iA := m_invIA;
   iB := m_invIB;

   {$IFDEF OP_OVERLOAD}
   rA := b2Mul(qA, m_localAnchorA - m_localCenterA);
   rB := b2Mul(qB, m_localAnchorB - m_localCenterB);
   {$ELSE}
   rA := b2Mul(qA, Subtract(m_localAnchorA, m_localCenterA));
   rB := b2Mul(qB, Subtract(m_localAnchorB, m_localCenterB));
   {$ENDIF}

   K.ex.x := mA + mB + rA.y * rA.y * iA + rB.y * rB.y * iB;
   K.ey.x := -rA.y * rA.x * iA - rB.y * rB.x * iB;
   K.ez.x := -rA.y * iA - rB.y * iB;
   K.ex.y := K.ey.x;
   K.ey.y := mA + mB + rA.x * rA.x * iA + rB.x * rB.x * iB;
   K.ez.y := rA.x * iA + rB.x * iB;
   K.ex.z := K.ez.x;
   K.ey.z := K.ez.y;
   K.ez.z := iA + iB;

   if m_frequencyHz > 0.0 then
   begin
      {$IFDEF OP_OVERLOAD}
      C1 :=  cB + rB - cA - rA;
      positionError := C1.Length;
      {$ELSE}
      C1 := Subtract(Add(cB, rB), Add(cA, rA));
      positionError := LengthVec(C1);
      {$ENDIF}
      angularError := 0.0;

      {$IFDEF OP_OVERLOAD}
      P := -K.Solve22(C1);
      cA.SubtractBy(mA * P);
      cB.AddBy(mB * P);
      {$ELSE}
      P := Negative(Solve22(K, C1));
      SubtractBy(cA, Multiply(P, mA));
      AddBy(cB, Multiply(P, mB));
      {$ENDIF}
      aA := aA - iA * b2Cross(rA, P);
      aB := aB + iB * b2Cross(rB, P);
   end
   else
   begin
      {$IFDEF OP_OVERLOAD}
      C1 :=  cB + rB - cA - rA;
      {$ELSE}
      C1 :=  Subtract(Add(cB, rB), Add(cA, rA));
      {$ENDIF}
      C2 := aB - aA - m_referenceAngle;

      {$IFDEF OP_OVERLOAD}
      positionError := C1.Length;
      {$ELSE}
      positionError := LengthVec(C1);
      {$ENDIF}
      angularError := Abs(C2);

      C.x := C1.x;
      C.y := C1.y;
      C.z := C2;

      {$IFDEF OP_OVERLOAD}
      impulse := -K.Solve33(C);
      {$ELSE}
      impulse := Negative(Solve33(K, C));
      {$ENDIF}

      P.x := impulse.x;
      P.y := impulse.y;

      {$IFDEF OP_OVERLOAD}
      cA.SubtractBy(mA * P);
      cB.AddBy(mB * P);
      {$ELSE}
      SubtractBy(cA, Multiply(P, mA));
      AddBy(cB, Multiply(P, mB));
      {$ENDIF}
      aA := aA - iA * (b2Cross(rA, P) + impulse.z);
      aB := aB + iB * (b2Cross(rB, P) + impulse.z);
   end;

   data.positions[m_indexA].c := cA;
   data.positions[m_indexA].a := aA;
   data.positions[m_indexB].c := cB;
   data.positions[m_indexB].a := aB;

   Result := (positionError <= b2_linearSlop) and (angularError <= b2_angularSlop);
end;

{ Tb2RopeJointDef }

constructor Tb2RopeJointDef.Create;
begin
		jointType := e_ropeJoint;
		SetValue(localAnchorA, -1.0, 0.0);
		SetValue(localAnchorB, 1.0, 0.0);
		maxLength := 0.0;
end;

{ Tb2RopeJoint }

// Limit:
// C = norm(pB - pA) - L
// u = (pB - pA) / norm(pB - pA)
// Cdot = dot(u, vB + cross(wB, rB) - vA - cross(wA, rA))
// J = [-u -cross(rA, u) u cross(rB, u)]
// K = J * invM * JT
//   = invMassA + invIA * cross(rA, u)^2 + invMassB + invIB * cross(rB, u)^2

constructor Tb2RopeJoint.Create(def: Tb2RopeJointDef);
begin
   inherited Create(def);
   m_localAnchorA := def.localAnchorA;
   m_localAnchorB := def.localAnchorB;

   m_maxLength := def.maxLength;

   m_mass := 0.0;
   m_impulse := 0.0;
   m_state := e_inactiveLimit;
   m_length := 0.0;
end;

{$IFDEF ENABLE_DUMP}
procedure Tb2RopeJoint.Dump;
var
   indexA, indexB: Int32;
begin
   indexA := m_bodyA.m_islandIndex;
   indexB := m_bodyB.m_islandIndex;

   b2DumpMethod(1, 'begin', []);
   b2DumpMethod(2, 'rope_jd := Tb2RopeJointDef.Create;', []);
   b2DumpMethod(2, 'rope_jd.bodyA := bodies[%d];', [indexA]);
   b2DumpMethod(2, 'rope_jd.bodyB := bodies[%d];', [indexB]);
   b2DumpMethod(2, 'rope_jd.collideConnected := %s;', [b2BoolToStr(m_collideConnected)]);
   b2DumpMethod(2, 'rope_jd.localAnchorA := MakeVector(%s, %s);', [b2FloatToStr(m_localAnchorA.x), b2FloatToStr(m_localAnchorA.y)]);
   b2DumpMethod(2, 'rope_jd.localAnchorB := MakeVector(%s, %s);', [b2FloatToStr(m_localAnchorB.x), b2FloatToStr(m_localAnchorB.y)]);
   b2DumpMethod(2, 'rope_jd.maxLength := %s;', [b2FloatToStr(m_maxLength)]);
   b2DumpMethod(2, 'joints[%d] := m_world.CreateJoint(rope_jd);', [m_index]);
   b2DumpMethod(1, 'end;', []);
end;
{$ENDIF}

procedure Tb2RopeJoint.InitVelocityConstraints(const data: Tb2SolverData);
var
   cA, cB: TVector2;
   vA, vB: TVector2;
   aA, aB: PhysicsFloat;
   wA, wB: PhysicsFloat;
   C: PhysicsFloat;
   qA, qB: Tb2Rot;
   crA, crB, invMass: PhysicsFloat;
   P: TVector2;
begin
   m_indexA := m_bodyA.m_islandIndex;
   m_indexB := m_bodyB.m_islandIndex;
   m_localCenterA := m_bodyA.m_sweep.localCenter;
   m_localCenterB := m_bodyB.m_sweep.localCenter;
   m_invMassA := m_bodyA.m_invMass;
   m_invMassB := m_bodyB.m_invMass;
   m_invIA := m_bodyA.m_invI;
   m_invIB := m_bodyB.m_invI;

   cA := data.positions[m_indexA].c;
   aA := data.positions[m_indexA].a;
   vA := data.velocities[m_indexA].v;
   wA := data.velocities[m_indexA].w;

   cB := data.positions[m_indexB].c;
   aB := data.positions[m_indexB].a;
   vB := data.velocities[m_indexB].v;
   wB := data.velocities[m_indexB].w;

   {$IFDEF OP_OVERLOAD}
   qA.SetAngle(aA);
   qB.SetAngle(aB);

   m_rA := b2Mul(qA, m_localAnchorA - m_localCenterA);
   m_rB := b2Mul(qB, m_localAnchorB - m_localCenterB);
   m_u := cB + m_rB - cA - m_rA;

   m_length := m_u.Length;
   {$ELSE}
   SetAngle(qA, aA);
   SetAngle(qB, aB);

   m_rA := b2Mul(qA, Subtract(m_localAnchorA, m_localCenterA));
   m_rB := b2Mul(qB, Subtract(m_localAnchorB, m_localCenterB));
   m_u := Subtract(Add(cB, m_rB), Add(cA, m_rA));

   m_length := LengthVec(m_u);
   {$ENDIF}

   C := m_length - m_maxLength;
   if C > 0.0 then
      m_state := e_atUpperLimit
   else
      m_state := e_inactiveLimit;

   if m_length > b2_linearSlop then
      {$IFDEF OP_OVERLOAD}
      m_u := m_u / m_length
      {$ELSE}
      m_u := Divide(m_u, m_length)
      {$ENDIF}
   else
   begin
      m_u := b2Vec2_Zero;
      m_mass := 0.0;
      m_impulse := 0.0;
      Exit;
   end;

   // Compute effective mass.
   crA := b2Cross(m_rA, m_u);
   crB := b2Cross(m_rB, m_u);
   invMass := m_invMassA + m_invIA * crA * crA + m_invMassB + m_invIB * crB * crB;

   if invMass <> 0.0 then
      m_mass := 1 / invMass
   else
      m_mass := 0.0;

   if data.step.warmStarting then
   begin
      // Scale the impulse to support a variable time step.
      m_impulse := m_impulse * data.step.dtRatio;

      {$IFDEF OP_OVERLOAD}
      P := m_impulse * m_u;
      vA.SubtractBy(m_invMassA * P);
      vB.AddBy(m_invMassB * P);
      {$ELSE}
      P := Multiply(m_u, m_impulse);
      SubtractBy(vA, Multiply(P, m_invMassA));
      AddBy(vB, Multiply(P, m_invMassB));
      {$ENDIF}
      wA := wA - m_invIA * b2Cross(m_rA, P);
      wB := wB + m_invIB * b2Cross(m_rB, P);
   end
   else
      m_impulse := 0.0;

   data.velocities[m_indexA].v := vA;
   data.velocities[m_indexA].w := wA;
   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
end;

procedure Tb2RopeJoint.SolveVelocityConstraints(const data: Tb2SolverData);
var
   vA, vB: TVector2;
   wA, wB: PhysicsFloat;
   vpA, vpB: TVector2;
   C, Cdot: PhysicsFloat;
   impulse, oldImpulse: PhysicsFloat;
   P: TVector2;
begin
   vA := data.velocities[m_indexA].v;
   wA := data.velocities[m_indexA].w;
   vB := data.velocities[m_indexB].v;
   wB := data.velocities[m_indexB].w;

   // Cdot := dot(u, v + cross(w, r))
   {$IFDEF OP_OVERLOAD}
   vpA := vA + b2Cross(wA, m_rA);
   vpB := vB + b2Cross(wB, m_rB);
   C := m_length - m_maxLength;
   Cdot := b2Dot(m_u, vpB - vpA);
   {$ELSE}
   vpA := Add(vA, b2Cross(wA, m_rA));
   vpB := Add(vB, b2Cross(wB, m_rB));
   C := m_length - m_maxLength;
   Cdot := b2Dot(m_u, Subtract(vpB, vpA));
   {$ENDIF}

   // Predictive constraint.
   if C < 0.0 then
      Cdot := Cdot + data.step.inv_dt * C;

   impulse := -m_mass * Cdot;
   oldImpulse := m_impulse;
   m_impulse := b2Min(0.0, m_impulse + impulse);
   impulse := m_impulse - oldImpulse;

   {$IFDEF OP_OVERLOAD}
   P := impulse * m_u;
   vA.SubtractBy(m_invMassA * P);
   vB.AddBy(m_invMassB * P);
   {$ELSE}
   P := Multiply(m_u, impulse);
   SubtractBy(vA, Multiply(P, m_invMassA));
   AddBy(vB, Multiply(P, m_invMassB));
   {$ENDIF}
   wA := wA - m_invIA * b2Cross(m_rA, P);
   wB := wB + m_invIB * b2Cross(m_rB, P);

   data.velocities[m_indexA].v := vA;
   data.velocities[m_indexA].w := wA;
   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
end;

function Tb2RopeJoint.SolvePositionConstraints(const data: Tb2SolverData): Boolean;
var
   cA, cB: TVector2;
   aA, aB: PhysicsFloat;
   qA, qB: Tb2Rot;
   rA, rB, u, P: TVector2;
   length, C, impulse: PhysicsFloat;
begin
   cA := data.positions[m_indexA].c;
   aA := data.positions[m_indexA].a;
   cB := data.positions[m_indexB].c;
   aB := data.positions[m_indexB].a;

   {$IFDEF OP_OVERLOAD}
   qA.SetAngle(aA);
   qB.SetAngle(aB);
   rA := b2Mul(qA, m_localAnchorA - m_localCenterA);
   rB := b2Mul(qB, m_localAnchorB - m_localCenterB);
   u := cB + rB - cA - rA;
   length := u.Normalize;
   {$ELSE}
   SetAngle(qA, aA);
   SetAngle(qB, aB);
   rA := b2Mul(qA, Subtract(m_localAnchorA, m_localCenterA));
   rB := b2Mul(qB, Subtract(m_localAnchorB, m_localCenterB));
   u := Subtract(Add(cB, rB), Add(cA, rA));
   length := Normalize(u);
   {$ENDIF}

   C := length - m_maxLength;
   C := b2Clamp(C, 0.0, b2_maxLinearCorrection);

   impulse := -m_mass * C;

   {$IFDEF OP_OVERLOAD}
   P := impulse * u;
   cA.SubtractBy(m_invMassA * P);
   cB.AddBy(m_invMassB * P);
   {$ELSE}
   P := Multiply(u, impulse);
   SubtractBy(cA, Multiply(P, m_invMassA));
   AddBy(cB, Multiply(P, m_invMassB));
   {$ENDIF}
   aA := aA - m_invIA * b2Cross(rA, P);
   aB := aB + m_invIB * b2Cross(rB, P);

   data.positions[m_indexA].c := cA;
   data.positions[m_indexA].a := aA;
   data.positions[m_indexB].c := cB;
   data.positions[m_indexB].a := aB;

   Result := (length - m_maxLength) < b2_linearSlop;
end;

function Tb2RopeJoint.GetAnchorA: TVector2;
begin
   Result := m_bodyA.GetWorldPoint(m_localAnchorA);
end;

function Tb2RopeJoint.GetAnchorB: TVector2;
begin
   Result := m_bodyB.GetWorldPoint(m_localAnchorB);
end;

function Tb2RopeJoint.GetReactionForce(inv_dt: PhysicsFloat): TVector2;
begin
   {$IFDEF OP_OVERLOAD}
   Result := (inv_dt * m_impulse) * m_u;
   {$ELSE}
   Result := Multiply(m_u, m_impulse * inv_dt);
   {$ENDIF}
end;

function Tb2RopeJoint.GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat;
begin
	 //B2_NOT_USED(inv_dt);
	 Result := 0.0;
end;

{ Tb2MotorJointDef }

constructor Tb2MotorJointDef.Create;
begin
   jointType := e_motorJoint;
   linearOffset := b2Vec2_Zero;
   angularOffset := 0.0;
   maxForce := 1.0;
   maxTorque := 1.0;
   correctionFactor := 0.3;
end;

procedure Tb2MotorJointDef.Initialize(bodyA, bodyB: Tb2Body);
begin
   Self.bodyA := bodyA;
   Self.bodyB := bodyB;
   linearOffset := bodyA.GetLocalPoint(bodyB.GetPosition);
   angularOffset := bodyB.GetAngle - bodyA.GetAngle;
end;

{ Tb2MotorJoint }

// Point-to-point constraint
// Cdot = v2 - v1
//      = v2 + cross(w2, r2) - v1 - cross(w1, r1)
// J = [-I -r1_skew I r2_skew ]
// Identity used:
// w k % (rx i + ry j) = w * (-ry i + rx j)

// Angle constraint
// Cdot = w2 - w1
// J = [0 0 -1 0 0 1]
// K = invI1 + invI2

constructor Tb2MotorJoint.Create(def: Tb2MotorJointDef);
begin
   inherited Create(def);
   m_linearOffset := def.linearOffset;
   m_angularOffset := def.angularOffset;

   m_linearImpulse := b2Vec2_Zero;
   m_angularImpulse := 0.0;

   m_maxForce := def.maxForce;
   m_maxTorque := def.maxTorque;
   m_correctionFactor := def.correctionFactor;
end;

{$IFDEF ENABLE_DUMP}
procedure Tb2MotorJoint.Dump;
var
   indexA, indexB: Int32;
begin
   indexA := m_bodyA.m_islandIndex;
   indexB := m_bodyB.m_islandIndex;

   b2DumpMethod(1, 'begin', []);
   b2DumpMethod(2, 'motor_jd := Tb2MotorJointDef.Create;', []);
   b2DumpMethod(2, 'motor_jd.bodyA := bodies[%d];', [indexA]);
   b2DumpMethod(2, 'motor_jd.bodyB := bodies[%d];', [indexB]);
   b2DumpMethod(2, 'motor_jd.collideConnected := %s;', [b2BoolToStr(m_collideConnected)]);
   b2DumpMethod(2, 'motor_jd.linearOffset := MakeVector(%s, %s);', [b2FloatToStr(m_linearOffset.x), b2FloatToStr(m_linearOffset.y)]);
   b2DumpMethod(2, 'motor_jd.angularOffset := %s;', [b2FloatToStr(m_angularOffset)]);
   b2DumpMethod(2, 'motor_jd.maxForce := %s;', [b2FloatToStr(m_maxForce)]);
   b2DumpMethod(2, 'motor_jd.maxTorque := %s;', [b2FloatToStr(m_maxTorque)]);
   b2DumpMethod(2, 'motor_jd.correctionFactor := %s;', [b2FloatToStr(m_correctionFactor)]);
   b2DumpMethod(2, 'joints[%d] := m_world.CreateJoint(motor_jd);', [m_index]);
   b2DumpMethod(1, 'end;', []);
end;
{$ENDIF}

procedure Tb2MotorJoint.FSetCorrectionFactor(value: PhysicsFloat);
begin
   if (value >= 0.0) and (value <= 1.0) then
      m_correctionFactor := value;
end;

procedure Tb2MotorJoint.InitVelocityConstraints(const data: Tb2SolverData);
var
   cA, vA, cB, vB: TVector2;
   aA, wA, aB, wB: PhysicsFloat;
   qA, qB: Tb2Rot;
   mA, mB, iA, iB: PhysicsFloat;
   K: TMatrix22;
begin
   m_indexA := m_bodyA.m_islandIndex;
   m_indexB := m_bodyB.m_islandIndex;
   m_localCenterA := m_bodyA.m_sweep.localCenter;
   m_localCenterB := m_bodyB.m_sweep.localCenter;
   m_invMassA := m_bodyA.m_invMass;
   m_invMassB := m_bodyB.m_invMass;
   m_invIA := m_bodyA.m_invI;
   m_invIB := m_bodyB.m_invI;

   cA := data.positions[m_indexA].c;
   aA := data.positions[m_indexA].a;
   vA := data.velocities[m_indexA].v;
   wA := data.velocities[m_indexA].w;

   cB := data.positions[m_indexB].c;
   aB := data.positions[m_indexB].a;
   vB := data.velocities[m_indexB].v;
   wB := data.velocities[m_indexB].w;

   {$IFDEF OP_OVERLOAD}
   qA.SetAngle(aA);
   qB.SetAngle(aB);

   // Compute the effective mass matrix.
   m_rA := b2Mul(qA, -m_localCenterA);
   m_rB := b2Mul(qB, -m_localCenterB);
   {$ELSE}
   SetAngle(qA, aA);
   SetAngle(qB, aB);

   // Compute the effective mass matrix.
   m_rA := b2Mul(qA, Negative(m_localCenterA));
   m_rB := b2Mul(qB, Negative(m_localCenterB));
   {$ENDIF}

   // J := [-I -r1_skew I r2_skew]
   //     [ 0       -1 0       1]
   // r_skew := [-ry; rx]

   // Matlab
   // K := [ mA+r1y^2*iA+mB+r2y^2*iB,  -r1y*iA*r1x-r2y*iB*r2x,          -r1y*iA-r2y*iB]
   //     [  -r1y*iA*r1x-r2y*iB*r2x, mA+r1x^2*iA+mB+r2x^2*iB,           r1x*iA+r2x*iB]
   //     [          -r1y*iA-r2y*iB,           r1x*iA+r2x*iB,                   iA+iB]

   mA := m_invMassA;
   mB := m_invMassB;
   iA := m_invIA;
   iB := m_invIB;

   K.ex.x := mA + mB + iA * m_rA.y * m_rA.y + iB * m_rB.y * m_rB.y;
   K.ex.y := -iA * m_rA.x * m_rA.y - iB * m_rB.x * m_rB.y;
   K.ey.x := K.ex.y;
   K.ey.y := mA + mB + iA * m_rA.x * m_rA.x + iB * m_rB.x * m_rB.x;

   {$IFDEF OP_OVERLOAD}
   m_linearMass := K.GetInverse;
   {$ELSE}
   m_linearMass := GetInverse(K);
   {$ENDIF}

   m_angularMass := iA + iB;
   if m_angularMass > 0.0 then
      m_angularMass := 1.0 / m_angularMass;

   {$IFDEF OP_OVERLOAD}
   m_linearError := cB + m_rB - cA - m_rA - b2Mul(qA, m_linearOffset);
   {$ELSE}
   m_linearError := Subtract(Add(cB, m_rB), Add(cA, m_rA, b2Mul(qA, m_linearOffset)));
   {$ENDIF}
   m_angularError := aB - aA - m_angularOffset;

   if data.step.warmStarting then
   begin
      // Scale impulses to support a variable time step.
      {$IFDEF OP_OVERLOAD}
      m_linearImpulse.MultiplyBy(data.step.dtRatio);
      {$ELSE}
      MultiplyBy(m_linearImpulse, data.step.dtRatio);
      {$ENDIF}
      m_angularImpulse := m_angularImpulse * data.step.dtRatio;

      {$IFDEF OP_OVERLOAD}
      vA.SubtractBy(mA * m_linearImpulse);
      vB.AddBy(mB * m_linearImpulse);
      {$ELSE}
      SubtractBy(vA, Multiply(m_linearImpulse, mA));
      AddBy(vB, Multiply(m_linearImpulse, mB));
      {$ENDIF}
      wA := wA - iA * (b2Cross(m_rA, m_linearImpulse) + m_angularImpulse);
      wB := wB + iB * (b2Cross(m_rB, m_linearImpulse) + m_angularImpulse);
   end
   else
   begin
      m_linearImpulse := b2Vec2_Zero;
      m_angularImpulse := 0.0;
   end;

   data.velocities[m_indexA].v := vA;
   data.velocities[m_indexA].w := wA;
   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
end;

procedure Tb2MotorJoint.SolveVelocityConstraints(const data: Tb2SolverData);
var
   vA, vB: TVector2;
   wA, wB: PhysicsFloat;
   mA, mB, iA, iB: PhysicsFloat;
   h, inv_h: PhysicsFloat;
   Cdot, impulse, oldImpulse, maxImpulse: PhysicsFloat;
   Cdot2, impulse2, oldImpulse2: TVector2;
begin
   vA := data.velocities[m_indexA].v;
   wA := data.velocities[m_indexA].w;
   vB := data.velocities[m_indexB].v;
   wB := data.velocities[m_indexB].w;

   mA := m_invMassA;
   mB := m_invMassB;
   iA := m_invIA;
   iB := m_invIB;

   h := data.step.dt;
   inv_h := data.step.inv_dt;

   // Solve angular friction
   begin
      Cdot := wB - wA + inv_h * m_correctionFactor * m_angularError;
      impulse := -m_angularMass * Cdot;

      oldImpulse := m_angularImpulse;
      maxImpulse := h * m_maxTorque;
      m_angularImpulse := b2Clamp(m_angularImpulse + impulse, -maxImpulse, maxImpulse);
      impulse := m_angularImpulse - oldImpulse;

      wA := wA - iA * impulse;
      wB := wB + iB * impulse;
   end;

   // Solve linear friction
   begin
      {$IFDEF OP_OVERLOAD}
      Cdot2 := vB + b2Cross(wB, m_rB) - vA - b2Cross(wA, m_rA) + inv_h * m_correctionFactor * m_linearError;

      impulse2 := -b2Mul(m_linearMass, Cdot2);
      oldImpulse2 := m_linearImpulse;
      m_linearImpulse.AddBy(impulse2);

      maxImpulse := h * m_maxForce;

      if m_linearImpulse.SqrLength > maxImpulse * maxImpulse then
      begin
         m_linearImpulse.Normalize();
         m_linearImpulse.MultiplyBy(maxImpulse);
      end;

      impulse2 := m_linearImpulse - oldImpulse2;

      vA.SubtractBy(mA * impulse2);
      vB.AddBy(mB * impulse2);
      {$ELSE}
      Cdot2 := Subtract(Add(vB, b2Cross(wB, m_rB), Multiply(m_linearError, inv_h * m_correctionFactor)), Add(vA, b2Cross(wA, m_rA)));

      impulse2 := Negative(b2Mul(m_linearMass, Cdot2));
      oldImpulse2 := m_linearImpulse;
      AddBy(m_linearImpulse, impulse2);

      maxImpulse := h * m_maxForce;

      if SqrLength(m_linearImpulse) > maxImpulse * maxImpulse then
      begin
         Normalize(m_linearImpulse);
         MultiplyBy(m_linearImpulse, maxImpulse);
      end;

      impulse2 := Subtract(m_linearImpulse, oldImpulse2);

      SubtractBy(vA, Multiply(impulse2, mA));
      AddBy(vB, Multiply(impulse2, mB));
      {$ENDIF}
      wA := wA - iA * b2Cross(m_rA, impulse2);
      wB := wB + iB * b2Cross(m_rB, impulse2);
   end;

   data.velocities[m_indexA].v := vA;
   data.velocities[m_indexA].w := wA;
   data.velocities[m_indexB].v := vB;
   data.velocities[m_indexB].w := wB;
end;

function Tb2MotorJoint.SolvePositionConstraints(const data: Tb2SolverData): Boolean;
begin
   //B2_NOT_USED(data);
   Result := True;
end;

function Tb2MotorJoint.GetAnchorA: TVector2;
begin
   Result := m_bodyA.GetPosition;
end;

function Tb2MotorJoint.GetAnchorB: TVector2;
begin
   Result := m_bodyB.GetPosition;
end;

function Tb2MotorJoint.GetReactionForce(inv_dt: PhysicsFloat): TVector2;
begin
   {$IFDEF OP_OVERLOAD}
   Result := inv_dt * m_linearImpulse;
   {$ELSE}
   Result := Multiply(m_linearImpulse, inv_dt);
   {$ENDIF}
end;

function Tb2MotorJoint.GetReactionTorque(inv_dt: PhysicsFloat): PhysicsFloat;
begin
   Result := inv_dt * m_angularImpulse;
end;

procedure Tb2MotorJoint.SetLinearOffset(const linearOffset: TVector2);
begin
   if (linearOffset.x <> m_linearOffset.x) or (linearOffset.y <> m_linearOffset.y) then
   begin
      m_bodyA.SetAwake(True);
      m_bodyB.SetAwake(True);
      m_linearOffset := linearOffset;
   end;
end;

procedure Tb2MotorJoint.SetAngularOffset(angularOffset: PhysicsFloat);
begin
   if angularOffset <> m_angularOffset then
   begin
      m_bodyA.SetAwake(True);
      m_bodyB.SetAwake(True);
      m_angularOffset := angularOffset;
   end;
end;

{ Tb2RopeDef }

constructor Tb2RopeDef.Create;
begin
		count := 0;
		gravity := b2Vec2_Zero;
		damping := 0.1;
		k2 := 0.9;
		k3 := 0.1;
end;

{ Tb2Rope }

constructor Tb2Rope.Create(def: Tb2RopeDef; AutoFreeDef: Boolean = True);
var
   i: Integer;
   m: PhysicsFloat;
   count2, count3: Int32;
   d1, d2: TVector2;
begin
   //b2Assert(def.count >= 3);
   m_count := def.count;
   SetLength(m_ps, m_count);
   SetLength(m_p0s, m_count);
   SetLength(m_vs, m_count);
   SetLength(m_ims, m_count);

   for i := 0 to m_count - 1 do
   begin
      m_ps[i] := def.vertices[i];
      m_p0s[i] := def.vertices[i];
      m_vs[i] := b2Vec2_Zero;

      m := def.masses[i];
      if m > 0.0 then
         m_ims[i] := 1.0 / m
      else
         m_ims[i] := 0.0;
   end;

   count2 := m_count - 1;
   count3 := m_count - 2;
   SetLength(m_Ls, count2);
   SetLength(m_as, count3);

   for i := 0 to count2 - 1 do
      m_Ls[i] := UPhysics2DTypes.b2Distance(m_ps[i], m_ps[i+1]);

   for i := 0 to count3 - 1 do
   begin
      {$IFDEF OP_OVERLOAD}
      d1 := m_ps[i + 1] - m_ps[i];
      d2 := m_ps[i + 2] - m_ps[i + 1];
      {$ELSE}
      d1 := Subtract(m_ps[i + 1], m_ps[i]);
      d2 := Subtract(m_ps[i + 2], m_ps[i + 1]);
      {$ENDIF}

      m_as[i] := ArcTan2(b2Cross(d1, d2), b2Dot(d1, d2));
   end;

   m_gravity := def.gravity;
   m_damping := def.damping;
   m_k2 := def.k2;
   m_k3 := def.k3;

   if AutoFreeDef then
      def.Free
end;

procedure Tb2Rope.SolveC2;
var
   i: Integer;
   count2: Int32;
   p1, p2, d: TVector2;
   L, im1, im2, s1, s2: PhysicsFloat;
begin
   count2 := m_count - 1;

   for i := 0 to count2 - 1 do
   begin
      p1 := m_ps[i];
      p2 := m_ps[i + 1];

      {$IFDEF OP_OVERLOAD}
      d := p2 - p1;
      L := d.Normalize;
      {$ELSE}
      d := Subtract(p2, p1);
      L := Normalize(d);
      {$ENDIF}

      im1 := m_ims[i];
      im2 := m_ims[i + 1];

      if im1 + im2 = 0.0 then
         Continue;

      s1 := im1 / (im1 + im2);
      s2 := im2 / (im1 + im2);

      {$IFDEF OP_OVERLOAD}
      p1.SubtractBy(m_k2 * s1 * (m_Ls[i] - L) * d);
      p2.AddBy(m_k2 * s2 * (m_Ls[i] - L) * d);
      {$ELSE}
      SubtractBy(p1, Multiply(d, m_k2 * s1 * (m_Ls[i] - L)));
      AddBy(p2, Multiply(d, m_k2 * s2 * (m_Ls[i] - L)));
      {$ENDIF}

      m_ps[i] := p1;
      m_ps[i + 1] := p2;
   end;
end;

procedure Tb2Rope.SolveC3;
var
   i: Integer;
   count3: Int32;
   p1, p2, p3, d1, d2, Jd1, Jd2, J1, J2, J3: TVector2;
   m1, m2, m3, L1sqr, L2sqr, a, b, angle, mass, C, impulse: PhysicsFloat;
   twoPi: PhysicsFloat;
begin
   count3 := m_count - 2;
   twoPi := 2.0 * Pi;

   for i := 0 to count3 - 1 do
   begin
      p1 := m_ps[i];
      p2 := m_ps[i + 1];
      p3 := m_ps[i + 2];

      m1 := m_ims[i];
      m2 := m_ims[i + 1];
      m3 := m_ims[i + 2];

      {$IFDEF OP_OVERLOAD}
      d1 := p2 - p1;
      d2 := p3 - p2;
      L1sqr := d1.SqrLength;
      L2sqr := d2.SqrLength;
      {$ELSE}
      d1 := Subtract(p2, p1);
      d2 := Subtract(p3, p2);
      L1sqr := SqrLength(d1);
      L2sqr := SqrLength(d2);
      {$ENDIF}

      if L1sqr * L2sqr = 0.0 then
         Continue;

      a := b2Cross(d1, d2);
      b := b2Dot(d1, d2);

      angle := ArcTan2(a, b);

      {$IFDEF OP_OVERLOAD}
      Jd1 := (-1.0 / L1sqr) * d1.Skew;
      Jd2 := (1.0 / L2sqr) * d2.Skew;
      J1 := -Jd1;
      J2 := Jd1 - Jd2;
      {$ELSE}
      Jd1 := Multiply(Skew(d1), -1.0 / L1sqr);
      Jd2 := Multiply(Skew(d2), 1.0 / L2sqr);
      J1 := Negative(Jd1);
      J2 := Subtract(Jd1, Jd2);
      {$ENDIF}

      J3 := Jd2;

      mass := m1 * b2Dot(J1, J1) + m2 * b2Dot(J2, J2) + m3 * b2Dot(J3, J3);
      if mass = 0.0 then
         Continue;

      mass := 1.0 / mass;

      C := angle - m_as[i];

      while (C > Pi) do
      begin
         angle := angle - twoPi;
         C := angle - m_as[i];
      end;

      while (C < -Pi) do
      begin
         angle := angle + twoPi;
         C := angle - m_as[i];
      end;

      impulse := - m_k3 * mass * C;

      {$IFDEF OP_OVERLOAD}
      p1.AddBy((m1 * impulse) * J1);
      p2.AddBy((m2 * impulse) * J2);
      p3.AddBy((m3 * impulse) * J3);
      {$ELSE}
      AddBy(p1, Multiply(J1, m1 * impulse));
      AddBy(p2, Multiply(J2, m2 * impulse));
      AddBy(p3, Multiply(J3, m3 * impulse));
      {$ENDIF}

      m_ps[i] := p1;
      m_ps[i + 1] := p2;
      m_ps[i + 2] := p3;
   end;
end;

procedure Tb2Rope.Step(timeStep: PhysicsFloat; iterations: Int32);
var
   i: Integer;
   d, inv_time: PhysicsFloat;
begin
   if timeStep = 0.0 then
      Exit;

   d := Exp(-timeStep * m_damping);

   for i := 0 to m_count - 1 do
   begin
      m_p0s[i] := m_ps[i];
      {$IFDEF OP_OVERLOAD}
      if m_ims[i] > 0.0 then
         m_vs[i].AddBy(timeStep * m_gravity);

      m_vs[i].MultiplyBy(d);
      m_ps[i].AddBy(timeStep * m_vs[i]);
      {$ELSE}
      if m_ims[i] > 0.0 then
         AddBy(m_vs[i], Multiply(m_gravity, timeStep));

      MultiplyBy(m_vs[i], d);
      AddBy(m_ps[i], Multiply(m_vs[i], timeStep));
      {$ENDIF}
   end;

   for i := 0 to iterations - 1 do
   begin
      SolveC2;
      SolveC3;
      SolveC2;
   end;

   inv_time := 1.0 / timeStep;
   for i := 0 to m_count - 1 do
      {$IFDEF OP_OVERLOAD}
      m_vs[i] := inv_time * (m_ps[i] - m_p0s[i]);
      {$ELSE}
      m_vs[i] := Multiply(Subtract(m_ps[i], m_p0s[i]), inv_time);
      {$ENDIF}
end;

procedure Tb2Rope.Draw(draw: Tb2Draw);
var
   i: Integer;
   c: RGBA;
begin
   c := MakeColor(0.4, 0.5, 0.7);

   for i := 0 to m_count - 2 do
      draw.DrawSegment(m_ps[i], m_ps[i+1], c);
end;

procedure Tb2Rope.SetAngle(angle: PhysicsFloat);
var
   i: Integer;
   count3: Int32;
begin
   count3 := m_count - 2;
   for i := 0 to count3 - 1 do
      m_as[i] := angle;
end;

initialization
   b2_defaultFilter := Tb2ContactFilter.Create;
   b2_defaultListener := Tb2ContactListener.Create;

   b2_gjkCalls := 0;
   b2_gjkIters := 0;
   b2_gjkMaxIters := 0;

   growable_stack := Tb2GrowableStack.Create;
   world_query_wrapper := Tb2WorldQueryWrapper.Create;
   world_raycast_wrapper := Tb2WorldRayCastWrapper.Create;
   world_solve_island := Tb2Island.Create;
   world_solve_stack := TList.Create;
   position_solver_manifold := Tb2PositionSolverManifold.Create;
   distance_simplex := Tb2Simplex.Create;
   toi_separation_fcn := Tb2SeparationFunction.Create;
   island_solve_contact_solver := Tb2ContactSolver.Create;
   static_edge_shape := Tb2EdgeShape.Create;
   ep_collieder := Tb2EPCollider.Create;

   {$IFDEF COMPUTE_PHYSICS_TIME}
//   QueryPerformanceFrequency(vCounterFrequency);
   AStopwatch := TStopwatch.Create;
   {$ENDIF}

finalization
   b2_defaultFilter.Free;
   b2_defaultListener.Free;

   growable_stack.Free;
   world_query_wrapper.Free;
   world_raycast_wrapper.Free;
   world_solve_island.Free;
   world_solve_stack.Free;
   position_solver_manifold.Free;
   distance_simplex.Free;
   toi_separation_fcn.Free;
   island_solve_contact_solver.Free;
   static_edge_shape.Free;
   ep_collieder.Free;

end.

