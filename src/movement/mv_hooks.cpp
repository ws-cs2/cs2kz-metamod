#include "movement.h"
#include "utils/detours.h"

// TODO: CVAR creation waiting room
// HACK HACK
// kz stuff shouldn't even be used in here!
#include "kz/kz.h"
#include "kz/mode/kz_mode.h"

#include "tier0/memdbgon.h"

void movement::InitDetours()
{
	INIT_DETOUR(GetMaxSpeed);
	INIT_DETOUR(ProcessUsercmds);
	INIT_DETOUR(ProcessMovement);
	INIT_DETOUR(PlayerMoveNew);
	INIT_DETOUR(CheckParameters);
	INIT_DETOUR(CanMove);
	INIT_DETOUR(FullWalkMove);
	INIT_DETOUR(MoveInit);
	INIT_DETOUR(CheckWater);
	INIT_DETOUR(CheckVelocity);
	INIT_DETOUR(Duck);
	INIT_DETOUR(LadderMove);
	INIT_DETOUR(CheckJumpButton);
	INIT_DETOUR(OnJump);
	INIT_DETOUR(AirAccelerate);
	INIT_DETOUR(Friction);
	INIT_DETOUR(WalkMove);
	INIT_DETOUR(TryPlayerMove);
	INIT_DETOUR(CategorizePosition);
	INIT_DETOUR(FinishGravity);
	INIT_DETOUR(CheckFalling);
	INIT_DETOUR(PostPlayerMove);
	INIT_DETOUR(PostThink);
}

f32 FASTCALL movement::Detour_GetMaxSpeed(CCSPlayerPawn *pawn)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(pawn);
	f32 newMaxSpeed = player->GetPlayerMaxSpeed();
	
	if (newMaxSpeed <= 0.0f) return GetMaxSpeed(pawn);
	return newMaxSpeed;
}

i32 FASTCALL movement::Detour_ProcessUsercmds(CBasePlayerPawn *pawn, void *cmds, int numcmds, bool paused)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(pawn);
	player->OnProcessUsercmds(cmds, numcmds);
	auto retValue = ProcessUsercmds(pawn, cmds, numcmds, paused);
	player->OnProcessUsercmdsPost(cmds, numcmds);
	return retValue;
}

void FASTCALL movement::Detour_ProcessMovement(CCSPlayer_MovementServices *ms, CMoveData *mv)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(ms);
	player->currentMoveData = mv;
	player->moveDataPre = CMoveData(*mv);
	player->OnProcessMovement();
	ProcessMovement(ms, mv);
	player->moveDataPost = CMoveData(*mv);
	player->OnProcessMovementPost();
}

bool FASTCALL movement::Detour_PlayerMoveNew(CCSPlayer_MovementServices *ms, CMoveData *mv)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(ms);
	player->OnPlayerMove();
	auto retValue = PlayerMoveNew(ms, mv);
	player->OnPlayerMovePost();
	return retValue;
}

void FASTCALL movement::Detour_CheckParameters(CCSPlayer_MovementServices *ms, CMoveData *mv)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(ms);
	player->OnCheckParameters();
	CheckParameters(ms, mv);
	player->OnCheckParametersPost();
}

bool FASTCALL movement::Detour_CanMove(CCSPlayerPawnBase *pawn)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(pawn);
	player->OnCanMove();
	auto retValue = CanMove(pawn);
	player->OnCanMovePost();
	return retValue;
}

void FASTCALL movement::Detour_FullWalkMove(CCSPlayer_MovementServices *ms, CMoveData *mv, bool ground)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(ms);
	player->OnFullWalkMove(ground);
	FullWalkMove(ms, mv, ground);
	player->OnFullWalkMovePost(ground);
}

bool FASTCALL movement::Detour_MoveInit(CCSPlayer_MovementServices *ms, CMoveData *mv)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(ms);
	player->OnMoveInit();
	auto retValue = MoveInit(ms, mv);
	player->OnMoveInitPost();
	return retValue;
}

bool FASTCALL movement::Detour_CheckWater(CCSPlayer_MovementServices *ms, CMoveData *mv)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(ms);
	player->OnCheckWater();
	auto retValue = CheckWater(ms, mv);
	player->OnCheckWaterPost();
	return retValue;
}

void FASTCALL movement::Detour_CheckVelocity(CCSPlayer_MovementServices *ms, CMoveData *mv, const char *a3)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(ms);
	player->OnCheckVelocity(a3);
	CheckVelocity(ms, mv, a3);
	player->OnCheckVelocityPost(a3);
}

void FASTCALL movement::Detour_Duck(CCSPlayer_MovementServices *ms, CMoveData *mv)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(ms);
	player->OnDuck();
	player->processingDuck = true;
	Duck(ms, mv);
	player->processingDuck = false;
	player->OnDuckPost();
}

bool FASTCALL movement::Detour_LadderMove(CCSPlayer_MovementServices *ms, CMoveData *mv)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(ms);
	player->OnLadderMove();
	Vector oldVelocity = mv->m_vecVelocity;
	MoveType_t oldMoveType = player->GetPawn()->m_MoveType();
	bool result = LadderMove(ms, mv);
	if (player->GetPawn()->m_lifeState() != LIFE_DEAD && !result && oldMoveType == MOVETYPE_LADDER)
	{
		// Do the setting part ourselves as well.
		utils::SetEntityMoveType(player->GetPawn(), MOVETYPE_WALK);
	}
	if (!result && oldMoveType == MOVETYPE_LADDER)
	{
		player->RegisterTakeoff(false);
		player->takeoffFromLadder = true;
		// Ladderjump takeoff detection is delayed by one process movement call, we have to use the previous origin.
		player->takeoffOrigin = player->lastValidLadderOrigin;
		player->takeoffGroundOrigin = player->lastValidLadderOrigin;
		player->OnChangeMoveType(MOVETYPE_LADDER);
	}
	else if (result && oldMoveType != MOVETYPE_LADDER && player->GetPawn()->m_MoveType() == MOVETYPE_LADDER)
	{
		player->RegisterLanding(oldVelocity, false);
		player->OnChangeMoveType(MOVETYPE_WALK);
	}
	else if (result && oldMoveType == MOVETYPE_LADDER && player->GetPawn()->m_MoveType() == MOVETYPE_WALK)
	{
		// Player is on the ladder, pressing jump pushes them away from the ladder.
		float curtime = g_pKZUtils->GetServerGlobals()->curtime;
		player->RegisterTakeoff(player->IsButtonDown(IN_JUMP));
		player->takeoffFromLadder = true;
		player->OnChangeMoveType(MOVETYPE_LADDER);
	}

	if (result && player->GetPawn()->m_MoveType() == MOVETYPE_LADDER)
	{
		player->GetOrigin(&player->lastValidLadderOrigin);
	}
	player->OnLadderMovePost();
	return result;
}

void FASTCALL movement::Detour_CheckJumpButton(CCSPlayer_MovementServices *ms, CMoveData *mv)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(ms);
	player->OnCheckJumpButton();
	CheckJumpButton(ms, mv);
	player->OnCheckJumpButtonPost();
}

void FASTCALL movement::Detour_OnJump(CCSPlayer_MovementServices *ms, CMoveData *mv)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(ms);
	player->OnJump();
	f32 oldJumpUntil = ms->m_flJumpUntil();
	MoveType_t oldMoveType = player->GetPawn()->m_MoveType();
	OnJump(ms, mv);
	if (ms->m_flJumpUntil() != oldJumpUntil)
	{
		player->hitPerf = (oldMoveType != MOVETYPE_LADDER && !player->oldWalkMoved);
		player->RegisterTakeoff(true);
		player->OnStopTouchGround();
	}
	player->OnJumpPost();
}

void FASTCALL movement::Detour_AirAccelerate(CCSPlayer_MovementServices *ms, CMoveData *mv, Vector &wishdir, f32 wishspeed, f32 accel)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(ms);
	player->OnAirAccelerate(wishdir, wishspeed, accel);
	AirAccelerate(ms, mv, wishdir, wishspeed, accel);
	player->OnAirAcceleratePost(wishdir, wishspeed, accel);
}

void FASTCALL movement::Detour_Friction(CCSPlayer_MovementServices *ms, CMoveData *mv)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(ms);
	player->OnFriction();
	Friction(ms, mv);
	player->OnFrictionPost();
}

void FASTCALL movement::Detour_WalkMove(CCSPlayer_MovementServices *ms, CMoveData *mv)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(ms);
	player->OnWalkMove();
	WalkMove(ms, mv);
	player->walkMoved = true;
	player->OnWalkMovePost();
}

internal void ClipVelocity_Custom(Vector &in, Vector &normal, Vector &out, f32 overbounce)
{
	// Determine how far along plane to slide based on incoming direction.
	f32 backoff = DotProduct(in, normal) * overbounce;
	
	for (i32 i = 0; i < 3; i++)
	{
		f32 change = normal[i] * backoff;
		out[i] = in[i] - change;
	}
	
	// Rampbug/wallbug fix: always move a little bit away from the plane
	float adjust = -0.5f;
	out -= (normal * adjust);
}

void TracePlayerBBox_Custom(const Vector &start, const Vector &end, const bbox_t &bounds, CTraceFilterPlayerMovementCS *filter, trace_t_s2 &pm)
{
	utils::TracePlayerBBox(start, end, bounds, filter, pm);
	
	trace_t_s2 test;
	
	Vector direction = end - start;
	f32 totalDistance = VectorNormalize(direction);
	
	f32 dotA = direction.Dot(pm.planeNormal);
	
	if (pm.fraction < 1 && dotA < -0.25f)
	{
		// TODO: this will be 0 if direction is perpendicular to (0,0,1)
		Vector perp1 = CrossProduct(direction, Vector(0, 0, 1));
		Vector perp2 = CrossProduct(direction, perp1);
		VectorNormalize(perp1);
		VectorNormalize(perp2);
		Vector perp3 = -perp1;
		Vector perp4 = -perp2;
		
		trace_t_s2 tr[4];
		utils::TracePlayerBBox(pm.endpos, pm.endpos + perp1, bounds, filter, tr[0]);
		utils::TracePlayerBBox(pm.endpos, pm.endpos + perp2, bounds, filter, tr[1]);
		utils::TracePlayerBBox(pm.endpos, pm.endpos + perp3, bounds, filter, tr[2]);
		utils::TracePlayerBBox(pm.endpos, pm.endpos + perp4, bounds, filter, tr[3]);
		
		i32 closest = -1;
		i32 closestFrac = 2.0f;
		for (i32 i = 0; i < 4; i++)
		{
			if (tr[i].fraction < closestFrac && tr[i].fraction != 1.0)
			{
				closest = i;
				closestFrac = tr[i].fraction;
			}
		}
		
		if (closest != -1)
		{
			f32 originalFrac = pm.fraction;
			Vector offset = tr[closest].planeNormal * 0.0625f;
			utils::TracePlayerBBox(
				pm.endpos + offset,
				end + offset, bounds, filter, test
			);
			if (test.fraction != 0)
			{
				pm = test;
				pm.startpos = start;
				// pm.endpos -= offset;
				pm.fraction = originalFrac + (1.0 - originalFrac) * pm.fraction;
			}
		}
	}
#if 0
	if (pm.fraction < 1 && dotA < -0.25f)
	{
		Vector normal = pm.planeNormal;
		if (dotA > -0.9999f)
		{
			normal = CrossProduct(pm.planeNormal, direction);
			VectorNormalize(normal);
		}
		else
		{
			// for breakpoint
			normal = pm.planeNormal;
		}
		Vector pos1 = pm.endpos + (normal * 0.03125f);
		Vector pos2 = pm.endpos - (normal * 0.03125f);
		utils::TracePlayerBBox(pos1, pos2, bounds, filter, test);
		if (test.startsolid)
		{
			utils::TracePlayerBBox(pos2, pos1, bounds, filter, test);
		}
		if (test.fraction < 1 && !test.startsolid)
		{
			f32 dot = direction.Dot(test.planeNormal);
			if (dot >= -0.25f)
			{
				// Msg("Success! a %f b %f\n", dotA, dot);
				pm.planeNormal = test.planeNormal;
			}
			else
			{
				// Msg("Fail :(! a %f b %f\n", dotA, dot);
			}
		}
	}
	
	// attempt to travel the rest of the distance if we're stuck somewhere or stuck on an edge
	for (i32 i = 0; i < 4; i++)
	{
		utils::TracePlayerBBox(pm.endpos, pm.endpos, bounds, filter, test);
		if (pm.fraction != 1.0 && test.startsolid)
		{
			utils::TracePlayerBBox(pm.endpos + (test.planeNormal * 0.03125f), end, bounds, filter, test);
			if (test.fraction)
			{
				pm.endpos = test.endpos;
				if (totalDistance > 0.0)
				{
					pm.fraction = VectorLength(test.endpos - start) / totalDistance;
					pm.endpos = test.endpos;
					// pm.planeNormal = test.planeNormal;
				}
			}
			else
			{
				break;
			}
		}
	}
#endif
}

#define	MAX_CLIP_PLANES	4
#define GM_MV_OPTIMISATIONS 1
// From https://github.com/ValveSoftware/source-sdk-2013/blob/master/mp/src/game/shared/gamemovement.cpp#L2560
internal void TryPlayerMove_Custom(CCSPlayer_MovementServices *ms, CMoveData *mv, Vector *pFirstDest, trace_t_s2 *pFirstTrace)
{
	int			bumpcount, numbumps;
	Vector		dir;
	float		d;
	int			numplanes;
	Vector		planes[MAX_CLIP_PLANES];
	Vector		primal_velocity, original_velocity;
	Vector      new_velocity;
	int			i, j;
	trace_t_s2	pm;
	Vector		end;
	float		time_left, allFraction;
	int			blocked;		
	
	numbumps  = 4;           // Bump up to four times
	
	blocked   = 0;           // Assume not blocked
	numplanes = 0;           //  and not sliding along any planes
	
	VectorCopy (mv->m_vecVelocity, original_velocity);  // Store original velocity
	VectorCopy (mv->m_vecVelocity, primal_velocity);
	
	allFraction = 0;
	gpGlobals = g_pKZUtils->GetServerGlobals();
	time_left = gpGlobals->frametime;   // Total time for this movement operation.
	
	new_velocity.Init();
	
	CCSPlayerPawn *player = g_pPlayerManager->ToPlayer(ms)->GetPawn();
	
	CTraceFilterPlayerMovementCS filter;
	utils::InitPlayerMovementTraceFilter(filter, player, player->m_Collision().m_collisionAttribute().m_nInteractsWith(), COLLISION_GROUP_PLAYER_MOVEMENT);
	
	bbox_t bounds;
	bounds.mins = { -16, -16, 0 };
	bounds.maxs = { 16, 16, 72 };
	
	if (ms->m_bDucked())
	{
		bounds.maxs.z = 54;
	}
	
	for (bumpcount=0 ; bumpcount < numbumps; bumpcount++)
	{
		if ( mv->m_vecVelocity.Length() == 0.0 )
			break;
		
		// Assume we can move all the way from the current origin to the
		//  end point.
		VectorMA( mv->m_vecAbsOrigin, time_left, mv->m_vecVelocity, end );
		
		// See if we can make it from origin to end point.
		if ( GM_MV_OPTIMISATIONS )
		{
			// If their velocity Z is 0, then we can avoid an extra trace here during WalkMove.
			if ( pFirstDest && end == *pFirstDest )
				pm = *pFirstTrace;
			else
			{
				TracePlayerBBox_Custom( mv->m_vecAbsOrigin, end, bounds, &filter, pm );
			}
		}
		else
		{
			TracePlayerBBox_Custom( mv->m_vecAbsOrigin, end, bounds, &filter, pm );
		}
		
		allFraction += pm.fraction;
		
		// If we started in a solid object, or we were in solid space
		//  the whole way, zero out our velocity and return that we
		//  are blocked by floor and wall.
		trace_t_s2 stuckTest;
		TracePlayerBBox_Custom( end, mv->m_vecAbsOrigin, bounds, &filter, stuckTest );
		if (pm.startsolid && pm.fraction == 0 && stuckTest.startsolid && stuckTest.fraction == 0)
		{
			// entity is trapped in another solid
			// VectorCopy (vec3_origin, mv->m_vecVelocity);
			// return;
		}
		
		// If we moved some portion of the total distance, then
		//  copy the end position into the pmove.origin and 
		//  zero the plane counter.
		if( pm.fraction > 0 )
		{
			if ( numbumps > 0 && pm.fraction == 1 )
			{
				// There's a precision issue with terrain tracing that can cause a swept box to successfully trace
				// when the end position is stuck in the triangle.  Re-run the test with an uswept box to catch that
				// case until the bug is fixed.
				// If we detect getting stuck, don't allow the movement
				trace_t_s2 stuck;
				TracePlayerBBox_Custom( pm.endpos, pm.endpos, bounds, &filter, stuck );
				if ( stuck.startsolid || stuck.fraction != 1.0f )
				{
					//Msg( "Player will become stuck!!!\n" );
					VectorCopy (vec3_origin, mv->m_vecVelocity);
					break;
				}
			}
			
			// actually covered some distance
			mv->m_vecAbsOrigin = pm.endpos;
			VectorCopy (mv->m_vecVelocity, original_velocity);
			numplanes = 0;
		}
		
		// If we covered the entire distance, we are done
		//  and can return.
		if (pm.fraction == 1)
		{
			break;		// moved the entire distance
		}
		
		// Save entity that blocked us (since fraction was < 1.0)
		//  for contact
		// Add it if it's not already in the list!!!
		// TODO:?
		//MoveHelper( )->AddToTouched( pm, mv->m_vecVelocity );
		
		// If the plane we hit has a high z component in the normal, then
		//  it's probably a floor
		if (pm.planeNormal[2] > 0.7)
		{
			blocked |= 1;		// floor
		}
		// If the plane has a zero z component in the normal, then it's a 
		//  step or wall
		if (!pm.planeNormal[2])
		{
			blocked |= 2;		// step / wall
		}
		
		// Reduce amount of m_flFrameTime left by total time left * fraction
		//  that we covered.
		time_left -= time_left * pm.fraction;
		
		// Did we run out of planes to clip against?
		if (numplanes >= MAX_CLIP_PLANES)
		{	
			// this shouldn't really happen
			//  Stop our movement if so.
			VectorCopy (vec3_origin, mv->m_vecVelocity);
			//Con_DPrintf("Too many planes 4\n");
			
			break;
		}
		
		// Set up next clipping plane
		VectorCopy (pm.planeNormal, planes[numplanes]);
		numplanes++;
		
		// modify original_velocity so it parallels all of the clip planes
		//
		
		// reflect player velocity 
		// Only give this a try for first impact plane because you can get yourself stuck in an acute corner by jumping in place
		//  and pressing forward and nobody was really using this bounce/reflection feature anyway...
		if ( numplanes == 1 &&
			player->m_MoveType() == MOVETYPE_WALK &&
			player->m_hGroundEntity() == NULL )	
		{
			for ( i = 0; i < numplanes; i++ )
			{
				if ( planes[i][2] > 0.7  )
				{
					// floor or slope
					ClipVelocity_Custom( original_velocity, planes[i], new_velocity, 1 );
					VectorCopy( new_velocity, original_velocity );
				}
				else
				{
					// TODO: m_flSurfaceFriction and maybe sv_bounce
					ClipVelocity_Custom( original_velocity, planes[i], new_velocity, 1.0);
				}
			}
			
			VectorCopy( new_velocity, mv->m_vecVelocity );
			VectorCopy( new_velocity, original_velocity );
		}
		else
		{
			for (i=0 ; i < numplanes ; i++)
			{
				ClipVelocity_Custom (
							  original_velocity,
							  planes[i],
							  mv->m_vecVelocity,
							  1);
				
				for (j=0 ; j<numplanes ; j++)
					if (j != i)
				{
					// Are we now moving against this plane?
					if (mv->m_vecVelocity.Dot(planes[j]) < 0)
						break;	// not ok
				}
				if (j == numplanes)  // Didn't have to clip, so we're ok
					break;
			}
			
			// Did we go all the way through plane set
			if (i != numplanes)
			{	// go along this plane
				// pmove.velocity is set in clipping call, no need to set again.
				;  
			}
			else
			{	// go along the crease
				if (numplanes != 2)
				{
					VectorCopy (vec3_origin, mv->m_vecVelocity);
					break;
				}
				CrossProduct (planes[0], planes[1], dir);
				dir.NormalizeInPlace();
				d = dir.Dot(mv->m_vecVelocity);
				VectorScale (dir, d, mv->m_vecVelocity );
			}
			
			//
			// if original velocity is against the original velocity, stop dead
			// to avoid tiny occilations in sloping corners
			//
			d = mv->m_vecVelocity.Dot(primal_velocity);
			if (d <= 0)
			{
				//Con_DPrintf("Back\n");
				VectorCopy (vec3_origin, mv->m_vecVelocity);
				break;
			}
		}
	}
	
	if ( allFraction == 0 )
	{
		VectorCopy (vec3_origin, mv->m_vecVelocity);
	}
	
#if 0
	// Check if they slammed into a wall
	float fSlamVol = 0.0f;
	
	float fLateralStoppingAmount = primal_velocity.Length2D() - mv->m_vecVelocity.Length2D();
	if ( fLateralStoppingAmount > PLAYER_MAX_SAFE_FALL_SPEED * 2.0f )
	{
		fSlamVol = 1.0f;
	}
	else if ( fLateralStoppingAmount > PLAYER_MAX_SAFE_FALL_SPEED )
	{
		fSlamVol = 0.85f;
	}
	
	PlayerRoughLandingEffects( fSlamVol );
	
	return blocked;
#endif
}

void FASTCALL movement::Detour_TryPlayerMove(CCSPlayer_MovementServices *ms, CMoveData *mv, Vector *pFirstDest, trace_t_s2 *pFirstTrace)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(ms);
	player->OnTryPlayerMove(pFirstDest, pFirstTrace);
	
	// TODO: CVAR creation waiting room
	// HACK HACK
	// kz stuff shouldn't even be used in here!
	KZPlayer *kzPlayer = g_pKZPlayerManager->ToPlayer(ms);
	const char *mv_rampbug_fixValue = kzPlayer->modeService->GetModeConVarValues()[0];
	if (mv_rampbug_fixValue[0] == '0')
	{
		TryPlayerMove(ms, mv, pFirstDest, pFirstTrace);
	}
	else
	{
		TryPlayerMove_Custom(ms, mv, pFirstDest, pFirstTrace);
	}
	player->OnTryPlayerMovePost(pFirstDest, pFirstTrace);
}

void FASTCALL movement::Detour_CategorizePosition(CCSPlayer_MovementServices *ms, CMoveData *mv, bool bStayOnGround)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(ms);
	player->OnCategorizePosition(bStayOnGround);
	Vector oldVelocity = mv->m_vecVelocity;
	bool oldOnGround = !!(player->GetPawn()->m_fFlags() & FL_ONGROUND);
	
	CategorizePosition(ms, mv, bStayOnGround);

	bool ground = !!(player->GetPawn()->m_fFlags() & FL_ONGROUND);

	if (oldOnGround != ground)
	{
		if (ground)
		{
			player->RegisterLanding(oldVelocity);
			player->duckBugged = player->processingDuck;
			player->OnStartTouchGround();
		}
		else
		{
			player->RegisterTakeoff(false);
			player->OnStopTouchGround();
		}
	}
	player->OnCategorizePositionPost(bStayOnGround);
}

void FASTCALL movement::Detour_FinishGravity(CCSPlayer_MovementServices *ms, CMoveData *mv)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(ms);
	player->OnFinishGravity();
	FinishGravity(ms, mv);
	player->OnFinishGravityPost();
}

void FASTCALL movement::Detour_CheckFalling(CCSPlayer_MovementServices *ms, CMoveData *mv)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(ms);
	player->OnCheckFalling();
	CheckFalling(ms, mv);
	player->OnCheckFallingPost();
}

void FASTCALL movement::Detour_PostPlayerMove(CCSPlayer_MovementServices *ms, CMoveData *mv)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(ms);
	player->OnPostPlayerMove();
	PostPlayerMove(ms, mv);
	player->OnPostPlayerMovePost();
}

void FASTCALL movement::Detour_PostThink(CCSPlayerPawnBase *pawn)
{
	MovementPlayer *player = g_pPlayerManager->ToPlayer(pawn);
	player->OnPostThink();
	PostThink(pawn);
	player->OnPostThinkPost();
}
