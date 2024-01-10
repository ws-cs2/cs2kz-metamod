#pragma once
#include "cdetour.h"
#include "irecipientfilter.h"
#include "datatypes.h"
#include "utils.h"
#include "movement/movement.h"
#include "sns.h"

void FASTCALL Detour_CBaseTrigger_StartTouch(CBaseTrigger *this_, CBaseEntity2 *pOther);
extern CDetour<decltype(Detour_CBaseTrigger_StartTouch)> CBaseTrigger_StartTouch;

void FASTCALL Detour_CBaseTrigger_EndTouch(CBaseTrigger *this_, CBaseEntity2 *pOther);
extern CDetour<decltype(Detour_CBaseTrigger_EndTouch)> CBaseTrigger_EndTouch;

int FASTCALL Detour_RecvServerBrowserPacket(RecvPktInfo_t &info, void* pSock);
extern CDetour<decltype(Detour_RecvServerBrowserPacket)> RecvServerBrowserPacket;

void FASTCALL Detour_CCSPP_Teleport(CCSPlayerPawn *this_, const Vector *newPosition, const QAngle *newAngles, const Vector *newVelocity);
extern CDetour<decltype(Detour_CCSPP_Teleport)> CCSPP_Teleport;

class CResourceStreamFixed;
internal i32 *Detour_RnMeshCreate(i32 a1, const u32 *a2, i64 a3, i32 a4, const f32 *a5, CResourceStreamFixed *a6, i32 *a7);
extern CDetour<decltype(Detour_RnMeshCreate)> RnMeshCreate;

internal i64 Detour_BuildMeshWings(u32 *a1, i32 *a2, i32 *a3);
extern CDetour<decltype(Detour_BuildMeshWings)> BuildMeshWings;

DECLARE_MOVEMENT_EXTERN_DETOUR(ProcessUsercmds);
DECLARE_MOVEMENT_EXTERN_DETOUR(GetMaxSpeed);
DECLARE_MOVEMENT_EXTERN_DETOUR(ProcessMovement);
DECLARE_MOVEMENT_EXTERN_DETOUR(PlayerMoveNew);
DECLARE_MOVEMENT_EXTERN_DETOUR(CheckParameters);
DECLARE_MOVEMENT_EXTERN_DETOUR(CanMove);
DECLARE_MOVEMENT_EXTERN_DETOUR(FullWalkMove);
DECLARE_MOVEMENT_EXTERN_DETOUR(MoveInit);
DECLARE_MOVEMENT_EXTERN_DETOUR(CheckWater);
DECLARE_MOVEMENT_EXTERN_DETOUR(WaterMove);
DECLARE_MOVEMENT_EXTERN_DETOUR(CheckVelocity);
DECLARE_MOVEMENT_EXTERN_DETOUR(Duck);
DECLARE_MOVEMENT_EXTERN_DETOUR(CanUnduck);
DECLARE_MOVEMENT_EXTERN_DETOUR(LadderMove);
DECLARE_MOVEMENT_EXTERN_DETOUR(CheckJumpButton);
DECLARE_MOVEMENT_EXTERN_DETOUR(OnJump);
DECLARE_MOVEMENT_EXTERN_DETOUR(AirAccelerate);
DECLARE_MOVEMENT_EXTERN_DETOUR(Friction);
DECLARE_MOVEMENT_EXTERN_DETOUR(WalkMove);
DECLARE_MOVEMENT_EXTERN_DETOUR(TryPlayerMove);
DECLARE_MOVEMENT_EXTERN_DETOUR(CategorizePosition);
DECLARE_MOVEMENT_EXTERN_DETOUR(FinishGravity);
DECLARE_MOVEMENT_EXTERN_DETOUR(CheckFalling);
DECLARE_MOVEMENT_EXTERN_DETOUR(PostPlayerMove);
DECLARE_MOVEMENT_EXTERN_DETOUR(PostThink);

void InitDetours();
void FlushAllDetours();
