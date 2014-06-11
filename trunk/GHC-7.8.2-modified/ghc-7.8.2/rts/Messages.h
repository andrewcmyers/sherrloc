/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2010
 *
 * Inter-Capability message passing
 *
 * --------------------------------------------------------------------------*/

#include "BeginPrivate.h"

nat messageBlackHole(Capability *cap, MessageBlackHole *msg);
StgTSO * blackHoleOwner (StgClosure *bh);

#ifdef THREADED_RTS
void executeMessage (Capability *cap, Message *m);
void sendMessage    (Capability *from_cap, Capability *to_cap, Message *msg);
#endif

#include "Capability.h"
#include "Updates.h" // for DEBUG_FILL_SLOP

INLINE_HEADER void
doneWithMsgThrowTo (MessageThrowTo *m)
{
    OVERWRITING_CLOSURE((StgClosure*)m);
    unlockClosure((StgClosure*)m, &stg_MSG_NULL_info);
    LDV_RECORD_CREATE(m);
}

#include "EndPrivate.h"
