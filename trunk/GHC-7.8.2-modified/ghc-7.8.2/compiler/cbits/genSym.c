
#include "Rts.h"

static HsInt GenSymCounter = 0;

HsInt genSym(void) {
#if defined(THREADED_RTS)
    if (n_capabilities == 1) {
        return GenSymCounter++;
    } else {
        return atomic_inc((StgWord *)&GenSymCounter, 1);
    }
#else
    return GenSymCounter++;
#endif
}

