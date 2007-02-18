#define UNIX_IMPL !defined(mingw32_HOST_OS)

#if UNIX_IMPL
#include "HsUnix.h"
#include <sys/types.h>

unsigned int unix_major(dev_t dev);
unsigned int unix_minor(dev_t dev);
dev_t unix_makedev(unsigned int maj, unsigned int min);

#define NEED_setSymbolicLinkOwnerAndGroup !HAVE_LCHOWN

#endif

