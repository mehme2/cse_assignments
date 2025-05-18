#ifndef _BANK_SERVER_H
#define _BANK_SERVER_H

#define SYNC_SHM_NAME "/220104004051sync.shm"
#define CLIENT_SHM_NAME "/220104004051client.shm"

extern int terminate;

extern const char * bankName;

void releaseServerResources();

#endif // _BANK_SERVER_H
