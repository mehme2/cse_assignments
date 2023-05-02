#define FEATURE_NAME_MAX_SIZE 15

int featureCount(const char filename[]);

int featureName(const char filename[], int featureNo, char out[]);

int featureNo(const char filename[], const char featureName[]);

int featureGet(const char filename[], int entry, int featureNo, char out[]);

int featureSet(const char filename[], int entry, int featureNo, const char in[]);

int featureAdd(const char filename[], const char featureName[], const char defVal[]);

int entryCount(const char filename[]);

int entryAdd(const char filename[], const char defVal[]);

int entryDelete(const char filename[], int n);

int entryGetNext(const char filename[], int start, int featureNo, const char featureVal[]);

int fileCreate(const char filename[], const char feature0[]);
