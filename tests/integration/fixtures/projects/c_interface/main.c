#include "api.h"

int main(void) {
    struct Standalone standalone = {3};
    Renamed renamed = {4};
    struct Node node = {0, 5};
    struct Opaque* opaque = 0;
    Count count = 6;
    Mode mode = Done;
    JobState job = JobState_Done;
    FileState file = FileState_Done;
    struct Envelope envelope = {{8}, {9}, {10}};
    PublicHidden hidden = {12};
    Constructed constructed = {13};
    Values values = {1, 2, 3};
    Callback callback = (Handler*)0;
    if (chl_add(1, 2) != 10 || exported_constant != 11 || mode != 4 || Ready != 3) return 1;
    exported_count = 10;
    if (chl_add(1, 2) != 13) return 2;
    if (chl_hidden(envelope.hidden).value != 8) return 3;
    if (chl_imported(envelope.external).value != 9) return 4;
    if (hidden.value != 12 || constructed.value != 13 || values[2] != 3 || callback != 0) return 5;
    if (job != 10 || file != 20) return 6;
    return standalone.value + renamed.value + node.value + count != 18 || opaque != 0;
}
