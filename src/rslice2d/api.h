#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
  double p1_1;
  double p1_2;
  double p2_1;
  double p2_2;
} SliceSeg;

SliceSeg* r_spi(double*, int32_t, int32_t,
                double*, size_t,
                int32_t, int32_t);

#ifdef __cplusplus
}
#endif
