
typedef struct wrapper_stats_s *wrapper_stats_t;

typedef struct wrapper_stats_s {
  void *wrapper_address;
  int  usage_count;
  int  usage_size;
} wrapper_stats_s;
