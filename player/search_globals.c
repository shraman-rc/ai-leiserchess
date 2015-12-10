// Copyright (c) 2015 MIT License by 6.172 Staff

#if PARALLEL
#include "./simple_mutex.h"
static simple_mutex_t bmh_mutex;
static simple_mutex_t tt_mutex;
#endif

#define KILLERS_PER_PLY 4
#define __KMT_dim__ [MAX_PLY_IN_SEARCH*KILLERS_PER_PLY]  // NOLINT(whitespace/braces)
#define KMT(ply, id) (KILLERS_PER_PLY * ply + id)
static move_t killer __KMT_dim__;  // up to 4 killers

// Best move history table and lookup function
// Format: best_move_history[color_t][piece_t][square_t][orientation]
#define __BMH_dim__ [2*6*ARR_SIZE*NUM_ORI]  // NOLINT(whitespace/braces)
#define BMH(color, piece, square, ori)                             \
    (color * 6 * ARR_SIZE * NUM_ORI + piece * ARR_SIZE * NUM_ORI + \
     square * NUM_ORI + ori)

// always positive
typedef uint32_t best_move_history_t;
static best_move_history_t best_move_history __BMH_dim__;

void init_best_move_history() {
  memset(best_move_history, 0, sizeof(best_move_history));
  #if PARALLEL
  init_simple_mutex(&bmh_mutex);
  init_simple_mutex(&tt_mutex);
  #endif
}

static void update_best_move_history(position_t *p, int index_of_best,
                                     sortable_move_t* lst, int count) {
  #if PARALLEL
  simple_acquire(&bmh_mutex);
  #endif
  tbassert(ENABLE_TABLES, "Tables weren't enabled.\n");

  int color_to_move = color_to_move_of(p);

  for (int i = 0; i < count; i++) {
    move_t   mv  = get_move(lst[i]);
    ptype_t  pce = ptype_mv_of(mv);
    rot_t    ro  = rot_of(mv);  // rotation
    square_t fs  = from_square(mv);
    int      ot  = ORI_MASK & (ori_of(p->board[fs]) + ro);
    square_t ts  = to_square(mv);

    best_move_history_t s = best_move_history[BMH(color_to_move, pce, ts, ot)];

    if (index_of_best == i) {
      s = s + 11200;  // number will never exceed 1017
    }
    s = s * 0.90;  // decay score over time

    tbassert(s < 102000, "s = %d\n", s);  // or else sorting will fail

    best_move_history[BMH(color_to_move, pce, ts, ot)] = s;
  }
  #if PARALLEL
  simple_release(&bmh_mutex);
  #endif
}

static void update_transposition_table(searchNode* node) {
  #if PARALLEL
  simple_acquire(&tt_mutex);
  #endif

  if (node->type == SEARCH_SCOUT) {
    if (node->best_score < node->beta) {
      tt_hashtable_put(node->position.key, node->depth,
                       tt_adjust_score_for_hashtable(node->best_score, node->ply),
                       UPPER, 0);
    } else {
      tt_hashtable_put(node->position.key, node->depth,
                       tt_adjust_score_for_hashtable(node->best_score, node->ply),
                       LOWER, node->subpv[0]);
    }
  } else if (node->type == SEARCH_PV) {
    if (node->best_score <= node->orig_alpha) {
      tt_hashtable_put(node->position.key, node->depth,
          tt_adjust_score_for_hashtable(node->best_score, node->ply), UPPER, 0);
    } else if (node->best_score >= node->beta) {
      tt_hashtable_put(node->position.key, node->depth,
          tt_adjust_score_for_hashtable(node->best_score, node->ply), LOWER, node->subpv[0]);
    } else {
      tt_hashtable_put(node->position.key, node->depth,
          tt_adjust_score_for_hashtable(node->best_score, node->ply), EXACT, node->subpv[0]);
    }
  }
  #if PARALLEL
  simple_release(&tt_mutex);
  #endif
}
