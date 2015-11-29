// Copyright (c) 2015 MIT License by 6.172 Staff

#include "./eval.h"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "./tbassert.h"

// -----------------------------------------------------------------------------
// Evaluation
// -----------------------------------------------------------------------------

typedef int32_t ev_score_t;  // Static evaluator uses "hi res" values

int RANDOMIZE;

int PCENTRAL;
int HATTACK;
int PBETWEEN;
int KFACE;
int KAGGRESSIVE;
int MOBILITY;
int PAWNPIN;

// Heuristics for static evaluation - described in the google doc
// mentioned in the handout.


// PCENTRAL heuristic: Bonus for Pawn near center of board
// double df = BOARD_WIDTH/2 - f - 1;
// if (df < 0)  df = f - BOARD_WIDTH/2;
// double dr = BOARD_WIDTH/2 - r - 1;
// if (dr < 0) dr = r - BOARD_WIDTH/2;
// double bonus = 1 - sqrt(df * df + dr * dr) / (BOARD_WIDTH / sqrt(2));
// return PCENTRAL * bonus;
static const ev_score_t pcentral_values[BOARD_WIDTH][BOARD_WIDTH] = {
  {199, 292, 367, 416, 434, 434, 416, 367, 292, 199},
  {292, 400, 490, 552, 575, 575, 552, 490, 400, 292},
  {367, 490, 599, 683, 717, 717, 683, 599, 490, 367},
  {416, 552, 683, 799, 858, 858, 799, 683, 552, 416},
  {434, 575, 717, 858, 1000, 1000, 858, 717, 575, 434},
  {434, 575, 717, 858, 1000, 1000, 858, 717, 575, 434},
  {416, 552, 683, 799, 858, 858, 799, 683, 552, 416},
  {367, 490, 599, 683, 717, 717, 683, 599, 490, 367},
  {292, 400, 490, 552, 575, 575, 552, 490, 400, 292},
  {199, 292, 367, 416, 434, 434, 416, 367, 292, 199}
};

ev_score_t pcentral(fil_t f, rnk_t r) {
  return pcentral_values[f][r];
}


// returns true if c lies on or between a and b, which are not ordered
bool between(int c, int a, int b) {
  bool x = ((c >= a) && (c <= b)) || ((c <= a) && (c >= b));
  return x;
}

// PBETWEEN heuristic: Bonus for Pawn at (f, r) in rectangle defined by Kings at the corners
int pbetween(fil_t f, rnk_t r, fil_t w_f, rnk_t w_r, fil_t b_f, rnk_t b_r) {
  bool is_between =
      between(f, w_f, b_f) &&
      between(r, w_r, b_r);

  return is_between;
}

// KFACE heuristic: bonus (or penalty) for King facing toward the other King
ev_score_t kface(fil_t f, rnk_t r, fil_t o_f, rnk_t o_r, int ori) {
  int delta_fil = o_f - f;
  int delta_rnk = o_r - r;
  int bonus;

  switch (ori) {
    case NN:
      bonus = delta_rnk;
      break;

    case EE:
      bonus = delta_fil;
      break;

    case SS:
      bonus = -delta_rnk;
      break;

    case WW:
      bonus = -delta_fil;
      break;

    default:
      bonus = 0;
      tbassert(false, "Illegal King orientation.\n");
  }

  return (bonus * KFACE) / (abs(delta_rnk) + abs(delta_fil));
}

// KAGGRESSIVE heuristic: bonus for King with more space to back
ev_score_t kaggressive(fil_t f, rnk_t r, fil_t o_f, rnk_t o_r) {
  int delta_fil = o_f - f;
  int delta_rnk = o_r - r;

  int bonus = 0;

  if (delta_fil >= 0 && delta_rnk >= 0) {
    bonus = (f + 1) * (r + 1);
  } else if (delta_fil <= 0 && delta_rnk >= 0) {
    bonus = (BOARD_WIDTH - f) * (r + 1);
  } else if (delta_fil <= 0 && delta_rnk <= 0) {
    bonus = (BOARD_WIDTH - f) * (BOARD_WIDTH - r);
  } else if (delta_fil >= 0 && delta_rnk <= 0) {
    bonus = (f + 1) * (BOARD_WIDTH - r);
  }

  return (KAGGRESSIVE * bonus) / (BOARD_WIDTH * BOARD_WIDTH);
}

// Marks the path of the laser until it hits a piece or goes off the board.
//
// p : current board state
// laser_map : end result will be stored here. Every square on the
//             path of the laser is marked with mark_mask
// c : color of king shooting laser
// marks all hit squares with true
// NOTE: most of this code is duplicated in compute_all_laser_path_heuristics

// NOTE: now ONLY MARKS PAWNS
void mark_laser_path_pinned_pawns(position_t *p, bool *laser_map, color_t c) {
  // Fire laser, recording in laser_map
  square_t sq = p->kloc[c];
  int8_t bdir = ori_of(p->board[sq]);

  tbassert(ptype_of(p->board[sq]) == KING,
           "ptype: %d\n", ptype_of(p->board[sq]));

  while (true) {
    sq += beam_of(bdir);
    tbassert(sq < ARR_SIZE && sq >= 0, "sq: %d\n", sq);

    switch (ptype_of(p->board[sq])) {
      case EMPTY:  // empty square
        break;
      case PAWN:  // Pawn
        laser_map[sq] = true;
        bdir = reflect_of(bdir, ori_of(p->board[sq]));
        if (bdir < 0) {  // Hit back of Pawn
          return;
        }
        break;
      case KING:  // King
        return;  // sorry, game over my friend!
        break;
      case INVALID:  // Ran off edge of board
        return;
        break;
      default:  // Shouldna happen, man!
        tbassert(false, "Not cool, man.  Not cool.\n");
        break;
    }
  }
}

#define HDV_1(i) (1.0/((i) + 1))
#define HDV_3(i) HDV_1(i), HDV_1(i+1), HDV_1(i+2),
static const float h_dist_values[BOARD_WIDTH] = {
  HDV_3(0) HDV_3(3) HDV_3(6) HDV_1(9)
};

// Harmonic-ish distance: 1/(|dx|+1) + 1/(|dy|+1)
float h_dist(square_t a, fil_t f, rnk_t r) {
  //  printf("a = %d, FIL(a) = %d, RNK(a) = %d\n", a, FIL(a), RNK(a));
  //  printf("b = %d, FIL(b) = %d, RNK(b) = %d\n", b, FIL(b), RNK(b));
  int delta_fil = abs(fil_of(a) - f);
  int delta_rnk = abs(rnk_of(a) - r);
  return h_dist_values[delta_fil] + h_dist_values[delta_rnk];
}

#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))

// PAWNPIN Heuristic --- is a pawn immobilized by the enemy laser.
// H_SQUARES_ATTACKABLE heuristic: for shooting the enemy king.
// MOBILITY heuristic: safe squares around king of color color.
// c is the color of the king shooting the laser
void compute_all_laser_path_heuristics(position_t* p, color_t c, ev_score_t* scores, uint8_t o_pawns, bool verbose) {
  color_t opp_c = opp_color(c);

  uint8_t o_pinned_pawns = 0;  // number of pinned pawns of color opp_c
  float h_attackable = 0;  // for king of color c

  square_t kloc = p->kloc[c];
  square_t o_kloc = p->kloc[opp_c];
  rnk_t o_r_king = rnk_of(o_kloc);
  fil_t o_f_king = fil_of(o_kloc);

  // mark true when hit by laser
  // only used by mobility computation, so only need to mark the eight squares around o_kloc
  bool laser_map[ARR_SIZE];
  for (uint8_t f = MAX(o_f_king - 1, 0); f < o_f_king + 2; ++f) {
    for (uint8_t r = MAX(o_r_king - 1, 0); r < o_r_king + 2; ++r) {
      square_t sq = square_of(f, r);
      laser_map[sq] = false;
    }
  }

  square_t sq = kloc;
  int8_t bdir = ori_of(p->board[sq]);

  tbassert(ptype_of(p->board[sq]) == KING,
           "ptype: %d\n", ptype_of(p->board[sq]));
  tbassert(ptype_of(p->board[o_kloc]) == KING,
           "ptype: %d\n", ptype_of(p->board[o_kloc]));
  tbassert(color_of(p->board[o_kloc]) == opp_c,
           "color: %d\n", color_of(p->board[o_kloc]));

  // laser_map[sq] = true;  // kings cannot occupy same square so don't have to do this
  h_attackable += h_dist(sq, o_f_king, o_r_king);

  bool brk = false;
  while (!brk) {
    sq += beam_of(bdir);
    tbassert(sq < ARR_SIZE && sq >= 0, "sq: %d\n", sq);

    laser_map[sq] = true;

    piece_t piece = p->board[sq];
    switch (ptype_of(piece)) {
      case EMPTY:  // empty square
        h_attackable += h_dist(sq, o_f_king, o_r_king);
        break;

      case INVALID:  // Ran off edge of board
        brk = true;
        break;

      case PAWN:  // Pawn
        h_attackable += h_dist(sq, o_f_king, o_r_king);

        if (color_of(piece) == opp_c) {
          o_pinned_pawns++;
        }

        bdir = reflect_of(bdir, ori_of(piece));
        if (bdir < 0) {  // Hit back of Pawn
          brk = true;
        }
        break;

      case KING:  // King
        h_attackable += h_dist(sq, o_f_king, o_r_king);
        brk = true;  // sorry, game over my friend!
        break;

      default:  // Shouldna happen, man!
        tbassert(false, "Not cool, man.  Not cool.\n");
        break;
    }
  }

  uint8_t o_mobility = 0;  // mobility of king of color opp_c
  for (uint8_t f = MAX(o_f_king - 1, 0); f < MIN(o_f_king + 2, BOARD_WIDTH); ++f) {
    for (uint8_t r = MAX(o_r_king - 1, 0); r < MIN(o_r_king + 2, BOARD_WIDTH); ++r) {
      square_t sq = square_of(f, r);
      if (laser_map[sq] == false) {
        o_mobility++;
      }
    }
  }

  scores[opp_c] += o_mobility * MOBILITY;
  scores[c] += (int)h_attackable * HATTACK;
  scores[opp_c] += PAWNPIN * (o_pawns - o_pinned_pawns);
}

// computes laser path heuristics as usual, but relies on previous scores to calculate
// pcentral, pbetween, and material/pawn count heuristics
// kface and kaggressive are done directly
score_t eval_incremental(position_t *p, bool verbose) {
  // TODO: store location of zapped piece so that this method can be used if a piece is zapped
  tbassert(p->victims.zapped == 0, "no way to find location of zapped piece\n");

  // seed rand_r with a value of 1, as per
  // http://linux.die.net/man/3/rand_r
  static __thread unsigned int seed = 1;

  color_t to_move = color_to_move_of(p);
  color_t last_moved = opp_color(to_move);

  // compute all as if the last move was made by white, then multiply by delta_mult later

  // // MATERIAL
  if (p->victims.stomped != 0) {
    if (to_move == WHITE) {
      p->pawn_count_white--;
    } else {
      p->pawn_count_black--;
    }
  }
  // if (p->victims.zapped != 0) {
  //   delta_pawncount--;
  // }

  // // PBETWEEN
  // // subtract stomped, zapped
  // // if last move was a pawn:
  // //    check if moved in or out of the square of the kings
  // // if last move was a king:
  // //    check all pawns in the changed row and column
  // ev_score_t delta_pcentral = 0;


  // PCENTRAL
  // subtract stomped, zapped
  // if last move was a pawn:
  //    subtract previous score, add new score
  ev_score_t delta_pcentral = 0;

  move_t last_move = p->last_move;
  square_t from_sq = from_square(last_move);
  square_t to_sq = to_square(last_move);

  if (p->victims.stomped != 0) {
    // white stomps black, increases score
    delta_pcentral += pcentral(fil_of(to_sq), rnk_of(to_sq));
  }
  if (ptype_mv_of(last_move) == PAWN) {
    delta_pcentral -= pcentral(fil_of(from_sq), rnk_of(from_sq));
    delta_pcentral += pcentral(fil_of(to_sq), rnk_of(to_sq));
  }

  // if (p->zapped != 0) {
  //   delta_pcentral -= ;
  // }

  // KFACE
  // KAGGRESSIVE
  ev_score_t k_scores = 0;
  square_t w_kloc = p->kloc[WHITE];
  square_t b_kloc = p->kloc[BLACK];
  rnk_t w_r_king = rnk_of(w_kloc);
  fil_t w_f_king = fil_of(w_kloc);
  rnk_t b_r_king = rnk_of(b_kloc);
  fil_t b_f_king = fil_of(b_kloc);
  k_scores += kface(w_f_king, w_r_king, b_f_king, b_r_king, ori_of(p->board[w_kloc]))
        + kaggressive(w_f_king, w_r_king, b_f_king, b_r_king);
  k_scores -= kface(b_f_king, b_r_king, w_f_king, w_r_king, ori_of(p->board[b_kloc]))
        + kaggressive(b_f_king, b_r_king, w_f_king, w_r_king);

  ev_score_t score[2] = { 0, 0 };

  // should also be able to do this for pcentral, but floating point rounding gives
  // different result (not necessarily a worse result though)
  int p_between[2] = {0};

  for (fil_t f = 0; f < BOARD_WIDTH; f++) {
    for (rnk_t r = 0; r < BOARD_WIDTH; r++) {
      square_t sq = ARR_WIDTH * (FIL_ORIGIN + f) + RNK_ORIGIN + r;
      piece_t x = p->board[sq];
      color_t c = (color_t) ((x >> COLOR_SHIFT) & COLOR_MASK);

      switch (ptype_of(x)) {
        case EMPTY:
          break;
        case PAWN:
          // PBETWEEN heuristic
          p_between[c] += pbetween(f, r, w_f_king, w_r_king, b_f_king, b_r_king);
          break;
        case INVALID:
          break;
        default:
          tbassert(false, "Jose says: no way!\n");   // No way, Jose!
      }
    }
  }

  int delta_mult = 2*(last_moved == WHITE) - 1;
  p->p_between = p_between[WHITE] - p_between[BLACK];
  p->p_central += delta_mult * delta_pcentral;
  p->ev_score_valid = true;

  score[WHITE] += p_between[WHITE] * PBETWEEN;
  score[BLACK] += p_between[BLACK] * PBETWEEN;

  compute_all_laser_path_heuristics(p, WHITE, score, p->pawn_count_black, verbose);
  compute_all_laser_path_heuristics(p, BLACK, score, p->pawn_count_white, verbose);

  // score from WHITE point of view

  ev_score_t tot = score[WHITE] - score[BLACK];
  tot += p->p_central;
  tot += PAWN_EV_VALUE * (p->pawn_count_white - p->pawn_count_black);
  tot += k_scores;

  if (RANDOMIZE) {
    ev_score_t  z = rand_r(&seed) % (RANDOMIZE*2+1);
    tot = tot + z - RANDOMIZE;
  }

  if (color_to_move_of(p) == BLACK) {
    tot = -tot;
  }

  return tot / EV_SCORE_RATIO;
}

// Static evaluation.  Returns score
score_t eval(position_t *p, bool verbose) {
  // tbassert(((p->pawn_count != NO_EV_SCORE) == (p->p_between != NO_EV_SCORE)) &&
  //          ((p->p_between != NO_EV_SCORE) == (p->p_central != NO_EV_SCORE)),
  //          "should be all set or all not set\n");

  if (ptype_mv_of(p->last_move) != INVALID && p->p_between != NO_EV_SCORE) {
    // TODO: when ev_score_valid, calculate even more quickly
    if (p->victims.zapped == 0 && !p->ev_score_valid) {
      return eval_incremental(p, verbose);
    }
  }

  // seed rand_r with a value of 1, as per
  // http://linux.die.net/man/3/rand_r
  static __thread unsigned int seed = 1;
  ev_score_t score[2] = { 0, 0 };

  // should also be able to do this for pcentral, but floating point rounding gives
  // different result (not necessarily a worse result though)
  uint8_t pawn_counts[2] = {0};
  int p_between[2] = {0};
  int p_central[2] = {0};

  square_t w_kloc = p->kloc[WHITE];
  square_t b_kloc = p->kloc[BLACK];
  rnk_t w_r_king = rnk_of(w_kloc);
  fil_t w_f_king = fil_of(w_kloc);
  rnk_t b_r_king = rnk_of(b_kloc);
  fil_t b_f_king = fil_of(b_kloc);

  for (fil_t f = 0; f < BOARD_WIDTH; f++) {
    for (rnk_t r = 0; r < BOARD_WIDTH; r++) {
      square_t sq = ARR_WIDTH * (FIL_ORIGIN + f) + RNK_ORIGIN + r;
      piece_t x = p->board[sq];
      color_t c = (color_t) ((x >> COLOR_SHIFT) & COLOR_MASK);

      switch (ptype_of(x)) {
        case EMPTY:
          break;
        case PAWN:
          // MATERIAL heuristic: Bonus for each Pawn
          pawn_counts[c]++;

          // PBETWEEN heuristic
          p_between[c] += pbetween(f, r, w_f_king, w_r_king, b_f_king, b_r_king);

          // PCENTRAL heuristic
          p_central[c] += pcentral(f,r);
          break;

        case KING:
          // KFACE heuristic
          // KAGGRESSIVE heuristic
          if (c == WHITE) {
            score[c] += kface(w_f_king, w_r_king, b_f_king, b_r_king, ori_of(x))
             + kaggressive(w_f_king, w_r_king, b_f_king, b_r_king);
          } else {
            score[c] += kface(b_f_king, b_r_king, w_f_king, w_r_king, ori_of(x))
              + kaggressive(b_f_king, b_r_king, w_f_king, w_r_king);
          }
          break;
        case INVALID:
          break;
        default:
          tbassert(false, "Jose says: no way!\n");   // No way, Jose!
      }
    }
  }


  p->pawn_count_white = pawn_counts[WHITE];
  p->pawn_count_black = pawn_counts[BLACK];
  p->p_between = p_between[WHITE] - p_between[BLACK];
  p->p_central = p_central[WHITE] - p_central[BLACK];
  p->ev_score_valid = true;

  score[WHITE] += p_central[WHITE];
  score[BLACK] += p_central[BLACK];


  // MATERIAL heuristic
  score[WHITE] += pawn_counts[WHITE] * PAWN_EV_VALUE;
  score[BLACK] += pawn_counts[BLACK] * PAWN_EV_VALUE;

  score[WHITE] += p_between[WHITE] * PBETWEEN;
  score[BLACK] += p_between[BLACK] * PBETWEEN;

  compute_all_laser_path_heuristics(p, WHITE, score, pawn_counts[BLACK], verbose);
  compute_all_laser_path_heuristics(p, BLACK, score, pawn_counts[WHITE], verbose);

  // score from WHITE point of view
  ev_score_t tot = score[WHITE] - score[BLACK];

  if (RANDOMIZE) {
    ev_score_t  z = rand_r(&seed) % (RANDOMIZE*2+1);
    tot = tot + z - RANDOMIZE;
  }

  if (color_to_move_of(p) == BLACK) {
    tot = -tot;
  }

  return tot / EV_SCORE_RATIO;
}
