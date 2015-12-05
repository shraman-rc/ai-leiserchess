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

// TODO: determine which king is on which corner just once (would slightly speed this up)
// TODO: iteration in eval_incremental without using this function at all
// PBETWEEN heuristic: Bonus for Pawn at (f, r) in rectangle defined by Kings at the corners
int pbetween(fil_t f, rnk_t r, fil_t w_f, rnk_t w_r, fil_t b_f, rnk_t b_r) {
  bool is_between =
      between(f, w_f, b_f) &&
      between(r, w_r, b_r);

  return is_between;
}

// KFACE heuristic: bonus (or penalty) for King facing toward the other King
ev_score_t kface(int delta_fil, int delta_rnk, int ori) {
  //int delta_fil = o_f - f;
  //int delta_rnk = o_r - r;
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
ev_score_t kaggressive(int delta_fil, int delta_rnk, fil_t f, rnk_t r) {
  //int delta_fil = o_f - f;
  //int delta_rnk = o_r - r;

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
// laser_map : now stored in high order bits of board pieces
// c : color of king shooting laser
// marks all hit squares with true
// NOTE: most of this code is duplicated in compute_all_laser_path_heuristics

// NOTE: now ONLY MARKS PINNED PAWNS
void mark_laser_path_pinned_pawns(position_t *p, color_t c) {
  // Fire laser and record pinned status in high order bits of pawn pieces
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
        mark_pinned(&p->board[sq]);
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
ev_score_t compute_all_laser_path_heuristics(position_t* p, color_t c) {
  uint8_t o_pinned_pawns = 0;  // number of pinned pawns of color opp_c
  float h_attackable = 0;      // for king of color c

  color_t opp_c = opp_color(c);
  square_t o_kloc = p->kloc[opp_c];
  rnk_t o_r_king = rnk_of(o_kloc);
  fil_t o_f_king = fil_of(o_kloc);

  // mark true when hit by laser
  // only used by mobility computation, so only need to mark the eight squares around o_kloc
  // slightly faster than marking the entire array with false
  for (uint8_t f = MAX(o_f_king - 1, 0); f < o_f_king + 2; ++f) {
    for (uint8_t r = MAX(o_r_king - 1, 0); r < o_r_king + 2; ++r) {
      square_t sq = square_of(f, r);
      unmark_pinned(&p->board[sq]);
    }
  }

  square_t sq = p->kloc[c];  // laser starts at king
  // laser_map[sq] = true;  // kings cannot occupy same square so don't have to do this
  h_attackable += h_dist(sq, o_f_king, o_r_king);
  int8_t bdir = ori_of(p->board[sq]);

  tbassert(ptype_of(p->board[sq]) == KING,
           "ptype: %d\n", ptype_of(p->board[sq]));
  tbassert(ptype_of(p->board[o_kloc]) == KING,
           "ptype: %d\n", ptype_of(p->board[o_kloc]));
  tbassert(color_of(p->board[o_kloc]) == opp_c,
           "color: %d\n", color_of(p->board[o_kloc]));

  bool brk = false;
  while (!brk) {
    sq += beam_of(bdir);
    mark_pinned(&p->board[sq]);
    piece_t piece = p->board[sq];

    tbassert(sq < ARR_SIZE && sq >= 0, "sq: %d\n", sq);

    switch (ptype_of(piece)) {
      case EMPTY:
        h_attackable += h_dist(sq, o_f_king, o_r_king);
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
      case INVALID:  // Ran off edge of board
        brk = true;
        break;
      case KING:  // King
        h_attackable += h_dist(sq, o_f_king, o_r_king);
        brk = true;
        break;
      default:
        tbassert(false, "Not cool, man.  Not cool.\n");
        break;
    }
  }

  uint8_t o_mobility = 0;  // mobility of king of color opp_c
  for (uint8_t f = MAX(o_f_king - 1, 0); f < MIN(o_f_king + 2, BOARD_WIDTH); ++f) {
    for (uint8_t r = MAX(o_r_king - 1, 0); r < MIN(o_r_king + 2, BOARD_WIDTH); ++r) {
      square_t sq = square_of(f, r);
      if (is_pinned(p->board[sq]) == false) {
        o_mobility++;
      }
    }
  }

  ev_score_t delta_score = (int)h_attackable * HATTACK
                           - o_mobility * MOBILITY
                           + PAWNPIN * o_pinned_pawns;
  return delta_score;
}

// relies on previous scores to calculate pcentral, pbetween, and material/pawn count heuristics
void update_eval_score(position_t *p) {
  tbassert(p->ev_score_valid && p->ev_score_needs_update, "scores not valid or update not necessary\n");

  color_t to_move = color_to_move_of(p);
  color_t last_moved = opp_color(to_move);

  // SOME delta scores calculated as if white had last moved and then multiplied by this
  int delta_mult = 2*(last_moved == WHITE) - 1;

  // // MATERIAL
  if (p->victims.stomped != 0) {
    if (to_move == WHITE) {
      p->pawn_count--;
    } else {
      p->pawn_count++;
    }
  }

  move_t last_move = p->last_move;
  square_t from_sq = from_square(last_move);
  square_t to_sq = to_square(last_move);

  square_t w_kloc = p->kloc[WHITE];
  square_t b_kloc = p->kloc[BLACK];
  fil_t w_f_king = fil_of(w_kloc);
  rnk_t w_r_king = rnk_of(w_kloc);
  fil_t b_f_king = fil_of(b_kloc);
  rnk_t b_r_king = rnk_of(b_kloc);

  // // PBETWEEN
  // // subtract stomped, zapped
  // // if last move was a king:
  // //    check all pawns in the changed row and column
  ev_score_t delta_pbetween = 0;
  if (p->victims.stomped != 0) {
    // compute as if white stomped black pawn
    delta_pbetween += pbetween(fil_of(to_sq), rnk_of(to_sq), w_f_king, w_r_king, b_f_king, b_r_king);
  }

  switch (ptype_mv_of(last_move)) {
    case PAWN:
      delta_pbetween -= pbetween(fil_of(from_sq), rnk_of(from_sq), w_f_king, w_r_king, b_f_king, b_r_king);
      delta_pbetween += pbetween(fil_of(to_sq), rnk_of(to_sq), w_f_king, w_r_king, b_f_king, b_r_king);

      delta_pbetween *= delta_mult;
      break;
    case KING:
      if (rot_of(last_move) !=  NONE) {
        // no change to pbetween
        break;
      }

      // see if king moved closer or farther
      // king move can be diagonal...
      // pbetween for stomped pawn has already been taken care of!
      square_t new_kloc = p->kloc[last_moved];
      tbassert(new_kloc == to_sq, "same\n");
      square_t old_kloc = from_sq;

      square_t o_kloc = p->kloc[to_move]; // opponent kloc

      fil_t delta_f_0 = abs(fil_of(old_kloc) - fil_of(o_kloc));
      fil_t delta_f_1 = abs(fil_of(new_kloc) - fil_of(o_kloc));

      fil_t d_f;    // iter bounds
      square_t d_kloc;  // iter bounds
      color_t pos_delta;    // which color has positive delta

      if (delta_f_0 > delta_f_1) {
        // king moved closer to opponent
        // iterate over fil_of(from_sq) with ranks between the two kings
        d_f = fil_of(old_kloc);
        d_kloc = old_kloc;
        pos_delta = BLACK;
      } else if (delta_f_0 < delta_f_1) {
        // moved away
        d_f = fil_of(new_kloc);
        d_kloc = new_kloc;
        pos_delta = WHITE;
      } else {
        goto RANKS;
      }

      for (rnk_t r = 0; r < BOARD_WIDTH; r++) {
        square_t sq = square_of(d_f, r);
        if (ptype_of(p->board[sq]) != PAWN) {
          continue;
        }
        // TODO: set iter bounds above and get rid of pbetween
        // TODO: compute fil_of and rnk_of once
        if (color_of(p->board[sq]) == pos_delta) {
          delta_pbetween += pbetween(d_f, r, fil_of(d_kloc), rnk_of(d_kloc), fil_of(o_kloc), rnk_of(o_kloc));
        } else {
          delta_pbetween -= pbetween(d_f, r, fil_of(d_kloc), rnk_of(d_kloc), fil_of(o_kloc), rnk_of(o_kloc));
        }
      }

RANKS:
      pos_delta = pos_delta;
      rnk_t delta_r_0 = abs(rnk_of(old_kloc) - rnk_of(o_kloc));
      rnk_t delta_r_1 = abs(rnk_of(new_kloc) - rnk_of(o_kloc));

      rnk_t d_r;    // iter bounds
      // square_t d_kloc;  // iter bounds
      // color_t pos_delta;    // which color has positive delta

      if (delta_r_0 > delta_r_1) {
        // king moved closer to opponent
        // iterate over rnk_of(from_sq) with files between the two kings
        d_r = rnk_of(old_kloc);
        d_kloc = old_kloc;
        pos_delta = BLACK;
      } else if (delta_r_0 < delta_r_1) {
        // moved away
        d_r = rnk_of(new_kloc);
        d_kloc = new_kloc;
        pos_delta = WHITE;
      } else {
        break;
      }

      for (fil_t f = 0; f < BOARD_WIDTH; f++) {
        square_t sq = square_of(f, d_r);
        if (ptype_of(p->board[sq]) != PAWN) {
          continue;
        }
        if (color_of(p->board[sq]) == pos_delta) {
          delta_pbetween += pbetween(f, d_r, fil_of(d_kloc), rnk_of(d_kloc), fil_of(o_kloc), rnk_of(o_kloc));
        } else {
          delta_pbetween -= pbetween(f, d_r, fil_of(d_kloc), rnk_of(d_kloc), fil_of(o_kloc), rnk_of(o_kloc));
        }
      }
      break;
    default:
      tbassert(false, "Bogus\n");
      break;
  }

  // PCENTRAL
  // subtract stomped, zapped
  // if last move was a pawn:
  //    subtract previous score, add new score
  ev_score_t delta_pcentral = 0;

  if (p->victims.stomped != 0) {
    delta_pcentral += pcentral(fil_of(to_sq), rnk_of(to_sq));
  }
  if (ptype_mv_of(last_move) == PAWN) {
    delta_pcentral -= pcentral(fil_of(from_sq), rnk_of(from_sq));
    delta_pcentral += pcentral(fil_of(to_sq), rnk_of(to_sq));
  }

/////////////////// zapping ///////////////////////
  if (p->victims.zapped != 0) {
    // white zapped black pawn
    square_t squ = p->victims.zapped_square;

    square_t old_kloc_w = p->kloc[WHITE];
    square_t old_kloc_b = p->kloc[BLACK];

    if (ptype_mv_of(last_move) == KING) {
      if (last_moved == WHITE) {
        old_kloc_w = from_sq;
      } else {
        old_kloc_b = from_sq;
      }
    }

    if (color_of(p->victims.zapped) == WHITE) {
      p->pawn_count--;
      // directly modify p->p_central because delta_pcentral is multiplied by delta_mult
      p->p_central -= pcentral(fil_of(p->victims.zapped_square), rnk_of(p->victims.zapped_square));
      delta_pbetween -= pbetween(fil_of(squ), rnk_of(squ), fil_of(old_kloc_w), rnk_of(old_kloc_w), fil_of(old_kloc_b), rnk_of(old_kloc_b));
    } else {
      p->pawn_count++;
      p->p_central += pcentral(fil_of(p->victims.zapped_square), rnk_of(p->victims.zapped_square));
      delta_pbetween += pbetween(fil_of(squ), rnk_of(squ), fil_of(old_kloc_w), rnk_of(old_kloc_w), fil_of(old_kloc_b), rnk_of(old_kloc_b));
    }
  }
/////////////////////////////////////

  p->p_between += delta_pbetween;
  p->p_central += delta_mult * delta_pcentral;
  p->ev_score_needs_update = false;
}

score_t compute_eval_score(position_t *p) {
  tbassert(p->ev_score_valid && !p->ev_score_needs_update, "compute_eval_score assert 474\n");

  ev_score_t score = 0;
  score += PAWN_EV_VALUE * p->pawn_count;   // MATERIAL
  score += PBETWEEN * p->p_between;         // PBETWEEN
  score += p->p_central;                    // PCENTRAL

  // PAWNPIN, MOBILITY, HATTACK
  score += PAWNPIN * p->pawn_count;   // don't condense with earlier line for clarity
  score += compute_all_laser_path_heuristics(p, WHITE);
  score -= compute_all_laser_path_heuristics(p, BLACK);

  // KFACE, KAGGRESSIVE
  square_t w_kloc = p->kloc[WHITE];
  square_t b_kloc = p->kloc[BLACK];
  rnk_t w_r_king = rnk_of(w_kloc);
  fil_t w_f_king = fil_of(w_kloc);
  rnk_t b_r_king = rnk_of(b_kloc);
  fil_t b_f_king = fil_of(b_kloc);
  int delta_fil_w = b_f_king - w_f_king;
  int delta_rnk_w = b_r_king - w_r_king;
  score += kface(delta_fil_w, delta_rnk_w, ori_of(p->board[w_kloc]));
  score += kaggressive(delta_fil_w, delta_rnk_w, w_f_king, w_r_king);
  score -= kface(-delta_fil_w, -delta_rnk_w, ori_of(p->board[b_kloc]));
  score -= kaggressive(-delta_fil_w, -delta_rnk_w, b_f_king, b_r_king);

  if (RANDOMIZE) {
    // not sure if seeding has to be done earlier?
    // seed rand_r with a value of 1, as per
    // http://linux.die.net/man/3/rand_r
    static __thread unsigned int seed = 1;

    ev_score_t  z = rand_r(&seed) % (RANDOMIZE*2+1);
    score += z - RANDOMIZE;
  }

  if (color_to_move_of(p) == BLACK) {
    score = -score;
  }

  return score / EV_SCORE_RATIO;
}

// Static evaluation.  Returns score
score_t eval(position_t *p, bool verbose) {
  if (p->ev_score_valid) {
    if (p->ev_score_needs_update) {
      update_eval_score(p);
    }
    return compute_eval_score(p);
  }

  // compute all from scratch

  uint8_t pawn_counts[2] = {0};
  uint8_t p_between[2] = {0};
  uint16_t p_central[2] = {0};

  square_t w_kloc = p->kloc[WHITE];
  square_t b_kloc = p->kloc[BLACK];
  rnk_t w_r_king = rnk_of(w_kloc);
  fil_t w_f_king = fil_of(w_kloc);
  rnk_t b_r_king = rnk_of(b_kloc);
  fil_t b_f_king = fil_of(b_kloc);

  for (fil_t f = 0; f < BOARD_WIDTH; f++) {
    for (rnk_t r = 0; r < BOARD_WIDTH; r++) {
      square_t sq = square_of(f, r);
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
          break;
        case INVALID:
          break;
        default:
          tbassert(false, "Jose says: no way!\n");   // No way, Jose!
      }
    }
  }

  p->pawn_count = pawn_counts[WHITE] - pawn_counts[BLACK];
  p->p_between = p_between[WHITE] - p_between[BLACK];
  p->p_central = p_central[WHITE] - p_central[BLACK];
  p->ev_score_valid = true;
  p->ev_score_needs_update = false;

  return compute_eval_score(p);
}
